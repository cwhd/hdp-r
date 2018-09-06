# HDP-evaluate-shiny
# where experts evaluate HDM models...and your HDM dreams come true
#TODO blog idea: 3 things for awesome Shiny UIs - valiation, consider the user, 

library(shiny)
library(data.tree)
library(mongolite)  #use Mongo for storage
library(DiagrammeR) #display the tree
library(DT)         #interface for selecting models from the DB
library(rjson)      #gives us more flexibility for storing and loading models

source("../modules/db.functions.r",local=T)
source("../modules/ui.elements.r",local=T)
source("../modules/tree.helper.r",local=T)
source("../modules/utilities.r",local=T)
source("../modules/matrix.helper.r",local=T)

ui <- fluidPage(
   
   # Application title
   titlePanel("HDP Model Evaluation"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h3("Instructions"),
        p("In this method, two elements are compared with each other at a time. 
          The expert allocates a total of 100 points to the two elements in the 
          proportion of their relative importance to the objective. For example:"),
        tags$ul(
          tags$li(" If A is 3 times as important as B, A gets 75 points, B gets 25 points"),
          tags$li("If the importance of A and B are the same, both get 50 points. This is 
             the case regardless of whether both are extremely important, mildly important 
             or unimportant."),
          tags$li("If A is ¼ as important as B, A gets 20 points, B gets 80 points."),
          tags$li("Zero is not used in the pairwise comparisons. If the importance of 
             A is negligible in comparison to B, A gets 1 point, B gets 99 points. ")
        ),
        actionButton("btnLoadFromQueryString", "Ready? Load the evaluations!")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h4("Compare each item against the other"),
        fluidRow(
          column(7, uiOutput("uiEvaluateCriteria")),
          column(5, 
                 actionButton("btnSaveAndCalculate", "Submit your evaluation"),
                 uiOutput("uiMessages"),
                 grVizOutput("modelTree")
          )
        )
      )
   )
)

server <- function(input, output, session) {
  
  #TODO load any saved values that we have for the expert -< loadResults(modelId, expertId)
  # - note eval below..not sure if that will work...
  #TODO maybe make the saving reactive?? 
  #TODO be nicer in the interface
  
  hdp=reactiveValues(tree=NULL, alternatives=NULL, evaluationId=NULL, 
                     expertId=NULL, modelId=NULL)
  
  #Load the form from the query string
  observeEvent(input$btnLoadFromQueryString, {
    query <- getQueryString()
    queryText <- paste(names(query), query,
                       sep = "=", collapse=", ")
    
    #variables from the query string
    requestedModelId <- query[["modelId"]]
    currentExpert <- query[["expertId"]]

    evalValues <- loadResults(requestedModelId, currentExpert)
    
    expertValues.exists <- FALSE
    
    alternatives <- NULL

    #if we have an eval, preload it
    #TODO need better error handling if the model comes back and can't load...
    goodDf <- if(nrow(evalValues) > 0) {
      #print(evalValues)
      print("------data found for this expert, loading it...")
      expertValues.exists <- TRUE
      #print(evalValues)
      froms <- eval(parse(text = evalValues$results$from))
      tos <- eval(parse(text = evalValues$results$to))
      pathStrings <- eval(parse(text = evalValues$results$pathString))
      evalWeights <- eval(parse(text = evalValues$results$weight))
      evalNorms <- eval(parse(text = evalValues$results$norm))
      sliderValues <- eval(parse(text = evalValues$results$sliderValues))
      
      data.frame(froms,tos,pathStrings, evalWeights, evalNorms, sliderValues)
    } else {
      #there is no existing evaluation data for this expert so just load the model
      print("----no eval found, loading model")
      mod <- loadModel(requestedModelId)
      
      froms <- eval(parse(text = mod$model$from))
      tos <- eval(parse(text = mod$model$to))
      pathStrings <- eval(parse(text = mod$model$pathString))
      
      alternatives <- eval(parse(text = mod$alternatives))

      data.frame(froms,tos,pathStrings)
    }
    print("------got data set, buiding tree")
    tree <- FromDataFrameNetwork(goodDf)
    
    #add alternatives as nodes in the tree
    if(!expertValues.exists && !is.null(alternatives)) {
      print("----adding new level of nodes to the tree...")
      bottomNodes <- getNodesAtLevel(tree, tree$height)
      #print(bottomNodes)
      lapply(1:nrow(bottomNodes),function(i) {
        lapply(1:length(alternatives), function(j) {
          FindNode(node=tree,name = bottomNodes[[i,"name"]])$AddChildNode(child=Node$new(trim(alternatives[[j]])))
        })
      })
    } 

    ui.evaluation.build.byTree(tree)

    hdp$tree <- tree
    hdp$alternatives <- NULL# alternatives
    hdp$expertId <- currentExpert
    hdp$modelId <- requestedModelId
    
    print("-----------StRT")
    
    ui.tree.render(hdp$tree)
  })
  
  ################################################
  # Get form values, calculate & save
  ###############################################
  
  #this is perfect for a unit test
  #calculate weight for requested node
  node.normalize <- function(currentNode) {
    #get parent
    parent <- currentNode$parent
    #do calculations for parent node
    #get unique combinations
    #combos <- node.combos.unique(parent, NULL)
    ##put the combinations into frames
    #comboFrames <- comboFrames.buildFromNodeSliders(combos, parent)
    #NEW code - make sure it works
    comboFrames <- expert.comboFrames.generate(currentNode)
    #build the comparison frames into a matrix
    matrixColumns <- lapply(1:length(parent$children), function(i){
      parent$children[[i]]$name
    })
    populatedMatrix <- matrix.buildFromComboFrames(matrixColumns,comboFrames)
    #now that we have the matrix of comparisons, run the calculations
    calculatedMatrix <- matrix.calculate(populatedMatrix)
    #return calculated values for this node    
    return(calculatedMatrix[[currentNode$name,2]])
  }
  
  #generate the combo frames so I can save them for later
  #TODO dup code, clean this up
  expert.comboFrames.generate <- function(currentNode) {
    parent <- currentNode$parent
    #do calculations for parent node
    #get unique combinations
    combos <- node.combos.unique(parent, NULL)
    #put the combinations into frames
    comboFrames <- comboFrames.buildFromNodeSliders(combos, parent)
    comboFrames
  }
  
  #calculate the weighted value for each node
  node.finalizeWeights <- function(node) {
    parent.norm <- node$parent$norm
    weight <- node$norm * parent.norm
    #TODO maybe if weight is NA just move norm over to that column...
    return(weight)
  }
  
  #get the value of a slider based on the node
  slider.get <- function(node) {
    combos <- node.combos.unique(node, NULL)
    nodeSliderValues <- lapply(1:nrow(combos), function(i) {
      input[[paste0("slider_",node$name,"_",i)]]
    })
    #print("nodeliders")
    #print(unlist(nodeSliderValues))
    unlist(nodeSliderValues)
  }
  
  #when the button is clicked, calculate and save everything  
  observeEvent(input$btnSaveAndCalculate, {
    #run the calculations across nodes in the tree
    hdp$tree$Do(function(node) {
      node$norm <-  node.normalize(node)
    }, filterFun = isNotRoot) 

    hdp$tree$Do(function(node) {
      node$weight <-  node.finalizeWeights(node)
    }, filterFun = isNotRoot) 
    #get the raw slider values and save them so we can pre-populate the form
    hdp$tree$Do(function(node) {
      node$sliderValues <- slider.get(node)
    }, filterFun = isNotLeaf)
    
    comboFrameList <- hdp$tree$Get(expert.comboFrames.generate, filterFun = isNotRoot)
    print("-------comboFrameList:")
    print(comboFrameList)
    
    #print("----bunch of new stuff in the tree")
    #print(hdp$tree)
    
    #convert the tree to a data frame and save it to the DB
    dfTreeAsNetwork <- ToDataFrameNetwork(hdp$tree, "pathString","weight","norm","sliderValues")
    #I am not sure why I have to do this, annoying
    dfTreeAsNetwork$from <- lapply(dfTreeAsNetwork$from,getLastElementInPath)
    dfTreeAsNetwork$to <- lapply(dfTreeAsNetwork$to,getLastElementInPath)
    
    print(dfTreeAsNetwork)
    
    dfTreeFlatResults <- ToDataFrameTree(hdp$tree,"pathString","weight","norm","sliderValues")
    dfTreeFlatResults$pathString <- lapply(dfTreeFlatResults$pathString,getLastElementInPath)
    
    #print("---maybe this")
    dfTreeFlatResults$levelName <- NULL
    #TODO add inconsistency to the flat results
    print(dfTreeFlatResults)
    
    fullJson <- paste0('{ "modelId" : "',hdp$modelId,'",
                        "expertId" : "',hdp$expertId,'",
                       "results":', toJSON(dfTreeAsNetwork),
                       ',"alternatives":',toJSON(hdp$alternatives),
                       ',"flatResults":',toJSON(dfTreeFlatResults),
                       ',"comboFrames":',toJSON(comboFrameList),
                       '}')
    saveEvaluation(fullJson, hdp$expertId, hdp$modelId)
    
    #TODO check tree to make sure we have reasonable values for everything
    output$uiMessages <- renderUI({
      h3("Thanks for taking the evaluation! Feel free to tweak your answers or just have a nice day :)")
    })
  })
  
  #hack to fix to and from containing entire pathString
  getLastElementInPath <- function(val) {
    splits <- unlist(strsplit(val, "/"))
    retVal <- splits[length(splits)]
    #print(retVal)
    retVal
  }
  
  #build the combo frames from the sliders
  comboFrames.buildFromNodeSliders <- function(combos, node) {
    dfCriteria <- split(combos,rep(1:nrow(combos),1))
    criteriaDfList <- lapply(1:nrow(combos), function(i) {
      dfOut <- data.frame(streOne = c(input[[paste0("slider_",node$name,"_",i)]]), streTwo = c(100 - input[[paste0("slider_",node$name,"_",i)]]))
      colnames(dfOut) <- c(dfCriteria[[i]][[1]], dfCriteria[[i]][[2]])
      return(dfOut)
    })
    criteriaDfList
  }
  
  #############################################
  # TODO clean this up...
  #############################################
  
  #TODO probably need to add level here to accomodate duplicate node names
  slider.new <- function(node) {
    combos <- node.combos.unique(node, NULL)
    rawValues <- sapply(unlist(strsplit(as.character(node$sliderValues), ",")),trim)
    print(node$name)
    print("--raw values??")
    print(rawValues)
    #TODO may need to make sure there are no spaces or special chars in the name
    sliders <- lapply(1:nrow(combos), function(i) {
      #print("--generating sliders")
      #print(rawValues[i])
      sliderValue <- 50
      sliderValue <- if(!is.null(rawValues[i])) {
        rawValues[i]
      } #else {
        #50
      ##}
      #print("----sliderValue")
      #print(sliderValue)
      
      fluidRow(
        column(1, 
               span(combos[i,1]),
               uiOutput(paste0("uiOutputValueA_",node$name,"_",i))
        ),
        column(5,
               sliderInput(paste0("slider_",node$name,"_",i),"",
                           value = sliderValue, 
                           min = 1, max = 99)
        ), 
        column(1,
               span(combos[i,2]),                 
               uiOutput(paste0("uiOutputValueB_",node$name,"_",i))
        )
      )
    })

    sliders <- c(sliders, grVizOutput(paste0("treeNode_",node$name)))
  }
  
  ui.evaluation.build.byTree <- function(tree) {
    print("ui.evaluation.build.byTree")
    nodeNamesHack <- tree$Get(hack.tree.names, filterFun = isNotLeaf)
    output$uiEvaluateCriteria <- renderUI({
      sliders <- tree$Get(slider.new, filterFun = isNotLeaf)
      tabSliders <- lapply(1:length(sliders), function(i) {
        taby <- tabPanel(paste0(nodeNamesHack[i]),sliders[i]) 
        taby
      })
      do.call(tabsetPanel,tabSliders)
    })
    #modelTree
    #add the observers
    tree$Get(ui.nodesliders.observers.add.byNode, filterFun = isNotLeaf)
    
    #tree$Do(ui.babytree.generate, filterFun = isNotLeaf)
    #TODO can probably update style in the observer with Do...
    #tree$Do(ui.tabs.observers.add, filterFun = isNotLeaf)
  }
  
  #TODO prob delete this too
  ui.babytree.generate <- function(node) {
    babyTree <- Node$new(node$name)
    #lapply(1:length(node$children), function(i) {
    #  babyTree$AddChildNode(child=Node$new(node$children[i]$name))
    #})
    output[[paste0("treeNode_",node$name)]]=renderGrViz({
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(node)),engine = "dot")
    })
  }
  #add observer to the tab? probably delete this
  ui.tabs.observers.add <- function(node) {
    testTree <- Node$new(node$name)
    
    observeEvent(input[[node$name]], {
      output$modelTree = renderGrViz({
        grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(testTree)),engine = "dot")
      })
    })
    
  }
  
  #add observers to the sliders here
  ui.nodesliders.observers.add.byNode <- function(node) {
    #tree$Get(ui.nodesliders.observers.add.byNode, filterFun = isNotLeaf)
    combos <- node.combos.unique(node, NULL)
    lapply(1:nrow(combos), function(i) {
      observeEvent(input[[paste0("slider_",node$name,"_",i)]], {
        output[[paste0("uiOutputValueA_",node$name,"_",i)]] <- renderUI({
          span(input[[paste0("slider_",node$name,"_",i)]])
        })
        output[[paste0("uiOutputValueB_",node$name,"_",i)]] <- renderUI({
          span(100 - input[[paste0("slider_",node$name,"_",i)]])
        })
        
        ui.tree.render(hdp$tree, node)
      })
    })
  }
  
  #render a nice tree
  ui.tree.render <- function(tree, specialNode) {
    print("rendering tree...")
    #TODO regenerate tree from here!!! this sets the state
    
    SetNodeStyle(tree,  style = "filled,rounded", shape = "box", fillcolor = "GreenYellow", 
                 fontname = "helvetica")

    if(!missing(specialNode)) {
      SetNodeStyle(specialNode,  inherit = FALSE, fillcolor = "Thistle", 
                   fontcolor = "Firebrick")
    }
    
    output$modelTree=renderGrViz({
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(tree)),engine = "dot")
    })
    
  }
  
  ########################################
  ## DELETE ME!!!
  #######################################
  #with a tree and alternatives build out the evaluation form
  #TODO this may get deleted
  ui.evaluation.build.byNode <- function(tree, alternatives) {
    print("ui.evaluation.build.byNode")
    nodes <- tree$Get('name')
    
    output$uiEvaluateCriteria <- renderUI({
      sliders <- lapply(1:length(nodes), function(i) {
        ui.sliders.generate.byNode(FindNode(node = tree, name = nodes[i]), alternatives)
      })
      do.call(tabsetPanel,sliders)
    })
    
    lapply(1:length(nodes), function(i) {
      ui.nodesliders.observers.add(FindNode(node = tree, name = nodes[i]), alternatives)
    })
  }
  #add observers to the evaluation sliders
  ui.nodesliders.observers.add <- function(node, alternatives) {
    #TODO only do this when there are children...because we are adding alts to he tree here
    # - w should be good to go
    combos <- node.combos.unique(node, alternatives)
    #print("------node")
    #print(node)
    #print("----alts")
    #print(alternatives)
    if(length(combos) > 1) {
      lapply(1:nrow(combos), function(i) {
        observeEvent(input[[paste0("slider_",node$name,"_",i)]], {
          output[[paste0("uiOutputValueA_",node$name,"_",i)]] <- renderUI({
            span(input[[paste0("slider_",node$name,"_",i)]])
          })
          output[[paste0("uiOutputValueB_",node$name,"_",i)]] <- renderUI({
            span(100 - input[[paste0("slider_",node$name,"_",i)]])
          })
        })
      })
    }
  }
}

# Run the application 
shinyApp(ui = ui, server = server)

