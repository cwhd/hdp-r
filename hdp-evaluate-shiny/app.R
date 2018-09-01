# HDP-evaluate-shiny
# where experts evaluate HDM models...and your HDM dreams come true

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

    #TODO this should be in a try catch
    mod <- loadModel(requestedModelId)
    #TODO figure out what to do with eval...
    eval <- loadResults(requestedModelId, currentExpert)
    print("------Eval from DB: ")
    print(eval)

    froms <- eval(parse(text = mod$model$from))
    tos <- eval(parse(text = mod$model$to))
    pathStrings <- eval(parse(text = mod$model$pathString))
    
    goodDf <- data.frame(froms,tos,pathStrings)
    tree <- FromDataFrameNetwork(goodDf)
    alternatives <- eval(parse(text = mod$alternatives))
    
    ui.evaluation.build.byNode(tree, alternatives)

    hdp$tree <- tree
    hdp$alternatives <- NULL# alternatives
    hdp$expertId <- currentExpert
    hdp$modelId <- requestedModelId
    
    #add alternatives as nodes in the tree
    bottomNodes <- getNodesAtLevel(tree, tree$height)
    print(bottomNodes)
    lapply(1:nrow(bottomNodes),function(i) {
      lapply(1:length(alternatives), function(j) {
        FindNode(node=tree,name = bottomNodes[[i,"name"]])$AddChildNode(child=Node$new(trim(alternatives[[j]])))
      })
    })

    #print("----start with:")
    #print(tree)
    #print(alternatives)
    #print(currentExpert)
    print("-----------StRT")
    
    output$modelTree=renderGrViz({
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(hdp$tree)),engine = "dot")
    })
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
    combos <- node.combos.unique(parent, NULL)
    #put the combinations into frames
    comboFrames <- comboFrames.buildFromNodeSliders(combos, parent)
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
  
  #calculate the weighted value for each node
  node.finalizeWeights <- function(node) {
    parent.norm <- node$parent$norm
    weight <- node$norm * parent.norm
    #TODO maybe if weight is NA just move norm over to that column...
    return(weight)
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
    
    print(hdp$tree, "norm", "weight")
    
    #convert the tree to a data frame and save it to the DB
    dfTreeAsNetwork <- ToDataFrameNetwork(hdp$tree, "pathString","weight","norm")
    fullJson <- paste0('{ "modelId" : "',hdp$modelId,'",
                        "expertId" : "',hdp$expertId,'",
                       "results":', toJSON(dfTreeAsNetwork),
                       ',"alternatives":',toJSON(hdp$alternatives),
                       '}')
    saveEvaluation(fullJson, hdp$expertId, hdp$modelId)
  })
  
  #############################################
  # TODO this is duplicate code...also in the admin tool
  #############################################
  
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
  #with a tree and alternatives build out the evaluation form
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
    combos <- node.combos.unique(node, alternatives)
    
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

# Run the application 
shinyApp(ui = ui, server = server)

