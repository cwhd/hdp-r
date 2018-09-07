# HDP-R - Admin tool to build models 
# A shiny based interface to collect data for HDP models

library(shiny)
library(data.tree)  #for managing the hierarchy
library(DiagrammeR) #display the tree
library(mongolite)  #use Mongo for storage 
library(rjson)      #gives us more flexibility for storing and loading models
library(DT)         #interface for selecting models from the DB
library(xtable)
library(plyr)
library(hdpr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  titlePanel("HDM Builder"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        textInput("txtModelName","Model Name",placeholder = "The name of your model"),
        textInput("txtDecison","Decision", placeholder = "Whatever you're trying to decide"),
        textInput("txtCriteria","Criteria", placeholder = "i.e.) criteria1,2, etc")
      ),
      wellPanel(
        h5("Factors"),
        uiOutput("uiDynaFactors")
      ),
      wellPanel(
        textInput("txtAlternatives", "Alternatives", placeholder = "i.e.) alternative 1, 2, etc"),
        actionButton("btnUpdateAlternatives", "Update Alternatives")
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Model Designer",
                 wellPanel(
                   h4("Decision Tree"),
                   actionButton("btnSaveModel", "Save Model"),
                   actionButton("btnRebuildTree", "Rebuild Tree From Form"),
                   actionButton("btnLoadExample", "Load Example"),
                   uiOutput("uiExpertUrl")
                 ),
                 grVizOutput("xx"),
                 wellPanel(
                   h3("Alternatives"),
                   uiOutput("uiDynaAlternatives")
                 )
        ),
        tabPanel("Experts",
                 h4("Add or remove experts here"),
                 p("After you've saved your model you can add some experts. Each 
                   expert will have a specific URL where they will rate your model, 
                   if you need to manually send a URL make sure to use the correct one."),
                 uiOutput("uiExperts"),
                 textInput("txtNewExpert", "New Expert", placeholder = "enter email here..."),
                 actionButton("btnUpdateExperts","Update Experts"),
                 actionButton("btnAddNewExpert","Add New")
        ),
        tabPanel("Evaluation Form",
                 h4("This is the form your experts will fill out"),
                 p("Your experts will see a form like this, you don't have to do anything
                   here."),
                 uiOutput("uiEvaluateCriteria")
        ),
        tabPanel("Results",
                 h4("Expert Evaluations"),
                 actionButton("btnLoadResults","Load Results"),
                 dataTableOutput("tblResults"),
                 uiOutput("uiIndividualExperts")
        ),
        tabPanel("Saved Models",
                 actionButton("btnLoadModels", "Load all models"),
                 h4("List of all previous models"),
                 uiOutput("uiDynaModels"),
                 verbatimTextOutput("modelSelectInfo"),
                 dataTableOutput(outputId = "dtMongoOutput")
        ),
        tabPanel("Instructions",
                 h4("Welcome to the HDM Admin tool, here is how to use it."),
                 tags$ol(
                   tags$li("Design your model."),
                   tags$li("Find some experts to help you evaluate the model."),
                   tags$li("Send your experts an evaluation to weight your options."),
                   tags$li("Use the results for your research.")
                 ),
                 h4("Model Designer"),
                 p("Use it to design your model."),
                 h4("Experts"),
                 p("Experts evaluate your model, manage them in the experts tab."),
                 h4("Evaluation Form"),
                 p("Once you have designed a model the evaluation form is generated. This is
             for you to see what your experts see, it won't actually evaluate anything."),
                 h4("Results"),
                 p("Once your model has been evaluated results will be here."),
                 h4("Saved Models"),
                 p("To load saved models.")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #reactive values used through the app
  hdp=reactiveValues(tree=NULL,criteria=NULL,factors=NULL,criteriaCombos=NULL,
                     alternatives=NULL,loadedModels=NULL,currentModelName=NULL,
                     currentModelId=NULL, expertList = NULL)
  
  #TODO this needs to come from a config or env variable
  evalUrl <- "http://localhost:3838"
  #This is a bit of a hack, but these defaults make managing state easier
  defaultTree <- Node$new("Hierarchical")
  defaultNode1 <- defaultTree$AddChild("Decision")
  defaultNode2 <- defaultTree$AddChild("Making")

  hdp$tree <- defaultTree

  #click the "Load Example" button, get an example
  observeEvent(input$btnLoadExample, {
    defaultTree <- GetExampleTree()
    
    hdp$tree <- defaultTree
    hdp$currentModelName <- "Breakfast Chooser"
    hdp$alternatives = c("eggs","waffles","pancakes","fruit")
    ui.refresh.fromTree(hdp$tree, hdp$alternatives)    
    ui.alternatives.update(hdp$alternatives)
    updateTextInput(session, "txtModelName", value = toString(hdp$currentModelName))
    updateTextInput(session, "txtAlternatives", value = hdp$alternatives)
    
    ui.tree.render(defaultTree)
  })
  
  #someone updated the form and clicked "Rebuild Tree From Form", so do it!
  observeEvent(input$btnRebuildTree, {
    ui.refresh.fromForm()
  })
    
  #this will update everything based on whatever is on the form
  ui.refresh.fromForm <- function() {
    #TODO this blows up if you remove the last level of the tree...need to fix that
    print("ui.refresh.fromForm...")
    newtree <- Node$new(input$txtDecison)
    #add criteria and trim
    criteria <- sapply(unlist(strsplit(input$txtCriteria, ",")),trim)
    for(v in criteria) {
      newtree$AddChildNode(child=Node$new(v))
    }
    
    knownHeight <- if(!is.null(hdp$tree$height)) {
      hdp$tree$height
    } else {
      2
    }
    #add all the features we know of
    print("adding features")
    features <- lapply(2:knownHeight, function(i){
      
      newTreeElements <- getNodeNamesAtLevel(newtree, i) #get all of the new elements
      oldTreeElements <- getNodeNamesAtLevel(hdp$tree, i) #get all of the old elements
      print(paste0("at level: ",i," old: ", 
                   unlist(oldTreeElements), " new: ",unlist(newTreeElements)))

      #combine the 2 lists, return common elements - only look for texts in both
      commonElements <- Reduce(intersect, list(newTreeElements,oldTreeElements))
      if(length(commonElements) > 0) {
        lapply(1:length(commonElements),function(j) {
          nextLevelChildrenText <- unlist(strsplit(input[[paste0('textLevel_',i,"_",commonElements[[j]])]],","))
          if(length(nextLevelChildrenText) > 0) {
            lapply(1:length(nextLevelChildrenText),function(k) {
              FindNode(node=newtree,name = commonElements[[j]])$AddChildNode(child=Node$new(trim(nextLevelChildrenText[[k]])))
            })
          }
        })
      }
    })
      
    hdp$tree <- newtree
    ui.tree.render(newtree)
    alts <- unlist(strsplit(input$txtAlternatives, ","))
    ui.refresh.fromTree(newtree, alts)
  }
  
  #this should only be used when we get a new tree from the DB  
  ui.refresh.fromTree <- function(tree, alternatives) {
    print("ui.refresh.fromtree")
    updateTextInput(session, "txtDecison", value = toString(tree$name))
    updateTextInput(session, "txtCriteria", value = toString(
      lapply(1:length(tree$children), function(i){
        paste(trim(tree$children[[i]]$name),sep = ",")
      })
    ))
    #update the textInputs for the factors
    ui.factors.textInput.build(tree)
    #update the expert evaluation example form
    ui.evaluation.build.byNode(tree, alternatives)
  }
  
  #update the factors
  ui.factors.textInput.build <- function(tree){
    print("ui.factors.textInput.build()")
      output$uiDynaFactors <- renderUI({
      featureLevels <- lapply(2:tree$height, function(i) { #3 = first level of factors
        ui.level.textInput.generate(i, tree)
      })
      do.call(tabsetPanel,featureLevels)
    })
  }
  
  #update alternatives
  ui.alternatives.update <- function(alternatives) {
    output$uiDynaAlternatives <- renderUI({
      alternativeList <- lapply(1:length(alternatives),function(i){
        #TODO need to style this better...
        span(alternatives[i],class="btn btn-success")
      })
      hdp$alternatives <- lapply(alternatives,trim)
      do.call(shiny::tagList,alternativeList)
    })
  }
  
  #manually update alternatives when a user changes them
  observeEvent(input$btnUpdateAlternatives, {
    altSlplitter <- unlist(strsplit(input$txtAlternatives, ","))
    ui.alternatives.update(altSlplitter)
  })
  
  #render the tree on the model page
  ui.tree.render <- function(tree) {
    output$xx=renderGrViz({
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(tree)),engine = "dot")
    })
  }
  
  ###############################################################
  # Functions for managing experts
  ###############################################################

  observeEvent(input$btnAddNewExpert, {
    newExpert <- input$txtNewExpert
    if(trim(newExpert) != "") {
      print(paste0("not null!: ",newExpert))
      hdp$experts <- c(hdp$experts,newExpert)
    }
    ui.experts.build(hdp$experts)
  })
  #when a user clicks the button, update everything
  observeEvent(input$btnUpdateExperts, {
    newExpert <- input$txtNewExpert
    if(length(hdp$experts) > 0) {
      ui.experts.refresh.fromForm(hdp$experts)
    }
    
    #TODO check for current model id, if it doesn't exist notify user
    print("updating experts...")

    saveEverything()
  })
  
  #rebuild the experts in memory from the form
  ui.experts.refresh.fromForm <- function(experts) {
    expertsFromForm <- lapply(1:length(experts), function(i) {
      input[[paste0("txtExpert_",i)]]
    })
    print("experts from form:")
    print(expertsFromForm)
    ui.experts.build(expertsFromForm)
    hdp$experts <- expertsFromForm
  }
  
  #build out the experts form
  ui.experts.build <-function(experts) {
    print("ui.experts.build")
    output$uiExperts <- renderUI({
      expertInputs <- lapply(1:length(experts), function(i) {
        tagList(
          textInput(paste0("txtExpert_",i),"", value = experts[i]),
          tags$a(href=paste0(evalUrl,"?modelId=",hdp$currentModelId,"&expertId=",experts[i]),paste0("Expert URL: ",evalUrl,"?modelId=",hdp$currentModelId,"&expertId=",experts[i]))
        )
      })
      do.call(shiny::tagList,expertInputs)
    })
    updateTextInput(session, "txtNewExpert", value = "", placeholder = "enter new email here...")
  }
  
  #load expert results
  #TODO this should really only have to run once, then save the stuff and load it later
  observeEvent(input$btnLoadResults, {
    print("loading results...")
    
    expertFlatResults <- lapply(1:length(hdp$experts), function(i) { 
      evaluations <- loadResults(hdp$currentModelId, hdp$experts[i])
      
      nodes <- eval(parse(text = evaluations$flatResults$pathString))
      evalWeights <- eval(parse(text = evaluations$flatResults$weight))
      evalNorms <- eval(parse(text = evaluations$flatResults$norm))
      sliderValues <- eval(parse(text = evaluations$flatResults$sliderValues))
      #TODO add means and stuff to the end of this data table...
      
      goodDf <- data.frame(nodes, evalWeights)
      colnames(goodDf) <- c("Criteria")
      goodDf
    })
    #build the matrix for the final results    
    flippedExpertResults <- lapply(1:length(expertFlatResults), function(i) {
      f <- t(expertFlatResults[[i]][-1])
      colnames(f) <- expertFlatResults[[i]][,1]
      rownames(f) <- hdp$experts[i]
      f
    })
    
    resultsTable <- do.call(rbind,flippedExpertResults)

    #build out the tabs for the experts
    output$uiIndividualExperts <- renderUI({
      expertComboFrameTabs <- lapply(1:length(hdp$experts), function(i) { 
        evaluations <- loadResults(hdp$currentModelId, hdp$experts[i])

        #TODO this should be the expert version of the tree...
        nodeNamesHack <- hdp$tree$Get(hack.tree.names)

        comboTableList <- lapply(1:length(nodeNamesHack), function(j) {
          comboFrameList <- evaluations$comboFrames[[nodeNamesHack[j]]]
          renderDataTable({
            datatable(
              as.data.frame(comboFrameList),
              caption = nodeNamesHack[j],
              width = 100, 
              rownames = FALSE,
              options = list(
                scrollX = FALSE,
                scrollY = FALSE,
                searching = FALSE,
                paging = FALSE,
                ordering = FALSE,
                info = FALSE,
                autoWidth = FALSE
              )
            )
          })
        })
        taby <- tabPanel(hdp$experts[i], comboTableList) 
        taby
      })
      do.call(tabsetPanel,expertComboFrameTabs)
    })
    
    # TODO still need other vals like inconsistency or whaever
    
    output$tblResults <- renderDataTable(
      datatable(resultsTable,  options = list(
        scrollX = FALSE,
        scrollY = FALSE,
        searching = FALSE,
        paging = FALSE,
        ordering = FALSE
      )
    ))
  })
  ##################################################
  # -> slider functions
  ##################################################
  #for all the combinations of elements in a Node, get the data from the
  #sliders on the page
  comboFrames.buildFromNodeSliders <- function(combos, node) {
    dfCriteria <- split(combos,rep(1:nrow(combos),1))
    criteriaDfList <- lapply(1:nrow(combos), function(i) {
      dfOut <- data.frame(streOne = c(input[[paste0("slider_",node$name,"_",i)]]), streTwo = c(100 - input[[paste0("slider_",node$name,"_",i)]]))
      colnames(dfOut) <- c(dfCriteria[[i]][[1]], dfCriteria[[i]][[2]])
      return(dfOut)
    })
    criteriaDfList
  }
  
  ui.evaluation.build.byNode <- function(tree, alternatives) {
    print("ui.evaluation.build.byNode")
    nodes <- tree$Get('name')

    #TODO this blows up if there is only one child leaf on a tree. Since that should never
    # happen then maybe just put a warning instead of handling it...
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
  
  ui.nodesliders.observers.add <- function(node, alternatives) {
    combos <- node.combos.unique(node, alternatives)
    if(length(combos) > 0) {
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

##################################################
# END slider functions
##################################################
  
    
#####################################################
# -> DB Functions
#####################################################

  #Load models from DB into dynamic grid
  observeEvent(input$btnLoadModels, {
    #TODO "load my models" - input email and get them 
    #TODO - maybe enter a user pin -not super secure...?
    observeEvent(input$btnLoadModels, {
      print("Loading Models")
      modelData <- loadAllModels()
      hdp$loadedModels <- modelData
      output$dtMongoOutput <- renderDataTable({
        datatable(modelData, list(mode = "single", target = "cell", selection = "single"))
      }, escape = FALSE, server = FALSE
      )
    })
  })
  
  #when a row is selected from the grid, load it onto the page
  observe({
    print("updating from db...")
    s = input$dtMongoOutput_rows_selected
    if (length(s)) {
      selectedObjectId <- hdp$loadedModels[[s,"_id"]] #get objectId based on selected index
      mod <- loadModel(selectedObjectId)
      
      #build the expert URL
      hdp$currentModelId <- selectedObjectId
      output$uiExpertUrl <- renderUI({
        tags$a(href=paste0(evalUrl,"?modelId=",selectedObjectId),paste0("Expert URL: ",evalUrl,"?modelId=",selectedObjectId))
      })
      
      goodDf <- RebuildDataFrameForHDMTree(mod)
      hdp$tree <- FromDataFrameNetwork(goodDf)
      
      hdp$currentModelName <- mod$modelName
      
      updateTextInput(session, "txtModelName", value = toString(hdp$currentModelName))
      #load the alternatives
      updateTextInput(session, "txtAlternatives", value = mod$alternatives)
      hdp$alternatives <- eval(parse(text = mod$alternatives))
      if(!is.null(mod$experts)) {
        hdp$experts <- eval(parse(text = mod$experts))
        ui.experts.build(hdp$experts)
      } else {
        output$uiExperts <- renderUI({})
        hdp$experts <- NULL
      }

      ui.refresh.fromTree(hdp$tree, hdp$alternatives)
      ui.tree.render(hdp$tree)
    }
  })
  
  #once model is designed, save it to the DB
  observeEvent(input$btnSaveModel, {
    saveEverything()
  })
  
  saveEverything <- function() {
    dfTreeAsNetwork <- ToDataFrameNetwork(hdp$tree, "pathString")
    
    fullJson <- paste0('{ "modelName" : "',input$txtModelName,'","model":', toJSON(dfTreeAsNetwork),
                       ',"alternatives":',toJSON(hdp$alternatives),
                       ',"experts":',toJSON(hdp$experts),
                       '}')
    
    if(!is.null(hdp$currentModelId)) {
      saveData(fullJson, hdp$currentModelId)
    } else {
      saveData(fullJson, NULL)
    }    
  }
  
#####################################################
# End DB Functions
#####################################################
  #send logs to stderr for production - ugly hack
  if (!interactive()) sink(stderr(), type = "output") 
}

# Run the application 
shinyApp(ui = ui, server = server)

