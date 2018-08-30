# HDP-evaluate-shiny
# where experts evaluate HDM models

library(shiny)
library(data.tree)
library(mongolite)  #use Mongo for storage 

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
                 textInput("txtExpertName","Your Name:"),
                 actionButton("btnSaveAndCalculate", "Submit your evaluation")
          )
        )
      )
   )
)

server <- function(input, output, session) {
  
  hdp=reactiveValues(tree=NULL, alternatives=NULL, evaluationId=NULL, 
                     expertId=NULL, modelId=NULL)

  #Load the form from the query string
  observeEvent(input$btnLoadFromQueryString, {
    query <- getQueryString()
    queryText <- paste(names(query), query,
                       sep = "=", collapse=", ")
    
    #print(queryText)
    #variables from the query string
    requestedModelId <- query[["modelId"]]
    currentExpert <- query[["expertId"]]

    updateTextInput(session, "txtExpertName", value = requestedModelId)
    
    #TODO this should be in a try catch
    mod <- loadModel(requestedModelId)

    froms <- eval(parse(text = mod$model$from))
    tos <- eval(parse(text = mod$model$to))
    pathStrings <- eval(parse(text = mod$model$pathString))
    
    goodDf <- data.frame(froms,tos,pathStrings)
    tree <- FromDataFrameNetwork(goodDf)
    alternatives <- eval(parse(text = mod$alternatives))
    
    ui.evaluation.build.byNode(tree, alternatives)

    hdp$tree <- tree
    hdp$alternatives <- alternatives
    hdp$expertId <- currentExpert
    hdp$modelId <- requestedModelId
    
    print("----start with:")
    print(tree)
    print(alternatives)
    print(currentExpert)
    print("-----------")

  })
  

  ################################################
  # Get form values, calculate & save
  ###############################################
  
  evaluation.nodes.calculate <- function(tree, alternatives) {
    #build out the matrix, do the caluclation
    nodes <- tree$Get('name')
    
    comparisonPanelNumber <- length(nodes)
    if(length(hdp$alternatives == 0)) { comparisonPanelNumber <- comparisonPanelNumber - 1 }
    
    normalizedValuesByNode <- lapply(1:comparisonPanelNumber, function(i) {
      currentNode <- FindNode(node = tree, name = nodes[i])
      combos <- node.combos.unique(currentNode, alternatives)
      comboFrames <- comboFrames.buildFromNodeSliders(combos, currentNode)
      matrixColumns <- if(length(currentNode$children) > 0) {
        lapply(1:length(currentNode$children), function(i){
          currentNode$children[[i]]$name
        })
      } else {
        alternatives
      }
      populatedMatrix <- matrix.buildFromComboFrames(matrixColumns,comboFrames)
      
      #2 - now that we have the matrix of comparisons, run the calculations
      calculatedMatrix <- matrix.calculate(populatedMatrix)
    })
  }
  
  observeEvent(input$btnSaveAndCalculate, {
    
    normalizedValuesByNode <- evaluation.nodes.calculate(hdp$tree, hdp$alternatives)
    
    print("---normalizedValuesByNode")
    print(normalizedValuesByNode)
    #match name of tree node to row name, add the normalized value to the node
    lapply(1: length(normalizedValuesByNode), function(i) {
      
      #this will update the tree with values, 
      #TODO what to do with the alternative matrixes?
      #TODO maybe I should just add the alternatives as nodes with prefixes, like 
      lapply(1:nrow(normalizedValuesByNode[[i]]), function(j) {
        #print(paste0("i:",i," of ",length(normalizedValuesByNode),"j:",j," of ",nrow(normalizedValuesByNode[[i]])))
        #add to the tree
        cNode <- FindNode(node=hdp$tree,name = rownames(normalizedValuesByNode[[i]])[j])
        cNode$normalizedValue <- normalizedValuesByNode[[i]][[j,2]]
        cNode$rawValue <- normalizedValuesByNode[[i]][[j,1]]
      })
    })
    
    #print("--updated tree")
    #print(hdp$tree, "normalizedValue", "rawValue")
    
    #TODO save the results of the tree to the DB here
    #build out the JSON expertId
    
    dfTreeAsNetwork <- ToDataFrameNetwork(hdp$tree, "pathString","rawValue","normalizedValue")
    
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

