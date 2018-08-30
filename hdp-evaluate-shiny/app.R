# HDP-evaluate-shiny
# where experts evaluate HDM models

library(shiny)
library(data.tree)
library(mongolite)  #use Mongo for storage 

source("modules/db.functions.r",local=T)
source("modules/ui.elements.r",local=T)
source("modules/tree.helper.r",local=T)
source("modules/utilities.r",local=T)

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
        #TODO expert evaluation goes here...
        #TODO get query string, load model from DB
        #TODO populate evaluation form
        #TODO button to save form
        h4("Compare each item against the other"),
        fluidRow(
          column(7, uiOutput("uiEvaluateCriteria")),
          column(5, 
                 textInput("txtExpertName","Your Name:"),
                 actionButton("btnSaveOpinion", "Submit your opinions")
          )
        )
      )
   )
)

server <- function(input, output, session) {
  observeEvent(input$btnLoadFromQueryString, {
    query <- getQueryString()
    queryText <- paste(names(query), query,
                       sep = "=", collapse=", ")
    
    print(queryText)

    requestedModelId <- query[["modelId"]]
    print(requestedModelId)
    
    updateTextInput(session, "txtExpertName", value = requestedModelId)
    
    #TODO this should be in a try catch
    mod <- loadModel(requestedModelId)

    froms <- eval(parse(text = mod$model$from))
    tos <- eval(parse(text = mod$model$to))
    pathStrings <- eval(parse(text = mod$model$pathString))
    
    goodDf <- data.frame(froms,tos,pathStrings)
    tree <- FromDataFrameNetwork(goodDf)
    print(tree)
    alternatives <- eval(parse(text = mod$alternatives))
    
    ui.evaluation.build.byNode(tree, alternatives)
  })
  
  #############################################
  # TODO this is duplicate code...also in the admin tool
  #############################################
  
  #correct functions
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

