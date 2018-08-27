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
  #TODO get query string
  #TODO load requested model from DB
  #btnLoadFromQueryString
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
    
    ui.evaluation.build(tree, "")
  })
  
  
  
  #############################################
  # TODO this is duplicate code...also in the admin tool
  #############################################
  
  ui.evaluation.build <- function(tree, alternatives) {
    
    #first convert the tree to data frame for matrix operations
    dfLevels <- ToDataFrameNetwork(tree, "level", "name")
    
    comparisonPanelNumber <- tree$height + 1
    #print(paste0("alts agaion:",hdp$alternatives," len:",length(hdp$alternatives)," cNum:",comparisonPanelNumber))
    #TODO why does this blow up????
    #if(length(hdp$alternatives > 0)) { comparisonPanelNumber <- comparisonPanelNumber + 1 }
    output$uiEvaluateCriteria <- renderUI({
      sliders <- lapply(2:comparisonPanelNumber, function(i) {
        ui.sliders.generate(i,dfLevels, tree, alternatives)
      })
      do.call(tabsetPanel,sliders)
    })
    
    lapply(2:comparisonPanelNumber, function(i) {
      ui.sliders.observers.add(i,dfLevels, tree, "")
    })
  }
  
  ui.sliders.observers.add <- function(level, dfLevels, tree, alternatives) {
    #add observers to the critiera sliders
    combos <- treeLevel.combos.unique(level, dfLevels, tree, alternatives)
    
    lapply(1:nrow(combos), function(i) {
      observeEvent(input[[paste0("slider_",level,"_",i)]], {
        output[[paste0("uiOutputValueA_",level,"_",i)]] <- renderUI({
          span(input[[paste0("slider_",level,"_",i)]])
        })
        output[[paste0("uiOutputValueB_",level,"_",i)]] <- renderUI({
          span(100 - input[[paste0("slider_",level,"_",i)]])
        })
      })
    })
  }
}

# Run the application 
shinyApp(ui = ui, server = server)

