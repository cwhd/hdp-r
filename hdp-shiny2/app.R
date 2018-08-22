# HDP-R 2
# A shiny based interface to collect data for HDP models

library(shiny)
library(data.tree)  #for managing the hierarchy
library(DiagrammeR) #display the tree
library(mongolite)  #use Mongo for storage 
library(rjson)      #gives us more flexibility for storing and loading models
library(DT)         #interface for selecting models from the DB
library(xtable)

source("modules/utilities.r",local=T)
source("modules/db.functions.r",local=T)
source("modules/tree.helper.r",local=T)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  titlePanel("HDP Builder"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        textInput("txtModelName","Model Name",placeholder = "The name of your model"),
        h4("Criteria"),
        textInput("txtDecison","Decision", placeholder = "Whatever you're trying to decide"),
        textInput("txtCriteria","Criteria", placeholder = "i.e.) criteria1,2, etc")
      ),
      wellPanel(
        h4("Factors"),
        uiOutput("uiDynaFactors"),
        actionButton("btnAddFactorLevel", "Add Level")
      ),
      wellPanel(
        h4("Alternatives"),
        textInput("txtAlternatives", "Alternatives", placeholder = "i.e.) alternative 1, 2, etc")
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Model Designer",
                 wellPanel(
                   h3("Decision Tree"),
                   actionButton("btnSaveModel", "Save Model"),
                   actionButton("btnRebuildTree", "Rebuild Tree")
                 ),
                 grVizOutput("xx"),
                 wellPanel(
                   h3("Alternatives"),
                   uiOutput("uiDynaAlternatives"),
                   h3("More stuff"),
                   uiOutput("uiDynaLevels")
                 )
        ),
        tabPanel("Model Results",
                 h4("Results wil be here..."),
                 uiOutput("uiCriteriaResults"),
                 tableOutput("tblCriteriaCombinations"),
                 tableOutput("tblFeatureCombinations")
        ),
        tabPanel("Evaluation Form",
                 h4("Evaluate your model below"),
                 p("Instructions will go here..."),
                 textInput("txtExpertName","Your Name:"),                   
                 actionButton("btnSaveOpinion", "Save Your Stuff"),
                 
                 tabsetPanel(
                   tabPanel("Criteria",
                            fluidRow(column(7,
                                            h3('Evaluate Criteria'),
                                            uiOutput("uiEvaluateCriteria")
                            ))
                   ),
                   tabPanel("Features",
                            fluidRow(column(7,
                                            h3("Evaluate Features"),
                                            uiOutput("uiEvaluateFeatures")
                            ))
                   ),
                   tabPanel("Alternatives",
                            fluidRow(column(7,
                                            h3("Evaluate Alternatives"),
                                            uiOutput("uiEvaluateAlternatives")
                            ))
                   )
                 )
        ),
        tabPanel("Previous Models",
                 actionButton("btnLoadModels", "Load all models"),
                 h4("List of all previous models"),
                 uiOutput("uiDynaModels"),
                 verbatimTextOutput("modelSelectInfo"),
                 dataTableOutput(outputId = "dtMongoOutput")
        )
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #reactive values used through the app
  hdp=reactiveValues(tree=NULL,criteria=NULL,factors=NULL,criteriaCombos=NULL,featureCombos=NULL,
                     alternatives=NULL,alternativeCombos=NULL,expertName=NULL,
                     loadedModels=NULL,currentModelName=NULL, treeLevels=NULL)

  defaultTree <- Node$new("What to eat for breakfast")
  taste <- defaultTree$AddChild("Taste")
  speed <- defaultTree$AddChild("Speed")
  salty <- taste$AddChild("Salty")
  sweet <- taste$AddChild("Sweet")
  fast <- speed$AddChild("Fast")
  slow <- speed$AddChild("Slow")
  
  hdp$tree <- defaultTree
  hdp$treeLevels <- 3
  hdp$currentModelName <- "Breakfast Chooser"
  
  #update values from Decision and Criteria
  observe({
    print("observe and update decsion and criteria")
    
    updateTextInput(session, "txtModelName", value = toString(hdp$currentModelName))
    updateTextInput(session, "txtDecison", value = toString(hdp$tree$name))
    updateTextInput(session, "txtCriteria", value = toString(
      lapply(1:length(hdp$tree$children), function(i){
        paste(trim(hdp$tree$children[[i]]$name),sep = ",")
      })
    ))
    #TODO update factors here...
  })
  
  AllInputs <- reactive({
    x <- reactiveValuesToList(input)
    data.frame(
      names = names(x),
      values = unlist(x, use.names = FALSE)
    )
  })
  
  observeEvent(input$btnRebuildTree, {
   tree.rebuild() 
  })
    
  
  tree.rebuild <- function() {
    #TODO I think I should build a new tree, then replace the old tree
    print("----Rebuilding Tree")
    #start with the defintion 
    newtree <- Node$new(input$txtDecison)
    #add criteria
    hdp$criteria <- unlist(strsplit(input$txtCriteria, ","))
    hdp$criteria <- sapply(hdp$criteria,trim)
    for(v in hdp$criteria) {
      trimV <- trim(v)
      newtree$AddChildNode(child=Node$new(trimV))
    }
    #print("Tree so far:")
    #print(newtree)

    #for the number of text input levels we know we have on the page (hdp$levels)
    print("adding features")
    features <- lapply(2:hdp$treeLevels, function(i){
      print(paste0("---On Level: ",i))
      #get the elementes at the current level of the tree
      currentElements <- getNodeNamesAtLevel(hdp$tree, i)
      # - get the textInput, add it to the tree
      lapply(1:length(currentElements),function(j) {
        #print(paste0('textLevel_',i,"_",currentElements[[j]]))
        nextLevelChildrenText <- unlist(strsplit(input[[paste0('textLevel_',i,"_",currentElements[[j]])]],","))
        print("-----nextLevelChildrenText: ")
        print(nextLevelChildrenText)
        lapply(1:length(nextLevelChildrenText),function(k) {
          print(paste0("currentElement: ", currentElements[[j]], " chit: ", nextLevelChildrenText[[k]]))
          FindNode(node=newtree,name = currentElements[[j]])$AddChildNode(child=Node$new(trim(nextLevelChildrenText[[k]])))
          #TODO add the new levels here!!!!!
        })
      })
      
    })
    print("------Final Tree")
    print(newtree)
    hdp$tree <- newtree
    hdp$treeLevels <- hdp$treeLevels + 1
    
    #TODO - we're done here?
    # - since we know the number of levels, we can just generate based on what's on the page
    # - if someone filled in the blanks, add another level
    # - if not, stop there
    #TODO once tree is rebuilt, rebuild the text inputs
    # - this may not do anything, but it might add another level if the bottom level was filled in
  }
  
    #TODO rebuild factor form...mostly working
  observeEvent(input$btnAddFactorLevel, {
    
    hdp$criteria <- unlist(strsplit(input$txtCriteria, ","))
    hdp$criteria <- sapply(hdp$criteria,trim)
    
    print(paste0("treeLevels: ",hdp$treeLevels))
    output$uiDynaFactors <- renderUI({
      featureLevels <- lapply(2:hdp$treeLevels, function(i) { #3 = first level of factors
        ui.level.textInput.generate(i)
      })
      do.call(shiny::tagList,featureLevels)
    })
    
    #hdp$treeLevels <- hdp$treeLevels + 1
    print(hdp$tree)
    #TODO add a dynamic button to remove the level
  })
  
  #TODO move this into a UI function file...
  #generate text inputs for somewhere on the page
  ui.level.textInput.generate <- function(level) {
    print(paste0("--On Level ",level," num of levels: ",hdp$treeLevels))
    nodesAtLevel <- getNodeNamesAtLevel(hdp$tree, level)
    
    textBoxes <- lapply(1:length(nodesAtLevel),function(i){   #for each node at the current level
      #print(paste0("Len: ",length(nodesAtLevel)," i: ",i))
      #add a node to the tree for the new text input
      currentNode <- FindNode(node=hdp$tree,name = nodesAtLevel[[i]])
      
      textInput(paste0("textLevel_",level,"_",nodesAtLevel[[i]]),
                nodesAtLevel[[i]],
                value = childrenOrNothing(currentNode)
      )
    })
    featureHeader <- h4(paste0("Level ",level))
    
    list(featureHeader, textBoxes)
  }
  
  #ugh, this is a hack!!!
  childrenOrNothing <- function(currentNode) {
    if(length(currentNode$children) > 0) {
      toString(
        lapply(1:length(currentNode$children), function(j){
          if(!is.null(currentNode$children[[j]]$name)) {
            toString(currentNode$children[[j]]$name)
          } else {
            ""
          }
      })
      )
    } else {
      ""
    }
  }
  
  #TODO move this into a DB functions function file...
  #Load models from DB into dynamic grid
  observeEvent(input$btnLoadModels, {
    #TODO this would be great to have "load my models" - input email and get them 
    #TODO - maybe enter a user pin -not super secure...?
    observeEvent(input$btnLoadModels, {
      print("Loading Models")
      #uiDynaModels
      modelData <- loadAllModels()
      #dtMongoOutput
      #print(modelData)
      hdp$loadedModels <- modelData
      output$dtMongoOutput <- renderDataTable({
        
        datatable(modelData, list(mode = "single", target = "cell", selection = "single"))
        
      }, escape = FALSE
      )
    })
  })
  
  
  observe({
    output$xx=renderGrViz({
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(hdp$tree)),engine = "dot")
    })
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

