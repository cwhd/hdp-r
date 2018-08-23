# HDP-R 2 - Clean rebuild of the other version
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
source("modules/slider.ui.r",local=T)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  titlePanel("HDP Builder"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        textInput("txtModelName","Model Name",placeholder = "The name of your model"),
        textInput("txtDecison","Decision", placeholder = "Whatever you're trying to decide"),
        textInput("txtCriteria","Criteria", placeholder = "i.e.) criteria1,2, etc"),
        actionButton("btnAddFactorLevel", "Initialize Factors")
      ),
      wellPanel(
        fluidRow(
          column(3,h5("Factors")),
          column(3,actionButton("btnRebuildTree", "Rebuild Tree"))
        ),
        uiOutput("uiDynaFactors")
      ),
      wellPanel(
        textInput("txtAlternatives", "Alternatives", placeholder = "i.e.) alternative 1, 2, etc")
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Model Designer",
                 wellPanel(
                   h3("Decision Tree"),
                   actionButton("btnSaveModel", "Save Model")
                 ),
                 grVizOutput("xx"),
                 wellPanel(
                   h3("uiDynaFactors"),
                   uiOutput("uiDynaAlternatives")
                 )
        ),
        tabPanel("Model Results",
                 h4("Results wil be here..."),
                 uiOutput("uiCriteriaResults"),
                 tableOutput("tblCriteriaCombinations"),
                 tableOutput("tblFeatureCombinations")
        ),
        tabPanel("Evaluation Form",
                 h4("Compare each item against the other"),
                 fluidRow(
                   column(7, uiOutput("uiEvaluateCriteria")),
                   column(5, 
                          textInput("txtExpertName","Your Name:"),
                          actionButton("btnSaveOpinion", "Submit your opinions")
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
    #TODO update factors here...or not??
    #ui.initialFactors.build()
  })
  
  observeEvent(input$btnRebuildTree, {
   tree.rebuild() 
  })
    
  #get data from the form, rebuild the tree
  tree.rebuild <- function() {
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

    #create features for each leve in the tree
    print("adding features")
    features <- lapply(2:hdp$tree$height, function(i){
      print(paste0("---On Level: ",i))
      #get the elementes at the current level of the tree
      currentElements <- getNodeNamesAtLevel(hdp$tree, i)
      # - get the textInput, add it to the tree
      lapply(1:length(currentElements),function(j) {
        nextLevelChildrenText <- unlist(strsplit(input[[paste0('textLevel_',i,"_",currentElements[[j]])]],","))
        if(length(nextLevelChildrenText) > 0) {
          lapply(1:length(nextLevelChildrenText),function(k) {
            FindNode(node=newtree,name = currentElements[[j]])$AddChildNode(child=Node$new(trim(nextLevelChildrenText[[k]])))
          })
        }
      })
    })
    print("------Final Tree")
    print(newtree)
    hdp$tree <- newtree
    
    ui.evaluation.build()
  }
  
  observeEvent(input$btnAddFactorLevel, {
    ui.initialFactors.build()
  })

  #update the factors
  ui.initialFactors.build <- function(){
    print("ui.initialFactors.build()")
    
    hdp$criteria <- unlist(strsplit(input$txtCriteria, ","))
    hdp$criteria <- sapply(hdp$criteria,trim)
    
    output$uiDynaFactors <- renderUI({
      featureLevels <- lapply(2:hdp$tree$height, function(i) { #3 = first level of factors
        ui.level.textInput.generate(i)
      })
      do.call(tabsetPanel,featureLevels)
    })
    
    #TODO add a dynamic button to remove the level
  }
  
  #TODO move this into a UI function file...
  #generate text inputs for somewhere on the page
  ui.level.textInput.generate <- function(level) {
    print("ui.leveltextInput.generate")
    nodesAtLevel <- getNodeNamesAtLevel(hdp$tree, level)
    
    textBoxes <- lapply(1:length(nodesAtLevel),function(i){   #for each node at the current level
      #add a node to the tree for the new text input
      if(length(nodesAtLevel > 0)) {
        currentNode <- FindNode(node=hdp$tree,name = nodesAtLevel[[i]])
        textInput(paste0("textLevel_",level,"_",nodesAtLevel[[i]]),
                  nodesAtLevel[[i]],
                  value = childrenOrNothing(currentNode)
        )
      }
    })

    taby <- tabPanel(paste0("Level ",level),
      textBoxes
    )
    
    taby
  }
  
  #When alternatives are added, put them on the screen under the tree
  #TODO this could be a good candidate for a module?
  observe({
    print("altSplitter")
    altSlplitter <- unlist(strsplit(input$txtAlternatives, ","))
    output$uiDynaAlternatives <- renderUI({
      alternativeList <- lapply(1:length(altSlplitter),function(i){
        #TODO need to style this better...
        span(altSlplitter[i],class="btn btn-success")
      })
      hdp$alternatives <- lapply(altSlplitter,trim)
      do.call(shiny::tagList,alternativeList)
    })
  })
  
  observe({
    print("rendering gridViz")
    output$xx=renderGrViz({
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(hdp$tree)),engine = "dot")
    })
  })
  
  
  ##################################################
  # -> slider functions
  ##################################################
  ui.evaluation.build <- function() {

    #first convert the tree to data frame for matrix operations
    dfLevels <- ToDataFrameNetwork(hdp$tree, "level", "name")
    
    comparisonPanelNumber <- hdp$tree$height
    if(length(hdp$alternatives > 0)) { comparisonPanelNumber <- comparisonPanelNumber + 1 }
    output$uiEvaluateCriteria <- renderUI({
      sliders <- lapply(2:comparisonPanelNumber, function(i) {
        ui.sliders.generate(i,dfLevels)
      })
      do.call(tabsetPanel,sliders)
    })

    lapply(2:comparisonPanelNumber, function(i) {
      ui.sliders.observers.add(i,dfLevels)
    })
  }
  
  slider.combos.unique <- function(level, dfLevels) {
    #TODO if this is the last level of the tree and there are alternatives,
    # - compare all alternatives will everything in the last level
    #print(paste0("alties: ",length(hdp$alternatives)))
    #comparisonPanelNumber
    if(level > hdp$tree$height) {
    #if(length(hdp$alternatives > 0) && level == hdp$tree$height + 1) {
      print("we have alternatives and are on the last row")
      dfComparisons <- dfLevels[dfLevels$level == level - 1,c("from","to","level","name")]
      combos <- expand.grid.unique(dfComparisons$name, hdp$alternatives)
      print(combos)
      combos    
    } else {
      #print("not doing alternatives right now")
      dfComparisons <- dfLevels[dfLevels$level == level,c("from","to","level","name")]
      combos <- expand.grid.unique(dfComparisons$name, dfComparisons$name)
      combos    
    }
  }
  
  ui.sliders.generate <- function(level, dfLevels) {
    combos <- slider.combos.unique(level, dfLevels)
    #build the critiera sliders for a level in the tree
    sliders <- lapply(1:nrow(combos), function(i) {
      fluidRow(
        column(1, 
               span(combos[i,1]),
               uiOutput(paste0("uiOutputValueA_",level,"_",i))
        ),
        column(5,
               sliderInput(paste0("slider_",level,"_",i),"",value = 50, min = 0, max = 100)
        ), 
        column(1,
               span(combos[i,2]),                 
               uiOutput(paste0("uiOutputValueB_",level,"_",i))
        )
      )
    })
    
    taby <- tabPanel(paste0("Level ",level), sliders)
    taby
  }
  
  ui.sliders.observers.add <- function(level, dfLevels) {
    #add observers to the critiera sliders
    combos <- slider.combos.unique(level, dfLevels)
    
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

  ##################################################
  # END slider functions
  ##################################################
  
    
#####################################################
# -> DB Functions
#####################################################

  #TODO I think this is a good candidate for creating a module...
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
  
  #when a row is selected from the grid, load it onto the page
  observe({
    s = input$dtMongoOutput_rows_selected
    if (length(s)) {
      selectedObjectId <- hdp$loadedModels[[s,"_id"]] #get objectId based on selected index
      #print(selectedObjectId)
      mod <- loadModel(selectedObjectId)
      
      #load the model name
      hdp$currentModelName <- mod$modelName
      updateTextInput(session, "txtModelName", value = hdp$currentModelName)
      
      #turn the model back into a tree
      froms <- eval(parse(text = mod$model$from))
      tos <- eval(parse(text = mod$model$to))
      pathStrings <- eval(parse(text = mod$model$pathString))
      
      goodDf <- data.frame(froms,tos,pathStrings)
      hdp$tree <- FromDataFrameNetwork(goodDf)
      
      #load the alternatives
      updateTextInput(session, "txtAlternatives", value = mod$alternatives)
      hdp$alternatives <- mod$alternatives
      
      #TODO reload everything...
    }
  })
  
  #once model is designed, save it to the DB
  observeEvent(input$btnSaveModel, {
    #TODO maybe the best thing is to read the URL and generate the UI that way...
    dfTreeAsNetwork <- ToDataFrameNetwork(hdp$tree, "pathString")
    #print("Structure going in: ")
    #print(str(dfTreeAsNetwork))
    
    fullJson <- paste0('{ "modelName" : "',input$txtModelName,'","model":', toJSON(dfTreeAsNetwork),
                       ',"alternatives":',toJSON(hdp$alternatives),
                       '}')
    #print(fullJson)
    
    saveData(fullJson)
  })

#####################################################
# End DB Functions
#####################################################
  
}

# Run the application 
shinyApp(ui = ui, server = server)

