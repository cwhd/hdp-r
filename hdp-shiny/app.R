# HDP-R - Clean rebuild of the other version
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
source("modules/ui.elements.r",local=T)
source("modules/matrix.helper.r",local=T)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  titlePanel("HDP Builder"),
  
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
                   h3("uiDynaFactors"),
                   uiOutput("uiDynaAlternatives")
                 )
        ),
        tabPanel("Model Results",
                 h4("Results wil be here..."),
                 uiOutput("uiCriteriaResults"),
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
    ui.initialFactors.build()
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

    #TODO this shouldn't look at the old tree...or should it?
    #create features for each level in the tree
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
    #update the expert evaluation
    ui.evaluation.build()
  }
  
  ui.refresh.fromTree <- function(tree) {
    #TODO rebuild the form from a tree from the DB
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
    
    #TODO this shouldn't look at the old tree...or should it?
    #create features for each level in the tree
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
    #update the expert evaluation
    ui.evaluation.build()
  }
  
  #update the factors
  ui.initialFactors.build <- function(){
    print("ui.initialFactors.build()")

        output$uiDynaFactors <- renderUI({
      featureLevels <- lapply(2:hdp$tree$height, function(i) { #3 = first level of factors
        ui.level.textInput.generate(i, hdp$tree)
      })
      do.call(tabsetPanel,featureLevels)
    })
    
    #TODO add a dynamic button to remove the level
  }
  
  #observer to add alternatives
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
  
  #observer to render the tree
  observe({
    print("rendering gridViz")
    output$xx=renderGrViz({
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(hdp$tree)),engine = "dot")
    })
  })
  
  #Get the values from dynamic sliders and save them
  #TODO need to add them as elements of the tree and save them
  observeEvent(input$btnSaveOpinion, {
    #### get the weights from the dynamic sliders
    #TODO maybe it would be better if this was reactive??
    
    dfLevels <- ToDataFrameNetwork(hdp$tree, "level", "name")
    comparisonPanelNumber <- hdp$tree$height
    if(length(hdp$alternatives > 0)) { comparisonPanelNumber <- comparisonPanelNumber + 1 }
    #TODO this needs to roll everything up every level
    criteriaDfList <- lapply(2:comparisonPanelNumber, function(i) {
      combos <- treeLevel.combos.unique(i, dfLevels, hdp$tree, hdp$alternatives)
      comboFrames <- comboFrames.buildFromSliders(combos, i)
      
      populatedMatrix <- {
        if(i == comparisonPanelNumber) {
          dfComparisons <- dfLevels[dfLevels$level == i-1,c("from","to","level","name")]
          #matrixColumns <- c(dfComparisons$name,hdp$alternatives)
          matrixColumns <- dfComparisons$name
          matrix.alternativesVsFeatures(matrixColumns, hdp$alternatives, comboFrames)
        } else {
          dfComparisons <- dfLevels[dfLevels$level == i,c("from","to","level","name")]
          matrixColumns <- dfComparisons$name
          matrix.buildFromComboFrames(matrixColumns,comboFrames)
        }
      }
      print("----pop Mat")
      print(populatedMatrix)

      calculatedMatrix <- matrix.calculate(populatedMatrix)
    })

    print("---criteriaDfList")
    print(criteriaDfList)

    output$uiCriteriaResults <- renderUI({
      matrixOutputs <- lapply(1:length(criteriaDfList),function(i){
        renderTable(
          criteriaDfList[i]
          )
      })
      do.call(tagList,matrixOutputs)
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
        ui.sliders.generate(i,dfLevels, hdp$tree, hdp$alternatives)
      })
      do.call(tabsetPanel,sliders)
    })

    lapply(2:comparisonPanelNumber, function(i) {
      ui.sliders.observers.add(i,dfLevels)
    })
  }
  
  comboFrames.buildFromSliders <- function(combos, level) {
    dfCriteria <- split(combos,rep(1:nrow(combos),1))
    criteriaDfList <- lapply(1:nrow(combos), function(i) {
      dfOut <- data.frame(streOne = c(input[[paste0("slider_",level,"_",i)]]), streTwo = c(100 - input[[paste0("slider_",level,"_",i)]]))
      colnames(dfOut) <- c(dfCriteria[[i]][[1]], dfCriteria[[i]][[2]])
      return(dfOut)
    })
    criteriaDfList
  }
  
  ui.sliders.observers.add <- function(level, dfLevels) {
    #add observers to the critiera sliders
    combos <- treeLevel.combos.unique(level, dfLevels, hdp$tree, hdp$alternatives)
    
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
        
      }, escape = FALSE, server = FALSE
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
      
      #TODO set criteria and stuff...
      
      #TODO reload everything...
      #tree.rebuild() 
      
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

