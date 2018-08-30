# HDP-R - Clean rebuild of the other version
# A shiny based interface to collect data for HDP models

library(shiny)
library(data.tree)  #for managing the hierarchy
library(DiagrammeR) #display the tree
library(mongolite)  #use Mongo for storage 
library(rjson)      #gives us more flexibility for storing and loading models
library(DT)         #interface for selecting models from the DB
library(xtable)

source("../modules/utilities.r",local=T)
source("../modules/db.functions.r",local=T)
source("../modules/tree.helper.r",local=T)
source("../modules/ui.elements.r",local=T)
source("../modules/matrix.helper.r",local=T)

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
        textInput("txtAlternatives", "Alternatives", placeholder = "i.e.) alternative 1, 2, etc"),
        actionButton("btnUpdateAlternatives", "Update Alternatives")
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Model Designer",
                 wellPanel(
                   h3("Decision Tree"),
                   actionButton("btnSaveModel", "Save Model"),
                   actionButton("btnRebuildTree", "Rebuild Tree From Form"),
                   actionButton("btnResetForm", "Reset Form"),
                   uiOutput("uiExpertUrl")
                 ),
                 grVizOutput("xx"),
                 wellPanel(
                   h3("Alternatives"),
                   uiOutput("uiDynaAlternatives")
                 )
        ),
        tabPanel("Results",
                 h4("Expert Evaluations"),
                 actionButton("btnLoadResults","Load Results"),
                 dataTableOutput("tblResults"),
                 uiOutput("uiCriteriaResults")
        ),
        tabPanel("Evaluation Form",
                 h4("This is the form your experts will fill out"),
                 p("Your experts will see a form like this, you don't have to do anything
                   here."),
                 uiOutput("uiEvaluateCriteria")
        ),
        tabPanel("Experts",
          h4("Add or remove experts here"),
          uiOutput("uiExperts"),
          textInput("txtNewExpert", "New Expert", placeholder = "enter email here..."),
          actionButton("btnUpdateExperts","Update Experts"),
          actionButton("btnAddNewExpert","Add New")
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
  hdp=reactiveValues(tree=NULL,criteria=NULL,factors=NULL,criteriaCombos=NULL,
                     alternatives=NULL,loadedModels=NULL,currentModelName=NULL,
                     currentModelId=NULL, expertList = NULL)

  defaultTree <- Node$new("What to eat for breakfast")
  taste <- defaultTree$AddChild("Taste")
  speed <- defaultTree$AddChild("Speed")
  #salty <- taste$AddChild("Salty")
  #sweet <- taste$AddChild("Sweet")
  #fast <- speed$AddChild("Fast")
  #slow <- speed$AddChild("Slow")
  
  hdp$tree <- defaultTree
  hdp$currentModelName <- "Breakfast Chooser"
  
  observeEvent(input$btnResetForm, {
    hdp$tree <- defaultTree
    hdp$currentModelName <- "Breakfast Chooser"
    hdp$alternatives = c("eggs","waffles","pancakes","fruit")
    #ui.refresh.fromTree(hdp$tree)    
  })
  
  observeEvent(input$btnRebuildTree, {
    ui.refresh.fromForm()
  })
    
  #this will update everything base on whatever is on the form
  ui.refresh.fromForm <- function() {
    #TODO this blows up if you remove the last level of the tree...need to fix that
    print("ui.refresh.fromForm...")
    newtree <- Node$new(input$txtDecison)
    #add criteria and trim
    criteria <- sapply(unlist(strsplit(input$txtCriteria, ",")),trim)
    for(v in criteria) {
      newtree$AddChildNode(child=Node$new(v))
    }
    #add all the features we know of
    print("adding features")
    features <- lapply(2:hdp$tree$height, function(i){
      
      newTreeElements <- getNodeNamesAtLevel(newtree, i) #get all of the new elements
      oldTreeElements <- getNodeNamesAtLevel(hdp$tree, i) #get all of the old elements
      print(paste0("at level: ",i," old: ", 
                   unlist(oldTreeElements), " new: ",unlist(newTreeElements)))

      #combine the 2 lists, return common elements - only look for texts in both
      commonElements <- Reduce(intersect, list(newTreeElements,oldTreeElements))
      print(paste0("common:",commonElements))
      #TODO this needs to get fixed :|
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
    #TODO alternatives should come from the form
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
    ui.factors.build(tree)
    
    #update the expert evaluation
    #ui.evaluation.build(tree, alternatives)
    ui.evaluation.build.byNode(tree, alternatives)
  }
  
  #update the factors
  ui.factors.build <- function(tree){
    print("ui.factors.build()")

      output$uiDynaFactors <- renderUI({
      featureLevels <- lapply(2:tree$height, function(i) { #3 = first level of factors
        ui.level.textInput.generate(i, tree)
      })
      do.call(tabsetPanel,featureLevels)
    })
    
    #TODO add a dynamic button to remove the level
  }
  
  observeEvent(input$btnAddNewExpert, {
    newExpert <- input$txtNewExpert
    if(trim(newExpert) != "") {
      print(paste0("not null!: ",newExpert))
      hdp$experts <- c(hdp$experts,newExpert)
    }
    ui.experts.build(hdp$experts)
  })
  #when a user clicks the button, upload everything
  observeEvent(input$btnUpdateExperts, {
    newExpert <- input$txtNewExpert
    if(length(hdp$experts) > 0) {
      ui.experts.refresh.fromForm(hdp$experts)
    }

    #TODO save and load experts too
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
        textInput(paste0("txtExpert_",i),"", value = experts[i])
      })
      do.call(shiny::tagList,expertInputs)
    })
    updateTextInput(session, "txtNewExpert", value = "", placeholder = "enter new email here...")
  }
  
  #TODO make this a function that gets called when we update the tree...
  observeEvent(input$btnUpdateAlternatives, {
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
    print("rendering tree")
    output$xx=renderGrViz({
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(hdp$tree)),engine = "dot")
    })
  })
  
  observeEvent(input$btnLoadResults, {
    print("loading results...")
    evaluations <- loadResults(hdp$currentModelId)
    
    #turn the model back into a tree
    froms <- eval(parse(text = evaluations$results$from))
    tos <- eval(parse(text = evaluations$results$to))
    pathStrings <- eval(parse(text = evaluations$results$pathString))
    rawValues <- eval(parse(text = evaluations$results$rawValue))
    normalizedValues <- eval(parse(text = evaluations$results$normalizedValue))

    goodDf <- data.frame(froms,tos,pathStrings,rawValues,normalizedValues)
    print("---good df")
    print(goodDf)
    
    output$tblResults <- renderDataTable(
      goodDf, options = list(
        scrollX = FALSE,
        scrollY = FALSE,
        searching = FALSE,
        paging = FALSE,
        ordering = FALSE
      )
    )
  })
##################################################
# -> slider functions
##################################################
  #old functions by level
  #old function
  ui.evaluation.build <- function(tree, alternatives) {
    print("ui.evaluation.build")
    #first convert the tree to data frame for matrix operations
    dfLevels <- ToDataFrameNetwork(tree, "level", "name")
    
    comparisonPanelNumber <- tree$height
    #print(paste0("alts agaion:",alternatives," len:",length(alternatives)," cNum:",comparisonPanelNumber))
    if(length(alternatives > 0)) { comparisonPanelNumber <- comparisonPanelNumber + 1 }
    output$uiEvaluateCriteria <- renderUI({
      sliders <- lapply(2:comparisonPanelNumber, function(i) {
        ui.sliders.generate(i,dfLevels, tree, alternatives)
      })
      do.call(tabsetPanel,sliders)
    })

    lapply(2:comparisonPanelNumber, function(i) {
      ui.sliders.observers.add(i,dfLevels)
    })
  }
  #old function
  comboFrames.buildFromSliders <- function(combos, level) {
    dfCriteria <- split(combos,rep(1:nrow(combos),1))
    criteriaDfList <- lapply(1:nrow(combos), function(i) {
      dfOut <- data.frame(streOne = c(input[[paste0("slider_",level,"_",i)]]), streTwo = c(100 - input[[paste0("slider_",level,"_",i)]]))
      colnames(dfOut) <- c(dfCriteria[[i]][[1]], dfCriteria[[i]][[2]])
      return(dfOut)
    })
    criteriaDfList
  }
  #old function
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
  
  #######################################################
  #new functions by nodes
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
    #TODO this would be great to have "load my models" - input email and get them 
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
      #TODO this URL needs to be dynamuic
      hdp$currentModelId <- selectedObjectId
      output$uiExpertUrl <- renderUI({
        tags$a(href=paste0("http://localhost:3838?modelId=",selectedObjectId),paste0("Expert URL: http://localhost:3838?modelId=",selectedObjectId))
      })

      #turn the model back into a tree
      froms <- eval(parse(text = mod$model$from))
      tos <- eval(parse(text = mod$model$to))
      pathStrings <- eval(parse(text = mod$model$pathString))
      
      goodDf <- data.frame(froms,tos,pathStrings)
      hdp$tree <- FromDataFrameNetwork(goodDf)
      
      hdp$currentModelName <- mod$modelName
      
      updateTextInput(session, "txtModelName", value = toString(hdp$currentModelName))
      #load the alternatives
      updateTextInput(session, "txtAlternatives", value = mod$alternatives)
      hdp$alternatives <- eval(parse(text = mod$alternatives))
      print(paste0("alts: ",hdp$alternatives))
      
      ui.refresh.fromTree(hdp$tree, hdp$alternatives)
    }
  })
  
  #once model is designed, save it to the DB
  observeEvent(input$btnSaveModel, {
    dfTreeAsNetwork <- ToDataFrameNetwork(hdp$tree, "pathString")

    fullJson <- paste0('{ "modelName" : "',input$txtModelName,'","model":', toJSON(dfTreeAsNetwork),
                       ',"alternatives":',toJSON(hdp$alternatives),
                       '}')
      
    if(!is.null(hdp$currentModelId)) {
      #fullJson <- paste0('{ "_id" : "',hdp$currentModelId,'",',fullJson)
      saveData(fullJson, hdp$currentModelId)
    } else {
      #fullJson <- paste0('{ ', fullJson)
      saveData(fullJson, NULL)
    }
    #print(fullJson)

  })
  
#####################################################
# End DB Functions
#####################################################
  #send logs to stderr for production - ugly hack
  if (!interactive()) sink(stderr(), type = "output") 
}

# Run the application 
shinyApp(ui = ui, server = server)

