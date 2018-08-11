# HDP-3
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#click on Decision: evaluate criteria against each other
#click on each criteria: evaluate the underlying features
#click on features: evaluatae the alternatives against each other

library(shiny)
library(data.tree)
library(DiagrammeR)
library(mongolite)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("HDP Builder"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        wellPanel(
          h4("Criteria"),
          textInput("txtDecison","Decision", placeholder = "Whatever you're trying to decide"),
          textInput("txtCriteria","Criteria", placeholder = "i.e.) criteria1,2, etc")
        ),
        wellPanel(
          h4("Factors"),
          uiOutput("uiDynaFactors")
          #actionButton("btnUpdateTree", "Update Tree")
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
                     uiOutput("uiDynaAlternatives")
                   )
          ),
          tabPanel("Model Results",
                   h4("Results wil be here..."),
                   tableOutput("tblCriteriaCombinations"),
                   tableOutput("tblFeatureCombinations")
                   ),
          tabPanel("Evaluation Form",
                   h4("Evaluate your Faschnizzle below"),
                   textInput("txtExpertName","Your Name:"),
                   fluidRow(column(7,
                                   h3('Evaluate Criteria'),
                                   uiOutput("uiEvaluateCriteria"),
                                   h3("Evaluate Features"),
                                   uiOutput("uiEvaluateFeatures"),
                                   h3("Evaluate Alternatives"),
                                   uiOutput("uiEvaluateAlternatives")
                      )),
                   actionButton("btnSaveOpinion", "Save Your Stuff")
              )
        )
      )
   )
)

server <- function(input, output, session) {
  
  # Start off with an example. This needs to be overriden when loading from the DB
  tree <- Node$new("Choose Breakfast")
  taste <- tree$AddChild("Taste")
  speed <- tree$AddChild("Speed")
  salty <- taste$AddChild("Salty")
  sweet <- taste$AddChild("Sweet")
  fast <- speed$AddChild("Fast")
  slow <- speed$AddChild("Slow")
  
  #TODO need to also pre-set the alternatives up here...

  hdp=reactiveValues(tree=NULL,criteria=NULL,factors=NULL,criteriaCombos=NULL,featureCombos=NULL,alternatives="Eggs, Waffles, Oatmeal",alternativeCombos=NULL)
  hdp$tree <- tree
  
  #################################################
  # Design the model
  #################################################
  
  ui.tree.rebuild <- function () {
    print("rebuilding tree...")
    hdp$tree <- Node$new(input$txtDecison)

    #add nodes to tree for each criteria
    hdp$criteria <- unlist(strsplit(input$txtCriteria, ","))
    for(v in hdp$criteria) {
      trimV <- trim(v)
      hdp$tree$AddChildNode(child=Node$new(trimV))
      
      #add the feature nodes
      #print(input[[paste0('criteraFeature_',trimV)]])
      #print(!is.null(input[[paste0('criteraFeature_',trimV)]]))
      if(!is.null(input[[paste0('criteraFeature_',trimV)]])) {
        fSplit <- unlist(strsplit(input[[paste0('criteraFeature_',trimV)]],","))
        for(n in fSplit) {
          FindNode(node=hdp$tree,name = trimV)$AddChildNode(child=Node$new(trim(n))) # add child
        }
      }
    }
  }
  
  ui.factors.update <- function() {
    print("updating factors...")
    output$uiDynaFactors <- renderUI({
      lapply(1:length(hdp$tree$children),function(i){   
        textInput(paste0("criteraFeature_",hdp$tree$children[[i]]$name),
                  hdp$tree$children[[i]]$name,
                  value = 
                    toString(
                      lapply(1:length(FindNode(node=hdp$tree,name = hdp$tree$children[[i]]$name)$children), function(j){
                        toString(FindNode(node=hdp$tree,name = hdp$tree$children[[i]]$name)$children[[j]]$name)
                      })
                    )
        )
      })
    })
  }
  
  ui.tree.draw <- function() {
    
  }

  #update values from Decision and Criteria
  observe({
    print("observe and update decsion and criteria")
    updateTextInput(session, "txtDecison", value = toString(hdp$tree$name))
    updateTextInput(session, "txtCriteria", value = toString(
      lapply(1:length(hdp$tree$children), function(i){
        paste(trim(hdp$tree$children[[i]]$name),sep = ",")
      })
    ))
    ui.factors.update()
  })

  #
  observeEvent(input$btnRebuildTree, {
    print("btn to update factors")
    ui.tree.rebuild()
    ui.factors.update()
    #ui.evaluation.update()
    ui.evaluation.update.correct()
  })
  
  #When alternatives are added, put them on the screen under the tree
  observe({
    altSlplitter <- unlist(strsplit(input$txtAlternatives, ","))
    output$uiDynaAlternatives <- renderUI({
      alternativeList <- lapply(1:length(altSlplitter),function(i){
        #TODO need to style this better...
        span(altSlplitter[i],class="btn btn-success")
      })
      hdp$alternatives <- alternativeList
      do.call(shiny::tagList,alternativeList)
    })
  })
  
  #once model is designed, save it to the DB
  observeEvent(input$btnSaveModel, {
    saveData(ToDataFrameTree(hdp$tree, "pathString"))
  })
  
  #TODO duplicate code, need to clean this up...
  observe({
    output$xx=renderGrViz({
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(hdp$tree)),engine = "dot")
    })
  })
  

  #################################################
  # Build out UI for expert evaluation
  #################################################
  ui.evaluation.update.correct <- function() {
    #first convert the tree to data frame for matrix operations
    dfLevels <- ToDataFrameNetwork(hdp$tree, "level", "name")
    
    #Get the criteria first
    dfCriteria <- dfLevels[dfLevels$level == 2,c("from","to","level","name")]
    criteriaCombos <- expand.grid.unique(dfCriteria$name, dfCriteria$name)
    hdp$criteriaCombos <- criteriaCombos
    #TODO this should be the symetrical matrix
    output$tblCriteriaCombinations <- renderTable(criteriaCombos)
    #build the critiera sliders
    output$uiEvaluateCriteria <- renderUI({
      sliders <- lapply(1:nrow(criteriaCombos), function(i) {
        fluidRow(
          column(1, 
                 span(criteriaCombos[i,1]),
                 uiOutput(paste0("uiOutputValueCA_",i))
          ),
          column(5,
                 sliderInput(paste0("slider_",i),"",value = 50, min = 0, max = 100)
          ), 
          column(1,
                 span(criteriaCombos[i,2]),                 
                 uiOutput(paste0("uiOutputValueCB_",i))
          )
        )
      })
      do.call(shiny::tagList,sliders)
    })
    
    #add observers to the critiera sliders
    lapply(1:nrow(hdp$criteriaCombos), function(i) {
      observeEvent(input[[paste0("slider_",i)]], {
        output[[paste0("uiOutputValueCA_",i)]] <- renderUI({
          span(input[[paste0("slider_",i)]])
        })
        output[[paste0("uiOutputValueCB_",i)]] <- renderUI({
          span(100 - input[[paste0("slider_",i)]])
        })
      })
    })
    
    ################################################
    # build the sliders for each feature group
    print("-----feature time----")
    dfFeatures <- dfLevels[dfLevels$level == 3,c("from","to","level","name")]
    print("----FEATURES")
    print(dfFeatures)
    output$uiEvaluateFeatures <- renderUI({
      criteriaApply <- lapply(1:nrow(dfCriteria), function(s){
        cNode <- dfCriteria[[s,"to"]]
        dfCriteriaFeatures <- dfFeatures[which(dfFeatures$from == cNode),"name"]
        featureCombos <- expand.grid.unique(dfCriteriaFeatures, dfCriteriaFeatures)
        featureHeader <- h4(cNode)
        featureSliders <- lapply(1:nrow(featureCombos), function(f) {
            fluidRow(
              column(1, 
                     span(featureCombos[f,1]),
                     uiOutput(paste0("uiOutputValueFA_",f,s))
              ),
              column(5,
                     sliderInput(paste0("sliderf_",f,s),"",value = 50, min = 0, max = 100)
              ), 
              column(1,
                     span(featureCombos[f,2]),
                     uiOutput(paste0("uiOutputValueFB_",f,s))
              )
            )
        })
        do.call(shiny::tagList,tagList(featureHeader,featureSliders))
      })
    })

    #TODO fix duplicate code...
    #add observers 
    lapply(1:nrow(dfCriteria), function(h){
      cNode <- dfCriteria[[h,"to"]]
      dfCriteriaFeatures <- dfFeatures[which(dfFeatures$from == cNode),"name"]
      featureCombos <- expand.grid.unique(dfCriteriaFeatures, dfCriteriaFeatures)
      
      lapply(1:nrow(featureCombos), function(g) {
        observeEvent(input[[paste0("sliderf_",g,h)]], {
          output[[paste0("uiOutputValueFA_",g,h)]] <- renderUI({
            span(input[[paste0("sliderf_",g,h)]])
          })
           output[[paste0("uiOutputValueFB_",g,h)]] <- renderUI({
           span(100 - input[[paste0("sliderf_",g,h)]])
          })
        })
      })
    })

    ################################################
    # build the sliders for alternatives for each feature
    alternativeCombos <- expand.grid.unique(hdp$alternatives, hdp$alternatives)
    print(alternativeCombos)
    
    output$uiEvaluateAlternatives <- renderUI({
      featureBuilder <- lapply(1:nrow(dfFeatures), function(b) {
        alternativeHeader <- h4(dfFeatures[[b,"name"]])
        alternativeSliders <- lapply(1:nrow(alternativeCombos),function(a){
          fluidRow(
            column(1, 
                   span(alternativeCombos[a,1]),
                   uiOutput(paste0("uiOutputValueAA_",b,a))
            ),
            column(5,
                   sliderInput(paste0("slidera_",b,a),"",value = 50, min = 0, max = 100)
            ), 
            column(1,
                   span(alternativeCombos[a,2]),
                   uiOutput(paste0("uiOutputValueAB_",b,a))
            )
          )
        })
        do.call(shiny::tagList,tagList(alternativeHeader,alternativeSliders))
      })
    })
    
    ######################################################
    #Observers for alternative sliders
    #TODO fix duplicate code
    lapply(1:nrow(dfFeatures), function(a){
      lapply(1:nrow(alternativeCombos),function(b){
        observeEvent(input[[paste0("slidera_",a,b)]], {
          output[[paste0("uiOutputValueAA_",a,b)]] <- renderUI({
            span(input[[paste0("slidera_",a,b)]])
          })
          output[[paste0("uiOutputValueAB_",a,b)]] <- renderUI({
            span(100 - input[[paste0("slidera_",a,b)]])
          })
        })
      })
    })
  }

  #turn the tree into the evaluation form
  ui.evaluation.update.incorrect <- function() {
    #make the tree a data frame
    dfLevels <- ToDataFrameTree(hdp$tree, "level", "name")
    #break the levels out
    dfCriteria <- dfLevels[dfLevels$level == 2,c("level","name")]
    dfFeatures <- dfLevels[dfLevels$level == 3,c("level","name")]
    
    hdp$factors <- dfFeatures
    #create unique combinations
    criteriaCombos <- expand.grid.unique(dfCriteria$name, dfCriteria$name)
    featureCombos <- expand.grid.unique(dfFeatures$name, dfFeatures$name)
    
    hdp$criteriaCombos <- criteriaCombos
    hdp$featureCombos <- featureCombos
    
    #show them on the page
    #TODO would be great to estimate how long evaluations will take so we can warn the user
    # - how likely people will be to take the evaluation
    output$tblCriteriaCombinations <- renderTable(criteriaCombos)
    output$tblFeatureCombinations <- renderTable(featureCombos)

      #add the sliders to the page
    #TODO this should be broken down by critieria node, not all features together
    #TODO each feature should actually compare the alternatives togehter
      output$uiEvaluateFeatures <- renderUI({
      moreSliders <- lapply(1:nrow(featureCombos), function(i) {
        fluidRow(
          column(1, 
                 span(featureCombos[i,1]),
                 uiOutput(paste0("uiOutputValueFA_",i))
          ),
          column(5,
                 sliderInput(paste0("sliderf_",i),"",value = 50, min = 0, max = 100)
          ), 
          column(1,
                 span(featureCombos[i,2]),
                 uiOutput(paste0("uiOutputValueFB_",i))
          )
        )
      })
      do.call(shiny::tagList,moreSliders)
    })  
    
    output$uiEvaluateCriteria <- renderUI({
      sliders <- lapply(1:nrow(criteriaCombos), function(i) {
        fluidRow(
          column(1, 
                 span(criteriaCombos[i,1]),
                 uiOutput(paste0("uiOutputValueCA_",i))
                 ),
          column(5,
                 sliderInput(paste0("slider_",i),"",value = 50, min = 0, max = 100)
          ), 
          column(1,
            span(criteriaCombos[i,2]),                 
            uiOutput(paste0("uiOutputValueCB_",i))
          )
        )
      })
      do.call(shiny::tagList,sliders)
    })
    
    #add observers to all the sliders
    lapply(1:nrow(hdp$criteriaCombos), function(i) {
      observeEvent(input[[paste0("slider_",i)]], {
        output[[paste0("uiOutputValueCA_",i)]] <- renderUI({
          span(input[[paste0("slider_",i)]])
        })
        output[[paste0("uiOutputValueCB_",i)]] <- renderUI({
          span(100 - input[[paste0("slider_",i)]])
        })
      })
    })
    
    lapply(1:nrow(hdp$featureCombos), function(i) {
      observeEvent(input[[paste0("sliderf_",i)]], {
        output[[paste0("uiOutputValueFA_",i)]] <- renderUI({
          span(input[[paste0("sliderf_",i)]])
        })
        output[[paste0("uiOutputValueFB_",i)]] <- renderUI({
          span(100 - input[[paste0("sliderf_",i)]])
        })
      })
    })
  }

  #Get the values from dynamic sliders and save them
  observeEvent(input$btnSaveOpinion, {
    #### get the weights from the dynamic sliders
    #criteria
    dfCriteria <- split(hdp$criteriaCombos,rep(1:nrow(hdp$criteriaCombos),1))
    #df1 <- data.frame("A" = c(80),"B" = c(20))
    criteriaDfList <- lapply(1:nrow(hdp$criteriaCombos), function(i) {
      dfOut <- data.frame(streOne = c(input[[paste0("slider_",i)]]), streTwo = c(100 - input[[paste0("slider_",i)]]))
      colnames(dfOut) <- c(dfCriteria[[i]][[1]], dfCriteria[[i]][[2]])
      return(dfOut)
    })

    #features
    dfFeatures <- split(hdp$featureCombos,rep(1:nrow(hdp$featureCombos),1))
    featureDfList <- lapply(1:nrow(hdp$featureCombos), function(i) {
      dfOut <- data.frame(streOne = c(input[[paste0("sliderf_",i)]]), streTwo = c(100 - input[[paste0("sliderf_",i)]]))
      colnames(dfOut) <- c(dfFeatures[[i]][[1]], dfFeatures[[i]][[2]])
      return(dfOut)
    })
    
    print(featureDfList)
    
    dynaMatrix <- matrix(nrow = length(hdp$factors), ncol = length(hdp$factors))
    dimnames(dynaMatrix) = list(hdp$factors,hdp$factors)
    print(dynaMatrix)
    print(hdp$factors)
    #TODO either debug or print these values...
    #dMatrix <- lapply(1:length(featureDfList), function(i){
      #dynaMatrix[as.character(colnames(featureDfList[[i]])[2]),as.character(colnames(featureDfList[[i]])[1])] <- as.double(featureDfList[[i]][[1,1]])
      #dynaMatrix[as.character(colnames(featureDfList[[i]])[1]),as.character(colnames(featureDfList[[i]])[2])] <- as.double(featureDfList[[i]][[1,2]])
      #return(dynaMatrix)
      #print(colnames(featureDfList[[i]])[2])
      #print(featureDfList[[i]][[1,1]])
      #print(featureDfList[[i]][[1,2]])
    #})
    
    #print(dMatrix)
    
    saveData(ToDataFrameTree(hdp$tree, "level", "name"))
    
    #dynaMatrix[as.character(colnames(df1)[2]),as.character(colnames(df1)[1])] <- as.double(df1[1][1])
    #dynaMatrix[as.character(colnames(df1)[1]),as.character(colnames(df1)[2])] <- as.double(df1[2][1])
    

    
    #TODO loop through dataframe list, create matrix like this, df1 would be the element of the loop:
    #dynaMatrix[as.character(colnames(df1)[2]),as.character(colnames(df1)[1])] <- as.double(df1[1][1])
    #dynaMatrix[as.character(colnames(df1)[1]),as.character(colnames(df1)[2])] <- as.double(df1[2][1])
    
    #TODO save the weights
    # - this will have to get dynamic values from sliders and save them
    #https://daattali.com/shiny/persistent-data-storage/
    # use this for mongoDB: https://mlab.com/plans/pricing/
    # HDPM0ng0DB!
  })
  
  #################################################
  # Utilities
  #################################################
  # returns string w/o leading or trailing whitespace
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  #create a unique comparison matrix
  expand.grid.unique <- function(x, y, include.equals=FALSE)
  {
    x <- unique(x)
    y <- unique(y)
    g <- function(i)
    {
      z <- setdiff(y, x[seq_len(i-include.equals)])
      if(length(z)) cbind(x[i], z, deparse.level=0)
    }
    do.call(rbind, lapply(seq_along(x), g))
  }
  
  #################################################
  # -> mongo stuff
  #################################################
  options(mongodb = list(
    "host" = "localhost:27017",
    "username" = "dev",
    "password" = "Password1"
  ))
  databaseName <- "hdp"
  collectionName <- "models"
  
  saveData <- function(data) {
    # Connect to the database
    db <- mongo(collection = collectionName,
                url = sprintf(
                  "mongodb://%s:%s@%s/%s",
                  options()$mongodb$username,
                  options()$mongodb$password,
                  options()$mongodb$host,
                  databaseName))
    # Insert the data into the mongo collection as a data.frame
    data <- as.data.frame(t(data))
    db$insert(data)
  }
  
  loadData <- function() {
    # Connect to the database
    db <- mongo(collection = collectionName,
                url = sprintf(
                  "mongodb://%s:%s@%s/%s",
                  options()$mongodb$username,
                  options()$mongodb$password,
                  options()$mongodb$host,
                  databaseName))
    # Read all the entries
    data <- db$find()
    data
  }
  #################################################
  # END mongo stuff
  #################################################

}

# Run the application 
shinyApp(ui = ui, server = server)

