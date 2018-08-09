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
          uiOutput("uiDynaFactors"),
          actionButton("btnUpdateTree", "Update Tree")
        ),
        wellPanel(
          h4("Alternatives"),
          textInput("txtAlternatives", "Alternatives", placeholder = "i.e.) alternative 1, 2, etc")
        )
      ),

      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Main Tab",
                   wellPanel(
                     h3("Decision Tree"),
                     actionButton("btnSaveModel", "Save Model"),
                     actionButton("btnExpertEvaluation", "Expert Evaluation")
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
                                   uiOutput("uiTester"),
                                   h3('Evaluate Criteria'),
                                   uiOutput("uiEvaluateCriteria"),
                                   h3("Evaluate Features"),
                                   uiOutput("uiEvaluateFeatures")
                      )),
                   actionButton("btnSaveOpinion", "Save Your Stuff")
              )
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #TODO - build an example tree
  tree <- Node$new("Choose Breakfast")
  taste <- tree$AddChild("Taste")
  speed <- tree$AddChild("Speed")
  salty <- taste$AddChild("Salty")
  sweet <- taste$AddChild("Sweet")
  fast <- speed$AddChild("Fast")
  slow <- speed$AddChild("Slow")

  #TOOO - update the code so tree and page inputs reflect each other

  hdp=reactiveValues(tree=NULL,criteria=NULL,factors=NULL,alternatives=NULL,criteriaCombos=NULL,featureCombos=NULL)
  #TODO one function to build the tree

  #Update the title node when changed
  observe({
    hdp$tree <- Node$new(input$txtDecison)

    #add nodes to tree for each criteria
    hdp$criteria <- unlist(strsplit(input$txtCriteria, ","))
    for(v in hdp$criteria) {
      hdp$tree$AddChildNode(child=Node$new(v))
    }
    
  })
  
  #create a text input for factors for each criteria
  observe({
      output$uiDynaFactors <- renderUI({
        lapply(1:length(unlist(strsplit(input$txtCriteria, ","))),function(i){ 
          textInput(paste0('criteraFeature_',hdp$criteria[i]), hdp$criteria[i], placeholder = "i.e. factor1, factor2")
        })
      })
  })
  
  #update the tree with features
  observeEvent(input$btnUpdateTree, {
    for(v in hdp$criteria) {
      parentNode <- paste0('criteraFeature_',v)
      fSplit <- unlist(strsplit(input[[paste0('criteraFeature_',v)]],","))
      for(n in fSplit) {
        FindNode(node=hdp$tree,name = v)$AddChildNode(child=Node$new(n)) # add child
      }
   }
    #render the tree when it updates
    output$xx=renderGrViz({
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(hdp$tree)),engine = "dot")
    })
  })
  
  #When alternatives are added, put them on the screen under the tree
  observe({
    altSlplitter <- unlist(strsplit(input$txtAlternatives, ","))
    output$uiDynaAlternatives <- renderUI({
      # Create rows for each criteria!
      rows <- lapply(1:length(altSlplitter),function(i){
        #add one panel for everything, a header and a text box for each factor
            span(altSlplitter[i])
      })
      do.call(shiny::tagList,rows)
    })
  })

  #turn the tree into the evaluation form
  observeEvent(input$btnExpertEvaluation, {
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

  })
  
  observeEvent(input$btnSaveModel, {
    #TODO save the model, use this as an example:
    #https://daattali.com/shiny/persistent-data-storage/
    
   
  })
  
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

  #TODO duplicate code, need to clean this up...
  observe({
    output$xx=renderGrViz({
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(hdp$tree)),engine = "dot")
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

