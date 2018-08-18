# HDP-R
# A shiny based interface to collect data for HDP models

library(shiny)
library(data.tree)  #for managing the hierarchy
library(DiagrammeR) #display the tree
library(mongolite)  #use Mongo for storage 
library(rjson)      #gives us more flexibility for storing and loading models
library(DT)         #interface for selecting models from the DB
library(xtable)

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

  hdp=reactiveValues(tree=NULL,criteria=NULL,factors=NULL,criteriaCombos=NULL,featureCombos=NULL,
                     alternatives="Eggs, Waffles, Oatmeal",alternativeCombos=NULL,expertName=NULL,
                     loadedModels=NULL,currentModelName=NULL, treeLevels=NULL)
  hdp$tree <- tree
  hdp$treeLevels <- 3
  
  #################################################
  # Design the model
  #################################################
  
  observe(
    hdp$expertName <- input$txtExpertName
  )
  
  #TODO this builds the tree from the form, needs to be more dynamic...
  ui.tree.rebuild <- function () {
    print("rebuilding tree...")
    hdp$tree <- Node$new(input$txtDecison)

    #add nodes to tree for each criteria
    hdp$criteria <- unlist(strsplit(input$txtCriteria, ","))
    hdp$criteria <- sapply(hdp$criteria,trim)
    for(v in hdp$criteria) {
      trimV <- trim(v)
      hdp$tree$AddChildNode(child=Node$new(trimV))
      
      #TODO we know how many levels
      
      #add the feature nodes
      if(!is.null(input[[paste0('criteraFeature_',trimV)]])) {
        fSplit <- unlist(strsplit(input[[paste0('criteraFeature_',trimV)]],","))
        for(n in fSplit) {
          FindNode(node=hdp$tree,name = trimV)$AddChildNode(child=Node$new(trim(n))) # add child
        }
      }
    }
  }
  
  #add text inputs for the factors
  ui.factors.update <- function() {
    print("updating factors...")
    output$uiDynaFactors <- renderUI({
      lapply(1:length(hdp$tree$children),function(i){   #for each criteria
        textInput(paste0("criteraFeature_",hdp$tree$children[[i]]$name),
                  hdp$tree$children[[i]]$name,
                  value = 
                    toString(
                      lapply(1:length(FindNode(node=hdp$tree,name = hdp$tree$children[[i]]$name)$children), function(j){
                        if(!is.null(FindNode(node=hdp$tree,name = hdp$tree$children[[i]]$name)$children[[j]]$name)) {
                          toString(FindNode(node=hdp$tree,name = hdp$tree$children[[i]]$name)$children[[j]]$name)
                        } else {
                          toString(paste0("Factor",j))
                        }
                      })
                    )
        )
      })
    })
  }
  
  #This button will add another level to the model  
  observeEvent(input$btnAddFactorLevel, {
    #TODO add another level in the tree
    # - find the nodes at the last level
    dfLevels <- ToDataFrameTree(hdp$tree, "level", "name")
    #print(dfLevels)
    dfLastLevel <- dfLevels[dfLevels$level == hdp$treeLevels,c("level","name")]
    #print(dfLastLevel)
    #add a text input for each leaf in the previous level
    output$uiDynaLevels <- renderUI({
      lapply(1:nrow(dfLastLevel), function(i) {
        textInput(paste0("factorLevel_",hdp$treeLevels,"_",dfLastLevel[[i,"name"]]),
                  dfLastLevel[[i,"name"]],
                  value="value here...")
      })
    })
    
    #update the tree
    lapply(1:nrow(dfLastLevel),function(i) {
      print(dfLastLevel[[i,"name"]])
      FindNode(node=hdp$tree,name = dfLastLevel[[i,"name"]])$AddChildNode(child=Node$new(paste0("test",i))) # add child
    })
    print(hdp$tree)

    #update the number of levels...
    hdp$treeLevels <- hdp$treeLevels + 1
    
    #TODO add a dynamic button to remove the level
  })
  
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

  #Update the tree and the evaluation form
  observeEvent(input$btnRebuildTree, {
    hdp$currentModelName <- input$txtModelName
    
    print("btn to update factors")
    ui.tree.rebuild()
    ui.factors.update()
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
      hdp$alternatives <- altSlplitter
      do.call(shiny::tagList,alternativeList)
    })
  })
  
  #once model is designed, save it to the DB - CHECK
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
  
  #TODO can prob get rid of this
  output$modelSelectInfo = renderPrint({
    s = input$dtMongoOutput_rows_selected
    if (length(s)) {
      cat('Rows selected: ')
      cat(s, sep = ', ')
    }
  })
  
  #when a row is selected from the grid, load it onto the page - CHECK
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

    }
  })
  
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
    #print(paste0("dfCrits:",dfCriteria))
    criteriaCombos <- expand.grid.unique(dfCriteria$name, dfCriteria$name)
    hdp$criteriaCombos <- criteriaCombos
    #TODO this should be the symetrical matrix
    #output$tblCriteriaCombinations <- renderTable(criteriaCombos)
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
    dfFeatures <- dfLevels[dfLevels$level == 3,c("from","to","level","name")]
    #print("----FEATURES")
    #print(dfFeatures)
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
      #print(paste0("fcombos:",featureCombos))
      if(length(featureCombos) > 0) {
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
      }
    })

    ################################################
    # build the sliders for alternatives for each feature
    alternativeCombos <- expand.grid.unique(hdp$alternatives, hdp$alternatives)
    #print(alternativeCombos)
    
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
    #output$tblCriteriaCombinations <- renderTable(criteriaCombos)
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
  
  hdp.matrix.calulate <- function(combos) {
    #TODO move code up here for matrix calcuations
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
    
    print(criteriaDfList)
    trimmedCriteria <- sapply(hdp$criteria,trim)
    
    A <- matrix(, 
                  nrow = length(trimmedCriteria), 
                  ncol = length(trimmedCriteria), 
                  dimnames = list(trimmedCriteria,trimmedCriteria))
    diag(A) <- 1
    for(df in criteriaDfList) {
      #print(paste0("Colname1:",colnames(df)[1]," Colname2:",colnames(df)[2]," df1,1:",df[[1,1]]," df1,2:",df[[1,2]]))
      A[colnames(df)[1],colnames(df)[2]] <- df[[1,1]]
      A[colnames(df)[2],colnames(df)[1]] <- df[[1,2]]
    }
    print(A)
    
    output$tblCriteriaCombinations <- renderTable(A)
    
    #calculate everything else    
    B <- t(A) / A
    diag(B) <- 1
    B.norm <- sweep(B,2,colSums(B),`/`)
    nMeans <- rowMeans(B.norm)
    nSd <- apply(B.norm,1,sd)
    nVar <- apply(B.norm,1,var)
    inconsistency <- sqrt(sum(nVar) * .25)
    
    aOut <- print(xtable(A, align=rep("c", ncol(A)+1)), 
                         floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
    
    bOut <- print(xtable(B, align=rep("c", ncol(B)+1)), 
                  floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
    
    
    M <- print(xtable(A, align=rep("c", ncol(A)+1)), 
               floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)

    html <- paste0("$$", M, "$$")
    
    output$uiCriteriaResults <- renderUI({
      list(
        withMathJax(html),
        withMathJax(B),
        div(paste0("nMeans: ",nMeans)),
        div(paste0("nVar: ",nVar)),
        div(paste0("inconsistency: ",inconsistency)),
        A,B
      )
    })
    
    

    #features
    #dfFeatures <- split(hdp$featureCombos,rep(1:nrow(hdp$featureCombos),1))
    #featureDfList <- lapply(1:nrow(hdp$featureCombos), function(i) {
    #  dfOut <- data.frame(streOne = c(input[[paste0("sliderf_",i)]]), streTwo = c(100 - input[[paste0("sliderf_",i)]]))
    #  colnames(dfOut) <- c(dfFeatures[[i]][[1]], dfFeatures[[i]][[2]])
    #  return(dfOut)
    #})
    
    #print(featureDfList)
    
    #saveData(ToDataFrameTree(hdp$tree, "level", "name"))
    
  })

  
  #################################################
  # end Build out UI for expert evaluation
  #################################################
  
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
  # end Utilities
  #################################################

  #################################################
  # -> mongo stuff
  #################################################
  #for hosting?
  #https://daattali.com/shiny/persistent-data-storage/
  # use this for mongoDB: https://mlab.com/plans/pricing/
  # HDPM0ng0DB!
  
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
    #data <- as.data.frame(t(data))
    #print("Saving data structure...")
    #str(data)
    db$insert(data)
  }
  
  loadModel <- function(modelId) {
    # Connect to the database
    db <- mongo(collection = collectionName,
                url = sprintf(
                  "mongodb://%s:%s@%s/%s",
                  options()$mongodb$username,
                  options()$mongodb$password,
                  options()$mongodb$host,
                  databaseName))
    # get by Object Id: https://jeroen.github.io/mongolite/query-data.html#select-by-id
    data <- db$find(query = paste0('{"_id" : {"$oid":"',modelId,'"}}'))
    data
  }
  
  #load up all the models ids and names for the list
  loadAllModels <- function() {
    db <- mongo(collection = collectionName,
                url = sprintf(
                  "mongodb://%s:%s@%s/%s",
                  options()$mongodb$username,
                  options()$mongodb$password,
                  options()$mongodb$host,
                  databaseName))
    # Read all the entries
    data <- db$find(
      query = "{}",
      fields = '{ "modelName" : true }'
    )
    data
  }
  
  #################################################
  # END mongo stuff
  #################################################

}

# Run the application 
shinyApp(ui = ui, server = server)

