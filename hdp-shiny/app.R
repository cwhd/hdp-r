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
          textInput("txtCriteria","Criteria", placeholder = "i.e.) criteria1,2, etc", value = "yip,yop")
        ),
        wellPanel(
          h4("Factors"),
          uiOutput("uiDynaFactors")
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
                     actionButton("btnUpdateTree", "Update Tree"),
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
                   fluidRow(column(7,
                                   h3('Evaluate Criteria'),
                                   uiOutput("uiEvaluateCriteria"),
                                   h3("Evaluate Features"),
                                   uiOutput("uiEvaluateFeatures")
                      )) # END fluidRow
              )
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  hdp=reactiveValues(tree=NULL,names=NULL,criteria=NULL,factors=NULL,alternatives=NULL,rendered=c(1))
  #TODO combine this whole thing together
  
  #Update the title node when changed
  observe({
    hdp$tree <- Node$new(input$txtDecison)
    
    #TODO this should probably not run until there are values in the tree...
    nodeSplitter <- unlist(strsplit(input$txtCriteria, ",")) #TODO should be a better name
    hdp$factors <- nodeSplitter
    for(v in nodeSplitter) {
      hdp$tree$AddChildNode(child=Node$new(v))
    }
    
    #TODO do I use this anywhere???
    hdp$names <- hdp$tree$Get('name')
    
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
  
  #this updates the whole tree when the button is clicked...probably better to not have this as a button
  observeEvent(input$btnUpdateTree, {
    for(v in hdp$factors) {
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
  

  #This wil update the textInputs for Factors
  observe({
    renderList <- 1:length(unlist(strsplit(input$txtCriteria, ",")))
    output$uiDynaFactors <- renderUI({
      lapply(renderList,function(i){ 
        #TODO if the node already exists, keep the values
        textInput(paste0('criteraFeature_',hdp$factors[i]), hdp$factors[i], placeholder = "i.e. factor1, factor2", value = "one,two")
      })
    })
  })
  
  observeEvent(input$btnExpertEvaluation, {
    #uiEvaluateCriteria needs to get updated with sliders for each comparison
    #TODO turn the combos into slider controls
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
    #create unique combinations
    criteriaCombos <- expand.grid.unique(dfCriteria$name, dfCriteria$name)
    featureCombos <- expand.grid.unique(dfFeatures$name, dfFeatures$name)
    #show them on the page
    #TODO would be great to estimate how long evaluations will take so we can warn the user
    # - how likely people will be to take the evaluation
    output$tblCriteriaCombinations <- renderTable(criteriaCombos)
    output$tblFeatureCombinations <- renderTable(featureCombos)
    # add stuff to uiEvaluateCriteria
    
    output$uiEvaluateFeatures <- renderUI({
      moreSliders <- lapply(1:nrow(featureCombos), function(i) {
        fluidRow(
          column(1, 
                 span(featureCombos[i,1])
          ),
          column(5,
                 sliderInput(paste0("sliderf_",i),"",value = 50, min = 0, max = 100)
          ), 
          column(1,
                 span(featureCombos[i,2])
          )
        )
      })
      do.call(shiny::tagList,moreSliders)
    })  
    
    
    output$uiEvaluateCriteria <- renderUI({
      sliders <- lapply(1:nrow(criteriaCombos), function(i) {
        fluidRow(
          column(1, 
                 span(criteriaCombos[i,1])
                 ),
          column(5,
                 sliderInput(paste0("slider_",i),"",value = 50, min = 0, max = 100)
          ), 
          column(1,
            span(criteriaCombos[i,2])
          )
        )
      })
      do.call(shiny::tagList,sliders)
      
      
    })
  })
  
  observeEvent(input$btnSaveModel, {
    #TODO save the model, use this as an example:
    #https://daattali.com/shiny/persistent-data-storage/
  })
  
  #TODO duplicate code, need to clean this up...
  output$xx=renderGrViz({
    grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(hdp$tree)),engine = "dot")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

