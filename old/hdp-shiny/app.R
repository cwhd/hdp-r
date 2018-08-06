# HDP - 1
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# - Add choices input boxes, each time you add one add a new box
# - add levels, hit plus button and add a new level
# - add alternatives, add one and add a new box
# - build heirarchy diagram in another panel
# - save model into format that's easy to import

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("HDP Shiny Tool"),
   
   textInput("txtObjective", "Objective", placeholder = "pick something..."),
   textInput("txtCriteria", "Criteria", placeholder = "color, memory, whatever..."),
   uiOutput('uiFactors'),

   actionButton("btnAddFactors", "Add Factors"),

   actionButton("btnCreateModel", "Create Model"),
   
   # Sidebar with dynamic text boxes 
   sidebarLayout(
     sidebarPanel(width=4,
                  fluidRow(column(12,
                                  h3('Features'),
                                  uiOutput('uiOutpt')
                  )), # END fluidRow
                  fluidRow(
                    column(4,div()),
                    column(4,actionButton("add", "Add!")),
                    column(4,actionButton('goButton',"Analyze"))
                  ) # END fluidRow
     ), # END sidebarPanel
     mainPanel(
       verbatimTextOutput("nText"),
       textOutput("text2"),
       tableOutput('tbl')
     )
     
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #HERE IS HOW THIS WORKS:
  # - hdp.factors is a reactive value, with redered as a dynamic propery
  # - rendered is just a list of numbers, it starts with 1
  # - when input add is clicked, it adds the next number value to the list 
  # - when redered is updated, we use lapply to create a list of inputs and add them to the page
  
  
   #### This is my attempt at dynamicing everything
   hdp.criteria <- reactiveValues(rendered=c(1))
   hdp.factors <- reactiveValues(rendered=c(1))
   hdp.alternatives <- reactiveValues(rendered=c(1))
   
   ####HERE IS WHAT I NEED TO DO
   # - when button is clicked, update the vector
   # - observe the vector, when it changes update the UI
   
   # Increment reactive values used to store how may rows we have rendered
   # when we add factors, make sure we keep track of the input number
   #TODO this totally doesn't work
   observeEvent(input$add,{
     hdp.factors$renderd <- c(hdp.factors$renderd, max(hdp.factors$renderd)+1)
   })
   
   observe({
     output$uiOutpt <- renderUI({
       # Create rows for each criteria!
       rows <- lapply(hdp.factors$renderd,function(i){
         #add one panel for everything, a header and a text box for each factor
         fluidRow(
           column(6,  h3(paste0('Feature',i))),   
           column(6,  textInput(paste0('criteraFeature_',i), "Feature something"))
         )
       })
       do.call(shiny::tagList,rows)
       
     })
   })
   
   
      observeEvent(input$btnAddFactors, {
        output$uiFactors <- renderUI({
          textInput("dynamic1", "Dynamic 1",
                    value = "starting value")
        })
    })
   
   
   ######Dynamic code from example...
   
   #create an object for storing reactive values
   features <- reactiveValues(renderd=c(1))
   
   #this is used for the list of inputs below, 
   ntext <- eventReactive(input$goButton, {
     out <- lapply(features$renderd,function(i){
       fv <- paste0('numInp_',i)
       vn <- paste0('Feature',i)
       # Get input values by name
       sprintf( 'Variable: %s, Value: %5.3f',input[[vn]],input[[fv]] )
     })
     do.call(paste,c(out,sep="\n"))
   })
   
   #this is the data frame of dynamic values, used as output for display below
   df <- eventReactive(input$goButton, {
     out <- lapply(features$renderd,function(i){
       fv <- paste0('numInp_',i)
       vn <- paste0('Feature',i)
       data.frame(Variable=input[[vn]], Value=input[[fv]] )
     })
     do.call(rbind,out)
   })
   
   output$nText <- renderText({
     ntext()
   })
   output$text2 <- renderText({ 
     sprintf("You have selected feature: %s", paste(features$renderd,collapse=", "))
   })
   
   output$tbl <- renderTable({
     df()
   })
   
   # Increment reactive values used to store how may rows we have rendered
   observeEvent(input$add,{
     if (max(features$renderd) > 2) return(NULL)
     features$renderd <- c(features$renderd, max(features$renderd)+1)
   })
   
   # If reactive vector updated we render the UI again
   observe({
     output$uiOutpt <- renderUI({
       # Create rows
       rows <- lapply(features$renderd,function(i){
         fluidRow(
           column(6,  selectInput(paste0('Feature',i), 
                                  label = "", 
                                  choices = list("Feature1","Feature2","Feature3"), 
                                  selected = paste0('Feature',i))),   
           column(6,  numericInput(paste0('numInp_',i), label="",min = 0, max = 100, value = runif(1,max=100)))
         )
       })
       do.call(shiny::tagList,rows)
       
     })
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

