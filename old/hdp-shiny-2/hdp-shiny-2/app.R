# HDP - 2
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.tree)
library(DiagrammeR)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("HDP Shiny Tool"),
  
  mainPanel(uiOutput("add_child_ui"),
            grVizOutput("xx")   )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Create reative value to app
  vv=reactiveValues(org=NULL,names=NULL)

  #create main tree
  observe({
    vv$org <- Node$new("Parent1")
    vv$org$AddChildNode(child = Node$new("1"))
    vv$names=vv$org$Get('name') # get names of main tree
  })
  
  #when child ui changes, update text boxes
  #because vv$names is a list, this knows to display a bunch of them
  output$add_child_ui=renderUI({
    list(
      wellPanel(
        selectInput("Name_to_change","Name_to_change",vv$names),
        textInput("new_name","new_name",""),
        actionButton("Change_name","Change_name")
      ),
      wellPanel(
        selectInput("Parent_name","Parent_name",vv$names),
        textInput("new_node_name","new_node_name",""),
        actionButton("add_child","add_child")
      ))
  })
  #if a name changes, update the corresponding node based on the drop down
  observeEvent(input$Change_name,{
    
    aa=FindNode(node=vv$org,name = input$Name_to_change) 
    aa$name=input$new_name # Change name
    vv$names=vv$org$Get('name')# get names of new tree
    
    #re-generate chart
    output$xx=renderGrViz({
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
    })
  })
  
  #add a new node child node based on the DD selected
  observeEvent(input$add_child,{
    
    FindNode(node=vv$org,name = input$Parent_name)$AddChildNode(Node$new(input$new_node_name)) # add child
    vv$names=vv$org$Get('name')# get names of new tree
    
    #re-generate chart
    output$xx=renderGrViz({
      
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
    })
  })
  
  #re render the grid
  output$xx=renderGrViz({
    SetGraphStyle(vv$org, rankdir = "TB")
    SetEdgeStyle(vv$org, arrowhead = "vee", color = "grey35", penwidth = 2)
    SetNodeStyle(vv$org, style = "filled,rounded", shape = "box", fillcolor = "GreenYellow", 
                 fontname = "helvetica", tooltip = GetDefaultTooltip)
    
    grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

