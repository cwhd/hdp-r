#################################################
# Functions that generate parts of our shiny app
#################################################

#generate text inputs in a tab panel for a level of the tree
ui.level.textInput.generate <- function(level, tree) {
  #print("ui.leveltextInput.generate")
  nodesAtLevel <- getNodeNamesAtLevel(tree, level)
  
  textBoxes <- lapply(1:length(nodesAtLevel),function(i){   #for each node at the current level
    #add a node to the tree for the new text input
    if(length(nodesAtLevel > 0)) {
      currentNode <- FindNode(node=tree,name = nodesAtLevel[[i]])
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

ui.sliders.generate <- function(level, dfLevels, tree, alternatives) {
  combos <- treeLevel.combos.unique(level, dfLevels, tree, alternatives)
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
  
  taby <- tabPanel(paste0("Level ",level-1), sliders)
  taby
}

