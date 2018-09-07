#################################################
# Functions that generate UI elements useful for
# collecting HDM data. These will generate groups
# of elements for a Shiny UI in tabPanels.
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

ui.sliders.generate.byNode <- function(node, alternatives) {
  combos <- getUniqueChildCombinations(node, alternatives)
  #TODO may need to make sure there are no spaces or special chars in the name
  #build the critiera sliders for a level in the tree
  sliders <- lapply(1:nrow(combos), function(i) {
    fluidRow(
      column(1,
             span(combos[i,1]),
             uiOutput(paste0("uiOutputValueA_",node$name,"_",i))
      ),
      column(5,
             sliderInput(paste0("slider_",node$name,"_",i),"",value = 50, min = 1, max = 99)
      ),
      column(1,
             span(combos[i,2]),
             uiOutput(paste0("uiOutputValueB_",node$name,"_",i))
      )
    )
  })

  taby <- tabPanel(paste0("Node: ",node$name), sliders)
  taby
}
