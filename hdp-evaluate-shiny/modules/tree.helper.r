#Tree functions to make working with trees easier

#source("./utilities.r",local=T)

#get all the nodes at a specified level in the tree
#note this doesn't work for level 1, but you don't really need it for that
getNodesAtLevel <- function(tree, level) {
  dfLevels <- ToDataFrameTree(tree, "level", "name")
  vals <- dfLevels[dfLevels$level == level,c("level","name")]
}

#return the names of nodes in a vector for the defined level
getNodeNamesAtLevel <- function(tree, level) {
  dfLevels <- ToDataFrameTree(tree, "level", "name")
  vals <- dfLevels[dfLevels$level == level,c("level","name")]
  eval(parse(text = vals))
}

addTreeBranches <- function(tree, level) {
  if(length(currentNode$children == 0)) {
    print(paste0("0 Chilandos at level ",level))
    currentNode$AddChildNode(child=Node$new(paste0("test",i))) # add child
  }
}

#get unique combinations of elements at a level in the tree
treeLevel.combos.unique <- function(level, dfLevels, tree, alternatives) {
  if(level > tree$height) {
    dfComparisons <- dfLevels[dfLevels$level == level - 1,c("from","to","level","name")]
    combos <- expand.grid.unique(dfComparisons$name, alternatives)
    combos    
  } else {
    dfComparisons <- dfLevels[dfLevels$level == level,c("from","to","level","name")]
    combos <- expand.grid.unique(dfComparisons$name, dfComparisons$name)
    combos    
  }
}
