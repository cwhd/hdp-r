##########################################################
# Functions to make working with trees easier
##########################################################

#' Get all the nodes at a specified level in the tree
#'
#' Given a tree and a desired level, returns all nodes at the
#' specified level. This is useful for interface building purposes but
#' not a normal tree traversal operation.
#'
#' @param tree the tree you want to get data from
#' @param level the level of the tree you want to get
#' @export
getNodesAtLevel <- function(tree, level) {
  dfLevels <- ToDataFrameTree(tree, "level", "name")
  vals <- dfLevels[dfLevels$level == level,c("level","name")]
}

#' Return the names of nodes in a vector for the defined level
#'
#' Similar to getNodesAtLevel, but will only return the names. Also
#' useful for building operations.
#'
#' @param tree the tree you want to get data from
#' @param level the level of the tree you want to get
#' @export
getNodeNamesAtLevel <- function(tree, level) {
  dfLevels <- ToDataFrameTree(tree, "level", "name")
  vals <- dfLevels[dfLevels$level == level,c("level","name")]
  eval(parse(text = vals))
}

#' Get unique combinations for children of a node.
#' If there are no children compare alternatives
#'
#' @param node the node whos children you want to compare
#' @param alternatives OPTIONAL if you're at the end of the tree and
#' this node has no children, compare alternatives
#' @export
getUniqueChildCombinations <- function(node, alternatives) { #node.combos.unique
  if(length(node$children) > 0) {
    children <- lapply(1:length(node$children), function(i){
      node$children[[i]]$name
    })
    combos <- grid.unique(children, children)
    combos
  } else {
    combos <- grid.unique(alternatives, alternatives)
    combos
  }
}

#' Get the names of the nodes in a tree
#'
#' Useful function used recursively to get a list of node names from a tree.
#' This somehow isn't default functionality from data.tree and is useful for UI
#' development.
#'
#' @example nodeNamesHack <- hdp$tree$Get(getNodeName)
#'
#' @param node the node to get name of; meant to use recursively through Get method on tree
#' @export
getNodeName <- function(node) {
  node$name
}

#' Get an example tree to use as a model
#'
#' This will return an example tree that can be loaded as a model
#' @export
getExampleTree <- function() {
  defaultTree <- Node$new("What to eat for breakfast")
  taste <- defaultTree$AddChild("Taste")
  speed <- defaultTree$AddChild("Speed")
  salty <- taste$AddChild("Salty")
  sweet <- taste$AddChild("Sweet")
  fast <- speed$AddChild("Fast")
  slow <- speed$AddChild("Slow")

  defaultTree
}

#'Create a unique comparison matrix
#'
#'This function will take in 2 lists and return the unique set of combinations
#'between them. This is necessary in HDM when you need to create the pairwise
#'comparisons based on children of a node in the hierarchy. This is only used
#'internally by getUniqueChildCombinations.
#'
#'@param x first list to compare
#'@param y second list to compare
#'@param include.equals not sure if we even need this anymore
grid.unique <- function(x, y, include.equals=FALSE)
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
