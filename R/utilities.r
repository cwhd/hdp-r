#############################################
# -> Utility functions to make life easier <-
#############################################

#' Returns string w/o leading or trailing whitespace
#'
#' This is a really useful function for building trees as whitespace is
#' valid in node names but messes up working with them later on.
#'
#' @param x where x is the string you want to trim.
#' @export
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#' If a node has children return them, otherwise return nothing
#'
#' Sometimes you want to either the children of a node or nothing at all. This
#' function handles that for you.
#'
#' @param currentNode the node you want to check
#' @export
childrenOrNothing <- function(currentNode) {
  if(length(currentNode$children) > 0) {
    toString(
      lapply(1:length(currentNode$children), function(j){
        if(!is.null(currentNode$children[[j]]$name)) {
          toString(currentNode$children[[j]]$name)
        } else {
          ""
        }
      })
    )
  } else {
    ""
  }
}

#'Get the last element in a path string
#'
#'For some reason sometimes data.tree will return an entire path string
#'to a node instead of just the last element we need to rebuild the tree. This
#'helps fix that by taking the last element out of the path and returning it.
#'
#'@param val the path to get the last element out of
#'@export
getLastElementInPath <- function(val) {
  splits <- unlist(strsplit(val, "/"))
  retVal <- splits[length(splits)]
  retVal
}
