#################################################
# Utilities
# utility functions
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

#ugh, this is a hack!!!
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

#hack to fix to and from containing entire pathString
getLastElementInPath <- function(val) {
  splits <- unlist(strsplit(val, "/"))
  retVal <- splits[length(splits)]
  retVal
}

#################################################
# end Utilities
#################################################
