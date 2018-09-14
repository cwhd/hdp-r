#################################################
# Matrix calculations
#################################################

#' Calculate the normalized weights for each node and add them to the tree
#'
#' This function will take a defined model and corresponding set of
#' comboFrames and add normalized and weighted values to the tree
#'
#' @param tree the tree representing the model to calculate
#' @param comboFrames a list of dataframes with comparative values corresponding
#' to the tree
#' @export
calculateHDMWeights <- function(tree, comboFrames) {

  tree$norm <- 1
  tree$weight <- 1
  tree$inconsistency <- 1

  tree$Do(function(node) {
    parent <- node$parent
    #build the comparison frames into a matrix
    matrixColumns <- lapply(1:length(parent$children), function(i){
      parent$children[[i]]$name
    })
    populatedMatrixAB <- matrix.buildFromComboFrames(matrixColumns,comboFrames[[node$name]])

    node$norm <-  normalizeValueForNode(node, populatedMatrixAB)
    node$weight <-  finalizeWeightsForNode(node)
    node$inconsistency <- inconsistency.calculate(populatedMatrixAB)
  }, filterFun = isNotRoot)

  tree
}

#' Calculate weight for requested node
#'
#' Designed to be used recursively in the tree, this function will return
#' the normalized value for a node.
#'
#' @param currentNode the node to operate on
#' @param populatedMatrix the populated matrix of comboFrames
normalizeValueForNode <- function(currentNode, populatedMatrix) {
  calculatedMatrix <- matrix.calculate(populatedMatrix)
  #return(calculatedMatrix[[currentNode$name,2]])
  return(round(calculatedMatrix[[currentNode$name,2]], 4)) #round to 4 digits
}

#' Calculate the weighted value for each node
#'
#' As the final step in the calculation return the finalized weight of each node.
#'
#' @param node the node to operate on
finalizeWeightsForNode <- function(node) {
  parent.norm <- node$parent$norm
  weight <- node$norm * parent.norm
  return(round(weight, 4)) #round to 4 digits
}

#' Given a set of names and dataframes with evaluation data,
#' build out the first matrix to operate on
#'
#' This is only used internally as the first and second step in the calculation
#'
#' @param names the names of the elements being compared in a pairwise manner
#' @param comboFrames the frames to transform into a matrix
matrix.buildFromComboFrames <- function(names,comboFrames) {
  A <- matrix(,
              nrow = length(names),
              ncol = length(names),
              dimnames = list(names,names))
  diag(A) <- 1
  for(df in comboFrames) {
    A[colnames(df)[1],colnames(df)[2]] <- df[[1,1]]
    A[colnames(df)[2],colnames(df)[1]] <- df[[1,2]]
  }
  B <- t(A) / A
  diag(B) <- 1
  B
}

#' Calculate inconsistency
#'
#' Helps determine how reliable this expert is
#'
#' @param B the matrix after it's gone through the first 2 steps of the calculation
inconsistency.calculate <- function(B){
  B.norm <- sweep(B,2,colSums(B),`/`)
  nMeans <- rowMeans(B.norm)
  nSd <- apply(B.norm,1,sd)
  nVar <- apply(B.norm,1,var)
  inconsistency <- sqrt(sum(nVar) * .25)

  print("---Inconsistency")
  print(inconsistency)

  return(round(inconsistency, 4)) #round to 4 digits
}

#' Pass in a list of experts and their final results, get back the disagreement
#'
#' First get the standard deviation across the values for each expert
#' Second get the mean across the experts
#'
#' @rdname disagreement.calculate
#'
#' @param expertsFinalResults a matrix with the experts and the weights at the bottom
#' of the tree
disagreement.calculate <- function(expertsFinalResults) {
  stepOne <- apply(expertsFinalResults,1,sd)
  stepTwo <- mean(stepOne)
  print("Step 2")
  print(round(stepTwo,4))
  return(round(stepTwo,4)) #round to 4 digits
}

#' Given a populated matrix, divide the comparisons against
#' each other to get relative weights
#'
#' This is only used internally as the final part of the calculation. The first
#' 2 steps are used to them get the normalized value for the nodes
#'
#' @param B the matrix to operate on
matrix.calculate <- function(B) {
  #divide col1 by col2, col2 by col3, etc to create Matrix C
  C <- matrix(ncol = ncol(B)-1, nrow = nrow(B))
  for(c in 1:ncol(C)) {
    C[,c] <- B[,c] / B[,c+1]
  }
  cMeans <- colMeans(C)

  #final calculation
  matrix.final <- matrix(ncol = 2, nrow = nrow(B), dimnames = list(rev(colnames(B)), list("Raw","Normalized")))
  #matrix.final[nrow(matrix.final),1] <- 1
  matrix.final[1,1] <- 1
  cMeans.reverse <- rev(cMeans)
  for(c in 2:nrow(matrix.final)) {
    matrix.final[c, 1] <- matrix.final[c-1,1] * cMeans.reverse[c-1]
  }

  matrix.final.raw.sum <- sum(matrix.final[,1])
  matrix.final[,2] <- matrix.final[,1] / matrix.final.raw.sum

  should.be.one <- sum(matrix.final[,2])

  matrix.final
}
