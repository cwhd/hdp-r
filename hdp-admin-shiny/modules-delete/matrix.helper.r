#################################################
# Matrix stuff, this will move to the HDP-R module
#################################################
#build the combo matrixes
matrix.buildFromComboFrames <- function(names,comboFrames) {
  A <- matrix(, 
              nrow = length(names), 
              ncol = length(names), 
              dimnames = list(names,names))
  diag(A) <- 1
  for(df in comboFrames) {
    #print(paste0("--------colnames:",colnames(df)[1],"-",colnames(df)[2]))
    A[colnames(df)[1],colnames(df)[2]] <- df[[1,1]]
    A[colnames(df)[2],colnames(df)[1]] <- df[[1,2]]
  }
  A
}

matrix.alternativesVsFeatures <- function(rnames,cnames,comboFrames) {
  A <- matrix(, 
              nrow = length(rnames), 
              ncol = length(cnames), 
              dimnames = list(rnames,cnames))
  print(A)
  for(df in comboFrames) {
    #print(paste0("--------colnames:",colnames(df)[1],"-",colnames(df)[2]))
    #print(paste0("A stuff:",A[colnames(df)[1],colnames(df)[2]]))
    A[colnames(df)[1],colnames(df)[2]] <- df[[1,1]]
    #A[colnames(df)[2],colnames(df)[1]] <- df[[1,2]]
  }
  A
}

matrix.factors.calculate <- function(A) {
  B <- as.vector(t(A) / A)
  #B.norm <- sweep(B,2,colSums(B),`/`)
  #nMeans <- rowMeans(B.norm)
  #nSd <- apply(B.norm,1,sd)
  #nVar <- apply(B.norm,1,var)
  #inconsistency <- sqrt(sum(nVar) * .25)
  
  B
}

matrix.calculate <- function(A) {
  #calculate everything else    
  B <- t(A) / A
  diag(B) <- 1
  B.norm <- sweep(B,2,colSums(B),`/`)
  nMeans <- rowMeans(B.norm)
  nSd <- apply(B.norm,1,sd)
  nVar <- apply(B.norm,1,var)
  inconsistency <- sqrt(sum(nVar) * .25)
  
  B.norm

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


