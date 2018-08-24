#################################################
# Matrix stuff, this will move to the HDP-R module
#################################################

matrix.buildFromComboFrames <- function(names,comboFrames) {
  A <- matrix(, 
              nrow = length(names), 
              ncol = length(names), 
              dimnames = list(names,names))
  diag(A) <- 1
  for(df in comboFrames) {
    print(paste0("--------colnames:",colnames(df)[1],"-",colnames(df)[2]))
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
    print(paste0("--------colnames:",colnames(df)[1],"-",colnames(df)[2]))
    print(paste0("A stuff:",A[colnames(df)[1],colnames(df)[2]]))
    A[colnames(df)[1],colnames(df)[2]] <- df[[1,1]]
    #A[colnames(df)[2],colnames(df)[1]] <- df[[1,2]]
  }
  A
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
  
  #aOut <- print(xtable(A, align=rep("c", ncol(A)+1)), 
  #              floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
  #bOut <- print(xtable(B, align=rep("c", ncol(B)+1)), 
  #              floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
}
