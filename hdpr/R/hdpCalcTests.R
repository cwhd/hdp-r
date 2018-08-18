
abLables <- list("A","B","C","D")

#TODO the formula to compare these is in the treeTester.R file

#TODO create a matrix with the dimentions and names of the model's labels
dynaMatrix <- matrix(nrow = length(abLables), ncol = length(abLables))
dimnames(dynaMatrix) = list(abLables,abLables)

#dynaMatrix[abLables[1],abLables[2]] <- 

#TODO figure out how to populate this autmagically
df1 <- data.frame("A" = c(80),"B" = c(20))
df2 <- data.frame("A" = c(90),"C" = c(10))
df3 <- data.frame("A" = c(95), "D" = c(5))
df4 <- data.frame("B" = c(80), "C" = c(20))
df5 <- data.frame("B" = c(90), "D" = c(10))
df6 <- data.frame("C" = c(80), "D" = c(20))

dfList <- list(df1,df2,df3,df4,df5,df5,df6)

mat <- matrix(, nrow = length(abLables), ncol = length(abLables), dimnames = list(abLables,abLables))
diag(mat) <- 1
for(df in dfList) {
  #print(paste0("Colname1:",colnames(df)[1]," Colname2:",colnames(df)[2]," df1,1:",df[[1,1]]," df1,2:",df[[1,2]]))
  mat[colnames(df)[1],colnames(df)[2]] <- df[[1,1]]
  mat[colnames(df)[2],colnames(df)[1]] <- df[[1,2]]
}


#the stuff above would create "A" then code below can go away
A <- structure(c(NA,df1$B[1],df2$C[1],df3$D[1],
                        df1$A[1],NA,df4$C[1],df5$D[1],
                        df2$A[1],df4$B[1],NA,df6$D[1],
                        df3$A[1],df5$B[1],df6$C[1],NA),
                      .Dim = c(4,4),
                      .Dimnames = list(LETTERS[1:4], LETTERS[1:4]))
diag(A) <- 1

B <- t(A) / A
diag(B) <- 1
B.norm <- sweep(B,2,colSums(B),`/`)

nMeans <- rowMeans(B.norm)
nSd <- apply(B.norm,1,sd)
nVar <- apply(B.norm,1,var)

inconsistency <- sqrt(sum(nVar) * .25)


########### New test stuff


