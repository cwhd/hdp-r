# create an example flat data set
# try to do the calculations going up the tree

choices <- list("A","B","C","D")
comboNum <- factorial(length(choices))

#TODO figure out how to populate this autmagically
df1 <- data.frame("A" = c(80),"B" = c(20))
df2 <- data.frame("A" = c(90),"C" = c(10))
df3 <- data.frame("A" = c(95), "D" = c(5))
df4 <- data.frame("B" = c(80), "C" = c(20))
df5 <- data.frame("B" = c(90), "D" = c(10))
df6 <- data.frame("C" = c(80), "D" = c(20))

#show the different comparisons in a matrix
matrixA <- matrix(c("",df1$B[1],df2$C[1],df3$D[1],
                    df1$A[1],"",df4$C[1],df5$D[1],
                    df2$A[1],df4$B[1],"",df6$D[1],
                    df3$A[1],df5$B[1],df6$C[1],""),
                  nrow=4,ncol = 4,byrow = TRUE)
dimnames(matrixA) = list(c("A","B","C","D"),c("A","B","C","D"))
diag(matrixA) <- NA

#perform calculations on the comparisons
matrixB <- matrix(c(1,df1$B[1] / df1$A[1], df2$C[1] / df2$A[1], df3$D[1] / df3$A[1],
                  df1$A[1] / df1$B[1], 1, df4$C[1] / df4$B[1], df5$D[1] / df5$B[1],
                  df2$A[1] / df2$C[1], df4$B[1] / df4$C[1], 1, df6$D[1] / df6$C[1],
                  df3$A[1] / df3$D[1], df5$B[1] / df5$D[1], df6$C[1] / df6$D[1], 1),
                  nrow = 4, ncol = 4, byrow = TRUE)
matrixB <- rbind(matrixB, colSums(matrixB)) #add the sum of the colums
dimnames(matrixB) = list(c("A","B","C","D","Sum"),c("A","B","C","D"))

#so some more calculations that I'll use later on

dfC <- data.frame("AB" = c(matrixB["A","A"] / matrixB["A","B"], 
                            matrixB["B","A"] / matrixB["B","B"],
                            matrixB["C","A"] / matrixB["C","B"],
                            matrixB["D","A"] / matrixB["D","B"]),
                  "BC" = c(matrixB["A","B"] / matrixB["A","C"],
                            matrixB["B","B"] / matrixB["B","C"],
                            matrixB["C","B"] / matrixB["C","C"],
                            matrixB["D","B"] / matrixB["D","C"]
                            ), 
                  "CD" = c(matrixB["A","C"] / matrixB["A","D"],
                            matrixB["B","C"] / matrixB["B","D"],
                            matrixB["C","C"] / matrixB["C","D"],
                            matrixB["D","C"] / matrixB["D","D"]))

dfCMeans <- colMeans(dfC)

#creat the normalization matrix, add the other stuff to it
matrixN <- matrix(c(
  matrixB["A","A"] / matrixB["Sum","A"], matrixB["A","B"] / matrixB["Sum","B"], matrixB["A","C"] / matrixB["Sum","C"], matrixB["A","D"] / matrixB["Sum","D"],
  matrixB["B","A"] / matrixB["Sum","A"], matrixB["B","B"] / matrixB["Sum","B"], matrixB["B","C"] / matrixB["Sum","C"], matrixB["B","D"] / matrixB["Sum","D"],
  matrixB["C","A"] / matrixB["Sum","A"], matrixB["C","B"] / matrixB["Sum","B"], matrixB["C","C"] / matrixB["Sum","C"], matrixB["C","D"] / matrixB["Sum","D"],
  matrixB["D","A"] / matrixB["Sum","A"], matrixB["D","B"] / matrixB["Sum","B"], matrixB["D","C"] / matrixB["Sum","C"], matrixB["D","D"] / matrixB["Sum","D"]
  ), nrow = 4, ncol = 4, byrow = TRUE)
  
nMeans <- rowMeans(matrixN)
nSd <- apply(matrixN,1,sd)
nVar <- apply(matrixN,1,var)

matrixN <- rbind(matrixN, colSums(matrixN))
dimnames(matrixN) = list(c("A","B","C","D","Sum"),c("A","B","C","D"))

#actually I'm not sure about this one...it seems to alwasy skew A
dRaw <- 1
cRaw <- dRaw * dfCMeans[3]
bRaw <- cRaw * dfcMeans[2]
aRaw <- bRaw * dfCMeans[1]
rawSums <- sum(aRaw, bRaw, cRaw, dRaw)


inconsistency <- sqrt(sum(nVar) * .25)

#TODO
#- Do the other grid that I missed with raw and normalized
#- Redo matrixA so it depends on data from from the data frames
#- figure out how to make this dynamic

abLables <- list("A","B","C")
AB <- c(80, 20) #df1
outerAB <- outer(AB, AB, '/')
AC <- c(90, 10) #df2
outerAC<- outer(AC,AC,'/')
BC <- c(80, 20) #df4
outerBC <- outer(BC,BC,'/')

dynaMatrix <- matrix(nrow = length(abLables), ncol = length(abLables))
dimnames(dynaMatrix) = list(abLables,abLables)

######################################
A <- structure(
  c(NA, 20, 10, 5, 80, NA, 20, 10, 90, 80, NA, 20, 95, 90, 80, NA),
  .Dim = c(4, 4),
  .Dimnames = list(LETTERS[1:4], LETTERS[1:4]))
B <- (100 - A) / A

#############################

matrixA2 <- structure(c(NA,df1$B[1],df2$C[1],df3$D[1],
                        df1$A[1],NA,df4$C[1],df5$D[1],
                        df2$A[1],df4$B[1],NA,df6$D[1],
                        df3$A[1],df5$B[1],df6$C[1],NA),
                      .Dim = c(4,4),
                      .Dimnames = list(LETTERS[1:4], LETTERS[1:4]))
diag(matrixA2) <- 1

matrixB2 <- t(matrixA2) / matrixA2
diag(matrixB2) <- 1
B.norm <- matrixB2 / colSums(matrixB2)
bsums <- colSums(matrixB2)
B.norm <- sweep(B,2,colSums(B),`/`)

