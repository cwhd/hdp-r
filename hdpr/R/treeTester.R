library(data.tree)

#build an example tree
acme <- Node$new("Acme Inc.")
accounting <- acme$AddChild("Accounting")
software <- accounting$AddChild("New Software")
standards <- accounting$AddChild("New Accounting Standards")
research <- acme$AddChild("Research")
newProductLine <- research$AddChild("New Product Line")
newLabs <- research$AddChild("New Labs")
it <- acme$AddChild("IT")
outsource <- it$AddChild("Outsource")
agile <- it$AddChild("Go agile")
goToR <- it$AddChild("Switch to R")

#this is all styling stuff
SetGraphStyle(acme, rankdir = "TB")
SetEdgeStyle(acme, arrowhead = "vee", color = "grey35", penwidth = 2)
SetNodeStyle(acme, style = "filled,rounded", shape = "box", fillcolor = "GreenYellow",
             fontname = "helvetica", tooltip = GetDefaultTooltip)
SetNodeStyle(acme$IT, fillcolor = "LightBlue", penwidth = "5px")

print(acme)
plot(acme)

#this is a helper function to get unique combinations
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

#this shows a grid with unique combinations
#abcLables <- c("A","B","C","D")
#testGrid <- expand.grid.unique(abcLables,abcLables)
#levelOneNodes <- acme$Get('level')
#I forget what this is...?
#print(levelOneNodes)
#levelOne <- lapply(
# levelOneNodes,function(i) {
#   out <- acme$Get(i)
# }
#)

#- Put the tree into a data frame
dfLevels <- ToDataFrameTree(acme, "level", "name")

#- filter the data frame by level to create a data frame for category, factors, alternatives
dfLevel1 <- dfLevels[dfLevels$level == 1,c("level","name")]
dfLevel2 <- dfLevels[dfLevels$level == 2,c("level","name")]
dfLevel3 <- dfLevels[dfLevels$level == 3,c("level","name")]

print(dfLevel3[[1,"name"]])

#- unique combinations for each level of the tree
levelTwoCombos <- expand.grid.unique(dfLevel2$name, dfLevel2$name)
levelThreeCombos <- expand.grid.unique(dfLevel3$name, dfLevel3$name)

#TODO
#- move this over to the shiny app
#- create interface for comparisons dynamically
#-
#- for each row of the unique combinations, create the comparison page
#- then I can take the values of the comparisons and use them in the hdpCalcTest file
#- save them using this example: https://daattali.com/shiny/persistent-data-storage/

#testing getting node names...
toString(acme$name)
toString(acme$children)

for(chit in acme$children) {
  print(chit$name)
}

#apply(acme$children, 1, name)
nameList <- lapply(1:length(acme$children), function(i){
  toString(acme$children[[i]]$name)
})
toString(nameList)

toString(
  lapply(1:length(acme$children), function(i){
    toString(acme$children[[i]]$name)
  })
)

toString(FindNode(node=acme,name = "Accounting")$children)

length(FindNode(node=acme,name = "Accounting")$children)

acme$Accounting$children[[1]]$name
acme$Accounting$parent
#testFrame1 <- as.data.frame(acme, "level","name","parent") #don't use this
testFrame2 <- ToDataFrameTree(acme, "pathString", "level","name")
#testFrame3 <- ToDataFrameTable(acme, "pathString", "level","name") # don't use this
testFrame3 <- ToDataFrameNetwork(acme, "level","name")

testAcme2 <- as.Node(testFrame2)
print(testAcme2)
plot(testAcme2)

levelThree <- Prune(acme,pruneFun = function(x) x$level == 3)

print(outsource$parent$name)
print(acme$height)
print(length(testAcme2$children))
print(length(testAcme2))
print(nrow(ToDataFrameNetwork(testAcme2)))

print(testAcme2$Get('name'))

nodes <- testAcme2$Get('name')

nodeApply <- lapply(1:length(nodes), function(i) {
  daNode <- FindNode(node = testAcme2, name = nodes[i])
  #print(paste0("----nodes: ",length(daNode$children)))
  #print(daNode)
  #print(daNode$children)
  if(length(daNode$children) > 0) {
    chitlinNames <- for(chit in daNode$children) {
      chit$name
      #print(chit$name)
    }
   # print("---chitnames")
    #print(chitlinNames)
    cAgain <- lapply(1:length(daNode$children), function(j){
      daNode$children[[j]]$name
    })
    print(cAgain)
  }
})

chitlinNames <- lapply(1:length(nodeApply), function(i) {
  nodeApply[[i]]$children$name
})

for(chit in testAcme2$children) {
  print(chit$name)
}

#build an example tree
acme <- Node$new("Acme Inc.")
accounting <- acme$AddChild("Accounting")
software <- accounting$AddChild("New Software")
standards <- accounting$AddChild("New Accounting Standards")
research <- acme$AddChild("Research")
newProductLine <- research$AddChild("New Product Line")
newLabs <- research$AddChild("New Labs")
it <- acme$AddChild("IT")
outsource <- it$AddChild("Outsource")
agile <- it$AddChild("Go agile")
goToR <- it$AddChild("Switch to R")
a1 <- goToR$AddChild("A Node")
a2 <- goToR$AddChild("B Node")
a3 <- goToR$AddChild("C Node")
b1 <- agile$AddChild("A Node")
b2 <- agile$AddChild("B Node")
b3 <- agile$AddChild("C Node")


