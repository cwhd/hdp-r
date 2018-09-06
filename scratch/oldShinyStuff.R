####
#old functions by level
#old function
ui.evaluation.build <- function(tree, alternatives) {
  print("ui.evaluation.build")
  #first convert the tree to data frame for matrix operations
  dfLevels <- ToDataFrameNetwork(tree, "level", "name")

  comparisonPanelNumber <- tree$height
  #print(paste0("alts agaion:",alternatives," len:",length(alternatives)," cNum:",comparisonPanelNumber))
  if(length(alternatives > 0)) { comparisonPanelNumber <- comparisonPanelNumber + 1 }
  output$uiEvaluateCriteria <- renderUI({
    sliders <- lapply(2:comparisonPanelNumber, function(i) {
      ui.sliders.generate(i,dfLevels, tree, alternatives)
    })
    do.call(tabsetPanel,sliders)
  })

  lapply(2:comparisonPanelNumber, function(i) {
    ui.sliders.observers.add(i,dfLevels)
  })
}
#old function
comboFrames.buildFromSliders <- function(combos, level) {
  dfCriteria <- split(combos,rep(1:nrow(combos),1))
  criteriaDfList <- lapply(1:nrow(combos), function(i) {
    dfOut <- data.frame(streOne = c(input[[paste0("slider_",level,"_",i)]]), streTwo = c(100 - input[[paste0("slider_",level,"_",i)]]))
    colnames(dfOut) <- c(dfCriteria[[i]][[1]], dfCriteria[[i]][[2]])
    return(dfOut)
  })
  criteriaDfList
}
#old function
ui.sliders.observers.add <- function(level, dfLevels) {
  #add observers to the critiera sliders
  combos <- treeLevel.combos.unique(level, dfLevels, hdp$tree, hdp$alternatives)

  lapply(1:nrow(combos), function(i) {
    observeEvent(input[[paste0("slider_",level,"_",i)]], {
      output[[paste0("uiOutputValueA_",level,"_",i)]] <- renderUI({
        span(input[[paste0("slider_",level,"_",i)]])
      })
      output[[paste0("uiOutputValueB_",level,"_",i)]] <- renderUI({
        span(100 - input[[paste0("slider_",level,"_",i)]])
      })
    })
  })
}



########################################
#This was calculating stuff in the admin tool
#expertEvalDfList <- lapply(1:length(hdp$experts), function(i) {
#  evaluations <- loadResults(hdp$currentModelId, hdp$experts[i])
#
#  froms <- eval(parse(text = evaluations$results$from))
#  tos <- eval(parse(text = evaluations$results$to))
#  pathStrings <- eval(parse(text = evaluations$results$pathString))
#  evalWeights <- eval(parse(text = evaluations$results$weight))
#  evalNorms <- eval(parse(text = evaluations$results$norm))
#  sliderValues <- eval(parse(text = evaluations$results$sliderValues))
#
#  goodDf <- data.frame(froms,tos,pathStrings, evalWeights, evalNorms, sliderValues)
#  goodDf
#})
#This will combine and average norms from experts - not sure if I need this?
#normsDf <- lapply(1:length(expertEvalDfList), function(i) {
#  expertEvalDfList[[i]]$evalNorms
#})
#print("----normsDf")
#normsDf <- as.data.frame(normsDf)
#print(normsDf)
#average the values across all the experts
#dfMeanWeights <- rowMeans(normsDf)
#print(dfMeanWeights)
#put the values in a nice data frame with labels
#treeWithMeans <- data.frame(expertEvalDfList[[1]]$froms, dfMeanWeights)
