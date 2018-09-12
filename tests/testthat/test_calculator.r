context("hdm calculator")
library("data.tree")

test_that("HDM Weights correctly calculated", {
  print(paste0("Running tests from ",getwd()))

  comboFrames <- readRDS("calculateHDMWeights-comboFrames.rds")
  tree <- readRDS("calculateHDMWeights-tree.rds")
  #calculate!
  calculatedTree <- calculateHDMWeights(tree, comboFrames)
  print(calculatedTree, "norm", "weight","inconsistency")

  easyNode <- FindNode(node=calculatedTree,name = "easy")

  #make sure everything is calculating
  expect_equal(easyNode$norm,0.67000000)
  expect_equal(easyNode$weight,0.596300000)
})
