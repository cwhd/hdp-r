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

test_that("Agree to disagree", {
  #build matrix
#  val1  val2  val3
#  expert1 0.010 0.010 0.010
#  expert2 0.012 0.012 0.012
#  expert3 0.200 0.010 0.200
  testMatrix <- matrix(c(.01,.012,.2,.01,.012,.01,.01,.012,.2),nrow = 3, ncol = 3)
  colnames(testMatrix) <- c("val1","val2","val3")
  rownames(testMatrix) <- c("expert1","expert2","expert3")

  dis <- calculateExpertDisagreement(testMatrix)

  expect_equal(0.0366, dis)
})
