context("hdm tree helper")
library("data.tree")

defaultTree <- Node$new("Choose Breakfast")
taste <- defaultTree$AddChild("Taste")
speed <- defaultTree$AddChild("Speed")
salty <- taste$AddChild("Salty")
sweet <- taste$AddChild("Sweet")
fast <- speed$AddChild("Fast")
slow <- speed$AddChild("Slow")

test_that("Get Tree Levels", {
  level2 <- getNodeNamesAtLevel(defaultTree, 2)
  level3 <- getNodeNamesAtLevel(defaultTree, 3)

  expect_equal(2,length(level2))
  expect_equal(4,length(level3))
})

test_that("Value is correct at level", {
  level3 <- getNodeNamesAtLevel(defaultTree, 3)
  expect_equal(level3[[1]],"Salty")
  expect_equal(level3[[2]],"Sweet")
})

test_that("Unique combinations is correct", {
  #TODO bring some more model data into here...
  combos1 <- getUniqueChildCombinations(taste, NULL)
  expect_equal(length(combos1),2)
})

test_that("Example tree works", {
  exampleTree <- getExampleTree()
  expect_equal(length(exampleTree$Speed$children),2)
  expect_equal(length(exampleTree$Taste$children),2)
})

test_that("getNodesAtLevel works", {
  #getNodesAtLevel
  exampleTree <- getExampleTree()

  expect_equal(getNodesAtLevel(exampleTree, 3)[1,"name"],"Salty")
  expect_equal(getNodesAtLevel(exampleTree, 3)[2,"name"],"Sweet")
  expect_equal(getNodesAtLevel(exampleTree, 3)[3,"name"],"Fast")
  expect_equal(getNodesAtLevel(exampleTree, 3)[4,"name"],"Slow")
})

test_that("getNodeName works", {
  exampleTree <- getExampleTree()
  expect_equal(exampleTree$Taste$name,getNodeName(exampleTree$Taste))
})

