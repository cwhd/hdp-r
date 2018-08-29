library(testthat)

source("../modules/tree.helper.r",local=T)
source("../modules/utilities.r",local=T)

test_tha("WTF", {
  expect_equal(1,1)
})

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
  print(paste0("2: ",level2," 3: ",level3))
  print(paste0("2: ",length(level2)," 3: ",length(level3)))
  expect_equal(2,length(level2))
  expect_equal(4,length(level3))
})

test_that("Value is correct at level", {
  level3 <- getNodeNamesAtLevel(defaultTree, 3)
  expect_equal(level3[[1]],"Salty")
  expect_equal(level3[[2]],"Sweet")
})


