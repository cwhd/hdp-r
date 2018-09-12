context("hdpr utilities")

test_that("Just a trim", {
  str1 <- " One"
  str2 <- "Two "
  str3 <- "This is string three"
  str4 <- " fo "
  expect_equal(trim(str1),"One")
  expect_equal(trim(str2),"Two")
  expect_equal(trim(str3),"This is string three")
  expect_equal(trim(str4),"fo")
})

test_that("Get last element in path works", {
  str1 <- "return/last/element/please"
  str2 <- "No path here"
  str3 <- "small/path"
  str4 <- "some works/have spaces/and that is ok"
  expect_equal(getLastElementInPath(str1),"please")
  expect_equal(getLastElementInPath(str2),"No path here")
  expect_equal(getLastElementInPath(str3),"path")
  expect_equal(getLastElementInPath(str4),"and that is ok")
})

test_that("Give me children or give me nothing", {
  exampleTree <- getExampleTree()
  leafNode <- FindNode(node=exampleTree,name = "Fast")

  expect_equal(childrenOrNothing(exampleTree$Taste),"Salty, Sweet")
  expect_equal(childrenOrNothing(leafNode),"")
})
