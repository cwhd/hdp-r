library(testthat)

test_results <- test_dir(path = "tests", reporter="Summary")
test_res <- test_file(path = "tests/test_tree.helper.r")
