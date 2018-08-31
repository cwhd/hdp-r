library(testthat)

#TODO print out CWD so I know where this thinks it is...
test_results <- test_dir(path = "tests", reporter="Summary")
test_res <- test_file(path = "tests/test_tree.helper.r")
