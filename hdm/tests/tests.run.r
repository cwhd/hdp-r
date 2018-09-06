library(testthat)

#in case things get strange, make sure you're in the right place
print(paste0("Running tests from ",getwd())) 

test_results <- test_dir(path = "tests", reporter="Summary")
