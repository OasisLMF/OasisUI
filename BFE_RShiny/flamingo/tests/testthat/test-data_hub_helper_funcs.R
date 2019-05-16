test_that("simplify_path returns correct sets",{
  orig_set <- c("/", "./", "output/", "output/test", "test1", "output_test2")
  expected_set <- c("test", "test1", "output_test2")
  constructed_set1 <- simplify_path(orig_set, "output")
  expect_equal(expected_set, constructed_set1)
  constructed_set2 <- simplify_path(orig_set, "output/")
  expect_equal(expected_set, constructed_set2)
})
