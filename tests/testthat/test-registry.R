source("R/registry.R")
testthat::test_that("registry is unique and complete", {
  x <- load_method_registry()
  testthat::expect_equal(nrow(x), 460)
  testthat::expect_equal(length(unique(x$id)), nrow(x))
  testthat::expect_true(all(nzchar(x$name)))
  testthat::expect_true(all(nzchar(x$package)))
})
