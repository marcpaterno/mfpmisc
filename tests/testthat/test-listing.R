
test_that("ls.objects on empty environment works", {
  res <- ls.objects(environment())
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0L)
  expect_named(res, c("Type", "Size", "Rows", "Columns"))
})

test_that("ls.objects recognizes functions", {
  f <- function(){}
  res = ls.objects(environment())
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1L)
  expect_equal(res$Type[[1]], "function")
})
