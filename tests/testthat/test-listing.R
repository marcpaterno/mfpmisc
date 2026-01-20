
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
test_that("lsos works correctly", {
  x <- 1:10
  y <- letters[1:5]
  res <- lsos(environment(), n = 2)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2L)
  expect_named(res, c("Type", "Size", "Rows", "Columns"))
})

test_that("lspackages returns data.frame", {
  res <- lspackages()
  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) > 0)
  expect_true("Package" %in% names(res))
})

test_that("make_document creates file", {
  temp_file <- tempfile(fileext = ".qmd")
  on.exit(unlink(temp_file))
  
  make_document(temp_file)
  expect_true(file.exists(temp_file))
  expect_true(file.exists("custom.css"))
  on.exit(unlink("custom.css"), add = TRUE)
})

test_that("make_document fails on existing file", {
  temp_file <- tempfile(fileext = ".qmd")
  file.create(temp_file)
  on.exit(unlink(temp_file))
  
  expect_error(make_document(temp_file), "already exists")
})