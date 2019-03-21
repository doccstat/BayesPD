context("test-thresholdingfunctions")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
  expect_equal(soft(3, 1), 2)
})

test_that("hard thresholding works", {
  expect_equal(2 * 2, 4)
  expect_equal(hard(3, 1), 3)
})
