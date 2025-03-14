#testing the number of outputs is correct
#100 taken from n=100, when creating the data frame

test_that("safety-function_output_works", {
  
  expect_length(prediction, 100)
  expect_length(stage, 100)
})