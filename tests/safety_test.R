#testing the function works

expected <- list(prediction = 1, message = "According to our model the water is not safe to swim in.")

test_that("safety-function_works", {
  expect_equal(predict_safety(flow_volume = 20, stage = 3), expected)

})
