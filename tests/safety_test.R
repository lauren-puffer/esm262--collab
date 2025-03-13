
test_that("safety-function_works", {
  
  expect_equal(predict_safety(flow_volume = 20, stage = 3), 1)
  expect_equal(predict_safety(flow_volume = 20, stage = 3), 
               "According to our model the water is not safe to swim in.")
})
