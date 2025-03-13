#' Predict safety of swimming using logistic regression
#' 
#' Use flow volume and stage to predict safety
#' @return list of the following 
#' \describe
#' \item {prediction} returns a zero or a one indicating safe or not safe to swim in
#' \item {message} returns a message indicating the result of the prediction
#' @param flow_volume the flow volume of the water body in cubic ft per second
#' @param stage the depth of the water body in feet
#' @param intercept -18.1 based on logistic regression tested with crossfold validation
#' @param velocity_coef 8.35 based on logistic regression tested with crossfold validation
#' @param log_odds linear predictor of water being unsafe to swim in
#' @param prob calculates the probability of the water being unsafe to swim in

predict_safety <- function(flow_volume, stage) {
  
  
  # Check for NA or non-numeric values
  if (is.na(flow_volume) | is.na(stage) | !is.numeric(flow_volume) | !is.numeric(stage)) {
    return(list(
      prediction = NA, 
      message = "Invalid input: flow_volume or stage is missing or not numeric"
    ))
  }
  
  #write error codes for uneven lengths of values
  if (length(flow_volume) != length(stage)) {
    return("Number of flow volume values is not equal to number of stage values.")
  }
  
  # Model coefficients
  intercept <- -18.1
  velocity_coef <- 8.35
  
  
  velocity_ft_s <- flow_volume / stage  
  
  # Calculate the log-odds (linear predictor)
  log_odds <- intercept + velocity_coef * velocity_ft_s
  
  # Convert log-odds to probability using the logistic function
  prob <- 1 / (1 + exp(-log_odds))
  
  # Predict safety: if prob > 0.5, predict unsafe (1), else safe (0)
  if (prob > 0.5) {
    prediction <- 1
    message <- "According to our model the water is not safe to swim in."
  } else {
    prediction <- 0
    message <- "According to our model the water is safe to swim in."
  }
  
  # Return the prediction and message
  return(list(prediction = prediction, message = message))
  
}
