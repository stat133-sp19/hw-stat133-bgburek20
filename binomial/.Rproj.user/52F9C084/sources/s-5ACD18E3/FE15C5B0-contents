# private function to create mean
aux_mean <- function(trials, prob){
  return(trials * prob)
}
# private function to get variance
aux_variance <-function(trials, prob){
  return(trials * prob * (1-prob))
}
# private function to get mode
aux_mode <- function(trials, prob){
  if(trials * prob + prob == round(trials*prob+prob)){
    return(c(trials*prob+prob,
             trials*prob+prob-1))
  }
  else{
    return(floor(trials*prob+prob))
  }
}
# private function to get skewness
aux_skewness <- function(trials, prob){
  std <- sqrt(trials * prob * (1 - prob))
  return((1 - 2 * prob)/ std)
}
# private function to get kurtosis
aux_kurtosis <- function(trials, prob) {
  return((1 - ((6 * prob) * (1 - prob)))/
           (trials * prob * (1 - prob)))
}

