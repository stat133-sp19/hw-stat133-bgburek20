# Title check_prob
# Description Tests if input prob is a valid probability
# Param Prob a probability
# Return If prob is a valid probability
check_prob <- function(prob){
  if(0 <= prob & prob <= 1) {
    return(TRUE)
  }
  else {
    stop('p has to be a number between 0 and 1')
  }
}
# Title check_trials
# Description Tests if input trials is a valid value for number of trials
# Param trials number of trials
# Return If trials is a valid value
# Examples
check_trials <- function(trials) {
  if(trials != round(trials) | trials < 0) {
    stop('invalid trials value')
  }
  else {
    return(TRUE)

  }
}
# Title check_sucess
# Description Tests if input success is a valid value for number of sucesses
# Param success number of successes
# Param trials number of trials
# Return If success is a valid value
# Examples
check_success <- function(success, trials){
  if(any(success < 0) | any(success > trials)){
    stop('invalid sucess value')
  }
  else if(any(success > trials)){
    stop('success cannot be greater than trials')
  }
  return(TRUE)
}

