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
    if(any(success) < 0 | any(success) > trials){
      stop('invalid sucess value')
    }
    else if(any(success) > trials){
      stop('success cannot be greater than trials')
    }
  return(TRUE)
}

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
#'@title bin_choose
#'@description Calculates number of combinations in which successes can occur in trials
#'@param k numeric number of successes
#'@param n numeric number of trials
#'@return number of combinations successes can occur in trials
#'@export
#'@examples
#' x = bin_choose(n = 5, k = 2)
#'
#' xa = bin_choose(5, 0)
#'
#' # Multiple success values
#' xb = bin_choose(5, 1:3)
bin_choose <- function(n, k) {
    if(any(k) > n){
      stop("k cannot be greater than n")
    }
  else {
  num = factorial(n)
  denom = factorial(k) * factorial(n-k)
  return(num / denom)
  }
  }

#'@title bin_probability
#'@description Calculates likelihood of bin
#'@param success numeric number of successes
#'@param trials numeric number of trials
#'@param prob probability of success
#'@return number of combinations successes can occur in trials
#'@export
#'@examples
#'
#' # probability of getting 2 successes in 5 trials
#' # (assuming prob of success = 0.5)
#'x = bin_probability(success = 2, trials = 5, prob = .5)
#'
#' # probabilities of getting 2 or less successes in 5 trials
#' # (assuming prob of success = 0.5)
#'xa = bin_probability(success = 0:2, trials = 5, prob = .5)
#'
#' # 55 heads in 100 tosses of a loaded coin with 45% chance of heads
#' bin_probability(success = 55, trials = 100, prob = .45)
bin_probability <- function(success, trials, prob){
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  return(bin_choose(trials, success) * prob ** success * (1 - prob) ** (trials - success))
}
#'@title bin_distribution
#'@description Calculates likelihood of each bin in binomial distribution
#'@param trials numeric number of trials
#'@param prob probability of success
#'@return data frame with number of successes and probability
#'@export
#'@examples
#' # binomial probability distribution
#' bin_distribution(trials = 5, prob = .5)
bin_distribution <- function(trials, prob){
  success <- 0:trials
  probability <- bin_probability(success, trials, prob)
  frame = data.frame(success = success, probability = probability)
  class(frame) = c("bindis", "data.frame")
  frame
}
#'@export
plot.bindis <- function(data){
  barplot(data$probability, xlab = 'successes', ylab = 'probability',
                  names.arg = data$success)
  invisible(data)
}

#'@title bin_cumulative
#'@description Calculates likelihood of each bin in binomial distribution and cumulative probability
#'@param trials numeric number of trials
#'@param prob probability of success
#'@return data frame with number of successes, probabiility, and cumulative probability
#'@export
#'@examples
#' # binomial cumulative distribution
#' bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials, prob){
  success <- c(0:trials)
  probability <- bin_probability(success, trials, prob)
  cumulative <- cumsum(probability)
  frame = data.frame(success, probability, cumulative)
  class(frame) = c("bincum", "data.frame")
  frame
}
#' @export
plot.bincum <- function(frame){
  plot <- plot(frame$cumulative, type = "o", xlab = 'successes', ylab = 'probability',
               xaxt = 'n')
          axis(1, at=1:length(frame$cumulative), labels = frame$success)
 plot
 invisible(frame)
}
#'@title bin_variable
#'@description Calculates likelihood of each bin in binomial distribution and cumulative probability
#'@param trials numeric number of trials
#'@param prob probability of success
#'@return List of class binvar with trials and prob
#'@export
#'@examples
#' bin_variable(5, .5)
bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  lst <- list(trials = trials, prob = prob)
  class(lst) = 'binvar'
  lst
}
#'@export
print.binvar <- function(lst){
  trials = lst$trials
  prob = lst$prob
  cat('"Binomial variable" \n',
      '\n',
      'Parameters \n',
      '- number of trials:', trials, '\n',
      '- prob of success :', prob)
  invisible(lst)
}

summary.binvar <- function(lst){
  lst$mean = aux_mean(lst$trials, lst$prob)
  lst$variance = aux_variance(lst$trials, lst$prob)
  lst$mode = aux_mode(lst$trials, lst$prob)
  lst$skewness = aux_skewness(lst$trials, lst$prob)
  lst$kurtosis = aux_kurtosis(lst$trials, lst$prob)
  lst
}

print.summary.binvar <- function(lst){
  cat('"Summary binomial" \n',
      '\n',
      'Parameters \n',
      '- number of trials:', lst$trials, '\n',
      '- prob of success :', lst$prob, '\n',
      '\n',
      'Measures \n',
      '- mean     :', aux_mean(lst$trials, lst$prob), '\n',
      '- variance :', aux_variance(lst$trials, lst$prob), '\n',
      '- mode     :', aux_mode(lst$trials, lst$prob), '\n',
      '- skewness :', aux_skewness(lst$trials, lst$prob), '\n',
      '- kurtosis :', aux_kurtosis(lst$trials, lst$prob), '\n')
}

bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}

bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}

