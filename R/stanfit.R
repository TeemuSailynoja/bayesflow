#' Convert a cmdstandr-fitted model into an rstan object.
#'
#' @param fit the cmdstandr-fitted model to be converted.
#'
#' @return converted rstan::stanfit object.
#' @export
stanfit <- function(fit) {
  require(cmdstanr)
  require(rstan)
  rstan::read_stan_csv(fit$output_files())
 }
