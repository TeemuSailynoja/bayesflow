##' Generate datasets from a data-generating process
##'
##' @param dgp Data-generating process function
##' @param n_datasets Number of datasets to generate
##' @param ... Arguments passed to data-generating process function
##' @return List of datasets, each generated from the dgp function
generate_from_dgp <- function(dgp, n_datasets, ...) {

  checkmate::assert_function(dgp)
  checkmate::assert_number(n_datasets)

  datasets <- lapply(1:n_datasets, dgp, ...)

  return(datasets)
}


##' Example data-generating process (normal distribution)
##'
##' @param n Number of observations
##' @param mean Mean of normal distribution
##' @param sd Standard deviation of normal distribution
##' @param ... unused but required to exist for use with `generate_from_dgp`
##' @return List of true parameters (mu and sigma) and observations
dgp_example_normal <- function(n, mean, sd, ...) {

  # save parameters
  true_pars <- list(mu = mean, sigma = sd)

  # generate data set
  y <- rnorm(n, mean, sd)

  return(list(true_pars = true_pars, y = y))

}
