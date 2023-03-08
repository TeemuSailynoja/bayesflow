##' Generate datasets from a data-generating process
##'
##' @param dgp Data-generating process function
##' @param n_datasets Number of datasets to generate
##' @param ... Arguments passed to data-generating process function
##' @return List of datasets, each generated from the dgp function
##'
##' @examples
##' \dontrun{
##' generate_from_dgp(
##'   dgp = dgp_example_normal,
##'   n_datasets = 10,
##'   mean = 5,
##'   sd = 2
##' )
##' }
##' @export
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
##' @export
dgp_example_normal <- function(rep_id, n, mu, sigma, ...) {

  # save parameters
  dgp_pars <- list(n = n, mu = mu, sigma = sigma)

  # generate data set
  y <- stats::rnorm(n = n, mean = mu, sd = sigma)

  return(
    list(
      rep_id = rep_id,
      dgp_pars = dgp_pars,
      data = list(y = y)
    )
  )

}
