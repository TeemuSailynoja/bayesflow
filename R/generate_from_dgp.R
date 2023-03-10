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
##'   mu = 5,
##'   sigma = 2
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
##' @param rep_id id of dataset
##' @param n_obs number of observations
##' @param mu mu parameter
##' @param sigma sigma parameter
##' @param ... Unused
##' @return List of true parameters (mu and sigma) and resulting observations
##' @export
dgp_example_normal <- function(rep_id, n_obs, mu, sigma, ...) {

  # save parameters
  dgp_args <- list(n_obs = n_obs, mu = mu, sigma = sigma)

  true_pars <- list(mu = mu, sigma = sigma)
  
  # generate data set
  y <- stats::rnorm(n = n_obs, mean = mu, sd = sigma)

  return(
    list(
      rep_id = rep_id,
      dgp_args = dgp_args,
      true_pars = list(mu = mu, sigma = sigma)
      data = list(y = y)
    )
  )

}
