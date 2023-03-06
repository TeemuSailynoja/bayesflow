

#' Expected pointwise predictive density
#'
#' Compute elpd for a set of models and model weights.
#'
#' @param x List of fitted models.
#' @param weights vector of model weights
#' @param ... arguments passed to `log_lik`, e.g. `newdata`
#'
#' @return `loo` `elpd_generic` object
#'
#' @aliases elpd
#' @import loo
#' @import rstanarm
#' @method elpd stanreg_list
#' @export
#'
#' @examples
elpd <- function(x, ...) {
    useMethod("elpd")
}

#' @rdname elpd
#' @export
elpd.stanreg_list <- function(model_list, weights, ...) {
    stopifnot(length(model_list) == length(weights), check_weights(weights))
    log_like_list <- lapply(model_list, rstanarm::log_lik, ...)
    elpd_compute(log_like_list, weights)
}

# ---------- Internals --------------

#' Check that given weihgts sum to approx one
#' @param weights model weights
#' @noRd 
check_weights <- function(weights) {
    abs(sum(weights) - 1.0) < 1e-6
}

#' Compute weighted elpd from a list of log-likelihood matrices or arrays
#' @param ll_list list of log-likelihood matrices or arrays
#' @return `elpd_generic` object
#' @noRd 
elpd_compute <- function(ll_list, weights) {
    list_types <- lapply(
        ll_list,
        function(x) any(class(x) %in% c('matrix', 'array'))
    )
    stopifnot(Reduce(all, list_types))

    # Weighted pointwise ll-evaluations.
    pw_likelihoods = mapply(
        function(x, y) exp(x + log(y)),
        x = ll_list,
        y = weights,
        SIMPLIFY = FALSE
    )
    # Compute log likelihood matrix for the averaged model.
    lls = log(Reduce("+", pw_likelihoods))

    # Finally, compute elpd
    loo::elpd(lls)
}