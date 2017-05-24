#' Fixed effects
#' @description Summary statistics of fixed marginals
#' @param model object of \code{\link{class}} \code{inla} with fixed marginals.
#' @param mean logical. If \code{TRUE} (default), the mean is computed.
#' @param sd logical. If \code{FALSE} (default), the standard deviation is not returned.
#' @param quantiles \code{\link{numeric}} \code{\link{vector}} to indicate the quantiles to be computed.
#' @param expo logical. If \code{TRUE} (default), summary statistics are expoentiated.
#' @param rnd integer indicating the number of decimal places (round) or significant digits (signif) to be used.
#' @return \code{\link{matrix}} with summary statistics for relative risks or odds ratios.
#' @details Exponentiated fixed marginals represent relative risks for models with one of the following likelihoods: \code{poisson}, \code{zeroinflated.poisson.0}, \code{zeroinflated.poisson.1}, \code{zeroinflated.poisson.2}, \code{nbinomial}, \code{zeroinflated.nbinomial.0}, \code{zeroinflated.nbinomial.1}, \code{zeroinflated.nbinomial.2}. Exponentiated random marginals represent odds ratios for models with one of the followinglikelihoods: \code{binomial}, \code{zeroinflated.binomial.0}, \code{zeroinflated.binomial.1}.
#' @references Blangiardo, Marta, and Michela Cameletti. Spatial and Spatio-temporal Bayesian Models with R-INLA. John Wiley & Sons, 2015.
#' @export
#' @examples
#' data(sp)
#' mod <- inla(aan ~ shvn + f(id, model = 'bym', graph = sp.adj),
#'             E = eaan, family = 'poisson', data = spn)
#' FixedEffects(mod)
FixedEffects <- function(model = NULL, mean = TRUE, sd = FALSE, quantiles = c(.025, .975), expo = TRUE, rnd = 3) {
    if (mean) {
        mn <- model$summary.fixed[, 1:2]
    }
    qts <- sapply(model$marginals.fixed,
                  function(x) inla.qmarginal(quantiles, x))
    rownames(qts) <- quantiles
    qts <- t(qts)
    if (sd) {
        mf <- cbind(mn, qts)
    } else {
        mf <- cbind(mn, qts)[, -2]
    }
    if (expo) {
        round(exp(mf), rnd)
    } else {
        round(mf, rnd)
    }
}