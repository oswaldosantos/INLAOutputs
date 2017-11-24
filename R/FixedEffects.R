#' Fixed effects
#' @description Summary statistics of fixed marginals
#' @param model object of \code{\link{class}} \code{inla} with fixed marginals.
#' @param quantiles \code{\link{numeric}} \code{\link{vector}} to indicate the quantiles to be computed.
#' @param hpd. Proportion of the distribution included in the highest density interval. 
#' @param sd logical. If \code{FALSE} (default), the standard deviation is not returned.
#' @param expo logical. If \code{TRUE} (default), summary statistics are expoentiated.
#' @param rnd integer indicating the number of decimal places to be used.
#' @return \code{\link{matrix}} with summary statistics.
#' @references Blangiardo, Marta, and Michela Cameletti. Spatial and Spatio-temporal Bayesian Models with R-INLA. John Wiley & Sons, 2015.
#' @export
#' @examples
#' data(sp)
#' mod <- inla(aan ~ shvn + f(id, model = 'bym', graph = sp.adj),
#'             E = eaan, family = 'poisson', data = spn)
#' FixedEffects(mod)
#' FixedEffects(mod, quantiles = NULL, hpd = .95)
FixedEffects <- function(model = NULL, quantiles = c(.025, .975), hpd = NULL, sd = FALSE, expo = TRUE, rnd = 3) {
    if (expo) {
        margs <- lapply(model$marginals.fixed,
                        function(x) inla.tmarginal(exp, x))
        mean <- sapply(model$marginals.fixed, function(x) inla.emarginal(exp, x))
    } else {
        margs <- model$marginals.fixed
        mean <- model$summary.fixed[, 1]
    }
    if (!is.null(quantiles)) {
        qts <- sapply(margs,
                      function(x) inla.qmarginal(quantiles, x))   
    }
    if (!is.null(hpd)) {
        hpds <- sapply(margs,
                       function(x) inla.hpdmarginal(hpd, x))
    }
    if (!is.null(quantiles) & !is.null(hpd)) {
        res0 <- rbind(qts, hpds)
        rownames(res0) <- c(paste0("qt", quantiles),
                            paste0(c("ll_hpd", "ul_hpd"), hpd))
        res0 <- t(res0)
    }
    if (!is.null(quantiles) & is.null(hpd)) {
        res0 <- rbind(qts)
        rownames(res0) <- paste0("qt", quantiles)
        res0 <- t(res0)
    }
    if (is.null(quantiles) & !is.null(hpd)) {
        res0 <- rbind(hpds)
        rownames(res0) <- paste0(c("ll_hpd", "ul_hpd"), hpd)
        res0 <- t(res0)
    }
    res <- cbind(mean, res0)
    if (sd) {
        sd <- sapply(margs, function(x) inla.emarginal(sd, x))
        res <- cbind(res, sd)
    }
    round(res, rnd)
}