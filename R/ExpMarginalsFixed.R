#' Exponentaited fixed marginals
#' @description Posterior relative risks or odds ratios.
#' @param mod object of \code{\link{class}} \code{inla} with fixed marginals.
#' @param mean logical. If \code{TRUE} (default), the mean is computed.
#' @param quantiles \code{\link{numeric}} \code{\link{vector}} to indicate the quantiles to be computed.
#' @param sd logical. If \code{FALSE} (default), the standard deviation is not computed.
#' @param mode logical. If \code{FALSE} (default), the mode is not computed.
#' @param rnd integer indicating the number of decimal places (round) or significant digits (signif) to be used.
#' @return \code{\link{matrix}} with summary statistics for relative risks or odds ratios.
#' @details Exponentaited random marginals represent relative risks for models with one of the following likelihoods: \code{poisson}, \code{zeroinflated.poisson.0}, \code{zeroinflated.poisson.1}, \code{zeroinflated.poisson.2}, \code{nbinomial}, \code{zeroinflated.nbinomial.0}, \code{zeroinflated.nbinomial.1}, \code{zeroinflated.nbinomial.2}. Exponentiated random marginals represent odds ratios for models with one of the followinglikelihoods: \code{binomial}, \code{zeroinflated.binomial.0}, \code{zeroinflated.binomial.1}.
#' @references Blangiardo, Marta, and Michela Cameletti. Spatial and Spatio-temporal Bayesian Models with R-INLA. John Wiley & Sons, 2015.
#' @export
#' @examples
#' data(sp)
#' mod <- inla(aan ~ shvn + offset(log(eaan)) +
#'                 f(id, model = 'bym', graph = sp.adj),
#'             family = 'poisson', data = spn)
#' ExpMarginalsFixed(mod)
ExpMarginalsFixed <- function(mod, mean = TRUE, quantiles = c(0.025, 0.975), sd = FALSE, mode = FALSE, rnd = 3) {
    fe <- mod$marginals.fixed
    
    if (!mean) {
        mean <- NULL
    } else {
        mean <- sapply(fe, function(x) {inla.emarginal(exp, x)})
        mean <- cbind(mean)
    }
    
    if (!sd) {
        sd <- NULL
    } else {
        sd <- sapply(fe,
                     function(x1) {
                         inla.zmarginal(
                             inla.tmarginal(function(x2) {exp(x2)}, x1),
                             silent = TRUE)
                     })
        sd.nms <- attr(sd, 'dimnames')[[2]]
        sd <- cbind(sd = unlist(sd[2, ]))
        row.names(sd) <- sd.nms
    }
    
    if (is.null(quantiles)) {
        quants <- NULL
    } else {
        quants <- 
            sapply(fe,
                   function(x1) {
                       inla.qmarginal(quantiles,
                                      inla.tmarginal(function(x2) exp(x2), x1))
                   })
        quants <- rbind(quants)
        row.names(quants) <- quantiles
        quants <- t(quants)
    }
    
    if (mode) {
        mode <-
            sapply(fe,
                   function(x1) {
                       inla.mmarginal(inla.tmarginal(function(x2) exp(x2), x1))
                   })
        mode <- cbind(mode)
    } else {
        mode <- NULL
    }
    round(cbind(mean, sd, quants, mode), rnd)
}