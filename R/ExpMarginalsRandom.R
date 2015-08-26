#' Exponentaited random marginals
#' @description Posterior mean of area-specific relative risks (odds ratio), compared to the mean relative risk (odds ratio).
#' @param mod object of \code{\link{class}} \code{inla} or list of objects of \code{\link{class}} \code{inla}. Objects must have outputs from a BYM model.
#' @param mod.names \code{\link{character}} \code{\link{vector}} with names for the exponentiated random marginals of each model.
#' @param rnd integer indicating the number of decimal places (round) or significant digits (signif) to be used.
#' @return \code{\link{matrix}} with as many rows as areas and as many columns as models in \code{mod}. 
#' @details Exponentaited random marginals represent relative risks for models with one of the following likelihoods: \code{poisson}, \code{zeroinflated.poisson.0}, \code{zeroinflated.poisson.1}, \code{zeroinflated.poisson.2}, \code{nbinomial}, \code{zeroinflated.nbinomial.0}, \code{zeroinflated.nbinomial.1}, \code{zeroinflated.nbinomial.2}. Exponentiated random marginals represent odds ratios for models with one of the followinglikelihoods: \code{binomial}, \code{zeroinflated.binomial.0}, \code{zeroinflated.binomial.1}.
#' @references Blangiardo, M., Cameletti, M., Baio, G., & Rue, H. (2013). Spatial and spatio-temporal models with R-INLA. Spatial and spatio-temporal epidemiology, 7, 39-55.
#' @export
#' @examples 
#' data(sp)
#' 
#' mod1 <- inla(aan ~ offset(log(eaan)) +
#'                  f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn)
#' 
#' mod2 <- inla(aan ~ shvn + offset(log(eaan))+
#'                  f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn)
#' 
#' models <- list(mod1, mod2)
#' exp.mr <- ExpMarginalsRandom(models, mod.names = c('pmrr.mod1', 'pmrr.mod2'))
#' summary(exp.mr)
#' 
#' ## Note that similar results can be obtained using another approach.
#' 
#' # Compute a linear combination to obtain the area-specific relative-risks.
#' lcs <- inla.make.lincombs(id = diag(96), '(Intercept)' = rep(1, 96))
#' mod3 <- inla(aan ~ offset(log(eaan)) +
#'                  f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn,
#'              lincomb = lcs)
#' 
#' # Area-specific relative-risks (2 equivalent alternatives to calculate them).
#' asrr1 <- exp(mod3$summary.lincomb.derived[ , 2])
#' asrr2 <- exp(as.numeric(mod3$summary.fixed[1]) + mod3$summary.random[[1]][, 2])
#' head(round(cbind(asrr1, asrr2), 3))
#' 
#' # Area-specific relative risks compared to the mean relative risk.
#' approach1 <- exp.mr[ , 1]
#' approach2 <- asrr1 / exp(as.numeric(mod3$summary.fixed[1]))
#' plot(approach1, approach2)
#' head(round(cbind(approach1, approach2), 3))
#' 
#' # For untransformed values (not relative risks), both approaches
#' # give the same results.
#' unt1 <- mod3$summary.lincomb.derived[ , 2] - as.numeric(mod3$summary.fixed[1])
#' unt2 <- mod3$summary.random[[1]][ , 2]
#' head(round(cbind(unt1, unt2), 3))
ExpMarginalsRandom <- function(mod = NULL, mod.names = NULL, rnd = 3) {
    if (class(mod) == 'inla') {
        mod <- list(mod)
    }
    n <- nrow(mod[[1]]$model.matrix)
    zeta <- matrix(rep(NA, length(mod) * n), nrow = n)
    for (i in 1:length(mod)) {
        m <- mod[[i]]$marginals.random[[1]][1:n]
        zeta[ , i] <- sapply(m, function(x) {inla.emarginal(exp, x)})
    }
    if (!is.null(mod.names)) {
        colnames(zeta) <- mod.names
    }
    round(zeta, rnd)
}
