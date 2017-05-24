#' Specific effects
#' @description Fixed plus random effects of each unit (area).
#' @param ... objects of \code{\link{class}} \code{inla} in random marginals.
#' @param expo logical. If \code{TRUE} (default), summary statistics are expoentiated.
#' @param rnd integer indicating the number of decimal places (round) or significant digits (signif) to be used.
#' @return \code{\link{matrix}} with as many rows as areas and as many columns as models. Exponentiated values represent the posterior mean of area-specific relative risks (odds ratio), compared to the mean relative risk (odds ratio). 
#' @details \code{\link{RandomEffects}} return the unit-specific effects relative to the fixed effect; thus, exponentiated random effects represent the relative risk (odds ratio) of each unit, relative to the mean relative risk. \code{SpecificEffects} return the fixed effect plus the unit-specific random effects; thus, exponentiated specific effects represent the relative risk (odds ratio) of each unit. Exponentiated specific marginals represent relative risks for models with one of the following likelihoods: \code{poisson}, \code{zeroinflated.poisson.0}, \code{zeroinflated.poisson.1}, \code{zeroinflated.poisson.2}, \code{nbinomial}, \code{zeroinflated.nbinomial.0}, \code{zeroinflated.nbinomial.1}, \code{zeroinflated.nbinomial.2}. Exponentiated random marginals represent odds ratios for models with one of the followinglikelihoods: \code{binomial}, \code{zeroinflated.binomial.0}, \code{zeroinflated.binomial.1}.
#' @references Blangiardo, Marta, and Michela Cameletti. Spatial and Spatio-temporal Bayesian Models with R-INLA. John Wiley & Sons, 2015.
#' @export
#' @examples 
#' head(spn)
#' mod1 <- inla(aan ~ f(id, model = 'bym', graph = sp.adj),
#'              E = eaan,
#'              family = 'poisson', data = spn)
#' 
## Area-specific relative-risks.
#'
#' summary(SpecificEffects(mod1))
#'
#' # or equivalent:
#' lcs <- inla.make.lincombs(id = diag(nrow(spn)),
#'                           '(Intercept)' = rep(1, nrow(spn)))
#' mod2 <- inla(aan ~ f(id, model = 'bym', graph = sp.adj),
#'              E = eaan,
#'              family = 'poisson', data = spn,
#'              lincomb = lcs)
#' summary(exp(mod2$summary.lincomb.derived[ , 2]))
#' 
#' summary(exp(mod2$summary.random[[1]][, 2]) *
#'             exp(as.numeric(mod2$summary.fixed[1])))
#' 
#' summary(exp(mod1$summary.random[[1]][, 2]) *
#'             exp(as.numeric(mod1$summary.fixed[1])))
#' 
#' # SMR for comparison
#' summary(spn$aan / spn$eaan)
#' 
#' ## Area-specific relative risks relative to the mean relative risk.
#' summary(RandomEffects(mod1))
#' summary(exp(mod2$summary.lincomb.derived[ , 2]) /
#'             exp(as.numeric(mod2$summary.fixed[1])))
#'
SpecificEffects <- function(..., expo = TRUE, rnd = 3) {
    mods <- list(...)
    nms <- deparse(substitute(list(...)))
    if (any(grepl("list\\(", nms))) {
        nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms)-1)), ", "))
    }
    ses <- sapply(mods,
                 function(x) as.numeric(RandomEffects(x, expo = expo) *
                                            FixedEffects(x, expo = expo)$mean))
    colnames(ses) <- nms
    ses
}