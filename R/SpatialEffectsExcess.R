#' Spatial effects in excess
#' @description Probability of the area-specific relative risk (odds ratio) being > 1. Mean relative risk (odds ratio) is the reference category.
#' @param mod object of \code{\link{class}} \code{inla} or list of objects of \code{\link{class}} \code{inla}. Objects must have outputs from a BYM model.
#' @param mod.names \code{\link{character}} \code{\link{vector}} with names for the exponentiated random marginals of each model.
#' @param rnd integer indicating the number of decimal places (round) or significant digits (signif) to be used.
#' @return \code{\link{matrix}} with as many rows as areas and as many columns as models in \code{mod}. 
#' @details Spatial effects in excess are represented in terms of relative risks for models with one of the following likelihoods: \code{poisson}, \code{zeroinflated.poisson.0}, \code{zeroinflated.poisson.1}, \code{zeroinflated.poisson.2}, \code{nbinomial}, \code{zeroinflated.nbinomial.0}, \code{zeroinflated.nbinomial.1}, \code{zeroinflated.nbinomial.2}. Sptial effects in excess are represented in terms of odds ratios for models with one of the followinglikelihoods: \code{binomial}, \code{zeroinflated.binomial.0}, \code{zeroinflated.binomial.1}.
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
#' see <- SpatialEffectsExcess(models, mod.names = c('see.mod1', 'see.mod2'))
#' summary(see)
SpatialEffectsExcess <- function(mod = NULL, mod.names = NULL, rnd = 3) {
    if (class(mod) == 'inla') {
        mod <- list(mod)
    }
    n <- nrow(mod[[1]]$model.matrix)
    res <- matrix(rep(NA, length(mod) * n), nrow = n)
    for (i in 1:length(mod)) {
        res[ , i] <- unlist(lapply(mod[[i]]$marginals.random[[1]][1:n],
                                   function(x) {1 - inla.pmarginal(0, x)}))
    }
    if (!is.null(mod.names)) {
        colnames(res) <- mod.names
    }
    round(res, rnd)
}