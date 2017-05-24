#' Spatial effects in excess
#' @description Probability of the area-specific relative risk (odds ratio) being > 1. Mean relative risk (odds ratio) is the reference category.
#' @param ... object of \code{\link{class}} \code{inla} with outputs from a BYM model.
#' @param rnd integer indicating the number of decimal places (round) or significant digits (signif) to be used.
#' @return \code{\link{matrix}} with as many rows as areas and as many columns as models in \code{mod}. 
#' @details Spatial effects in excess are represented in terms of relative risks for models with one of the following likelihoods: \code{poisson}, \code{zeroinflated.poisson.0}, \code{zeroinflated.poisson.1}, \code{zeroinflated.poisson.2}, \code{nbinomial}, \code{zeroinflated.nbinomial.0}, \code{zeroinflated.nbinomial.1}, \code{zeroinflated.nbinomial.2}. Sptial effects in excess are represented in terms of odds ratios for models with one of the followinglikelihoods: \code{binomial}, \code{zeroinflated.binomial.0}, \code{zeroinflated.binomial.1}.
#' @references Blangiardo, Marta, and Michela Cameletti. Spatial and Spatio-temporal Bayesian Models with R-INLA. John Wiley & Sons, 2015.
#' @export
#' @examples 
#' data(sp)
#' 
#' mod1 <- inla(aan ~ f(id, model = 'bym', graph = sp.adj),
#'              E = eaan,
#'              family = 'poisson', data = spn)
#' 
#' mod2 <- inla(aan ~ shvn + f(id, model = 'bym', graph = sp.adj),
#'              E = eaan,
#'              family = 'poisson', data = spn)
#'
#' see <- SpatialEffectsExcess(mod1, mod2)
#' summary(see)
SpatialEffectsExcess <- function(..., rnd = 3) {
  mods <- list(...)
  nms <- deparse(substitute(list(...)))
  if (any(grepl("list\\(", nms))) {
    nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms)-1)), ", "))
  }
  n <- nrow(mods[[1]]$model.matrix)
  res <- matrix(rep(NA, length(mods) * n), nrow = n)
  for (i in 1:length(mods)) {
    res[ , i] <- unlist(lapply(mods[[i]]$marginals.random[[1]][1:n],
                               function(x) {1 - inla.pmarginal(0, x)}))
  }
  names(res) <- nms
  round(res, rnd)
}