#' Random effects
#' @description Alternative display of computed random marginals.
#' @param ... objects of \code{\link{class}} \code{inla} in random marginals.
#' @param expo logical. If \code{TRUE} (default), summary statistics are expoentiated.
#' @param rnd integer indicating the number of decimal places (round) or significant digits (signif) to be used.
#' @return \code{\link{matrix}} with as many rows as areas and as many columns as models. Exponentiated values represent the posterior mean of area-specific relative risks (odds ratio), relative to the mean relative risk (odds ratio). 
#' @details Exponentiated random marginals represent relative risks for models with one of the following likelihoods: \code{poisson}, \code{zeroinflated.poisson.0}, \code{zeroinflated.poisson.1}, \code{zeroinflated.poisson.2}, \code{nbinomial}, \code{zeroinflated.nbinomial.0}, \code{zeroinflated.nbinomial.1}, \code{zeroinflated.nbinomial.2}. Exponentiated random marginals represent odds ratios for models with one of the followinglikelihoods: \code{binomial}, \code{zeroinflated.binomial.0}, \code{zeroinflated.binomial.1}.
#' @references Blangiardo, Marta, and Michela Cameletti. Spatial and Spatio-temporal Bayesian Models with R-INLA. John Wiley & Sons, 2015.
#' @export
#' @examples 
#' data(sp)
#' 
#' mod1 <- inla(aan ~ f(id, model = 'bym', graph = sp.adj),
#'              E = eaan,
#'              family = 'poisson', data = spn)
#' summary(RandomEffects(mod1))
#' 
RandomEffects <- function(..., expo = TRUE, rnd = 3) {
  mods <- list(...)
  nms <- deparse(substitute(list(...)))
  if (any(grepl("list\\(", nms))) {
    nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms)-1)), ", "))
  }
  re <- sapply(mods,
               function(x) x$summary.random[[1]]$mean[1:nrow(x$model.matrix)])
  colnames(re) <- nms
  if (expo) {
    re <- exp(re)
  }
  re
}
