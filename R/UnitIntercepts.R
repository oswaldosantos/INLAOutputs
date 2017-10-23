#' Unit intercepts
#' @description Fixed intercept plus random marginals of each unit.
#' @param ... objects of \code{\link{class}} \code{inla} in random marginals.
#' @param ran_marg when objects passed to \code{...} have more than one random marginal and all have a common random marginal with the same ID, this ID can be specified as a string. \code{ran_marg} can be ignored for objects with only one random marginal.
#' @param expo logical. If \code{TRUE} (default), summary statistics are expoentiated.
#' @param rnd integer indicating the number of decimal places (round) or significant digits (signif) to be used.
#' @return \code{\link{vector}} if only one model with one random effect is evaluated; \code{\link{data.frame}} or \code{\link{list}} otherwise. 
#' @references Blangiardo, Marta, and Michela Cameletti. Spatial and Spatio-temporal Bayesian Models with R-INLA. John Wiley & Sons, 2015.
#' @export
#' @examples 
#' head(spn)
#' mod1 <- inla(aan ~ f(id, model = 'bym', graph = sp.adj),
#'              E = eaan,
#'              family = 'poisson', data = spn)
#' 
#' ## Area-specific relative-risks.
#' 
#' summary(UnitIntercepts(mod1, expo = F))
#' 
# or equivalent:
#' lcs <- inla.make.lincombs(id = diag(nrow(spn)),
#'                           '(Intercept)' = rep(1, nrow(spn)))
#' mod2 <- inla(aan ~ f(id, model = 'bym', graph = sp.adj),
#'              E = eaan,
#'              family = 'poisson', data = spn,
#'              lincomb = lcs)
#' summary(mod2$summary.lincomb.derived[ , 2])
#' 
#' summary(mod2$summary.random[[1]][, 2] +
#'             as.numeric(mod2$summary.fixed[1]))
#' 
#' summary(mod1$summary.random[[1]][, 2] +
#'             as.numeric(mod1$summary.fixed[1]))
#' 
#' # SMR for comparison
#' summary(spn$aan / spn$eaan)
#' summary(UnitIntercepts(mod1))
#' summary(RandomEffects(mod1) * FixedEffects(mod1)[1, "mean"])
#' 
#' ## Area-specific relative risks compaared to the mean relative risk.
#' summary(RandomEffects(mod1, expo = FALSE))
#' summary(mod2$summary.lincomb.derived[ , 2] - 
#'             as.numeric(mod2$summary.fixed[1]))
#'
UnitIntercepts <- function(..., ran_marg = NULL, expo = TRUE, rnd = 3) {
  mods <- list(...)
  nms <- deparse(substitute(list(...)))
  if (any(grepl("list\\(", nms))) {
    nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms)-1)), ", "))
  }
  if (expo) {
    use <- sapply(mods,
                  function(x) {
                    if (!is.null(ran_marg)) {
                      re <- RandomEffects(x, expo = TRUE)[[ran_marg]] 
                    } else {
                      re <- RandomEffects(x, expo = TRUE)
                    }
                    re * FixedEffects(x, expo = TRUE)[1, "mean"]
                  })
  } else {
    use <- sapply(mods,
                  function(x) {
                    if (!is.null(ran_marg)) {
                      re <- RandomEffects(x, expo = FALSE)[[ran_marg]] 
                    } else {
                      re <- RandomEffects(x, expo = FALSE)
                    }
                    re + FixedEffects(x, expo = FALSE)[1, "mean"]
                  })
  }
  use_nms <- sapply(mods, function(x) names(x$summary.random))
  if (length(mods) > 1) {
      use_nms <- unlist(mapply(function(x, y) paste0(x, "_", 
                                                    y), nms, use_nms))
  }
  if (length(mods) == 1) {
      use <- c(unlist(use))
      return(round(use, rnd))
  }
  if (sum(diff(sapply(use, length))) == 0) {
      use <- as.data.frame(use)
      names(use) <- use_nms
      return(use)
  }
  use
}