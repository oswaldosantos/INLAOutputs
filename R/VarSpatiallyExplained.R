#' Variance spatially explained
#' @description Proportion of variance explained by the spatially structured component.
#' @param ... objects of \code{\link{class}} \code{inla} with outputs from a BYM model.
#' @param sims number of random values generated for each area, from the corresponding marginal posterior distribution of the unstructured residual component.
#' @param rnd integer indicating the number of decimal places (round) or significant digits (signif) to be used.
#' @return Proportion of variance explained by the spatially structured component, for each model.
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
#' VarSpatiallyExplained(mod1, mod2)
VarSpatiallyExplained <- function(..., sims = 1000, rnd = 3) {
  mods <- list(...)
  nms <- deparse(substitute(list(...)))
  if (any(grepl("list\\(", nms))) {
    nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms)-1)), ", "))
  }
  res <- c()
  for (i in 1:length(mods)) {
    n <- nrow(mods[[1]]$model.matrix)
    mat.marg <- matrix(NA, nrow = n, ncol = sims)
    m <- mods[[i]]$marginals.random[[1]]
    for (j in 1:n) {
      u <- m[[n + j]]
      mat.marg[j, ] <- inla.rmarginal(sims, u)}
    var.rr.spatial <- mean(apply(mat.marg, 2, var))
    iid <- grep('iid', names(mods[[i]]$marginals.hyper), value = TRUE)
    var.rr.het <- inla.emarginal(
      function(x) {1 / x},
      mods[[i]]$marginals.hyper[[iid]])
    res[i] <- var.rr.spatial / (var.rr.spatial + var.rr.het)
  }
  names(res) <- nms
  round(res, rnd)
}