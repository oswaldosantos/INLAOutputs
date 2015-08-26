#' Variance spatially explained
#' @description Proportion of variance explained by the spatially structured component.
#' @param mod object of \code{\link{class}} \code{inla} or list of objects of \code{\link{class}} \code{inla}. Objects must have outputs from a BYM model.
#' @param sims number of random values generated for each area, from the corresponding marginal posterior distribution of the unstructured residual component.
#' @param rnd integer indicating the number of decimal places (round) or significant digits (signif) to be used.
#' @return Proportion of variance explained by the spatially structured component, for each of the models in \code{mods}.
#' @references Blangiardo, Marta, and Michela Cameletti. Spatial and Spatio-temporal Bayesian Models with R-INLA. John Wiley & Sons, 2015.
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
#' 
#' VarSpatiallyExplained(models)
VarSpatiallyExplained <- function(mod = NULL, sims = 1000, rnd = 3) {
    res <- c()
    if (class(mod) == 'inla') {
        mod <- list(mod)
    }
    for (i in 1:length(mod)) {
        n <- nrow(mod[[1]]$model.matrix)
        mat.marg <- matrix(NA, nrow = n, ncol = sims)
        m <- mod[[i]]$marginals.random[[1]]
        for (j in 1:n) {
            u <- m[[n + j]]
            mat.marg[j, ] <- inla.rmarginal(sims, u)}
        var.rr.spatial <- mean(apply(mat.marg, 2, var))
        iid <- grep('iid', names(mod[[i]]$marginals.hyper), value = TRUE)
        var.rr.het <- inla.emarginal(
            function(x) {1 / x},
            mod[[i]]$marginals.hyper[[iid]])
        res[i] <- var.rr.spatial / (var.rr.spatial + var.rr.het)
    }
    round(res, rnd)
}