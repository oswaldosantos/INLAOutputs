#' Watanabe-Akaike information criteria
#' @description Sort and display the Watanabe-Akaike information criteria of INLA models.
#' @param ... objects of \code{\link{class}} \code{inla}  with computed WAIC.
#' @param decreasing logical. If \code{FALSE} (default), WAIC's are displayed in increasing order.
#' @param rnd integer indicating the number of decimal places to be used.
#' @return \code{\link{matrix}} with models' WAIC.
#' @details \code{control.compute = list(waic = TRUE)} must be used within \code{inla} function.
#' @references Gelman, Andrew, Jessica Hwang, and Aki Vehtari. "Understanding predictive information criteria for Bayesian models." Statistics and Computing 24.6 (2014): 997-1016.
#' @export
#' @examples 
#' data(sp)
#' 
#' mod1 <- inla(aan ~ f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn, E = eaan,
#'              control.compute = list(waic = TRUE))
#' 
#' mod2 <- inla(aan ~ shvn + f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn, E = eaan,
#'              control.compute = list(waic = TRUE))
#' 
#' WAIC(mod1, mod2)
#' 
WAIC <- function(..., decreasing = FALSE, rnd = 3) {
    mods <- list(...)
    nms <- deparse(substitute(list(...)))
    if (any(grepl("list\\(", nms))) {
        nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms)-1)), ", "))
    }
    waics <- t(sapply(mods,
                     function(x) unlist(x$waic[c("waic", "p.eff")])))
    if (length(mods) > 1) {
    rownames(waics) <- nms
    waics <- apply(waics[order(waics[, 1], decreasing = decreasing), ], 2,
          function(x) round(x, rnd))
    }
    round(waics, rnd)
}
