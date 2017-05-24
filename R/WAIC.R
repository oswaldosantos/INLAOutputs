#' Watanabe-Akaike information criteria
#' @description Sort and display the Watanabe-Akaike information criteria of INLA models.
#' @param ... objects of \code{\link{class}} \code{inla}  with computed WAIC.
#' @param decreasing logical. If \code{FALSE} (default), WAIC's are displayed in increasing order.
#' @return \code{\link{matrix}} with models' WAIC.
#' @references Gelman, Andrew, Jessica Hwang, and Aki Vehtari. "Understanding predictive information criteria for Bayesian models." Statistics and Computing 24.6 (2014): 997-1016.
#' @export
#' @examples 
#' data(sp)
#' 
#' mod1 <- inla(aan ~ offset(log(eaan)) +
#'                  f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn,
#'              control.compute = list(waic = TRUE))
#' 
#' mod2 <- inla(aan ~ shvn + offset(log(eaan))+
#'                  f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn,
#'              control.compute = list(waic = TRUE))
#' 
#' WAIC(mod1, mod2)
#' 
WAIC <- function(..., decreasing = F) {
    mods <- list(...)
    nms <- deparse(substitute(list(...)))
    if (any(grepl("list\\(", nms))) {
        nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms)-1)), ", "))
    }
    waics <- sapply(mods, function(x) x$waic$waic)
    names(waics) <- nms
    waics <- cbind(sort(waics, decreasing = decreasing))
    colnames(waics) <- 'WAIC'
    waics
}
