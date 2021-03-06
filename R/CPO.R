#' Conditional predictive ordinates
#' @description Sort and display the sum of the log of CPO's of INLA models.
#' @param ... objects of \code{\link{class}} \code{inla} with computed CPO's.
#' @param failure_cutoff value between 0 and 1. CPO's with a failure score greater than \code{failure_cutoff} are considered unreliable and are ignored.
#' @param decreasing logical. If \code{TRUE} (default), CPO's are displayed in decreasing order (higher values indicate better prediction).
#' @param rnd integer indicating the number of decimal places to be used.
#' @return \code{\link{matrix}} with the sum of the log of CPO's and the proportion of failures of each models' CPO.
#' @details \code{control.compute = list(cpo = TRUE)} must be used within \code{inla} function.
#' @references Blangiardo, M., Cameletti, M., Baio, G., & Rue, H. (2013). Spatial and spatio-temporal models with R-INLA. Spatial and spatio-temporal epidemiology, 7, 39-55.
#' @export
#' @examples 
#' data(sp)
#' 
#' mod1 <- inla(aan ~ f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn, E = eaan,
#'              control.compute = list(cpo = TRUE))
#' 
#' mod2 <- inla(aan ~ shvn + f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn,  E = eaan,
#'              control.compute = list(cpo = TRUE))
#' 
#' CPO(mod1, mod2)
#' 
CPO <- function(..., failure_cutoff = 0, decreasing = TRUE, rnd = 3) {
    mods <- list(...)
    nms <- deparse(substitute(list(...)))
    if (any(grepl("list\\(", nms))) {
        nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms)-1)), ", "))
    }
    cpos <- t(sapply(mods,
                     function(x) {
                         idx <- x$cpo$failure <= failure_cutoff
                         c("sum(log(cpo))" = sum(log(x$cpo$cpo[idx])),
                           "failures" = 1 - mean(idx))}))
    if (length(mods) > 1) {
        rownames(cpos) <- nms
        cpos <- apply(cpos[order(cpos[, 1], decreasing = decreasing), ], 2,
              function(x) round(x, rnd))
    }
    round(cpos, rnd)
}