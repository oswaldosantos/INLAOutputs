#' Root mean square error
#' @description Calculates the RMSE of INLA models.
#' @param ... objects of \code{\link{class}} \code{inla}  with computed fitted marginals.
#' @param decreasing logical. If \code{FALSE} (default), DIC's are displayed in increasing order.
#' @return na.rm \code{\link{logical}} if \code{FALSE}, NA's are note removed.
#' @export
#' @examples 
#' data(sp)
#' 
#' mod1 <- inla(aan ~ f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn, E = eaan,
#'              control.predictor=list(link = 1, compute = TRUE))
#' 
#' mod2 <- inla(aan ~ shvn + f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn, E = eaan,
#'              control.predictor=list(link = 1, compute = TRUE))
#'
RMSE <- function(..., observed = NULL, decreasing = FALSE, na.rm = FALSE) {
    mods <- list(...)
    nms <- deparse(substitute(list(...)))
    if (any(grepl("list\\(", nms))) {
        nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms)-1)), ", "))
    }
    rmse <- sapply(mods,
                   function(x) sqrt(mean((x$summary.fitted.values$mean - 
                                              observed)^2, na.rm = na.rm)))
    names(rmse) <- nms
    rmse <- sort(rmse, decreasing = decreasing)
    rmse
}
