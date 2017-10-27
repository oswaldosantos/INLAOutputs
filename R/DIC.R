#' Deviance information criteria
#' @description Sort and display the Deviance information criteria of INLA models.
#' @param ... objects of \code{\link{class}} \code{inla}  with computed DIC.
#' @param decreasing logical. If \code{FALSE} (default), DIC's are displayed in increasing order.
#' @param rnd integer indicating the number of decimal places to be used.
#' @return \code{\link{matrix}} with models' DIC, mean deviance and effective number of paramenters.
#' @details \code{control.compute = list(dic = TRUE)} must be used within \code{inla} function.
#' @references Blangiardo, M., Cameletti, M., Baio, G., & Rue, H. (2013). Spatial and spatio-temporal models with R-INLA. Spatial and spatio-temporal epidemiology, 7, 39-55.
#' @export
#' @examples 
#' data(sp)
#' 
#' mod1 <- inla(aan ~ f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn, E = eaan,
#'              control.compute = list(dic = TRUE))
#' 
#' mod2 <- inla(aan ~ shvn + f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn, E = eaan,
#'              control.compute = list(dic = TRUE))
#' 
#' DIC(mod1, mod2)
#' 
DIC <- function(..., decreasing = FALSE, rnd = 3) {
    mods <- list(...)
    nms <- deparse(substitute(list(...)))
    if (any(grepl("list\\(", nms))) {
        nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms)-1)), ", "))
    }
    dics <- t(sapply(mods,
                   function(x) unlist(x$dic[c("dic", "mean.deviance", "p.eff")])))
    rownames(dics) <- nms
    apply(dics[order(dics[, 1], decreasing = decreasing), ], 2,
          function(x) round(x, rnd))
}
