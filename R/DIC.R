#' Deviance information criteria
#' @description Sort and display the Deviance information criteria of INLA models.
#' @param mods \code{\link{list}} of objects of \code{\link{class}} \code{inla} with computed DIC.
#' @param decreasing logical. If \code{FALSE} (default), DIC's are displayed in increasing order.
#' @return \code{\link{matrix}} with models' DIC.
#' @references Blangiardo, M., Cameletti, M., Baio, G., & Rue, H. (2013). Spatial and spatio-temporal models with R-INLA. Spatial and spatio-temporal epidemiology, 7, 39-55.
#' @export
#' @examples 
#' data(sp)
#' 
#' mod1 <- inla(aan ~ offset(log(eaan)) +
#'                  f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn,
#'              control.compute = list(dic = TRUE))
#' 
#' mod2 <- inla(aan ~ shvn + offset(log(eaan))+
#'                  f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn,
#'              control.compute = list(dic = TRUE))
#' 
#' DIC(list(mod1 = mod1, mod2 = mod2))
DIC <- function(mods, decreasing = F) {
    dics <- sapply(mods, function(x) x$dic$dic)
    names(dics) <- names(mods)
    dics <- cbind(sort(dics, decreasing = decreasing))
    colnames(dics) <- 'DIC'
    dics
}
