#' Fitted values in excess
#' @description Probability of fitted values being higher than a cutoff value.
#' @param ... objects of \code{\link{class}} \code{inla} with fitted values.
#' @param cutoff value above which fitted values would be in excess.
#' @param rnd integer indicating the number of decimal places to be used.
#' @return \code{\link{vector}} if only one model with fitted values is evaluated; \code{\link{data.frame}} or \code{\link{list}} otherwise.
#' @references Blangiardo, Marta, and Michela Cameletti. Spatial and Spatio-temporal Bayesian Models with R-INLA. John Wiley & Sons, 2015.
#' @export
#' @examples 
#' data(sp)
#' 
#' mod1 <- inla(aan ~ f(id, model = 'bym', graph = sp.adj),
#'              E = eaan,
#'              family = 'poisson', data = spn,
#'              control.predictor = list(compute = TRUE))
#' 
#' mod2 <- inla(aan ~ shvn + f(id, model = 'bym', graph = sp.adj),
#'              E = eaan,
#'              family = 'poisson', data = spn,
#'              control.predictor = list(compute = TRUE))
#'
#' see <- RandomEffectsExcess(mod1, mod2, cutoff = 1)
#' summary(see)
FittedExcess <- function (..., cutoff = 1, rnd = 3) 
{
    mods <- list(...)
    nms <- deparse(substitute(list(...)))
    if (any(grepl("list\\(", nms))) {
        nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms) - 1)), ", "))
    }
    res <- list()
    for (i in 1:length(mods)) {
        res[[i]] <- sapply(mods[[i]]$marginals.fitted.values,
                           function(x) 1 - inla.pmarginal(cutoff, x))
    }
    if (length(nms) == 1) {
        return(unname(res[[1]]))
    }
    res <- lapply(res, function(x) unname(round(x, rnd)))
    names(res) <- nms
    if (sum(diff(sapply(res, length))) == 0) {
        return(as.data.frame(res))
    }
    res
}