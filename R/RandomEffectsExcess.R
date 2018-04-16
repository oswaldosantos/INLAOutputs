#' Rnadom effects in excess
#' @description @description Probability of random effects being higher than a cutoff value.
#' @param ... objects of \code{\link{class}} \code{inla} with random marginals.
#' @param cutoff value above which random effects would be in excess. The cutoff is used for untransformed marginal posteriors, so \code{cutoff = 0} (default) is used to calculate the probability of relative risk (odds ratio) > 1.
#' @param rnd integer indicating the number of decimal places to be used.
#' @return \code{\link{vector}} if only one model with one random effect is evaluated; \code{\link{data.frame}} or \code{\link{list}} otherwise.
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
#' see <- RandomEffectsExcess(mod1, mod2)
#' summary(see)
RandomEffectsExcess <- function (..., cutoff = 0, rnd = 3) 
{
    mods <- list(...)
    nms <- deparse(substitute(list(...)))
    if (any(grepl("list\\(", nms))) {
        nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms) - 
                                                 1)), ", "))
    }
    res <- list()
    for (i in 1:length(mods)) {
        res2 <- list()
        for (j in 1:length(mods[[i]]$marginals.random)) {
            tmp <- mods[[i]]$marginals.random[[j]]
            if (mods[[i]]$model.random[j] == "BYM model" |
                mods[[i]]$model.random[j] == "BYM2 model") {
                res2[[j]] <- unname(unlist(sapply(tmp[1:(length(tmp)/2)], 
                                                  function(x) {
                                                      1 - inla.pmarginal(cutoff, x)
                                                  })))
            }
            else {
                res2[[j]] <- unname(unlist(sapply(tmp[1:length(tmp)], 
                                                  function(x) {
                                                      1 - inla.pmarginal(cutoff, x)
                                                  })))
            }
        }
        res[[i]] <- res2
    }
    re_nms <- sapply(mods, function(x) names(x$summary.random))
    if (length(mods) > 1) {
        re_nms <- unlist(mapply(function(x, y) paste0(x, "_", 
                                                      y), nms, re_nms))
    }
    if (length(nms) == 1) {
        res <- res[[1]]
    }
    if (length(res) == 1) {
        res <- unlist(res)
        return(round(res, rnd))
    }
    if (all(sapply(res, length) == 1)) {
        res <- lapply(res, unlist)
        res <- lapply(res, function(x) round(x, rnd))
        names(res) <- re_nms
        if (sum(diff(sapply(res, length))) == 0) {
            return(as.data.frame(res))
        }
        return(res)
    }
    if (is.list(res[[1]])) {
        res2 <- res[[1]]
        for (i in 2:length(res)) {
            res2 <- c(res2, res[[i]])
        }
        res <- res2
    }
    names(res) <- re_nms
    if (sum(diff(sapply(res, length))) == 0) {
        return(as.data.frame(res))
    }
    res
}

