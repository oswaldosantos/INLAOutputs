#' Random effects
#' @description Alternative display of computed random marginals.
#' @param ... objects of \code{\link{class}} \code{inla} with random marginals. Only provide more than one object if the number of aggregation units is the same for all random marginals sets among all models (i.e. when the random model (iid, BYM, etc.) is the same for all objects).
#' @param expo logical. If \code{TRUE} (default), summary statistics are expoentiated.
#' @param rnd integer indicating the number of decimal places (round) or significant digits (signif) to be used.
#' @return If \code{...} is an object with more than one random model and these differ in the number of aggregation units (i.e. a multilevel model with more than two levels of different size), a list with as many elements as random models. Otherwise, a \code{\link{data.frame}} with as many rows as aggregation units and as many columns as random mmodels If \code{...} is a set of one or more obejcts and all have a BYM random model, the \code{data.frame} will have only the first half of rows present in the \code{summary.random} element of the objects (iid plus BYM residuals); the second half has only the BYM residuals.  
#' @details Exponentiated values represent the posterior mean of unit-specific relative risks (odds ratio), relative to the mean relative risk (odds ratio). Exponentiated random marginals represent relative risks for models with one of the following likelihoods: \code{poisson}, \code{zeroinflated.poisson.0}, \code{zeroinflated.poisson.1}, \code{zeroinflated.poisson.2}, \code{nbinomial}, \code{zeroinflated.nbinomial.0}, \code{zeroinflated.nbinomial.1}, \code{zeroinflated.nbinomial.2}. Exponentiated random marginals represent odds ratios for models with one of the followinglikelihoods: \code{binomial}, \code{zeroinflated.binomial.0}, \code{zeroinflated.binomial.1}.
#' @references Blangiardo, Marta, and Michela Cameletti. Spatial and Spatio-temporal Bayesian Models with R-INLA. John Wiley & Sons, 2015.
#' @export
#' @examples 
#' data(sp)
#' 
#' mod1 <- inla(aan ~ f(id, model = 'bym', graph = sp.adj),
#'              E = eaan,
#'              family = 'poisson', data = spn)
#' summary(RandomEffects(mod1))
#' 
RandomEffects <- function (..., expo = TRUE, rnd = 3) 
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
            if (mods[[i]]$model.random[j] == "BYM model") {
                if (!expo) {
                    tmp <- mods[[i]]$summary.random[[j]]$mean
                    res2[[j]] <- unname(unlist(tmp[1:(length(tmp)/2)]))
                } else {
                    tmp <- mods[[i]]$marginals.random[[j]]
                    res2[[j]] <- unname(unlist(sapply(tmp[1:(length(tmp)/2)], 
                                                      function(x) {
                                                          inla.emarginal(exp, x)
                                                      })))
                }
            } else {
                if (!expo) {
                    tmp <- mods[[i]]$summary.random[[j]]$mean
                    res2[[j]] <- unname(unlist(tmp[1:length(tmp)]))
                } else {
                    tmp <- mods[[i]]$marginals.random[[j]]$mean
                    res2[[j]] <- unname(unlist(sapply(tmp[1:length(tmp)], 
                                                      function(x) {
                                                          inla.emarginal(exp, x)
                                                      })))
                }
            }
        }
        res[[i]] <- res2
    }
    re_nms <- sapply(mods, function(x) names(x$summary.random))
    if (length(mods) > 1) {
        re_nms <- unlist(mapply(function(x, y) paste0(x, "_", 
                                                      y), nms, re_nms))
    }
    res <- res[[1]]
    if (length(res) == 1) {
        res <- unlist(res)
        return(round(res, rnd))
    }
    names(res) <- re_nms
    res <- lapply(res, function(x) round(x, rnd))
    try({
        return(as.data.frame(res))
    }, silent = TRUE)
    res
}