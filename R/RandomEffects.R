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
        nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms) - 1)), ", "))
    }
    RandEffs <- function(x) {
        if (all(x$model.random == "BYM model")) {
            sapply(x$summary.random, function(y) y$mean[1:(nrow(y)/2)])
        } else {
            sapply(x$summary.random, function(y) y$mean)
        }
    }
    re <- lapply(mods, RandEffs)
    if (class(re[[1]]) == "list" & expo) {
        return(lapply(re, function(x) lapply(x, exp)))
    } else if (expo) {
        re <- lapply(re, exp)
    }
    if (length(re) == 1) {
        return(data.frame(re))
    }
    re_nms <- lapply(re, function(x) colnames(x))
    re_nms <- unlist(mapply(function(x, y) paste0(x, "_", y), nms, re_nms))
    re <- as.data.frame(re)
    names(re) <- unname(re_nms)
    re
}

