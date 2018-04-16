#' Priority index.
#' @description @description Probability of fitted values being higher than a cutoff value.
#' @param ... objects of \code{\link{class}} \code{inla} with fitted values or random marginals.
#' @param  effect string indicating if the priority index should be calculated using fitted values \code{"fitted"} or random effects \code{"random"}.
#' @param cutoff value above which fitted values or random effects would be in excess. If \code{effect = "random"}, the cutoff is used for untransformed marginal posteriors, so \code{cutoff = 0} (default) is used to calculate the probability of relative risk (odds ratio) > 1.
#' @param rnd integer indicating the number of decimal places to be used.
#' @return \code{\link{vector}} if only one model with fitted values is evaluated; \code{\link{data.frame}} or \code{\link{list}} otherwise.
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
PriorityIndex <- function(..., effect = NULL, cutoff = NULL, rnd = 1) {
    mods <- list(...)
    nms <- deparse(substitute(list(...)))
    if (any(grepl("list\\(", nms))) {
        nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms) - 1)), ", "))
    }
    priority_idx <- list()
    for (m in 1:length(mods)) {
        if (effect == "fitted") {
            fitted <- mods[[m]]$summary.fitted.values$mean
            exc <- sapply(mods[[m]]$marginals.fitted.values,
                          function(x) 1 - inla.pmarginal(cutoff, x))
            priority_idx[[m]] <- unname(fitted * exc)
            priority_idx[[m]] <-
                round(priority_idx[[m]] / max(priority_idx[[m]]) * 100, rnd)
        }
        if (effect == "random") {
            re <- RandomEffects(mods[[m]])
            ree <- RandomEffectsExcess(mods[[m]], cutoff = cutoff)
            if (is.list(re)) {
                if (is.data.frame(re)) {
                    re <- as.list(re)
                    ree <- as.list(ree)
                }
                priority_idx2 <- re
                for (i in 1:length(re)) {
                    priority_idx2[[i]] <- re[[i]] * ree[[i]]
                    priority_idx2[[i]] <-
                        round(priority_idx2[[i]] /
                                  max(priority_idx2[[i]]) * 100, rnd)
                }
                priority_idx[[m]] <- priority_idx2
            } else {
                priority_idx[[m]] <- unname(re * ree)
                priority_idx[[m]] <-
                    round(priority_idx[[m]] / max(priority_idx[[m]]) * 100, rnd)
            }   
        }
    }
    if (length(nms) == 1) {
        return(priority_idx[[1]])
    }
    names(priority_idx) <- nms
    if (sum(diff(sapply(priority_idx, length))) == 0) {
        return(as.data.frame(priority_idx))
    }
    priority_idx
}
