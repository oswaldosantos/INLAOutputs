#' Priority index.
#' @description Priority index (PI) given by Fitted or random effects weighted by the probability of the effects being higher than a cutoff value. PI values are scaled shuch tath 0 <= PI <= 100.
#' @param ... objects of \code{\link{class}} \code{inla} with fitted values or random marginals.
#' @param  effect string indicating if the priority index should be calculated using fitted values \code{"fitted"} or random effects \code{"random"}.
#' @param cutoff value above which fitted values or random effects would be in excess. If \code{effect = "random"}, the cutoff is used for untransformed marginal posteriors, so \code{cutoff = 0} (default) is used to calculate the probability of relative risk (odds ratio) > 1.
#' @param rescale_by column name or column index of the data used to fit the model. It works only when \code{effect = "fitted"}. See \code{Details}.
#' @return PI for each observation..PI values will be returned in a \code{\link{vector}} only if one model with fitted values (or one random effect) is evaluated; \code{\link{data.frame}} or \code{\link{list}} otherwise.
#' @details
#' 
#' \code{rescale_by} is intended for spatiotemporal models, where \code{rescale_by} is the temporal variable. The result is a priority index rescaled within each temporal unit. If more than one model is provided to the first argument \code{...}, all of them must model the same spatiotemporal units. 
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
PriorityIndex <- function(..., effect = NULL, cutoff = NULL, rescale_by = NULL, rnd = 1) {
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
            priority_idx[[m]] <- priority_idx[[m]]/max(priority_idx[[m]]) * 100
            if (!is.null(rescale_by)) {
                call <- gsub("\\s+", " ", paste(mods[[m]]$call, collapse = ""))
                if (grepl("offset\\(", call)) {
                    stop("Use E instead of offset")
                }
                data_rex <- regexpr("data = [[:alnum:]|[:punct:]]+", call)
                data_call <- substring(call, data_rex, data_rex +
                                           attributes(data_rex)[[1]] - 2)
                data_string <- substring(data_call, 8)
                data <- as.data.frame(eval(parse(text = data_string)))
                tmp <- tapply(priority_idx[[m]],
                              data[, rescale_by],
                              function(x) x / max(x) * 100)
                priority_idx[[m]] <- as.vector(t(Reduce(cbind, tmp)))
            }
            priority_idx[[m]] <- round(priority_idx[[m]], rnd)
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
