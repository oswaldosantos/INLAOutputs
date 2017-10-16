#' Posterior predictive p-value
#' @description Computes de probability Pr(predicted_i <= observed_i | observed).
#' @param ... objects of \code{\link{class}} \code{inla} with computed fitted marginals.
#' @param observed \code{\link{vector}} observed values.
#' @param cutoff \code{\link{vector}} with lower and upper values to define the tails of the p-values cumulative distribution. Default: c(0.1, 0.9).
#' @param decreasing logical. If \code{FALSE} (default), models are displayed in increasing order, according to the proportion of values in both tails (the first model has the best fit).
#' @return \code{\link{list}}. The first element contains the proportion of p-values in the lower and upper tails. The second element contains the p-values.
#' @references Blangiardo, Marta, and Michela Cameletti. Spatial and Spatio-temporal Bayesian Models with R-INLA. John Wiley & Sons, 2015.
#' @export
#' @examples 
#' data(sp)
#' 
#' mod <- inla(aan ~ f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn, E= eaan,
#'              control.predictor=list(link = 1, compute = TRUE))
#'
#' PredPValue(mod, observed = spn$aan)
PredPValue <- function(..., observed = NULL, cutoff = c(0.1, 0.9), decreasing = FALSE) {
    mods <- list(...)
    nms <- deparse(substitute(list(...)))
    if (any(grepl("list\\(", nms))) {
        nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms)-1)), ", "))
    }
    ps <- list()
    p_tails <- matrix(NA, ncol = 2, nrow = length(mods))
    colnames(p_tails) <- c(paste("lower", cutoff[1]),
                           paste("upper", cutoff[2]))
    for (i in 1:length(mods)) {
        n <- length(mods[[i]]$marginals.fitted.values)
        p <- c()
        for(i2 in 1:n) {
            p[i2] <- inla.pmarginal(
                q = observed[i2],
                marginal = mods[[i]]$marginals.fitted.values[[i2]])
        }
        p_tails[i, ] <- round(c(sum(p <= .1)/ n, sum(p >= .9)/ n), 4)
        ps[[i]] <- p
    }
    if (length(mods) > 1) {
        rownames(p_tails) <- names(ps) <- nms
        idx <- order(apply(p_tails, 1, sum), decreasing = decreasing)
        p_tails <- p_tails[idx, ]
        ps <- sapply(ps, function(x) x)
    }
    if (length(mods) == 1) {
     ps <- unlist(ps)
    }
    list(p_tails = p_tails, p = ps)
}

