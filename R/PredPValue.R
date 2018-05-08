#' Posterior predictive p-value
#' @description Computes de probability Pr(predicted_i <= observed_i | observed).
#' @param ... objects of \code{\link{class}} \code{inla} with computed fitted marginals.
#' @param cutoff \code{\link{vector}} with lower and upper values to define the tails of the p-values cumulative distribution. Default: c(0.1, 0.9).
#' @param decreasing logical. If \code{FALSE} (default), models are displayed in increasing order, according to the proportion of values in both tails (the first model has the best fit).
#' @param rnd integer indicating the number of decimal places to be used.
#' @return \code{\link{list}}. The first element contains the proportion of p-values in the lower and upper tails. The second element contains the p-values.
#' @details \code{control.predictor = list(link = 1, compute = TRUE)} must be used within \code{inla} function.
#' @references Blangiardo, Marta, and Michela Cameletti. Spatial and Spatio-temporal Bayesian Models with R-INLA. John Wiley & Sons, 2015.
#' @export
#' @examples 
#' data(sp)
#' 
#' mod <- inla(aan ~ f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn, E= eaan,
#'              control.predictor = list(link = 1, compute = TRUE))
#'
#' p_vals <- PredPValue(mod)
#' p_vals$p_tails
PredPValue <- function(..., cutoff = c(0.1, 0.9), decreasing = FALSE, rnd = 3) {
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
        
        call <- gsub("\\s+", " ", paste(mods[[i]]$call, collapse = ""))
        data_rex <- regexpr("data = [[:alnum:]|[:punct:]]+,", call)
        data_string <- substring(call,
                                 data_rex + 7,
                                 data_rex + attributes(data_rex)[[1]] - 2)
        y_rex <- regexpr("formula = [[:alnum:]|[:punct:]]+ ", call)
        y_string <- substring(call,
                              y_rex + 10,
                              y_rex + attributes(y_rex)[[1]] - 2)
        if(any(regexpr("E = [[:alnum:]|[:punct:]]+,", call) != -1)) {
            e_rex <- regexpr("E = [[:alnum:]|[:punct:]]+,", call)
            e_string <- substring(call, e_rex + 4, e_rex + attributes(e_rex)[[1]] - 
                                      2)
        } else {
            e_string <- NULL
        }
        dat <- as.data.frame(eval(parse(text = data_string)))
        y <- dat[, y_string]
        if (!is.null(e_string)) {
            y <- y / dat[, e_string]
        }
        n <- length(mods[[i]]$marginals.fitted.values)
        p <- c()
        for(i2 in 1:n) {
            p[i2] <- inla.pmarginal(
                q = y[i2],
                marginal = mods[[i]]$marginals.fitted.values[[i2]])
        }
        p_tails[i, ] <- round(c(sum(p <= .1)/ n, sum(p >= .9)/ n), rnd)
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

