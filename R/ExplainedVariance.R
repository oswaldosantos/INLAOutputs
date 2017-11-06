#' Variance explained by each variance component
#' @description Proportion of variance explained by each variance component.
#' @param ... objects of \code{\link{class}} \code{inla} with with one or more random effects.
#' @param n number of random values generated from hyperparameters.
#' @param rnd integer indicating the number of decimal places to be used.
#' @return \code{\link{data.frame}} with the proportion of variance explained by each variance component.
#' @details The argument \code{scale.model} must be TRUE within \code{f} function.
#' @references Blangiardo, Marta, and Michela Cameletti. Spatial and Spatio-temporal Bayesian Models with R-INLA. John Wiley & Sons, 2015.
#' @export
#' @examples 
#' data(sp)
#' 
#' mod1 <- inla(aan ~ f(id, model = 'bym', graph = sp.adj,
#'                      scale.model = TRUE),
#'              E = eaan,
#'              family = 'poisson', data = spn)
#' 
#' mod2 <- inla(aan ~ shvn + f(id, model = 'bym', graph = sp.adj,
#'                             scale.model = TRUE),
#'              E = eaan,
#'              family = 'poisson', data = spn)
#' 
#' ExplainedVariance(mod1, mod2)
ExplainedVariance <- function(..., n = 1e4, rnd = 3) {
    mods <- list(...)
    nms <- deparse(substitute(list(...)))
    if (any(grepl("list\\(", nms))) {
        nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms) - 
                                                 1)), ", "))
    }
    hs <- list()
    for (i in 1:length(mods)) {
        hyper_sample <- inla.hyperpar.sample(n, mods[[i]])
        ran_names <- names(mods[[i]]$summary.random)
        idx <- apply(sapply(ran_names,
                            function(x) grepl(x, colnames(hyper_sample))), 1,
                     function(x) any(x))
        hyper_sample <- hyper_sample[, idx]
        hs2 <- c()
        for (j in 1:ncol(hyper_sample)) {
            hs2 <- c(hs2, mean((1 / hyper_sample[, j]) /
                                   apply(hyper_sample, 1, function(x) sum(1/x))))
        }
        names(hs2) <- colnames(hyper_sample)
        hs[[i]] <- hs2
    }
    prec_nms <- unique(unlist(lapply(hs, names)))
    hs3 <- matrix(nrow = length(prec_nms), ncol = length(mods))
    rownames(hs3) <- prec_nms
    for (i in 1:length(mods)) {
        hs3[names(hs[[i]]), i] <- round(hs[[i]], rnd)
    }
    hs3 <- data.frame(prec_nms, hs3)
    row.names(hs3) <- NULL
    names(hs3) <- c("variance_component", nms)
    hs3$variance_component <- gsub("Precision for ", "", hs3$variance_component)
    hs3
}
