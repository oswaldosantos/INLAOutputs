#' Standard deviation of hyperparameters
#' @description Transformation of precision to standard deviation of hyperparameters.
#' @param model object of \code{\link{class}} \code{inla}.
#' @param summarise \code{\link{logical}}. If \code{TRUE} (default), the posterior mean of the standard deviation is returned.
#' @param quantiles \code{\link{numeric}} \code{\link{vector}} to indicate the quantiles to be computed.
#' @param rnd integer indicating the number of decimal places to be used.
#' @return \code{\link{matrix}} with summary statistics.
#' @references Blangiardo, Marta, and Michela Cameletti. Spatial and Spatio-temporal Bayesian Models with R-INLA. John Wiley & Sons, 2015.
#' @export
#' @examples
#' data(sp)
#' mod <- inla(aan ~ shvn + f(id, model = 'bym', graph = sp.adj),
#'             E = eaan, family = 'poisson', data = spn)
#' Prec2SD(mod)
Prec2SD <- function(model, summarise = TRUE, sd = TRUE, quantiles = c(.025, .975), rnd = 3) {
  hypers <- model$marginals.hyperpar
  for (i in 1:length(hypers)) {
    if (grepl("Precision", names(hypers)[[i]])) {
      hypers[[i]] <- round(inla.tmarginal(function(x) sqrt(1/x), hypers[[i]]), rnd)
      names(hypers)[[i]] <- gsub("Precision", "SD", names(hypers)[[i]])
    }
    if (summarise) {
      tmp <- inla.emarginal(function(x) x, hypers[[i]])
      names(tmp) <- "mean"
      if (sd) {
        sd0 <- inla.emarginal(function(x) c(x, x^2), hypers[[i]])
        sd1 <- sqrt(sd0[2] - sd0[1]^2)
        tmp <- c(tmp, sd1)
        names(tmp)[2] <- "sd"
      }
      if (!is.null(quantiles)) {
        qts <- inla.qmarginal(quantiles, hypers[[i]])
        ltmp <- length(tmp)
        tmp <- c(tmp, qts)
        names(tmp)[-c(1:ltmp)] <- paste0("qt", quantiles)
      }
      hypers[[i]] <- round(tmp, rnd)
    }
  }
  hypers <- sapply(hypers, function(x) x)
  if (is.matrix(hypers)) {
    hypers <- t(hypers)
  }
  hypers
} 