#' Compare two mdoles in terms of the standard deviation of their Random effects
#' @description Compare two mdoles in terms of the standard deviation of their Random effects.
#' @param mod1 object of \code{\link{class}} \code{inla} with random marginals.
#' @param mod2 object of \code{\link{class}} \code{inla} with the same random marginals of mod1.
#' @param rnd integer indicating the number of decimal places to be used.
#' @return \code{\link{vector}} if only one model with one random effect is evaluated; \code{\link{data.frame}} or \code{\link{list}} otherwise.
#' @export
#' @examples 
#'
CompareRandomEfectSD <- function(mod1 = NULL, mod2 = NULL, rnd = 1, percentage = TRUE) {
    if(length(setdiff(mod1$model.random, mod2$model.random)) != 0) {
        stop("mod1 and mod2 must have the same random effects.")
    }
    nms <- rownames(Prec2SD(mod1))
    sds_mod1 <- Prec2SD(mod1)[, "mean"]
    sds_mod2 <- Prec2SD(mod2)[, "mean"]
    explained_fractions <- vector(length = length(sds_mod1))
    for (i in seq_along(sds_mod1)) {
        explained_fractions[i] <- (sds_mod2[i] - sds_mod1[i]) / sds_mod2[i]
    }
    if (percentage) {
        explained_fractions <- explained_fractions * 100
    }
    names(explained_fractions) <- nms
    round(explained_fractions, rnd)
}