#' Root mean squared error
#' @description Calculates the RMSE of INLA models.
#' @param ... objects of \code{\link{class}} \code{inla}  with computed fitted marginals.
#' @param decreasing logical. If \code{FALSE} (default), RMSE's are displayed in increasing order.
#' @param rnd integer indicating the number of decimal places to be used.
#' @return \code{\link{vector}} with RMSE for each model.
#' @export
#' @examples 
#' data(sp)
#' 
#' mod1 <- inla(aan ~ f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn, E = eaan,
#'              control.predictor=list(link = 1, compute = TRUE))
#' 
#' mod2 <- inla(aan ~ shvn + f(id, model = 'bym', graph = sp.adj),
#'              family = 'poisson', data = spn, E = eaan,
#'              control.predictor=list(link = 1, compute = TRUE))
#' RMSE(mod1, mod2, observed = spn$aan / spn$eaan)
RMSE <- function(..., decreasing = FALSE, rnd = 3) {
    mods <- list(...)
    nms <- deparse(substitute(list(...)))
    if (any(grepl("list\\(", nms))) {
        nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms)-1)), ", "))
    }
    rmse <- c()
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
        
        e_rex <- regexpr("E = [[:alnum:]|[:punct:]]+,", call)
        e_string <- substring(call,
                              e_rex + 4,
                              e_rex + attributes(e_rex)[[1]] - 2)
        dat <- eval(parse(text = data_string))
        y <- dat[, y_string]
        if (!is.null(e_string)) {
            y <- y / dat[, e_string]
        }
        y_pred <- mods[[i]]$summary.fitted.values$mean
        rmse <- c(rmse, sqrt(mean((y_pred - y)^2)))
    }
    if (length(mods) > 1) {
        names(rmse) <- nms   
    }
    rmse <- sort(rmse, decreasing = decreasing)
    round(rmse, rnd)
}
