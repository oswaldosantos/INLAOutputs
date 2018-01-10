#' Cross-validated model performance
#' @description Calculates the RMSE of INLA models using cross-validation.
#' @param ... objects of \code{\link{class}} \code{inla}  with computed fitted marginals.
#' @param k number of folds in k-fold cross-validation.
#' @param test_prop proportion of observations to compose the test set in a training/test split.
#' @param test_idx numeric index of observations to compose the test set in a training/test split.
#' @param rep number of repetitions of the cross-validation procedure.
#' @param seed number passed to \code{\link{set.seed}} to avoid variation in the data split invoked by \code{k} or \code{test_prop}.
#' @param decreasing logical. If \code{FALSE} (default), RMSE's are displayed in increasing order.
#' @param rnd integer indicating the number of decimal places to be used.
#' @return \code{\link{vector}} with mean RMSE for each model.
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
#' CV(mod1, mod2, test_prop = .1, rep = 5)
#' CV(mod1, mod2, k = 3, rep = 2)
CV <- function(..., k = NULL, test_prop = NULL, test_idx = NULL, rep = 1, seed = NULL, decreasing = FALSE, rnd = 3) {
    mods <- list(...)
    nms <- deparse(substitute(list(...)))
    if (any(grepl("list\\(", nms))) {
        nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms) - 
                                                 1)), ", "))
    }
    mods_rmse <- c()
    for (m in 1:length(mods)) {
        call <- gsub("\\s+", " ", paste(mods[[m]]$call, collapse = ""))
        if (grepl("offset\\(", call)) {
            stop("Use E instead of offset")
        }
        data_rex <- regexpr("data = [[:alnum:]|[:punct:]]+,", call)
        data_call <- substring(call,
                               data_rex,
                               data_rex + attributes(data_rex)[[1]] - 2)
        data_string <- substring(data_call, 8)
        
        y_rex <- regexpr("formula = [[:alnum:]|[:punct:]]+ ", call)
        y_string <- substring(call,
                              y_rex + 10,
                              y_rex + attributes(y_rex)[[1]] - 2)
        
        e_rex <- regexpr("E = [[:alnum:]|[:punct:]]+,", call)
        e_string <- substring(call,
                              e_rex + 4,
                              e_rex + attributes(e_rex)[[1]] - 2)
        
        call_tails <- unlist(strsplit(call, data_call))
        
        data_cv <- eval(parse(text = data_string))
        y0 <- y <- as.data.frame(data_cv[, y_string])
        if (!is.null(e_string)) {
            y <- y / data_cv[, e_string]
        }
        if (!is.null(seed)) {
            set.seed(seed)
        }
        rmse <- c()
        
        if (!is.null(k)) {
            for (i0 in 1:rep) {
                idx <- rep(sample(1:nrow(data_cv)), 2)
                folds <- round(seq(1, nrow(data_cv), l = k + 1))
                folds[1] <- 1
                folds[length(folds)] <- nrow(data_cv)
                if (!is.null(seed)) {
                    set.seed(seed)
                }
                folds <- round(folds + runif(1, 1, nrow(data_cv)))
                for (i in 1:(length(folds) - 1)) {
                    data_cv[, y_string] <- y0
                    idx2 <- idx[folds[i]:(folds[i] + diff(folds)[i]) - 1]
                    if(i > 1) idx2 <- idx2[-1]
                    data_cv[idx2, y_string] <- NA
                    call_cv <- paste0(call_tails[1], "data = data_cv", call_tails[2])
                    res <- eval(parse(text = call_cv))
                    y_pred <- res$summary.fitted.values$mean[idx2]
                    rmse <- c(rmse, sqrt(mean((y_pred - y[idx2])^2, na.rm = TRUE)))
                }
            }
            rmse_tmp <- mean(rmse)
        }
        
        if (!is.null(test_prop)) {
            for (i0 in 1:rep) {
                data_cv[, y_string] <- y0
                if (!is.null(seed)) {
                    set.seed(seed)
                }
                idx <- runif(round(nrow(data_cv) * test_prop), 1, nrow(data_cv))
                data_cv[idx, y_string] <- NA
                call_cv <- paste0(call_tails[1], "data = data_cv", call_tails[2])
                res <- eval(parse(text = call_cv))
                y_pred <- res$summary.fitted.values$mean[idx]
                rmse <- c(rmse, sqrt(mean((y_pred - y[idx])^2, na.rm = TRUE)))
            }
            rmse_tmp <- mean(rmse)
        }
        
        if (!is.null(test_idx)) {
            data_cv[, y_string] <- y0
            data_cv[test_idx, y_string] <- NA
            call_cv <- paste0(call_tails[1], "data = data_cv", call_tails[2])
            res <- eval(parse(text = call_cv))
            y_pred <- res$summary.fitted.values$mean[test_idx]
            rmse <- c(rmse, sqrt(mean((y_pred - y[test_idx])^2, na.rm = TRUE)))
            rmse_tmp <- mean(rmse)
        }
        mods_rmse <- c(mods_rmse, rmse_tmp)
    }
    if (length(mods) > 1) {
        names(mods_rmse) <- nms
        mods_rmse <- sort(mods_rmse, decreasing = decreasing)
    }
    round(mods_rmse, rnd)
}