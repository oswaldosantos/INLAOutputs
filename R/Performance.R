#' Performance metrics
#' @description Calculates performance metrics for regression and classification models..
#' @param ... objects of \code{\link{class}} \code{inla}  with computed fitted marginals.
#' @param samples \code{NULL} or \code{\link{list}} with resamples. If \code{NULL}, performance metrics are calculated using the entaire dataset. If \code{list}, each element should be a \code{\link{vector}} with indexes to train the model. Indexes out of the vector (out-of-bag samples) are used to calculate performance metrics.
#' @param regression logical. If \code{TRUE} (default), the RMSE and R2 are calculated. If \code{FALSE}, the following mwtrics are calculated: accuracy, sensitivity, specificity, Positive Predictive Value (PPV) and Negative Predictive Value (NPV).
#' @param threshold When \code{regression = FALSE}, the threshold to assign class probabilities to classes is the value that maximizes \code{max(sensitivities + specificities)} (see details). Class probabilities below the threshold are assigned to \code{min(y)}, where \code{y} is the predicted variable (class probabilities above the threshold are assigned to the other class). If \code{threshold} is provided, it overrides the calculated threshold; it should be a number x such that 0 < x < 1.
#' @param rnd integer indicating the number of decimal places to be used.
#' @return \code{vector} or \code{\link{matrix}} with performance metrics.
#' @details When \code{regression = FALSE}, the threshold is calculated with the Youoden's algorithm, using the \code{\link{pROC::coords}} function.
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

Performance <- function (..., samples = NULL, regression = TRUE, threshold = NULL, rnd = 3) {
    mods <- list(...)
    nms <- deparse(substitute(list(...)))
    if (any(grepl("list\\(", nms))) {
        nms <- unlist(strsplit(unlist(substr(nms, 6, nchar(nms) - 1)), ", "))
    }
    mean_rmse <- mean_r2 <- c()
    mean_acc <- mean_se <- mean_sp <- mean_ppv <- mean_npv <- c()
    for (m in 1:length(mods)) {
        call <- gsub("\\s+", " ", paste(mods[[m]]$call, collapse = ""))
        if (grepl("offset\\(", call)) {
            stop("Use E instead of offset")
        }
        data_rex <- regexpr("data = [[:alnum:]|[:punct:]]+", call)
        data_call <- substring(call, data_rex, data_rex +
                                   attributes(data_rex)[[1]] - 2)
        data_string <- substring(data_call, 8)
        y_rex <- regexpr("formula = [[:alnum:]|[:punct:]]+ ", 
                         call)
        y_string <- substring(call, y_rex + 10, y_rex +
                                  attributes(y_rex)[[1]] - 2)
        e_rex <- regexpr("E = [[:alnum:]|[:punct:]]+", call)
        e_string <- substring(call, e_rex + 4, e_rex +
                                  attributes(e_rex)[[1]] - 2)
        call_tails <- unlist(strsplit(call, data_call))
        data_cv <- eval(parse(text = data_string))
        y0 <- y <- unname(unlist(data_cv[, y_string]))
        if (e_string != "") {
            y <- y/data_cv[, e_string]
        }
        rmse <- r2 <- acc <- se <- sp <- ppv <- npv <- c()
        if (is.null(samples)) {
            samples <- 1
        }
        for (i in 1:length(samples)) {
            data_cv[, y_string] <- y0
            if (is.list(samples)) {
                data_cv[-samples[[i]], y_string] <- NA
            }
            call_cv <- paste0(call_tails[1], "data = data_cv", call_tails[2])
            res <- eval(parse(text = call_cv))
            if (is.list(samples)) {
                y_pred <- res$summary.fitted.values$mean[-samples[[i]]]
                y_test <- y[-samples[[i]]]
            } else {
                y_pred <- res$summary.fitted.values$mean
                y_test <- y
            }
            if (regression) {
                rmse <- c(rmse, sqrt(mean((y_pred - y_test)^2, na.rm = TRUE)))
                r2 <- c(r2, cor(y_pred, y_test)^2)   
            } else {
                rocs <- roc(y_test, y_pred)
                crds <- coords(rocs, x = "best")
                threshold <- ifelse(is.null(threshold), crds[1], threshold)
                y_pred <- ifelse(y_pred > threshold,
                                 max(as.numeric(y)),
                                 min(as.numeric(y)))
                acc <- c(acc, sum(y_test == y_pred) / length(y_test))
                se <- c(se, sum(y_test == 1 & y_pred == 1) / sum(y_test == 1))
                sp <- c(sp, sum(y_test == 0 & y_pred == 0) / sum(y_test == 0))
                ppv <- c(ppv, sum(y_test == 1 & y_pred == 1) / sum(y_pred == 1))
                npv <- c(npv, sum(y_test == 0 & y_pred == 0) / sum(y_pred == 0))
            }
        }
        if (regression) {
            mean_rmse <- c(mean_rmse, mean(rmse, na.rm = TRUE))
            mean_r2 <- c(mean_r2, mean(r2, na.rm = TRUE))
        } else {
            mean_acc <- c(mean_acc, mean(acc, na.rm = TRUE))
            mean_se <- c(mean_se, mean(se, na.rm = TRUE))
            mean_sp <- c(mean_sp, mean(sp, na.rm = TRUE))
            mean_ppv <- c(mean_ppv, mean(ppv, na.rm = TRUE))
            mean_npv <- c(mean_npv, mean(npv, na.rm = TRUE))
        }
    }
    if (length(mods) > 1) {
        if (regression) {
            performance <- round(cbind(RMSE = mean_rmse, R2 = mean_r2), rnd)
        } else {
            performance <- round(cbind(Accuracy = mean_acc,
                                       Sensitivity = mean_se,
                                       Specificity = mean_sp,
                                       PPV = mean_ppv,
                                       NPV = mean_npv),
                                 rnd)
        }
        rownames(performance) <- nms
        return(performance)
    }
    if (regression) {
        return(round(c(RMSE = mean_rmse, R2 = mean_r2), rnd))
    } else {
        round(c(Accuracy = mean_acc,
                Sensitivity = mean_se,
                Specificity = mean_sp,
                PPV = mean_ppv,
                NPV = mean_npv),
              rnd)
    }
}
