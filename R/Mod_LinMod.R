



MAE <- function(x, ...) {

  UseMethod("MAE")
}

MAE.lm <- function(x, ...)
  # regr will escalate to lm, so no need for another interface here
  MAE(predict(x, type="response"), model.response(x$model), na.rm=FALSE)

MAE.default <- function (x, ref, na.rm=FALSE, ...) {
  # mean will bark, if there are NAs, so no need to do here anyhing further
  # (the difference will report NAs anyway)
  mean(abs(ref-x), na.rm=na.rm, ...)
}

MSE <- function(x, ...) {


  UseMethod("MSE")
}

MSE.lm <- function(x, ...)
  # regr will escalate to lm, so no need for another interface here
  MSE(predict(x, type="response"), model.response(x$model), na.rm=FALSE)

MSE.default <- function (x, ref, na.rm=FALSE, ...) {
  mean((ref-x)^2, na.rm=na.rm, ...)
}

RMSE <- function(x, ...) {

  UseMethod("RMSE")
}

RMSE.lm <- function(x, ...)
  # regr will escalate to lm, so no need for another interface here
  RMSE(predict(x, type="response"), model.response(x$model), na.rm=FALSE)


RMSE.default <- function (x, ref, na.rm=FALSE, ...) {
  sqrt(MSE(x, ref, na.rm, ...))
}


MAPE <- function(x, ...) {

  UseMethod("MAPE")
}


MAPE.lm <- function(x, ...)
  # regr will escalate to lm, so no need for another interface here
  MAPE(predict(x, type="response"), model.response(x$model), na.rm=FALSE)

MAPE.default <- function (x, ref, na.rm=FALSE, ...) {
  # mean will bark, if there are NAs, so no need to do here anyhing further
  # (the difference will report NAs anyway)
  mean(abs((ref-x)/ref), na.rm=na.rm, ...)
}


SMAPE <- function(x, ...) {


  UseMethod("SMAPE")
}

SMAPE.lm <- function(x, ...)
  # regr will escalate to lm, so no need for another interface here
  SMAPE(predict(x, type="response"), model.response(x$model), na.rm=FALSE)

SMAPE.default <- function (x, ref, na.rm=FALSE, ...) {

  mean( 2 * abs(ref-x) / (abs(x) + abs(ref)), na.rm=na.rm, ...)
}

# Chen and Yang (2004), in an unpublished working paper, defined the sMAPE as
# \[\text{sMAPE} = \text{mean}(2|y_t - \hat{y}_t|/(|y_t| + |\hat{y}_t|)).\]
# They still called it a measure of "percentage error" even though they dropped the multiplier 100.
# At least they got the range correct, stating that this measure has a maximum value of two when
# either y_t or \hat{y}_t is zero, but is undefined when both are zero.
# The range of this version of sMAPE is (0,2). Perhaps this is the definition that Makridakis and
# Armstrong intended all along, although neither has ever managed to include it correctly
# in one of their papers or books.
# source: http://robjhyndman.com/hyndsight/smape/



NMSE <- function(x, ref, train.y){



  sse <- sum((ref-x)^2)
  sse/sum((ref-mean(train.y))^2)
}

NMAE <- function(x, ref, train.y){



  sae <- sum(abs(ref-x))
  sae/sum(abs(ref-mean(train.y)))
}









.partialsd <- function(x, sd, vif, n, p = length(x) - 1) {
  sd * sqrt(1 / vif) * sqrt((n - 1)/(n - p))
}


PartialSD <- function(x) {



  mm <- model.matrix(x)
  .partialsd(coef(x), apply(mm, 2L, sd), VIF(x), nobs(x),
             sum(attr(mm, "assign") != 0))

}

coeffs <-
  function (model) UseMethod("coeffs")

coeffs.multinom <-
  function (model) {
    cf <- coef(model)
    if (!is.vector(cf)) {
      cf <- t(as.matrix(cf))
      cfnames <- expand.grid(dimnames(cf), stringsAsFactors = FALSE)
      cfnames <- sprintf("%s(%s)", cfnames[,2L], cfnames[,1L])
      structure(as.vector(cf), names = cfnames)
    } else cf
  }

coeffs.survreg <-
  function (model) {
    rval <- coef(model)
    if (nrow(vcov(model)) > length(rval)) { # scale was estimated
      lgsc <- log(model$scale)
      names(lgsc) <- if(is.null(names(lgsc)))
        "Log(scale)" else
          paste0("Log(scale):", names(lgsc))
      rval <- c(rval, lgsc)
    }
    rval
  }

coeffs.default <-
  function(model) coef(model)


coefTable <-
  function (model, ...) UseMethod("coefTable")

.makeCoefTable <-
  function(x, se, df = NA_real_, coefNames = names(x)) {
    if(n <- length(x)) {
      xdefined <- !is.na(x)
      ndef <- sum(xdefined)
      if(ndef < n) {
        if(length(se) == ndef) {
          y <- rep(NA_real_, n); y[xdefined] <- se; se <- y
        }
        if(length(df) == ndef) {
          y <- rep(NA_real_, n); y[xdefined] <- df; df <- y
        }
      }
    }
    if(n && n != length(se)) stop("length(x) is not equal to length(se)")
    ret <- matrix(NA_real_, ncol = 3L, nrow = length(x),
                  dimnames = list(coefNames, c("Estimate", "Std. Error", "df")))
    if(n) ret[, ] <- cbind(x, se, rep(if(is.null(df)) NA_real_ else df,
                                      length.out = n), deparse.level = 0L)
    class(ret) <- c("coefTable", "matrix")
    ret
  }

coefTable.default <-
  function(model, ...) {
    dfs <- tryCatch(df.residual(model), error = function(e) NA_real_)
    cf <- summary(model, ...)$coefficients
    .makeCoefTable(cf[, 1L], cf[, 2L], dfs, coefNames = rownames(cf))
  }

coefTable.lm <-
  function(model, ...)
    .makeCoefTable(coef(model), sqrt(diag(vcov(model, ...))), model$df.residual)


coefTable.survreg <-
  function(model, ...) {
    .makeCoefTable(
      coeffs(model),
      sqrt(diag(vcov(model, ...))),
      NA
    )
  }

coefTable.coxph <-
  function(model, ...) {
    .makeCoefTable(coef(model), if(all(is.na(model$var)))
      rep(NA_real_, length(coef(model))) else sqrt(diag(model$var)),
      model$df.residual)
  }

coefTable.multinom <-
  function (model, ...) {
    .makeCoefTable(coeffs(model), sqrt(diag(vcov(model, ...))))
  }

coefTable.zeroinfl <-
  function(model, ...)
    .makeCoefTable(coef(model), sqrt(diag(vcov(model, ...))))

coefTable.hurdle <-
  function(model, ...) {
    cts <- summary(model)$coefficients
    ct <- do.call("rbind", unname(cts))
    cfnames <- paste0(rep(names(cts), vapply(cts, nrow, 1L)), "_", rownames(ct))
    .makeCoefTable(ct[, 1L], ct[, 2L], coefNames = cfnames)
    #.makeCoefTable(coef(model), sqrt(diag(vcov(model, ...))))
  }




StdCoef <- function(x, partial.sd = FALSE, ...) {



  coefmat <- coefTable(x, ...)

  mm <- model.matrix(x)

  if(partial.sd) {
    bx <- .partialsd(coefmat[, 1L], apply(mm, 2L, sd),
                     VIF(x), nobs(x), sum(attr(mm, "assign") != 0))
  } else {
    response.sd <- sd(model.response(model.frame(x)))
    bx <- apply(mm, 2L, sd) / response.sd
  }
  coefmat[, 1L:2L] <- coefmat[, 1L:2L] * bx
  colnames(coefmat)[1L:2L] <- c("Estimate*", "Std. Error*")
  return (coefmat)

}





