
#' Breusch-Godfrey Test
#'
#' \code{BreuschGodfreyTest} performs the Breusch-Godfrey test for higher-order
#' serial correlation.
#'
#' Under \eqn{H_0} the test statistic is asymptotically Chi-squared with
#' degrees of freedom as given in \code{parameter}.  If \code{type} is set to
#' \code{"F"} the function returns a finite sample version of the test
#' statistic, employing an \eqn{F} distribution with degrees of freedom as
#' given in \code{parameter}.
#'
#' By default, the starting values for the lagged residuals in the auxiliary
#' regression are chosen to be 0 (as in Godfrey 1978) but could also be set to
#' \code{NA} to omit them.
#'
#' \code{BreuschGodfreyTest} also returns the coefficients and estimated
#' covariance matrix from the auxiliary regression that includes the lagged
#' residuals.  Hence, \code{CoefTest} (package: RegClassTools) can be used to
#' inspect the results. (Note, however, that standard theory does not always
#' apply to the standard errors and t-statistics in this regression.)
#'
#' @param formula a symbolic description for the model to be tested (or a
#' fitted \code{"lm"} object).
#' @param order integer. maximal order of serial correlation to be tested.
#' @param order.by Either a vector \code{z} or a formula with a single
#' explanatory variable like \code{~ z}. The observations in the model are
#' ordered by the size of \code{z}. If set to \code{NULL} (the default) the
#' observations are assumed to be ordered (e.g., a time series).
#' @param type the type of test statistic to be returned. Either \code{"Chisq"}
#' for the Chi-squared test statistic or \code{"F"} for the F test statistic.
#' @param data an optional data frame containing the variables in the model. By
#' default the variables are taken from the environment which
#' \code{BreuschGodfreyTest} is called from.
#' @param fill starting values for the lagged residuals in the auxiliary
#' regression. By default \code{0} but can also be set to \code{NA}.

#' @return A list with class \code{"BreuschGodfreyTest"} inheriting from
#' \code{"htest"} containing the following components: \item{statistic}{the
#' value of the test statistic.} \item{p.value}{the p-value of the test.}
#' \item{parameter}{degrees of freedom.} \item{method}{a character string
#' indicating what type of test was performed.} \item{data.name}{a character
#' string giving the name(s) of the data.} \item{coefficients}{coefficient
#' estimates from the auxiliary regression.} \item{vcov}{corresponding
#' covariance matrix estimate.}

#' @note This function was previously published as \code{bgtest} in the
#' \pkg{lmtest} package and has been integrated here without logical changes.

#' @author David Mitchell <david.mitchell@@dotars.gov.au>, Achim Zeileis
#' @seealso \code{\link[DescTools]{DurbinWatsonTest}}
#' @references Johnston, J. (1984): \emph{Econometric Methods}, Third Edition,
#' McGraw Hill Inc.
#'
#' Godfrey, L.G. (1978): `Testing Against General Autoregressive and Moving
#' Average Error Models when the Regressors Include Lagged Dependent
#' Variables', \emph{Econometrica}, 46, 1293-1302.
#'
#' Breusch, T.S. (1979): `Testing for Autocorrelation in Dynamic Linear
#' Models', \emph{Australian Economic Papers}, 17, 334-355.

#' @keywords htest
#' @examples
#'
#' ## Generate a stationary and an AR(1) series
#' x <- rep(c(1, -1), 50)
#'
#' y1 <- 1 + x + rnorm(100)
#'
#' ## Perform Breusch-Godfrey test for first-order serial correlation:
#' BreuschGodfreyTest(y1 ~ x)
#'
#' ## or for fourth-order serial correlation
#' BreuschGodfreyTest(y1 ~ x, order = 4)
#'
#' ## Compare with Durbin-Watson test results:
#' DurbinWatsonTest(y1 ~ x)
#'
#' y2 <- stats::filter(y1, 0.5, method = "recursive")
#' BreuschGodfreyTest(y2 ~ x)



BreuschGodfreyTest <- function(formula, order = 1, order.by = NULL,
                               type = c("Chisq", "F"),
                               data = list(), fill = 0) {

  # from lmtest

  dname <- paste(deparse(substitute(formula)))

  if(!inherits(formula, "formula")) {
    X <- if(is.matrix(formula$x))
      formula$x
    else model.matrix(terms(formula), model.frame(formula))
    y <- if(is.vector(formula$y))
      formula$y
    else model.response(model.frame(formula))
  } else {
    mf <- model.frame(formula, data = data)
    y <- model.response(mf)
    X <- model.matrix(formula, data = data)
  }

  if(!is.null(order.by))
  {
    if(inherits(order.by, "formula")) {
      z <- model.matrix(order.by, data = data)
      z <- as.vector(z[,ncol(z)])
    } else {
      z <- order.by
    }
    X <- as.matrix(X[order(z),])
    y <- y[order(z)]
  }

  n <- nrow(X)
  k <- ncol(X)
  order <- 1:order
  m <- length(order)
  resi <- lm.fit(X,y)$residuals

  Z <- sapply(order, function(x) c(rep(fill, length.out = x), resi[1:(n-x)]))
  if(any(na <- !complete.cases(Z))) {
    X <- X[!na, , drop = FALSE]
    Z <- Z[!na, , drop = FALSE]
    y <- y[!na]
    resi <- resi[!na]
    n <- nrow(X)
  }
  auxfit <- lm.fit(cbind(X,Z), resi)

  cf <- auxfit$coefficients
  vc <- chol2inv(auxfit$qr$qr) * sum(auxfit$residuals^2) / auxfit$df.residual
  names(cf) <- colnames(vc) <- rownames(vc) <- c(colnames(X), paste("lag(resid)", order, sep = "_"))

  switch(match.arg(type),

         "Chisq" = {
           bg <- n * sum(auxfit$fitted^2)/sum(resi^2)
           p.val <- pchisq(bg, m, lower.tail = FALSE)
           df <- m
           names(df) <- "df"
         },

         "F" = {
           uresi <- auxfit$residuals
           bg <- ((sum(resi^2) - sum(uresi^2))/m) / (sum(uresi^2) / (n-k-m))
           df <- c(m, n-k-m)
           names(df) <- c("df1", "df2")
           p.val <- pf(bg, df1 = df[1], df2 = df[2], lower.tail = FALSE)
         })

  names(bg) <- "LM test"
  RVAL <- list(statistic = bg, parameter = df,
               method = paste("Breusch-Godfrey test for serial correlation of order up to", max(order)),
               p.value = p.val,
               data.name = dname,
               coefficients = cf,
               vcov = vc)

  class(RVAL) <- c("BreuschGodfreyTest", "htest")
  return(RVAL)

}


# vcov.BreuschGodfreyTest <- function(object, ...) object$vcov
# df.residual.BreuschGodfreyTest <- function(object, ...) if(length(df <- object$parameter) > 1L) df[2] else NULL



