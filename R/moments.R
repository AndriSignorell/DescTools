#' Mean and Variance of the Binomial Distribution
#'
#' Formula:
#'   \eqn{\mu = n \cdot p}
#'   \eqn{\mathrm{Var}(X) = n \cdot p \cdot (1 - p)}
#'
#' @param size Number of trials
#' @param prob Probability of success
#'
#' @return List with mean and variance
#' @seealso \code{\link[stats]{dbinom}}
#' @references Casella, G. and Berger, R.L. (2002). Statistical Inference. Duxbury.
#' @examples
#' mbinom(size = 10, prob = 0.3)
mbinom <- function(size, prob) {
  mu <- size * prob
  var <- size * prob * (1 - prob)
  list(mean = mu, variance = var)
}

#' Mean and Variance of the Poisson Distribution
#'
#' Formula:
#'   \eqn{\mu = \lambda}
#'   \eqn{\mathrm{Var}(X) = \lambda}
#'
#' @param lambda Rate parameter (mean)
#'
#' @return List with mean and variance
#' @seealso \code{\link[stats]{dpois}}
#' @references Ross, S.M. (2014). Introduction to Probability Models. Academic Press.
#' @examples
#' mpois(lambda = 4)
mpois <- function(lambda) {
  list(mean = lambda, variance = lambda)
}

#' Mean and Variance of the Geometric Distribution
#'
#' Formula:
#'   \eqn{\mu = \frac{1 - p}{p}}
#'   \eqn{\mathrm{Var}(X) = \frac{1 - p}{p^2}}
#'
#' @param prob Probability of success
#'
#' @return List with mean and variance
#' @seealso \code{\link[stats]{dgeom}}
#' @references Grimmett, G. and Stirzaker, D. (2001). Probability and Random Processes. Oxford University Press.
#' @examples
#' mgeom(prob = 0.2)
mgeom <- function(prob) {
  mu <- (1 - prob) / prob
  var <- (1 - prob) / prob^2
  list(mean = mu, variance = var)
}

#' Mean and Variance of the Negative Binomial Distribution
#'
#' Formula:
#'   \eqn{\mu = r \cdot \frac{1 - p}{p}}
#'   \eqn{\mathrm{Var}(X) = r \cdot \frac{1 - p}{p^2}}
#'
#' @param size Number of successes
#' @param prob Probability of success
#'
#' @return List with mean and variance
#' @seealso \code{\link[stats]{dnbinom}}
#' @references Casella, G. and Berger, R.L. (2002). Statistical Inference. Duxbury.
#' @examples
#' mnbinom(size = 5, prob = 0.4)
mnbinom <- function(size, prob) {
  mu <- size * (1 - prob) / prob
  var <- size * (1 - prob) / prob^2
  list(mean = mu, variance = var)
}

#' Mean and Variance of the Hypergeometric Distribution
#'
#' Formula:
#'   \eqn{\mu = k \cdot \frac{m}{m + n}}
#'   \eqn{\mathrm{Var}(X) = k \cdot \frac{m}{m + n} \cdot \frac{n}{m + n} \cdot \frac{m + n - k}{m + n - 1}}
#'
#' @param m Number of successes in the population
#' @param n Number of failures in the population
#' @param k Sample size
#'
#' @return List with mean and variance
#' @seealso \code{\link[stats]{dhyper}}
#' @references Rice, J.A. (2006). Mathematical Statistics and Data Analysis. Cengage Learning.
#' @examples
#' mhyper(m = 20, n = 30, k = 10)
mhyper <- function(m, n, k) {
  N <- m + n
  mu <- k * m / N
  var <- k * m / N * n / N * (N - k) / (N - 1)
  list(mean = mu, variance = var)
}

#' Mean and Variance of the Normal Distribution
#'
#' Formula:
#'   \eqn{\mu = \mu}
#'   \eqn{\mathrm{Var}(X) = \sigma^2}
#'
#' @param mean Mean of the distribution
#' @param sd Standard deviation
#'
#' @return List with mean and variance
#' @seealso \code{\link[stats]{dnorm}}
#' @references Wackerly, D., Mendenhall, W., and Scheaffer, R. (2008). Mathematical Statistics with Applications. Cengage.
#' @examples
#' mnorm(mean = 0, sd = 1)
mnorm <- function(mean, sd) {
  list(mean = mean, variance = sd^2)
}

#' Mean and Variance of the Exponential Distribution
#'
#' The exponential distribution with rate \eqn{\lambda} has mean and variance
#'   \eqn{\mu = \frac{1}{\lambda}}
#'   \eqn{\mathrm{Var}(X) = \frac{1}{\lambda^2}}
#'
#' @param rate Rate parameter (1 / mean)
#'
#' @return List with mean and variance
#' @seealso \code{\link[stats]{dexp}}
#' @references Hogg, R.V., McKean, J., and Craig, A.T. (2018). Introduction to Mathematical Statistics. Pearson.
#' @examples
#' mexp(rate = 0.5)
mexp <- function(rate) {
  mu <- 1 / rate
  var <- 1 / rate^2
  list(mean = mu, variance = var)
}

#' Mean and Variance of the Gamma Distribution
#'
#' Formula:
#'   \eqn{\mu = \frac{\mathrm{shape}}{\mathrm{rate}}}
#'   \eqn{\mathrm{Var}(X) = \frac{\mathrm{shape}}{\mathrm{rate}^2}}
#'
#' @param shape Shape parameter
#' @param rate Rate parameter
#'
#' @return List with mean and variance
#' @seealso \code{\link[stats]{dgamma}}
#' @references Evans, M., Hastings, N., and Peacock, B. (2000). Statistical Distributions. Wiley.
#' @examples
#' mgamma(shape = 2, rate = 0.5)
mgamma <- function(shape, rate) {
  mu <- shape / rate
  var <- shape / rate^2
  list(mean = mu, variance = var)
}

#' Mean and Variance of the Log-Normal Distribution
#'
#' Formula:
#'   \eqn{\mu = \exp(\mu_{log} + 0.5 \cdot \sigma_{log}^2)}
#'   \eqn{\mathrm{Var}(X) = (\exp(\sigma_{log}^2) - 1) \cdot \exp(2\mu_{log} + \sigma_{log}^2)}
#'
#' @param meanlog Mean on the log scale
#' @param sdlog Standard deviation on the log scale
#'
#' @return List with mean and variance
#' @seealso \code{\link[stats]{dlnorm}}
#' @references Aitchison, J. and Brown, J.A.C. (1957). The Lognormal Distribution. Cambridge University Press.
#' @examples
#' mlnorm(meanlog = 0, sdlog = 1)
mlnorm <- function(meanlog, sdlog) {
  mu <- exp(meanlog + 0.5 * sdlog^2)
  var <- (exp(sdlog^2) - 1) * exp(2 * meanlog + sdlog^2)
  list(mean = mu, variance = var)
}

#' Mean and Variance of the Beta Distribution
#'
#' Formula:
#'   \eqn{\mu = \frac{\alpha}{\alpha + \beta}}
#'   \eqn{\mathrm{Var}(X) = \frac{\alpha \cdot \beta}{(\alpha + \beta)^2 (\alpha + \beta + 1)}}
#'
#' @param shape1 Alpha parameter
#' @param shape2 Beta parameter
#'
#' @return List with mean and variance
#' @seealso \code{\link[stats]{dbeta}}
#' @references Johnson, N.L., Kotz, S., and Balakrishnan, N. (1995). Continuous Univariate Distributions, Vol. 2. Wiley.
#' @examples
#' mbeta(shape1 = 2, shape2 = 3)
mbeta <- function(shape1, shape2) {
  mu <- shape1 / (shape1 + shape2)
  var <- (shape1 * shape2) / ((shape1 + shape2)^2 * (shape1 + shape2 + 1))
  list(mean = mu, variance = var)
}

#' Mean and Variance of the Chi-Squared Distribution
#'
#' Formula:
#'   \eqn{\mu = df}
#'   \eqn{\mathrm{Var}(X) = 2 \cdot df}
#'
#' @param df Degrees of freedom
#'
#' @return List with mean and variance
#' @seealso \code{\link[stats]{dchisq}}
#' @references Rice, J.A. (2006). Mathematical Statistics and Data Analysis. Cengage Learning.
#' @examples
#' mchisq(df = 4)
mchisq <- function(df) {
  list(mean = df, variance = 2 * df)
}


#' Mean and Variance of the t-Distribution
#'
#' The \eqn{t} distribution with \code{df} \eqn{= \nu}{= n} degrees of 
#' freedom has mean and variance of:
#'   \eqn{\mu = 0 \quad (\nu > 1)}
#'   \eqn{\mathrm{Var}(X) = \frac{\nu}{\nu - 2} \quad (\nu > 2)}
#'
#' @param df degrees of freedom (\eqn{> 0}, maybe non-integer). \code{df = Inf} is allowed.
#'
#' @return List with mean and variance
#' @seealso \code{\link[stats]{dt}}
#' @references Casella, G. and Berger, R.L. (2002). Statistical Inference. Duxbury.
#' @examples
#' mt(df = 5)

mt <- function(df) {
  mu <- if (df > 1) 0 else NA
  var <- if (df > 2) df / (df - 2) else NA
  list(mean = mu, variance = var)
}

#' Mean and Variance of the F Distribution
#'
#' The F distribution with \code{df1 =} \eqn{n_1}{n1} and \code{df2 =} 
#' \eqn{n_2}{n2} degrees of freedom has mean and variance:
#'   \eqn{\mu = \frac{n_2}{n_2 - 2} \quad (n_2 > 2)}
#'   \eqn{\mathrm{Var}(X) = \frac{2 n_2^2 (n_1 + n_2 - 2)}{n_1 (n_2 - 2)^2 (n_2 - 4)} \quad (n_2 > 4)}
#'
#' @param df1 Numerator degrees of freedom
#' @param df2 Denominator degrees of freedom
#'
#' @return List with mean and variance
#' @seealso \code{\link[stats]{df}}
#' @references Hogg, R.V., McKean, J., and Craig, A.T. (2018). Introduction to Mathematical Statistics. Pearson.
#' @examples
#' mf(df1 = 5, df2 = 10)
#' 
mf <- function(df1, df2) {
  mu <- if (df2 > 2) df2 / (df2 - 2) else NA
  var <- if (df2 > 4) {
    2 * df2^2 * (df1 + df2 - 2) / (df1 * (df2 - 2)^2 * (df2 - 4))
  } else {
    NA
  }
  list(mean = mu, variance = var)
}
