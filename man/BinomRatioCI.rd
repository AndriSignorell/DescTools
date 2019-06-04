\name{BinomRatioCI}
\alias{BinomRatioCI}

\title{
Confidence Intervals for the Ratio of Binomial and Multinomial Proportions
}

\description{
A number of methods have been develeloped for obtaining confidence intervals for the ratio of two binomial proportions.  These include the Wald/Katz-log method (Katz et al. 1978), 
adjusted-log (Walter 1975, Pettigrew et al. 1986), Koopman asymptotic score (Koopman 1984), Inverse hyperbolic sine transformation (Newman 2001), the Bailey method (Bailey (1987), 
and the Noether (1957) procedure. Koopman results are found iteratively for most intervals using root finding.   
}

\usage{
BinomRatioCI(x1, n1, x2, n2, conf.level = 0.95, method = "katz.log", 
             bonf = FALSE, tol = .Machine$double.eps^0.25, R = 1000, r = length(x1))
}

\arguments{

  \item{x1}{
The ratio numerator number of successes.  A scalar or vector.
}
  \item{n1}{
The ratio numerator number of trials.  A scalar or vector of \code{length(y1)}
}
  \item{x2}{
The ratio denominator number of successes.  A scalar or vector of \code{length(y1)}
}
  \item{n2}{
The ratio denominator number of trials. A scalar or vector of \code{length(y1)}
}
  \item{conf.level}{
The level of confidence, i.e. 1 - \emph{P}(type I error). 
}
  \item{method}{
Confidence interval method.  One of \code{"adj.log"}, \code{"bailey"},  
\code{"boot"}, \code{"katz.log"}, \code{"koopman"}, \code{"sinh-1"} or 
\code{"noether"}.  Partial distinct names can be used.  
}
  \item{bonf}{
Logical, indicating whether or not Bonferroni corrections should be applied for simultaneous inference if \code{y1, y2, n1} and \code{n2} are vectors.
}  
  \item{tol}{The desired accuracy (convergence tolerance) for the iterative root finding procedure when finding Koopman intevals. The default is taken to be the smallest positive floating-point number of the workstation implementing the function, raised to the 0.25 power, and will normally be approximately 0.0001.
}
\item{R}{If method \code{"boot"} is chosen, the number of bootstrap iterations.
}
\item{r}{The number of ratios to which family-wise inferences are being made.  Assumed to be \code{length(y1)}. 
}
}
 
      
\details{
Let \eqn{Y_1} and \eqn{Y_2} be multinomial random variables with parameters \eqn{n_1, \pi_{1i}},  and  \eqn{n_2, \pi_{2i}}, respectively; where \eqn{i = \{1, 2, 3, \dots, r\}}.  This encompasses the binomial case in which \eqn{r = 1}. We define the true selection ratio for the \emph{i}th resource of \emph{r} total resources to be:
 \deqn{\theta_{i}=\frac{\pi _{1i}}{\pi _{2i}}}

where \eqn{\pi_{1i}} and \eqn{\pi_{2i}} represent the proportional use and availability of the \emph{i}th resource, respectively. Note that if \eqn{r = 1} the selection ratio becomes relative risk.  The maximum likelihood estimators for \eqn{\pi_{1i}} and \eqn{\pi_{2i}} are the sample proportions: 

\deqn{{{\hat{\pi }}_{1i}}=\frac{{{y}_{1i}}}{{{n}_{1}}},} and
\deqn{{{\hat{\pi }}_{2i}}=\frac{{{y}_{2i}}}{{{n}_{2}}}}

where \eqn{y_{1i}} and \eqn{y_{2i}} are the observed counts for use and availability for the \emph{i}th resource.  The estimator for \eqn{\theta_i} is:

\deqn{\hat{\theta}_{i}=\frac{\hat{\pi}_{1i}}{\hat{\pi }_{2i}}.}

\tabular{ll}{
Method \tab Algorithm \cr
\tab \cr 

% Katz-log	
Katz-log \tab \eqn{\hat\theta_i\times} exp\eqn{(\pm z_1-\alpha/2\hat{\sigma}_W)}, \cr
\tab where \eqn{\hat\sigma_W^2=\frac{(1-\hat{\pi} _{1i})}{\hat{\pi}_{1i}n_1}+\frac{(1-\hat{\pi}_{2i})}{\hat{\pi}_{2i}n_2}}.  \cr 
\tab \cr

% Adjusted log
Adjusted-log \tab \eqn{\hat{\theta}_{Ai}\times} exp\eqn{(\pm z_1-\alpha /2\hat{\sigma}_A)}, \cr 
\tab where \eqn{\hat{\theta}_{Ai}=\frac{y_{1i}+0.5/n_1+0.5}{y_{2i}+0.5/n_2+0.5}}, \cr 
\tab \eqn{\hat{\sigma}_A^2=\frac{1}{y_1+0.5}-\frac{1}{n_1+0.5}+\frac{1}{y_2+0.5}-\frac{1}{n_2+0.5}}. \cr
\tab \cr

% Bailey
Bailey \tab \eqn{\hat{\theta} _i\left[\frac{1\pm z_1-\left( \alpha /2 \right)\left( \hat{\pi}_{1i}'/y_{1i}+\hat{\pi}_{2i}'/y_{2i}-z_1-\left(\alpha/2 \right)^2\hat{\pi} _{1i}'\hat{\pi}_{2i}'/9y_{1i}y_{2i} \right)^{1/2}/3}{1-z_{1-\left(\alpha/2 \right)^2}\hat{\pi} _{2i}'/9y_{2i}} \right]^3},\cr 
\tab where \eqn{\hat{\pi_{1i}}'} = 1 - \eqn{\hat{\pi}_{1i}}, and \eqn{\hat{\pi}_{2i}'} = 1 - \eqn{\hat{\pi}_{2i}}.\cr
\tab \cr

% Inv sin
Inv. hyperbolic sine \tab \eqn{\ln({{\hat{\theta }}_{i}})\pm \left[ 2sin{{h}^{-1}}\left( \frac{{{z}_{(1-\alpha /2)}}}{2}\sqrt{\frac{1}{{{y}_{1i}}}-\frac{1}{{{n}_{1}}}+\frac{1}{{{y}_{2i}}}-\frac{1}{{{n}_{2}}}} \right) \right]}, \cr
\tab\cr 

% Koopman
Koopman \tab Find \eqn{X^2(\theta_0)} = \eqn{\chi _1^2(1 - \alpha)}, where \cr
\tab  \eqn{{{\tilde{\pi }}_{1i}}=\frac{{{\theta }_{0}}({{n}_{1}}+{{y}_{2i}})+{{y}_{1i}}+{{n}_{2}}-{{[{{\{{{\theta }_{0}}({{n}_{1}}+{{y}_{2i}})+{{y}_{1i}}+
{{n}_{2}}\}}^{2}}-4{{\theta }_{0}}({{n}_{1}}+{{n}_{2}})({{y}_{1i}}+{{y}_{2i}})]}^{0.5}}}{2({{n}_{1}}+{{n}_{2}})}}, \cr
\tab \eqn{{{\tilde{\pi }}_{2i}}=\frac{{{{\tilde{\pi }}}_{1i}}}{{{\theta }_{0}}}$, and ${{X}^{2}}({{\theta}_{0}})=\frac{{{\left( {{y}_{1i}}-{{n}_{1}}{{{\tilde{\pi }}}_{1i}} \right)}^{2}}}
{{{n}_{1}}{{{\tilde{\pi }}}_{1i}}(1-{{{\tilde{\pi }}}_{1i}})}\left\{ 1+\frac{{{n}_{1}}({{\theta}_{0}}-{{{\tilde{\pi }}}_{1i}})}{{{n}_{2}}(1-{\tilde{\pi}_{1i}})} \right\}}. \cr 
\tab \cr
%% Noether
Noether \tab \eqn{\hat{\theta}_i\pm z_1-\alpha/2\hat{\sigma}_N},   \cr
\tab where \eqn{\hat{\sigma }_{N}^{2}=\hat{\theta }_{i}^{2}\left( \frac{1}{{{y}_{1i}}}-\frac{1}{{{n}_{1}}}+\frac{1}{{{y}_{2i}}}-\frac{1}{{{n}_{2}}} \right)}.  
}

Exception handling strategies are generally necessary in the cases \eqn{y_1} = 0, \eqn{n_1} = \eqn{y_1}, \eqn{y_2} = 0, and \eqn{n_2} = \eqn{y_2} (see Aho and Bowyer, in review).  

The bootstrap method currently employs percentile confidence intervals.

}

\value{Returns a list of \code{class = "ci"}.  Default output is a matrix with the point and interval estimate. 
}
\references{
Agresti, A., Min, Y. (2001) On small-sample confidence intervals for parameters in discrete distributions.  \emph{Biometrics} 57: 963-97.

Aho, K., and Bowyer, T. (In review) Confidence intervals for ratios of multinomial proportions: implications for selection ratios. \emph{Methods in Ecology and Evolution}.

Bailey, B.J.R. (1987) Confidence limits to the risk ratio.  \emph{Biometrics} 43(1): 201-205.

Katz, D., Baptista, J., Azen, S. P., and Pike, M. C. (1978) Obtaining confidence intervals for the risk ratio in cohort studies. \emph{Biometrics} 34: 469-474

Koopman, P. A. R. (1984) Confidence intervals for the ratio of two binomial proportions. \emph{Biometrics} 40:513-517.

Manly, B. F., McDonald, L. L., Thomas, D. L., McDonald, T. L. and Erickson, W.P. (2002)  \emph{Resource Selection by Animals: Statistical Design and Analysis for Field Studies.  2nd  edn.}  Kluwer, New York, NY

Newcombe, R. G. (2001)  Logit confidence intervals and the inverse sinh transformation.  \emph{The American Statistician} 55: 200-202.

Pettigrew H. M., Gart, J. J., Thomas, D. G. (1986)  The bias and higher cumulants of the logarithm of a
binomial variate.  \emph{Biometrika} 73(2): 425-435.

Walter, S. D. (1975) The distribution of Levins measure of attributable risk. \emph{Biometrika} 62(2): 371-374.
}
\author{
Ken Aho <kenaho1@gmail.com>
}
\seealso{
\code{\link{BinomCI}, \link{BinomDiffCI}}
}
\examples{
# From Koopman (1984)
BinomRatioCI(x1 = 36, n1 = 40, x2 = 16, n2 = 80, method = "katz")
BinomRatioCI(x1 = 36, n1 = 40, x2 = 16, n2 = 80, method = "koop")
}
