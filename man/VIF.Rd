\name{VIF}
\alias{VIF}


\title{Variance Inflation Factors}
\description{
  Calculates variance-inflation and generalized variance-inflation factors
  for linear and generalized linear models. It's a measure describing how much the variance of an estimated coefficient is increased because of collinearity.
}
\usage{
VIF(mod)

}

\arguments{
  \item{mod}{an object that responds to \code{\link{coef}}, \code{\link{vcov}}, and
  \code{\link{model.matrix}}, such as an \code{lm} or \code{glm} object.}
}
\details{
  If all terms in an unweighted linear model have 1 df, then the usual variance-inflation
  factors are calculated.

  The vif are defined as
  \deqn{vif_j=\frac{1}{1-R_j^2}}{vif_j = 1/(1-R_j^2)}

where \eqn{R_j^2} equals the coefficient of determination for regressing the explanatory variable \code{j} in question on the other terms in the model. This is one of the well-known collinearity diagnostics.

  If any terms in an unweighted linear model have more than 1 df, then generalized variance-inflation factors
  (Fox and Monette, 1992) are calculated. These are interpretable as the inflation
  in size of the confidence ellipse or ellipsoid for the coefficients of the term in
  comparison with what would be obtained for orthogonal data.

  The generalized vifs
  are invariant with respect to the coding of the terms in the model (as long as
  the subspace of the columns of the model matrix pertaining to each term is
  invariant). To adjust for the dimension of the confidence ellipsoid, the function
  also prints \eqn{GVIF^{1/(2\times df)}}{GVIF^[1/(2*df)]} where \eqn{df} is the degrees of freedom
  associated with the term.

  Through a further generalization, the implementation here is applicable as well to other sorts of models,
  in particular weighted linear models and  generalized linear models.

  Values of vif up to 5 are usually interpreted as uncritical, values above 5 denote a considerable multicollinearity.
}
\value{
  A vector of vifs, or a matrix containing one row for each term in the model, and
  columns for the GVIF, df, and \eqn{GVIF^{1/(2\times df)}}{GVIF^[1/(2*df)]}.
}
\note{This is a verbatim copy from the function \code{car::vif}.}
\references{
  Fox, J. and Monette, G. (1992)
  Generalized collinearity diagnostics.
  \emph{JASA}, \bold{87}, 178--183.

  Fox, J. (2008)
  \emph{Applied Regression Analysis and Generalized Linear Models},
  Second Edition. Sage.

  Fox, J. and Weisberg, S. (2011)
  \emph{An R Companion to Applied Regression}, Second Edition, Sage.
}
\author{Henric Nilsson and John Fox \email{jfox@mcmaster.ca}}

\examples{
VIF(lm(Fertility ~ Agriculture + Education, data=swiss))
VIF(lm(Fertility ~ ., data=swiss))
}

\keyword{regression}
