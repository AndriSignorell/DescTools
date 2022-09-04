\name{StdCoef}
\alias{StdCoef}
\alias{PartialSD}

\encoding{utf-8}
\newcommand{\myequation}{\ifelse{latex}{\eqn{#1}{#3}}{\ifelse{html}{\enc{\eqn{#2}}{#3}}{#3}}}

\title{Standardized Model Coefficients}
\description{
Standardize model coefficients by Standard Deviation or Partial Standard Deviation.
}

\usage{

StdCoef(x, partial.sd = FALSE, ...)

PartialSD(x)

}
\arguments{
  \item{x}{a fitted model object. }
  \item{partial.sd}{logical, if set to \code{TRUE}, model coefficients are
	multiplied by partial \acronym{SD}, otherwise they are multiplied by the
	ratio of the standard deviations of the independent variable and dependent
	variable. }
 \item{\dots}{additional arguments passed to \code{coefTable}, e.g.
    \code{dispersion}. }
}

\details{
The standardized coefficients are meant to allow for a comparison of the importance of explanatory
variables that have different variances. Each of them shows the effect on the response of increasing
its predictor X(j) by one standard deviation, as a multiple of the response's standard deviation.
This is often a more meaningful comparison of the relevance of the input variables.

Note, however, that increasing one X(j) without also changing others may not be possible in a
given application, and therefore, interpretation of coefficients can always be tricky. Furthermore,
for binary input variables, increasing the variable by one standard deviation is impossible, since an
increase can only occur from 0 to 1, and therefore, the standardized coeffient is somewhat counterintuitive in this case.

	Standardizing model coefficients has the same effect as centring and
	scaling the input variables.

	\dQuote{Classical} standardized coefficients
	are calculated as
    \myequation{\beta^{*}_i = \beta_i\frac{s_{X_{i}}}{s_{y}}
        }{\betaᵢ* = \betaᵢ (sₓᵢ / Sᵧ)
        }{Bi* = Bi * (SXi / Sy)}
    , where
	\myequation{\beta}{\beta}{B} is the unstandardized coefficient,
    \myequation{s_{X_{i}}}{sₓᵢ}{SXi}
    is the
	standard deviation of associated depenent variable
    \myequation{X_i}{Xᵢ}{Xi}  and
	\myequation{s_{y}}{Sᵧ}{Sy}
    is \acronym{SD} of the response variable.


	If the variables are intercorrelated, the standard deviation of
    \myequation{X_i}{Xᵢ}{Xi}
	used in computing the standardized coefficients
    \myequation{\beta_i^{*}}{\betaᵢ*}{Bi*} should be
	replaced by a partial standard deviation of
    \myequation{X_i}{Xᵢ}{Xi} which is adjusted for
	the multiple correlation of
    \myequation{X_i}{Xᵢ}{Xi}  with the other \eqn{X} variables
	included in the regression equation. The partial standard deviation is
	calculated as
    \myequation{s_{X_{i}}^{*}=s_{X_{i}} {VIF}(X_i)^{-0.5} (\frac{n-1}{n-p} )^{0.5}
        }{s*ₓᵢ = sₓᵢ √(VIFₓᵢ⁻¹) √((n-1)/(n-p))
        }{sXi* = SXi * sqrt(1/VIF(Xi)) * sqrt((n-1)/(n-p))},
    where VIF is the variance inflation factor,
	n is the number of observations and p number of predictors in
	the model. Coefficient is then transformed as
    \myequation{\beta^{*}_i = \beta_i s_{X_{i}}^{*}
        }{\betaᵢ* = \betaᵢ s*ₓᵢ
        }{Bi* = Bi * SXi*}.

}

\value{
   A matrix with at least two columns for standardized coefficient estimate and
   its standard error. Optionally, third column holds degrees of freedom
   associated with the coefficients.
}


\author{Kamil Barto\enc{ń}{n}
}

\references{

Cade, B.S. (2015) Model averaging and muddled multimodel inferences.
    \emph{Ecology} 96, 2370-2382.

Afifi A., May S., Clark V.A. (2011) \emph{Practical Multivariate Analysis},
Fifth Edition. CRC Press.

Bring, J. (1994). How to standardize regression coefficients. \emph{The American
Statistician} 48, 209-213.
}

\seealso{
% \code{PartialSD} can be used with \code{\link{stdize}}.

\code{\link{coef}} % or \code{\link{coeffs}} and \code{\link{coefTable}} for
% unstandardized coefficients.
}
\examples{

# Fit model to original data:
fm  <- lm(Fertility ~ Agriculture + Examination + Education + Catholic,
          data = swiss)

# Partial SD for the default formula:
psd <- PartialSD(lm(data = swiss))[-1] # remove first element for intercept

# Standardize data:
zswiss <- scale(swiss, scale = c(NA, psd), center = TRUE)
# Note: first element of 'scale' is set to NA to ignore the first column 'y'

# Coefficients of a model fitted to standardized data:
# zapsmall(coefTable(stdizeFit(fm, data = zGPA)))
# Standardized coefficients of a model fitted to original data:
# zapsmall(StdCoef(fm, partial = TRUE))


# Standardizing nonlinear models:
fam <- Gamma("inverse")
fmg <- glm(log(Fertility) ~ Agriculture + Examination + Education + Catholic,
           data = swiss, family = fam)

psdg <- PartialSD(fmg)
# zGPA <- stdize(GPA, scale = c(NA, psdg[-1]), center = FALSE)
# fmgz <- glm(log(y) ~ z.x1 + z.x2 + z.x3 + z.x4, zGPA, family = fam)

# Coefficients using standardized data:
# coef(fmgz) # (intercept is unchanged because the variables haven't been
           #  centred)
# Standardized coefficients:
# coef(fmg) * psdg

}


\keyword{models}
