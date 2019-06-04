\name{PseudoR2}
\alias{PseudoR2}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pseudo R2 Statistics
  %%  ~~function to do ... ~~
}
\description{The goodness of fit of the logistic regression model can be expressed by some variants of pseudo R squared statistics, most of which being based on the deviance of the model.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PseudoR2(x, which = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the \code{glm}, \code{polr} or \code{multinom} model object to be evaluated.
    %%     ~~Describe \code{x} here~~
  }
  \item{which}{character, one out of \code{"McFadden"},\code{"AldrichNelson"}, \code{"McFaddenAdj"}, \code{"Nagelkerke"},
                                        \code{"CoxSnell"}, \code{"Effron"}, \code{"McKelveyZavoina"},  \code{"Tjur"}, \code{"all"}. Partial matching is supported.}
}
\details{Cox and Snell's \eqn{R^2} is based on the log likelihood for the model compared to the log likelihood for a baseline model. However, with categorical outcomes, it has a theoretical maximum value of less than 1, even for a "perfect" model.

  Nagelkerke's \eqn{R^2} is an adjusted version of the Cox and Snell's \eqn{R^2} that adjusts the scale of the statistic to cover the full range from 0 to 1.

  McFadden's \eqn{R^2} is another version, based on the log-likelihood kernels for the intercept-only model and the full estimated model.
  %%  ~~ If necessary, more details than the description above ~~
}
\value{the value of the specific statistic. \code{AIC}, \code{LogLik}, \code{LogLikNull} and \code{G2} will only be reported with option \code{"all"}.
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...

  \item{AldrichNelson}{AldrichNelson pseudo-\eqn{R^2}}
  \item{McFadden}{McFadden pseudo-\eqn{R^2}}
  \item{McFaddenAdj}{McFadden adjusted pseudo-\eqn{R^2}}
  \item{CoxSnell}{Cox and Snell pseudo-\eqn{R^2} (also known as ML pseudo-\eqn{R^2})}
  \item{Nagelkerke}{Nagelkerke pseudo\eqn{R^2} (also known as CraggUhler \eqn{R^2})}
  \item{McKelveyZavoina}{McKelvey and Zavoina pseudo-\eqn{R^2}}
  \item{Effron}{Effron pseudo-\eqn{R^2}}
  \item{Tjur}{Tjur's pseudo-\eqn{R^2}}
  \item{AIC}{Akaike's information criterion}
  \item{LogLik}{log-Likelihood for the fitted model (by maximum likelihood)}
  \item{LogLikNull}{log-Likelihood for the null model. The null model will include the offset, and an intercept if there is one in the model.}
  \item{G2}{differenz of the null deviance - model deviance}
  }
    \references{

    Aldrich, J. H. and Nelson, F. D. (1984): Linear Probability, Logit, and probit Models, \emph{Sage
University Press}, Beverly Hills.

    Cox D R & Snell E J (1989) \emph{The Analysis of Binary Data} 2nd ed. London: Chapman and Hall.

    Efron, B. (1978). Regression and ANOVA with zero-one data: Measures of residual variation. \emph{Journal of the American Statistical Association, 73}(361), 113--121.

    Hosmer, D. W., & Lemeshow, S. (2000). \emph{Applied logistic regression} (2nd ed.). Hoboke, NJ: Wiley.

    McFadden D (1979). Quantitative methods for analysing travel behavior of individuals: Some recent developments. In D. A. Hensher & P. R. Stopher (Eds.), \emph{Behavioural travel modelling} (pp. 279-318). London: Croom Helm.

    McKelvey, R. D., & Zavoina, W. (1975). A statistical model for the analysis of ordinal level dependent variables. \emph{The Journal of Mathematical Sociology, 4}(1), 103--120

    Nagelkerke, N. J. D. (1991). A note on a general definition of the coefficient of determination. \emph{Biometrika, 78}(3), 691--692.

Tjur, T. (2009) Coefficients of determination in logistic regression models -
              a new proposal: The coefficient of discrimination. \emph{The American
              Statistician},
              63(4): 366-372
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{logLik}}, \code{\link{AIC}}, \code{\link{BIC}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
r.glm <- glm(Survived ~ ., data=Untable(Titanic), family=binomial)
PseudoR2(r.glm)

PseudoR2(r.glm, c("McFadden", "Nagel"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{model}



