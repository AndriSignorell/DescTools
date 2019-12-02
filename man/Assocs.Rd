\name{Assocs}
\alias{Assocs}
\alias{print.Assocs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Association Measures
%%  ~~function to do ... ~~
}
\description{Collects a number of association measures for nominal and ordinal data.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Assocs(x, conf.level = 0.95, verbose = NULL)

\method{print}{Assocs}(x, digits = 4, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a 2 dimensional contingency table or a matrix.
%%     ~~Describe \code{x} here~~
}
  \item{conf.level}{confidence level of the interval. If set to \code{NA} no confidence interval will be calculated. Default is 0.95.

%%     ~~Describe \code{conf.level} here~~
}
\item{verbose}{integer out of \code{c(2, 1, 3)} defining the verbosity of the reported results. 2 (default) means medium, 1 less and 3 extensive results. \cr Applies only to tables and is ignored else.
}

  \item{digits}{integer which determines the number of digits used in formatting the measures of association.
%%     ~~Describe \code{digits} here~~
}
  \item{\dots}{further arguments to be passed to or from methods.}

}
\details{This function wraps the association measures phi, contingency coefficient, Cramer's V, Goodman Kruskal's Gamma, Kendall's Tau-b,
Stuart's Tau-c, Somers' Delta, Pearson and Spearman correlation, Guttman's Lambda, Theil's Uncertainty Coefficient and the mutual information.

}
\value{ numeric matrix, dimension [1:17, 1:3]\cr
the first column contains the estimate, the second the lower confidence interval, the third the upper one.
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{Phi}}, \code{\link{ContCoef}}, \code{\link{CramerV}}, \code{\link{GoodmanKruskalGamma}}, \code{\link{KendallTauB}}, \code{\link{StuartTauC}},
\code{\link{SomersDelta}}, \code{\link{SpearmanRho}}, \code{\link{Lambda}}, \code{\link{UncertCoef}}, \code{\link{MutInf}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
options(scipen=8)

# Example taken from: SAS/STAT(R) 9.2 User's Guide, Second Edition, The FREQ Procedure
# http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
# Hair-Eye-Color pp. 1816

tob <- as.table(matrix(c(
  69, 28, 68, 51,  6,
  69, 38, 55, 37,  0,
  90, 47, 94, 94, 16
), nrow=3, byrow=TRUE,
   dimnames=list(eye=c("blue","green","brown"),
                 hair=c("fair","red","medium","dark","black")) ))
Desc(tob)
Assocs(tob)

# Example taken from: http://www.math.wpi.edu/saspdf/stat/chap28.pdf
# pp. 1349

pain <- as.table(matrix(c(
   26,  6,
   26,  7,
   23,  9,
   18, 14,
    9, 23
   ), ncol=2, byrow=TRUE))

Desc(pain)
Assocs(pain)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate}
