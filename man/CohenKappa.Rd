\name{CohenKappa}
\alias{CohenKappa}
\title{Cohen's Kappa and Weighted Kappa }

\description{Computes the agreement rates Cohen's kappa and weighted kappa and 
their confidence intervals.
}
\usage{
CohenKappa(x, y, weights = c("Unweighted", "Equal-Spacing", "Fleiss-Cohen"),
           conf.level = NA, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{can either be a numeric vector or a confusion matrix. In the latter case x must be a square matrix.
%%     ~~Describe \code{x} here~~
}
 \item{y}{NULL (default) or a vector with compatible dimensions to \code{x}. If \code{y} is provided, \code{table(x, y, \dots)} is calculated. In order to get a square matrix, \code{x} and \code{y} are
 coerced to factors with synchronized levels. (Note, that the vector interface can not be used together with weights.)
%%     ~~Describe \code{y} here~~
}
  \item{weights}{either one out of \code{"Unweighted"} (default), \code{"Equal-Spacing"}, \code{"Fleiss-Cohen"}, which will calculate the weights accordingly, or a user-specified matrix having the same dimensions as x containing the weights for each cell.

%%     ~~Describe \code{weights} here~~
}
\item{conf.level}{confidence level of the interval. If set to \code{NA} (which is the default) no confidence intervals will be calculated.
%%     ~~Describe \code{conf.level} here~~
}
  \item{\dots}{further arguments are passed to the function \code{\link{table}}, allowing i.e. to set \code{useNA}. This refers only to the vector interface.
%%     ~~Describe \code{\dots} here~~
}

}
\details{Cohen's kappa is the diagonal sum of the (possibly weighted) relative frequencies, corrected for expected values and standardized by its maximum value. \cr
The equal-spacing weights (see Cicchetti and Allison 1971) are defined by \deqn{1 - \frac{|i - j|}{r - 1}}
\code{r} being the number of columns/rows, and the Fleiss-Cohen weights by \deqn{1 - \frac{(i - j)^2}{(r - 1)^2}}
The latter attaches greater importance to closer disagreements.\cr
\cr
Data can be passed to the function either as matrix or data.frame in \code{x}, or as two numeric vectors \code{x} and \code{y}. In the latter case \code{table(x, y, ...)} is calculated. Thus \code{NA}s are handled the same way as \code{\link{table}} does. Note that tables are by default calculated \bold{without} NAs. The specific argument \code{useNA} can be passed via the ... argument.\cr
The vector interface \code{(x, y)} is only supported for the calculation of unweighted kappa. This is because we cannot ensure a safe construction of a confusion table for two factors with different levels, which is independent of the order of the levels in \code{x} and \code{y}. So weights might lead to inconsistent results. The function will raise an error in this case.
%%  ~~ If necessary, more details than the description above ~~
}

\value{if no confidence intervals are requested:
the estimate as numeric value\cr\cr
else a named numeric vector with 3 elements
\item{est}{estimate}
\item{lci}{lower confidence interval}
\item{uci}{upper confidence interval}
}

\references{
Cohen, J. (1960) A coefficient of agreement for nominal scales. \emph{Educational and Psychological Measurement}, 20, 37-46.

Everitt, B.S. (1968), Moments of statistics kappa and weighted kappa. \emph{The British Journal of Mathematical and Statistical Psychology}, 21, 97-103.

Fleiss, J.L., Cohen, J., and Everitt, B.S. (1969), Large sample standard errors of kappa and weighted kappa. \emph{Psychological Bulletin}, 72, 332-327.

Cicchetti, D.V., Allison, T. (1971) A New Procedure for Assessing Reliability
of Scoring EEG Sleep Recordings \emph{American Journal of EEG Technology}, 11,
101-109.
}
\author{
David Meyer <david.meyer@r-project.org>, some changes and tweaks Andri Signorell <andri@signorell.net>
}


\seealso{ \code{\link{CronbachAlpha}}, \code{\link{KappaM}}, \code{\link{KrippAlpha}}
}
\examples{
# from Bortz et. al (1990) Verteilungsfreie Methoden in der Biostatistik, Springer, pp. 459
m <- matrix(c(53,  5, 2,
              11, 14, 5,
               1,  6, 3), nrow=3, byrow=TRUE,
            dimnames = list(rater1 = c("V","N","P"), rater2 = c("V","N","P")) )

# confusion matrix interface
CohenKappa(m, weight="Unweighted")

# vector interface
x <- Untable(m)
CohenKappa(x$rater1, x$rater2, weight="Unweighted")

# pairwise Kappa
rating <- data.frame(
  rtr1 = c(4,2,2,5,2, 1,3,1,1,5, 1,1,2,1,2, 3,1,1,2,1, 5,2,2,1,1, 2,1,2,1,5),
  rtr2 = c(4,2,3,5,2, 1,3,1,1,5, 4,2,2,4,2, 3,1,1,2,3, 5,4,2,1,4, 2,1,2,3,5),
  rtr3 = c(4,2,3,5,2, 3,3,3,4,5, 4,4,2,4,4, 3,1,1,4,3, 5,4,4,4,4, 2,1,4,3,5),
  rtr4 = c(4,5,3,5,4, 3,3,3,4,5, 4,4,3,4,4, 3,4,1,4,5, 5,4,5,4,4, 2,1,4,3,5),
  rtr5 = c(4,5,3,5,4, 3,5,3,4,5, 4,4,3,4,4, 3,5,1,4,5, 5,4,5,4,4, 2,5,4,3,5),
  rtr6 = c(4,5,5,5,4, 3,5,4,4,5, 4,4,3,4,5, 5,5,2,4,5, 5,4,5,4,5, 4,5,4,3,5)
)

PairApply(rating, FUN=CohenKappa, symmetric=TRUE)

# Weighted Kappa
cats <- c("<10\%", "11-20\%", "21-30\%", "31-40\%", "41-50\%", ">50\%")
m <- matrix(c(5,8,1,2,4,2, 3,5,3,5,5,0, 1,2,6,11,2,1,
              0,1,5,4,3,3, 0,0,1,2,5,2, 0,0,1,2,1,4), nrow=6, byrow=TRUE,
            dimnames = list(rater1 = cats, rater2 = cats) )
CohenKappa(m, weight="Equal-Spacing")


# supply an explicit weight matrix
ncol(m)
(wm <- outer(1:ncol(m), 1:ncol(m), function(x, y) {
        1 - ((abs(x-y)) / (ncol(m)-1)) } ))
CohenKappa(m, weight=wm, conf.level=0.95)


# however, Fleiss, Cohen and Everitt weight similarities
fleiss <- matrix(c(
  106, 10,  4,
  22,  28, 10,
   2,  12,  6
  ), ncol=3, byrow=TRUE)

#Fleiss weights the similarities
weights <- matrix(c(
 1.0000, 0.0000, 0.4444,
 0.0000, 1.0000, 0.6666,
 0.4444, 0.6666, 1.0000
 ), ncol=3)

CohenKappa(fleiss, weights)


# using the formula interface
d.long <- data.frame(
  subj = c("1", "2", "3", "4", "5", "1", "2", "3", "4", "5", 
           "1", "2", "3", "4", "5", "1", "2", "3", "4", "5"), 
  rater = c("rtr1", "rtr1", "rtr1", "rtr1", "rtr1", "rtr2", 
            "rtr2", "rtr2", "rtr2", "rtr2", "rtr3", "rtr3", 
            "rtr3", "rtr3", "rtr3", "rtr4", "rtr4", "rtr4", 
            "rtr4", "rtr4"), 
  rat = factor(c("V","V","V","V","P","V","N","V","V","P","V",
                 "P","V","V","P","V","V","V","V","N")))

CohenKappa(rat ~ subj | rater, d.long, 
           subset=rater \%in\% c("rtr1","rtr2"), conf.level = 0.95)

}

\keyword{ multivar }
\concept{ irr }
