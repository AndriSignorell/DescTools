\name{MADCI}
\alias{MADCI}
\alias{MADDiffCI}
\alias{MADRatioCI}
\title{Confidence Intervals for Median Absolute Deviations }

\description{A function for the median absolute deviation is 
included in base R, \code{\link{mad}}, but there's no function 
for calculating confidence intervals. Arachchige/Prendergast 
introduce interval estimators of the MAD to make reliable inferences 
for dispersion for a single population, as well as for differences and ratios
of MADs for comparing two populations.  }

\usage{
MADCI(x,  
      conf.level = 0.95, sides = c("two.sided","left","right"), 
      gld.method = "TM", na.rm = FALSE, ...)      

MADDiffCI(x, y, 
      conf.level = 0.95, sides = c("two.sided","left","right"), 
      gld.method = "TM", na.rm = FALSE, ...)      

MADRatioCI(x, y,  
      conf.level = 0.95, sides = c("two.sided","left","right"), 
      gld.method = "TM", na.rm = FALSE, ...)      
}

\arguments{

  \item{x}{a (non-empty) numeric vector of data values.
}
  \item{y}{a second (non-empty) numeric vector of data values.
}
  \item{conf.level}{confidence level of the interval.
}
\item{sides}{a character string specifying the side of the confidence interval, must be one of \code{"two.sided"} (default), \code{"left"} or \code{"right"}. You can specify just the initial letter. \code{"left"} would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.}

  \item{gld.method}{A character string, to select the estimation method for the generalized lambda distribution. One of: \code{ML} for numerical Maximum Likelihood, \code{MPS} or \code{MSP} for Maximum Spacings Product, \code{TM} for Titterington's Method (default), \code{SM} for Starship Method, \code{TL} for method of Trimmed L-moments, \code{Lmom} for method of L-moments, \code{DLA} for the method of Distributional Least Absolutes, or \code{Mom} for method of Moments. 
  See \code{\link[gld]{fit.fkml}()}.
}
  \item{na.rm}{logical. Should missing values be removed? Defaults to \code{FALSE}.
}
\item{\dots}{further arguments, are passed on to the boot function.}
 }
\value{a numeric vector with 3 elements:
  \item{mad}{median absolute deviation}
  \item{lci}{lower bound of the confidence interval}
  \item{uci}{upper bound of the confidence interval}
}

\references{
Arachchige Chandima N. P. G., Prendergast Luke A. (2019) Confidence intervals for median absolute deviations, arXiv:1910.00229 [math.ST]
}
\author{Arachchige Chandima N. P. G., Prendergast Luke A., 
 Andri Signorell <andri@signorell.net> (only interface)
}

\seealso{\code{\link{mad}}, \code{\link{MAD}}
}
\examples{
x <- rlnorm(100)
y <- rlnorm(200, meanlog=1.2)

MADCI(x)                        # single sample
MADDiffCI(x, y)                 # two sample difference
MADRatioCI(x, y)                # two sample squared ratio 

# Bootstrapinterface
MADCI(x, method="boot", type="bca", R=499)  
}
