\name{SampleTwins}
\alias{SampleTwins}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sample Twins
%%  ~~function to do ... ~~
}
\description{Draw a twin sample out of a population for a given recordset, by matching some strata criteria.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SampleTwins(x, stratanames = NULL, twins, 
            method = c("srswor", "srswr", "poisson", "systematic"), 
            pik, description = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the data to draw the sample from
%%     ~~Describe \code{data} here~~
}
  \item{stratanames}{the stratanames to use
%%     ~~Describe \code{stratanames} here~~
}
  \item{twins}{the twin sample
%%     ~~Describe \code{twins} here~~
}
\item{method}{method to select units; the following methods are implemented: simple random 
sampling without replacement (srswor), simple random sampling with replacement (srswr), 
Poisson sampling (poisson), systematic sampling (systematic); if "method" is missing, 
the default method is "srswor". See \code{\link{Strata}}.}

\item{pik}{vector of inclusion probabilities or auxiliary information used to compute them; 
this argument is only used for unequal probability sampling (Poisson and systematic). If an
auxiliary information is provided, the function uses the inclusionprobabilities function for
computing these probabilities. If the method is "srswr" and the sample size is larger than
the population size, this vector is normalized to one.}

\item{description}{a message is printed if its value is TRUE; the message gives the number 
of selected units and the number of the units in the population. 
By default, the value is FALSE.}
}
\value{ 
The function produces an object, which contains the following information:
\item{id}{the identifier of the selected units.}
\item{stratum}{the unit stratum.}
\item{prob}{the final unit inclusion probability.}
}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{Strata}}, \code{\link{sample}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
m <- rbind(matrix(rep("nc",165), 165, 1, byrow=TRUE), 
           matrix(rep("sc", 70), 70, 1, byrow=TRUE))
m <- cbind.data.frame(m, c(rep(1, 100), rep(2,50), rep(3,15), 
                           rep(1,30), rep(2,40)), 1000*runif(235))
names(m) <- c("state","region","income")

# this would be our sample to be reproduced by a twin sample
d.smp <- m[sample(nrow(m), size=10, replace=TRUE),]

# draw the sample
s <- SampleTwins(x = m, stratanames=c("state","region"), twins = d.smp, method="srswor")

d.twin <- m[s$id,]
d.twin
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{survey }
