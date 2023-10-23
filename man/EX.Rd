\name{EX}
\alias{EX}
\alias{VarX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Expected Value and Variance 
%%  ~~function to do ... ~~
}
\description{Expected Value and Variance for the distribution of a discrete random variable.
(For didactical purposes..)
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EX(x, p)
VarX(x, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the values of the random variable
%%     ~~Describe \code{x} here~~
}
  \item{p}{the probabilities of the values
%%     ~~Describe \code{p} here~~
}
}
\value{numeric value
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\examples{
EX(x=c(1:3), p=c(0.2, 0.5, 0.3))
VarX(x=c(1:3), p=c(0.2, 0.5, 0.3))
}
