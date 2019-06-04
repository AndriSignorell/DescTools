\name{GiniSimpson}
\alias{GiniSimpson}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute Gini-Simpson Coefficient
%%  ~~function to do ... ~~
}
\description{Calculate the Gini-Simpson coefficient.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GiniSimpson(x, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector containing at least non-negative elements.}
  \item{na.rm}{logical. Should missing values be removed? Defaults to FALSE.}
}

\details{
The original Simpson index \eqn{\lambda}{lambda} equals the probability that two entities taken at random from the dataset of interest (with replacement) represent the same type. 
The Simpson index was introduced in 1949 by Edward H. Simpson to measure the degree of concentration when individuals are classified into types. The same index was rediscovered by Orris C. Herfindahl in 1950. The square root of the index had already been introduced in 1945 by the economist Albert O. Hirschman. As a result, the same measure is usually known as the Simpson index in ecology, and as the Herfindahl index or the Herfindahl-Hirschman index (HHI) in economics.\cr
Its transformation 1 - \eqn{\lambda}{lambda} therefore equals the probability that the two entities represent different types. 
This measure is also known in ecology as the probability of interspecific encounter (PIE) and the Gini-Simpson index.
}

\value{a numeric value.
}
\references{
Cover Thomas M. and Thomas Joy A. (1991) \emph{Elements of Information Theory}. Wiley. 
%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{DivCoef}}, \code{\link{Entropy}}, \code{\link{Gini}}, \code{\link{Herfindahl}} 
}
\examples{
x <- c(261,29,33,15,39,28,95,5,6,28,69,8,105,38,15)

GiniSimpson(x)

# is the same as 
1 - Herfindahl(x)

GiniSimpson(c(783,121,112,70,201,153,425,19,37,126,325,51,442,193,41))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar}
