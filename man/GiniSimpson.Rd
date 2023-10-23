\name{GiniSimpson}
\alias{GiniSimpson}
\alias{GiniDeltas}

\alias{HunterGaston}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Gini-Simpson Coefficient, Gini-Deltas coefficient and Hunter-Gaston Index
%%  ~~function to do ... ~~
}
\description{Calculate the Gini-Simpson coefficient, the Gini variant proposed by Deltas and the Hunter-Gaston Index.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GiniSimpson(x, na.rm = FALSE)
GiniDeltas(x, na.rm = FALSE)

HunterGaston(x, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a factor containing at least non-negative elements.}
  \item{na.rm}{logical. Should missing values be removed? Defaults to \code{FALSE}.}
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

Hunter, P., Gaston, A. G. (1988) Numerical Index of the Discriminatory Ability of Typing Systems:  an Application of Simpson's Index of Diversity, \emph{JOURNAL OF CLINICAL MICROBIOLOGY}, Nov. 1988, p. 2465-2466, 0095-1137/88/112465-02$02.00/0

Deltas (2003) DOI:10.1162/rest.2003.85.1.226.

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
