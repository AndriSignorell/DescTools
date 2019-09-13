\name{Eps}
\alias{Eps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Greenhouse-Geisser And Huynh-Feldt Epsilons
%%  ~~function to do ... ~~
}
\description{Calculate Greenhouse-Geisser and Huynh-Feldt epsilons.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Eps(S, p, g, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{p x p covariance matrix}
  \item{p}{dimension of observation vectors}
  \item{g}{number of groups}
  \item{n}{number of subjects}
}
\value{
a numeric value
}
\references{
Vonesh, E.F., Chinchilli, V.M. (1997) \emph{Linear and Nonlinear Models for the Analysis of Repeated Measurements} Marcel Dekker, New York, p.84-86

Crowder, M.J., Hand, D.J. (1990) \emph{Analysis of Repeated Measures}. Chapman & Hall, London, p.54-55
%% ~put references to the literature/web site here ~
}
\author{Hans Rudolf Roth <hroth@retired.ethz.ch>
%%  ~~who you are~~
}

\seealso{\code{\link{aov}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## find!

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression}

