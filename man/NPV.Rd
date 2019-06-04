\name{NPV}
\alias{NPV}
\alias{IRR}
\alias{OPR}
\alias{NPVFixBond}
\alias{YTM}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Short Selection of Financial Mathematical Functions
%%  ~~function to do ... ~~
}
\description{Calculate the one period returns, the net present value (\code{NPV()}),  the internal rate of return (\code{IRR()}) of a sequence of payments. \code{NPVFixBond()} returns the netpresent value for a fixed-rate bond, \code{YTM()} the yield to maturity for a bond.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
OPR(K, D = NULL, log = FALSE)
NPV(i, cf, t = seq(along = cf) - 1)
IRR(cf, t = seq(along = cf) - 1, interval = c(-1.5, 1.5), ...)

NPVFixBond(i, Co, RV, n)
YTM(Co, PP, RV, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i}{the interest rate
%%     ~~Describe \code{i} here~~
}
  \item{cf}{numeric vector with the payments
%%     ~~Describe \code{cf} here~~
}
  \item{t}{periods
%%     ~~Describe \code{t} here~~
}
  \item{K}{the capital at time t}
  \item{D}{dividend at time t }
  \item{log}{logical, determining if the simple returns (default) or log returns are to be calculated.}
\item{interval}{a vector containing the end-points of the interval to
    be searched for the root in the function \code{IRR}.}

   \item{Co}{coupon payments of a fixed-rate bond}
   \item{PP}{purchase price for a fixed-rate bond}
   \item{RV}{redemption value}
   \item{n}{the term of the bond, total number of periods}
\item{\dots}{the dots are passed to the \code{\link{UnirootAll}} function in \code{IRR}}

}
\value{a numeric value
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\details{The one period returns are calculated as
\deqn{r_t = \frac{D_t+K_t-K_t-1}{K_t-1}}

}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{Gmean}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# one root
IRR(cf <- c(-900, -250+450-90, 460-100, 500-120, 550-140))
# several IRR solutions
IRR(cf = c(-100, 500, -600))
# no solution
IRR(cf = c(-100, 400, -600))
# negative and huge solution
IRR(cf = c(-100, 1000, -600), interval = c(-1.5, 1000))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }
