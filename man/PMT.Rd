\name{PMT}
\alias{PMT}
\alias{IPMT}
\alias{PPMT}
\alias{RBAL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Periodic Payment of an Annuity.
%%  ~~function to do ... ~~
}
\description{\code{PMT} computes the periodic payment of an annuity.
\code{IPMT} calculates what portion of a period payment is going towards interest in a particular period and
\code{PPMT} what portion of a period payment is going towards principal in a particular period. \code{RBAL} yields the remaining balance in a particular period.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PMT(rate, nper, pv, fv = 0, type = 0)
IPMT(rate, per, nper, pv, fv = 0, type = 0)
PPMT(rate, per, nper, pv, fv = 0, type = 0)
RBAL(rate, per, nper, pv, fv = 0, type = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rate}{specifies the interest rate.
%%     ~~Describe \code{rate} here~~
}
  \item{per}{specifies the period of the payment to be applied to interest or to principal.
%%     ~~Describe \code{nper} here~~
}
  \item{nper}{specifies the number of payment periods.
%%     ~~Describe \code{nper} here~~
}
  \item{pv}{specifies the present value or the lump-sum amount that a series of future payments is worth currently. \code{pv} can be 0 if a positive \code{fv} argument is included.
%%     ~~Describe \code{pv} here~~
}
  \item{fv}{specifies the future value or a cash balance that you want to attain after the last payment is made. Default is 0.
%%     ~~Describe \code{fv} here~~
}
  \item{type}{specifies the number 0 or 1 and indicates when payments are due. Default is 0.
%%     ~~Describe \code{type} here~~
}
}
\value{a numeric value
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{ \code{\link{NPV}}, \code{\link{SLN}}
%% ~~objects to See Also as, ~~~
}
\examples{
# original principal:    20'000
# loan term (years):     5
# annual interest rate:  8%
# annual payment:        -4'156.847

# simple amortization schedule
cbind(
  year      = 1:5,
  payment   = PMT(rate=0.08, nper=5, pv=20000, fv=-5000, type=0),
  interest  = IPMT(rate=0.08, per=1:5, nper=5, pv=20000, fv=-5000, type=0),
  principal = PPMT(rate=0.08, per=1:5, nper=5, pv=20000, fv=-5000, type=0),
  balance   = RBAL(rate=0.08, per=1:5, nper=5, pv=20000, fv=-5000, type=0)
)

#     year   payment   interest principal   balance
# [1,]    1 -4156.847 -1600.0000 -2556.847 17443.153
# [2,]    2 -4156.847 -1395.4523 -2761.395 14681.759
# [3,]    3 -4156.847 -1174.5407 -2982.306 11699.452
# [4,]    4 -4156.847  -935.9562 -3220.891  8478.562
# [5,]    5 -4156.847  -678.2849 -3478.562  5000.000


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }% use one of  RShowDoc("KEYWORDS")
