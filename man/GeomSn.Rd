\name{GeomSn}
\alias{GeomSn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Geometric Series
%%  ~~function to do ... ~~
}
\description{A geometric sequence is a sequence, such that each term is given by a multiple of q of the previous one. A geometric series consists out of the sum of all former values of a geometric sequence..
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GeomSn(a1, q, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a1}{the first element of the sequence
%%     ~~Describe \code{a1} here~~
}
  \item{q}{the factor of the sequence
%%     ~~Describe \code{q} here~~
}
  \item{n}{number of elements to include in the sum
%%     ~~Describe \code{n} here~~
}
}
\value{the sum as numeric value
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{sum}}
}
\examples{
GeomSn(a1=3, q=2, n=5)

# calculates the sum of the first 5 elements of the sequence
(gseq <- 3 * (2^(0:5)))
sum(gseq)

GeomSn(a1=3, q=2, n=0:5)
}

