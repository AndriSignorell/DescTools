\name{Entropy}
\alias{Entropy}
\alias{MutInf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Shannon Entropy and Mutual Information
%%  ~~function to do ... ~~
}
\description{Computes Shannon entropy and the mutual information of two variables. The entropy quantifies the expected value of the information contained in a vector. The mutual information is a quantity that measures the mutual dependence of the two random variables. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Entropy(x, y = NULL, base = 2, \dots)

MutInf(x, y, base = 2, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector or a matrix of numerical or categorical type. If only x is supplied it will be interpreted as 
  contingency table.
%%     ~~Describe \code{x} here~~
}
  \item{y}{a vector with the same type and dimension as x. If y is not \code{NULL} then the entropy of \code{table(x, y, ...)} 
  will be calculated.
%%     ~~Describe \code{x} here~~
}
  \item{base}{base of the logarithm to be used, defaults to 2.
%%     ~~Describe \code{x} here~~
}
 \item{\dots}{further arguments are passed to the function \code{\link{table}}, allowing i.e. to set \code{useNA}.
  }

}
\details{The Shannon entropy equation provides a way to estimate the average minimum number of bits needed to encode a string of symbols, based on the frequency of the symbols.\cr 
It is given by the formula \eqn{H = - \sum(\pi log(\pi))} where \eqn{\pi} is the 
probability of character number i showing up in a stream of characters of the given "script".\cr
The entropy is ranging from 0 to Inf.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a numeric value.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Shannon, Claude E. (July/October 1948). A Mathematical Theory of Communication, \emph{Bell System Technical Journal} 27 (3): 379-423.

Ihara, Shunsuke (1993) \emph{Information theory for continuous systems}, World Scientific. p. 2. ISBN 978-981-02-0985-8. 

%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{package \pkg{entropy} which implements various estimators of entropy
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

Entropy(as.matrix(rep(1/8, 8)))

# http://r.789695.n4.nabble.com/entropy-package-how-to-compute-mutual-information-td4385339.html
x <- as.factor(c("a","b","a","c","b","c")) 
y <- as.factor(c("b","a","a","c","c","b")) 

Entropy(table(x), base=exp(1))
Entropy(table(y), base=exp(1))
Entropy(x, y, base=exp(1))

# Mutual information is 
Entropy(table(x), base=exp(1)) + Entropy(table(y), base=exp(1)) - Entropy(x, y, base=exp(1))
MutInf(x, y, base=exp(1))

Entropy(table(x)) + Entropy(table(y)) - Entropy(x, y)
MutInf(x, y, base=2)

# http://en.wikipedia.org/wiki/Cluster_labeling
tab <- matrix(c(60,10000,200,500000), nrow=2, byrow=TRUE)
MutInf(tab, base=2) 

d.frm <- Untable(as.table(tab))
str(d.frm)
MutInf(d.frm[,1], d.frm[,2])

table(d.frm[,1], d.frm[,2])

MutInf(table(d.frm[,1], d.frm[,2]))


# Ranking mutual information can help to describe clusters
#
#   r.mi <- MutInf(x, grp)
#   attributes(r.mi)$dimnames <- attributes(tab)$dimnames
# 
#   # calculating ranks of mutual information
#   r.mi_r <- apply( -r.mi, 2, rank, na.last=TRUE )
#   # show only first 6 ranks
#   r.mi_r6 <- ifelse( r.mi_r < 7, r.mi_r, NA) 
#   attributes(r.mi_r6)$dimnames <- attributes(tab)$dimnames
#   r.mi_r6
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }

