\name{RndPairs}
\alias{RndPairs}
\alias{RndWord}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create Pairs of Correlated Random Numbers
%%  ~~function to do ... ~~
}
\description{Create pairs of correlated random numbers.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RndPairs(n, r, rdist1 = rnorm(n = n, mean = 0, sd = 1),
         rdist2 = rnorm(n = n, mean = 0, sd = 1), prop = NULL)

RndWord(size, length, x = LETTERS, replace = TRUE, prob = NULL)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{number of pairs. If length(n) > 1, the length is taken to be the number required.
%%     ~~Describe \code{n} here~~
}
  \item{r}{the correlation between the two sets.
%%     ~~Describe \code{r} here~~
}
  \item{rdist1, rdist2}{the distribution of the random vector X1 and X2. Default is standard normal distribution.
%%     ~~Describe \code{rdist1} here~~
}
\item{size}{a non-negative integer giving the number of artificial words to build.}
\item{length}{a non-negative integer giving the length of the words.}
\item{x}{elements to choose from.}
\item{replace}{Should sampling be with replacement?}
\item{prop}{proportions for ordinal variable, must sum to 1.}
\item{prob}{a vector of probability weights for obtaining the elements of the vector being sampled.}
}
\value{a data.frame with 2 columns, X1 and X2 containing the random numbers
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{runif}}, \code{\link{rnorm}}, \code{\link{Random}} and friends
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# produce 100 pairs of a normal distributed random number with a correlation of 0.7
d.frm  <- RndPairs(n=100, r=0.7)

plot(d.frm)
lines(lm(y ~ x,d.frm))

# change the distribution
d.frm  <- RndPairs(n=100, r=0.7, rdist2 = rlnorm(n = 100, meanlog = 1, sdlog = .8))
d.frm  <- RndPairs(n=100, r=0.7, rdist2 = runif(n = 100, -1, 4))

x <- StrCap(sapply(sample(3:15, 10), function(i) RndWord(1, i, x=letters)))


# produce some artificial words with defined probabilities for the letters
p <- c(6.51,1.89,3.06,5.08,17.4,1.66,3.01,4.76,7.55,0.27,1.21,3.44,2.53,
       9.78,2.51,0.79,0.02,7,7.27,6.15,4.35,0.67,1.89,0.03,0.04,1.13)
sapply(sample(3:15, 10), function(i) RndWord(1, i, x=letters, prob=p))

# produce associated ordinal variables
d.ord <- RndPairs(500, r=0.8, prop = list(c(.15, .3, .55), 
                                      c(.3, .5, .2)))
levels(d.ord$y) <- levels(d.ord$x) <- LETTERS[1:3]
PlotMosaic(table(d.ord$x, d.ord$y), las=1, main="")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}% __ONLY ONE__ keyword per line
\keyword{IO}% __ONLY ONE__ keyword per line
