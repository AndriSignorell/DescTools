\name{RevCode}
\alias{RevCode}

\title{Reverse Codes}

\description{In psychology variables often need to be recoded into reverse order in cases that items are negatively worded. So it can be ensured that a high value indicate the same type of response on every item.
Let's say we have a Likert scale from 1 to 5 and we want to recode the variable so that a 5 becomes a 1, 4 a 2 and so on. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}

\usage{
RevCode(x)
}
\arguments{
  \item{x}{a numerical or logical vector, or a factor.}
}

\details{The function recodes based on:

  \code{min(x, na.rm=TRUE) + max(x, na.rm=TRUE) - x}

}
\value{the recoded vector}

\author{Andri Signorell <andri@signorell.net>}

\seealso{\code{\link{Recode}}}

\examples{
x <- 1:5
data.frame(x, rev_num=RevCode(x), rev_fac=RevCode(factor(x)))

s <- c(3,4,2,7,4,9,NA,10) 
RevCode(factor(s, levels=1:10))

i <- c(1,0,0,0,1,1)
cbind(i, RevCode(i))

k <- as.logical(c(1,0,0,0,1,1))
cbind(k, RevCode(k))

x <- factor(sample(letters[1:5], 10, replace = TRUE))
RevCode(x)

# we want to set the level 5 to NA before reversing
RevCode(factor(NAIf(x, "e")))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
