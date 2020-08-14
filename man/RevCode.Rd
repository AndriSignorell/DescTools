\name{RevCode}
\alias{RevCode}

\title{Reverse Codes}

\description{In psychology variables often need to be recoded into the other direction (reverse coding). Let's say we have a Likert scale from 1 to 5 and we want to recode the variable so that a 5 becomes a 1, 4 a 2 and so on. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}

\usage{
RevCode(x, lbound = min(x, na.rm = TRUE), ubound = max(x, na.rm = TRUE))
}
\arguments{
  \item{x}{a numerical vector, will be converted to numeric if it's not.
}
  \item{lbound}{the low bound of the variable, default is the minmal value of \code{x}.
%%     ~~Describe \code{lbound} here~~
}
  \item{ubound}{the high bound of the variable, default is the maximal value of \code{x}.
%%     ~~Describe \code{ubound} here~~
}
}
\details{The function recodes based on:

  lbound + ubound - x

%%  ~~ If necessary, more details than the description above ~~
}
\value{the recoded vector
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{Recode}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
s <- c(3,4,2,7,4,9,NA,10) 
RevCode(s, lbound=1, ubound=10)

i <- c(1,0,0,0,1,1)
cbind(i, RevCode(i))
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
