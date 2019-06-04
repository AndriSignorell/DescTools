\name{List Variety Of Objects}
\alias{LsFct}
\alias{LsObj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{List Objects, Functions Or Data in a Package
%%  ~~function to do ... ~~
}
\description{List all the objects, functions or data in a package.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LsObj(package)
LsFct(package)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{package}{the name of the package
%%     ~~Describe \code{package} here~~
}
}
\details{This is just a wrapper for \code{\link{ls}}, \code{\link{ls.str}} and \code{\link{lsf.str}} with the appropriate arguments (as I always forgot how to do the trick).
\code{LsObj()} lists all objects, \code{LsFct()} just the functions in a package.
%%  ~~ If necessary, more details than the description above ~~
}
\references{
Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The New S Language}. Wadsworth & Brooks/Cole.
%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{ls}}, \code{\link{ls.str}}, \code{\link{lsf.str}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
LsFct("DescTools")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities }
