\name{SaveAs}
\alias{SaveAs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Saves an R Object Under a Different Name
%%  ~~function to do ... ~~
}
\description{An R object cannot be saved in binary mode under a different name using the default \code{save()} function. \code{SaveAs()} extends the save function for this option. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SaveAs(x, objectname, file, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the object to save
%%     ~~Describe \code{x} here~~
}
  \item{objectname}{the new name for the object.
%%     ~~Describe \code{objectname} here~~
}
  \item{file}{a (writable binary-mode) connection or the name of the file where the data will be saved (when tilde expansion is done). 
%%     ~~Describe \code{fn} here~~
}
  \item{\dots}{the dots are passed to the save function.
%%     ~~Describe \code{\dots} here~~
}
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{save}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- stats::runif(20)
SaveAs(x=x, objectname="NewX", file = "NewXFile.rda")
unlink("NewXFile.rda")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }% use one of  RShowDoc("KEYWORDS")
