\name{GetCurrWrd}
\alias{GetCurrWrd}
\alias{GetCurrXL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get a Handle to a Running Word Instance
%%  ~~function to do ... ~~
}
\description{Look for a running Word instance and return its handle. \code{NULL} is returned if nothing's found.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GetCurrWrd()
GetCurrXL()
}


\value{a handle (pointer) to the running Word, resp. Excel instance. 
}
\note{Closing an instance does not update the value of the pointer. So it may contain an invalid address. 
Whether the pointer is still valid can be checked by \code{\link{IsValidHwnd}}. }

\note{This does unfortunately not work with RDCOMClient (but it would with rcom)! Any better idea out there?}

\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{GetNewWrd}}, \code{\link{IsValidHwnd}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{# Windows-specific example

x <- rnorm(100)

wrd <- GetCurrWrd()

if(IsValidHwnd(wrd)){ 
  Desc(x, wrd=wrd)
} else {
  print("GetCurrWrd: no running word instance found...")
}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
