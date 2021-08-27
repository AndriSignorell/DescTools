\name{GetCurrWrd}
\alias{GetCurrWrd}
\alias{GetCurrXL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get a Handle to a Running Word/Excel Instance
%%  ~~function to do ... ~~
}
\description{Look for a running Word, resp. Excel instance and return its handle. If no running instance is found a new instance will be created (which will be communicated with a warning).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GetCurrWrd()
GetCurrXL()
}


\value{a handle (pointer) to the running Word, resp. Excel instance. 
}
\note{When closing an application instance, the value of the pointer in R is not somehow automatically invalidated. In such cases the corresponding variable contains an invalid address. 
Whether the pointer still refers to a valid running application instance can be checked by \code{\link{IsValidHwnd}}. }

%% \note{This does unfortunately not work with RDCOMClient (but it would with rcom)! Any better idea out there?}
%% it does work with RDCOMClient, what was it exactly?


\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{GetNewWrd}}, \code{\link{IsValidHwnd}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{# Windows-specific example

# Start a new instance
GetNewWrd()

# grab the handle to this instance
wrd <- GetCurrWrd()

# this should be valid
IsValidHwnd(wrd)

# close the instance
wrd$quit()

# now it should be gone and the pointer invalid
if(IsValidHwnd(wrd)){ 
  print("Ouups! Still there?")
} else {  
  print("GetCurrWrd: no running word instance found...")
}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
