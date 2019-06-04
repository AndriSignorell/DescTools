\name{GetNewXL}
\alias{GetNewXL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a New Excel Instance
%%  ~~function to do ... ~~
}
\description{Start a new instance of Excel and return its handle.
This is needed to address the Excel application and objects afterwards.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GetNewXL(visible = TRUE, newdoc = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{visible}{logical, should Excel made visible? Defaults to \code{TRUE}.
%%     ~~Describe \code{visible} here~~
}
  \item{newdoc}{logical, determining if a new workbook should be created. Defaults to \code{TRUE}.
  }
}

\details{
Here's a list of some frequently used commands.\cr
Let's assume:
\preformatted{xl <- GetNewXL()
}
\tabular{ll}{
workbooks \tab \code{xl$workbooks()$count()} \cr
quit excel \tab \code{xl$quit()} \cr
}

}
\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{XLView}}, \code{\link{XLGetRange}}, \code{\link{XLGetWorkbook}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{ # Windows-specific example
# get a handle to a new excel instance
xl <- GetNewXL()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
