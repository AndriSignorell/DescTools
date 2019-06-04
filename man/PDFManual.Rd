\name{PDFManual}
\alias{PDFManual}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get PDF Manual of a Package From CRAN
%%  ~~function to do ... ~~
}
\description{PDF versions of the manual are usually not included as vignettes in R packages. Still this format is convenient for reading and doing full text search. \cr This function creates the appropriate link to the pdf file on CRAN and opens the pdf manual in a browser window.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PDFManual(package)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{package}{name of the package.
%%     ~~Describe \code{library} here~~
}
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\examples{
\dontrun{
PDFManual(DescTools)
}
}

