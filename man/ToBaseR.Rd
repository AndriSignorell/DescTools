\name{ToBaseR}
\alias{ToBaseR}
\title{ Back to Basics with Tibbles
}
\description{Sometimes we might wish for the old days be back and want to 
work with familiar objects. This function helps to convert \code{tibbles} 
to \code{data.frames} as smoothly as possible.
}
\usage{
ToBaseR(x, ...)
}
\arguments{
  \item{x}{the object to be converted.
}
  \item{\dots}{arguments passed on.
}
}
\value{
converted object}
\author{Andri Signorell <andri@signorell.net>
}
\examples{
\dontrun{
# read a Stata file
url <- "http://www.stata.com/videos13/data/webclass.dta"
d.webclass <- ToBaseR(haven::read_dta(url))

# read a SPSS file
url <- "https://stats.idre.ucla.edu/wp-content/uploads/2020/10/missing.sav"
d.miss <- ToBaseR(haven::read_sav(url))
}
}
