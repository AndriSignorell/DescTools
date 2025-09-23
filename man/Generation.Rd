\name{Generation}
\alias{Generation}
\title{Generation by Birthyear
}
\description{Yields the generation of a person based on the year of birth.
    
    
}
\usage{
Generation(year)
}
\arguments{
\item{year}{year of birth}
}
\details{
The generations are defined as:\cr

\tabular{ll}{
\code{1946-1964} \verb{  } \tab Babyboomer\cr 
\code{1965-1979} \tab  Generation X \cr
\code{1980-1995} \tab  Generation Y  – also known as Millennials\cr
\code{1996-2010} \tab  Generation Z \cr
\code{2011-2025} \tab  Generation Alpha\cr 
}
}

\value{
Ordered factor with levels \code{c("Babyboomer", "Gen X", "Millennials", "Gen Z", "Gen Alpha")}\cr
Values which fall outside the range of breaks are coded as \code{NA}, as are \code{NaN} and \code{NA} values.
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{CutAge}}, \code{\link{Zodiac}}
}
\examples{
Generation(c(1946, 1964, 1972, 2001, 2003, 2018, 2026))
}
