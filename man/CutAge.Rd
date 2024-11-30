\name{CutAge}
\alias{CutAge}
\alias{CutGen}
\title{Create a Factor Variable by Cutting an Age Variable
}
\description{Dividing the range of an age variable \code{x} into intervals is a frequent task. The commonly used function \code{\link{cut}} has unfavourable default values for this.
\code{CutAge()} is a convenient wrapper for cutting age variables in groups 
    of e.g. 10 years with more suitable defaults.\cr
    \code{CutGen} yields the generation of a person based on the year of birth.
    
    
}
\usage{
CutAge(x, from = 0, to = 90, by = 10, right = FALSE, ordered_result = TRUE, ...)
CutGen(vintage)
}
\arguments{
  \item{x}{ continuous variable. }
  
  \item{from, to}{the starting and (maximal) end values of the sequence.
}
  \item{by}{number: increment of the sequence. Default is 10, alternatives could be 5 or 20.
}
  \item{right}{logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa. Default is \code{FALSE} - unlike in \code{\link{cut}}!
}
  \item{ordered_result}{	
logical: should the result be an ordered factor? Default is \code{TRUE} - unlike in \code{\link{cut}}!
}

  \item{\dots}{the dots are passed on to the underlying function \code{\link{cut}()}. Use these for e.g. change the labels. 
}
\item{vintage}{year of birth}
}
\details{
The generations are defined as:\cr

1946-1964 Babyboomer\cr 
1965-1979 Generation X \cr
1980-1995 Generation Y  – also known as Millennials\cr
1996-2010 Generation Z \cr
2011-.... Generation Alpha\cr 

}

\value{
A factor is returned, unless labels = FALSE which results in an integer vector of level codes.

Values which fall outside the range of breaks are coded as \code{NA}, as are \code{NaN} and \code{NA} values.
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{cut}}, \code{\link{seq}}
}
\examples{
Desc(CutAge(sample(100, 100)))
}
