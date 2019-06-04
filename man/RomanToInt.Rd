\name{RomanToInt}
\alias{RomanToInt}
\title{Convert Roman Numerals to Integers}
\description{
Convert roman numerals to integers
}
\usage{
RomanToInt(x)
}
\arguments{
  \item{x}{character vector containing roman numerals}
}
\details{
  This functon will convert roman numerals to integers without the upper bound
  imposed by R (3899), ignoring case.
}
\value{
  A integer vector with the same length as \code{roman}.  Character
  strings which are not valid roman numerals will be converted to \code{NA}.
}
\author{
  Gregory R. Warnes <greg@warnes.net>
}
\seealso{
  \code{\link[utils]{as.roman}}
}
\examples{
RomanToInt( c('I', 'V', 'X', 'C', 'L', 'D', 'M' )  )

# works regardless of case
RomanToInt( 'MMXVI' )
RomanToInt( 'mmxvi' )

# works beyond R's limit of 3899
val.3899 <- 'MMMDCCCXCIX'
val.3900 <- 'MMMCM'
val.4000 <- 'MMMM'
as.numeric(as.roman( val.3899 ))
as.numeric(as.roman( val.3900 ))
as.numeric(as.roman( val.4000 ))

RomanToInt(val.3899)
RomanToInt(val.3900)
RomanToInt(val.4000)

}
\keyword{arith}
