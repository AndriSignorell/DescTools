\name{HmsToSec}
\alias{HmsToSec}
\alias{SecToHms}
\title{Convert h:m:s To/From Seconds}
\usage{
HmsToSec(x)
SecToHms(x, digits = NULL)
}
\arguments{
\item{x}{A vector of times in h:m:s (for \code{HmsToSec}) or seconds (for
\code{SecToHms}).}
\item{digits}{the number of digits to use for potential fractions of seconds.}
}
\value{
\code{HmsToSec} - Returns a vector of times in seconds.

\code{SecToHms} - Returns a vector of times in h:m:s format.
}
\description{
\code{HmsToSec} - Converts a vector of h:m:s to seconds.

\code{SecToHms} - Converts a vector of seconds to h:m:s.
}
\author{Tyler Rinker <tyler.rinker@gmail.com>}
\examples{
HmsToSec(c("02:00:03", "04:03:01"))
HmsToSec(SecToHms(c(222, 1234, 55)))
SecToHms(c(256, 3456, 56565))
}
\seealso{
\code{\link[chron]{times}}
}
\keyword{chron}

