\name{SortMixed}
\alias{SortMixed}
\alias{OrderMixed}
\title{Sort Strings with Embedded Numbers Based on Their Numeric
 Order}
\description{
  These functions sort or order character strings containing embedded
  numbers so that the numbers are numerically sorted rather than sorted
  by character value.  I.e. "Asprin 50mg" will come before
  "Asprin 100mg".  In addition, case of character strings is ignored so
  that "a", will come before "B" and "C".
}
\usage{
SortMixed(x, decreasing=FALSE, na.last=TRUE, blank.last=FALSE,
          numeric.type=c("decimal", "roman"),
          roman.case=c("upper","lower","both") )

OrderMixed(x, decreasing=FALSE, na.last=TRUE, blank.last=FALSE,
          numeric.type=c("decimal", "roman"),
          roman.case=c("upper","lower","both") )
}
\arguments{
  \item{x}{vector to be sorted.}
  \item{decreasing}{logical.  Should the sort be increasing or
    decreasing? Note that \code{descending=TRUE} reverses the meanings of
    \code{na.last} and \code{blanks.last}.}
  \item{na.last}{logical, controlling the treatment of \code{NA} values.  If \code{TRUE}, missing
    values in the data are put last; if \code{FALSE}, they are put
    first; if \code{NA}, they are removed.}
  \item{blank.last}{logical, controlling the treatment of blank values.  If \code{TRUE}, blank
    values in the data are put last; if \code{FALSE}, they are put
    first; if \code{NA}, they are removed.}
  \item{numeric.type}{either \code{"decimal"} (default) or \code{"roman"}.  Are numeric values represented as decimal numbers (\code{numeric.type="decimal"}) or as Roman numerals
    (\code{numeric.type="roman"})? }
  \item{roman.case}{one of \code{"upper"}, \code{"lower"}, or \code{"both"}.  Are roman
    numerals represented using only capital letters ('IX') or lower-case
    letters ('ix') or both?}
}
\details{
  I often have character vectors (e.g. factor labels), such as compound
  and dose, that contain both text and numeric data.  This function
  is useful for sorting these character vectors into a logical order.

  It does so by splitting each character vector into a sequence of
  character and numeric sections, and then sorting along these sections,
  with numbers being sorted by numeric value (e.g. "50" comes before
  "100"), followed by characters strings sorted by character
  value (e.g. "A" comes before "B") \emph{ignoring case} (e.g. 'A' has
  the same sort order as 'a').

  By default, sort order is ascending, empty strings are sorted to the front,
  and \code{NA} values to the end.   Setting \code{descending=TRUE}
  changes the sort order to descending and reverses the meanings of
  \code{na.last} and \code{blank.last}.

  Parsing looks for decimal numbers unless \code{numeric.type="roman"},
  in which parsing looks for roman numerals, with character case
  specified by \code{roman.case}.

}
\value{
  \code{OrderMixed} returns a vector giving the sort order of the input
  elements. \code{SortMixed} returns the sorted vector.
}
\author{ Gregory R. Warnes \email{greg@warnes.net} }
\seealso{ \code{\link[base]{sort}}, \code{\link[base]{order}} }
\examples{
## compound & dose labels
Treatment <- c("Control", "Asprin 10mg/day", "Asprin 50mg/day",
               "Asprin 100mg/day", "Acetomycin 100mg/day",
               "Acetomycin 1000mg/day")

## ordinary sort puts the dosages in the wrong order
sort(Treatment)

## but SortMixed does the 'right' thing
SortMixed(Treatment)

## Here is a more complex example
x <- rev(c("AA 0.50 ml", "AA 1.5 ml", "AA 500 ml", "AA 1500 ml",
           "EXP 1", "AA 1e3 ml", "A A A", "1 2 3 A", "NA", NA, "1e2",
           "", "-", "1A", "1 A", "100", "100A", "Inf"))

OrderMixed(x)

SortMixed(x)  # Notice that plain numbers, including 'Inf' show up
              # before strings, NAs at the end, and blanks at the
              # beginning .


SortMixed(x, na.last=TRUE)  # default
SortMixed(x, na.last=FALSE) # push NAs to the front


SortMixed(x, blank.last=FALSE) # default
SortMixed(x, blank.last=TRUE)  # push blanks to the end

SortMixed(x, decreasing=FALSE) # default
SortMixed(x, decreasing=TRUE)  # reverse sort order

## Roman numerals
chapters <- c("V. Non Sequiturs", "II. More Nonsense",
              "I. Nonsense", "IV. Nonesensical Citations",
              "III. Utter Nonsense")
SortMixed(chapters, numeric.type="roman" )

## Lower-case Roman numerals
vals <- c("xix", "xii", "mcv", "iii", "iv", "dcclxxii",   "cdxcii",
          "dcxcviii",   "dcvi",   "cci")
(ordered <- SortMixed(vals, numeric.type="roman", roman.case="lower"))
RomanToInt(ordered)
}
\keyword{univar}
\keyword{manip}
\concept{natural sort}
\concept{dictionary sort}

