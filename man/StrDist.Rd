\name{StrDist}
\alias{StrDist}
\title{Compute Distances Between Strings}
\description{
 \code{StrDist} computes distances between strings following to Levenshtein or Hamming method.
}
\usage{
StrDist(x, y, method = "levenshtein", mismatch = 1, gap = 1, ignore.case = FALSE)
}
\arguments{
  \item{x}{character vector, first string.}
  \item{y}{character vector, second string.}
  \item{method}{character, name of the distance method. This must be
        \code{"levenshtein"}, \code{"normlevenshtein"} or \code{"hamming"}. Default is \code{"levenshtein"}, the classical
        Levenshtein distance.}
  \item{mismatch}{numeric, distance value for a mismatch between symbols.}
  \item{gap}{numeric, distance value for inserting a gap.}
  \item{ignore.case}{if \code{FALSE} (default), the distance measure will be case sensitive and if \code{TRUE}, case is ignored.}

}
\details{
  The function computes the Hamming and the Levenshtein (edit) distance of two given strings
  (sequences). The Hamming distance between two vectors is the number mismatches between corresponding entries.

  In case of the Hamming distance the two strings must have the same length.

  In case of the Levenshtein (edit) distance a scoring and a trace-back matrix are computed
  and are saved as attributes \code{"ScoringMatrix"} and \code{"TraceBackMatrix"}.
  The numbers in the trace-back matrix reflect insertion of a gap in string \code{y} (1),
  match/missmatch (2), and insertion of a gap in string \code{x} (3).

  The edit distance is useful, but normalizing the distance to fall within the interval [0,1] is
preferred because it is somewhat diffcult to judge whether an LD of for example 4 suggests a high or low degree of similarity.
  The method \code{"normlevenshtein"} for normalizing the LD is sensitive to this scenario.
In this implementation, the Levenshtein distance is transformed to fall in this interval as
follows:
\deqn{lnd = 1 - \frac{ld}{max(length(x), length(y))}}{lnd = 1 - ld / max(length(x), length(y))}

where \code{ld} is the edit distance and \code{max(length(x), length(y))} denotes that we divide by the length of the larger of the two character strings. This normalization, referred to as the Levenshtein normalized distance (lnd), yields a statistic where 1 indicates perfect agreement between the two strings, and a 0 denotes imperfect agreement. The closer a value is to 1, the more certain we can be that the character strings are the same; the closer to 0, the less certain.

}
\value{
  \code{StrDist} returns an object of class \code{"dist"}; cf. \code{\link[stats]{dist}}.
}
\references{
  R. Merkl and S. Waack (2009) \emph{Bioinformatik Interaktiv}. Wiley.

  Harold C. Doran (2010) \emph{MiscPsycho. An R Package for Miscellaneous Psychometric Analyses}
}
\author{Matthias Kohl <Matthias.Kohl@stamats.de> }
\note{
  For distances between strings and for string alignments see also Bioconductor package
  \pkg{Biostrings}
}

\seealso{\code{\link[utils]{adist}}, \code{\link[stats]{dist}} }
\examples{
x <- "GACGGATTATG"
y <- "GATCGGAATAG"
## Levenshtein distance
d <- StrDist(x, y)
d
attr(d, "ScoringMatrix")
attr(d, "TraceBackMatrix")

## Hamming distance
StrDist(x, y, method="hamming")
}
\keyword{math}
\keyword{character}
\keyword{utilities}

