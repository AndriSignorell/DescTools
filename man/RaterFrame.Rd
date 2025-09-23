\name{RaterFrame}
\alias{RaterFrame}
\title{Create a Data.frame for Interrater Agreement
}
\description{ Creates a \code{data.frame} for a formula  \code{rating ~ subjects | raters}
with the subjects in rows and the raters in columns as base structure for 
IRA functions.
}
\usage{
RaterFrame(formula, data, subset, na.action)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{something like \code{rating ~ subjects | raters}}
  \item{data}{ the data }
  \item{subset}{ potential subset}
  \item{na.action}{what should happen with missings}
}
\details{Here the details
}
\value{a data.frame containing structured data}
\author{Andri Signorell <andri@signorell.net>}

\seealso{\code{\link{KrippAlpha}}}

\examples{

d.long <- data.frame(
    rater = c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B", 
              "C", "C", "C", "C", "C"), 
    subj = c("1", "2", "3", "4", "5", "1", "2", "3", "4", "5", 
             "1", "2", "3", "4", "5"), 
    rating = c(1, 4, 5, 7, 2, 2, 5, 6, 7, 1, 1, 4, 6, 6, 2))

RaterFrame(rating ~ subj | rater, d.long)

RaterFrame(rating ~ subj | rater, d.long[-c(3, 6), ])

RaterFrame(rating ~ subj | rater, d.long[-c(3, 6), ], 
           na.action=na.omit)
}
\concept{ ~irr }
