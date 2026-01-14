\name{Phrase}
\alias{Phrase}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Phrasing Results of t-Test
%%  ~~function to do ... ~~
}
\description{Formulating the results of a comparison of means is quite common. This function assembles a descriptive text about the results of a t-test, describing group sizes, means, p-values and confidence intervals.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Phrase(x, g, glabels = NULL, xname = NULL, unit = NULL, lang = "en", na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
%%     ~~Describe \code{x} here~~
}
  \item{g}{a vector or factor object giving the group for the corresponding elements of x. The number of levels must equal 2.
%%     ~~Describe \code{g} here~~
}
  \item{glabels}{the labels of the two groups, if left to NULL, the levels will be used.
%%     ~~Describe \code{glabels} here~~
}
  \item{xname}{the name of the variable to be used in the text.
%%     ~~Describe \code{xname} here~~
}
  \item{unit}{an optional unit for be appended to the numeric results.
%%     ~~Describe \code{unit} here~~
}
  \item{lang}{the language to be used. Only english (default) and german implemented (so far).
%%     ~~Describe \code{lang} here~~
}
\item{na.rm}{logical, should \code{NA}s be omitted? Defaults to \code{FALSE}.
%%     ~~Describe \code{na.rm} here~~
}

}
\value{a text
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{t.test}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data("cats", package = "MASS")
cat(Phrase(cats$Bwt, cats$Sex, xname="weight", unit="grams", glabels=c("female", "male")))

# oder auf deutsch
cat(Phrase(cats$Bwt, cats$Sex, xname="Geburtsgewicht",
           glabels=c("weiblich", "maennlich"), lang="german"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{character}% __ONLY ONE__ keyword per line
