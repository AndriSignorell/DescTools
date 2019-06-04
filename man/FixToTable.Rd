\name{FixToTable}
\alias{FixToTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert a Text to a Table
%%  ~~function to do ... ~~
}
\description{Convert a text to a table by using complete columns of spaces (or any other separator) as delimiting point.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
FixToTable(txt, sep = " ", delim = "\t", trim = TRUE, header = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{txt}{the text to be partitioned. Works best, if txt is  a matrix.
%%     ~~Describe \code{txt} here~~
}
  \item{sep}{the separator to use. Will frequently be " ".
%%     ~~Describe \code{sep} here~~
}
  \item{delim}{the new delimiter to insert. (default tab)
%%     ~~Describe \code{delim} here~~
}
  \item{trim}{logical. Should the separated text be trimmed from whitespace? Defaults to TRUE.
%%     ~~Describe \code{trim} here~~
}
  \item{header}{logical. Should the first line be interpreted as header?
%%     ~~Describe \code{header} here~~
}
}
\details{Only a complete appearance of the separator character in the same position over all rows will
be accepted as column delimiter.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a matrix of the separated text.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{
\code{\link{StrChop}}
}

\examples{
# let's get some tabbed text
txt <- matrix(capture.output(Titanic[,,2,1]))
FixToTable(txt[-1,])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip}


