\name{WrdStyle}
\alias{WrdStyle}
\alias{WrdStyle<-}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get or Set the Style in Word
%%  ~~function to do ... ~~
}
\description{\code{WrdStyle} can be used to get and set the style in Word for the text to be inserted. \code{WrdStyle} returns the style at the current cursor position.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdStyle(wrd = DescToolsOptions("lastWord"))
WrdStyle(wrd) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{value}{the name of the style to be used to the output. This should be defined an existing name.
%%     ~~Describe \code{font} here~~
}
  \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()}
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{DescToolsOptions("lastWord")}.}

}
\value{
character, name of the style

}
\author{
Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{ToWrd}}, \code{\link{WrdPlot}}, \code{\link{GetNewWrd}}, \code{\link{GetCurrWrd}}
}
\examples{
\dontrun{ # Windows-specific example

wrd <- GetNewWrd()
# the current stlye
WrdStyle(wrd)
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
