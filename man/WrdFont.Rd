\name{WrdFont}
\alias{WrdFont}
\alias{WrdFont<-}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get or Set the Font in Word
%%  ~~function to do ... ~~
}
\description{\code{WrdFont} can be used to get and set the font in Word for the text to be inserted. \code{WrdFont} returns the font at the current cursor position.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdFont(wrd = DescToolsOptions("lastWord"))
WrdFont(wrd) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{value}{the font to be used to the output. This should be defined as a list containing fontname, fontsize, bold and italic flags:\cr \code{list(name="Arial", size=10, bold=FALSE, italic=TRUE, color=wdConst$wdColorBlack)}.
%%     ~~Describe \code{font} here~~
}
  \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()}
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{DescToolsOptions("lastWord")}.}

}
\details{The font color can be defined by a Word constant beginning with \code{wdConst$wdColor}.
The defined colors can be listed with \code{grep("wdColor", names(wdConst), val=TRUE)}.
}
\value{
a list of the attributes of the font in the current cursor position:
  \item{name}{the fontname}
  \item{size}{the fontsize}
  \item{bold}{bold}
  \item{italic}{italic}
  \item{color}{the fontcolor}

}
\author{
Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{ToWrd}}, \code{\link{WrdPlot}}, \code{\link{GetNewWrd}}, \code{\link{GetCurrWrd}}
}
\examples{
\dontrun{ # Windows-specific example

wrd <- GetNewWrd()

for(i in seq(10, 24, 2))
  ToWrd(gettextf("This is Arial size %s \n", i), font=list(name="Arial", size=i))

for(i in seq(10, 24, 2))
  ToWrd(gettextf("This is Times size %s \n", i), font=list(name="Times", size=i))
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
\keyword{MS-Office}
