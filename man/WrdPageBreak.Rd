\name{WrdPageBreak}
\alias{WrdPageBreak}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Insert a Page Break
%%  ~~function to do ... ~~
}
\description{Insert a page break in a MS-Word (R) document at the position of the cursor.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdPageBreak(wrd = DescToolsOptions("lastWord"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()}
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{DescToolsOptions("lastWord")}.}
}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{WrdFont}}, \code{\link{WrdPlot}}, \code{\link{GetNewWrd}}, \code{\link{GetCurrWrd}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{ # Windows-specific example
wrd <- GetNewWrd()
WrdText("This is text on page 1.\n\n")
WrdPageBreak()
WrdText("This is text on another page.\n\n")
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
\concept{MS-Office}
