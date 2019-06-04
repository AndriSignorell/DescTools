\name{wdConst}
\alias{wdConst}
\alias{xlConst}
\docType{data}

\title{Word VBA Constants
}

\description{This is a list with all VBA constants for MS Word 2010, which is useful for writing R functions based on recorded macros in Word.
This way the constants need not be replaced by their numeric values and can only be complemented with the list's name,
say the VBA-constant \code{wd10Percent} for example can be replaced by \code{wdConst$wd10Percent}.
A handful constants for Excel are consolidated in \code{xlConst}.
}

\usage{
data(wdConst)
data(xlConst)}

\format{
The format is:\cr
List of 2755\cr
$ wd100Words:    num -4\cr
$ wd10Percent:   num -6\cr
$ wd10Sentences: num -2\cr
...\cr
}

\source{Microsoft
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}

\keyword{datasets}
