\name{StrAlign}
\alias{StrAlign}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{String Alignment
%%  ~~function to do ... ~~
}
\description{Align a vector of strings to the left, to the right, to the center or to the first occurance of a specified character, e.g. to the decimal separator. Alignment is achieved by padding the strings with empty spaces (which evidently only will have an alignment effect if the text is displayed with a monospaced font).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrAlign(x, sep = "\\\\r")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a character vector to be aligned.
%%     ~~Describe \code{x} here~~
}
  \item{sep}{the character on whose position the strings will be aligned. Left alignment can be requested by setting \code{sep = "\\\\l"}, right alignment by \code{"\\\\r"} and center alignment by \code{"\\\\c"}. Mind the backslashes, as if they are omitted, strings would be aligned to the \bold{character} l, r or c respectively. Default value is \code{"\\\\r"}, thus right alignment.
%%     ~~Describe \code{sep} here~~
}
}
\details{Alignment to the left, right or center leave no room for misinterpretation. The function will determine the maximum string size in the vector, resize all the strings to this size by padding empty spaces either at the beginning or at the end of the strings.
When it comes to center strings, it's not clear where to place strings with an even length in case the maximum length is odd (or vice versa). We will put the shorter distance of an uneven string at the left.\cr
Any specific length of the strings can be created by \code{\link{StrPad}} if required.

In case of a given separator the strings will be aligned towards this separator. If a string does not contain the separator, the affected string will be aligned as if it had a separator as last character. This seems to be a good default, when integer numbers are to be aligned with numerical values. Note that the character length of the resulting strings can excceed the maximum length of the supplied strings.

The character strings will not be pruned of whitespaces, if the requested alignment does not explicitly require it. \code{\link{StrTrim}} can be used for that.
}
\value{a character vector containing the aligned strings
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{StrTrim}}, \code{\link{StrPad}}, \code{\link{Format}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# align on (the first occuring) B
x <- c("ABCDMNB", "CDGHEBK", "BCI")
cbind(StrAlign(x, sep="B"))

# align to decimal point
z <- c("    6.0", "6.00  ", " 45.12    ", "784", NA)
cbind(StrAlign(z, sep="."))

# right align, the width will be the max number of characters in x
cbind(StrAlign(x, sep="\\\\r"))
# left align
cbind(StrAlign(x, sep="\\\\l"))
# center
cbind(StrAlign(x, sep="\\\\c"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{character}
