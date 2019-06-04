\name{Base Conversions}
\alias{BinToDec}
\alias{DecToBin}
\alias{OctToDec}
\alias{DecToOct}
\alias{HexToDec}
\alias{DecToHex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Converts Numbers From Binmode, Octmode or Hexmode to Decimal and Vice Versa
%%  ~~function to do ... ~~
}
\description{These functions convert numbers from one base to another.
There are several solutions for this problem out there, but the naming is quite heterogeneous and so
consistent function names might be helpful.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BinToDec(x)
DecToBin(x)
OctToDec(x)
DecToOct(x)
HexToDec(x)
DecToHex(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector of numbers, resp. alphanumerical representation of numbers (hex), to be converted.
%%     ~~Describe \code{x} here~~
}
}
\details{BinToDec converts numbers from binary mode into decimal values. DecToBin does it the other way round.\cr
Oct means octal system and hex hexadecimal.

%%  ~~ If necessary, more details than the description above ~~
}
\value{
A numeric or character vector of the same length as x containing the converted values.
Binary, octal and decimal values are numeric, hex-values are returned as class \code{hexmode}.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{strtoi}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
DecToBin(c(17, 25))
BinToDec(c(101, 11101))

DecToOct(c(17, 25))
OctToDec(c(11, 77))

DecToHex(c(17, 25))
HexToDec(c("FF", "AA", "ABC"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }

