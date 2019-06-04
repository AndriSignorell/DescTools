\name{StrSpell}
\alias{StrSpell}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Spell a String Using the NATO Phonetic or the Morse Alphabet
%%  ~~function to do ... ~~
}
\description{The function splits a string into single characters and returns their representation in either the NATO phonetic alphabet or the Morse alphabet. The 26 code words in the NATO phonetic alphabet are assigned to the 26 letters of the English alphabet in alphabetical order as follows: Alfa, Bravo, Charlie, Delta, Echo, Foxtrot, Golf, Hotel, India, Juliett, Kilo, Lima, Mike, November, Oscar, Papa, Quebec, Romeo, Sierra, Tango, Uniform, Victor, Whiskey, X-ray, Yankee, Zulu. Digits 0-9 are also supported.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrSpell(x, upr = "CAP", type = c("NATO", "Morse"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{character, the string to be encoded.
%%     ~~Describe \code{x} here~~
}
  \item{upr}{character, a shortcut to be used to characterise capital letters. Ignored if \code{type} is set to \code{"Morse"}. No distinction is made between upper and lower case if \code{upr} is set to \code{NA} or to an empty string \code{""}.
%%     ~~Describe \code{upr} here~~
}
\item{type}{the type of phonetic alphabet, either \code{"NATO"} or \code{"Morse"}.
}
}
\value{a character vector containing the code words
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
\url{https://en.wikipedia.org/wiki/NATO_phonetic_alphabet}
%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{strsplit}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# ... ever had to communicate a password by phone? ;-)
StrSpell("Yailov9teb6i")

paste(StrSpell("Andri", type="Morse"), collapse="|")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ character}% __ONLY ONE__ keyword per line
