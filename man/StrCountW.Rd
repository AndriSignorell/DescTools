\name{StrCountW}
\alias{StrCountW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Count Words in a String
%%  ~~function to do ... ~~
}
\description{Count the number of words that appear within a character string. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrCountW(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector of strings to be parsed.
%%     ~~Describe \code{x} here~~
}
}
\details{This is just a wrapper for a fine regexpr. It uses the expression \code{\\b\\W+\\b} to separate the 
words. The code \code{\\W} is equivalent to \code{[^[:alnum:]_])} wheras \code{[:alnum:]} contains \code{[:alpha:]} and \code{[:digit:]}.
So everything that is not an alphanumeric character, a digit or a _ (underscore) is used as separator for the words to be counted.
%%  ~~ If necessary, more details than the description above ~~
}
\value{an integer defining the number of word in the string
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{\url{http://stackoverflow.com/questions/8920145/count-the-number-of-words-in-a-string-in-r}}
\author{Andri Signorell <andri@signorell.net>, based on code from Adam Bradley <hisself@adambradley.net>
%%  ~~who you are~~
}

\seealso{\code{\link{nchar}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
StrCountW("This is a true story!")

StrCountW("Just_one_word")
StrCountW("Not-just.one/word")

StrCountW("And what about numbers 8899 or special characters $$$/*?")
StrCountW("   Starting'n ending with some whitespace  ")

StrCountW(c("This is a", "text in more", "than one line."))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{character}
\keyword{utilities}
