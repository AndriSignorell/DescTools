\name{Vigenere}
\alias{Vigenere}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Vigenere Cypher}

\description{Implements a Vigenere cypher, both encryption and decryption. The function handle keys and text of unequal length and discards non-alphabetic characters. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Vigenere(x, key = NULL, decrypt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the text to be encrypted
%%     ~~Describe \code{x} here~~
}
  \item{key}{the key to be used. If this remains to \code{NULL} the PasswordDlg will be presented and the key can be entered there.
%%     ~~Describe \code{key} here~~
}
  \item{decrypt}{boolean defining if the text should be encrypted or decrypted.
%%     ~~Describe \code{decrypt} here~~
}
}
\details{
All characters beside charlist = c(LETTERS, letters, 0:9) will be discarded from the text and from the key.
}

\value{the encrypted, resp. decrypted text
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>\cr
strongly based on code found at \url{http://rosettacode.org/wiki/Vigen\%C3\%A8re_cipher#R} (credits to the unknown soldier)
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
key <- "My FavoriteKey452"
(xenc <- Vigenere("Beware the Jabberwock, my son! The jaws that bite, the claws that catch!", key))

Vigenere(xenc, key, decrypt = TRUE)
# note that everything besides the characters in the list will be discarded
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
