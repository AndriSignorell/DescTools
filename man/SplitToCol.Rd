\name{SplitToCol}
\alias{SplitToCol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Split Data Frame String Column Into Multiple Columns
%%  ~~function to do ... ~~
}
\description{Splitting the string columns of a data frame into multiple columns requires a considerable number of codelines, which are condensed in this function for convenience.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SplitToCol(x, split = " ", fixed = TRUE, na.form = "", colnames = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a data frame containing the string columns to be splitted.
%%     ~~Describe \code{x} here~~
}
  \item{split}{
    character vector (or object which can be coerced to such)
    containing \link{regular expression}(s) (unless \code{fixed = TRUE})
    to use for splitting.  If empty matches occur, in particular if
    \code{split} has length 0, \code{x} is split into single characters.
    If \code{split} has length greater than 1, it is re-cycled along
    \code{x}.
  }
  \item{fixed}{
    logical.  If \code{TRUE} match \code{split} exactly, otherwise
    use regular expressions.  Has priority over \code{perl}.
  }
  \item{na.form}{character, string specifying how \code{NAs} should be specially formatted. Default is a blank \code{""}.}
  \item{colnames}{columnnames for the resulting data.frame. Will be recycled. Can easily be set to \code{""} if no columnnames should be set. }
}
\value{A data.frame with all the columns splitted

A vector with the length of the number of columns of the data.frame containing the number of the found columns is returned as attribute namede \code{"ncols"}.

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{strsplit}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
d.frm <- data.frame(res1=c("2 [-3,5] **", "5 [-2,6] ***", "9 [-3,1]"),
                    res2=c("5 [6,8] **", "7 [-2,9]", "4 [3,5] **"), 
                    stringsAsFactors=FALSE)

SplitToCol(d.frm, na.form="-", colnames=c("coef", "ci", "pval"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ character }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
