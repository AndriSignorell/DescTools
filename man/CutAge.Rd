\name{CutAge}
\alias{CutAge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a Factor Variable by Cutting an Age Variable
%%  ~~function to do ... ~~
}
\description{Dividing the range of an age variable \code{x} into intervals is a frequent task. The commonly used function \code{\link{cut}} has unfavourable default values for this.
\code{CutAge()} is a convenient wrapper for cutting age variables in groups 
    of e.g. 10 years with more suitable defaults.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CutAge(x, from = 0, to = 90, by = 10, right = FALSE, ordered_result = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ continuous variable. }
  
  \item{from, to}{the starting and (maximal) end values of the sequence.
%%     ~~Describe \code{from} here~~
}
  \item{by}{number: increment of the sequence. Default is 10, alternatives could be 5 or 20.
%%     ~~Describe \code{by} here~~
}
  \item{right}{logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa. Default is \code{FALSE} - unlike in \code{\link{cut}}!
}
  \item{ordered_result}{	
logical: should the result be an ordered factor? Default is \code{TRUE} - unlike in \code{\link{cut}}!
}

  \item{\dots}{the dots are passed on to the underlying function \code{\link{cut}()}. Use these for e.g. change the labels. 
%%     ~~Describe \code{\dots} here~~
}
}
\value{
A factor is returned, unless labels = FALSE which results in an integer vector of level codes.

Values which fall outside the range of breaks are coded as \code{NA}, as are \code{NaN} and \code{NA} values.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{cut}}, \code{\link{seq}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Desc(CutAge(sample(100, 100)))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
