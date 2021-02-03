\name{Between, Outside}
\alias{Between}
\alias{\%()\%}
\alias{\%(]\%}
\alias{\%[)\%}
\alias{\%[]\%}

\alias{\%][\%}
\alias{\%)[\%}
\alias{\%](\%}
\alias{\%)(\%}
\alias{\%:\%}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Operators To Check, If a Value Lies Within Or Outside a Given Range
}
\description{
The between and outside operators are used to check, whether a vector of given values x lie within a defined range (or outside respectively). The values can be numbers, text or dates.
Ordered factors are supported.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
x \%()\% rng
x \%(]\% rng
x \%[)\% rng
x \%[]\% rng

x \%][\% rng
x \%](\% rng
x \%)[\% rng
x \%)(\% rng

x \%:\% rng

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  is a variable with at least ordinal scale, usually a numeric value, but can be an ordered factor or a text as well. Texts would be treated alphabetically.
%%     ~~Describe \code{x} here~~
}
  \item{rng}{
  a vector of two values or a matrix with 2 columns, defining the minimum and maximum of the range for x. \cr If rng is a matrix, x or rng will be recycled.
%%     ~~Describe \code{rng} here~~
}
}
\details{The between operators basically combine two conditional statements into one and simplify so the query process.\cr
They are merely a wrapper for:   \code{x >= rng[1] & x <= rng[2]},
where the round bracket  \code{(}  means "strictly greater than >" and the square bracket  \code{[}  means ">=".  Numerical values of x will be handled by C-code, which is significantly faster than two comparisons in R (especially when x is huge).
.\cr
\code{\%][\%} is the negation of \code{\%()\%}, meaning all values lying outside the given range. Elements on the limits will return TRUE.
\cr
\code{\%:\%} returns all the elements of a vector between the (first found) element \code{rng[1]} and \code{rng[2]}. If no match is found it returns \code{NA}. If \code{rng[2]} occurs first in the vector the elements will be returned in reverse order (which is the same behaviour as the \code{:} operator.
\cr
Both arguments will be recycled to the highest dimension, which is either the length of the vector or the number of rows of the matrix.\cr
See also the routines used to check, whether two ranges overlap (\code{\link{Overlap}}, \code{\link{Interval}}).
}
\value{
A logical vector of the same length as x.
}
\author{
Andri Signorell <andri@signorell.net> based on C-code by Kevin Ushey <kevinushey@gmail.com>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{if}}, \code{\link{ifelse}}, \code{\link{Comparison}},
\code{\link{Overlap}}, \code{\link{Interval}}  }

\examples{
x <- 1:9
x \%[]\% c(3,5)

# outside
x <- 1:9
x \%][\% c(3,5)

c(x,NA) \%[]\% c(3,5)

x \%(]\% c(3,5)

# no result when from > to:
x \%[]\% c(5,3)
x \%(]\% c(5,5)

# no problem:
ordered(x) \%[]\% c(3,5)

# not meaningful:
factor(x) \%[]\% c(3,5)

# characters
letters[letters \%(]\% c("d","h")]

data(d.pizza)
x <- levels(d.pizza$driver)
x \%[]\% c("C","G")

# select diamonds with a price between 2400 and 2510
data(d.diamonds)
d.diamonds[d.diamonds$price \%[]\% c(2400,2510),]

# use it with an ordered factor and select all diamonds with
#   symmetry between G (included) and X (excluded).
mean(d.diamonds[d.diamonds$symmetry \%[)\% c("G","X"),"price"])


# use multiple ranges
2 \%[]\% cbind(1:4,2:5)

# both arguments are recycled
c(2,3) \%[]\% cbind(1:4,2:5)


# between operator for vector positions
set.seed(4)
(x <- sample(LETTERS, size=10, replace=TRUE))
# [1] "X" "K" "S" "C" "G" "L" "S" "V" "U" "Z"

# return all elements between "S" and "L" 
x \%:\% c("S","L")
# [1] "S" "C" "G" "L"
 
x \%:\% c("S","A")
# [1] "S" "C" "G" "L" "S" "V" "U" "Z"
 
x \%:\% c("A","S")
# [1] "X" "K" "S"

# reverted matches return the elements in reverse order
x \%:\% c("G","X")
# [1] "G" "C" "S" "K" "X"

# no match results in NA
x \%:\% c("Y","B")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{logic}
\keyword{manip}

