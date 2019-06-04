\name{AllDuplicated}
\alias{AllDuplicated}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Index Vector of All Values Involved in Ties

%%  ~~function to do ... ~~
}
\description{The function \code{\link{duplicated}} returns a logical vector indicating which elements x are duplicates, but will not include the very first appearance of subsequently duplicated elements. \code{AllDuplicated} returns an index vector of ALL the values in \code{x} which are involved in ties. \cr
So \code{!AllDuplicated} can be used to determine all elements of x, which
 appear exactly once (thus with frequency 1).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AllDuplicated(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector of any type.}
}

\value{logical vector of the same dimension as x.
}

\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{unique}} returns a unique list of all values in x\cr
\code{\link{duplicated}} returns an index vector flagging all elements, which appeared more than once (leaving out the first appearance!)\cr
\code{\link{union}}(A, B) returns a list with the unique values from A and B\cr
\code{\link{intersect}} returns all elements which appear in A and in B\cr
\code{\link{setdiff}}(A, B) returns all elements appearing in A but not in B \cr
\code{\link{setequal}}(A, B) returns \code{TRUE} if A contains exactly the same elements as B\cr
\code{\link{split}}(A, A) returns a list with all the tied values in A (see examples)\cr

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- c(1:10, 4:6)

AllDuplicated(x)

# compare to:
duplicated(x)

x[!AllDuplicated(x)]

# union, intersect and friends...
A <- c(sort(sample(1:20, 9)),NA)
B <- c(sort(sample(3:23, 7)),NA)

# all elements from A and B (no duplicates)
union(A, B)
# all elements appearing in A and in B
intersect(A, B)
# elements in A, but not in B
setdiff(A, B)
# elements in B, but not in A
setdiff(B, A)
# Does A contain the same elements as B?
setequal(A, B)


# Find ties in a vector x
x <- sample(letters[1:10], 20, replace=TRUE)
ties <- split(x, x)

# count tied groups
sum(sapply(ties, length) > 1)

# length of tied groups
(x <- sapply(ties, length))[x>1]

# by means of table
tab <- table(x)
tab[tab>1]

# count elements involved in ties
sum(tab>1)
# count tied groups
sum(tab[tab>1])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
