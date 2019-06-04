\name{Append}
\alias{Append}
\alias{Append.matrix}
\alias{Append.data.frame}
\alias{Append.default}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Append Elements to Objects
%%  ~~function to do ... ~~
}
\description{Append elements to a number of various objects as vectors, matrices, data.frames and lists. In a matrix either rows or columns can be inserted at any position. In data frames any vectors can be inserted. \code{values} will be recycled to the necessary length.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Append(x, values, after = NULL, ...)

\method{Append}{matrix}(x, values, after = NULL, rows = FALSE, names = NULL, ...)
\method{Append}{data.frame}(x, values, after = NULL, names = NULL, ...)
\method{Append}{default}(x, values, after = NULL, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{object for the elements to be inserted
%%     ~~Describe \code{m} here~~
}
  \item{values}{the elements to be inserted
%%     ~~Describe \code{x} here~~
}
  \item{after}{a subscript, after which the values are to be appended. If it's missing the values will be appended after the last element (or column/row).
%%     ~~Describe \code{i} here~~
}
\item{rows}{logical, defining if vector should be added as row or as column. Default is column (\code{rows}=\code{FALSE}).}
  \item{names}{the dimension names for the inserted elements(s)
%%     ~~Describe \code{row.names} here~~
}
\item{\dots}{further arguments (not used here)}
}
\details{The vector \code{x} will be recycled to a length of the next multiple of the number of rows (or columns) of the matrix \code{m} and will be inserted such that the first inserted row (column) has the index \code{i}. If the dimnames are given, they will be used no matter if the matrix m has already dimnames defined or not.
%%  ~~ If necessary, more details than the description above ~~
}
\value{An object containing the values in x with the elements of values appended after the specified element of x.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{rbind}}, \code{\link{cbind}}, \code{\link{append}}
}
\examples{
Append(1:5, 0:1, after = 3)    # the same as append

# Insert columns and rows
x <- matrix(runif(25), 5)

Append(x, values=1:10, after=2, names = c("X","Y"))
Append(x, values=1:10, after=2)

Append(x, values=1:10, after=2, names = c("X","Y"))
Append(x, values=1:10, after=2)

# append to a data.frame
d.frm <- data.frame("id"   = c(1,2,3),
                    "code" = c("AAA", "BBB", "CCC"),
                    "val"  = c(111, 222, 333))
z <- c(10, 20, 30)

Append(d.frm, z, after=2, names="ZZZ")
}
\keyword{ manip }% __ONLY ONE__ keyword per line
