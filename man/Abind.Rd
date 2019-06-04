\name{Abind}
\alias{Abind}
\title{Combine Multidimensional Arrays}
\description{
 Base R functions \code{cbind} and \code{rbind} bind columns and rows, but there's no built-in function for binding higher dimensional datastructures like matrices. \code{Abind} takes a sequence of
 vectors, matrices, or arrays and produces a single array of
 the same or higher dimension.
}
\usage{
Abind(..., along = N, rev.along = NULL, new.names = NULL, force.array = TRUE,
      make.names = FALSE, use.first.dimnames = FALSE, hier.names = FALSE,
      use.dnns = FALSE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{\dots}{ Any number of vectors, matrices, arrays, or data frames.
The dimensions of all the arrays must match, except on one dimension
(specified by \code{along=}).  If these arguments are named, the name
will be used for the name of the dimension along which the arrays are
joined.  Vectors are treated as having a dim attribute of length one.

Alternatively, there can be one (and only one) list argument supplied,
whose components are the objects to be bound together.  Names of the
list components are treated in the same way as argument names.
}

\item{along}{The dimension along which to bind the arrays.
The default is the last dimension, i.e., the maximum length of the dim
attribute of the supplied arrays.  \code{along=} can take any
non-negative value up to the minimum length of the dim attribute of
supplied arrays plus one.  When \code{along=} has a fractional value, a
value less than 1, or a value greater than N (N is the maximum of the
lengths of the dim attribute of the objects to be bound together), a new
dimension is created in the result.  In these cases, the dimensions of
all arguments must be identical.  }

\item{rev.along}{
Alternate way to specify the dimension along which to bind the arrays:
\code{along = N + 1 - rev.along}.  This is provided mainly to allow easy
specification of \code{along = N + 1} (by supplying
\code{rev.along=0}).  If both \code{along} and \code{rev.along} are
supplied, the supplied value of \code{along} is ignored.
}

\item{new.names}{
If new.names is a list, it is the first choice for the
dimnames attribute of the result.  It should have the same
structure as a dimnames attribute.  If the names for a
particular dimension are \code{NULL}, names for this dimension are
constructed in other ways.

If \code{new.names} is a character vector, it is used for dimension
names in the same way as argument names are used.  Zero
length ("") names are ignored.
}
  \item{force.array}{  If \code{FALSE}, rbind or cbind are
called when possible, i.e., when the arguments are all vectors, and
along is not 1, or when the arguments are vectors or matrices or data
frames and along is 1 or 2.  If rbind or cbind are used, they will
preserve the data.frame classes (or any other class that r/cbind
preserve).  Otherwise, Abind will convert objects to class array.  Thus,
to guarantee that an array object is returned, supply the argument
\code{force.array=TRUE}.  Note that the use of rbind or cbind introduces
some subtle changes in the way default dimension names are constructed:
see the examples below.  }

\item{make.names}{
If \code{TRUE}, the last resort for dimnames for the along
dimension will be the deparsed versions of anonymous
arguments.  This can result in cumbersome names when
arguments are expressions.

    <p>The default is \code{FALSE}.
}

  \item{use.first.dimnames}{
    When dimension names are present on more than one
argument, should dimension names for the result be take from
the first available (the default is to take them from the
last available, which is the same behavior as
\code{rbind} and \code{cbind}.)
}
\item{hier.names}{
  If \code{TRUE}, dimension names on the concatenated dimension will be
  composed of the argument name and the dimension names of the objects
  being bound.  If a single list argument is supplied, then the names of
  the components serve as the argument names.  \code{hier.names} can
  also have values \code{"before"} or \code{"after"}; these determine
  the order in which the argument name and the dimension name are put
  together (\code{TRUE} has the same effect as \code{"before"}).}
\item{use.dnns}{ (default \code{FALSE}) Use names on dimensions, e.g.,
  so that \code{names(dimnames(x))} is non-empty.  When there are
  multiple possible sources for names of dimnames, the value of
  \code{use.first.dimnames} determines the result.
}
}
\details{
The dimensions of the supplied vectors or arrays do not need
to be identical, e.g., arguments can be a mixture of vectors
and matrices.  \code{Abind} coerces arguments by the addition
of one dimension in order to make them consistent with other
arguments and \code{along=}.  The extra dimension is
added in the place specified by \code{along=}.

The default action of Abind is to concatenate on the last
dimension, rather than increase the number of dimensions.
For example, the result of calling Abind with vectors is a
longer vector (see first example below).  This differs from
the action of \code{rbind} and cbind which is to return a matrix when
called with vectors.  Abind can be made to behave like cbind
on vectors by specifying \code{along=2}, and like rbind by
specifying \code{along=0}.

The dimnames of the returned object are pieced together
from the dimnames of the arguments, and the names of the
arguments.  Names for each dimension are searched for in the
following order: new.names, argument name, dimnames (or
names) attribute of last argument, dimnames (or names)
attribute of second last argument, etc.  (Supplying the
argument \code{use.first.dimnames=TRUE} changes this to
cause \code{Abind} to use dimnames or names from the
first argument first.  The default behavior is the same as
for \code{rbind} and \code{cbind}: use dimnames
from later arguments.)  If some names are supplied for the
along dimension (either as argument names or dimnames in
arguments), names are constructed for anonymous arguments
unless \code{maken.ames=FALSE}.
}
\value{

  An array with a dim attribute calculated as follows.

  Let \code{rMin=min(sapply(list(...), function(x) length(dim(x))))} and \cr
  \code{rMax=max(sapply(list(...), function(x) length(dim(x))))} (where
  the length of the dimensions of a vector are taken to be 1).  Then \code{rMax} should be
  equal to or one greater than \code{rMin}.

  If \code{along} refers to an existing dimension, then the length of
  the dim attribute of the result is \code{rMax}.  If \code{along} does
  not refer to an existing dimension, then \code{rMax} should equal
  \code{rMin} and the length of the dim attribute of the result will be \code{rMax+1}.

\code{rbind} or \code{cbind} are
called to compute the result if (a)
\code{force.array=FALSE}; and (b) the result will be a
two-dimensional object.

}
\author{Tony Plate <tplate@acm.org> and Richard Heiberger }
\note{
It would be nice to make \code{Abind()} an S3 generic, but S3 generics
cannot dispatch off anonymous arguments.

The ability of \code{Abind()} to accept a single list argument removes
much of the need for constructs like \code{do.call("Abind",
  list.of.arrays)}.  Instead, just do \code{Abind(list.of.arrays)}.  The
direct construct is preferred because \code{do.call()} construct can
sometimes consume more memory during evaluation.
}
\seealso{\code{\link{rbind}}, \code{\link{cbind}}, \code{\link{array}}}

\examples{
# Five different ways of binding together two matrices
x <- matrix(1:12, 3, 4)
y <- x + 100
dim(Abind(x, y, along=0))     # binds on new dimension before first
dim(Abind(x, y, along=1))     # binds on first dimension
dim(Abind(x, y, along=1.5))
dim(Abind(x, y, along=2))
dim(Abind(x, y, along=3))
dim(Abind(x, y, rev.along=1)) # binds on last dimension
dim(Abind(x, y, rev.along=0)) # binds on new dimension after last

# Unlike cbind or rbind in that the default is to bind
# along the last dimension of the inputs, which for vectors
# means the result is a vector (because a vector is
# treated as an array with length(dim(x))==1).
Abind(x=1:4, y=5:8)

# Like cbind
Abind(x=1:4, y=5:8, along=2)
Abind(x=1:4, matrix(5:20, nrow=4), along=2)
Abind(1:4, matrix(5:20, nrow=4), along=2)

# Like rbind
Abind(x=1:4, matrix(5:20, nrow=4), along=1)
Abind(1:4, matrix(5:20, nrow=4), along=1)

# Create a 3-d array out of two matrices
Abind(x=matrix(1:16, nrow=4), y=matrix(17:32, nrow=4), along=3)

# Use of hier.names
Abind(x=cbind(a=1:3, b=4:6), y=cbind(a=7:9, b=10:12), hier.names=TRUE)

# Use a list argument
Abind(list(x=x, y=x), along=3)
# Use lapply(..., get) to get the objects
an <- c('x', 'y')
names(an) <- an
Abind(lapply(an, get), along=3)
}
\keyword{ manip }% at least one, from doc/KEYWORDS
\keyword{ array }% __ONLY ONE__ keyword per line
