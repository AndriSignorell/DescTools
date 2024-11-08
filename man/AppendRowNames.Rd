\name{AppendRowNames}
\alias{AppendRowNames}
\title{Append Rownames to a Data Frame
}
\description{Append rownames to a data.frame as first column.
}
\usage{
AppendRowNames(x, names = "rownames", after = 0, remove_rownames = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a data.frame
%%     ~~Describe \code{x} here~~
}
  \item{names}{the name of the new inserted column containing the rownames.
%%     ~~Describe \code{names} here~~
}
  \item{after}{a subscript, after which the values are to be appended. 
  If missing the rownames will be inserted as first column.
%%     ~~Describe \code{after} here~~
}
  \item{remove_rownames}{logical defining if the existing rownames should be removed. 
  Default is \code{TRUE}.
%%     ~~Describe \code{remove_rownames} here~~
}
}
\value{the object x with appended rownames
}
\author{Andri Signorell <andri@signorell.net> }
\seealso{
\code{\link{Append}}
}
\examples{
(dd <- data.frame(x=1:5, y=6:10, z=LETTERS[1:5], 
                  row.names = letters[1:5]))
AppendRowNames(dd)
}
