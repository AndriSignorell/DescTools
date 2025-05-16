\name{DoBy}
\alias{DoBy}
\alias{DoBy.formula}
\alias{DoBy.default}

\title{Evaluates a Function Groupwise
}
\description{Split the vector x into partitions and apply the function to each partition separately. Computation restarts for each partition.\cr
The logic is the same as the OLAP functions in SQL, e.g. \code{SUM(x) OVER (PARTITION BY group)}.
}
\usage{
DoBy(x, ...)

\method{DoBy}{formula}(formula, data = parent.frame(), subset, na.action,
     vnames = NULL, ...)
\method{DoBy}{default}(x, by, FUN, vnames = NULL, collapse = FALSE, ...)
}
\arguments{
  \item{x}{a vector that should be operated.
}

  \item{by}{list of one or more factors, each of same length as \code{x}. If \code{by} is not a factor, the elements are coerced to factors by \code{\link{as.factor}()}.
}
  \item{FUN}{Function to apply for each factor level combination.
}
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the data values and rhs the corresponding groups.}
  \item{data}{an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula \code{formula}.
    By default the variables are taken from the \code{parent.frame()}.}
  \item{subset}{an optional vector specifying a subset of observations to be used.}
  \item{na.action}{a function which indicates what should happen when the data contain \code{NA}s. Defaults to \code{getOption("na.action")}.}

\item{vnames}{ name for the new variables.}
\item{collapse}{ logical, determining if the results should be collapsed to groups. Default is \code{FALSE}.}
  \item{\dots}{optional arguments to \code{FUN}: See the "Note" section.
}

}
\note{Optional arguments to \code{FUN} supplied by the ... argument are not divided into cells. It is therefore inappropriate for \code{FUN} to expect additional arguments with the same length as \code{x}.}

\details{This is more or less the same as the function \code{ave}, with the arguments organized a bit different and offering more flexibility.
}
\value{a data.frame with the same number of rows as length as \code{x} containing the groupwise results of \code{FUN} and the used group factors.\cr
The attribute \code{response} denotes the name of the response variable in case the formula interface was used.
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{ave}}, \code{\link{tapply}}, \code{\link{aggregate}}
}
\examples{

d.frm <- data.frame(x=rep(1:4,3), v=sample(x=1:3, size=12, replace=TRUE),
                    b=round(runif(12, 1, 10), 1),
                    g=gl(4,3,labels=letters[1:4]), m=gl(3,4,labels=LETTERS[1:3]))

# SQL-OLAP: sum() over (partition by g)
DoBy(d.frm$x, d.frm$g, FUN=sum, vnames="sum")
# DoBy(d.frm$x, FUN=sum)

# more than 1 grouping variables are organized as list as in tapply:
DoBy(d.frm$x, list(d.frm$g, d.frm$m), mean, vnames=c("g", "m"))

# count
d.frm$count <- DoBy(d.frm$x, d.frm$g, length)

# rank
d.frm$rank <- DoBy(d.frm$v, d.frm$g, rank)
d.frm$dense_rank <- DoBy(d.frm$v, d.frm$g, Rank, ties.method="dense")
d.frm$rank_desc <- DoBy(d.frm$x, d.frm$g, function(x) rank(-x))

# row_number
d.frm$row_number <- DoBy(d.frm$v, d.frm$g, function(x) order(x))
d.frm


# difference to preceding record per group
d.frm$dd <- with(d.frm[order(d.frm$m, d.frm$x), ], 
                        DoBy(b, m, 
                             FUN = function(x) c(NA, diff(x)),
                             vnames = "diff")[, -c(1:2)]
                 )

# change compared to preceding
d.frm$char <- as.character(d.frm$g)
d.frm$chg <- with(d.frm[order(d.frm$m, d.frm$x), ], 
                  DoBy(char, m, 
                       FUN = function(x) c(NA, x[-1] != head(x, -1))*1L,
                       vnames = "chg")[, -c(1:2)]
             ) 

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar}

