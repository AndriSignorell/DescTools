\name{GenRandGroups}
\alias{GenRandGroups}
\title{Generate Random Groups
}
\description{Generates a random grouping from a given data vector, where the group sizes correspond to the numeric vector grp_n.
}
\usage{
GenRandGroups(x, grp_n)
}
\arguments{
  \item{x}{a vector containing the objects which should be grouped
}
  \item{grp_n}{an integer vector with the required group sizes
}
}
\details{For group divisions in class, it is often useful to have a function available that randomizes these divisions. 
}
\value{a list sized length of \code{grp_n} with the x elements assigned to their group.
}
\author{Andri Signorell <andri@signorell.net>
}
\seealso{\code{\link{CombN}}, \code{\link{CombSet}}
}
\examples{
# say we have 12 students and want 3 groups with sizes 4,3, and 5

GenRandGroups(x=LETTERS[1:12], grp_n=c(4,3,5))
}
\concept{ combinatorics }
