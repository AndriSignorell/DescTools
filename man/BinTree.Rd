\name{BinTree}
\alias{BinTree}
\alias{PlotBinTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Binary Tree
%%  ~~function to do ... ~~
}
\description{Create a binary tree of a given number of nodes \code{n}. Can be used to organize a sorted numeric vector as  a binary tree.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BinTree(n)

PlotBinTree(x, main="Binary tree", horiz=FALSE, cex=1.0, col=1, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{integer, size of the tree
%%     ~~Describe \code{n} here~~
}
  \item{x}{numeric vector to be organized as binary tree.
%%     ~~Describe \code{n} here~~
}
  \item{main}{main title of the plot.
%%     ~~Describe \code{main} here~~
}
\item{horiz}{logical, should the plot be oriented horizontally or vertically. The latter is default.}
\item{cex}{character extension factor for the labels.
%%     ~~Describe \code{n} here~~
}
  \item{col}{color of the linesegments of the plot.
%%     ~~Describe \code{n} here~~
}
  \item{\dots}{the dots are sent to \code{\link{Canvas}}.
%%     ~~Describe \code{n} here~~
}
}
\details{
If we index the nodes of the tree as 1 for the top, 2--3 for the next
horizontal row, 4--7 for the next, \ldots then the parent-child
traversal becomes particularly easy.
The basic idea is that the rows of the tree start at indices 1, 2, 4, \ldots.

BinTree(13) yields the vector
\code{c(8, 4, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 7)} meaning that the smallest element
will be in position 8 of the tree, the next smallest in position 4, etc.

%%  ~~ If necessary, more details than the description above ~~
}
\value{an integer vector of length n
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Terry Therneau <therneau.terry@mayo.edu>\cr
Andri Signorell <andri@signorell.net> (plot)
%%  ~~who you are~~
}

\examples{
BinTree(12)

x <- sort(sample(100, 24))
z <- PlotBinTree(x, cex=0.8)


# Plot example - Titanic data, for once from a somwhat different perspective
tab <- apply(Titanic, c(2,3,4), sum)
cprob <- c(1, prop.table(apply(tab, 1, sum))
           , as.vector(aperm(prop.table(apply(tab, c(1,2), sum), 1), c(2, 1)))
           , as.vector(aperm(prop.table(tab, c(1,2)), c(3,2,1)))
)

PlotBinTree(round(cprob[BinTree(length(cprob))],2), horiz=TRUE, cex=0.8,
            main="Titanic")
text(c("sex","age","survived"), y=0, x=c(1,2,3)+1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ list }
