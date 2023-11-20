\name{PlotFaces}
\alias{PlotFaces}
\title{    Chernoff Faces    }
\description{
     Plot Chernoff faces. The rows of a data matrix represent cases and the columns the variables.
     }
\usage{
PlotFaces(xy, which.row, fill = FALSE, nr, nc,
          scale = TRUE, byrow = FALSE, main, labels, col = "white")
}
\arguments{
  \item{xy}{   \code{xy} data matrix, rows represent individuals and columns attributes.  }
  \item{which.row}{   defines a permutation of the rows of the input matrix.   }
  \item{fill}{  logic. If set to \code{TRUE}, only the first \code{nc} attributes of the faces are
                  transformed, \code{nc} is the number of columns of \code{x}.   }
  \item{nr}{   number of columns of faces on graphics device   }
  \item{nc}{   number of rows of faces   }
  \item{scale}{ logic. If set to \code{TRUE}, attributes will be normalized.   }
  \item{byrow}{   \code{if(byrow==TRUE)}, \code{x} will be transposed.   }
  \item{main}{   title.   }
  \item{labels}{   character strings to use as names for the faces.   }
  \item{col}{a vector of colors used for the parts of the faces. Colors are recycled in the order: "nose", "eyes", "hair", "face", "lips", "ears". Default is NA, which will omit colors.}
}
\details{The features paramters of this implementation are:
\itemize{
  \item{1 } height of face
  \item{2 } width of face
  \item{3 } shape of face
  \item{4 } height of mouth
  \item{5 } width of mouth
  \item{6 } curve of smile
  \item{7 } height of eyes
  \item{8 } width of eyes
  \item{9 } height of hair
  \item{10 } width of hair
  \item{11 } styling of hair
  \item{12 } height of nose
  \item{13 } width of nose
  \item{14 } width of ears
  \item{15 } height of ears
}

\figure{faces.png}{Some faces}

For details look at the literate program of \code{faces}
}
\value{
  information about usage of variables for face elements is returned invisibly
}

\references{  Chernoff, H. (1973) The use of faces to represent statistiscal assoziation,
\emph{JASA}, 68, pp 361--368.\cr

The smooth curves are computed by an algorithm found in:\cr
Ralston, A. and Rabinowitz, P. (1985)
\emph{A first course in numerical analysis}, McGraw-Hill, pp 76ff.}
\author{   H. P. Wolf, some changes Andri Signorell <andri@signorell.net>   }
\note{ based on version 12/2009 }

\examples{
PlotFaces(rbind(1:3,5:3,3:5,5:7))

data(longley)
PlotFaces(longley[1:9,])

set.seed(17)
PlotFaces(matrix(sample(1:1000,128,), 16, 8), main="random faces")


means <- lapply(iris[,-5], tapply, iris$Species, mean)
m <- t(do.call(rbind, means))
m <- cbind(m, matrix(rep(1, 11*3), nrow=3))

# define the colors, first for all faces the same
col <- replicate(3, c("orchid1", "olivedrab", "goldenrod4",
                      "peachpuff", "darksalmon", "peachpuff3"))
rownames(col) <- c("nose","eyes","hair","face","lips","ears")
# change haircolor individually for each face
col[3, ] <- c("lightgoldenrod", "coral3", "sienna4")

z <- PlotFaces(m, nr=1, nc=3, col=col)

# print the used coding
print(z$info, right=FALSE)
}

\keyword{ hplot }
