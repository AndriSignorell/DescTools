\encoding{latin1}
\name{DivCoefMax}
\alias{DivCoefMax}
\title{Maximal value of Rao's diversity coefficient also called 
quadratic entropy}
\description{
For a given dissimilarity matrix, this function calculates the 
maximal value of Rao's diversity coefficient over all frequency 
distribution. It uses an optimization technique based on Rosen's 
projection gradient algorithm and is verified using the 
Kuhn-Tucker conditions.
}
\usage{
DivCoefMax(dis, epsilon, comment)
}
\arguments{
  \item{dis}{an object of class \code{dist} containing distances 
  or dissimilarities among elements.}
  \item{epsilon}{a tolerance threshold : a frequency is non null 
  if it is higher than epsilon.}
  \item{comment}{a logical value indicating whether or not 
  comments on the optimization technique should be printed.}
}
\value{
Returns a list 
  \item{value}{the maximal value of Rao's diversity coefficient.}
  \item{vectors}{a data frame containing four frequency 
  distributions : sim is a simple distribution which is equal 
  to \eqn{\frac{D1}{1^tD1}}{D1/1^tD1}, pro is equal to 
  \eqn{\frac{z}{1^tz1}}{z/1^tz1}, where z is the nonnegative 
  eigenvector of the matrix containing the squared dissimilarities 
  among the elements, met is equal to \eqn{z^2}{z^2}, num is a frequency 
  vector maximizing Rao's diversity coefficient.}
}
\references{
Rao, C.R. (1982) Diversity and dissimilarity coefficients: 
a unified approach. \emph{Theoretical Population Biology}, 
\bold{21}, 24--43.

Gini, C. (1912) Variabilita e mutabilita. 
\emph{Universite di Cagliari III}, Parte II.

Simpson, E.H. (1949) Measurement of diversity. 
\emph{Nature}, \bold{163}, 688.

Champely, S. and Chessel, D. (2002) Measuring biological diversity 
using Euclidean metrics. \emph{Environmental and Ecological Statistics}, 
\bold{9}, 167--177.

Pavoine, S., Ollier, S. and Pontier, D. (2005) 
Measuring diversity from dissimilarities with Rao's quadratic entropy: 
are any dissimilarities suitable? \emph{Theoretical Population Biology},
\bold{67}, 231--239.

}
\author{
Stéphane Champely <Stephane.Champely@univ-lyon1.fr> \cr
Sandrine Pavoine <pavoine@biomserv.univ-lyon1.fr>
}
\examples{
\dontrun{
par.safe <- par()$mar
data(elec88)
par(mar = c(0.1, 0.1, 0.1, 0.1))
# Departments of France.
area.plot(elec88$area)

# Dissimilarity matrix.
d0 <- dist(elec88$xy)

# Frequency distribution maximizing spatial diversity in France
# according to Rao's quadratic entropy.
France.m <- DivCoefMax(d0)
w0 <- France.m$vectors$num
v0 <- France.m$value
(1:94) [w0 > 0]

# Smallest circle including all the 94 departments.
# The squared radius of that circle is the maximal value of the
# spatial diversity.
w1 = elec88$xy[c(6, 28, 66), ]
w.c = apply(w1 * w0[c(6, 28, 66)], 2, sum)
symbols(w.c[1], w.c[2], circles = sqrt(v0), inc = FALSE, add = TRUE)
s.value(elec88$xy, w0, add.plot = TRUE)
par(mar = par.safe)


# Maximisation of Rao's diversity coefficient
# with ultrametric dissimilarities.
data(microsatt)
mic.genet <- count2genet(microsatt$tab)
mic.dist <- dist.genet(mic.genet, 1)
mic.phylog <- hclust2phylog(hclust(mic.dist))
plot.phylog(mic.phylog)
mic.maxpond <- DivCoefMax(mic.phylog$Wdist)$vectors$num
dotchart.phylog(mic.phylog, mic.maxpond)
}
}
\keyword{multivariate}
