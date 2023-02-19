\name{BarnardTest}
\alias{BarnardTest}
\title{ Barnard's Unconditional Test}
\description{
Barnard's unconditional test for superiority applied to \eqn{2 \times 2}{2x2} contingency tables
using Score or Wald statistics for the difference between two binomial proportions.
}
\usage{
BarnardTest(x, y = NULL, alternative = c("two.sided", "less", "greater"),
            method = c("csm", "csm approximate", "z-pooled", "z-unpooled",
                       "boschloo", "santner and snell"), 
            fixed = 1, useStoredCSM = FALSE, ...)
}
\arguments{

 \item{x}{a numeric vector or a two-dimensional contingency table in matrix form. \code{x} and \code{y} can also both be factors. }
 \item{y}{a factor object; ignored if \code{x} is a matrix.}

  \item{alternative}{a character string specifying the alternative
    hypothesis, must be one of \code{"two.sided"} (default),
    \code{"greater"} or \code{"less"}.  You can specify just the initial
    letter.}

  \item{method}{Indicates the method for finding the more extreme tables: must be either \code{"Zpooled"}, \code{"Z-unpooled"}, \code{"Santner and Snell"}, \code{"Boschloo"}, \code{"CSM"}, or \code{"CSM approximate"}. CSM tests cannot be calculated for multinomial models.}

  \item{fixed}{indicates which margins are fixed. \code{1} stands for row, \code{2} for columns, \code{NA} for none of both. }

\item{useStoredCSM}{logical, use a stored ordering matrix for the CSM test to greatly reduce the computation time (default is \code{FALSE}).}

\item{\dots}{the dots are passed on to the \code{Exact::exact.test()} function.}

}


\value{
   A list with class \code{"htest"} containing the following components:
   \item{p.value}{the p-value of the test.}
   \item{estimate}{an estimate of the nuisance parameter where the p-value is maximized.}
   \item{alternative}{a character string describing the alternative
     hypothesis.}
   \item{method}{the character string
     \code{"Barnards Unconditional 2x2-test"}.}
   \item{data.name}{a character string giving the names of the data.}
  \item{statistic.table }{The contingency tables considered in the analysis represented by \code{n1} and \code{n2}, their scores, and whether they are included in the one-sided (\code{1}), two-sided (\code{2}) tests, or not included at all (\code{0})}
   \item{nuisance.matrix }{Nuisance parameters, \code{p}, and the corresponding p-values for both one- and two-sided tests}

 }

\details{
There are two fundamentally different exact tests for comparing the equality of two binomial probabilities - Fisher's exact test (Fisher, 1925), and Barnard's exact test (Barnard, 1945). Fisher's exact test (Fisher, 1925) is the more popular of the two. In fact, Fisher was bitterly critical of Barnard's proposal for esoteric reasons that we will not go into here. For 2 x 2 tables, Barnard's test is more powerful than Fisher's, as Barnard noted in his 1945 paper, much to Fisher's chagrin. Anyway, perhaps due to its computational difficulty the Barnard's is not widely used. (Mehta et.al., 2003)

Unconditional exact tests can be performed for binomial or multinomial models. The binomial model assumes the row or column margins (but not both) are known in advance, while the multinomial model assumes only the total sample size is known beforehand. 
For the binomial model, the user needs to specify which margin is fixed (default is rows). Conditional tests (e.g., Fisher's exact test) have both row and column margins fixed, but this is a very uncommon design. (See Calhoun (2019) for more details.)

If \code{x} is a matrix, it is taken as a two-dimensional contingency
   table, and hence its entries should be nonnegative integers.
   Otherwise, both \code{x} and \code{y} must be vectors of the same
   length.  Incomplete cases are removed, the vectors are coerced into
   factor objects, and the contingency table is computed from these.

For a 2x2 contingency table, such as \eqn{X=[n_1,n_2;n_3,n_4]}, the normalized difference in proportions between the two categories, given in each column, can be written with pooled variance (Score statistic) as
\deqn{T(X)=\frac{\hat{p}_2-\hat{p}_1}{\sqrt{\hat{p}(1-\hat{p})(\frac{1}{c_1}+\frac{1}{c_2})}},}
where \eqn{\hat{p}=(n_1+n_3)/(n_1+n_2+n_3+n_4)}, \eqn{\hat{p}_2=n_2/(n_2+n_4)}, \eqn{\hat{p}_1=n_1/(n_1+n_3)}, \eqn{c_1=n_1+n_3} and \eqn{c_2=n_2+n_4}. Alternatively, with unpooled variance (Wald statistic), the difference in proportions can we written as
\deqn{T(X)=\frac{\hat{p}_2-\hat{p}_1}{\sqrt{\frac{\hat{p}_1(1-\hat{p}_1)}{c_1}+\frac{\hat{p}_2(1-\hat{p}_2)}{c_2}}}.}
The probability of observing \eqn{X} is
\deqn{P(X)=\frac{c_1!c_2!}{n_1!n_2!n_3!n_4!}p^{n_1+n_2}(1-p)^{n_3+n_4},}
where \eqn{p} is the unknown nuisance parameter.

Barnard's test considers all tables with category sizes \eqn{c_1} and \eqn{c_2} for a given \eqn{p}. The p-value is the sum of probabilities of the tables having a score in the rejection region, e.g. having significantly large difference in proportions for a two-sided test. The p-value of the test is the maximum p-value calculated over all \eqn{p} between 0 and 1.

If \code{useStoredCSM} is set to \code{TRUE} a companion data package called \pkg{ExactData} must be installed from GitHub.  

The author states:
\emph{"The CSM test is computationally intensive due to iteratively maximizing the p-value calculation to order the tables. The CSM ordering matrix has been stored for all possible sample sizes less than or equal to 100 (i.e., max(n1,n2)<=100). Thus, using the useStoredCSM = TRUE can greatly improve computation time. However, the stored ordering matrix was computed with npNumbers=100 and it is possible that the ordering matrix was not optimal for larger npNumbers. Increasing npNumbers and setting useStoredCSM = FALSE ensures the p-value is correctly calculated at the expense of
significantly greater computation time. The stored ordering matrix is not used in the calculation of confidence intervals or non-inferiority tests, so CSM can still be very computationally intensive."}
}
\references{
Barnard, G.A. (1945) A new test for 2x2 tables. \emph{Nature}, 156:177.

Barnard, G.A. (1947) Significance tests for 2x2 tables. \emph{Biometrika}, 34:123-138.

Suissa, S. and Shuster, J. J. (1985), Exact Unconditional Sample Sizes for the 2x2 Binomial Trial, \emph{Journal of the Royal Statistical Society}, Ser. A, 148, 317-327.

Cardillo G. (2009) MyBarnard: a very compact routine for Barnard's exact test on 2x2 matrix. \url{https://ch.mathworks.com/matlabcentral/fileexchange/25760-mybarnard}

Galili T. (2010) \url{https://www.r-statistics.com/2010/02/barnards-exact-test-a-powerful-alternative-for-fishers-exact-test-implemented-in-r/}

Lin C.Y., Yang M.C. (2009) Improved p-value tests for comparing two independent binomial proportions. \emph{Communications in Statistics-Simulation and Computation}, 38(1):78-91.

Trujillo-Ortiz, A., R. Hernandez-Walls, A. Castro-Perez, L. Rodriguez-Cardozo N.A. Ramos-Delgado and R. Garcia-Sanchez. (2004). Barnardextest:Barnard's Exact Probability Test. A MATLAB file. [WWW document]. \url{https://www.mathworks.com/}

Mehta, C.R., Senchaudhuri, P. (2003) Conditional versus unconditional exact tests for comparing two binomials. \url{https://www.researchgate.net/publication/242179503_Conditional_versus_Unconditional_Exact_Tests_for_Comparing_Two_Binomials}
     
Calhoun, P. (2019) Exact: Unconditional Exact Test. R package version
  2.0. \cr \url{https://CRAN.R-project.org/package=Exact}
}
\author{
Peter Calhoun <calhoun.peter@gmail.com>, Andri Signorell <andri@signorell.net> (interface)
}
\seealso{\code{\link{fisher.test}}}

\examples{
tab <- as.table(matrix(c(8, 14, 1, 3), nrow=2,
                dimnames=list(treat=c("I","II"), out=c("I","II"))))
BarnardTest(tab)

# Plotting the search for the nuisance parameter for a one-sided test
bt <- BarnardTest(tab)

# Plotting the tables included in the p-value
ttab <- as.table(matrix(c(40, 14, 10, 30), nrow=2,
                 dimnames=list(treat=c("I","II"), out=c("I","II"))))

\donttest{
bt <- BarnardTest(ttab)
bts <- bt$statistic.table
}

# Mehta et. al (2003)
tab <- as.table(matrix(c(7, 12, 8, 3), nrow=2,
                       dimnames=list(treat=c("vaccine","placebo"),
                                     infection=c("yes","no"))))
BarnardTest(tab, alternative="less")
}

\keyword{nonparametric}
\keyword{htest}

