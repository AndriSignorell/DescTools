\name{Conf}
\alias{Conf}
\alias{Conf.table}
\alias{Conf.default}
\alias{Conf.matrix}
\alias{Conf.rpart}
\alias{Conf.multinom}
\alias{Conf.glm}
\alias{Conf.randomForest}
\alias{Conf.svm}
\alias{Conf.regr}

\alias{plot.Conf}
\alias{print.Conf}
\alias{Sens}
\alias{Spec}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Confusion Matrix And Associated Statistics
%%  ~~function to do ... ~~
}
\description{Calculates a cross-tabulation of observed and predicted classes with associated statistics.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Conf(x, ...)

\method{Conf}{table}(x, pos = NULL, ...)
\method{Conf}{matrix}(x, pos = NULL, ...)
\method{Conf}{default}(x, ref, pos = NULL, na.rm = TRUE, ...)

\method{Conf}{rpart}(x, ...)
\method{Conf}{multinom}(x, ...)
\method{Conf}{glm}(x, cutoff = 0.5, ...)
\method{Conf}{randomForest}(x, ...)
\method{Conf}{svm}(x, ...)
\method{Conf}{regr}(x, ...)

\method{plot}{Conf}(x, main = "Confusion Matrix", ...)

\method{print}{Conf}(x, digits = max(3, getOption("digits") - 3), ...)

Sens(x, ...)
Spec(x, ...)

}
%- maybe also 'usage' for other objects documented here.
% \item{cutoff}{the cutoff point for separating the classes. This is only used if x is a glm-model and ignored else. }
\arguments{
\item{x}{a vector, normally a factor, of predicted classes or an object of following classes \code{rpart}, \code{randomForest}, \code{svm}, \code{C50}, \code{glm}, \code{multinom}, \code{reg}r, \code{ld}a, \code{qda} or \code{\link{table}}, resp. \code{matrix}. When a model is given, the predicted classes will be determined. A table or a matrix will be interpreted as a confusion matrix.
  }
\item{ref}{a vector, normally a factor, of classes to be used as the reference. This is ignored if \code{x} is a \code{table} or \code{matrix}.}
\item{pos}{a character string that defines the factor level corresponding to the "positive" results. Will be ignored for a \eqn{n \times n}{n x n} table n > 2.}

\item{cutoff}{used in logit models. The cutoff for changing classes.}
  \item{main}{overall title for the plot. }

  \item{digits}{controls the number of digits to print.
}

  \item{na.rm}{a logical value indicating whether or not missing values should be removed. Defaults to \code{FALSE}.
}


  \item{\dots}{further arguments to be passed to or from methods.}

  }
\details{
The functions require the factors to have the same levels.

For two class problems, the sensitivity, specificity, positive
predictive value and negative predictive value is calculated using the
\code{positive} argument. Also, the prevalence of the "event" is computed from the
data (unless passed in as an argument), the detection rate (the rate of true events also
predicted to be events) and the detection prevalence (the prevalence of predicted events).

Suppose a \eqn{2 \times 2}{2 x 2} table with notation

 \tabular{rcc}{
                    \tab Reference \tab          \cr
         Predicted  \tab Event     \tab No Event \cr
         Event      \tab A         \tab B        \cr
         No Event   \tab C         \tab D        \cr
       }

The formulas used here are:
\deqn{Sensitivity = A/(A+C)}
\deqn{Specificity = D/(B+D)}
\deqn{Prevalence = (A+C)/(A+B+C+D)}
\deqn{PPV = (sensitivity * Prevalence)/((sensitivity*Prevalence) + ((1-specificity)*(1-Prevalence)))}
\deqn{NPV = (specificity * (1-Prevalence))/(((1-sensitivity)*Prevalence) + ((specificity)*(1-Prevalence)))}
\deqn{Detection Rate =  A/(A+B+C+D)}
\deqn{Detection Prevalence =  (A+B)/(A+B+C+D)}
\deqn{F-val Accuracy =  2 / (1/PPV + 1/Sensitivity)}
\deqn{Matthews Cor.-Coef = (A*D-B*C)/sqrt((A+B)*(A+C)*(D+B)*(D+C)) }

See the references for discusions of the first five formulas.

For more than two classes, these results are
calculated comparing each factor level to the remaining levels
(i.e. a "one versus all" approach).

The overall accuracy and unweighted Kappa statistic are calculated. A p-value from McNemar's test is also computed using \code{\link[stats]{mcnemar.test}} (which can produce \code{NA} values with sparse tables).

The overall accuracy rate is computed along with a 95 percent confidence interval for this rate (using \code{\link[DescTools]{BinomCI}}) and a one-sided test to see if the accuracy is better than the "no information rate," which is taken to be the largest class percentage in the data.

 The sensitivity is defined as the proportion of positive results out of the number of
 samples which were actually positive. When there are no positive results, sensitivity is
 not defined and a value of \code{NA} is returned. Similarly, when there are no negative
 results, specificity is not defined and a value of \code{NA} is returned. Similar
 statements are true for predictive values.

Confidence intervals for sensitivity, specificity etc. could be calculated as binomial confidence intervals (see \code{\link{BinomCI}}). \code{BinomCI(A, A+C)} yields the ci for sensitivity.
}
\value{
a list with elements

  \item{table}{the results of \code{table} on \code{data} and  \code{reference}}
  \item{positive}{the positive result level}
  \item{overall}{a numeric vector with overall accuracy and Kappa statistic values}
  \item{byClass}{the sensitivity, specificity, positive predictive value, negative predictive value, prevalence, dection rate and detection prevalence for each class. For two class systems, this is calculated once using the \code{positive} argument}
}
\references{
Kuhn, M. (2008) Building predictive models in R using the caret package \emph{Journal of Statistical Software}, (\url{http://www.jstatsoft.org/v28/i05/}).

Powers, David M W (2011) Evaluation: From Precision, Recall and F-Measure to ROC, Informedness, Markedness & Correlation (PDF). \emph{Journal of Machine Learning Technologies} 2 (1): 37-63.

Collett D (1999) Modelling Binary Data. \emph{Chapman & Hall/CRC}, Boca Raton Florida, pp. 24.

Matthews, B. W. (1975) Comparison of the predicted and observed secondary structure of T4 phage lysozyme. \emph{Biochimica et Biophysica Acta (BBA) - Protein Structure} 405 (2): 442-451. doi:10.1016/0005-2795(75)90109-9. PMID 1180967.

}

\author{
Andri Signorell <andri@signorell.net> \cr
rewritten based on the ideas of \code{\link[caret]{confusionMatrix}} by Max Kuhn <Max.Kuhn@pfizer.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{OddsRatio}}, \code{\link{RelRisk}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# let tab be a confusion table
tab <- TextToTable("
   lo hi
lo 23 13
hi 10 18 ", dimnames=c("pred", "obs"))

Conf(tab, pos="hi")


pred <- Untable(tab)[,"pred"]
obs <- Untable(tab)[,"obs"]

Conf(x = pred, ref = obs)
Conf(x = pred, ref = obs, pos="hi")

Sens(tab)   # Sensitivity
Spec(tab)   # Specificity


tab <- TextToTable("
      terrible poor marginal clear
terrible       10    4        1     0
poor            5   10       12     2
marginal        2    4       12     5
clear           0    2        6    13
", dimnames=c("pred", "obs"))

Conf(tab)
}

\keyword{multivar}
