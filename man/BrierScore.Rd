\name{BrierScore}
\alias{BrierScore}
\alias{BrierScore.glm}
\alias{BrierScore.default}
\title{Brier Score for Assessing Prediction Accuracy
}
\description{Calculate Brier score for assessing the quality of the probabilistic predictions of binary events.
}
\usage{
BrierScore(x, pred = NULL, scaled = FALSE, ...)
}
\arguments{
  \item{x}{either a model object if pred is not supplied or the response variable if it is.
}
  \item{pred}{the predicted values
}
  \item{scaled}{logical, defining if scaled or not. Default is \code{FALSE}.
}
   \item{\dots}{further arguments to be passed to other functions. }
}
\details{The Brier score is a proper score function that measures the accuracy of probabilistic predictions. It is applicable to tasks in which predictions must assign probabilities to a set of mutually exclusive discrete outcomes. The set of possible outcomes can be either binary or categorical in nature, and the probabilities assigned to this set of outcomes must sum to one (where each individual probability is in the range of 0 to 1).

It's calculated as
\deqn{\frac{1}{n} \cdot \sum_{i=1}^{n}\left ( p_{i}-o_{i} \right )^2  \; \; \; \textup{where} \;
p_{i} \textup{ predicted probability} \; \textup{and} \; o_{i} \textup{ observed value out of (0,1)}}{1/n sum(p_i - o_i)^2, where p_i...predicted probability and o_i...observed value out of (0,1)}

The lower the Brier score is for a set of predictions, the better the predictions are calibrated. Note that the Brier score, in its most common formulation, takes on a value between zero and one, since this is the largest possible difference between a predicted probability (which must be between zero and one) and the actual outcome (which can take on values of only 0 and 1).
(In the original (1950) formulation of the Brier score, the range is double, from zero to two.)

The scaled Brier score is scaled by its maximum score under a 
non-informative model: 
\deqn{Brier_{scaled} = 1 - \frac{Brier}{Brier_{max}} \; \; \; \textup{where} \;
Brier_{max} = mean(p_i)*(1 – mean(p_i))}

 to let it range between 0\% and 100\%. This scaled Brier score happens to be very similar to Pearson’s \eqn{R^2} statistic.

}
\value{a numeric value}

\references{Brier, G. W. (1950) Verification of forecasts expressed in terms of probability. \emph{Monthly Weather Review, 78}, 1-3.

Hu B, Palta M, Shao J. (2006) Properties of R(2) statistics for logistic regression. \emph{Stat Med.} 2006;25(8):1383–95. doi: 10.1002/sim.2300. 
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link[DescTools]{Conf}}
}
\examples{
r.glm <- glm(Survived ~ ., data=Untable(Titanic), family=binomial)

BrierScore(r.glm)
}
\keyword{models}
