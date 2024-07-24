\name{BrierScore}
\alias{BrierScore}
\alias{BrierScore.glm}
\alias{BrierScore.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Brier Score for Assessing Prediction Accuracy
%%  ~~function to do ... ~~
}
\description{Calculate Brier score for assessing the quality of the probabilistic predictions of binary events.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BrierScore(x, pred = NULL, scaled = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{either a model object if pred is not supplied or the response variable if it is.
%%     ~~Describe \code{x} here~~
}
  \item{pred}{the predicted values
%%     ~~Describe \code{pred} here~~
}
  \item{scaled}{logical, defining if scaled or not. Default is FALSE.
%%     ~~Describe \code{scaled} here~~
}
   \item{\dots}{further arguments to be passed to other functions. }
}
\details{The Brier score is a proper score function that measures the accuracy of probabilistic predictions. It is applicable to tasks in which predictions must assign probabilities to a set of mutually exclusive discrete outcomes. The set of possible outcomes can be either binary or categorical in nature, and the probabilities assigned to this set of outcomes must sum to one (where each individual probability is in the range of 0 to 1).

It's calculated as
\deqn{\frac{1}{n} \cdot \sum_{i=1}^{n}\left ( p_{i}-o_{i} \right )^2  \; \; \; \textup{where} \;
p_{i} predicted probability \; \textup{and} \; o_{i} observed value out of (0,1)}{1/n sum(p_i - o_i)^2, where p_i...predicted probability and o_i...observed value out of (0,1)}

The lower the Brier score is for a set of predictions, the better the predictions are calibrated. Note that the Brier score, in its most common formulation, takes on a value between zero and one, since this is the largest possible difference between a predicted probability (which must be between zero and one) and the actual outcome (which can take on values of only 0 and 1).
(In the original (1950) formulation of the Brier score, the range is double, from zero to two.)
%%  ~~ If necessary, more details than the description above ~~
}
\value{a numeric value
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Brier, G. W. (1950) Verification of forecasts expressed in terms of probability. \emph{Monthly Weather Review, 78}, 1-3.
%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link[DescTools]{Conf}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
r.glm <- glm(Survived ~ ., data=Untable(Titanic), family=binomial)

BrierScore(r.glm)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
