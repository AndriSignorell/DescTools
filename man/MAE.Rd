\name{Measures of Accuracy}
\alias{MAE}
\alias{MAE.default}
\alias{MAE.lm}
\alias{MAPE}
\alias{MSE}
\alias{RMSE}
\alias{MAPE.default}
\alias{MSE.default}
\alias{RMSE.default}
\alias{MAPE.lm}
\alias{MSE.lm}
\alias{RMSE.lm}
\alias{NMAE}
\alias{NMSE}
\alias{SMAPE}
\alias{SMAPE.default}
\alias{SMAPE.lm}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Measures of Accuracy
%%  ~~function to do ... ~~
}
\description{Some measures of model accuracy like mean absolute error (MAE), mean absolute percentage error (MAPE), symmetric mean absolute percentage error (SMAPE), mean squared error (MSE) and root mean squared error (RMSE).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MAE(x, ...)
\method{MAE}{default}(x, ref, na.rm = FALSE, ...)
\method{MAE}{lm}(x, ...)

MAPE(x, ...)
\method{MAPE}{default}(x, ref, na.rm = FALSE, ...)
\method{MAPE}{lm}(x, ...)

SMAPE(x, ...)
\method{SMAPE}{default}(x, ref, na.rm = FALSE, ...)
\method{SMAPE}{lm}(x, ...)

MSE(x, ...)
\method{MSE}{default}(x, ref, na.rm = FALSE, ...)
\method{MSE}{lm}(x, ...)

RMSE(x, ...)
\method{RMSE}{default}(x, ref, na.rm = FALSE, ...)
\method{RMSE}{lm}(x, ...)


NMAE(x, ref, train.y)
NMSE(x, ref, train.y)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the predicted values of a model or a model-object itself.
%%     ~~Describe \code{preds} here~~
}
  \item{ref}{the observed true values.
%%     ~~Describe \code{trues} here~~
}
  \item{train.y}{the observed true values in a train dataset.
%%     ~~Describe \code{trues} here~~
}
  \item{na.rm}{a logical value indicating whether or not missing values should be removed. Defaults to FALSE.

%%     ~~Describe \code{na.rm} here~~
}
\item{\dots}{further arguments}
}
\details{The function will remove \code{NA} values first (if requested).\cr
MAE calculates the mean absolute error:
\deqn{\frac{1}{n} \cdot \sum_{i=1}^{n}\left | ref_{i}-x_{i} \right |}{1/n sum( | ref_i-x_i | )}
\cr

MAPE calculates the mean absolute percentage error:
\deqn{\frac{1}{n} \cdot \sum_{i=1}^{n}\left | \frac{ref_{i}-x_{i}}{ref_{i}} \right |}{1/n sum( | (ref_i-x_i) / ref_i | )}
\cr

SMAPE calculates the symmetric mean absolute percentage error:
\deqn{\frac{1}{n} \cdot \sum_{i=1}^{n}\frac{2 \cdot \left | ref_{i}-x_{i} \right |}{\left | ref_{i} \right | + \left | x_{i} \right |}}{1/n sum(| ref_i-x_i | / (|ref_i| + |x_i|)) }
\cr

MSE calculates mean squared error:
\deqn{\frac{1}{n} \cdot \sum_{i=1}^{n}\left ( ref_{i}-x_{i} \right )^2}{1/n sum( (ref_i-x_i)^2  )}
\cr

RMSE calculates the root mean squared error:
\deqn{\sqrt{\frac{1}{n} \cdot \sum_{i=1}^{n}\left ( ref_{i}-x_{i} \right )^2}}{sqrt(1/n sum( (ref_i-x_i)^2))}
\cr
%%  ~~ If necessary, more details than the description above ~~
}
\value{the specific numeric value
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Armstrong, J. S. (1985) \emph{Long-range Forecasting: From Crystal Ball to Computer}, 2nd. ed. Wiley. ISBN 978-0-471-82260-8\cr
\url{http://en.wikipedia.org/wiki/Symmetric_mean_absolute_percentage_error}

Torgo, L. (2010) \emph{Data Mining with R: Learning with Case Studies}, Chapman and Hall/CRC Press
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{lm}}, \code{\link{resid}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
r.lm <- lm(Fertility ~ ., data=swiss)

MAE(r.lm)

# the same as:
MAE(predict(r.lm), swiss$Fertility)

MAPE(r.lm)
MSE(r.lm)
RMSE(r.lm)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ model }

