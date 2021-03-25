\name{Cstat}
\alias{Cstat}
\alias{Cstat.glm}
\alias{Cstat.default}

\title{C Statistic (Area Under the ROC Curve)
%%  ~~function to do ... ~~
}
\description{Calculate the C statistic, a measure of goodness of fit for binary outcomes in a logistic regression or any other classification model. The C statistic is equivalent to the area under the ROC-curve (Receiver Operating Characteristic).
}
\usage{
Cstat(x, ...)

\method{Cstat}{glm}(x, ...)

\method{Cstat}{default}(x, resp, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the logistic model for the glm interface or the predicted probabilities of the model for the default.
%%     ~~Describe \code{x} here~~
}
 \item{resp}{the response variable (coded as c(0, 1))
%%     ~~Describe \code{resp} here~~
}
   \item{\dots}{further arguments to be passed to other functions. }
}


\details{Values for this measure range from 0.5 to 1.0, with higher values indicating better predictive models. A value of 0.5 indicates that the model is no better than chance at making a prediction of membership in a group and a value of 1.0 indicates that the model perfectly identifies those within a group and those not. Models are typically considered reasonable when the C-statistic is higher than 0.7 and strong when C exceeds 0.8.

Confidence intervals for this measure can be calculated by bootstrap.
}
\value{numeric value
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\references{Hosmer D.W., Lemeshow S. (2000) Applied Logistic Regression (2nd Edition). New York, NY: \emph{John Wiley & Sons}
}

\seealso{\code{\link{BrierScore}}
}
\examples{
d.titanic = Untable(Titanic)
r.glm <- glm(Survived ~ ., data=d.titanic, family=binomial)
Cstat(r.glm)

# default interface
Cstat(x = predict(r.glm, method="response"), 
      resp = model.response(model.frame(r.glm)))
  
      
# calculating bootstrap confidence intervals
FUN <- function(d.set, i) {
   r.glm <- glm(Survived ~ ., data=d.set[i,], family=binomial)
   Cstat(r.glm)
   }
   
\dontrun{
library(boot)
boot.res <- boot(d.titanic, FUN, R=999) 

# the percentile confidence intervals
boot.ci(boot.res, type="perc")

## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 999 bootstrap replicates
##
## CALL : 
## boot.ci(boot.out = res, type = "perc")
## 
## Intervals : 
## Level     Percentile     
## 95\%   ( 0.7308,  0.7808 )  
## Calculations and Intervals on Original Scale
}   }
\keyword{mod}

