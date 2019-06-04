\name{Logit}
\alias{Logit}
\alias{LogitInv}
\title{Generalized Logit and Inverse Logit Function}
\description{
  Compute generalized logit and generalized inverse logit functions.
}
\usage{
Logit(x, min = 0, max = 1)
LogitInv(x, min = 0, max = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{value(s) to be transformed}
  \item{min}{lower end of logit interval}
  \item{max}{upper end of logit interval}
}
\details{

  The generalized logit function takes values on [min, max] and
  transforms them to span \eqn{[-\infty, \infty ]}{[-Inf, Inf]}. \cr It is defined as:

    \deqn{y = log\left (\frac{p}{1-p} \right ) \;\;\;  \; \textup{where} \; \;\;  p=\frac{x-min}{max-min}}{y = log(p/(1-p)) where p=(x-min)/(max-min)}


%s    \deqn{y = log(\frac{p}{(1-p)})}{y = log(p/(1-p))}
%s
%s  where
%s
%s    \deqn{p=\frac{(x-min)}{(max-min)}}{p=(x-min)/(max-min)}

  The generalized inverse logit function provides the inverse
  transformation:

    \deqn{x = p' \cdot (max-min) + min  \;\;\;  \; \textup{where} \; \;\;  p'=\frac{exp(y)}{1+exp(y)}}{x = p' * (max-min) + min   where  p' = exp(y)/(1+exp(y))}



}
\value{
  Transformed value(s).
}
\author{ Gregory R. Warnes \email{greg@warnes.net} }

\seealso{ \code{\link[car]{logit}}}
\examples{

x <- seq(0,10, by=0.25)
xt <- Logit(x, min=0, max=10)
cbind(x,xt)

y <- LogitInv(xt, min=0, max=10)
cbind(x, xt, y)

}
\keyword{math}
