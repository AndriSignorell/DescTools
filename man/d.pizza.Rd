\name{d.pizza}
\alias{d.pizza}
\docType{data}
\title{Data pizza
%%   ~~ data name/kind ... ~~
}
\description{An artificial dataset inspired by a similar dataset pizza.sav in \emph{Arbeitsbuch zur deskriptiven und induktiven Statistik} by Toutenburg et.al.\cr
The dataset contains data of a pizza delivery service in London, delivering pizzas to three areas. Every record defines one order/delivery and the according properties. A pizza is supposed to taste good, if its temperature is high enough, say 45 Celsius. So it might be interesting for the pizza delivery service to minimize the delivery time.\cr
The dataset is designed to be possibly evil. It contains the most used datatypes as numerics, factors, ordered factors, integers, logicals and a date. NAs are scattered everywhere partly systematically, partly randomly (except in the index).
}
\usage{data(d.pizza)}
\format{
  A data frame with 1209 observations on the following 17 variables.
  \describe{
    \item{\code{index}}{a numeric vector, indexing the records (no missings here).}
    \item{\code{date}}{Date, the delivery date}
    \item{\code{week}}{integer, the weeknumber}
    \item{\code{weekday}}{integer, the weekday}
    \item{\code{area}}{factor, the three London districts: \code{Brent}, \code{Camden}, \code{Westminster} }
    \item{\code{count}}{integer, the number of pizzas delivered}
    \item{\code{rabate}}{logical, \code{TRUE} if a rabate has been given }
    \item{\code{price}}{numeric, the total price of delivered pizza(s) }
    \item{\code{operator}}{a factor with levels \code{Allanah} \code{Maria} \code{Rhonda}}
    \item{\code{driver}}{a factor with levels \code{Carpenter} \code{Carter} \code{Taylor} \code{Butcher} \code{Hunter} \code{Miller} \code{Farmer}}
    \item{\code{delivery_min}}{numeric, the delivery time in minutes (decimal)}
    \item{\code{temperature}}{numeric, the temperature of the pizza in degrees Celsius when delivered to the customer }
    \item{\code{wine_ordered}}{integer, 1 if wine was ordered, 0 if not}
    \item{\code{wine_delivered}}{integer, 1 if wine was delivered, 0 if not}
    \item{\code{wrongpizza}}{logical, \code{TRUE} if a wrong pizza was delivered }
    \item{\code{quality}}{ordered factor with levels \code{low} < \code{medium} < \code{high}, defining the quality of the pizza when delivered}
  }
}
\details{The dataset contains NAs randomly scattered.
%%  ~~ If necessary, more details than the __description__ above ~~
}
\references{Toutenburg H, Schomaker M, Wissmann M, Heumann C (2009): \emph{Arbeitsbuch zur deskriptiven und induktiven Statistik} Springer, Berlin Heidelberg
}
\examples{
str(d.pizza)
head(d.pizza)

Desc(d.pizza)
}
\keyword{datasets}
