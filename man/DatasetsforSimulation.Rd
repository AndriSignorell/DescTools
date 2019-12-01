\name{Datasets for Simulation}
\alias{cards}
\alias{tarot}
\alias{roulette}
\docType{data}

\title{Datasets for Probabilistic Simulation
}

\description{For performing elementary probability calculations in introductory statistic courses, we might want to simulate random games.
The dataset \code{roulette} contains the standard sample space for one spin on a roulette wheel.
\code{cards} contains the standard set of 52 playing cards in four colours (without Jokers). \code{tarot} does the same with a classic tarot deck.
}

\usage{
cards
tarot
roulette
}
\value{
\code{cards} is a \code{data.frame} with three columns named \code{card}, \code{rank} and \code{suit}

\code{tarot} is a \code{data.frame} with four columns named \code{card}, \code{rank}, \code{suit} and \code{desc}

\code{roulette} is a \code{data.frame} with seven columns named \code{num} and \code{col}, \code{parity}, \code{highlow}, \code{dozens}, \code{column}, \code{pocketrange}
}

\seealso{\code{\link{Sample}}, \code{\link{sample}()}}

\examples{
head(cards)
head(tarot)
head(roulette)

# drawing 5 cards
sample(cards$card, 5)

# drawing 5 cards with jokers
sample(c(cards$card, rep("Joker", 3)), 5)

# spin the wheel by using the DescTools::Sample() for sampling
# rows from a data frame
Sample(roulette, size=1)

# simulate the evening in Las Vegas with 10 games
Sample(roulette, 10, replace=TRUE)
}

\keyword{datasets}
