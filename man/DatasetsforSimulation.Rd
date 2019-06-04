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
\code{cards} is a \code{data.frame} with two columns named \code{rank} and \code{suit}

\code{tarot} is a \code{data.frame} with three columns named \code{rank}, \code{suit} and \code{desc}

\code{roulette} is a \code{data.frame} with two columns named \code{num} and \code{col}
}

\examples{
head(cards)
head(tarot)
head(roulette)

# drawing 5 cards
sample(do.call(paste, cards), 5)

# spin the wheel
sample(do.call(paste, roulette), 1)

# simulate the evening in Las Vegas
sample(do.call(paste, roulette), 32, replace=TRUE)
}

\keyword{datasets}
