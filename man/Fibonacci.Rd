\name{Fibonacci}
\alias{Fibonacci}
\title{
  Fibonacci Numbers
}
\description{
  Generates Fibonacci numbers.
}
\usage{
Fibonacci(n)
}
\arguments{
  \item{n}{nonnegative integer or vector of nonnegative integers.}
}
\details{
  Generates the \code{n}-th Fibonacci number, whereas Fibonacci(0) = 0.
}
\references{\url{https://en.wikipedia.org/wiki/Fibonacci_number}}
\value{
  A single integer, or a vector of integers.
}

\author{Andri Signorell <andri@signorell.net>}

\examples{
Fibonacci(0)                            # 1
Fibonacci(2)                            # 2
Fibonacci(0:3)                          # 0 1 1 2


# Golden ratio
F <- Fibonacci(1:25)                    # ... 75025 121393
f25 <- F[25]/F[24]                      #     1.618033989
phi <- (sqrt(5) + 1)/2
abs(f25 - phi)                          # 7.945178e-11

# Fibonacci numbers without iteration
fibo <- function(n) {
  phi <- (sqrt(5) + 1)/2
  fib <- (phi^(n+1) - (1-phi)^(n+1)) / (2*phi - 1)
  round(fib)
}
  
fibo(30:33)                             # 1346269 2178309 3524578 5702887

}
