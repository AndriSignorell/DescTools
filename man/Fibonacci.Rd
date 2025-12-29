\name{Fibonacci}
\alias{Fibonacci}
\alias{phi}
\title{
  Fibonacci Numbers
}
\description{
  Generate Fibonacci numbers. The Fibonacci numbers can also be calculated using the golden ratio 
  \code{phi}, as demonstrated in the examples.
}
\usage{
Fibonacci(n)
phi
}
\arguments{
  \item{n}{nonnegative integer or vector of nonnegative integers.}
}
\details{
  Generates the \code{n}-th Fibonacci number, whereas \code{Fibonacci(0) = 0}.
  \cr
  The golden ratio is defined as \code{phi = 0.5*(1+sqrt(5))}.
}
\references{\url{https://en.wikipedia.org/wiki/Fibonacci_number}\cr
\url{https://mathworld.wolfram.com/GoldenRatio.html}}
\value{
  A single integer, or a vector of integers.
}

\author{Andri Signorell <andri@signorell.net>}

\examples{
Fibonacci(0)                            # 1
Fibonacci(2)                            # 2
Fibonacci(0:3)                          # 0 1 1 2
Fibonacci(0:25)                         # ... 75025 121393

# Golden ratio = Fib(25)/ Fib(24)
f25 <- Quot(Fibonacci(24:25))           # 1.618033989
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
