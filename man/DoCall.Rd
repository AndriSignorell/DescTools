\name{DoCall}
\alias{DoCall}
\title{Fast Alternative To The Internal \code{do.call}}
\usage{
DoCall(what, args, quote = FALSE, envir = parent.frame())
}
\arguments{
  \item{what}{either a function or a non-empty character
  string naming the function to be called.}

  \item{args}{a \emph{list} of arguments to the function
  call.  The \code{names} attribute of \code{args} gives
  the argument names.}

  \item{quote}{a logical value indicating whether to quote
  the arguments.}

  \item{envir}{an environment within which to evaluate the
  call.  This will be most useful if \code{what} is a
  character string and the arguments are symbols or quoted
  expressions.}
}
\description{
The \code{\link[base]{do.call}} can be somewhat slow,
especially when working with large objects. This function
is based upon the suggestions from Hadley Wickham on the R
mailing list (reference not available anymore).
Also thanks to \emph{Tommy} at StackOverflow for
\href{https://stackoverflow.com/questions/10022436/do-call-in-combination-with}{suggesting}
how to handle double and triple colon operators, \code{::},
further enhancing the function.
}
\author{Max Gordon <max@gforge.se>}
\section{Note}{
  While the function attempts to do most of what
  \code{\link[base]{do.call}} can it has limitations. It
  can currently not parse the example code from the
  original function: \cr
  \code{do.call(paste,
  list(as.name("A"), as.name("B")), quote = TRUE)} and the
  funcitonality of \code{quote} has not been thoroughly
  tested.
}
\note{This is a verbatim copy from \code{Gmisc::fastDoCall.}}
\examples{
DoCall("complex", list(imaginary = 1:3))

## if we already have a list (e.g. a data frame)
## we need c() to add further arguments
tmp <- expand.grid(letters[1:2], 1:3, c("+", "-"))
DoCall("paste", c(tmp, sep = ""))

## examples of where objects will be found.
A <- 2
f <- function(x) print(x^2)
env <- new.env()
assign("A", 10, envir = env)
assign("f", f, envir = env)
f <- function(x) print(x)
f(A)                                         # 2
DoCall("f", list(A))                         # 2
DoCall("f", list(A), envir = env)            # 4
DoCall(f, list(A), envir = env)              # 2
DoCall("f", list(quote(A)), envir = env)     # 100
DoCall(f, list(quote(A)), envir = env)       # 10
DoCall("f", list(as.name("A")), envir = env) # 100

eval(call("f", A))                           # 2
eval(call("f", quote(A)))                    # 2
eval(call("f", A), envir = env)              # 4
eval(call("f", quote(A)), envir = env)       # 100
}

