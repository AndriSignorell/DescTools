\name{ParseFormula}
\alias{ParseFormula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Parse a Formula and Create a Model Frame
%%  ~~function to do ... ~~
}
\description{Create a model frame for a formula object, by handling the left hand side the same way
the right hand side is handled in model.frame. Especially variables separated by + are interpreted as separate 
variables.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ParseFormula(formula, data = parent.frame(), drop = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description 
  for the variables to be described.
%%     ~~Describe \code{formula} here~~
}
  \item{data}{an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) 
  containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which lm is called.
%%     ~~Describe \code{data} here~~
}
  \item{drop}{if \code{drop} is \code{TRUE}, unused factor levels are dropped from the result when creating interaction terms. 
  The default is to drop all unused factor levels.
%%     ~~Describe \code{drop} here~~
}
}
\details{This is used by \code{\link{Desc.formula}} for describing data by groups while remaining flexible for using
\code{I(...)} constructions, functions or interaction terms.
}
\value{a list of 3 elements

  \item{formula}{the formula which had to be parsed}
  \item{lhs}{  a list of 3 elements:\cr
        mf:  data.frame, the model.frame of the left hand side of the formula\cr
        mf.eval: data.frame, the evaluated model.frame of the left hand side of the formula\cr
        vars: the names of the evaluated model.frame
        }
  \item{rhs}{   a list of 3 elements:\cr
        mf:   data.frame, the model.frame of the right hand side of the formula\cr
        mf.eval: data.frame, the evaluated model.frame of the right hand side of the formula\cr
        vars:  the names of the evaluated model.frame
        }
}

\author{Andri Signorell <andri@signorell.net>
}

\seealso{The functions used to handle formulas: \code{\link{model.frame}}, \code{\link{terms}}, \code{\link{formula}} \cr
Used in: \code{\link{Desc.formula}}
}
\examples{
set.seed(17)
piz <- d.pizza[sample(nrow(d.pizza),10), c("temperature","price","driver","weekday")]

f1 <- formula(. ~ driver)
f2 <- formula(temperature ~ .)
f3 <- formula(temperature + price ~ .)
f4 <- formula(temperature ~ . - driver)
f5 <- formula(temperature + price ~ driver)
f6 <- formula(temperature + price ~ driver * weekday)
f7 <- formula(I(temperature^2) + sqrt(price) ~ driver + weekday)
f8 <- formula(temperature + price ~ 1)
f9 <- formula(temperature + price ~ driver * weekday - price)

ParseFormula(f1, data=piz)  
ParseFormula(f2, data=piz)  
ParseFormula(f3, data=piz)
ParseFormula(f4, data=piz)
ParseFormula(f5, data=piz)
ParseFormula(f6, data=piz)
ParseFormula(f7, data=piz)
ParseFormula(f8, data=piz)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
