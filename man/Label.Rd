\name{Label, Unit}
\alias{Label}
\alias{Label<-}
\alias{Labels}
\alias{Labels<-}
\alias{Unit}
\alias{Unit<-}

\title{Label, Unit Attribute of an Object
%%  ~~function to do ... ~~
}
\description{ Set and retrieve the \code{label}, resp. \code{unit} attribute of \code{x}. This can be helpful for documenting the specific meaning of a variable, of an entire data.frame or any other object. For single vectors it can be useful to store the unit.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Label(x)
Label(x) <- value

Labels(x)
Labels(x) <- value

Unit(x)
Unit(x) <- value

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{any object
%%     ~~Describe \code{x} here~~
}
  \item{value}{a single string describing the object
%%     ~~Describe \code{x} here~~
}
}

\details{The label should consist of a single text (length of 1). The text may contain line feeds.
It can be deleted by setting the label to \code{NULL}.

\code{Labels()} can be used to retrieve and assign vectorized labels to data.frames or lists.
}
\value{
\code{Label} and \code{Unit} return the label attribute of x, if any; otherwise, NULL.
}

\author{Andri Signorell <andri@signorell.net>
}

\seealso{
A more elaborated label version can be found in package \pkg{Hmisc} \code{\link[Hmisc]{label}()}.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# add a descriptive label to a variable
Label(d.diamonds$colour) <- "The rating scale applied to diamonds ranges from colorless
to yellow, as any other color is extremely rare."

# technically just appending the text as attribute to the variable
attributes(d.diamonds$colour)

# label is supported while describing data
Desc(d.diamonds$colour)

# The label can be deleted by setting it to NULL
Label(d.diamonds$colour) <- NULL

# Labelling the columns of a data.frame is best done with a loop
# (all so far seen *apply aproaches lead to more complicated code...)
lbl <-  RndWord(16, 7)
for(i in seq_along(lbl))
  Label(d.pizza[, i]) <- lbl[i]

Str(d.pizza)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{misc}
\keyword{utilities}
