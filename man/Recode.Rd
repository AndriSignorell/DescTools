\name{Recode}
\alias{Recode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Recode a Factor
%%  ~~function to do ... ~~
}
\description{Combining or rearranging a factor can be tedious if it has many levels. \code{Recode} supports this step by
accepting a direct definition of new levels by enumerating old levelnames as argument and adding an \code{"elselevel"} option. If new levels are given as integer values they will be translated in the according levels.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Recode(x, ..., elselevel = NA, use.empty = FALSE, num = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the factor whose levels are to be altered.
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{the old levels (combined by \code{c}() if there are several) named with the new level:\cr
  \code{newlevel_a=c("old_a", "old_b"), }\cr\code{newlevel_b=c("old_c", "old_d")}\cr See examples.
%%     ~~Describe \code{newlevels} here~~
}
  \item{elselevel}{the value for levels, which are not matched by newlevel list.
  If this is set to \code{NULL}, the elselevels will be left unchanged.
  If set to \code{NA} (default) non matched levels will be set to \code{NA}.
}
  \item{use.empty}{logical. Defines how a new level, which can't be found in x, should be handled.  Should it be left in the level's list or be dropped? The default is \code{FALSE},
   which drops empty levels.
%%     ~~Describe \code{newlevels} here~~
}
\item{num}{logical. If set to \code{TRUE} the result will be numeric.}
}
\value{
the factor having the new levels applied.
}

\author{Andri Signorell <andri@signorell.net>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{factor}}, \code{\link{levels}}\cr
There's another solution in the package \bold{car}.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
set.seed(1984)
x <- factor(sample(1:15, 20, replace=TRUE))
levels(x) <- paste("old", levels(x), sep="_")

y <- Recode(x,
            "new_1"   = c("old_1","old_4","old_5"),
            "new_2"   = c("old_6","old_10","old_11"),
            "new_3"   = c("old_12","old_13"),
            elselevel = "other")
data.frame(x=x, y=y)


x <- factor(letters[1:6])

z1 <- Recode(x, AB=c("a","b"), CD=c("c","d"), elselevel="none of these")
z2 <- Recode(x, AB=c("a","b"), CD=c("c","d"), elselevel=NA)
z3 <- Recode(x, AB=c("a","b"), CD=c("c","d"), elselevel=NULL)
z4 <- Recode(x, AB=c("a","b"), GH=c("g","h"), elselevel=NA, use.empty=TRUE)
z5 <- Recode(x, AB=c("a","b"), GH=c("g","h"), elselevel=NA, use.empty=FALSE)

data.frame(z1, z2, z3, z4, z5)

lapply(data.frame(z1, z2, z3, z4, z5), levels)

# empty level GH exists in z4...
table(z4, useNA="ifany")
# and is dropped in z5
table(z5, useNA="ifany")

# use integers to define the groups to collapse
set.seed(1972)
(likert <- factor(sample(1:10, size=15, replace=TRUE),
                  levels=1:10, labels=gettextf("(\%s)", 1:10)))
Recode(likert, det=1:6, pas=7:8, pro=9:10)

# or directly turned to numeric
Recode(likert, "1"=1:6, "2"=7:8, "5"=9:10, num=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
