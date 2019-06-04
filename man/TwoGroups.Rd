\name{TwoGroups}
\alias{TwoGroups}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Describe a Variable by a Factor with Two Levels
%%  ~~function to do ... ~~
}
\description{This function describes a numeric variable by a grouping factor with two levels. First, a descriptive text listing the frequencies and means of the two groups and the results of the significance test is generated. The results of \code{Desc(x~g)} are reported as they are provided by the function, followed by a plot consisting of a density plot and a box plot.
This description makes sense, for example, if the age distribution of a collective is to be represented for both sexes.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
TwoGroups(x, g, test = t.test, main = NULL,
          font.txt = NULL, font.desc = NULL, wrd = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the numeric variable to describe.
%%     ~~Describe \code{x} here~~
}
  \item{g}{the grouping factor (preferably with two levels.)
%%     ~~Describe \code{g} here~~
}
  \item{test}{the test to be applied, default is \code{\link{t.test}()}.
%%     ~~Describe \code{test} here~~
}
  \item{main}{the main title.
%%     ~~Describe \code{main} here~~
}
  \item{font.txt}{the font chosen for the introducing text, when sending the output to Word.
%%     ~~Describe \code{font.txt} here~~
}
  \item{font.desc}{the font chosen for the description, when sending the output to Word.
%%     ~~Describe \code{font.desc} here~~
}
  \item{wrd}{the pointer to a running MS Word instance, as created by GetNewWrd() (for a new one) or by GetCurrWrd() for an existing one. Default is \code{NULL}, which will report all results to the console.
%%     ~~Describe \code{wrd} here~~
}
\item{\dots}{the dots are sent to the internally used function \code{\link{Phrase}()}. They can be used to choose the language (\code{lang}) or provide variable name (\code{xname}).}
}
\value{list with the results calculated by the used functions
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{Desc}}, \code{\link{PlotMultiDens}}, \code{\link{Phrase}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- d.pizza$temperature
g <- factor(d.pizza$rabate)

# we can change the colors for the plot by setting the DescToolsOptions
DescToolsOptions(col=c(horange, hgreen))
TwoGroups(x, g, main="Temperature ~ Rebate")

# for an output to Word simply define the wrd argument
# wrd <- GetNewWrd()
# TwoGroups(x, g, font.desc=list(name="Consolas", size=8),
#           main="Temperature ~ Rebate", wrd=wrd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ "print"}
\keyword{ "multivariate"}
\keyword{ "distribution"}

% use one of  RShowDoc("KEYWORDS")

