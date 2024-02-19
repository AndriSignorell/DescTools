\name{TwoGroups}
\alias{TwoGroups}
\alias{TwoGroups.default}
\alias{TwoGroups.formula}
\alias{ToWrd.TwoGroups}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Describe a Variable by a Factor with Two Levels
%%  ~~function to do ... ~~
}
\description{This function describes a numeric variable by a grouping factor with two levels. First, a descriptive text listing the frequencies and means of the two groups and the results of the significance test is generated. The results of \code{Desc(x~g)} are reported as they are provided by the function, followed by a plot consisting of a density plot and a box plot.
This description makes sense, for example, if the age distribution of a collective is to be represented for both sexes.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

TwoGroups(x, ..., plotit = TRUE)

\method{TwoGroups}{default}(x, g, main = NULL, vname = NULL, ..., plotit = TRUE)

\method{TwoGroups}{formula}(formula, data, subset, na.action, \dots)

\method{ToWrd}{TwoGroups}(x, font = NULL, ..., wrd = DescToolsOptions("lastWord"))

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the numeric variable to describe.
%%     ~~Describe \code{x} here~~
}
  \item{g}{the grouping factor (preferably with two levels.)
%%     ~~Describe \code{g} here~~
}
  \item{main}{the main title.
%%     ~~Describe \code{main} here~~
}
  \item{vname}{the variable names used in the description text.
%%     ~~Describe \code{main} here~~
}
\item{plotit}{boolean. Should a plot be created? Default can be defined by \code{DescToolsOptions(plotit=TRUE/FALSE)}, if it does not exist then it's set to \code{FALSE}.
}
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the data values and rhs the corresponding groups.}  
  \item{data}{an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula \code{formula}. 
    By default the variables are taken from \code{environment(formula)}.}  
  \item{subset}{an optional vector specifying a subset of observations to be used.}  
  \item{na.action}{a function which indicates what should happen when the data contain NAs. Defaults to \code{getOption("na.action")}.}  

  \item{font}{the first font will be chosen for the introducing text, when sending the output to Word, the second for the description.
%%     ~~Describe \code{font.txt} here~~
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
DescToolsOptions(col=c(DescTools::horange, DescTools::hgreen))
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

