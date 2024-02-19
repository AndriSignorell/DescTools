\name{PlotConDens}
\alias{PlotConDens}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Conditional Densities
%%  ~~function to do ... ~~
}
\description{Plot conditional densities by group. For describing how the conditional distribution of a categorical variable y changes over a numerical variable x we have the function cdplot. But if we want to compare multiple densities much work is required. PlotConDens allows to easily enter a grouping variable. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotConDens(formula, data, col = NULL, lwd = 2, lty = 1, xlim = NULL, rev = TRUE, 
            args.dens = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a \code{"formula"} of type \code{y ~ x | g} with a single dependent \code{factor}, a single numerical explanatory variable and a grouping \code{factor} g.
%%     ~~Describe \code{formula} here~~
}
  \item{data}{a data frame containing values for any variables in the formula. By default the environment where \code{PlotConDens} was called from is used.
%%     ~~Describe \code{data} here~~
}
  \item{col}{a vector of colors to be used to plot the lines. If too short, the values are recycled.
%%     ~~Describe \code{col} here~~
}
  \item{lwd}{a vector of linewidths to be used to plot the lines. If too short, the values are recycled.
%%     ~~Describe \code{lwd} here~~
}
  \item{lty}{a vector of linetypes to be used to plot the lines. If too short, the values are recycled.
%%     ~~Describe \code{lty} here~~
}
  \item{xlim}{the range for the x axis.
%%     ~~Describe \code{xlim} here~~
}
  \item{rev}{logical, should the values of the response variable be reversed? Default is \code{TRUE}.
%%     ~~Describe \code{rev} here~~
}
  \item{args.dens}{additional arguments for the densitiy curves.
%%     ~~Describe \code{args.dens} here~~
}
  \item{\dots}{the dots are passed on to \code{plot()}.
%%     ~~Describe \code{\dots} here~~
}
}
\details{Especially when we're modelling binary response variables we might want to know, how the binary variable behaves along some numeric predictors.
%%  ~~ If necessary, more details than the description above ~~
}
\value{the functions for the curves
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{cdplot}}, \code{\link{spineplot}}, \code{\link{density}}, \code{\link{PlotMultiDens}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(Pima.tr2, package="MASS")
PlotConDens (type ~ age | I((npreg > 0)*1L), 
             data=Pima.tr2, col=c(DescTools::hblue, DescTools::hred), rev=FALSE, 
             panel.first=quote(grid()))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
