\name{PlotVenn}
\alias{PlotVenn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a Venn Diagram
%%  ~~function to do ... ~~
}
\description{This function produces Venn diagrams for up to 5 datasets. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotVenn(x, col = "transparent", plotit = TRUE, labels = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the list with the sets to be analysed. Those can be factors or something coercable to a factor.
%%     ~~Describe \code{x} here~~
}
  \item{col}{the colors for the sets on the plot.
%%     ~~Describe \code{col} here~~
}
  \item{plotit}{logical. Should a plot be produced or just the results be calculated.
}
  \item{labels}{special labels for the plot. By default the names of the list x will be used. If those are missing, the LETTERS A..E will be chosen. Set this argument to NA, if no labels at all should be plotted.
}
}

\details{The function calculates the necessary frequencies and plots the venn diagram. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{a list with 2 elements, the first contains a table with the observed frequencies in the given sets.
The second returns a data.frame with the xy coordinates for the labels in the venn diagram, the specific combination of factors and 
the frequency in that intersection area. The latter can be 0 as well.

}
\references{
Venn, J. (1880): On the Diagrammatic and Mechanical Representation of Propositions and Reasonings. 
  \emph{Dublin Philosophical Magazine and Journal of Science} 9 (59): 1-18.
  
Edwards, A.W.F. (2004): Cogwheels of the mind: the story of Venn diagrams. \emph{JHU Press} ISBN 978-0-8018-7434-5.

%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\examples{
element <- function() paste(sample(LETTERS, 5, replace=TRUE), collapse="")
group <- replicate(1000, element())

GroupA <- sample(group, 400, replace=FALSE)
GroupB <- sample(group, 750, replace=FALSE)
GroupC <- sample(group, 250, replace=FALSE)
GroupD <- sample(group, 300, replace=FALSE)

x <- list(GroupA, GroupB, GroupC, GroupD)
x

PlotVenn(x=list(GroupA, GroupB))
PlotVenn(x=list(Set_1=GroupA, Set_2=GroupB))
PlotVenn(x=list(GroupA, GroupB), labels=c("English","Spanish"))

PlotVenn(x=x[1:3])
PlotVenn(x=x[1:4], col=SetAlpha(c("blue","red","yellow","green","lightblue"), 0.2))

r.venn <- PlotVenn(x=x[1:5], col=SetAlpha(c("blue","red","yellow","green","lightblue"), 0.2))
r.venn
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

