\name{CourseData}
\alias{CourseData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get HWZ Datasets
%%  ~~function to do ... ~~
}
\description{Wrapper for didactical datasets used in statistic courses.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CourseData(name, url = NULL, header = TRUE, sep = ";", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{the name of the file, usually without extension.
%%     ~~Describe \code{name} here~~
}
  \item{url}{a url where the data reside, should have the form \code{"http://www.mysite.net/data/"}. Defaults to the data folder on my site.
%%     ~~Describe \code{url} here~~
}
  \item{header}{a logical value indicating whether the file contains the names of the variables as its first line. If missing, the value is determined from the file format: header is set to \code{TRUE} if and only if the first row contains one fewer field than the number of columns.
%%     ~~Describe \code{header} here~~
}
  \item{sep}{the field separator character. Values on each line of the file are separated by this character. Default is - unlike in \code{\link{read.table}} - ";" instead of 'white space'.
%%     ~~Describe \code{sep} here~~
}
  \item{\dots}{the dots are sent to \code{\link{read.table}}.
%%     ~~Describe \code{\dots} here~~
}
}
\value{A \code{\link{data.frame}} containing a representation of the data in the file.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{read.table}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
d.farm <- CourseData("farmer")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ datasets }% use one of  RShowDoc("KEYWORDS")
