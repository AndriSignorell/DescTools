\name{MultMerge}
\alias{MultMerge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Merge Multiple Data Frames
%%  ~~function to do ... ~~
}
\description{Merge multiple data frames by row names, or do other versions of database join operations.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MultMerge(..., all.x = TRUE, all.y = TRUE, by = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{data frames to be coerced to one.
%%     ~~Describe \code{\dots} here~~
}
  \item{all.x}{logical; if \code{TRUE}, then extra rows will be added to the output, one for each row in x that has no matching row in y. These rows will have \code{NA}s in those columns that are usually filled with values from y. The default is \code{FALSE}, so that only rows with data from both x and y are included in the output.
%%     ~~Describe \code{all.x} here~~
}
  \item{all.y}{logical; analogous to \code{all.x}.
%%     ~~Describe \code{all.y} here~~
}
\item{by}{column used for merging, if this is not defined rownames will be used by default. The column must be included in all the provided data frames.}
}
\value{A data frame. The rows are sorted according to the appearance of previously unobserved rownames. So the rownames appearing in the first data frame are first, then the rownames in the second data frame, which have no corespondence in the first data frame and so on. The columns are the remaining columns in x1 and then those in x2 and then those in x3. The result has the row names resulting from the merge.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{merge}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x1 <- SetNames(data.frame(v=letters[1:6], w=1:6), 
               rownames=c("A", "B", "C", "D", "E", "F"))
x2 <- SetNames(data.frame(v=letters[1:3], ww=11:13), 
               rownames=c("B", "C", "D"))
x3 <- SetNames(data.frame(v=letters[12:16], wwww=22:26), 
               rownames=c("A", "C", "E", "G", "J"))

# default is "merge by rownames" 
MultMerge(x1, x2, x3)
# ... which does not really make sense here

# merge by column v
MultMerge(x1, x2, x3, by="v")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{data}
\keyword{manip}
\keyword{array}
\keyword{methods1}
% __ONLY ONE__ keyword per line




