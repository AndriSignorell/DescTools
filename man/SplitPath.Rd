\name{SplitPath}
\alias{SplitPath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Split Path In Drive, Path, Filename
%%  ~~function to do ... ~~
}
\description{Split a full path in its components. This is specifically an issue in Windows and not really interesting for other OSs.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SplitPath(path, last.is.file = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{a path
%%     ~~Describe \code{path} here~~
}
  \item{last.is.file}{logical, determining if the basename should be interpreted as filename or as last directory. If set to \code{NULL} (default), the last entry will be interpreted if the last character is either \ or / and as filename else.}
}
\value{
A list, containing the following components:
  \item{normpath}{ the normalized path as returned by \code{\link{normalizePath}}}
  \item{drive}{the drive if the OS is Windows, \code{NA} else}                  
  \item{dirname}{the path without directory and without filename}
  \item{fullfilename}{the filename including extension}
  \item{filename}{the filename without extension}
  \item{extension}{  the file extension}
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{dirname}}, \code{\link{basename}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{ # Windows-specific example
path <- "C:/Documents/Projects/Import/eyestudy.dta"
SplitPath(path)

path <- "C:/Documents/Projects/Import/"
SplitPath(path)

path <- "C:/Documents/Projects/Import"
SplitPath(path)  # last entry will be taken as filename
SplitPath(path, last.is.file=FALSE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
