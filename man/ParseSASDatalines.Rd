\name{ParseSASDatalines}
\alias{ParseSASDatalines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Parse a SAS Dataline Command
%%  ~~function to do ... ~~
}
\description{A parser for simple SAS dataline command texts. A \code{data.frame} is being built with the columnnames listed in the input section. The data object will be created in the given environment.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ParseSASDatalines(x, env = .GlobalEnv, overwrite = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the SAS text }
  \item{env}{environment in which the dataset should be created.}
  \item{overwrite}{logical. If set to TRUE, the function will silently overwrite a potentially existing object in \code{env} with the same name as declared in the SAS \code{DATA} section. If set to \code{FALSE} (default) an error will be raised if there already exists an object with the same name. }


}
\details{The SAS function \code{DATA} is designed for quickly creating a dataset from scratch. The whole step  normally consists out of the \code{DATA} part defining the name of the dataset, an \code{INPUT} line declaring the variables and a \code{DATALINES} command followed by the values.\cr
The default delimiter used to separate the different variables is a space (thus each variable should be one word). The $ after the variable name indicates that the variable preceding contain character values and not numeric values. Without specific instructions, SAS assumes that variables are numeric. The function will fail, if it encounters a character in the place of an expected numeric value.\cr
Each new row in datalines will create a corresponding unique row in the dataset. Notice that a ; is not needed after every row, rather it is included at the end of the entire data step.

More complex command structures, i.e. other delimiters (dlm), in the \code{INPUT}-section are not (yet) supported.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a data.frame}
\author{Andri Signorell <andri@signorell.net>}
\seealso{ \code{\link{scan}}}

\examples{
txt <- "
DATA asurvey;
INPUT id sex $ age inc r1 r2 r3 ;
DATALINES;
1   F  35 17  7 2 2
17  M  50 14  5 5 3
33  F  45  6  7 2 7
49  M  24 14  7 5 7
65  F  52  9  4 7 7
81  M  44 11  7 7 7
2   F  34 17  6 5 3
18  M  40 14  7 5 2
34  F  47  6  6 5 6
50  M  35 17  5 7 5
;
"

(d.frm <- ParseSASDatalines(txt))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
