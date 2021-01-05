\name{PasswordDlg}
\alias{PasswordDlg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Password Dialog
%%  ~~function to do ... ~~
}
\description{Brings up a tcltk dialog centered on the screen, designed for entering passwords while displaying only ****.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PasswordDlg(option_txt = NULL)
}
\arguments{
  \item{option_txt}{an optional text, if it is defined, there will be a checkbox added to the dialog with the label being set with \code{option_txt}.}
}
%- maybe also 'usage' for other objects documented here.
\value{the entered password\cr
the status of the optional checkbox will be returned as attribute: 
\code{attr(pw, "option")}
}
\author{Markus Naepflin <markus@naepfl.in>
}
\examples{
\dontrun{
pw <- PasswordDlg()
pw}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities }
