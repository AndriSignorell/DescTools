\name{SendOutlookMail}
\alias{SendOutlookMail}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Send a Mail Using Outlook as Mail Client
%%  ~~function to do ... ~~
}
\description{Sending emails in R can be required in some reporting tasks. As we already have RDCOMClient available we wrap the send code in a function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SendOutlookMail(to, cc = NULL, bcc = NULL, subject, body, attachment = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{to}{a vector of recipients
%%     ~~Describe \code{to} here~~
}
  \item{cc}{a vector of recipients receiving a carbon copy
%%     ~~Describe \code{cc} here~~
}
  \item{bcc}{a vector of recipients receiving a blind carbon copy
%%     ~~Describe \code{bcc} here~~
}
  \item{subject}{the subject of the mail
%%     ~~Describe \code{subject} here~~
}
  \item{body}{the body text of the mail
%%     ~~Describe \code{body} here~~
}
  \item{attachment}{a vector of paths to attachments
%%     ~~Describe \code{attachment} here~~
}
}
\value{Nothing is returned
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net> strongly based on code of Franziska Mueller 
%%  ~~who you are~~
}

\seealso{\code{\link{ToXL}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{\dontrun{
SendOutlookMail(to=c("me@microsoft.com", "you@rstudio.com"), subject = "Some Info", 
                body = "Hi all\r Find the files attached\r Regards, Andri", 
                attachment = c("C:/temp/fileA.txt", 
                               "C:/temp/fileB.txt"))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{MS-Office}% __ONLY ONE__ keyword per line
