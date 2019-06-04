\name{IsValidWrd}
\alias{IsValidWrd}
\title{Check Word Pointer
}
\description{Check if a pointer points to a valid and running Word instance. The function does this by
trying to get the current selection of the Word instance and returns \code{FALSE} if it's NULL.
}
\usage{
IsValidWrd(wrd = DescToolsOptions("lastWord"))
}
\arguments{
  \item{wrd}{the pointer to a word instance as created by \code{GetNewWrd()} or \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{DescToolsOptions("lastWord")}.}
}
\value{logical value
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{ \code{\link{GetCurrWrd}}()

}
\keyword{ print }
