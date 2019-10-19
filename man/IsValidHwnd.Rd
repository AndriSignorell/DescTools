\name{IsValidHwnd}
\alias{IsValidHwnd}
\title{Check Windows Pointer
}
\description{Check if a pointer points to a valid and running MS-Office instance. The function does this by first checking for \code{NULL} and \code{nil} pointer and then
trying to get the current selection of the application.
}
\usage{
IsValidHwnd(hwnd)
}
\arguments{
  \item{hwnd}{the pointer to a word instance as created by \code{GetNewWrd()} or \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{DescToolsOptions("lastWord")}.}
}
\value{logical value, \code{TRUE} if hwnd is a valid pointer to a running application
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{ \code{\link{GetCurrWrd}}(), \code{\link{GetCurrXL}}(), \code{\link{GetCurrPP}}()

}

