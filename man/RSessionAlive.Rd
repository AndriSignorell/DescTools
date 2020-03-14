\name{RSessionAlive}
\alias{RSessionAlive}
\alias{RTempdirAlive}

\title{How Long Has the RSession Been Running?
}
\description{\code{RSessionAlive()} returns the time the R session has been running in hours. The function uses powershell in Windows and is thus restricted to run in windows only. \code{RTempdirAlive()} does the same for temporary directories, but runs on all systems. 
}
\usage{
RSessionAlive()
RTempdirAlive()
}
\value{time in hours
}
\author{Markus Napflin <markus.naepfl@in>, Andri Signorell <andri@signorell.net>
}
\seealso{\code{\link{Sys.getenv}}
}
