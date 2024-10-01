\name{ReadSPSS}
\alias{ReadSPSS}
\title{
Read SPSS Datafiles and Return It in Good Old Style Data Frame Structure
}
\description{While haven is a great package it uses tibbles as basic data
structures. Older R users (as myself) might prefer a more archaic structures.
This function returns SPSS files in form of a data.frame and the nominal 
variables as factors. 
}
\usage{
ReadSPSS(fn, encoding = NULL)
}
\arguments{
  \item{fn}{Either a path to a file, a connection, or literal data 
          (either a single string or a raw vector). }
  \item{encoding}{The character encoding used for the file. 
        The default, NULL, use the encoding specified in the file, 
        but sometimes this value is incorrect and it is useful to 
        be able to override it.  }
}
\value{
A data frame.

Variable labels are stored in the "label" attribute of 
each variable. It is not printed on the console, 
but the RStudio viewer will show it.
}
\author{Andri Signorell <andri@signorell.net>
}
\seealso{\link[haven]{read_spss}
}
