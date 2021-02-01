\name{WrdPlot}
\alias{WrdPlot}
\title{Insert Active Plot to Word}

\description{This function inserts the plot on the active plot device to Word. The image is transferred
by saving the picture to a file in R and inserting the file in Word.
The format of the plot can be selected, as well as crop options and the size factor for inserting.
}
\usage{
WrdPlot(type = "png", append.cr = TRUE, crop = c(0, 0, 0, 0), main = NULL,
        picscale = 100, height = NA, width = NA, res = 300,
        dfact = 1.6, wrd = DescToolsOptions("lastWord"))
}
\arguments{
  \item{type}{the format for the picture file, default is \code{"png"}.}
  \item{append.cr}{should a carriage return be appended? Default is TRUE.}
  \item{crop}{crop options for the picture, defined by a 4-elements-vector. The first element is the bottom side, the second the left and so on.}
  \item{main}{a caption for the plot. This will be inserted by InserCaption in Word. Default is NULL, which will insert nothing.}
  \item{picscale}{scale factor of the picture in percent, default ist 100.}
  \item{height}{height in cm, this overrides the picscale if both are given.}
  \item{width}{width in cm, this overrides the picscale if both are given.}
  \item{res}{resolution for the png file, defaults to 300.}
  \item{dfact}{the size factor for the graphic.}
  \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()}
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{DescToolsOptions("lastWord")}.}
}
\value{Returns a pointer to the inserted picture.}
\author{Andri Signorell <andri@signorell.net>}

\seealso{\code{\link{ToWrd}}, \code{\link{WrdCaption}}, \code{\link{GetNewWrd}}}

\examples{
\dontrun{ # Windows-specific example
# let's have some graphics
plot(1,type="n", axes=FALSE, xlab="", ylab="", xlim=c(0,1), ylim=c(0,1), asp=1)
rect(0,0,1,1,col="black")
segments(x0=0.5, y0=seq(0.632,0.67, length.out=100),
  y1=seq(0.5,0.6, length.out=100), x1=1, col=rev(rainbow(100)))
polygon(x=c(0.35,0.65,0.5), y=c(0.5,0.5,0.75), border="white",
  col="black", lwd=2)
segments(x0=0,y0=0.52, x1=0.43, y1=0.64, col="white", lwd=2)
x1 <- seq(0.549,0.578, length.out=50)
segments(x0=0.43, y0=0.64, x1=x1, y1=-tan(pi/3)* x1 + tan(pi/3) * 0.93,
  col=rgb(1,1,1,0.35))


# get a handle to a new word instance
wrd <- GetNewWrd()
# insert plot with a specified height
WrdPlot(wrd=wrd, height=5)
ToWrd("Remember?\n", fontname="Arial", fontsize=14, bold=TRUE, wrd=wrd)
# crop the picture
WrdPlot(wrd=wrd, height=5, crop=c(9,9,0,0))


wpic <- WrdPlot(wrd=wrd, height=5, crop=c(9,9,0,0))
wpic
}
}
\keyword{ print }
\concept{MS-Office}
