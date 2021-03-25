\name{PowerPoint Interface}
\alias{PpPlot}
\alias{PpText}
\alias{PpAddSlide}
\alias{GetNewPP}
\alias{GetCurrPP}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add Slides, Insert Texts and Plots to PowerPoint
}
\description{A couple of functions to get R-stuff into MS-Powerpoint.

\code{GetNewPP()} starts a new instance of PowerPoint and returns its handle. A new presentation with one empty slide will be created thereby. The handle is needed for addressing the presentation afterwards.\cr
\code{GetCurrPP()} will look for a running PowerPoint instance and return its handle. \code{NULL} is returned if nothing's found.
\code{PpAddSlide()} inserts a new slide into the active
presentation.\cr
\code{PpPlot()} inserts the active plot into PowerPoint. The image is transferred by saving the picture to a file in R and
inserting the file in PowerPoint. The format of the plot can be selected, as well as crop options and the
size factor for inserting.\cr
\code{PpText()} inserts a new textbox with given text and box properties.

}
\usage{
GetNewPP(visible = TRUE, template = "Normal")
GetCurrPP()

PpAddSlide(pos = NULL, pp = DescToolsOptions("lastPP"))

PpPlot(type = "png", crop = c(0, 0, 0, 0), picscale = 100, x = 1, y = 1,
       height = NA, width = NA, res=200, dfact=1.6, pp = DescToolsOptions("lastPP"))

PpText(txt, x = 1, y = 1, height = 50, width = 100, fontname = "Calibri", fontsize = 18,
       bold = FALSE, italic = FALSE, col = "black", bg = "white",
       hasFrame = TRUE, pp = DescToolsOptions("lastPP"))

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{visible}{logical, should PowerPoint made visible by \code{GetNewPP()}? Defaults to TRUE.
%%     ~~Describe \code{visible} here~~
}
  \item{template}{the name of the template to be used for the new presentation.
%%     ~~Describe \code{template} here~~
}

  \item{pos}{position of the new inserted slide within the presentation.}
  \item{type}{the format for the picture file, default is \code{"png"}.}
  \item{crop}{crop options for the picture, defined by a 4-elements-vector. The first element is the bottom side, the second the left and so on.}
  \item{picscale}{scale factor of the picture in percent, default ist 100.}
  \item{x, y}{left/upper xy-coordinate for the plot or for the textbox.}
  \item{height}{height in cm, this overrides the picscale if both are given.}
  \item{width}{width in cm, this overrides the picscale if both are given.}
  \item{res}{resolution for the png file, defaults to 200.}
  \item{dfact}{the size factor for the graphic.}

  \item{txt}{text to be placed in the textbox}
  \item{fontname}{used font for textbox}
  \item{fontsize}{used fontsize for textbox}
  \item{bold}{logic. Text is set bold if this is set to \code{TRUE} (default is FALSE).}
  \item{italic}{logic. Text is set italic if this is to \code{TRUE} (default is FALSE).}
  \item{col}{font color, defaults to \code{"black"}.}
  \item{bg}{background color for textboxdefaults to \code{"white"}.}
  \item{hasFrame}{logical. Defines if a textbox is to be framed. Default is TRUE.}

  \item{pp}{the pointer to a PowerPoint instance, can be a new one, created by \code{GetNewPP()} or
    the last created by \code{DescToolsOptions("lastPP")} (default).}

    }
\details{See PowerPoint-objectmodel for further informations.
%%  ~~ If necessary, more details than the description above ~~
}
\value{The functions return the pointer to the created object.
}
\author{
Andri Signorell <andri@signorell.net>
}


\seealso{\code{\link{WrdPlot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{# Windows-specific example

# let's have some graphic
plot(1,type="n", axes=FALSE, xlab="", ylab="", xlim=c(0,1), ylim=c(0,1))
rect(0,0,1,1,col="black")
segments(x0=0.5, y0=seq(0.632,0.67, length.out=100),
  y1=seq(0.5,0.6, length.out=100), x1=1, col=rev(rainbow(100)))
polygon(x=c(0.35,0.65,0.5), y=c(0.5,0.5,0.75), border="white",
  col="black", lwd=2)
segments(x0=0,y0=0.52, x1=0.43, y1=0.64, col="white", lwd=2)
x1 <- seq(0.549,0.578, length.out=50)
segments(x0=0.43, y0=0.64, x1=x1, y1=-tan(pi/3)* x1 + tan(pi/3) * 0.93,
  col=rgb(1,1,1,0.35))


# get a handle to a new PowerPoint instance
pp <- GetNewPP()
# insert plot with a specified height
PpPlot(pp=pp,  x=150, y=150, height=10, width=10)

PpText("Remember?\n", fontname="Arial", x=200, y=70, height=30, fontsize=14,
       bold=TRUE, pp=pp, bg="lemonchiffon", hasFrame=TRUE)

PpAddSlide(pp=pp)
# crop the picture
pic <- PpPlot(pp=pp, x=1, y=200, height=10, width=10, crop=c(9,9,0,0))
pic


# some more automatic procedure
pp <- GetNewPP()
PpText("Hello to my presentation", x=100, y=100, fontsize=32, bold=TRUE,
       width=300, hasFrame=FALSE, col="blue", pp=pp)

for(i in 1:4){
  barplot(1:4, col=i)
  PpAddSlide(pp=pp)
  PpPlot(height=15, width=21, x=50, y=50, pp=pp)
  PpText(gettextf("This is my barplot nr \%s", i), x=100, y=10, width=300, pp=pp)
}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
\concept{MS-Office}
