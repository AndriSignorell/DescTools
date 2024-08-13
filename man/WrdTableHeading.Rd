\name{WrdTableHeading}
\alias{WrdTableHeading}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Insert Headings for a Table in Word
}
\description{
Inserting headings in a table can be hard, when column headings should span several columns. This function helps to easily insert headings and merge cells.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdTableHeading(wtab, text, bold = TRUE, 
                alignment = wdConst$wdAlignParagraphCenter, 
                merge_cols = NULL, wrd = DescToolsOptions("lastWord"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wtab}{the handle to a table in a word document
%%     ~~Describe \code{wtab} here~~
}
  \item{text}{the text for the headings
%%     ~~Describe \code{text} here~~
}
  \item{bold}{logical, for setting bold column headings, will be recycled. Default is \code{TRUE}.
%%     ~~Describe \code{bold} here~~
}
  \item{alignment}{the alignment in the column headings, must be one out of the Word constant list \code{wdConst$wdAlignParagraph}.
%%     ~~Describe \code{alignment} here~~
}
  \item{merge_cols}{a vector consisting of entries to merge cells in the form \code{"<first cell>:<last cell>"}, example \code{merge_cols=c("2:4", "7:8")} would merge the column headings 2:4 to one cell and 7:8 as well.
%%     ~~Describe \code{merge_cols} here~~
}
\item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()}
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{DescToolsOptions("lastWord")}.
}
}

\value{Nothing returned.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Andri Signorell <andri@signorell.net}

\seealso{\code{\link{WrdTable}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{ # Windows-specific example
wrd <- GetNewWrd()
wtab <- WrdTable(nrow=3, ncol=5, wrd=wrd)

# insert headings and merge 1:2 and 4:5, such as there are 
# only 3 headings
WrdTableHeading(wtab, text=c("First","Second", 
                              "Third"),
                  alignment=c(wdConst$wdAlignParagraphLeft, 
                              rep(wdConst$wdAlignParagraphCenter, 2)), 
                  merge_cols = c("1:2", "4:5"))
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
