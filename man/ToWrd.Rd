\name{ToWrd}
\alias{ToWrd}
\alias{ToWrd.table}
\alias{ToWrd.ftable}
\alias{ToWrd.character}
\alias{ToWrd.lm}
\alias{ToWrd.TOne}
\alias{ToWrd.TMod}
\alias{ToWrd.Freq}
\alias{ToWrd.default}
\alias{ToWrd.data.frame}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Send Objects to Word
%%  ~~function to do ... ~~
}
\description{Send objects like tables, ftables, lm tables, TOnes or just simple texts to a MS-Word document.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ToWrd(x, font = NULL, ..., wrd = DescToolsOptions("lastWord"))

\method{ToWrd}{Freq}(x, font = NULL, main = NULL, ..., wrd = DescToolsOptions("lastWord"))

\method{ToWrd}{table}(x, font = NULL, main = NULL, align = NULL,
      tablestyle = NULL, autofit = TRUE,
      row.names = FALSE, col.names = TRUE, ..., wrd = DescToolsOptions("lastWord"))

\method{ToWrd}{data.frame}(x, font = NULL, main = NULL, row.names = NULL, ...,
      wrd = DescToolsOptions("lastWord"))

\method{ToWrd}{ftable}(x, font = NULL, main = NULL, align = NULL,
      method = "compact", ..., wrd = DescToolsOptions("lastWord"))

\method{ToWrd}{TOne}(x, font = NULL, para = NULL, main = NULL, align = NULL,
      autofit = TRUE, ..., wrd = DescToolsOptions("lastWord"))

\method{ToWrd}{TMod}(x, font = NULL, para = NULL, main = NULL, align = NULL, 
      split = " ", fixed=TRUE, autofit = TRUE, digits = 3, na.form = "-", ..., 
      wrd = DescToolsOptions("lastWord")) 

\method{ToWrd}{lm}(x, font = NULL, ..., wrd = DescToolsOptions("lastWord"))

\method{ToWrd}{character}(x, font = NULL, para = NULL, style = NULL, bullet = FALSE,
      ..., wrd = DescToolsOptions("lastWord"))

\method{ToWrd}{default}(x, font = NULL, ..., wrd = DescToolsOptions("lastWord"))

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the object to be transferred to Word.
%%     ~~Describe \code{x} here~~
}
  \item{font}{the font to be used to the output. This should be defined as a list containing fontname, fontsize, bold and italic flags:\cr \code{list(name="Arial", size=10, bold=FALSE, italic=TRUE)}.
%%     ~~Describe \code{font} here~~
}
\item{para}{list containing paragraph format properties to be applied to the inserted text. For right align the paragraph one can set: \cr
\code{list(alignment="r", LineBefore=0.5)}. See details for the full set of properties.}

  \item{main}{a caption for a table. This will be inserted by \code{\link{WrdCaption}} in Word and can be listed afterwards in a specific index. Default is \code{NULL}, which will insert nothing. Ignored if \code{x} is not a table.
%%     ~~Describe \code{main} here~~
}
  \item{align}{character vector giving the alignment of the table columns. \code{"l"} means left, \code{"r"} right and \code{"c"} center alignement. The code will be recyled to the length of thenumber of columns.
%%     ~~Describe \code{align} here~~
}
 \item{method}{string specifying how the \code{"ftable"} object is formatted
     (and printed if used as in \code{write.ftable()} or the \code{print}
     method).  Can be abbreviated.  Available methods are (see the examples):
     \describe{
       \item{\code{"non.compact"}}{the default representation of an
         \code{"ftable"} object.}
       \item{\code{"row.compact"}}{a row-compact version without empty cells
         below the column labels.}
       \item{\code{"col.compact"}}{a column-compact version without empty cells
         to the right of the row labels.}
       \item{\code{"compact"}}{a row- and column-compact version.  This may imply
         a row and a column label sharing the same cell.  They are then
         separated by the string \code{lsep}.}
     }
   }

  \item{autofit}{logical, defining if the columns of table should be fitted to the length of their content.
%%     ~~Describe \code{autofit} here~~
}
  \item{row.names}{logical, defining whether the row.names should be included in the output. Default is \code{FALSE}.
%%     ~~Describe \code{row.names} here~~
}
 \item{col.names}{logical, defining whether the col.names should be included in the output. Default is \code{TRUE}.
%%     ~~Describe \code{row.names} here~~
}
 \item{tablestyle}{either the name of a defined Word tablestyle or its index.
%%     ~~Describe \code{row.names} here~~
}

\item{style}{character, name of a style to be applied to the inserted text.}
  \item{\dots}{further arguments to be passed to or from methods.
%%     ~~Describe \code{\dots} here~~
}
\item{bullet}{logical, defines if the text should be formatted as bullet points.}
\item{split}{character vector (or object which can be coerced to such) containing regular expression(s) (unless \code{fixed = TRUE}) to use for splitting. If empty matches occur, in particular if \code{split} has length 0, x is split into single characters. If \code{split} has length greater than 1, it is re-cycled along x.}
\item{fixed}{logical. If TRUE match split exactly, otherwise use regular expressions. Has priority over perl.}
  \item{digits}{integer, the desired (fixed) number of digits after the decimal point. Unlike \code{\link{formatC}} you will always get this number of digits even if the last digit is 0.
%%     ~~Describe \code{digits} here~~
}
  \item{na.form}{character, string specifying how \code{NA}s should be specially formatted.
  If set to \code{NULL} (default) no special action will be taken.
%%     ~~Describe \code{pad} here~~
}
\item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()}
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{DescToolsOptions("lastWord")}.
%%     ~~Describe \code{wrd} here~~
}
}
\value{if \code{x} is a table a pointer to the table will be returned
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\details{The paragraph format can be defined by means of these properties:

\code{LeftIndent}, \code{RightIndent}, \code{SpaceBefore}, \code{SpaceBeforeAuto}, \code{SpaceAfter}, \code{SpaceAfterAuto}, \code{LineSpacingRule},
\code{Alignment}, \code{WidowControl}, \code{KeepWithNext}, \code{KeepTogether}, \code{PageBreakBefore}, \code{NoLineNumber}, \code{Hyphenation},
\code{FirstLineIndent}, \code{OutlineLevel}, \code{CharacterUnitLeftIndent}, \code{CharacterUnitRightIndent}, \code{CharacterUnitFirstLineIndent},
\code{LineUnitBefore}, \code{LineUnitAfter}, \code{MirrorIndents}.
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{GetNewWrd}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
# we can't get this through the CRAN test - run it with copy/paste to console

wrd <- GetNewWrd()
ToWrd("This is centered Text in Arial Black\n",
      para=list(Alignment=wdConst$wdAlignParagraphCenter,
                SpaceBefore=3, SpaceAfter=6),
      font=list(name="Arial Black", size=14),
      wrd=wrd)

sel <- wrd$Selection()$Borders(wdConst$wdBorderBottom)
sel[["LineStyle"]] <- wdConst$wdLineStyleSingle


t1 <- TOne(x = d.pizza[, c("temperature","delivery_min","driver","wine_ordered")],
           grp=d.pizza$wine_delivered)

ToWrd(t1, font=list(name="Algerian"), wrd=wrd)


tab <- table(d.pizza$driver, d.pizza$area)

tab <- table(d.pizza$driver, d.pizza$area)
ToWrd(tab, font = list(size=15, name="Arial"), row.names = TRUE, col.names = TRUE,
      main= "my Title", wrd=wrd)
ToWrd(tab, font = list(size=10, name="Arial narrow"),
      row.names = TRUE, col.names=FALSE, wrd=wrd)
ToWrd(tab, font = list(size=15, name="Arial"), align="r",
      row.names = FALSE, col.names=TRUE, wrd=wrd)
ToWrd(tab, font = list(size=15, name="Arial"),
      row.names = FALSE, col.names=FALSE, wrd=wrd)

ToWrd(tab, tablestyle = "Mittlere Schattierung 2 - Akzent 4",
      row.names=TRUE, col.names=TRUE, wrd=wrd)

ToWrd(Format(tab, big.mark = "'", digits=0), wrd=wrd)

zz <- ToWrd(Format(tab, big.mark = "'", digits=0), wrd=wrd)
zz$Rows(1)$Select()
WrdFont(wrd = wrd) <- list(name="Algerian", size=14, bold=TRUE)


# Send a TMod table to Word using a split to separate columns
r.ols <- lm(Fertility ~ . , swiss)
r.gam <- glm(Fertility ~ . , swiss, family=Gamma(link="identity"))

# Build the model table for some two models, creating a user defined
# reporting function (FUN) with | as column splitter
tm <- TMod(OLS=r.ols, Gamma=r.gam, 
           FUN=function(est, se, tval, pval, lci, uci){
              gettextf("\%s|[\%s, \%s]|\%s",
                       Format(est, fmt=Fmt("num"), digits=2),
                       Format(lci, fmt=Fmt("num"), digits=2), 
                       Format(uci, fmt=Fmt("num"), digits=2),
                       Format(pval, fmt="*")
              )})

# send it to Word, where we get a table with 3 columns per model
# coef | confint | p-val
wrd <- GetNewWrd()
ToWrd(tm, split="|", align=StrSplit("lrclrcl"))
)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
% use one of  RShowDoc("KEYWORDS")
