\name{WrdParagraphFormat}
\alias{WrdParagraphFormat}
\alias{WrdParagraphFormat<-}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get or Set the Paragraph Format in Word
%%  ~~function to do ... ~~
}
\description{\code{WrdParagraphFormat} can be used to get and set the font in Word for the text to be inserted.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WrdParagraphFormat(wrd = DescToolsOptions("lastWord"))
WrdParagraphFormat(wrd) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{value}{
  a list defining the paragraph format.
  This can contain any combination of:
    \code{LeftIndent}, \code{RightIndent}, \code{SpaceBefore}, \code{SpaceBeforeAuto}, \code{SpaceAfter}, \code{SpaceAfterAuto},
  \code{LineSpacingRule}, \code{Alignment}, \code{WidowControl}, \code{KeepWithNext}, \code{KeepTogether}, \code{PageBreakBefore},
  \code{NoLineNumber}, \code{Hyphenation}, \code{FirstLineIndent}, \code{OutlineLevel}, \code{CharacterUnitLeftIndent},
  \code{CharacterUnitRightIndent}, \code{CharacterUnitFirstLineIndent}, \code{LineUnitBefore}, \code{LineUnitAfter} and/or \code{MirrorIndents}.
  The possible values of the arguments are found in the Word constants with the respective name. \cr The alignment for example can be set to \code{wdAlignParagraphLeft}, \code{wdAlignParagraphRight}, \code{wdAlignParagraphCenter} and so on.
  \cr Left alignment with indentation would be set as:\cr \code{list(Alignment=wdConst$wdAlignParagraphLeft, LeftIndent=42.55)}.
%%     ~~Describe \code{font} here~~

}
  \item{wrd}{the pointer to a word instance. Can be a new one, created by \code{GetNewWrd()}
  or an existing one, created by \code{GetCurrWrd()}.
  Default is the last created pointer stored in \code{DescToolsOptions("lastWord")}.}

}
\value{
an object with the class \code{paragraph}, basically a list with the attributes of the paragraph in the current cursor position:
  \item{LeftIndent}{left indentation in (in points) for the specified paragraphs.}
  \item{RightIndent}{right indent (in points) for the specified paragraphs.}
  \item{SpaceBefore}{spacing (in points) before the specified paragraphs.}
  \item{SpaceBeforeAuto}{\code{TRUE} if Microsoft Word automatically sets the amount of spacing before the specified paragraphs.}
  \item{SpaceAfter}{amount of spacing (in points) after the specified paragraph or text column.}
    \item{SpaceAfterAuto}{\code{TRUE} if Microsoft Word automatically sets the amount of spacing after the specified paragraphs.}
  \item{LineSpacingRule}{line spacing for the specified paragraph formatting. Use \code{wdLineSpaceSingle}, \code{wdLineSpace1pt5}, or \code{wdLineSpaceDouble} to set the line spacing to one of these values. To set the line spacing to an exact number of points or to a multiple number of lines, you must also set the \code{LineSpacing} property.}
  \item{Alignment}{\code{WdParagraphAlignment} constant that represents the alignment for the specified paragraphs.}
  \item{WidowControl}{\code{TRUE} if the first and last lines in the specified paragraph remain on the same page as the rest of the paragraph when Word repaginates the document. Can be \code{TRUE}, \code{FALSE} or \code{wdUndefined}.}
  \item{KeepWithNext}{\code{TRUE} if the specified paragraph remains on the same page as the paragraph that follows it when Microsoft Word repaginates the document. Read/write Long.}
  \item{KeepTogether}{\code{TRUE} if all lines in the specified paragraphs remain on the same page when Microsoft Word repaginates the document.}
  \item{PageBreakBefore}{\code{TRUE} if a page break is forced before the specified paragraphs. Can be \code{TRUE}, \code{FALSE}, or \code{wdUndefined}.}
  \item{NoLineNumber}{\code{TRUE} if line numbers are repressed for the specified paragraphs. Can be \code{TRUE}, \code{FALSE}, or \code{wdUndefined}.}
  \item{Hyphenation}{\code{TRUE} if the specified paragraphs are included in automatic hyphenation. \code{FALSE} if the specified paragraphs are to be excluded from automatic hyphenation.}
  \item{FirstLineIndent}{value (in points) for a first line or hanging indent. Use a positive value to set a first-line indent, and use a negative value to set a hanging indent.}
  \item{OutlineLevel}{outline level for the specified paragraphs.}
  \item{CharacterUnitLeftIndent}{left indent value (in characters) for the specified paragraphs.}
  \item{CharacterUnitRightIndent}{right indent value (in characters) for the specified paragraphs. }
  \item{LineUnitBefore}{amount of spacing (in gridlines) before the specified paragraphs. }
  \item{LineUnitAfter}{amount of spacing (in gridlines) after the specified paragraphs.}
  \item{MirrorIndents}{Long that represents whether left and right indents are the same width. Can be \code{TRUE}, \code{FALSE}, or \code{wdUndefined}.}

}
\author{
Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{ToWrd}}, \code{\link{WrdPlot}}, \code{\link{GetNewWrd}}, \code{\link{GetCurrWrd}}
}
\examples{
\dontrun{
# Windows-specific example
wrd <- GetNewWrd()  # get the handle to a new word instance

WrdParagraphFormat(wrd=wrd) <- list(Alignment=wdConst$wdAlignParagraphLeft,
                                    LeftIndent=42.55)

ToWrd("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy
eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.
At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd
gubergren, no sea takimata sanctus est.\n", wrd=wrd)

# reset
WrdParagraphFormat(wrd=wrd) <- list(LeftIndent=0)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
\keyword{MS-Office}
