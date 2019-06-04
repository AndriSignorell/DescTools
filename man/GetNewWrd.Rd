\name{GetNewWrd}
\alias{GetNewWrd}
\alias{WrdKill}
\alias{createCOMReference}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a New Word Instance
%%  ~~function to do ... ~~
}
\description{Start a new instance of Word and return its handle.
By means of this handle we can then control the word application. \cr
\code{WrdKill} ends a running MS-Word task.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GetNewWrd(visible = TRUE, template = "Normal", header = FALSE,
          main = "Descriptive report")

WrdKill()


}

% createCOMReference(ref, className)

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{visible}{logical, should Word made visible? Defaults to \code{TRUE}.
%%     ~~Describe \code{visible} here~~
}
  \item{template}{the name of the template to be used for creating a new document.
%%     ~~Describe \code{template} here~~
}
  \item{header}{logical, should a caption and a list of contents be inserted? Default is \code{FALSE}.
%%     ~~Describe \code{header} here~~
}
  \item{main}{the main title of the report
%%     ~~Describe \code{main} here~~
}
% \item{ref}{the S object that is an external pointer containing the reference to the COM object}
% \item{className}{the name of the class that is "suggested" by the caller}

}
\details{The package \bold{RDCOMClient} reveals the whole VBA-world of MS-Word. So generally speaking any VBA code can be run fully controlled by R. In practise, it might be a good idea to record a macro and rewrite the VB-code in R.\cr

Here's a list of some frequently used commands. Let's assume we have a handle to the application  and a handle to the current selection defined as:
\preformatted{
wrd <- GetNewWrd()
sel <- wrd$Selection()
}
Then we can access the most common properties as follows:
\tabular{ll}{
new document \tab \code{wrd[["Documents"]]$Add(template, FALSE, 0)}, template is the templatename. \cr
open document \tab \code{wrd[["Documents"]]$Open(Filename="C:/MyPath/MyDocument.docx")}. \cr
save document \tab \code{wrd$ActiveDocument()$SaveAs2(FileName="P:/MyFile.docx")} \cr
quit word \tab \code{wrd$quit()} \cr
kill word task \tab \code{WrdKill} kills a running word task (which might not be ended with quit.) \cr
normal text \tab Use \code{\link{ToWrd}} which offers many arguments as fontname, size, color, alignment etc. \cr
 \tab  \code{ToWrd("Lorem ipsum dolor sit amet, consetetur", }\cr
 \tab    \code{font=list(name="Arial", size=10, col=wdConst$wdColorRed)} \cr
simple text \tab \code{sel$TypeText("sed diam nonumy eirmod tempor invidunt ut labore")} \cr
heading \tab  \code{WrdCaption("My Word-Story", index=1)} \cr
insert R output \tab \code{ToWrd(capture.output(str(d.diamonds)))} \cr
pagebreak \tab \code{sel$InsertBreak(wdConst$wdPageBreak)} \cr
sectionbreak \tab \code{sel$InsertBreak(wdConst$wdSectionBreakContinuous)} \cr\tab (\code{wdSectionBreakNextPage}) \cr
move cursor right \tab \code{sel$MoveRight(Unit=wdConst$wdCharacter, Count=2, Extend=wdConst$wdExtend)} \cr
goto end \tab \code{sel$EndKey(Unit=wdConst$wdStory)} \cr
pagesetup \tab \code{sel[["PageSetup"]][["Bottommargin"]] <- 4 * 72} \cr
orientation \tab \code{sel[["PageSetup"]][["Orientation"]] <- wdConst$wdOrientLandscape} \cr
add bookmark \tab \code{wrd[["ActiveDocument"]][["Bookmarks"]]$Add("myBookmark")} \cr
goto bookmark \tab \code{sel$GoTo(wdConst$wdGoToBookmark, 0, 0, "myBookmark")} \cr
update bookmark \tab \code{WrdUpdateBookmark("myBookmark", "New text for my bookmark")} \cr
show document map \tab \code{ wrd[["ActiveWindow"]][["DocumentMap"]] <- TRUE} \cr
create table \tab  \code{\link{WrdTable}}() which allows to define the table's geometry \cr
insert caption \tab \code{sel$InsertCaption(Label="Abbildung", TitleAutoText="InsertCaption",}\cr
\tab \code{Title="My Title")} \cr
tables of figures \tab \code{wrd$ActiveDocument()$TablesOfFigures()$Add(Range=sel$range(),}\cr
\tab \code{Caption="Figure")} \cr
insert header
  \tab \code{wview <- wrd[["ActiveWindow"]][["ActivePane"]][["View"]][["SeekView"]] }\cr
  \tab \code{wview <- ifelse(header, wdConst$wdSeekCurrentPageHeader, wdConst$wdSeekCurrentPageFooter) }\cr
  \tab \code{ToWrd(x, ..., wrd=wrd) }\cr

}

%s \code{createCOMReference()} is just a wrapper for \code{RDCOMClient::createCOMReference}, as the function is not visible, if \bold{RDCOMClient} is only used by required namespace.

}
\note{
Note that the list of contents has to be refreshed by hand after inserting text (if inserted by \code{header = TRUE}).
%%  ~~ If necessary, more details than the description above ~~
}
\value{a handle (pointer) to the created Word instance.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Andri Signorell <andri@signorell.net>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{GetNewXL}}, \code{\link{GetNewPP}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{ # Windows-specific example

wrd <- GetNewWrd()
Desc(d.pizza[,1:4], wrd=wrd)

wrd <- GetNewWrd(header=TRUE)
Desc(d.pizza[,1:4], wrd=wrd)

# enumerate all bookmarks in active document
for(i in 1:wrd[["ActiveDocument"]][["Bookmarks"]]$count()){
  print(wrd[["ActiveDocument"]][["Bookmarks"]]$Item(i)$Name())
}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }

