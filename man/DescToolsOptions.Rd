\name{DescToolsOptions}
\alias{DescToolsOptions}
\title{DescTools Options}
\description{Get and set a variety of options which affect the way in which DescTools functions display results.}
\usage{DescToolsOptions(..., default = NULL, reset = FALSE)
}
\arguments{
  \item{\dots}{any options can be defined, using \code{name = value}. However, only the ones below are used by DescTools functions.
}
  \item{default}{if the specified option is not set in the options list, this value is returned. This facilitates retrieving an option and checking whether it is set and setting it separately if not.}

 \item{reset}{logical. If this is set to \code{TRUE}, the options will be overwritten with their default values. Other arguments will be ignored in this case. Default is \code{FALSE}.}
}


\details{
Invoking \code{DescToolsOptions()} with no arguments returns a list with the current values of the options. Note that not all options listed below are set initially. To access the value of a single option, one can simply use \code{DescToolsOptions("plotit")}.\cr
To set a new value use the same rationale as with the R options:  \code{DescToolsOptions(plotit=FALSE)}



\bold{Options used by DescTools}
  \describe{
    \item{\code{col}:}{a vector of colours, defined as names or as RGB-longs (\code{"#RRGGBB"}). By now three colors are used in several plots as defaults. By default they're set to \code{hblue}, \code{hred} and \code{horange}. Change the values by defining \code{DescToolsOptions(col=c("pink", "blue", "yellow"))}. Any color definition can be used here.}

    \item{\code{digits}:}{the number of \bold{FIXED} digits, used throughout the print functions.}

    \item{\code{fixedfont}:}{this font will be used by default, when \code{Desc} writes
     to a Word document. Must be defined as a font object, say enumerating \code{name}, \code{face} and \code{size} of the font and setting the class \code{font}, e.g. \code{structure(list(name="Courier New", size=7), class="font")}.
     }


    \item{\code{fmt}:}{Three number format definitions are currently used in the \code{Desc} routines. The format used for integer values is named \code{"abs"}, for percentages \code{"perc"} and for floating point numeric values \code{"num"}.
    The format definitions must be of class \code{"fmt"} and may contain any argument used in the function \code{\link{Format}}.\cr
    Use \code{\link{Fmt}} to access and update formats (as they are organised in a nested list).}

    \item{\code{footnote}:}{a character vector, containing characters to be used as footnote signs.
    Any character can be defined here. This is currently used by \code{\link{TOne}}.}

    \item{\code{lang}:}{either \code{"engl"} or \code{"local"}, defining the language to be used for the names of weekdays and months when using \code{\link{Format}}.}

    \item{\code{plotit}:}{logical, defining whether the \code{Desc}-procedures should produce plots by default. This is usually a good thing, but it may clutter up your desktop, if you're not using RStudio. Therefore it can be turned off.}

    \item{\code{stamp}:}{text or expression to be placed in the right bottom corner of the \code{DescTools} plots. This can be useful, if some author or date information should automatically be inserted by default. Any text can be set as option, but also dynamic expressions can be used. The default would use an expression as <username>/<date>, which will use the username from the system and the current date. See defaults below. }

}

Calling \code{DescToolsOptions(reset=TRUE)} will reset the options to these defaults:
\preformatted{
options(DescTools = list(
  col       = c(hblue="#8296C4", hred="#9A0941", horange="#F08100"),
  digits    = 3,
  fixedfont = structure(list(name = "Consolas", size = 7), class = "font"),
  fmt       = list(abs = structure(list(digits = 0, big.mark = "'"),
                     name = "abs", label = "Number format for counts", default = TRUE,
                     class = "fmt"),
                   per = structure(list(digits = 1, fmt = "\%"),
                     name = "per", label = "Percentage number format", default = TRUE,
                     class = "fmt"),
                   num = structure(list(digits = 3, big.mark = "'"),
                     name = "num", label = "Number format for floats", default = TRUE,
                     class = "fmt")
              ),
  footnote  = c("'", "\"", "\"\""),
  lang      = "engl",
  plotit    = TRUE,
  stamp     = expression(gettextf("\%s/\%s", Sys.getenv("USERNAME"),
                                  Format(Today(), fmt = "yyyy-mm-dd")))
))
}

This code can as well be copied and pasted to the users' \code{RProfile} file, in order to have the options permanently available.

}
\value{

For a given vector of strings the current value set for option \code{x}, or \code{NULL} if the option is unset.

If called with no arguments, returns all option settings in a list. Otherwise, it changes the named settings and invisibly returns their previous values.
}

\seealso{\code{\link{Format}}, \code{\link{Pal}}}

\author{Andri Signorell <andri@signorell.net>}

\examples{
DescToolsOptions("plotit")

\dontrun{

# Get all options, defaults are attributed as such
DescToolsOptions()

# get some options
DescToolsOptions("plotit", "lang")

# get some potentially undefined option, while taking a user default and
# overriding system defaults
DescToolsOptions("stamp", default="Condor, 2016")

# get an undefined option, should return default
DescToolsOptions("stampede", default="Condor, 2016")

# set options, while getting the old values
opt <- DescToolsOptions(plotit=789, lang="portugues")
DescToolsOptions()
# output the old values
opt

# just a single argument
DescToolsOptions(digits=2)

# reset the old values
DescToolsOptions(opt)
DescToolsOptions()

# reset factory defaults
DescToolsOptions(reset=TRUE)
}}









