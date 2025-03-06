\name{Format}
\alias{Format}
\alias{Format.default}
\alias{Format.matrix}
\alias{Format.table}
\alias{Fmt}
\alias{as.fmt}
\alias{as.CDateFmt}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Format Numbers and Dates
%%  ~~function to do ... ~~
}
\description{Formatting numbers with base R tools often degenerates into a major intellectual challenge for us little minds down here in the valley of tears. There are a number of options available and quite often it's hard to work out which one to use, when a more uncommon setting is needed.
The \code{Format()} function wraps all these functions and tries to offer a simpler, less technical, but still flexible interface.

There's also an easygoing interface for format templates, defined as a list consisting of any accepted format features. This enables to define templates globally and easily change or modify them later.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Format(x, digits = NULL, sci = NULL, big.mark = NULL,
       ldigits = NULL, zero.form = NULL, na.form = NULL,
       fmt = NULL, align = NULL, width = NULL, lang = NULL, 
       eps = NULL, ...)

\method{Format}{table}(x, digits = NULL, sci = NULL, big.mark = NULL,
       ldigits = NULL, zero.form = NULL, na.form = NULL,
       fmt = NULL, align = NULL, width = NULL, lang = NULL, 
       eps = NULL, ...)

\method{Format}{matrix}(x, digits = NULL, sci = NULL, big.mark = NULL,
       ldigits = NULL, zero.form = NULL, na.form = NULL,
       fmt = NULL, align = NULL, width = NULL, lang = NULL, 
       eps = NULL, ...)

\method{Format}{default}(x, digits = NULL, sci = NULL, big.mark = NULL,
       ldigits = NULL, zero.form = NULL, na.form = NULL,
       fmt = NULL, align = NULL, width = NULL, lang = NULL, 
       eps = NULL, ...)

Fmt(...)

as.fmt(...)

as.CDateFmt(fmt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an atomic numerical, typically a vector of real numbers or a matrix of numerical values. Factors will be converted to strings.
%%     ~~Describe \code{x} here~~
}
  \item{digits}{integer, the desired (fixed) number of digits after the decimal point. Unlike \code{\link{formatC}} you will always get this number of digits even if the last digit is 0. 
  Negative numbers of digits round to a power of ten (\code{digits=-2} would round to the nearest hundred).
%%     ~~Describe \code{digits} here~~
}
  \item{sci}{integer. The power of 10 to be set when deciding to print numeric values in exponential notation. Fixed notation will be preferred unless the number is larger than 10^scipen. If just one value is set it will be used for the left border 10^(-scipen) as well as for the right one (10^scipen). A negative and a positive value can also be set independently. Default is
  \code{getOption("scipen")}, whereas \code{scipen=0} is overridden.
%%     ~~Describe \code{sci} here~~
}
  \item{big.mark}{character; if not empty used as mark between every 3 decimals before the decimal point. Default is "" (none).
%%     ~~Describe \code{big.mark} here~~  big.interval is not implemented, would anyone need that??
}
  \item{ldigits}{number of leading zeros. \code{ldigits=3} would make sure that at least 3 digits on the left side will be printed, say \code{3.4} will be printed as \code{003.4}. Setting \code{ldigits} to \code{0} will yield results like \code{.452} for \code{0.452}. The default \code{NULL} will leave the numbers as they are (meaning at least one 0 digit).
%%     ~~Describe \code{drop0leading} here~~
}
  \item{zero.form}{character, string specifying how zeros should be specially formatted. Useful for pretty printing 'sparse' objects.
  If set to \code{NULL} (default) no special action will be taken.
%%     ~~Describe \code{pad} here~~
}
  \item{na.form}{character, string specifying how \code{NA}s should be specially formatted.
  If set to \code{NULL} (default) no special action will be taken.
%%     ~~Describe \code{pad} here~~
}
  \item{fmt}{either a format string, allowing to flexibly define special formats or an object of class \code{fmt}, consisting of a list of \code{Format} arguments. See Details.
%%     ~~Describe \code{fmt} here~~
}
\item{align}{ the character on whose position the strings will be aligned. Left alignment can be requested by setting \code{sep = "\\\\l"}, right alignment by \code{"\\\\r"} and center alignment by \code{"\\\\c"}. Mind the backslashes, as if they are omitted, strings would be aligned to the \bold{character} l, r or c respectively. The default is \code{NULL} which would just leave the strings as they are.\cr
This argument is send directly to the function \code{\link{StrAlign}()} as argument \code{sep}.
%%     ~~Describe \code{sep} here~~
}

  \item{width}{integer, the defined fixed width of the strings.
%%     ~~Describe \code{width} here~~
}
  \item{lang}{optional value setting the language for the months and daynames. Can be either \code{"local"} for current locale or \code{"engl"} for english. If left to \code{NULL}, the DescToolsOption \code{"lang"} will be searched for and if not found \code{"local"} will be taken as default.
%%     ~~Describe \code{value} here~~
}
\item{eps}{a numerical tolerance used mainly for formatting p values, those less than eps are formatted as "\code{< [eps]}" (where '[eps]' stands for \code{format(eps, digits))}. Default is \code{.Machine$double.eps}.}
% \item{value}{the value to be overridden in the function \code{fmt}}.
% \item{name}{the name of the format in \code{fmt}.}
\item{\dots}{further arguments to be passed to or from methods. }

}
\details{
\code{Format()} is the workhorse here and formats numbers and dates. \cr\cr
The argument \code{fmt} is very flexible and is used to generate a variety of different formats. When \code{x} is a date, it can take ISO-8601-date-and-time-format codes consisting of (\code{d}, \code{m} and \code{y} for day, month or year) and defining the combination of day month and year representation. Repeating the specific code defines the degree of abbreviation. The format \code{'yyyy-mm-dd'} would yield a date as \code{2020-10-12}.

\tabular{ll}{
\bold{Date Codes} \tab  \cr
  \code{d } \tab  day of the month without leading zero (1 - 31) \cr
  \code{dd} \tab  day of the month with leading zero (01 - 31)\cr
  \code{ddd} \tab abbreviated name for the day of the week (e.g. Mon) in the current user's language  \cr
                  \code{dddd} \tab    full name for the day of the week (e.g. Monday) in the current user's language  \cr
                  \code{m } \tab  month without leading zero (1 - 12) \cr
   \code{mm} \tab    month with leading zero (01 - 12) \cr
                 \code{mmm }  \tab   abbreviated month name (e.g. Jan) in the current user's language \cr
                \code{mmmm}  \tab    full month name (e.g. January) in the current user's language \cr
            \code{y } \tab   year without century, without leading zero (0 - 99) \cr
                 \code{yy } \tab      year without century, with leading zero (00 - 99) \cr
            \code{yyyy }  \tab   year with century. For example: 2005 \cr
\cr
}

The function \code{as.CDateFmt()} converts ISO-8601 codes into the C-format codes used in base R.\cr So
\code{as.CDateFmt("yyyy mm dd")} yields \code{ "\%Y \%m \%d"}.

Even more variability is needed to display numeric values. For the most frequently used formats there are the following special codes available:
\tabular{lll}{
\bold{Code} \tab  \tab  \cr
\code{e} \tab scientific \tab  forces scientific representation of x, e.g. 3.141e-05. The number of digits,\cr 
\tab \tab alignment and zero values are further respected.\cr
\tab\cr

\code{eng} \tab engineering \tab  forces scientific representation of \code{x}, but only with powers that are a multiple of 3. \cr
\code{engabb} \tab engineering abbr. \tab  same as \code{eng}, but replaces the exponential representation by codes, \cr \tab\tab e.g. \code{M} for mega (1e6). See \code{\link{d.prefix}}.\cr

\code{\%} \tab percent \tab will divide the given number by 100 and append the \%-sign (without a separator).\cr
\tab\cr
\code{p} \tab p-value \tab will wrap the function \code{\link{format.pval}} and return a p-value format. \cr \tab \tab Use \code{eps} to define the threshold to switch to a \code{ < 000 } representation.\cr
\tab\cr
\code{frac} \tab fractions \tab will (try to) convert numbers to fractions. So 0.1 will be displayed as 1/10. \cr \tab\tab See \code{\link[MASS]{fractions}()}.\cr
\tab\cr

\code{*} \tab significance \tab will produce a significance representation of a p-value consisting of *  and .,  \cr
\tab \tab while the breaks are set according to the used defaults e.g. in \code{lm} as  \cr
\tab \tab [0, 0.001] = \code{***}  \cr
\tab \tab (0.001, 0.01] = \code{**}  \cr
\tab \tab (0.01, 0.05] = \code{*}  \cr
\tab \tab (0.05, 0.1] = \code{.}  \cr
\tab \tab (0.1,1] = \code{ }\cr

\code{p*}\tab p-value stars\tab will produce p-value and significance stars

}

\code{fmt} can as well be an object of class \code{fmt} consisting of a list out of the arguments above.
This allows to store and manage the full format in variables or as options (in \code{DescToolsOptions()}) and use it as format template subsequently.  

Finally \code{fmt} can also be a function in x, which makes formatting very flexible.

New formats can be created by means of \code{as.fmt()}. This works quite straight on. We can use any of the arguments from \code{Format()} and combine them to a list. \cr
The following code will define a new format template named "\code{myNumFmt}" of the class \code{"fmt"}. Provided to \code{Format()} this will result in a number displayed with 2 fixed digits and a comma as big mark:
\preformatted{myNumFmt <- as.fmt(digits=2, big.mark=",")
Format(12222.89345, fmt=myNumFmt) = 12,222.89}
The latter returns the same result as if the arguments would have been supplied directly: \cr
\code{Format(12222.89345, digits=2, big.mark=",")}.

Many report functions (e.g. \code{\link{TOne}()}) in \bold{DescTools} use three default formats for counts (named \code{"abs"}), numeric values (\code{"num"}) and percentages (\code{"per"}). These formats can be set by the user as options (see \code{\link{DescToolsOptions}()}. For other purposes any number of any named formats can be defined. 

\code{Fmt()} is used to access and edit already defined Formats. It can directly adapt defined properties and returns the format template. \code{Fmt("num", digits=1, sci=10)} will use the current version of the numeric format and change the digits to 1 and the threshold to switch to scientifc presentation to numbers >1e10 and <1e-10.
Format templates can be altered using their names. With \code{Fmt(abs=Fmt("abs", big.mark=" "))} the format template for count values \code{"abs"} will be overwritten with the new values and stored as option for the current session.

The formats can as well be organized as options. \code{DescToolsOptions("fmt")} would display the currently defined formats. This mechanic works analogously to the \code{options()} procedure of base R. So to store the current settings we can use 
\preformatted{opt <- DescToolsOptions("fmt")
... do some stuff like redefining the global formats ...
DescToolOptions(opt)  }
The last command resets the options and so we have again the initial definitions for the format templates.

}
\value{the formatted values as characters. \cr
If \code{x} was a \code{matrix}, then a the result will also be a \code{matrix}. (Hope this will not surprise you...)

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{format}}, \code{\link{formatC}}, \code{\link{prettyNum}}, \code{\link{sprintf}}, \code{\link{symnum}},\cr
\code{\link{StrAlign}}, \code{\link{StrPad}}, \code{\link{Sys.setlocale}},\cr
\code{\link{Weekday}}, \code{\link{Month}},
\code{\link{DescToolsOptions}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Format(as.Date(c("2014-11-28", "2014-1-2")), fmt="ddd, d mmmm yyyy")
Format(as.Date(c("2014-11-28", "2014-1-2")), fmt="ddd, d mmmm yyyy", lang="engl")

x <- pi * 10^(-10:10)

Format(x, digits=3, fmt="\%", sci=NA)
Format(x, digits=4, sci=c(4, 6), ldigits=0, width=9, align=".")


# format a matrix
m <- matrix(runif(100), nrow=10,
            dimnames=list(LETTERS[1:10], LETTERS[1:10]))

Format(m, digits=1)

# engineering format
Format(x, fmt="eng",  digits=2)
Format(x, fmt="engabb", ldigits=2, digits=2)
# combine with grams [g]
paste(Format(x, fmt="engabb", ldigits=2, digits=2), "g", sep="")

# example form symnum
pval <- rev(sort(c(outer(1:6, 10^-(1:3)))))
noquote(cbind(Format(pval, fmt="p"), Format(pval, fmt="*")))

# special formats, e.g. confidence intervals
Format(c(3.2, 4.5, 6.7)/100, fmt="ci\%", digits=2, conf.level=0.90)

# use Fmt() to get and define new formats stored as option
Fmt()                        # all defined formats
Fmt("abs")                   # only format named "abs"
Fmt("nexist")                # only format named "nexist" (nonexisting)
Fmt("abs", "per", "nexist")
Fmt("abs", digits=3)         # get Fmt("abs") and overwrite digits
Fmt("abs", na.form="-")      # get Fmt("abs") and add user defined na.form

# define totally new format and store as option
Fmt(nob=as.fmt(digits=10, na.form="nodat"))

# overwrite an existing format
Fmt(nob=Fmt("nob", digits=5))
Fmt("nob")

# change the character to be used as the decimal point
opt <- options(OutDec=",")
Format(1200, digits=2, big.mark = ".")
options(opt)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
