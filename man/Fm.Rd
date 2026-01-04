\name{Fm}
\alias{Fm}
\alias{Fm.default}
\alias{Fm.matrix}
\alias{Fm.table}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Format Numbers and Dates
%%  ~~function to do ... ~~
}
\description{Formatting numbers with base R tools often degenerates into a major intellectual challenge for us little minds down here in the valley of tears. There are a number of options available and quite often it's hard to work out which one to use, when a more uncommon setting is needed.
The \code{Fm()} function wraps all these functions and tries to offer a simpler, less technical, but still flexible interface.

There's also an easygoing interface for format templates, defined as a list consisting of any accepted format features. This enables to define templates globally and easily change or modify them later.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Fm(x, digits = NULL, ldigits = NULL, sci = NULL, 
   big.mark=NULL, outdec = NULL, na.form = NULL, zero.form = NULL, 
   fmt = NULL, p_eps = NULL, width = NULL, align = NULL, lang = NULL, ...)

\method{Fm}{table}(x, digits = NULL, ldigits = NULL, sci = NULL, 
       big.mark=NULL, outdec = NULL, na.form = NULL, zero.form = NULL, 
       fmt = NULL, p_eps = NULL, width = NULL, align = NULL, lang = NULL, ...)

\method{Fm}{matrix}(x, digits = NULL, ldigits = NULL, sci = NULL, 
       big.mark=NULL, outdec = NULL, na.form = NULL, zero.form = NULL, 
       fmt = NULL, p_eps = NULL, width = NULL, align = NULL, lang = NULL, ...)

\method{Fm}{default}(x, digits = NULL, ldigits = NULL, sci = NULL, 
       big.mark=NULL, outdec = NULL, na.form = NULL, zero.form = NULL, 
       fmt = NULL, p_eps = NULL, width = NULL, align = NULL, lang = NULL, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an atomic numerical, typically a vector of real numbers or a matrix of numerical values. Factors will be converted to strings.
%%     ~~Describe \code{x} here~~
}

  \item{digits}{integer, the desired (fixed) number of digits after the decimal point. Unlike \code{\link{formatC}} you will always get this number of digits even if the last digit is 0. 
  Negative numbers of digits round to a power of ten (\code{digits=-2} would round to the nearest hundred).
}
  \item{ldigits}{number of leading zeros. \code{ldigits=3} would make sure that at least 3 digits on the left side will be printed, say \code{3.4} will be printed as \code{003.4}. Setting \code{ldigits} to \code{0} will yield results like \code{.452} for \code{0.452}. The default \code{NULL} will leave the numbers as they are (meaning at least one 0 digit).
}
  \item{sci}{integer. The power of 10 to be set when deciding to print numeric values in exponential notation. Fixed notation will be preferred unless the number is larger than 10^scipen. If just one value is set it will be used for the left border 10^(-scipen) as well as for the right one (10^scipen). A negative and a positive value can also be set independently. Default is
  \code{getOption("scipen")}, whereas \code{scipen=0} is overridden.
}
  \item{big.mark}{character; if not empty used as mark between every 3 decimals before the decimal point. Default is "" (none).
}
\item{outdec}{character, specifying the decimal mark to be used. If not provided, the 
default set as \code{OutDec} option is used.}

  \item{na.form}{character, string specifying how \code{NA}s should be specially formatted.
  If set to \code{NULL} (default) no special action will be taken.
}
  \item{zero.form}{character, string specifying how zeros should be specially formatted. Useful for pretty printing 'sparse' objects.
  If set to \code{NULL} (default) no special action will be taken.
}

  \item{fmt}{either a format string, allowing to flexibly define special formats 
  or an object of class \code{style}, consisting of a list of \code{Fm} arguments. See Details.
}
\item{p_eps}{a numerical tolerance used mainly for formatting p values, those 
less than p_eps are formatted as "\code{< [p_eps]}" (where '[p_eps]' stands for \code{format(p_eps, digits))}. 
Default is \code{0.001}.}

  \item{width}{integer, the defined fixed width of the strings.
}

\item{align}{ the character on whose position the strings will be aligned. Left alignment can be requested by setting \code{sep = "\\\\l"}, right alignment by \code{"\\\\r"} and center alignment by \code{"\\\\c"}. Mind the backslashes, as if they are omitted, strings would be aligned to the \bold{character} l, r or c respectively. The default is \code{NULL} which would just leave the strings as they are.\cr
This argument is send directly to the function \code{\link{StrAlign}()} as argument \code{sep}.
}

  \item{lang}{optional value setting the language for the months and daynames. Can be either 
  \code{"local"} for current locale or \code{"en"} for english. 
  If left to \code{NULL}, the DescToolsOption \code{"lang"} will be 
  searched for and if not found \code{"local"} will be taken as default.
}


\item{\dots}{further arguments to be passed to or from methods. }


}
\details{
\code{Fm()} is the workhorse for formatting numbers and dates, supporting 
a comprehensive range of format options that are likely to occur 
in everyday reporting. Among these, the argument \code{fmt} deserves a more detailed description due to its flexibility. It is used to generate a variety of 
different special formats. \cr\cr If \code{x} is a date, it can take 
ISO-8601–inspired token syntax similar to .NET or Moment.js (consisting 
of \code{d}, \code{M} and \code{y} for day, month or year and \code{h/H}, \code{m},  \code{s}, \code{t}
for hours, minutes, seconds and AM/PM designator) and defining 
the combination of day month and year representation.\cr 


\tabular{ll}{
\bold{Code\verb{  }} \tab  \bold{Description}\cr
  \code{d } \tab  day of the month without leading zero (1 - 31) \cr
  \code{dd} \tab  day of the month with leading zero (01 - 31)\cr
  \code{ddd} \tab abbreviated name for the day of the week (e.g. Mon) in the current user's language  \cr
  \code{dddd} \tab    full name for the day of the week (e.g. Monday) in the current user's language  \cr
  \code{do}  \tab The token \code{do} (aka 'day ordinal') formats the day of month using English ordinal
suffixes (e.g. 1st, 2nd, 3rd, 4th).
This is an English-only feature. (For most other languages,
ordinal dates are written using punctuation in the format string,
e.g. \code{"d. MMM yyyy"}.
Locale-specific ordinal rules beyond English are not implemented by design.)\cr
  \code{M } \tab  month without leading zero (1 - 12) \cr
  \code{MM} \tab    month with leading zero (01 - 12) \cr
  \code{MMM }  \tab   abbreviated month name (e.g. Jan) in the current user's language \cr
  \code{MMMM}  \tab    full month name (e.g. January) in the current user's language \cr
  \code{y } \tab   year without century, without leading zero (0 - 99) \cr
  \code{yy } \tab      year without century, with leading zero (00 - 99) \cr
  \code{yyyy }  \tab   year with century. For example: 2005 \cr\cr
  \code{H/HH }  \tab   Hour in 24h format, one digit / two digits \cr
  \code{h/hh }  \tab   Hour in 12h format, one digit / two digits, note that in this case t must be set also to ensure uniqueness.\cr
  \code{t/tt }  \tab   AM/PM description (one/two characters)\cr
  \code{m/mm }  \tab   Minutes one digit / two digits\cr
  \code{s/ss }  \tab   Seconds one digit / two digits\cr

\cr
}
Weekdays and month names can be expressed in the local language or in English. The language can be controlled by
the argument "\code{lang}".\cr



Even more variability is needed to display numeric values. For the most frequently used formats there are the following special codes available:
\tabular{lll}{
\bold{Code} \tab  \bold{Type} \tab \bold{Description}  \cr
\code{e} \tab scientific \tab  forces scientific representation of x, e.g. 3.141e-05. The number of digits,\cr 
\tab \tab alignment and zero values are further respected.\cr
\tab\cr

\code{eng} \tab engineering \tab  forces scientific representation of \code{x}, but only with powers that are a multiple of 3. \cr
\code{engabb\verb{  }} \tab engineering abbr.\verb{ } \tab  same as \code{eng}, but replaces the exponential representation by codes, \cr \tab\tab e.g. \code{M} for mega (1e6). See \code{\link{d.prefix}}.\cr

\code{\%} \tab percent \tab will divide the given number by 100 and append the \%-sign (without a separator).\cr
\tab\cr
\code{p} \tab p-value \tab will wrap the function \code{\link{format.pval}} and return a p-value format. \cr 
\tab \tab Use \code{p_eps} to define the threshold to e.g. switch to a \code{ <0.001 } representation.\cr
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

\code{fmt} can as well be an object of class "\code{style}" consisting of a list out of the arguments above 
(as created by \code{\link{Style}()}).
This allows to store and manage the full format in variables or as options and use it as format template subsequently.  

Finally \code{fmt} can also be a function in x, which makes formatting very flexible.

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
\code{\link{DescToolsOptions}}, \code{\link{Style}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Fm(as.Date(c("2014-11-28", "2014-1-2")), fmt="ddd, d mmmm yyyy")
Fm(as.Date(c("2014-11-28", "2014-1-2")), fmt="ddd, d mmmm yyyy", lang="engl")

# using english ordinal suffixes
Fm(as.Date("2026-01-21"), fmt="MMMM do yyyy", lang="en")
# e.g. in context:
gettextf("Report generated on \%s", 
         Fm(as.Date("2026-05-04"), fmt="MMMM do, yyyy", lang="en"))

# numeric formats
x <- pi * 10^(-10:10)

Fm(x, digits=3, fmt="\%")
Fm(x, digits=4, sci=4, ldigits=0, width=9, align=".")


# format a matrix
m <- matrix(runif(100), nrow=10,
            dimnames=list(LETTERS[1:10], LETTERS[1:10]))

Fm(m, digits=1)

# engineering format
Fm(x, fmt="eng",  digits=2)
Fm(x, fmt="engabb", ldigits=2, digits=2)
# combine with grams [g]
paste(Fm(x, fmt="engabb", ldigits=2, digits=2), "g", sep="")

# example form symnum
pval <- rev(sort(c(outer(1:6, 10^-(1:3)))))
noquote(cbind(Fm(pval, fmt="p"), Fm(pval, fmt="*")))

# change the character to be used as the decimal point
Fm(1200, digits=2, big.mark = ".", outdec=",")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
