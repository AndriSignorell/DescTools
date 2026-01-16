\name{style}
\alias{style}
\alias{styles}

\title{Format Styles
}
\description{Interface for format templates, defined as a list consisting of any accepted format features 
in \code{fm()}. 
This enables to define templates globally and easily change or modify them later.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
style(x, digits = NULL, ldigits = NULL, sci = NULL, 
      big.mark=NULL, outdec = NULL, na.form = NULL, zero.form = NULL, 
      fmt = NULL, p_eps = NULL, width = NULL, align = NULL, lang = NULL,
      label = NULL, ...)
      
styles()
}
\arguments{
  \item{x}{an object of class \code{Style} or a the name of a style, defined either in the 
  global enviroment or in the options.}

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
  or an object of class \code{style}, consisting of a list of \code{fdm} arguments. See Details.
}
\item{p_eps}{a numerical tolerance used mainly for formatting p values, those 
less than p_eps are formatted as "\code{< [p_eps]}" (where '[p_eps]' stands for \code{format(p_eps, digits))}. 
Default is \code{0.001}.}

  \item{width}{integer, the defined fixed width of the strings.
}

\item{align}{ the character on whose position the strings will be aligned. Left alignment can be requested by setting \code{sep = "\\\\l"}, right alignment by \code{"\\\\r"} and center alignment by \code{"\\\\c"}. Mind the backslashes, as if they are omitted, strings would be aligned to the \bold{character} l, r or c respectively. The default is \code{NULL} which would just leave the strings as they are.\cr
This argument is send directly to the function \code{\link{StrAlign}()} as argument \code{sep}.
}

  \item{lang}{optional value setting the language for the months and daynames. 
  Can be either \code{"local"} for current locale or \code{"en"} for english. 
  If left to \code{NULL}, the DescToolsOption \code{"lang"} will 
  be searched for and if not found \code{"local"} will be taken as default.
}

  \item{label}{a description for the style}

\item{\dots}{further arguments to be passed to or from methods. }

}
\details{
% \code{style()} can either create new styles or edit existing ones. 
\code{style()} can be used to create new styles.
It takes any of the arguments from \code{fm()} and combines them to an object of
class \code{"Style"}, which then can be handed over to \code{fm()} as argument \code{fmt}. \cr
Following will define a new format template named "\code{num.sty}". 
Passed to \code{fm()} this will result 
in a number displayed with 2 fixed digits and a comma as big mark:
\preformatted{num.sty <- style(digits=2, big.mark=",")
fm(12222.89345, fmt=num.sty) = 12,222.89}
This is the same result as if the arguments would have been supplied directly, but helps
to avoid boilerplate code: \cr
\code{fm(12222.89345, digits=2, big.mark=",")}.

To edit a style we can provide \code{style()} with its name and overwrite, resp. add new format options. 
\code{style("num.sty", digits=1, sci=10)} will use the current 
version of the numeric format and change the digits to 1 and the threshold 
to switch to scientifc presentation to numbers > 1e10 and < 1e-10.

\code{styles()} returns all found style definitions in the global environment or
in the options. 

The styles can be stored as options for convenience. 
To store a new format we use the default \code{options()} approach:
\code{options(num.sty = style(digits=1, big.mark=" "))}
Defined styles in the options can be passed on to \code{fm()} simply by their name.

Many report functions (e.g. \code{\link{TOne}()}) in \bold{DescTools} use 
three default formats for counts (named \code{"abs.sty"}), numeric 
values (\code{"num.sty"}) and percentages (\code{"per.sty"}).

}
\value{\code{style()} returns an object of class \code{Style}\cr
\code{styles()} returns a list of styles
}

\author{Andri Signorell <andri@signorell.net>}


\seealso{\code{\link{fm}()}}

\examples{
# use style() to get and define new formats stored as option
num.sty <- style(digits=2, big.mark=" ")
abs.sty <- style(digits=0, big.mark=" ")
dat.sty <- style(fmt="MM, dd yyyy")

num.sty                             # displays the details of the style
# editing styles
style("abs.sty")                    # looks for format "abs.sty"
style("nexist")                     # return for nonexisting style
style("abs.sty", big.mark="")       # get Style("abs") and overwrite big.mark
style("abs.sty", na.form="-")       # get Style("abs") and add user defined na.form

styles()                            # all defined formats
styles()[c("num.sty", "abs.sty")]   # numeric and integer styles

# define totally new format and store as option
options(nob.sty=style(digits=5, na.form="nodat"))

# using styles
fm(314.1563, fmt=abs.sty)
fm(314.1563, fmt=num.sty)

fm(Today(), fmt=dat.sty)
}

\keyword{ IO }
