\name{TOne}
\alias{TOne}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create Table One Describing Baseline Characteristics
%%  ~~function to do ... ~~
}
\description{
Create a table summarizing continuous, categorical and dichotomous variables, optionally stratified by one or more variables, while performing adequate statistical tests.
}
\usage{
TOne(x, grp = NA, add.length = TRUE, colnames = NULL, vnames = NULL, 
     total = TRUE, align = "\\\\l", 
     FUN = NULL, TEST = NULL, intref = "high",
     fmt = list(abs = Fmt("abs"), num  = Fmt("num"), per = Fmt("per"),
                pval = as.fmt(fmt = "*", na.form = "   ")) )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a data.frame containing all the variables to be included in the table.
%%     ~~Describe \code{x} here~~
}
  \item{grp}{the grouping variable.
%%     ~~Describe \code{grp} here~~
}
  \item{add.length}{logical. If set to \code{TRUE} (default), a row with the lengths of the group will be inserted as first row of the table.
%%     ~~Describe \code{grp} here~~
}
  \item{colnames}{a vector of columnnames for the result table.
%%     ~~Describe \code{grp} here~~
}
  \item{vnames}{a vector of variablenames to be placed in the first column instead of the real names.
%%     ~~Describe \code{grp} here~~
}
\item{total}{logical (default \code{TRUE}), defines whether the results should also be displayed for the whole, ungrouped variable.}

  \item{align}{the character on whose position the strings will be aligned. Left alignment can be requested by setting \code{sep = "\\\\l"}, right alignment by \code{"\\\\r"} and center alignment by \code{"\\\\c"}. Mind the backslashes, as if they are omitted, strings would be aligned to the \bold{character} l, r or c respectively. Default value is \code{"\\\\l"}, thus left alignment.
%%     ~~Describe \code{sep} here~~
}
\item{FUN}{the function to be used as location and dispersion measure for numeric (including integer) variables (typically \code{mean}/\code{sd} (default) or alternatively \code{median}/\code{IQR}). See examples.}

\item{TEST}{a list of functions to be used to test the variables. Must be named as \code{"num"}, \code{"cat"} and \code{"dich"} and be defined as function with arguments \code{(x, g)}, generating something similar to a p-value. (See examples.) }
\item{intref}{one out of \code{"high"} (default) or \code{"low"}, defining which value of a dichotomous numeric or logical variable should be reported. Usually this will be \code{1} or \code{TRUE}. Setting it to \code{"low"} will report the lower value \code{0} or \code{FALSE}. }

\item{fmt}{format codes for absolute, numeric and percentage values, and for the p-values of the tests. }
}
\details{In research the characteristics of study populations are often characterised through some kind of a "Table 1", containing descriptives of the used variables, as mean/standard deviation for continuous variables, and proportions for categorical variables. In many cases, a comparison is made between two or more groups within the framework of the scientific question. Creating such a table can be very time consuming and there's a need for a flexible function that helps us to solve the task. \code{TOne()} should be as easy to use as possible and yet so flexible that the essential design elements can be freely defined.

This is done by dividing the world into 3 groups: numeric, factor and dichotomous variables (having exactly two values or levels). Depending on the group, the descriptives and the according sensible tests are chosen. By default mean/sd is chosen for numeric values. Their difference is tested with the Kruskal-Wallis test. For categorical variables the absolute and relative frequencies are calculated and tested with a chi-square test. The tests can be changed with the argument \code{TEST}. These must be organised as list containing elements named \code{"num"}, \code{"cat"} and \code{"dich"}. Each of them must be a function with arguments \code{(x, g)}, returning something similar to a p-value. The legend text of the test, which is appended to the table, can be set with the variable \code{lbl}.

Great importance was attached to the free definition of the number formats. By default, the optionally definable format templates of \bold{DescTools} are used. Deviations from this can be freely passed as arguments to the function. Formats can be defined for integers, floating point numbers, percentages and for the p-values of statistical tests. All options of the function \code{\link{Format}()} are available and can be provided as a list. See examples which show several different implementations.

The function returns a character matrix as result, which can easily be subset or combined with other matrices. An interface for \code{\link{ToWrd}()} is available such that the matrix can be transferred to MS-Word. Both font and alignment are freely selectable in the Word table. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{a character matrix
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

\seealso{ \code{\link{WrdTable}()}, \code{\link{ToWrd.TOne}()}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
options(scipen = 8)
opt <- DescToolsOptions()

# define some special formats for count data, percentages and numeric results
# (those will be supported by TOne)
Fmt(abs = as.fmt(digits = 0, big.mark = "'"))   # counts
Fmt(per = as.fmt(digits = 1, fmt = "\%"))        # percentages
Fmt(num = as.fmt(digits = 1, big.mark = "'"))   # numeric

TOne(x = d.pizza[, c("temperature", "delivery_min", "driver", "wine_ordered")],
  grp = d.pizza$quality)

# define median/IQR as describing functions for the numeric variables
TOne(iris[, -5], iris[, 5],
  FUN = function(x) {
    gettextf("\%s / \%s",
      Format(median(x, na.rm = TRUE), digits = 1), 
      Format(IQR(x, na.rm = TRUE), digits = 3))
  }
)

# replace kruskal.test by ANOVA and report the p.value
# Change tests for all the types
TOne(x = iris[, -5], grp = iris[, 5],
     FUN = function(x) gettextf("\%s / \%s",
            Format(mean(x, na.rm = TRUE), digits = 1),
            Format(sd(x, na.rm = TRUE), digits = 3)), 

     TEST = list(
       num = list(fun = function(x, g){summary(aov(x ~ g))[[1]][1, "Pr(>F)"]},
                        lbl = "ANOVA"),
               cat = list(fun = function(x, g){chisq.test(table(x, g))$p.val},
                        lbl = "Chi-Square test"),
               dich = list(fun = function(x, g){fisher.test(table(x, g))$p.val},
                         lbl = "Fisher exact test")),
       fmt = list(abs = Fmt("abs"), num  = Fmt("num"), per = Fmt("per"),
                pval = as.fmt(fmt = "*", na.form = "   ")) 
)

# dichotomous integer or logical values can be reported by the high or low value
x <- sample(x = c(0, 1), size = 100, prob = c(0.3, 0.7), replace = TRUE)
y <- sample(x = c(0, 1), size = 100, prob = c(0.3, 0.7), replace = TRUE) == 1
z <- factor(sample(x = c(0, 1), size = 100, prob = c(0.3, 0.7), replace = TRUE))
g <- sample(x = letters[1:4], size = 100, replace = TRUE)
d.set <- data.frame(x = x, y = y, z = z, g = g)

TOne(d.set[1:3], d.set$g, intref = "low")

TOne(d.set[1:3], d.set$g, intref = "high")

# intref would not control factors, use relevel to change reported value
TOne(data.frame(z = relevel(z, "1")), g)

TOne(data.frame(z = z), g)

options(opt)


\dontrun{  
  
# Send the whole stuff to Word
wrd <- GetNewWrd()
ToWrd(
  TOne(x   = d.pizza[, c("temperature", "delivery_min", "driver", "wine_ordered")],
       grp = d.pizza$quality,
       fmt = list(num=Fmt("num", digits=1))
       ),
  font = list(name="Arial narrow", size=8),
  align = c("l","r")      # this will be recycled: left-right-left-right ...
)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO }
