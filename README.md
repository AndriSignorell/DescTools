
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version-last-release/DescTools)](https://CRAN.R-project.org/package=DescTools)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/DescTools)](https://CRAN.R-project.org/package=DescTools)
[![downloads](http://cranlogs.r-pkg.org/badges/last-week/DescTools)](https://CRAN.R-project.org/package=DescTools)
[![License: GPL
v2+](https://img.shields.io/badge/License-GPL%20v2+-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R build
status](https://github.com/AndriSignorell/DescTools/workflows/R-CMD-check/badge.svg)](https://github.com/AndriSignorell/DescTools/actions)
[![pkgdown](https://github.com/AndriSignorell/DescTools/workflows/pkgdown/badge.svg)](https://andrisignorell.github.io/DescTools/)
<!-- badges: end -->

# Tools for Descriptive Statistics and Exploratory Data Analysis

**DescTools** is an extensive collection of miscellaneous basic
statistics functions and comfort wrappers not available in the R basic
system for efficient description of data. The author’s intention was to
create a toolbox, which facilitates the (notoriously time consuming)
first descriptive tasks in data analysis, consisting of calculating
descriptive statistics, drawing graphical summaries and reporting the
results. The package contains furthermore functions to produce documents
using MS Word (or PowerPoint) and functions to import data from Excel.

A considerable part of the included functions can be found scattered in
other packages and other sources written partly by Titans of R. The
reason for collecting them here, was primarily to have them consolidated
in ONE instead of dozens of packages (which themselves might depend on
other packages, which are not needed at all), and to provide a common
and consistent interface as far as function and arguments naming, `NA`
handling, recycling rules etc. are concerned. Google style guides were
used as naming rules (in absence of convincing alternatives). The
‘CamelStyle’ was consequently applied to functions borrowed from
contributed R packages as well.

Feedback, feature requests, bug reports and other suggestions are
welcome\! Please report problems to [GitHub issues
tracker](https://github.com/AndriSignorell/DescTools/issues)
(preferred), Stack Overflow mentioning **DescTools** or directly to the
maintainer.

## Installation

You can install the released version of **DescTools** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("DescTools")
```

And the development version from GitHub with:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("AndriSignorell/DescTools")
```

# Warning

**Warning:** This package is still under development. Although the code
seems meanwhile quite stable, until release of version 1.0 you should be
aware that everything in the package might be subject to change.
Backward compatibility is not yet guaranteed. Functions may be deleted
or renamed and new syntax may be inconsistent with earlier versions. By
release of version 1.0 the “deprecated-defunct process” will be
installed.

# MS-Office

To make use of MS-Office features, you must have Office in one of its
variants installed. All `Wrd*`, `XL*` and `Pp*` functions require the
package **RDCOMClient** to be installed as well. Hence the use of these
functions is restricted to *Windows* systems. **RDCOMClient** can be
installed with:

``` r
install.packages("RDCOMClient", repos="http://www.omegahat.net/R")
```

The *omegahat* repository does not benefit from the same update service
as CRAN. So you may be forced to install a package compiled with an
earlier version, which usually is not a problem. Use e.g. for R 3.6.x/R
4.0.x:

``` r
url <- "http://www.omegahat.net/R/bin/windows/contrib/3.5.1/RDCOMClient_0.93-0.zip"
url <- "http://www.omegahat.net/R/bin/windows/contrib/4.0/RDCOMClient_0.94-0.zip"
install.packages(url, repos = NULL, type = "binary")
```

**RDCOMClient** does not exist for Mac or Linux, sorry.

# Authors

Andri Signorell  
Helsana Versicherungen AG, Health Sciences, Zurich  
HWZ University of Applied Sciences in Business Administration Zurich.

R is a community project. This can be seen from the fact that this
package includes R source code and/or documentation previously published
by [various authors and
contributors](https://andrisignorell.github.io/DescTools/authors.html).
Special thanks go to Beat Bruengger, Mathias Frueh, Daniel Wollschlaeger
for their valuable contributions and testing. The good things come from
all these guys, any problems are likely due to my tweaking. Thank you
all\!

**Maintainer:** Andri Signorell

# Examples

``` r
library(DescTools)
```

<!-- ## Demo "describe" -->

``` r
demo(describe, package = "DescTools")
```

<!-- ## Demo "plots" -->

``` r
demo(plots, package = "DescTools")
```
