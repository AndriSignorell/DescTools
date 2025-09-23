
#' Create Table One Describing Baseline Characteristics 
#' 
#' Create a table summarizing continuous, categorical and dichotomous
#' variables, optionally stratified by one or more variables, while performing
#' adequate statistical tests.
#' 
#' In research the characteristics of study populations are often characterised
#' through some kind of a "Table 1", containing descriptives of the used
#' variables, as mean/standard deviation for continuous variables, and
#' proportions for categorical variables. In many cases, a comparison is made
#' between groups within the framework of the scientific question.
#' 
#' \figure{tone.png}{Table 1}
#' Creating such a table can be very time consuming and there's a need for a
#' flexible function that helps us to solve the task. \code{TOne()} is designed
#' to be easily used with sensible defaults, and yet flexible enough to allow
#' free definition of the essential design elements.
#' 
#' This is done by breaking down the descriptive task to three types of
#' variables: quantitative (numeric, integer), qualitative (factor, characters)
#' and dichotomous variables (the latter having exactly two values or levels).
#' Depending on the variable type, the descriptives and the according sensible
#' tests are chosen. By default mean/sd are chosen to describe numeric
#' variables.  \preformatted{ FUN = function(x) gettextf("%s / %s",
#' Format(mean(x, na.rm = TRUE), digits = 1), Format(sd(x, na.rm = TRUE),
#' digits = 3)) }
#' 
#' Their difference is tested with the Kruskal-Wallis test. For categorical
#' variables the absolute and relative frequencies are calculated and tested
#' with a chi-square test. \cr The tests can be changed with the argument
#' \code{TEST}. These must be organised as list containing elements named
#' \code{"num"}, \code{"cat"} and \code{"dich"}. Each of them must be a
#' function with arguments \code{(x, g)}, returning something similar to a
#' p-value.  \preformatted{ TEST = list( num = list(fun = function(x,
#' g){summary(aov(x ~ g))[[1]][1, "Pr(>F)"]}, lbl = "ANOVA"), cat = list(fun =
#' function(x, g){chisq.test(table(x, g))$p.val}, lbl = "Chi-Square test"),
#' dich = list(fun = function(x, g){fisher.test(table(x, g))$p.val}, lbl =
#' "Fisher exact test")) } The legend text of the test, which is appended to
#' the table together with the significance codes, can be set with the variable
#' \code{lbl}.
#' 
#' Great importance was attached to the free definition of the number formats.
#' By default, the optionally definable format templates of \bold{DescTools}
#' are used. Deviations from this can be freely passed as arguments to the
#' function. Formats can be defined for integers, floating point numbers,
#' percentages and for the p-values of statistical tests. All options of the
#' function \code{\link{Format}()} are available and can be provided as a list.
#' See examples which show several different implementations. \preformatted{
#' fmt = list(abs = Fmt("abs"), num = Fmt("num"), per = Fmt("per"), pval =
#' as.fmt(fmt = "*", na.form = " ")) }
#' 
#' The function returns a character matrix as result, which can easily be
#' subset or combined with other matrices. An interface for
#' \code{\link{ToWrd}()} is available such that the matrix can be transferred
#' to MS-Word. Both font and alignment are freely selectable in the Word table.
#'
#' 
#' @param x a data.frame containing all the variables to be included in the
#' table. 
#' @param grp the grouping variable. 
#' @param add.length logical. If set to \code{TRUE} (default), a row with the
#' group sizes will be inserted as first row of the table. 
#' @param colnames a vector of column names for the result table. 
#' @param vnames a vector of variable names to be placed in the first column
#' instead of the real names. 
#' @param total logical (default \code{TRUE}), defines whether the results
#' should also be displayed for the whole, ungrouped variable.
#' @param align the character on whose position the strings will be aligned.
#' Left alignment can be requested by setting \code{sep = "\\l"}, right
#' alignment by \code{"\\r"} and center alignment by \code{"\\c"}. Mind the
#' backslashes, as if they are omitted, strings would be aligned to the
#' \bold{character} \bold{l}, \bold{r} or \bold{c} respectively. Default value
#' is \code{"\\l"}, thus left alignment. 
#' @param FUN the function to be used as location and dispersion measure for
#' numeric (including integer) variables (\code{mean}/\code{sd} is default,
#' alternatives as \code{median}/\code{IQR} are possible by defining a
#' function). See examples.
#' 
#' @param TEST a list of functions to be used to test the variables. Must be
#' named as \code{"num"}, \code{"cat"} and \code{"dich"} and be defined as
#' function with arguments \code{(x, g)}, generating something similar to a
#' p-value. Use \code{TEST=NA} to suppress test. (See examples.)
#' 
#' @param intref one out of \code{"high"} (default) or \code{"low"}, defining
#' which value of a dichotomous numeric or logical variable should be reported.
#' Usually this will be \code{1} or \code{TRUE}. Setting it to \code{"low"}
#' will report the lower value \code{0} or \code{FALSE}.
#' 
#' @param fmt format codes for absolute, numeric and percentage values, and for
#' the p-values of the tests.
#' 
#' @return a character matrix 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' 
#' @seealso \code{\link{WrdTable}()}, \code{\link{ToWrd.TOne}()} 
#' 
#' @keywords IO
#' @examples
#' 
#' options(scipen = 8)
#' opt <- DescToolsOptions()
#' 
#' # define some special formats for count data, percentages and numeric results
#' # (those will be supported by TOne)
#' Fmt(abs = as.fmt(digits = 0, big.mark = "'"))   # counts
#' Fmt(per = as.fmt(digits = 1, fmt = "%"))        # percentages
#' Fmt(num = as.fmt(digits = 1, big.mark = "'"))   # numeric
#' 
#' TOne(x = d.pizza[, c("temperature", "delivery_min", "driver", "wine_ordered")],
#'   grp = d.pizza$quality)
#' 
#' # the same but no groups now...
#' TOne(x = d.pizza[, c("temperature", "delivery_min", "driver", "wine_ordered")])
#' 
#' # define median/IQR as describing functions for the numeric variables
#' TOne(iris[, -5], iris[, 5],
#'   FUN = function(x) {
#'     gettextf("%s / %s",
#'       Format(median(x, na.rm = TRUE), digits = 1), 
#'       Format(IQR(x, na.rm = TRUE), digits = 3))
#'   }
#' )
#' 
#' # replace kruskal.test by ANOVA and report the p.value
#' # Change tests for all the types
#' TOne(x = iris[, -5], grp = iris[, 5],
#'      FUN = function(x) gettextf("%s / %s",
#'             Format(mean(x, na.rm = TRUE), digits = 1),
#'             Format(sd(x, na.rm = TRUE), digits = 3)), 
#' 
#'      TEST = list(
#'        num = list(fun = function(x, g){summary(aov(x ~ g))[[1]][1, "Pr(>F)"]},
#'                         lbl = "ANOVA"),
#'                cat = list(fun = function(x, g){chisq.test(table(x, g))$p.val},
#'                         lbl = "Chi-Square test"),
#'                dich = list(fun = function(x, g){fisher.test(table(x, g))$p.val},
#'                          lbl = "Fisher exact test")),
#'        fmt = list(abs = Fmt("abs"), num  = Fmt("num"), per = Fmt("per"),
#'                 pval = as.fmt(fmt = "*", na.form = "   ")) 
#' )
#' 
#' t1 <- TOne(x     = d.pizza[,c("temperature", "driver", "rabate")], 
#'            grp   = d.pizza$area, 
#'            align = " ", 
#'            total = FALSE,
#'             
#'            FUN = function(x) gettextf("%s / %s (%s)",
#'                                       Format(mean(x, na.rm = TRUE), digits = 1),
#'                                       Format(sd(x, na.rm = TRUE), digits = 3),
#'                                       Format(median(x, na.rm = TRUE), digits = 1)),
#'            
#'            TEST = NA,
#'             
#'            fmt = list(abs  = as.fmt(big.mark = " ", digits=0), 
#'                       num  = as.fmt(big.mark = " ", digits=1), 
#'                       per  = as.fmt(fmt=function(x) 
#'                           StrPad(Format(x, fmt="%", digits=1), width=5, adj = "r")), 
#'                       pval = as.fmt(fmt = "*", na.form = "   ")) 
#' )
#' # add a userdefined legend
#' attr(t1, "legend") <- "numeric: mean / sd (median)), factor: n (n%)"
#' 
#' t1
#' 
#' 
#' # dichotomous integer or logical values can be reported by the high or low value
#' x <- sample(x = c(0, 1), size = 100, prob = c(0.3, 0.7), replace = TRUE)
#' y <- sample(x = c(0, 1), size = 100, prob = c(0.3, 0.7), replace = TRUE) == 1
#' z <- factor(sample(x = c(0, 1), size = 100, prob = c(0.3, 0.7), replace = TRUE))
#' g <- sample(x = letters[1:4], size = 100, replace = TRUE)
#' d.set <- data.frame(x = x, y = y, z = z, g = g)
#' 
#' TOne(d.set[1:3], d.set$g, intref = "low")
#' 
#' TOne(d.set[1:3], d.set$g, intref = "high")
#' 
#' # intref would not control factors, use relevel to change reported value
#' TOne(data.frame(z = relevel(z, "1")), g)
#' 
#' TOne(data.frame(z = z), g)
#' 
#' options(opt)
#' 
#' 
#' \dontrun{  
#'   
#' # Send the whole stuff to Word
#' wrd <- GetNewWrd()
#' ToWrd(
#'   TOne(x   = d.pizza[, c("temperature", "delivery_min", "driver", "wine_ordered")],
#'        grp = d.pizza$quality,
#'        fmt = list(num=Fmt("num", digits=1))
#'        ),
#'   font = list(name="Arial narrow", size=8),
#'   align = c("l","r")      # this will be recycled: left-right-left-right ...
#' )
#' }
#' 


TOne <- function(x, grp = NA, add.length=TRUE,
                 colnames=NULL, vnames=NULL, total=TRUE,
                 align="\\l", FUN = NULL, TEST = NULL, intref="high",
                 fmt=list(abs  = Fmt("abs"),
                          num  = Fmt("num"), per=Fmt("per"),
                          pval = as.fmt(fmt = "*", na.form = "   ")) ) {
  
  
  # set the formats, take the provided fmt and combine with defaults
  fmt <- c(fmt,
           list(abs  = Fmt("abs"),
                num  = Fmt("num"), 
                per=Fmt("per"),
                pval = as.fmt(fmt = "*", na.form = "   ")))
  # use the first instance, so user defined formats are preferred 
  # and the standards come into effect if there are no user specifications
  fmt <- fmt[!duplicated(fmt)]
  # we could restrict the names here to c("abs","num","per","pval")
  
  
  # set the variablenames per row
  if(is.null(vnames)){
    vnames <- if(is.null(colnames(x))) "Var1" else colnames(x)
    default_vnames <- TRUE
  } else {
    default_vnames <- TRUE
  }
  
  # creates the table one in a study
  if(is.null(FUN)){
    num_fun <- function(x){
      # the cell for numeric data
      gettextf("%s (%s)",
               Format(mean(x, na.rm=TRUE), fmt=fmt$num),
               Format(sd(x, na.rm=TRUE), fmt=fmt$num))
    }
  } else {
    num_fun <- FUN
  }
  
  
  if(identical(grp, NA)){
    # no grouping factor, let's define something appropriate
    grp <- rep(1, nrow(x))
    TEST <- NA
  }
  
  
  if(identical(TEST, NA)){
    
    TEST <- list(num=list(fun=function(x, g) 1, lbl="None"),
                 cat=list(fun=function(x, g) 1, lbl="None"),
                 dich=list(fun=function(x, g) 1, lbl="None"))
    notest <- TRUE
    
  } else {
    
    # the default tests for quantitative and categorical data
    TEST.def <- list(num=list(fun=function(x, g){kruskal.test(x, g)$p.val},
                              lbl="Kruskal-Wallis test"),
                     cat=list(fun=function(x, g){chisq.test(table(x, g))$p.val},
                              lbl="Chi-Square test"),
                     dich=list(fun=function(x, g){fisher.test(table(x, g))$p.val},
                               lbl="Fisher exact test"))
    
    if(is.null(TEST))  # the defaults
      TEST <- TEST.def
    
    # define test for the single tests
    if(is.null(TEST[["num"]]))
      TEST[["num"]] <- TEST.def[["num"]]
    if(is.null(TEST[["cat"]]))
      TEST[["cat"]] <- TEST.def[["cat"]]
    if(is.null(TEST[["dich"]]))
      TEST[["dich"]] <- TEST.def[["dich"]]
    
    notest <- FALSE
    
  }
  
  num_test <- TEST[["num"]]$fun
  cat_test <- TEST[["cat"]]$fun
  dich_test <- TEST[["dich"]]$fun
  

  num_row <- function(x, g, total=TRUE, vname = deparse(substitute(x))){
    if(!identical(g, NA)) {
      res <- Format(num_test(x, g), fmt=fmt$pval)
      num_test_label <- names(res)
    } else {
      res <- ""
    }
   
    return(
      cbind(var=vname, total = num_fun(x), rbind(tapply(x, g, num_fun)),
          paste(res, .FootNote(1)))
    )
  }
  
  
  cat_mat <- function(x, g, vname=deparse(substitute(x))){
    
    if(inherits(x, "character"))
      x <- factor(x)
    
    tab <- table(x, g)
    ptab <- prop.table(tab, margin = 2)
    tab <- addmargins(tab, 2)
    ptab <- cbind(ptab, Sum=prop.table(table(x)))
    
    
    # crunch tab and ptab
    m <- matrix(NA, nrow=nrow(tab), ncol=ncol(tab))
    m[,] <- gettextf("%s (%s)",
                     Format(tab, fmt=fmt$abs),
                     Format(ptab, fmt=fmt$per))
    # totals to the left
    m <- m[, c(ncol(m), 1:(ncol(m)-1))]
    
    # set rownames
    m <- cbind( c(vname, paste(" ", levels(x))),
                rbind("", m))
    # add test
    if(nrow(tab)>1)
      # p <- chisq.test(tab)$p.value
      p <- cat_test(x, g)
    else
      p <- NA
    m <- cbind(m, c(paste(Format(p, fmt=fmt$pval), ifelse(is.na(p), "", .FootNote(3))), rep("", nlevels(x))))
    
    # this reduces binary categories to a single flag, which should not be necessary here,
    # as it would be handled be dich_mat
    # if(nrow(m) <=3) {
    #   m[2,1] <- gettextf("%s (= %s)", m[1, 1], row.names(tab)[1])
    #   m <- m[2, , drop=FALSE]
    # }
    
    colnames(m) <- c("var","total", head(colnames(tab), -1), "")
    
    return(m)
    
  }
  
  dich_mat <- function(x, g, vname=deparse(substitute(x))){
    
    tab <- table(x, g)
    
    if(identical(dim(tab), c(2L,2L))){
      #      p <- fisher.test(tab)$p.value
      p <- dich_test(x, g)
      foot <- .FootNote(2)
    } else {
      #      p <- chisq.test(tab)$p.value
      p <- cat_test(x, g)
      foot <- .FootNote(3)
    }
    
    ptab <- prop.table(tab, 2)
    tab <- addmargins(tab, 2)
    ptab <- cbind(ptab, Sum = prop.table(tab[,"Sum"]))
    
    m <- matrix(NA, nrow=nrow(tab), ncol=ncol(tab))
    m[,] <- gettextf("%s (%s)",
                     Format(tab, fmt=fmt$abs),
                     Format(ptab, fmt=fmt$per))
    
    # totals to the left
    m <- m[, c(ncol(m), 1:(ncol(m)-1)), drop=FALSE]
    
    m <- rbind(c(vname, m[1,], paste(Format(p, fmt=fmt$pval), foot)))
    colnames(m) <- c("var","total", head(colnames(tab), -1), "")
    
    return(m)
    
  }
  
  
  
  if(!identical(x, NA)) {
    
    # NA is handled as subtitle
    intref <- match.arg(intref, choices = c("high", "low", "both"))
    
    if(mode(x) %in% c("logical","numeric","complex","character"))
      x <- data.frame(x)
    
    # find description types
    ctype <- sapply(x, class)
    # should we add "identical type": only one value??
    ctype[sapply(x, IsDichotomous, strict=TRUE, na.rm=TRUE)] <- "dich"
    
    ctype[sapply(ctype, function(x) any(x %in% c("numeric","integer")))] <- "num"
    ctype[sapply(ctype, function(x) any(x %in% c("factor","ordered","character")))] <- "cat"
    
    lst <- list()
    for(i in 1:ncol(x)){
      if(ctype[i] == "num"){
        lst[[i]] <- num_row(x[,i], grp, vname=vnames[i])

      } else if(ctype[i] == "cat") {
        lst[[i]] <- cat_mat(x[,i], grp, vname=vnames[i])
        
      } else if(ctype[i] == "dich") {
        
        if(intref=="both"){
          lst[[i]] <- cat_mat(factor(x[,i]), grp, vname=vnames[i])
          
        } else {
          
          # refactor all types, numeric, logic but not factors and let user choose
          # the level to be reported.
          if(!is.factor(x[, i])) {   # should only apply to boolean integer or numerics
            xi <- factor(x[, i])
          } else {
            xi <- x[, i]
          }
          
          if(match.arg(intref, choices = c("high", "low", "both")) == "high")
            xi <- relevel(xi, tail(levels(xi), 1))

          if (default_vnames) {
            lst[[i]] <- dich_mat(xi, grp, vname = gettextf("%s (= %s)", vnames[i], head(levels(xi), 1)))
          } else {
            lst[[i]] <- dich_mat(xi, grp, vname = gettextf("%s", vnames[i]))
          }
        }
        
      } else {
        lst[[i]] <- rbind(c(colnames(x)[i], rep(NA, nlevels(grp) + 2)))
      }
    }
  } else {
    m <- cat_mat(grp, grp, vnames)
    lst <- list(c(vnames, rep("", ncol(m)-1)))
  }
  
  res <- do.call(rbind, lst)

  
  if(add.length)
    res <- rbind(c("n", c(Format(sum(!is.na(grp)), fmt=fmt$abs),
                          paste(Format(table(grp), fmt=fmt$abs), " (",
                                Format(prop.table(table(grp)), fmt=fmt$per), ")", sep=""), ""))
                 , res)
  
  # align the table
  if(align != "\\l")
    res[,-c(1, ncol(res))] <- StrAlign(res[,-c(1, ncol(res))], sep = align)
  
  if(all(grp==1)){
    res <- res[, -3]
    total <- TRUE
  }
  
  if(!total)
    res <- res[, -2]
  
  if(!is.null(colnames))
    colnames(res) <- rep(colnames, length.out=ncol(res))
  
  
  if(!notest)
    attr(res, "legend") <- gettextf("%s) %s, %s) %s, %s) %s\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
                                    .FootNote(1), TEST[["num"]]$lbl, .FootNote(2), TEST[["dich"]]$lbl, .FootNote(3), TEST[["cat"]]$lbl)
  else {
    attr(res, "legend") <- ""
    res <- res[, -ncol(res)]
  }
  
  class(res) <- "TOne"
  return(res)
}


.FootNote <- function(i){
  
  # internal function, not exported
  
  # x <- getOption("footnote")
  x <- DescToolsOptions("footnote")
  if(is.null(x))
    x <- c("'", '"', '""')
  return(x[i])
}



# Old, replaced by 0.99.54.6:
# print.TOne <- function(x, ...){
#   
#   cat("\n")
#   
#   write.table(format(rbind(colnames(x), x), justify="left"),
#               row.names=FALSE, col.names=FALSE, quote=FALSE)
#   
#   if(!is.null(attr(x, "legend"))){
#     cat("---\n")
#     cat(attr(x, "legend"), "\n")
#   }
#   cat("\n")
#   
# }

print.TOne <- function(x, ...){
  
  cat("\n")
  
  if(.has_color()){
    
    t1 <- as.data.frame.matrix(x)
    colnames(t1) <- colnames(x)
    
    out <- capture.output(print((t1), right=FALSE, sep="   ", 
                                print.gap=3, col.names=FALSE))
    cat(cli::style_bold(out[1]))
    print(unname(t1), right=FALSE, sep="   ", print.gap=3, col.names=FALSE)
    
    if(!is.null(attr(x, "legend"))){
      cat(cli::col_silver("---\n"))
      cat(cli::col_silver(attr(x, "legend"), "\n"))
    }
    cat("\n")
    
    
  } else {
    
    write.table(format(rbind(colnames(x), x), justify="left"),
                row.names=FALSE, col.names=FALSE, quote=FALSE)
    
    if(!is.null(attr(x, "legend"))){
      cat("---\n")
      cat(attr(x, "legend"), "\n")
    }
    cat("\n")
    
  } 
  
}



# subsetting TOne

`[.TOne` <- function(x, i, j, ..., drop=FALSE) {
  
  # subset main character matrix, don't drop structure by default
  res <- unclass(x)[i, j, drop=drop]
  
  # attribute dim should not be restore all relevant attributes
  attr(res, "legend") <- attr(x, "legend")
  attr(res, "class") <- attr(x, "class")
  
  return(res)
  
}



