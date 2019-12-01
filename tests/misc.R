# require("DescTools")
#
# options(warn=2)
#
# x <- d.pizza$temperature
# z <- Desc(x, plotit=FALSE)
#
# stopifnot(identical(z[1:6],
#           list(length=length(x), n=sum(!is.na(x)), NAs=sum(is.na(x)),
#                unique=length(unique(na.omit(x))), `0s`=sum(na.omit(x)==0),
#                mean=mean(x, na.rm=TRUE))))
#
# stopifnot(IsZero(z[["MeanSE"]] - MeanSE(x, na.rm=TRUE)))


library(DescTools)

# stopifnot(exprs = {
#   all.equal(pretty10exp(10^expo, drop.1=TRUE, sub10 = c(-2, 2)),
#             expression(10^-3, 0.01, 0.1, 1, 10, 100, 10^3, 10^4))
#   
#   identical(pretty10exp(10^expo, drop.1=TRUE, sub10 = c(-2, 2), lab.type="latex"),
#             c("$10^{-3}$", "0.01", "0.1", "1", "10", "100",
#               "$10^{3}$", "$10^{4}$"))
#   ## gave exponential format for "latex" case.
# })
# 

stopifnot(all(
  identical(Mode(5), structure(5, freq = 1L))
  , identical(Mode(NA), structure(NA_real_, freq = NA_integer_)) 
  , identical(Mode(c(NA, NA)), structure(NA_real_, freq = NA_integer_)) 
  , identical(Mode(c(NA, 0:5)), structure(NA_real_, freq = NA_integer_)) 
  , identical(Mode(c(NA, 0:5), na.rm=TRUE), structure(NA_real_, freq = 1L)) 
  , identical(Mode(c(NA, 0:5, 5), na.rm=TRUE), structure(5, freq = 2L)) 
  , identical(Mode(c(0:5, 4, 5, 6)), structure(c(4, 5), freq = 2L)) 
  , identical(Mode(c(0:8, rep(c(1,3, 8), each=5))), structure(c(1, 3, 8), freq = 6L)) 
))

