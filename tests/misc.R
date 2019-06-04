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
