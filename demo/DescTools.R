## -----------------------------------------------------------------------------------
## Demo file for DescTools; start with 'demo(DescTools)'
## -----------------------------------------------------------------------------------

cat(cli::col_blue(cli::style_bold("  DescTools  "), " is a package for descriptive and explorative statistics. 
  It contains many basic statistic functions, tests and plots complementing 
  the base R functions' set.
"))

# Describing numeric, factor and binary variables
Desc(d.pizza$temperatur, plotit=TRUE)
Desc(d.pizza$driver, plotit=TRUE)
Desc(d.pizza$wine_ordered)

with(d.pizza, Desc(area ~ operator, verbose=3))
with(d.pizza, Desc(temperature ~ delivery_min, verbose=3))
with(d.pizza, Desc(temperature ~ area, verbose=3))
with(d.pizza, Desc(area ~ temperature, verbose=3))
layout(0)

# Many special plots 
with(cars, PlotBag(speed, dist))

PlotArea(WorldPhones, col=Pal("Helsana", alpha =.60), las=1)

PlotLinesA(WorldPhones/1e3, col=Pal("Helsana"), lwd=2, 
           main="WorldPhones [in 1'000]")


tab <- table(d.pizza$weekday, d.pizza$operator)
par(mfrow=c(1,2))
PlotCirc(tab, main="operator ~ weekday",
         acol = c("dodgerblue","seagreen2","limegreen","olivedrab2","goldenrod2","tomato2"),
         rcol = SetAlpha(c("red","orange","olivedrab1"), 0.5)
)

PlotCirc(tab, main="operator ~ weekday", acol = Pal("Helsana"))


tab <- matrix(c(2,5,8,3,10,12,5,7,15), nrow=3, byrow=FALSE)
dimnames(tab) <- list(c("A","B","C"), c("D","E","F"))
PlotCirc( tab,
          acol = c("dodgerblue","seagreen2","limegreen","olivedrab2","goldenrod2","tomato2"),
          rcol = SetAlpha(c("red","orange","olivedrab1"), 0.5)
)


set.seed(1789)
N <- 20
area <- rlnorm(N)
grp <- sample(x=1:3, size=20, replace=TRUE, prob=c(0.2,0.3,0.5))
z <- Sort(data.frame(area=area, grp=grp), c("grp","area"), decreasing=c(FALSE,TRUE))
z$col <- SetAlpha(c("steelblue","green","yellow")[z$grp],
                  unlist(lapply(split(z$area, z$grp),
                                function(...) LinScale(..., newlow=0.1, newhigh=0.6))))

PlotTreemap(x=z$area, grp=z$grp, labels=letters[1:20], col=z$col, main="Treemap")


# statistic functions, supporting weights and their confidence intervals

# use weights
x <- sample(20, 30, replace = TRUE)
y <- sample(20, 30, replace = TRUE)
z <- as.numeric(names(w <- table(x)))

fun <- list(mean=Mean, median=Median, "std. deviation"=SD, variance=Var, 
            "median absolute deviation"=MAD, "mean absolute deviation"=MeanAD, 
            quantile=Quantile, iqr=IQRw, 
            skewness=Skew, kurtosis=Kurt)

sapply(fun, function(f) f(x))

# the same using weights
sapply(fun, function(f) f(z, weights=w))


# confidence intervals
MeanCI(x, conf.level = 0.95)
MedianCI(x, conf.level = 0.95)
QuantileCI(x, conf.level = 0.95)
VarCI(x, conf.level = 0.95)
MADCI(x, conf.level = 0.95)
Skew(x, conf.level = 0.95)
Kurt(x, conf.level = 0.95)

x <- sample(5, 30, replace = TRUE)
y <- sample(5, 30, replace = TRUE)
Cor(x, y)
tt <- table(x, y)
ttt <- tt[-2,-2]

cor(x, y)

with(as.data.frame(sapply(Untable(ttt), 
                          function(x) as.numeric(as.character(x)))), 
     cor(x,y))

cov.wt( cbind(rep(as.numeric(rownames(ttt)), times=nrow(ttt)), 
        rep(as.numeric(colnames(ttt)), each=ncol(ttt))),
        wt=c(ttt), cor = TRUE)$cor[1,2]

# Desc(ttt, verbose = 3)
Assocs(ttt)


# Tables: TOne
TOne(x    = d.pizza[,c("temperature", "driver", "rabate")], 
     grp   = d.pizza$area, 
     align = " ", 
     total = FALSE)







