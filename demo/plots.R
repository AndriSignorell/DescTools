## -----------------------------------------------------------------------------------
## Demo file for plots; start with 'demo(plots)'
## -----------------------------------------------------------------------------------


tab <- matrix(c(2,5,8,3,10,12,5,7,15), nrow=3, byrow=FALSE,
              dimnames = list(c("A","B","C"), c("D","E","F")) )

par(mfrow=c(1,1), xpd=TRUE)
PlotCirc( tab,
          acol = c("dodgerblue","seagreen2","limegreen","olivedrab2","goldenrod2","tomato2"),
          rcol = SetAlpha(c("red","orange","olivedrab1"), 0.5)
)


# distribution plot (combination of histogram, densitycurve, boxplot and ecdf.plot)
old.par <- par(no.readonly=TRUE)
PlotFdist(x=d.pizza$delivery_min, na.rm=TRUE)

# plot multiple density curves
par(old.par)
PlotMultiDens( split(d.pizza$delivery_min, d.pizza$driver), na.rm=TRUE
               , main="delivery time ~ driver", xlab="delivery time [min]", ylab="density"
               , panel.first=grid())


# areaplot with stapled areas
tab <- table( d.pizza$date, d.pizza$driver )
PlotArea(x=as.Date(rownames(tab)), y=tab, xaxt="n", xlab="Date", ylab="Pizzas delivered" )
# add x-axis and some text labels
xrng <- pretty(range(as.Date(rownames(tab))))
axis(side=1, at=xrng, labels=xrng)
text( x=min(d.pizza$date + .5, na.rm=TRUE), y=cumsum(tab[2,])-2.5,
      label=levels(d.pizza$driver), adj=c(0,0.5), col=TextContrastColor( gray.colors(7)))


# dotchart with confidence intervals
x <- do.call("rbind", tapply( d.pizza$temperature, d.pizza$driver, MeanCI, na.rm=TRUE))
rownames(x) <- levels(d.pizza$driver)
PlotDot(x)


# Plot pyramid
xy.pop <- c(3.2,3.5,3.6,3.6,3.5,3.5,3.9,3.7,3.9,3.5,3.2,2.8,2.2,1.8,1.5,1.3,0.7,0.4)
xx.pop <- c(3.2,3.4,3.5,3.5,3.5,3.7,4,3.8,3.9,3.6,3.2,2.5,2,1.7,1.5,1.3,1,0.8)
agelabels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
               "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-44","85+")

PlotPyramid( xy.pop, xx.pop, ylab=agelabels, lxlab="men", rxlab="women",
             main="Australian population pyramid 2002", col=Pal("Helsana")[c(6,1)])


# Plot violin
PlotViolin(temperature ~ driver, d.pizza, col="brown", bw="SJ")


# PlotPolar
testlen <- c(sin(seq(0, 1.98*pi, length=100))+2+rnorm(100)/10)
testpos <- seq(0, 1.98*pi, length=100)

PlotPolar(testlen, testpos, type="l", main="Test Polygon", col="blue")
PolarGrid(ntheta=9, col="grey", lty="solid", lblradians=FALSE)

# spiderweb
posmat <- matrix(sample(2:9,30,TRUE),nrow=3)
PlotPolar(posmat, type="l", main="Spiderweb plot", col=2:4, lwd=1:3)
PolarGrid( nr=NA, ntheta=ncol(posmat), alabels=paste("X", 1:ncol(posmat)
                                                     , sep=""), col="grey", lty="solid" )

# radarplot
data(mtcars)
d.car <- scale(mtcars[1:6,1:7], center=FALSE)
# let's have a palette with thransparent colors (alpha = 32)
cols <- paste(colorRampPalette(c("red","yellow","green","blue"), space = "rgb")(6), "32", sep="")
PlotPolar(d.car, type="l", fill=cols, main="Cars in radar")
PolarGrid(nr=NA, ntheta=ncol(d.car), alabels=colnames(d.car), lty="solid", col="black")
par(old.par)


# PlotBag: Two-dimensional Boxplot
d.frm <- d.pizza[complete.cases(d.pizza[,c("temperature","delivery_min")]),]
PlotBag( x=d.frm$delivery_min, y=d.frm$temperature
         , xlab="delivery_min", ylab="temperature", main="Two-dimensional Boxplot")

# Chernoff faces
par(old.par)
m <- data.frame( lapply( d.pizza[,c("temperature","price","delivery_min","wine_ordered","weekday")]
                         , tapply, d.pizza$driver, mean, na.rm=TRUE))
PlotFaces(m, main = "Driver's characteristics")


# PlotWeb
m <- cor(d.pizza[, sapply(d.pizza, IsNumeric, na.rm=TRUE)], use="pairwise.complete.obs")
PlotWeb(m, xpd=TRUE, main="Pizza Correlations" )

PlotCorr(m, cols=colorRampPalette(c("red", "black", "green"), space = "rgb")(20))
mtext("Correlation plot", side=3, line=3, font=2, cex=1.5)


# histograms were yesterday, use marginal densities instead
# would be best with: x11(7.5,4.7)
PlotMarDens( y=d.pizza$temperature, x=d.pizza$delivery_min, grp=d.pizza$area
             , xlab="delivery_min", ylab="temperature", col=c("brown","orange","lightsteelblue")
             , panel.first= grid()
)






