## -----------------------------------------------------------------------------------
## Demo file for the description functions of DescTools; start with 'demo(describe)'
## -----------------------------------------------------------------------------------


# Descriptions **************
# use a subset of built-in data.frame
d.sub <- d.pizza[,c("temperature","driver","delivery_min","count","operator","date","wine_ordered")]


# all univariate descriptions
Desc(d.sub, plotit=TRUE)

# just a few groupwise descriptions on the console
Desc(temperature ~ driver, d.pizza, plotit=TRUE)
Desc(driver ~ temperature, d.pizza, plotit=TRUE)
Desc(temperature ~ delivery_min, d.pizza, plotit=TRUE)
Desc(quality ~ driver, d.pizza, plotit=TRUE, rfrq="111") # show all rel. frequencies


# generate all pairwise descriptions
m <- outer(names(d.sub), names(d.sub), paste, sep=" ~ ")  
m <- m[lower.tri(m)]         # take only lower triangular
m <- sapply(m, formula)   # 

# .. do it
out <- sapply(m, Desc, data=d.pizza, plotit=TRUE)


# do n:1 
Desc( . ~ driver, d.sub, plotit=TRUE)


# set options - and be lazy typewriter...
options(plotit=TRUE, col1="olivedrab1", col2="yellow")


# without data parameter
Desc(d.pizza$delivery_min ~ d.pizza$driver, digits=1)

# use functions and interactions
Desc(sqrt(price) ~ operator : factor(wrongpizza), data=d.pizza, digits=2) 
Desc(log(price+1) ~ cut(delivery_min, breaks=seq(10,90,10)),  
     data=d.pizza, digits=c(2,3,4,2,0,3,0,0)) 



# ******************************************************************************
# The following code presumes, that you are on Windows and have Office installed!
# ******************************************************************************


# (we can't run this example in CRAN environment, but here's how it would work)

# export results to Word by just declaring a new output device

# wrd <- GetNewWrd(header=TRUE)
# Desc(d.pizza[,c("driver","price","wrongpizza","date","count")], wrd=wrd)
# 
# Desc( price ~ driver + operator, d.pizza, digits=c(1,1,1,1,0,3,0,0), wrd=wrd )
# Desc( factor(weekday) ~ driver, d.pizza, digits=c(1,1,1,1,0,3,0,0), wrd=wrd)
# 
# Desc( temperature ~ delivery_min, d.pizza, digits=c(1,1,1,1,0,3,0,0), wrd=wrd )
# Desc( log(temperature) ~ log(delivery_min), d.pizza, digits=c(1,1,1,1,0,3,0,0), wrd=wrd )
# 
# Desc( log(price+1) ~ cut(delivery_min, breaks=seq(10,90,10))  
#       , data=d.pizza, digits=c(2,2,2,2,0,3,0,0), wrd=wrd) 





