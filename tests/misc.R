

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

set.seed(45)
(z <- as.numeric(names(w <- table(x <- sample(-10:20, size=50, r=T)))))

stopifnot(all(
  identical(Mode(5), structure(5, freq = 1L))
  , identical(Mode(NA), structure(NA_real_, freq = NA_integer_)) 
  , identical(Mode(c(NA, NA)), structure(NA_real_, freq = NA_integer_)) 
  , identical(Mode(c(NA, 0:5)), structure(NA_real_, freq = NA_integer_)) 
  , identical(Mode(c(NA, 0:5), na.rm=TRUE), structure(NA_real_, freq = 1L)) 
  , identical(Mode(c(NA, 0:5, 5), na.rm=TRUE), structure(5, freq = 2L)) 
  , identical(Mode(c(0:5, 4, 5, 6)), structure(c(4, 5), freq = 2L)) 
  , identical(Mode(c(0:8, rep(c(1,3, 8), each=5))), structure(c(1, 3, 8), freq = 6L)) 
  
  , all.equal(Kurt(x = z, weights = w, method = 1), Kurt(x = x, method = 1))
  , all.equal(Kurt(x = z, weights = w, method = 2), Kurt(x = x, method = 2))
  , all.equal(Kurt(x = z, weights = w, method = 3), Kurt(x = x, method = 3))
  
  , all.equal(Skew(x = z, weights = w, method = 1), Skew(x = x, method = 1))
  , all.equal(Skew(x = z, weights = w, method = 2), Skew(x = x, method = 2))
  , all.equal(Skew(x = z, weights = w, method = 3), Skew(x = x, method = 3))
  
  , all.equal(CoefVar(z, weights = w, unbiased = TRUE), CoefVar(x, unbiased = TRUE))
  , all.equal(CoefVar(z, weights = w, unbiased = FALSE), CoefVar(x, unbiased = FALSE))
  
  , all.equal(MeanAD(x), MeanAD(z, w))
  , all.equal(MeanAD(x, center = Median), MeanAD(z, w, center = Median))
  , all.equal(MeanAD(x, center = 7), MeanAD(z, w, center = 7))
  
))



# test Desc base function

x <- c(rnorm(n = 100, sd = 10), NA) 
z <- Desc(x)[[1]]

stopifnot(all(  
  identical(z$length,    length(x))
  , identical(z$NAs,     sum(is.na(x)))
  , identical(z$unique,  length(unique(na.omit(x))))
  , identical(z$`0s`,    sum(x==0, na.rm=TRUE))
  , IsZero(z$mean - mean(x, na.rm=TRUE))
  , identical(unname(z$quant), 
              unname(quantile(x, na.rm=TRUE, probs=c(0,0.05,.1,.25,.5,.75,.9,.95,1))))
  , identical(z$range,   diff(range(x, na.rm=TRUE)))
  , IsZero(z$sd - sd(x, na.rm=TRUE))
  , IsZero(z$vcoef - sd(x, na.rm=TRUE)/mean(x, na.rm = TRUE))
  , identical(z$mad,     mad(x, na.rm=TRUE))
  , identical(z$IQR,     IQR(x, na.rm=TRUE))
))




# test BinomDiffCI with https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf

# 5. Mee is given as 0.0533 in the literature, which probably is a rounding error
# it's corrected from 0.533 to 0.534 in ‘lit1’ and from 0.7225 to 0.7224 in ‘lit2’ for comparison reasons
# Mee 4 from  0.0857 to 0.0858

meth <- c("wald","waldcc","hal","jp","mee","mn","score","scorecc","ha","ac","blj")

stopifnot(all(
  identical(unname(round(BinomDiffCI(56, 70, 48, 80, method = meth), 4)[, -1]), 
            cbind(c(0.0575, 0.0441, 0.0535, 0.0531, 0.0534, 
                    0.0528, 0.0524, 0.0428, 0.0494, 0.0525, 0.054), 
                  c(0.3425, 0.3559, 0.3351, 0.3355, 0.3377, 
                    0.3382, 0.3339, 0.3422, 0.3506, 0.3358, 0.34))),
  identical(unname(round(BinomDiffCI(9, 10, 3, 10, method = meth), 4)[, -1]), 
            cbind(c(0.2605, 0.1605, 0.1777, 0.176, 0.1821, 
                    0.17, 0.1705, 0.1013, 0.1922, 0.16, 0.1869), 
                  c(0.9395, 1, 0.8289, 0.8306, 0.837, 0.8406, 
                    0.809, 0.8387, 1, 0.84, 0.904))),
  identical(unname(round(BinomDiffCI(10, 10, 0, 20, method = meth), 4)[, -1]), 
            cbind(c(1, 0.925, 0.7482, 0.7431, 0.7224, 0.7156, 
                    0.6791, 0.6014, 0.95, 0.6922, 0.7854), 
                  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))), 
  identical(unname(round(BinomDiffCI(84, 101, 89, 105, method = meth), 4)[, -1]), 
            cbind(c(-0.1162, -0.1259, -0.1152, -0.116, -0.1188, 
                    -0.1191, -0.1177, -0.1245, -0.1216, -0.1168, -0.117), 
                  c(0.0843, 0.094, 0.0834, 0.0843, 0.0858, 0.086, 0.0851, 
                    0.0918, 0.0898, 0.085, 0.0852)))
))



# test for median, calculated by Quantile
x <- sample(19, 30, replace = TRUE)
z <- as.numeric(names(w <- table(x)))
stopifnot(AllIdentical(Median(z, weights=w), Median(x), median(x), Median(c(x, NA, NA), na.rm=TRUE)))

x <- sample(40, 30, replace = TRUE)
z <- as.numeric(names(w <- table(x)))
stopifnot(AllIdentical(Median(z, weights=w), Median(x), median(x), Median(c(x, NA, NA), na.rm=TRUE)))

x <- runif(40)
z <- as.numeric(names(w <- table(x)))
stopifnot(AllIdentical(Median(z, weights=w), Median(x), median(x), Median(c(x, NA, NA), na.rm=TRUE)))


