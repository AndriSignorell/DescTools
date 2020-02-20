library(DescTools)
library(haven) #for read_dta
library(MASS) #for polr
library(nnet) #for multinom
library(VGAM)


# ==== notes ===

#checks needed:
# A) Ensure data parameter should work it is 1) explicitly defined, 2 found in environment, 3) not found
# B) Check "special" parameters (substitute, weight, and na.action parameters)
# C) Ensure appropriate behaviour regardless of x, y, and data parameters


# ==== GLM ====

hsb2 <- as.data.frame(read_dta("https://stats.idre.ucla.edu/stat/stata/notes/hsb2.dta"))
hsb2$honcomp <- hsb2$write >= 60

#"Data" and model objects are both safe
base.logit <- glm(honcomp ~ female + read + science + ses, hsb2, family="binomial")
PseudoR2(base.logit)

#"Data" object is reference to global environment (but we have a modekl object)
base2.logit <- glm(hsb2$honcomp ~ hsb2$female + hsb2$read + hsb2$science + hsb2$ses, family="binomial")
PseudoR2(base2.logit)


# ---- A ----
#A1 tests are covered above

#A2
y <- hsb2$honcomp
test_a2.logit <- glm(y ~ hsb2$female + hsb2$read + hsb2$science + hsb2$ses, family="binomial", model = FALSE)
PseudoR2(test_a2.logit)

#A3a - "data" object is reference to global environment
#Consider more informative error messages?
z <- hsb2$honcomp
test_a3a.logit <- glm(z ~ hsb2$female + hsb2$read + hsb2$science + hsb2$ses, family="binomial", model = FALSE, x = TRUE, y = TRUE)
rm(z)
PseudoR2(test_a3a.logit)

#A3b - "data" object is contained in data frame
tempdf <- hsb2
test_a3b.logit <- glm(honcomp ~ female + read + science + ses, tempdf, family="binomial", model = FALSE)
rm(tempdf)
PseudoR2(test_a3b.logit)



# ---- B ----
#WEIGHTS
#Weights are created on-the-fly via runif
test_b1.logit <- glm(honcomp ~ female + read + science + ses, hsb2, family="binomial", weights = runif(nrow(hsb2)), model = FALSE)
PseudoR2(test_b1.logit)

#Weights are saved
test_weights <- runif(nrow(hsb2))
test_b2.logit <- glm(hsb2$honcomp ~ hsb2$female + hsb2$read + hsb2$science + hsb2$ses, family="binomial", weights = test_weights, model = FALSE)
rm(test_weights)
PseudoR2(test_b2.logit)

withna.df <- rbind(hsb2[1:100,], NA, NA, hsb2[101:200,])
test_b3.logit <- glm(honcomp ~ female + read + science + ses, data = withna.df, family="binomial", weights = runif(nrow(withna.df)), model = FALSE)
PseudoR2(test_b3.logit)

#What happens if weight is combined with subset/na's? We're fine if we have a model parameter, or the original data parameter
#However, if "data" referneces an environment or is omitted, it's possible that weight will be the wrong length
#This will generate an error so its okay (a re-sorted dataframe is the main risk, but we generate a warning for this)

#NA.ACTION
test_b4.logit <- glm(honcomp ~ female + read + science + ses, data = rbind(hsb2, NA), family="binomial", model = FALSE, na.action = na.omit)
PseudoR2(test_b4.logit)

test_naAction <- na.omit
test_b5.logit <-  glm(honcomp ~ female + read + science + ses, data = rbind(hsb2, NA), family="binomial", model = TRUE, na.action = test_naAction)
rm(test_naAction)
PseudoR2(test_b5.logit)

test_naAction <- na.omit
test_b6.logit <-  glm(honcomp ~ female + read + science + ses, data = rbind(hsb2, NA), family="binomial", model = FALSE, na.action = test_naAction)
rm(test_naAction)
PseudoR2(test_b6.logit)





# ---- 

