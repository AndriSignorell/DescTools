library(DescTools)
library(haven) #for read_dta
library(MASS) #for polr
library(nnet) #for multinom
library(VGAM)
library(plyr) #for testing recode of factor, using revalue
source("R/LinMod.R")


# ==== notes ===

#checks needed:
# A) Ensure data parameter should work it is 1) explicitly defined, 2 found in environment, 3) not found
# B) Check "special" parameters (substitute, weight, and na.action parameters)
# C) check non-literal variables

hsb2 <- as.data.frame(read_dta("https://stats.idre.ucla.edu/stat/stata/notes/hsb2.dta"))
hsb2$honcomp <- hsb2$write >= 60

hsb2$write_cat <- cut(hsb2$write, breaks = c(30,40,50,60,70))
hsb2$race_cat <- factor(hsb2$race)


# ==== GLM ====

#"Data" and "model" object components are both usable (we give priority to model)
base.logit <- glm(honcomp ~ female + read + science + ses, hsb2, family="binomial")
PseudoR2(base.logit)

#"Data" object is reference to global environment (but we have a model object)
base2.logit <- glm(hsb2$honcomp ~ hsb2$female + hsb2$read + hsb2$science + hsb2$ses, family="binomial")
PseudoR2(base2.logit)

#POSSIBLE ISSUE: no model object (only data), and a non-literal DV (eg, read > 60)
#A1 tests are covered above

#A2a - variables in global environment
y <- hsb2$honcomp
test_a2.logit <- glm(y ~ hsb2$female + hsb2$read + hsb2$science + hsb2$ses, family="binomial", model = FALSE)
PseudoR2(test_a2.logit)
#NB: doesn't give useful name of object that needs new evaluation

#A3a - "data" object is reference to global environment
z <- hsb2$honcomp
test_a3a.logit <- glm(z ~ hsb2$female + hsb2$read + hsb2$science + hsb2$ses, family="binomial", model = FALSE, x = TRUE, y = TRUE)
rm(z)
PseudoR2(test_a3a.logit)

#A3b - "data" object is contained in data frame
tempdf <- hsb2
test_a3b.logit <- glm(honcomp ~ female + read + science + ses, tempdf, family="binomial", model = FALSE)
PseudoR2(test_a3b.logit)
tempdf <- tempdf[1:100,]
PseudoR2(test_a3b.logit)
rm(tempdf)
PseudoR2(test_a3b.logit)


# ---- B ----
#WEIGHTS
#Weights are created on-the-fly via runif
test_b1.logit <- glm(honcomp ~ female + read + science + ses, hsb2, family="binomial", weights = runif(nrow(hsb2)), model = FALSE)
PseudoR2(test_b1.logit)
PseudoR2(test_b1.logit)

#Weights are saved
test_weights <- runif(nrow(hsb2))
test_b2.logit <- glm(honcomp ~ female + read + science + ses, data = hsb2, family="binomial", weights = test_weights, model = FALSE)
rm(test_weights)
PseudoR2(test_b2.logit)

withna.df <- rbind(hsb2[1:100,], NA, NA, hsb2[101:200,])
test_b3.logit <- glm(honcomp ~ female + read + science + ses, data = withna.df, family="binomial", weights = runif(nrow(withna.df)), model = FALSE)
PseudoR2(test_b3.logit)

#NA.ACTION
#Could try using the na.omit attribute of glms here to handle, but it's a lot of work for little return
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


# ---- C ----

#DV, With model
test_c1.logit <- glm((hsb2$write_cat == "(30,40]" | hsb2$write_cat == "(40,50]") ~ female + read + science + ses, hsb2, family="binomial")
PseudoR2(test_c1.logit)

#DV, without model, out of data frame
test_c2.logit <- glm((hsb2$write_cat == "(30,40]" | hsb2$write_cat == "(40,50]") ~ hsb2$female + hsb2$read + hsb2$science + hsb2$ses, family="binomial", model = FALSE)
PseudoR2(test_c2.logit)

#DV, without model, in data frmae
test_c3.logit <- glm((write_cat == "(30,40]" | hsb2$write_cat == "(40,50]") ~ female + read + science + ses, hsb2, family="binomial", model = FALSE)
PseudoR2(test_c3.logit)

#IV, without model, no data frame
test_c4.logit <- glm(hsb2$honcomp ~ hsb2$female + (hsb2$read > 50) + hsb2$science + hsb2$ses, family="binomial", model = FALSE)
PseudoR2(test_c4.logit)

#IV, without model, with data frame
test_c5.logit <- glm(honcomp ~ female + (read > 50) + science + ses, family="binomial", data = hsb2, model = FALSE)
PseudoR2(test_c5.logit)


# ==== POLR ====

#"Data" and "model" object components are both usable (we give priority to model)
base.polr <- polr(write_cat ~ female + read + science + ses, hsb2)
PseudoR2(base.polr)

# ---- A ----

#A1: polr does not return "data" component, so explicit reference is impossible

#A2: polr object "call" component references valid data frame
#Unlike in glm, polr doesn't save a data object
test_a2.polr <- polr(write_cat ~ female + read + science + ses, hsb2, model = FALSE)
PseudoR2(test_a2.polr)

#POSSIBLE ISSUE: no model object (only data), and a non-literal DV (eg, read > 60)

#A3
#"call" references objects in global environment
y <- cut(hsb2$write, breaks = c(30,40,50,60,70))
test_a3a.polr <- polr(y ~ hsb2$female + hsb2$read + hsb2$science + hsb2$ses, model = FALSE)
PseudoR2(test_a3a.polr)

#A3b - "call" references invalid data frame
tempdf <- hsb2
test_a3b.polr <- polr(write_cat ~ female + read + science + ses, data = tempdf, model = FALSE)
rm(tempdf)
PseudoR2(test_a3b.polr)



# ---- B ----
#WEIGHTS
#Weights are created on-the-fly via runif
test_b1.polr <- polr(write_cat ~ female + read + science + ses, hsb2, weights = runif(nrow(hsb2)), model = FALSE)
PseudoR2(test_b1.polr)

#Weights are saved
test_weights <- runif(nrow(hsb2))
test_b2.polr <- polr(write_cat ~ female + read + science + ses, weights = test_weights, data = hsb2, model = FALSE)
PseudoR2(test_b2.polr)
rm(test_weights)
PseudoR2(test_b2.polr)

withna.df <- rbind(hsb2[1:100,], NA, NA, hsb2[101:200,])
test_b3.polr <- polr(write_cat ~ female + read + science + ses, data = withna.df, weights = runif(nrow(withna.df)), model = FALSE)
PseudoR2(test_b3.polr)

#NA.ACTION
test_b4.polr <- polr(write_cat ~ female + read + science + ses, data = rbind(hsb2, NA), model = FALSE, na.action = na.omit)
PseudoR2(test_b4.polr)

test_naAction <- na.omit
test_b5.polr <-  polr(write_cat~ female + read + science + ses, data = rbind(hsb2, NA), model = TRUE, na.action = test_naAction)
PseudoR2(test_b5.polr)
test_naAction <- na.fail
PseudoR2(test_b5.polr)

test_naAction <- na.omit
test_b6.logit <-  glm(honcomp ~ female + read + science + ses, data = rbind(hsb2, NA), family="binomial", model = FALSE, na.action = test_naAction)
PseudoR2(test_b6.logit)
test_naAction <- na.fail
PseudoR2(test_b6.logit)

# ---- C ----


#DV, without model, out of data frame
test_c1.polr <- polr(revalue(write_cat, c("(30,40]" = "(30,50]", "(40,50]" = "(30,50]")) ~ female + read + science + ses, hsb2, model = FALSE)
PseudoR2(test_c1.polr)

#DV, without model, in data frmae
test_c2.polr <- polr(revalue(hsb2$write_cat, c("(30,40]" = "(30,50]", "(40,50]" = "(30,50]")) ~ hsb2$female + hsb2$read + hsb2$science + hsb2$ses, model = FALSE)
PseudoR2(test_c2.polr)

#IV, without model, no data frame
test_c3.polr <- polr(hsb2$write_cat ~ hsb2$female + (hsb2$read > 50) + hsb2$science + hsb2$ses, model = FALSE)
PseudoR2(test_c3.polr)

#IV, without model, with data frame
test_c4.polr <- polr(write_cat ~ female + (read > 50) + science + ses, hsb2, model = FALSE)
PseudoR2(test_c4.polr)


# ==== Multinom ====

#"Data" and "model" object components are both usable (we give priority to model)
base.multinom <- multinom(race_cat ~ female + read + science + ses, hsb2, model = TRUE)
PseudoR2(base.multinom)

# ---- A ----

#A1: multinom does not return "data" component, so expciCit reference is impossible

#A2: multinom object "call" component references valid data frame
test_a2.multinom <- multinom(race_cat ~ female + read + science + ses, hsb2, model = FALSE)
PseudoR2(test_a2.multinom)

#A3
#Unlike in glm, multinom won't save an enviornment labelled as "data"
#"call" references objects in global environment
#NOTE: could theoretically get the variables from call$formula, althiugh this would be risky
y_nominal <- hsb2$race_cat
test_a3a.multinom <- multinom(y_nominal ~ hsb2$female + hsb2$read + hsb2$science + hsb2$ses, model = FALSE)
PseudoR2(test_a3a.multinom)

#A3b - "call" references invalid data frame
tempdf <- hsb2
test_a3b.multinom <- multinom(race_cat ~ female + read + science + ses, data = tempdf, model = FALSE)
PseudoR2(test_a3b.multinom)
rm(tempdf)
PseudoR2(test_a3b.multinom)



# ---- B ----
#WEIGHTS
#Weights are created on-the-fly via runif
#Multinom saves a "weights" element, which is equivalent to the glm "prior.weights" element
test_b1.multinom <- multinom(race_cat ~ female + read + science + ses, hsb2, weights = runif(nrow(hsb2)), model = TRUE)
PseudoR2(test_b1.multinom)

#Weights are saved
test_weights <- runif(nrow(hsb2))
test_b2.multinom <- multinom(race_cat ~ female + read + science + ses, weights = test_weights, data = hsb2, model = FALSE)
PseudoR2(test_b2.multinom)
rm(test_weights)
PseudoR2(test_b2.multinom)

withna.df <- rbind(hsb2[1:100,], NA, NA, hsb2[101:200,])
test_b3.multinom <- multinom(race_cat  ~ female + read + science + ses, data = withna.df, weights = runif(nrow(withna.df)), model = FALSE)
PseudoR2(test_b3.multinom)

#NA.ACTION
test_b4.multinom <- multinom(race_cat  ~ female + read + science + ses, data = rbind(hsb2, NA), model = FALSE, na.action = na.omit)
PseudoR2(test_b4.multinom)

test_naAction <- na.omit
test_b5.multinom <-  multinom(race_cat ~ female + read + science + ses, data = rbind(hsb2, NA), model = TRUE, na.action = test_naAction)
PseudoR2(test_b5.multinom)
test_naAction <- na.fail
PseudoR2(test_b5.multinom)

#QUIETLY RE-FIT MULTINOM

# ---- C ----

#DV, With model
test_c1.multinom <- multinom(revalue(race_cat, c("1" = "1", "2" = "1")) ~ female + read + science + ses, hsb2, model = TRUE)
PseudoR2(test_c1.multinom)

#DV, without model, out of data frame
test_c2.multinom <- multinom(revalue(hsb2$race_cat, c("1" = "1", "2" = "1")) ~ hsb2$female + hsb2$read + hsb2$science + hsb2$ses, model = FALSE)
PseudoR2(test_c2.multinom)

#DV, without model, in data frame
test_c2.multinom <- multinom(revalue(race_cat, c("1" = "1", "2" = "1")) ~ female + read + science + ses, hsb2, model = FALSE)
PseudoR2(test_c2.multinom)

#IV, without model, no data frame
test_c4.multinom <- multinom(hsb2$race_cat ~ hsb2$female + (hsb2$read > 50) + hsb2$science + hsb2$ses, model = FALSE)
PseudoR2(test_c4.multinom)

#IV, without model, with data frame
test_c5.multinom <- multinom(race_cat ~ female + (read > 50) + science + ses, data = hsb2, model = FALSE)
PseudoR2(test_c5.multinom)



# === VGLM ====

#Because of the very wide variety of possible VGLM models and related parameters + functional forms, we can't easily take the same testing approach as above
#We'll instead starty by testing the models listed in the VGAM help file

# Example 1. See help(glm)
print(d.AD <- data.frame(treatment = gl(3, 3),
                         outcome = gl(3, 1, 9),
                         counts = c(18,17,15,20,10,20,25,13,12)))
vglm.D93 <- vglm(counts ~ outcome + treatment, family = poissonff,
                 data = d.AD, trace = TRUE, model = TRUE)
summary(vglm.D93)
PseudoR2(vglm.D93)


# Example 2. Multinomial logit model
pneumo <- transform(pneumo, let = log(exposure.time))
vglm.pneumo <- vglm(cbind(normal, mild, severe) ~ let, multinomial, data = pneumo, model = TRUE)
PseudoR2(vglm.pneumo)

# Example 3. Proportional odds model
fit3 <- vglm(cbind(normal, mild, severe) ~ let, propodds, data = pneumo, model = TRUE)
PseudoR2(fit3)

# Example 4. Bivariate logistic model
fit4 <- vglm(cbind(nBnW, nBW, BnW, BW) ~ age, binom2.or, coalminers, model = TRUE)
PseudoR2(fit4)


# Example 5. The use of the xij argument (simple case).
# The constraint matrix for 'op' has one column.
nn <- 1000
eyesdat <- round(data.frame(lop = runif(nn),
                            rop = runif(nn),
                            op = runif(nn)), digits = 2)
eyesdat <- transform(eyesdat, eta1 = -1 + 2 * lop,
                     eta2 = -1 + 2 * lop)
eyesdat <- transform(eyesdat,
                     leye = rbinom(nn, size = 1, prob = logit(eta1, inverse = TRUE)),
                     reye = rbinom(nn, size = 1, prob = logit(eta2, inverse = TRUE)))
fit5 <- vglm(cbind(leye, reye) ~ op,
             binom2.or(exchangeable = TRUE, zero = 3),
             data = eyesdat, trace = TRUE,
             xij = list(op ~ lop + rop + fill(lop)),
             form2 = ~  op + lop + rop + fill(lop),
             model = TRUE)
PseudoR2(fit5)