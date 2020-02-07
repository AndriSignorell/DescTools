library(DescTools)
library(haven) #for read_dta
library(MASS) #for polr
library(nnet) #for multinom
library(VGAM)


##### ISSUE A #####
#PseudoR2 computes null models based on a call to glm with no variables (" ~ .") and a "data" parameter, taken from the original model object
#If cases are removed from the original (non-null) model based on NAs, these cases will *not* be removed from the null model
#The null model will be fitted on a different set of data, producing a bad comparison between models

## ==== Binomial logistic reg ====

# ---- Working as expected ----
hsb2 <- as.data.frame(read_dta("https://stats.idre.ucla.edu/stat/stata/notes/hsb2.dta"))
hsb2$honcomp <- hsb2$write >= 60
model1.logit <- glm(honcomp ~ female + read + science + ses, hsb2, family="binomial")
out1 <- PseudoR2(model1.logit, "a")

# ---- Not working as expected ----
#now let's add missing values to ses
hsb2$ses[hsb2$ses == 1] <- NA

#Manually subset data to remove missing values
model2.logit <- glm(honcomp ~ female + read + science + ses, subset(hsb2, ses >= 2), family="binomial")
out2 <- PseudoR2(model2.logit, "a")

#avoid manual subsetting, and instead count on the modelling process to avoid missing data
model3.logit <- glm(honcomp ~ female + read + science + ses, hsb2, family="binomial")
out3 <- PseudoR2(model3.logit, "a")

#compare outcomes - notice they don't match
out2
out3

# ---- Explanation ----

#The log-likelihood of the main models match
logLik(model2.logit)
logLik(model3.logit)

#However, the log-likelihood of the null models don't match
#the calculation of likelihood for the null model works on the entire dataset specified in x$data, without removing missing values
logLik(glm(formula = reformulate('1', 
                                 # replace the right side of the formula by 1                                           
                                 gsub(" .*$", "", 
                                      # not all glms have a formula element, e.g. MASS::negbin                                           
                                      deparse(unlist(list(model2.logit$formula, model2.logit$call$formula, formula(model2.logit)))[[1]]))),
           # use the first non null list element
           # note model2.logit$call$data is a symbol and must first be evaluated
           data = Filter(Negate(is.null), list(model2.logit$data, eval(model2.logit$call$data) ))[[1]],
           family = model2.logit$family))

logLik(glm(formula = reformulate('1', 
                                 # replace the right side of the formula by 1                                           
                                 gsub(" .*$", "", 
                                      # not all glms have a formula element, e.g. MASS::negbin                                           
                                      deparse(unlist(list(model3.logit$formula, model3.logit$call$formula, formula(model3.logit)))[[1]]))),
           # use the first non null list element
           # note model3.logit$call$data is a symbol and must first be evaluated
           data = Filter(Negate(is.null), list(model3.logit$data, eval(model3.logit$call$data) ))[[1]],
           family = model3.logit$family))

#Given that it is common (and often accepted) to let "glm" automatically delete missing values, this could cause incorrect evaluations of null-model log likelihood with some frequency
#We are better off using the "model" parameter, which removes cases that are not used for analysis

# ---- Working but risky ----

#Define formula externally
logit.form <- formula(honcomp ~ female + read + science + ses)
model4.logit <- glm(logit.form, hsb2, family="binomial")
rm(logit.form)
PseudoR2(model4.logit)

#Pseudo-R2 checks three places for a glm formula - the second (after x$formula) is the function call
#if this references an external object (or a changed object) we will have problems
#The precautionary solution is to look in formula(x) before call$x$formula


##### ISSUE B #####
#Model types other than GLM generate model by running "update" on the original function call
#This quite literally re-runs the call, referencing the *current* data object with the saved name - as well as current objects for other elements of the call
#This will cause problems if the data frame (or another variable used in the original function call) has been changed after the original was created
#Update will re-run the model using the *changed* variables, and produce a null model from different parameters


# ==== POLR ====

# ---- Working as expected ----
#Generate model
rec.df <- hsb2
which.method <- "probit"
rec.df$ordvar <- cut(rec.df$write, breaks = c(30,40,50,60,70))
model1.polr <- polr(ordvar ~ female + read + science, rec.df)

which.method <- "probit"
model2.polr <- polr(ordvar ~ female + read + science, rec.df, method = which.method)

#Correct behaviour (these shouldn't match)
out1.polr <- PseudoR2(model1.polr)
out2.polr <- PseudoR2(model2.polr)

# ---- Not working as expected ----
#Let's say we then drop cases from the original data frame
rec.df <- rec.df[!is.na(rec.df$ses),]
rec.df[!is.na(rec.df$ses),]
out3.polr <- PseudoR2(model1.polr)

#Outcomes don't match
out1.polr 
out3.polr

#if we delete the variable that was used to specify method, we will get an error
rm(which.method)
out4.polr <- PseudoR2(model2.polr)

out2.polr
out4.polr


# ==== multinom ====

# ---- working as expected ----
#The same issue arises with multinom
rec.df <- hsb2
model1.multinom <- multinom(factor(race) ~ read + write + math, data = rec.df)
PseudoR2(model1.multinom)

#If the data frame is deleted, we can't compute PseudoR2
rm(rec.df)
PseudoR2(model1.multinom)



# ==== notes ===

#Additional checks needed:
#substitute, weight, and na.action parameters for each model type
#non-literal DVs (IE, "ifelse(y_cont > =5) ~ x1 + x2)

#Ensure parameters should work when they are A) explicitly defined, B) found in environment, C) not found