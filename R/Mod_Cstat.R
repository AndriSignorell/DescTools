
# Cstat

# Cstat <- function(x){
#
#   y <- as.numeric(factor(model.response(x$model)))
#
#   probs <- predict(x, type = "response")
#   d.comb <- expand.grid(pos = probs[y == 2L],
#                         neg = probs[y == 1L])
#
#   mean(d.comb$pos > d.comb$neg)
#
# }


Cstat <- function (x, ...){


  UseMethod("Cstat")
}


Cstat.glm <- function(x, ...) {
  Cstat.default(predict(x, type = "response"), model.response(x$model))
}


Cstat.default <- function(x, resp, ...) {


  # this is algorithmically clumsy and O(n^2)
  # the response ("class")
  # y <- as.numeric(factor(resp))
  #
  # prob <- x # predicted probs
  # d.comb <- expand.grid(pos = prob[y == 2L],
  #                       neg = prob[y == 1L])
  #
  # mean(d.comb$pos > d.comb$neg)

  # ... instead of elegant O(n log(n))
  # changed by 0.99.27
  z <- .DoCount(as.numeric(factor(resp)), x)
  return((z$C + 0.5*z$T)/(z$D+z$C+z$T))

}

# test:
# resp <- c(1,1,0,0)
# pred <- c(1,1,1,0)
# model <- glm(resp~pred, family = binomial())
#
# Cstat(model)
# Cstat(pred, resp = resp)
# ROC(FitMod(resp~pred, fitfn="logit"))$auc


# alternative
# library(rms)
# rcorr.cens(pred, resp)


# Example Code from:
# https://www.listendata.com/2014/08/learn-area-under-curve-auc.html
#
# # Read Data
# df = read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
#
# # Factor Variables
# df$admit = as.factor(df$admit)
# df$rank = as.factor(df$rank)
#
# # Logistic Model
# df$rank <- relevel(df$rank, ref='4')
# mylogistic <- glm(admit ~ ., data = df, family = "binomial")
# summary(mylogistic)$coefficient
#
# # Predict
# pred = predict(mylogistic, type = "response")
# finaldata = cbind(df, pred)
#
#
# AUC <- function (actuals, predictedScores){
#   fitted <- data.frame (Actuals=actuals, PredictedScores=predictedScores)
#   colnames(fitted) <- c('Actuals','PredictedScores')
#   ones <- fitted[fitted$Actuals==1, ] # Subset ones
#   zeros <- fitted[fitted$Actuals==0, ] # Subsetzeros
#   totalPairs <- nrow (ones) * nrow (zeros) # calculate total number of pairs to check
#   conc <- sum (c(vapply(ones$PredictedScores, function(x) {((x > zeros$PredictedScores))}, FUN.VALUE=logical(nrow(zeros)))), na.rm=T)
#   disc <- sum(c(vapply(ones$PredictedScores, function(x) {((x < zeros$PredictedScores))}, FUN.VALUE = logical(nrow(zeros)))), na.rm = T)
#   concordance <- conc/totalPairs
#   discordance <- disc/totalPairs
#   tiesPercent <- (1-concordance-discordance)
#   AUC = concordance + 0.5*tiesPercent
#   Gini = 2*AUC - 1
#   return(list("Concordance"=concordance, "Discordance"=discordance,
#               "Tied"=tiesPercent, "AUC"=AUC, "Gini or Somers D"=Gini))
# }
#
# AUC(finaldata$admit, finaldata$pred)

