

Conf <- function(x, ...) UseMethod("Conf")


Conf.table <- function(x, pos = NULL, ...) {
  
  CollapseConfTab <- function(x, pos = NULL, ...) {
    
    if(nrow(x) > 2) {
      names(attr(x, "dimnames")) <- c("pred", "obs")
      x <- CollapseTable(x, obs=c("neg", pos)[(rownames(x)==pos)+1],
                         pred=c("neg", pos)[(rownames(x)==pos)+1])
    }
    
    # order confusion table so
    # that the positive class is the first and the others keep their position
    ord <- c(pos, rownames(x)[-grep(pos, rownames(x), fixed=TRUE)])
    # the columnnames must be the same as the rownames
    x <- as.table(x[ord, ord])
    return(x)
  }
  
  p <- (d <- dim(x))[1L]
  if(!is.numeric(x) || length(d) != 2L || p != d[2L]) {    # allow nxn!  || p != 2L)
    stop("'x' is not a nxn numeric matrix.")
    # print(x)
    # invisible()
  }
  
  # observed in columns, predictions in rows
  if(!identical(rownames(x), colnames(x)))
    stop("rownames(x) and colnames(x) must be identical")
  
  if(is.null(pos)) pos <- rownames(x)[1]
  if(nrow(x)!=2) {
    # ignore pos for nxn tables, pos makes only sense for sensitivity
    # and that is not defined for n-dim tables
    pos <- NULL
    
  } else {
    # order 2x2-confusion table so
    # that the positive class is the first and the others keep their position
    # fixed=TRUE as we might run into problems with columnnames like (8-9] ...
    ord <- c(pos, rownames(x)[-grep(pos, rownames(x), fixed=TRUE)])
    # the columnnames must be the same as the rownames
    x <- as.table(x[ord, ord])
  }
  
  # overall statistics first
  res <- list(
    table   = x,
    pos     = pos,
    diag    = sum(diag(x)),
    n       = sum(x)
  )
  res <- c(res,
           acc     = BinomCI(x=res$diag, n=res$n),
           sapply(binom.test(x=res$diag, n=res$n,
                             p=max(apply(x, 2, sum) / res$n),
                             alternative = "greater")[c("null.value", "p.value")], unname),
           kappa   = CohenKappa(x),
           mcnemar = mcnemar.test(x)$p.value
  )
  names(res) <- c("table","pos","diag","n","acc","acc.lci","acc.uci",
                  "nri","acc.pval","kappa","mcnemar.pval")
  
  # byclass
  lst <- list()
  for(i in 1L:nrow(x)){
    
    z <- CollapseConfTab(x=x, pos=rownames(x)[i])
    z[] <- as.double(z)
    A <- z[1, 1]; B <- z[1, 2]; C <- z[2, 1]; D <- z[2, 2]
    
    lst[[i]] <- rbind(
      sens    = A / (A + C),                 # sensitivity
      spec    = D / (B + D),                 # specificity
      ppv     = A / (A + B),                 # positive predicted value
      npv     = D / (C + D),                 # negative predicted value
      prev    = (A + C) / (A + B + C + D),   # prevalence
      detprev = (A + B) / (A + B + C + D),   # detection prevalence
      detrate = A / (A + B + C + D),         # detection rate
      bacc    = mean(c(A / (A + C), D / (B + D)) ),  # balanced accuracy
      fval    = Hmean(c(A / (A + B), A / (A + C)), conf.level = NA), # guetemass wollschlaeger s. 150
      #   this would overflow for already small frequencies if we don't cast z to double ..
      mcc     = (A*D-B*C) / sqrt((A+B)*(A+C)*(D+B)*(D+C))  # Matthews correlation coefficient (=Phi(x) with sign!)      
    )
  }
  
  res <- c(res, byclass=list(do.call(cbind, lst)))
  colnames(res[["byclass"]]) <- rownames(x)
  
  if(nrow(x)==2) res[["byclass"]] <- res[["byclass"]][, res[["pos"]], drop=FALSE]
  
  class(res) <- "Conf"
  
  return(res)
  
}


Conf.default <-  function(x, ref, pos = NULL, na.rm = TRUE, ...) {
  if(na.rm) {
    idx <- complete.cases(data.frame(x, ref))
    x <- x[idx]
    ref <- ref[idx]
  }
  clvl <- CombLevels(x, ref)
  
  Conf.table(table(Prediction=factor(x, levels=clvl), 
                   Reference=factor(ref, levels=clvl)), pos = pos, ...)
}

Conf.matrix <- function(x, pos = NULL, ...) {
  Conf.table(as.table(x), pos=pos, ...)
}


# the confusion interface for rpart
Conf.rpart <- function(x, ...){
  # y <- attr(x, "ylevels")
  Conf(x=attr(x,"ylevels")[x$frame$yval[x$where]],
       ref=attr(x,"ylevels")[x$y], ...)
}

Conf.multinom <- function(x, ...){
  if(is.null(x$model)) stop("x does not contain model. Run multinom with argument model=TRUE!")
  resp <- model.extract(x$model, "response")
  
  # attention: this will not handle correctly responses defined as dummy codes
  # adapt for that!!  ************************************************************
  # resp <- x$response[,1]
  
  pred <- predict(x, type="class")
  Conf(x=pred, resp, ... )
}


Conf.glm <- function(x, cutoff = 0.5, pos=NULL, ...){
  resp <- model.extract(x$model, "response")
  if(is.factor(resp)){
    pred <- levels(resp)[(predict(x, type="response") > cutoff)+1]
    if(is.null(pos)) pos <- levels(resp)[2]
  } else {
    lvl <- levels(factor(resp))
    pred <- lvl[(predict(x, type="response") > cutoff)+1]
    if(is.null(pos)) pos <- lvl[2]
  }
  Conf(x=pred, ref=resp, pos=pos, ... )
}


Conf.randomForest <- function(x, ...){
  Conf(x=x$predicted, ref=x$y, ... )
}


Conf.svm <- function(x, ...){
  
  # old:  Conf(x=predict(x), ref=model.extract(model.frame(x), "response"), ... )
  Conf(x=predict(x, type="class"), ref=model.response(model.frame(x)), ... )
}


Conf.lda <- function(x, ...){
  
  # extract response from the model
  
  Conf(x=predict(x)$class,
       ref=model.extract(model.frame(x), "response") , ... )
}

Conf.qda <- function(x, ...){
  Conf(x=predict(x)$class,
       ref=model.extract(model.frame(x), "response") , ... )
}



Conf.regr <- function(x, ...){
  NextMethod()
  # Conf(x=Predict(x, type="class"), reference=x$response[,], ... )
}



plot.Conf <- function(x, main="Confusion Matrix", ...){
  mosaicplot(t(x$table), shade=TRUE, main=main, col=c("red", "green"), ...)
}


print.Conf <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\nConfusion Matrix and Statistics\n\n")
  
  if(all(names(attr(x$table, "dimnames")) == ""))
    names(attr(x$table, "dimnames")) <- c("Prediction","Reference")
  print(x$table, ...)
  
  if(nrow(x$table)!=2) cat("\nOverall Statistics\n")
  
  txt <- gettextf("
                Total n : %s
               Accuracy : %s
                 95%s CI : (%s, %s)
    No Information Rate : %s
    P-Value [Acc > NIR] : %s

                  Kappa : %s
 Mcnemar's Test P-Value : %s\n\n",
                  Format(x$n, digits=0, big.mark="'"),
                  Format(x$acc, digits=digits), "%",
                  Format(x$acc.lci, digits=digits), Format(x$acc.uci, digits=digits),
                  Format(x$nri, digits=digits), Format(x$acc.pval, fmt="p", na.form="NA"),
                  Format(x$kappa, digits=digits), Format(x$mcnemar.pval, fmt="p", na.form="NA")
  )
  cat(txt)
  
  rownames(x$byclass) <- c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "Prevalence",
                           "Detection Rate", "Detection Prevalence", "Balanced Accuracy","F-val Accuracy", "Matthews Cor.-Coef")
  
  if(nrow(x$table)==2){
    cat(
      paste(StrPad(paste(rownames(x$byclass), ":"), width=25, adj = "right"),
            Format(x$byclass, digits=digits))
      , sep="\n")
    
    txt <- gettextf("\n       'Positive' Class : %s\n\n", x$pos)
    cat(txt)
    
  } else {
    
    cat("\nStatistics by Class:\n\n")
    print(Format(x$byclass, digits = digits, na.form="NA"), quote = FALSE)
    cat("\n")
    
  }
  
}



Sens <- function(x, ...) Conf(x, ...)[["byclass"]]["sens",]

Spec <- function(x, ...) Conf(x, ...)[["byclass"]]["spec",]




# PseudoR2 <- function(x, which = NULL) {
#   
#   # this function will not work with weights, neither with cbind lhs!!
#   # http://stats.stackexchange.com/questions/183699/how-to-calculate-pseudo-r2-when-using-logistic-regression-on-aggregated-data-fil
#   
#   # test: https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-what-are-pseudo-r-squareds/
#   # library(haven)
#   # hsb2 <- as.data.frame(read_dta("https://stats.idre.ucla.edu/stat/stata/notes/hsb2.dta"))
#   # hsb2$honcomp <- hsb2$write >= 60
#   # r.logit <- glm(honcomp ~ female + read + science, hsb2, family="binomial")
#   # PseudoR2(r.logit, "a")
#   
#   
#   
#   # http://digitalcommons.wayne.edu/cgi/viewcontent.cgi?article=2150&context=jmasm
#   # Walker, Smith (2016) JMASM36: Nine Pseudo R^2 Indices for Binary Logistic Regression Models (SPSS)
#   # fuer logit Korrektur https://langer.soziologie.uni-halle.de/pdf/papers/rc33langer.pdf
#   
#   # check with pscl::pR2(x); rcompanion::nagelkerke(x)
#   
#   
#   if (!(inherits(x, what="glm") || inherits(x, what="polr")
#         || inherits(x, what = "multinom") || inherits(x, what = "vglm")))
#     return(NA)
#   
#   
#   if(inherits(x, what="vglm") && !requireNamespace("VGAM", quietly=TRUE)) {
#     stop("Could not find package 'VGAM' - please install first") }
#   
#   if (!(inherits(x, what="vglm")) && !is.null(x$call$summ) && !identical(x$call$summ, 0))
#     stop("can NOT get Loglik when 'summ' argument is not zero")
#   
#   L.full <- logLik(x)
#   D.full <- -2 * L.full          # deviance(x)
#   
#   if(inherits(x, what="multinom"))
#     L.base <- logLik(update(x, ~1, trace=FALSE))
#   
#   else if(inherits(x, what="glm"))
#     # replaced 2019-08-19, based on mail by inferrator:
#     #
#     #   L.base <- logLik(update(x, ~1))
#     
#     
#     L.base <- logLik(glm(formula = reformulate('1', 
#                                                # replace the right side of the formula by 1                                           
#                                                gsub(" .*$", "", 
#                                                     # not all glms have a formula element, e.g. MASS::negbin                                           
#                                                     deparse(unlist(list(x$formula, x$call$formula, formula(x)))[[1]]))),
#                          # use the first non null list element
#                          # note x$call$data is a symbol and must first be evaluated
#                          data = Filter(Negate(is.null), list(x$data, eval(x$call$data) ))[[1]],
#                          family = x$family))
#   
#   
#   else 
#     L.base <- logLik(update(x, ~1))
#   
#   
#   D.base <- -2 * L.base # deviance(update(x, ~1))
#   G2 <- -2 * (L.base - L.full)
#   
#   # n <- if(length(weights(x)))
#   #   sum(weights(x))
#   # else
#   n <- attr(L.full, "nobs")   # alternative: n <- dim(x$residuals)[1]
#   
#   
#   if(inherits(x, "multinom"))
#     edf <- x$edf
#   else if(inherits(x, "vglm")){
#     edf <- x@rank
#     n <- nobs(x)  # logLik does not return nobs for vglm
#   } else
#     edf <- x$rank
#   
#   # McFadden
#   McFadden <- 1 - (L.full/L.base)
#   # adjusted to penalize for the number of predictors (k) in the model
#   McFaddenAdj <- 1 - ((L.full - edf)/L.base)
#   
#   # Nagelkerke / CraggUhler
#   Nagelkerke <- (1 - exp((D.full - D.base)/n))/(1 - exp(-D.base/n))
#   
#   # CoxSnell / Maximum Likelihood R2
#   CoxSnell <- 1 - exp(-G2/n)
#   
#   res <- c(McFadden=McFadden, McFaddenAdj=McFaddenAdj,
#            CoxSnell=CoxSnell, Nagelkerke=Nagelkerke, AldrichNelson=NA,
#            VeallZimmermann=NA,
#            Efron=NA, McKelveyZavoina=NA, Tjur=NA,
#            AIC=AIC(x), BIC=BIC(x), logLik=L.full, logLik0=L.base, G2=G2)
#   
#   
#   if(inherits(x, what="glm") || inherits(x, what="vglm") ) {
#     
#     if(inherits(x, what="vglm")){
#       fam <- x@family@vfamily
#       link <- if(all(x@extra$link == "logit")){
#         "logit"
#       } else if(all(x@extra$link == "probit")){
#         "probit"
#       } else {
#         NA
#       }
#       y <- x@y
#       
#     } else {
#       fam <- x$family$family
#       link <- x$family$link
#       y <- x$y
#     }
#     
#     
#     s2 <- switch(link, probit = 1, logit = pi^2/3, NA)
#     
#     # corrected based on mail by Chiroc Han, 2019-08-01 ******
#     # Aldrich/Nelson
#     # from: 
#     # res["AldrichNelson"] <- G2 / (G2 + n * s2)
#     # to:
#     res["AldrichNelson"] <- G2 / (G2 + n)
#     
#     # Veall/Zimmermann
#     # res["VeallZimmermann"] <- res["AldrichNelson"] * (2*L.base - n * s2)/(2*L.base)
#     res["VeallZimmermann"] <- res["AldrichNelson"] * (2*L.base - n)/(2*L.base)
#     
#     
#     # McKelveyZavoina
#     # y.hat <- predict(x, type="link")
#     
#     # remark Daniel Wollschlaeger, vglm would not dispatch correctly 30.11.2019
#     y.hat <- if(inherits(x, "vglm")) {
#       VGAM::predictvglm(x, type="link")
#     } else {
#       predict(x, type="link")
#     }
#     
#     sse <- sum((y.hat - mean(y.hat))^2)
#     res["McKelveyZavoina"] <- sse/(n * s2 + sse)
#     
#     # EfronR2
#     y.hat.resp <- predict(x, type="response")
#     res["Efron"] <- (1 - (sum((y - y.hat.resp)^2)) /
#                        (sum((y - mean(y))^2)))
#     
#     # Tjur's D
#     # compare with binomTools::Rsq.glm()
#     if(identical(fam, "binomial"))
#       res["Tjur"] <- unname(diff(tapply(y.hat.resp, y, mean, na.rm=TRUE)))
#     
#   }
#   
#   
#   if(is.null(which))
#     which <- "McFadden"
#   else
#     which <- match.arg(which, c("McFadden","AldrichNelson","VeallZimmermann","McFaddenAdj", "CoxSnell", "Nagelkerke",
#                                 "Efron", "McKelveyZavoina", "Tjur","AIC", "BIC", "logLik", "logLik0","G2","all"),
#                        several.ok = TRUE)
#   
#   if(any(which=="all"))
#     return(res)
#   else
#     return(res[which])
#   
# }




BrierScore <- function(...){
  UseMethod("BrierScore")
}


BrierScore.default <- function(resp, pred, scaled = FALSE, ...){
  
  res <- mean(resp * (1-pred)^2 + (1-resp) * pred^2)
  
  if(scaled){
    mean_y <- mean(resp)
    
    Bmax <- mean_y * (1-mean_y)^2 + (1-mean_y) * mean_y^2
    res <- 1 - res/Bmax
  }
  
  return(res)
  
}


BrierScore.glm <- function(x, scaled = FALSE, ...){
  BrierScore.default(resp=x$y, pred=predict(x, type="response"), scaled = scaled)
}


BrierScore.mult <- function(x, scaled=FALSE, ...){
  
  # https://en.wikipedia.org/wiki/Brier_score
  
  ref <- model.response(model.frame(x))
  res <- mean(apply((Dummy(ref, method = "full") - predict(x, type="prob"))^2, 1, sum))
  
  # check for reference, this is not correct!!
  # if(scaled){
  #   mean_y <- mean(x)
  #
  #   Bmax <- mean_y * (1-mean_y)^2 + (1-mean_y) * mean_y^2
  #   res <- 1 - res/Bmax
  # }
  
  return(res)
  
}


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


Cstat <- function (x, ...)
  UseMethod("Cstat")


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



MAE <- function(x, ...) UseMethod("MAE")

MAE.lm <- function(x, ...)
  # regr will escalate to lm, so no need for another interface here
  MAE(predict(x, type="response"), model.response(x$model), na.rm=FALSE)

MAE.default <- function (x, ref, na.rm=FALSE, ...) {
  # mean will bark, if there are NAs, so no need to do here anyhing further
  # (the difference will report NAs anyway)
  mean(abs(ref-x), na.rm=na.rm, ...)
}

MSE <- function(x, ...) UseMethod("MSE")

MSE.lm <- function(x, ...)
  # regr will escalate to lm, so no need for another interface here
  MSE(predict(x, type="response"), model.response(x$model), na.rm=FALSE)

MSE.default <- function (x, ref, na.rm=FALSE, ...) {
  mean((ref-x)^2, na.rm=na.rm, ...)
}

RMSE <- function(x, ...) UseMethod("RMSE")

RMSE.lm <- function(x, ...)
  # regr will escalate to lm, so no need for another interface here
  RMSE(predict(x, type="response"), model.response(x$model), na.rm=FALSE)


RMSE.default <- function (x, ref, na.rm=FALSE, ...) {
  sqrt(MSE(x, ref, na.rm, ...))
}


MAPE <- function(x, ...) UseMethod("MAPE")

MAPE.lm <- function(x, ...)
  # regr will escalate to lm, so no need for another interface here
  MAPE(predict(x, type="response"), model.response(x$model), na.rm=FALSE)

MAPE.default <- function (x, ref, na.rm=FALSE, ...) {
  # mean will bark, if there are NAs, so no need to do here anyhing further
  # (the difference will report NAs anyway)
  mean(abs((ref-x)/ref), na.rm=na.rm, ...)
}


SMAPE <- function(x, ...) UseMethod("SMAPE")

SMAPE.lm <- function(x, ...)
  # regr will escalate to lm, so no need for another interface here
  SMAPE(predict(x, type="response"), model.response(x$model), na.rm=FALSE)

SMAPE.default <- function (x, ref, na.rm=FALSE, ...) {
  
  mean( 2 * abs(ref-x) / (abs(x) + abs(ref)), na.rm=na.rm, ...)
}

# Chen and Yang (2004), in an unpublished working paper, defined the sMAPE as
# \[\text{sMAPE} = \text{mean}(2|y_t - \hat{y}_t|/(|y_t| + |\hat{y}_t|)).\]
# They still called it a measure of "percentage error" even though they dropped the multiplier 100.
# At least they got the range correct, stating that this measure has a maximum value of two when
# either y_t or \hat{y}_t is zero, but is undefined when both are zero.
# The range of this version of sMAPE is (0,2). Perhaps this is the definition that Makridakis and
# Armstrong intended all along, although neither has ever managed to include it correctly
# in one of their papers or books.
# source: http://robjhyndman.com/hyndsight/smape/



NMSE <- function(x, ref, train.y){
  sse <- sum((ref-x)^2)
  sse/sum((ref-mean(train.y))^2)
}

NMAE <- function(x, ref, train.y){
  sae <- sum(abs(ref-x))
  sae/sum(abs(ref-mean(train.y)))
}



VIF <- function(mod) {
  
  # original from car: Henric Nilsson and John Fox
  
  if (any(is.na(coef(mod))))
    stop ("there are aliased coefficients in the model")
  
  v <- vcov(mod)
  assign <- attr(model.matrix(mod), "assign")
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  }
  else warning("No intercept: vifs may not be sensible.")
  
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  
  if (n.terms < 2) stop("model contains fewer than 2 terms")
  
  R <- cov2cor(v)
  
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
  
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) *
      det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  
  if (all(result[, 2] == 1)) result <- result[, 1]
  else result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  result
}

# ????
# this will presumably not be found without a S3method declaration, but John doesn't declare it either
model.matrix.gls <- function(object, ...){
  model.matrix(formula(object), data=eval(object$call$data))
}





ModSummary <- function(x, ...){
  UseMethod("ModSummary")
}


ModSummary.lm <- function(x, conf.level=0.95, ...){
  
  
  smrx <- summary(x)
  
  #coefx <- cbind(smrx$coefficients, confint(x, level=conf.level), stbeta=c(NA, StdCoeff(x)))
  # coefx <- data.frame(rownames(smrx$coefficients), smrx$coefficients, confint(x, level=conf.level),
  #                     stringsAsFactors = FALSE)
  
  coefx <- merge(smrx$coefficients, confint(x, level = conf.level), by="row.names", sort=FALSE, all=TRUE)
  # order might have been changed by merging procedure
  coefx[order(match(coefx$Row.names, row.names(smrx$coefficients))),]
  
  colnames(coefx) <- c("name","est","se","stat","p","lci","uci")
  
  fit <- x$fitted.values
  y <- model.response(x$model)
  
  statsx <- c(with(smrx, c(
    sigma         = sigma,
    r.squared     = r.squared,
    adj.r.squared = adj.r.squared,
    "n vars" = length(attr(x$terms, "term.labels")),
    "n coef" = nrow(smrx$coefficients), 
    
    F             = fstatistic[[1]],
    numdf         = fstatistic[[2]],
    dendf         = fstatistic[[3]],
    p             = pf(fstatistic[[1]], fstatistic[[2]], fstatistic[[3]], lower.tail=FALSE)
  )),
  N             = nobs(x),
  logLik        = logLik(x),
  deviance      = deviance(x),
  AIC           = AIC(x),
  BIC           = BIC(x),
  MAE           = MAE(x=fit, ref = y),
  MAPE          = MAPE(x=fit, ref = y),
  MSE           = MSE(x=fit, ref = y),
  RMSE          = RMSE(x=fit, ref = y)
  )
  
  list(coef=coefx, ncoef=length(x$coefficients), statsx=statsx, contrasts=x$contrasts, xlevels=x$xlevels, call=x$call)
  
}



ModSummary.lmrob <- function (x, conf.level = 0.95, ...) {
  
  smrx <- summary(x)
  coefx <- data.frame(rownames(smrx$coefficients), smrx$coefficients,
                      confint(x, level = conf.level), stringsAsFactors = FALSE)
  colnames(coefx) <- c("name", "est", "se", "stat", "p", "lci",
                       "uci")
  fit <- x$fitted.values
  y <- model.response(x$model)
  statsx <- c(with(smrx, c(sigma = sigma, r.squared = r.squared,
                           adj.r.squared = adj.r.squared, F = df[[1]], numdf = df[[3]],
                           dendf = df[[2]], p = pf(df[[1]], df[[2]],
                                                   df[[3]], lower.tail = FALSE))), N = nobs(x),
              #           logLik = logLik(x), deviance = deviance(x), AIC = AIC(x),
              logLik = NA, deviance = NA, AIC = NA,
              "n vars" = length(attr(x$terms, "term.labels")),
              "n coef" = nrow(smrx$coefficients), 
              BIC = NA, MAE = MAE(x = fit, ref = y), MAPE = MAPE(x = fit, ref = y),
              MSE = MSE(x = fit, ref = y), RMSE = RMSE(x = fit, ref = y))
  
  list(coef = coefx, ncoef = length(x$coefficients), statsx = statsx,
       contrasts = x$contrasts, xlevels = x$xlevels, call = x$call)
  
  # https://stat.ethz.ch/pipermail/r-help/2005-April/070611.html
  # More fundamentally, `AIC' is about maximum-likelihood fitting of true
  # models.  Now rlm does usually correspond to ML fitting of a non-normal
  # linear model, so it would be possible to compute a likelihood and hence
  # AIC.  The point however is that the model is assumed to be false.  There
  # are AIC-like criteria for that situation, but they are essentially
  # impossible to compute accurately as they depend on fine details of the
  # unknown true error distribution (and still assume a linear model).
  # Ripley
  
  
}


ModSummary.glm <- function(x, conf.level=0.95, use.profile = TRUE, ...){
  
  sumry <- summary(x)
  
  # coefx <- cbind(summod$coefficients, confint(x, level=conf.level), stbeta=c(NA, StdCoeff(x)))
  if(use.profile)
    ci <- confint(x, level=conf.level)
  else 
    ci <- confint.default(x, level=conf.level)
  
  if(nrow(sumry$coefficients)==1)
    ci <- t(ci)
  coefx <- data.frame(row.names(sumry$coefficients), sumry$coefficients, ci,
                      stringsAsFactors = FALSE)
  colnames(coefx) <- c("name", "est","se","stat","p","lci","uci")
  
  pred <- x$fitted.values
  y <- model.response(x$model)
  
  
  N <- if(length(weights(x))) {
    sum(weights(x), na.rm=TRUE)
  } else {
    sum(sumry$df[1:2])
  }
  
  phi <- sumry$dispersion
  degf <- sumry$df.null - sumry$df.residual
  
  # if(degf > 0){
  LR <- sumry$null.deviance - sumry$deviance
  p <- pchisq(LR, degf, lower.tail=FALSE)
  L0.pwr <- exp(-sumry$null.deviance / N)
  
  
  if(x$family$family == "binomial"){
    
    statsx <- PseudoR2(x, which = "all")
    statsy <- .assocs_condis(pred, model.response(x$model))
    
    statsx <- c(statsx,
                "N" =  nobs(x),
                "n vars" = length(attr(x$terms, "term.labels")),
                "n coef" = nrow(x$coefficients),
                "numdf" = attr(logLik(x), "df"),
                "Kendall Tau-a" = unname(statsy["taua"]),
                "Somers Delta" = unname(statsy["somers_r"]),
                "Gamma" = unname(statsy["gamma"]),
                "Brier" = BrierScore(x), "C"= Cstat(x)
    )
  } else {
    
    statsx <- PseudoR2(x, which =c("McFadden","McFaddenAdj","Nagelkerke","CoxSnell",
                                   "AIC","BIC","logLik",
                                   "logLik0", "G2"))
    
    statsx <- c(statsx[],
                "N" =  nobs(x),
                "n vars" = length(attr(x$terms, "term.labels")),
                "n coef" = length(x$coefficients),
                "numdf" = attr(logLik(x), "df"),
                "MAE" = MAE(pred, model.response(x$model)),
                "MAPE" = MAPE(pred, model.response(x$model)),
                "MSE" = MSE(pred, model.response(x$model)),
                "RMSE" = RMSE(pred, model.response(x$model))
                
    )
  }
  
  # L0.pwr        = exp(-summod$null.deviance / N),
  # logLik        = logLik(x),
  # deviance      = deviance(x),
  
  
  list(coef=coefx, ncoef=length(x$coefficients), statsx=statsx, contrasts=x$contrasts, xlevels=x$xlevels, call=x$call)
  
}


ModSummary.OddsRatio <- function(x, conf.level=0.95, ...){
  
  statsx <- x$PseudoR2
  statsx <- c(N = x$nobs,
              "n vars" = length(x$terms),
              "n coef" = nrow(x$res), 
              statsx[],
              "BrierScore" = x$BrierScore)
  
  coef <- data.frame(name=rownames(x$or), est=x$res$or, se=NA, stat=NA, p=x$res$`Pr(>|z|)`,
                     lci=x$res$or.lci, uci=x$res$or.uci, stringsAsFactors = FALSE)
  
  list(coef=coef, ncoef=nrow(x$or),
       statsx=statsx, contrasts=NULL, xlevels=NULL,
       call=x$call)
}



TMod <- function(..., FUN = NULL){
  
  
  # prepare function to put together coefficients and stats
  if(is.null(FUN))
    FUN <- function(est, se, tval, pval, lci, uci){
      res <- gettextf("%s %s",
                      Format(est, fmt=Fmt("num")),
                      Format(pval, fmt="*"))
      replace(res, is.na(est), NA)
    }
  
  
  to.frame <- function(x){
    res <- data.frame(names(x), x)
    colnames(res) <- c("name", "val")
    res
  }
  
  # convert language to string either with: toString or deparse, but not as.character!!!
  # modname <- unlist(lapply(match.call(expand.dots=FALSE)$..., as.character))
  modname <- unlist(lapply(match.call(expand.dots=FALSE)$..., toString))
  
  lmod <- list(...)
  lst <- lapply(lmod, ModSummary)
  
  modname[names(lst) != ""] <- names(lst)[names(lst) != ""]
  
  lcoef <- lapply(lst, "[[", "coef")
  lstatsx <- lapply(lst, "[[", "statsx")
  
  
  # merge coefficients of all models
  m <- lcoef[[1]][, c("name", "est")]
  m$est <- apply(lcoef[[1]][,-1], 1, function(x) FUN(x["est"], x["se"], x["stat"], x["p"], x["lci"], x["uci"]))
  colnames(m) <- c("name", modname[1])
  
  if(length(lcoef)>1) {
    for(i in 2L:length(lcoef)){
      ordm <- m$name
      m2 <- lcoef[[i]][, c("name", "est")]
      m2$est <- apply(lcoef[[i]][,-1], 1, function(x) FUN(x["est"], x["se"], x["stat"], x["p"], x["lci"], x["uci"]))
      m <- merge(x=m, y=m2, by.x="name", by.y="name",
                 all.x=TRUE, all.y=TRUE, sort=FALSE)
      colnames(m)[i+1] <- modname[i]
      
      # keeping the order of m, then m2
      # ord <- c("red","green","blue")
      # x[order(match(x, ord))]
      
      ord <- c(ordm, m2$name[m2$name %nin% ordm])
      m <- m[order(match(m$name, ord)), ]
      
    }
  }
  colnames(m)[1] <- "coef"
  
  # merge statistics of all models
  mm <- to.frame(lstatsx[[1]])
  colnames(mm) <- c("name", modname[1])
  if(length(lstatsx) > 1){
    for(i in 2L:length(lstatsx)){
      mm <- merge(x=mm, y=to.frame(lstatsx[[i]]), by.x="name", by.y="name",
                  all.x=TRUE, all.y=TRUE, sort=FALSE)
      colnames(mm)[i+1] <- modname[i]
    }
  }
  colnames(mm)[1] <- "stat"
  
  row.names(mm) <- mm$stat
  mm <- mm[match(c("r.squared", "adj.r.squared","sigma","logLik","logLik0","G2","deviance",
                   "AIC","BIC","numdf","dendf","N","n vars","n coef","F","p","MAE","MAPE","MSE","RMSE","McFadden",
                   "McFaddenAdj","Nagelkerke","CoxSnell","Kendall Tau-a","Somers Delta","Gamma","Brier","C"),
                 rownames(mm))
           , ]
  mm <- mm[!is.na(mm$stat), ]
  
  row.names(mm) <- NULL
  
  
  
  # # compose est-lci-uci table
  # merge_mod <- function(z, ord){
  #   lst <- lapply(lcoef, function(x) cbind(SetNames(x[[z]], names=x[["name"]])))
  #   mcoef <- lst[[1]]
  #   for(i in 2:length(lst)){
  #     mcoef <- merge(mcoef, lst[[i]], by = "row.names",
  #                    all.x=TRUE, all.y=TRUE, sort=FALSE)
  #     rownames(mcoef) <- mcoef$Row.names
  #     mcoef$Row.names <- NULL
  #     colnames(mcoef) <- NULL
  #     }
  # 
  #   mcoef[order(match(rownames(mcoef), ord)),]
  # 
  # }
  # 
  # # define a better order than merge is returning, coefficients from left to right
  # seq_ord <- function(lst){
  #   jj <- character(0)
  #   for(i in seq_along(lst)){
  #     jj <- c(jj, setdiff(lst[[i]], jj))
  #   }
  #   return(jj)
  # }
  # 
  # # the coefficients should be ordered such, that the coeffs of the first model
  # # come first, then the coeffs from the second model which were not included
  # # in the model one, then the coeffs from mod3 not present in mod1 and mod2
  # # and so forth...
  # coef_order <- seq_ord(lapply(lcoef, rownames))
  # 
  # # set coefficient order to all result object
  # m <- m[order(match(m$coef, coef_order)),]
  # 
  # if(length(lmod) > 1){
  #   mall <- Abind(merge_mod("est", coef_order),
  #               merge_mod("lci", coef_order),
  #               merge_mod("uci", coef_order), along=3)
  # 
  # } else {
  #   mall <- as.matrix(lcoef[[1]][, c("est","lci","uci")])
  #   dim(mall) <- c(nrow(mall), 1, 3)
  # }
  # dimnames(mall) <- list(m$coef, modname, c("est","lci","uci"))
  
  mall <- Abind(
    est = do.call(MultMerge, lapply(lst, 
                                    function(x) SetNames(x$coef[,c("est"), drop=FALSE], rownames=x$coef$name))),
    lci = do.call(MultMerge, lapply(lst, 
                                    function(x) SetNames(x$coef[,c("lci"), drop=FALSE], rownames=x$coef$name))),
    uci = do.call(MultMerge, lapply(lst, 
                                    function(x) SetNames(x$coef[,c("uci"), drop=FALSE], rownames=x$coef$name))), 
    along=3)
  
  dimnames(mall)[[2]] <- modname
  
  # return the terms of the model in order to be able to set a filter on them
  # when plotting
  
  mterms <- lapply(lmod, function(m) {
    res <- lapply(labels(terms(m)), function(x) 
      colnames(model.matrix(formula(gettextf("~ 0 + %s", x)), data=model.frame(m))))
    names(res) <- labels(terms(m))
    res
  } )
  
  names(mterms) <- modname
  
  return(structure(list(m, mm, lcoef, mall=mall, terms=mterms), class="TMod"))
  
  
}


print.TMod <- function(x, digits=3, na.form = "-", ...){
  
  colnames(x[[1]])[-1] <- paste0(colnames(x[[1]])[-1], strrep(" ", times=4))
  x[[1]][, -1] <- Format(x[[1]][, -1], digits=digits, na.form = na.form)
  
  x2 <- x[[2]]
  x[[2]][, -1] <- Format(x[[2]][, -1], digits=digits, na.form = na.form)
  
  x[[2]][x[[2]]$stat %in% c("numdf", "dendf", "N", "n vars", "n coef"), -1] <-
    Format(x2[x[[2]]$stat %in% c("numdf", "dendf", "N", "n vars", "n coef"), -1], digits=0, na.form=na.form)
  
  m <- rbind(x[[1]],  setNames(c("---", rep("", ncol(x[[1]]) -1)), colnames(x[[1]])),
             setNames(x[[2]], colnames(x[[1]])))
  
  m[, -1] <- apply(m[, -1, drop=FALSE], 2, StrAlign, sep=".")
  row.names(m) <- NULL
  print(m, ...)
  
}



plot.TMod <- function(x, terms=NULL, intercept=FALSE, ...){
  
  # see also: termplot
  
  if(length(dim(x$mall)) > 2)
    xx <- aperm(x$mall, perm = c(2, 1, 3))
  else {
    xx <- x$mall
  }
  
  if(!is.null(terms)){
    # v <- unlist(x$terms)
    # coefnames <- unique(v[v %in% terms])
    # xx <- xx[, coefnames, , drop=FALSE]
    # 
    v <- unique(c(sapply(x$terms, labels)))
    coefnames <- unlist(x$terms[[1]][labels(x$terms[[1]]) %in% terms])
    xx <- xx[, dimnames(xx)[[2]] %in% coefnames, , drop=FALSE]
  }
  
  if(!intercept)
    xx <- xx[, !grepl("intercept", dimnames(xx)[[2]], ignore.case = TRUE), , drop=FALSE]
  
  args.plotdot1 <- list(x=xx[,,1], pch=21, bg="white",
                        args.errbars = list(from=xx[,,2], to=xx[,,3], mid=xx[,,1]))
  
  # Attention:
  # this evaluates the dots, which goes wrong e.g. for panel.first arguments!
  # dots <- list(...)
  dots <- match.call(expand.dots = FALSE)$`...`
  
  if (!is.null(dots)) {
    args.plotdot1[names(dots)] <- dots
  }
  do.call("PlotDot", args.plotdot1)
  
}




# ToWrd.TMod <- function(x, font=NULL, para=NULL, main=NULL, align=NULL,
#                        autofit=TRUE, ..., wrd=DescToolsOptions("lastWord")) {
#   m <- FixToTable(capture.output(x))
#   if(is.null(align))
#     align <- "l"
#   wt <- ToWrd.matrix(x=m, font=font, para=para, main=main, align=align, autofit=autofit, ..., wrd=wrd)
#   
#   # insert decimal tabs
#   # Selection.ParagraphFormat.TabStops(CentimetersToPoints(1.14)).Position = CentimetersToPoints(1.14)
#   # Selection.TypeText Text:=vbTab
#   
# }





ToWrd.TMod <- function (x, font = NULL, para = NULL, main = NULL, align = NULL, 
                        split=" ", fixed = TRUE, 
                        autofit = TRUE, digits = 3, na.form = "-", ..., 
                        wrd = DescToolsOptions("lastWord")) {
  

  # prepare quality measures  
  x2 <- x[[2]]
  x[[2]][, -1] <- Format(x[[2]][, -1], digits = digits, na.form = na.form)
  x[[2]][x[[2]]$stat %in% c("numdf", "dendf", "N", "n vars", "n coef"), -1] <- 
    Format(x2[x[[2]]$stat %in% c("numdf", "dendf", "N", "n vars", "n coef"), -1], 
           digits = 0, na.form = na.form)
  
  if(!is.null(split)) {
    # xx <- SplitToCol(x[[1]][, -1], split=split, fixed=fixed)
    xx <- SplitToCol(as.data.frame(lapply(x[[1]], StrTrim))[, -1], 
                     split=split, fixed=fixed)
    
    
    zz <- x[[2]][,-1]
    vn <- character()
    for(i in seq_along(attr(xx, "cols"))) {
      j <- attr(xx, "cols")[i]
      zz <- Append(zz, values = matrix("", ncol=j-1), 
                   after = cumsum(c(1, attr(xx, "cols")))[i], names="", stringsAsFactors=FALSE)
      vn <- c(vn, names(attr(xx, "cols"))[i], rep("", j-1))
    }
    
  } else {
    xx <- x[[1]][-1]
    zz <- x[[2]][-1]
  } 
  
  tt <- do.call(rbind, list(SetNames(xx, ""), 
                            SetNames(rep("",  ncol(xx)), ""),
                            SetNames(zz, "")))
  
  ttt <- SetNames(data.frame(c(x[[1]][,1], "---", as.character(x[[2]][,1])), tt, stringsAsFactors = FALSE),
                  c(colnames(x[[1]])[1], vn))
  
  ToWrd(as.matrix(ttt), 
        font=font, 
        align=align)
  
}






.partialsd <- function(x, sd, vif, n, p = length(x) - 1) {
  sd * sqrt(1 / vif) * sqrt((n - 1)/(n - p))
}


PartialSD <- function(x) {
  
  mm <- model.matrix(x)
  .partialsd(coef(x), apply(mm, 2L, sd), VIF(x), nobs(x),
             sum(attr(mm, "assign") != 0))
  
}

coeffs <-
  function (model) UseMethod("coeffs")

coeffs.multinom <-
  function (model) {
    cf <- coef(model)
    if (!is.vector(cf)) {
      cf <- t(as.matrix(cf))
      cfnames <- expand.grid(dimnames(cf), stringsAsFactors = FALSE)
      cfnames <- sprintf("%s(%s)", cfnames[,2L], cfnames[,1L])
      structure(as.vector(cf), names = cfnames)
    } else cf
  }

coeffs.survreg <-
  function (model) {
    rval <- coef(model)
    if (nrow(vcov(model)) > length(rval)) { # scale was estimated
      lgsc <- log(model$scale)
      names(lgsc) <- if(is.null(names(lgsc)))
        "Log(scale)" else
          paste0("Log(scale):", names(lgsc))
      rval <- c(rval, lgsc)
    }
    rval
  }

coeffs.default <-
  function(model) coef(model)


coefTable <-
  function (model, ...) UseMethod("coefTable")

.makeCoefTable <-
  function(x, se, df = NA_real_, coefNames = names(x)) {
    if(n <- length(x)) {
      xdefined <- !is.na(x)
      ndef <- sum(xdefined)
      if(ndef < n) {
        if(length(se) == ndef) {
          y <- rep(NA_real_, n); y[xdefined] <- se; se <- y
        }
        if(length(df) == ndef) {
          y <- rep(NA_real_, n); y[xdefined] <- df; df <- y
        }
      }
    }
    if(n && n != length(se)) stop("length(x) is not equal to length(se)")
    ret <- matrix(NA_real_, ncol = 3L, nrow = length(x),
                  dimnames = list(coefNames, c("Estimate", "Std. Error", "df")))
    if(n) ret[, ] <- cbind(x, se, rep(if(is.null(df)) NA_real_ else df,
                                      length.out = n), deparse.level = 0L)
    class(ret) <- c("coefTable", "matrix")
    ret
  }

coefTable.default <-
  function(model, ...) {
    dfs <- tryCatch(df.residual(model), error = function(e) NA_real_)
    cf <- summary(model, ...)$coefficients
    .makeCoefTable(cf[, 1L], cf[, 2L], dfs, coefNames = rownames(cf))
  }

coefTable.lm <-
  function(model, ...)
    .makeCoefTable(coef(model), sqrt(diag(vcov(model, ...))), model$df.residual)


coefTable.survreg <-
  function(model, ...) {
    .makeCoefTable(
      coeffs(model),
      sqrt(diag(vcov(model, ...))),
      NA
    )
  }

coefTable.coxph <-
  function(model, ...) {
    .makeCoefTable(coef(model), if(all(is.na(model$var)))
      rep(NA_real_, length(coef(model))) else sqrt(diag(model$var)),
      model$df.residual)
  }

coefTable.multinom <-
  function (model, ...) {
    .makeCoefTable(coeffs(model), sqrt(diag(vcov(model, ...))))
  }

coefTable.zeroinfl <-
  function(model, ...)
    .makeCoefTable(coef(model), sqrt(diag(vcov(model, ...))))

coefTable.hurdle <-
  function(model, ...) {
    cts <- summary(model)$coefficients
    ct <- do.call("rbind", unname(cts))
    cfnames <- paste0(rep(names(cts), vapply(cts, nrow, 1L)), "_", rownames(ct))
    .makeCoefTable(ct[, 1L], ct[, 2L], coefNames = cfnames)
    #.makeCoefTable(coef(model), sqrt(diag(vcov(model, ...))))
  }




StdCoef <- function(x, partial.sd = FALSE, ...) {
  
  coefmat <- coefTable(x, ...)
  
  mm <- model.matrix(x)
  
  if(partial.sd) {
    bx <- .partialsd(coefmat[, 1L], apply(mm, 2L, sd),
                     VIF(x), nobs(x), sum(attr(mm, "assign") != 0))
  } else {
    response.sd <- sd(model.response(model.frame(x)))
    bx <- apply(mm, 2L, sd) / response.sd
  }
  coefmat[, 1L:2L] <- coefmat[, 1L:2L] * bx
  colnames(coefmat)[1L:2L] <- c("Estimate*", "Std. Error*")
  return (coefmat)
  
}





PseudoR2 <- function(x, which = NULL) {
  
  # this function will not work with weights, neither with cbind lhs!!
  # http://stats.stackexchange.com/questions/183699/how-to-calculate-pseudo-r2-when-using-logistic-regression-on-aggregated-data-fil
  
  # test: https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-what-are-pseudo-r-squareds/
  # library(haven)
  # hsb2 <- as.data.frame(read_dta("https://stats.idre.ucla.edu/stat/stata/notes/hsb2.dta"))
  # hsb2$honcomp <- hsb2$write >= 60
  # r.logit <- glm(honcomp ~ female + read + science, hsb2, family="binomial")
  # PseudoR2(r.logit, "a")
  
  
  
  # http://digitalcommons.wayne.edu/cgi/viewcontent.cgi?article=2150&context=jmasm
  # Walker, Smith (2016) JMASM36: Nine Pseudo R^2 Indices for Binary Logistic Regression Models (SPSS)
  # fuer logit Korrektur https://langer.soziologie.uni-halle.de/pdf/papers/rc33langer.pdf
  
  # check with pscl::pR2(x); rcompanion::nagelkerke(x)
  #       or with  library(blorr)
  #                c(blr_rsq_mcfadden(r.glm), 
  #                  blr_rsq_cox_snell(r.glm), 
  #                  blr_rsq_nagelkerke(r.glm))
  
  
  
  if(inherits(x, what="multinom")) modeltype <- "multinom"
  else if(inherits(x, what="glm")) modeltype <- "glm"
  else if(inherits(x, what="polr")) modeltype <- "polr"
  else if(inherits(x, what="vglm")) modeltype <- "vglm"
  else return(NA)
  
  if(inherits(x, what="vglm") && !requireNamespace("VGAM", quietly=TRUE)) {
    stop("Could not find package 'VGAM' - please install first") }
  
  if (!(inherits(x, what="vglm")) && !is.null(x$call$summ) && !identical(x$call$summ, 0))
    stop("can NOT get Loglik when 'summ' argument is not zero")
  
  L.full <- logLik(x)
  D.full <- -2 * L.full          # deviance(x)
  AIC_score <- AIC(x)
  BIC_score <- BIC(x)
  
  #Compute predicted values (do this before converting VGLM object to S3 list)
  if(modeltype == "vglm" | modeltype == "glm"){
    # remark Daniel Wollschlaeger, vglm would not dispatch correctly 30.11.2019
    y.hat <- if(modeltype == "vglm") {
      VGAM::predictvglm(x, type="link")
    } else {
      predict(x, type="link")
    }
    y.hat.resp <- predict(x, type="response")
  }
  
  # For compatibility with other method types, convert vglm S4 object into normal S3 object
  # EG, it's easier if we can consistently use x$model to access the "model" data frame
  # Note that this needs to be done after running logLik above
  if(modeltype == "vglm"){
    n_vglm <- nobs(x)          #save for later
    
    S4_xnames <- slotNames(x)
    x <- lapply(S4_xnames, slot, object = x)
    names(x) <- S4_xnames
    
    if(!is.null(x$call$form2)) stop("Cannot compute PseudoR2 values for VGLM models with form2 parameter")
  }
  
  orig.formula <- deparse(unlist(list(x$formula, formula(x), x$call$formula))[[1]])
  
  # ---- Get all parameters that we don't explicitly know what to do with ----
  if(modeltype == "multinom"){other_params <- x$call[!(names(x$call) %in% c("formula", "data", "weights", "subset", "censored", "", #Parameters whose values are stored in model object 
                                                                            "na.action", "subset", #parameters that we can ignore if model = TRUE
                                                                            "model", "contrasts"))] ##parameters that don't affect model results and can be dropped from null model call
  }else if(modeltype == "glm"){ other_params <- x$call[!(names(x$call) %in% c("formula", "family", "data", "weights", "subset", "offset", "method", "control", "", #Parameters whose values are stored in model object 
                                                                              "na.action", "subset", #parameters that we can ignore if model = TRUE
                                                                              "model", "x", "y", "contrasts"))] #parameters that don't affect model results and can be dropped from null model call
  }else if(modeltype == "polr"){ other_params <- x$call[!(names(x$call) %in% c("formula", "data", "weights", "subset", "method",  "", #Parameters whose values are stored in model object 
                                                                               "na.action", "subset", #parameters that we can ignore if model = TRUE
                                                                               "model", "contrasts"))] #parameters that don't affect model results and can be dropped from null model call
  }else if(modeltype == "vglm"){ other_params <- x$call[!(names(x$call) %in% c("formula", "weights", "family", "data", "control", "", #Parameters whose values are stored in model object 
                                                                               "na.action", "subset", #parameters that we can ignore if model = TRUE
                                                                               "model", "contrasts", "qr.arg", "trace"))] #parameters that don't affect model results and can be dropped from null model call
  if(nrow(x$model) == 0) stop("Can only calculate PseudoR2 for VGLM when model = TRUE, try refitting VGLM")
  }
  orig_call <- x$call
  
  #Check whether the other parameters, when called, will evaluate in the current environment
  other_params_exist.yn <- mapply(function(x, x.name){ #for each other_param (and the associated name)
    tryCatch({ #return TRUE if the expression evaluates
      eval(x)
      TRUE
    }, error = function(cond){
      message("Could not evaluate '", as.character(x), "' for fitting PseudoR2 null model with parameter ", as.character(x.name), " = ", as.character((x)))
      message("Will evaluate null model without parameter; results may not be valid if this parameter affects model fit")
      return(FALSE)
    })
  }, x = other_params, x.name = names(other_params))
  other_params <- other_params[other_params_exist.yn]
  
  #If model parameter was not specified, add subset and na.action parameters to the other params list (we don't need to rerun model.frame if we already have a valid model object)
  #These parameters *do* affect null model fit, and so we separately check for their existence when refitting model.frame (and generate an error, rather than warning, if they don't exist)
  if(!(exists("model", x))) other_params <- c(other_params, x$call[names(x$call) %in% c("subset", "na.action")])
  
  # ---- Construct appropriate data/model object for null model call ----
  
  calltype.char <- as.character(orig_call[1])
  
  #Get initial data
  if(exists("model", x)){
    data <- x$model #If x has a model frame component, use that - the safest bet
  }else if(exists("data", x) & !("environment" %in% class(x$data))){ #If x has a data object (but no model), take it
    data <- x$data
    #may need to check for subset and na.action parameters to be included in model.frame as well
  }else if(!is.null(x$call$data) & !("environment" %in% class(x$data))){ #If there is a data frame specified in the call
    # if("environment" %in% class(x$data)) warning("Could not find model element of ", modeltype, " object for evaluating PseudoR2 null model. Will fit null model with new evaluation of variables in environment ",  environmentName(x$data), ". Ensure variables have not changed since initial call, or try running ", calltype.char, " with 'model = TRUE'")
    
    #This is a very lazy use of tryCatch (as we are effectively evaluating x$call$data once here, then once below); will fix at some point
    isValidCallRef <- tryCatch(({
      eval(x$call$data)
      TRUE
    }), error = function(cond){
      return(FALSE)
    })
    
    if(!isValidCallRef)  stop("Could not find model, data, or valid call element of ", modeltype, " object for evaluating PseudoR2 null model (could not find '", as.character(x$call$data), "'). Try running ", calltype.char, " with 'model = TRUE'")
    warning("Could not find model or data element of ", modeltype, " object for evaluating PseudoR2 null model. Will fit null model with new evaluation of '", as.character(x$call$data), "'. Ensure object has not changed since initial call, or try running ", calltype.char, " with 'model = TRUE'")
    data <- eval(x$call$data)
    
  } else if(!is.null(x$call$formula)){ #if the call only references objects an environment
    
    if("environment" %in% class(x$data)) eval_env <- x$data else eval_env <- parent.frame()
    
    isValidCallRef <- tryCatch(({ #return TRUE if the variable has a valid evaluation, false otherwise
      model.frame(x$call$formula, data = eval_env)
      TRUE
    }), error = function(cond){
      return(FALSE)
    })
    
    if(!isValidCallRef) stop("Could not find model, data, or valid call element of ", modeltype, " object for evaluating PseudoR2 null model (objects in formula could not be found). Try running ", calltype.char, " with 'model = TRUE'")
    warning("Could not find model or data element of ", modeltype, " object for evaluating PseudoR2 null mode. Will fit null model with new evaluation of objects in formula. Ensure object has not changed since initial call, or try running ", calltype.char, " with 'model = TRUE'")
    data <- model.frame(x$call$formula, data = eval_env)
    
  }else stop("Could not find model, data, or valid call element of ", modeltype, " object for evaluating PseudoR2 null model. Try running ", calltype.char, " with 'model = TRUE'")
  
  #If data wasn't taken from a "model" object, we will need to re-run model.frame to drop NAs and evaluate subsets
  if(!exists("model", x)){
    
    #again, we are being very lazy in the implementation of tryCatch here - but this can be fixed another time
    
    #evaluate "subset" calls to check if it's a valid value (including "NULL" as a valid value)
    validSubset.yn <- 
      tryCatch(({
        eval(x$call$subset)
        TRUE
      }), error = function(cond){
        return(FALSE)
      })
    if(validSubset.yn == FALSE) stop("Could not evaluate '", as.character(x$call$subset), "' for fitting PseudoR2 null model with parameter subset = ", as.character(x$call$subset), ".  Try running ", calltype.char, " with 'model = TRUE'")
    if(!is.null(x$call$subset)) warning("Re-evaluating ", as.character(x$call$subset), " for fitting PseudoR2 null model with parameter subset = ", as.character(x$call$subset))
    
    #evaluate "na.action" to see if it has a valid non-null value, and add to call only if non-null
    validNaAction.yn <- 
      tryCatch(({
        eval(x$call$na.action)
        TRUE
      }), error = function(cond){
        return(FALSE)
      })
    if(!is.null(x$call$na.action) & validNaAction.yn == FALSE){
      stop("Could not evaluate '", as.character(x$call$na.action), "' for fitting PseudoR2 null model with parameter na.action = ", as.character(x$call$na.action), ".  Try running ", calltype.char, " with 'model = TRUE'")
    } else if(!is.null(x$call$na.action)) warning("Re-evaluating ",  as.character(x$call$na.action), " for fitting PseudoR2 null model with parameter na.action = ", as.character(x$call$na.action))
    
    #if we are using an object type that doesn't not contain a prior.weights output, check that the weights call is valid
    #for other model types, we extract weights from the model object instead
    if(modeltype == "polr"){
      validWeights.yn <- 
        tryCatch(({
          eval(x$call$weights)
          TRUE
        }), error = function(cond){
          return(FALSE)
        })
      if(validWeights.yn == FALSE) stop("Could not evaluate '", as.character(x$call$weights), "' for fitting PseudoR2 null model with parameter weights = ", as.character(x$call$weights), ".  Try running ", calltype.char, " with 'model = TRUE'")
      if(!is.null(x$call$weights)) warning("Re-evaluating ", as.character(x$call$weights), " for fitting PseudoR2 null model with parameter weights = ", as.character(x$call$weights))
      
      weights.call <- x$call$weights
    } else weights.call <- NULL
    
    modelcall <- call('model.frame', formula = orig.formula, data = data, subset = x$call$subset, weights = weights.call)
    if("na.action" %in% names(x$call)) modelcall$na.action <- x$call$na.action #check whether a na.action parameter was explicitly called (default value for na.action is NOT null, so is.null(x$call$na.action) does not work)
    
    data <- eval(modelcall)
  }
  
  if(!is.null(x$prior.weights) & length(x$prior.weights) > 0) weights <- x$prior.weights
  else if(modeltype == "multinom") weights <- x$weights #"weights' in multinom are equivalent to 'prior.weights' in glm
  else if(!is.null(data$`(weights)`) & length(data$`(weights)` > 0)) weights <- data$`(weights)`
  else weights <- NULL
  
  #vglm saves prior.weights as a matrix, but then requires a vector as input
  if(!is.null(weights) & modeltype == "vglm"){
    if(ncol(weights) == 1) weights <- as.vector(weights) 
  }
  
  #Drop other columns from data, to avoid literal names (eg, "factor(y)" as DV not matching any DV columns)
  data <- data[,1,drop = FALSE]
  names(data) <- "y"
  null.formula <- as.formula("y ~ 1")
  
  #Costruct the call, then evaluate
  if(modeltype == "multinom") 
    nullcall <- call(calltype.char, formula = null.formula, data = data, weights = weights, 
                     censored = x$censored, trace = FALSE) #specify elements that come from a known part of the multinom object
  else if(modeltype == "glm") 
    nullcall <- call(calltype.char, formula = null.formula, data = data, weights = weights, 
                     family = x$family, method = x$method, control = x$control, 
                     offset = x$offset) #specify elements that come from a known part of the glm object
  else if(modeltype == "polr") 
    nullcall <- call(calltype.char, formula = null.formula, data = data, weights = weights, 
                     method = x$method) #specify elements that come from a known part of the polr object
  else if(modeltype == "vglm"){ 
    nullcall <- call(calltype.char, formula = null.formula, data = data, weights = weights, 
                     control = x$control, family = x$family, trace = FALSE)}
  
  if(length(other_params) > 0){
    nullcall[names(other_params)] <- other_params
  }
  L.base <- logLik(eval(nullcall))
  
  D.base <- -2 * L.base # deviance(update(x, ~1))
  G2 <- -2 * (L.base - L.full)
  
  # n <- if(length(weights(x)))
  #   sum(weights(x))
  # else
  n <- attr(L.full, "nobs")   # alternative: n <- dim(x$residuals)[1]
  
  if(modeltype ==  "multinom")
    edf <- x$edf
  else if(modeltype ==  "vglm"){
    edf <- x$rank
    n <- n_vglm  # logLik does not return nobs for vglm
  } else
    edf <- x$rank
  
  # McFadden
  McFadden <- 1 - (L.full/L.base)
  # adjusted to penalize for the number of predictors (k) in the model
  McFaddenAdj <- 1 - ((L.full - edf)/L.base)
  
  # Nagelkerke / CraggUhler
  Nagelkerke <- (1 - exp((D.full - D.base)/n))/(1 - exp(-D.base/n))
  
  # CoxSnell / Maximum Likelihood R2
  CoxSnell <- 1 - exp(-G2/n)
  
  res <- c(McFadden=McFadden, McFaddenAdj=McFaddenAdj,
           CoxSnell=CoxSnell, Nagelkerke=Nagelkerke, AldrichNelson=NA,
           VeallZimmermann=NA,
           Efron=NA, McKelveyZavoina=NA, Tjur=NA,
           AIC=AIC_score, BIC=BIC_score, logLik=L.full, logLik0=L.base, G2=G2)
  
  
  if(modeltype == "glm" || modeltype == "vglm" ) {
    
    if(modeltype == "vglm"){
      fam <- x$family@vfamily
      link <- if(all(x$extra$link == "logit")){
        "logit"
      } else if(all(x$extra$link == "probit")){
        "probit"
      } else {
        NA
      }
      y <- x$y
      
    } else {
      fam <- x$family$family
      link <- x$family$link
      y <- x$y
    }
    
    
    s2 <- switch(link, probit = 1, logit = pi^2/3, NA)
    
    # corrected based on mail by Chiroc Han, 2019-08-01 ******
    # Aldrich/Nelson
    # from: 
    # res["AldrichNelson"] <- G2 / (G2 + n * s2)
    # to:
    res["AldrichNelson"] <- G2 / (G2 + n)
    
    # Veall/Zimmermann
    # res["VeallZimmermann"] <- res["AldrichNelson"] * (2*L.base - n * s2)/(2*L.base)
    res["VeallZimmermann"] <- res["AldrichNelson"] * (2*L.base - n)/(2*L.base)
    
    
    # McKelveyZavoina
    # y.hat <- predict(x, type="link")
    
    
    sse <- sum((y.hat - mean(y.hat))^2)
    res["McKelveyZavoina"] <- sse/(n * s2 + sse)
    
    # EfronR2
    res["Efron"] <- (1 - (sum((y - y.hat.resp)^2)) /
                       (sum((y - mean(y))^2)))
    
    # Tjur's D
    # compare with binomTools::Rsq.glm()
    if(identical(fam, "binomial"))
      res["Tjur"] <- unname(diff(tapply(y.hat.resp, y, mean, na.rm=TRUE)))
    
  }
  
  if(is.null(which))
    which <- "McFadden"
  else
    which <- match.arg(which, c("McFadden","AldrichNelson","VeallZimmermann","McFaddenAdj", "CoxSnell", "Nagelkerke",
                                "Efron", "McKelveyZavoina", "Tjur","AIC", "BIC", "logLik", "logLik0","G2","all"),
                       several.ok = TRUE)
  
  if(any(which=="all"))
    return(res)
  else
    return(res[which])
  
}


