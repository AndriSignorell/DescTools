


# Confusion matrix

Conf <- function(x, ...) UseMethod("Conf")


Conf.table <- function(x, pos = NULL, ...) {

  CollapseConfTab <- function(x, pos = NULL, ...) {

    if(nrow(x) > 2) {
      names(attr(x, "dimnames")) <- c("pred", "obs")
      x <- DescTools::CollapseTable(x, obs=c("neg", pos)[(rownames(x)==pos)+1],
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
           acc     = DescTools::BinomCI(x=res$diag, n=res$n),
           sapply(binom.test(x=res$diag, n=res$n,
                             p=max(apply(x, 2, sum) / res$n),
                             alternative = "greater")[c("null.value", "p.value")], unname),
           kappa   = DescTools::CohenKappa(x),
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
      fval    = DescTools::Hmean(c(A / (A + B), A / (A + C)), conf.level = NA), # guetemass wollschlaeger s. 150
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
      paste(DescTools::StrPad(paste(rownames(x$byclass), ":"), width=25, adj = "right"),
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



Sens <- function(x, ...) {

  Conf(x, ...)[["byclass"]]["sens",]
}

Spec <- function(x, ...) {

  Conf(x, ...)[["byclass"]]["spec",]
}

