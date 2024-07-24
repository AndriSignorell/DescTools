
# TMod


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



TMod <- function(..., FUN = NULL, order = NA){

  if (!requireNamespace("DescTools", quietly = TRUE))
    stop("package 'DescTools' must be installed")


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

      ord <- c(ordm, m2$name[!(m2$name %in% ordm)])
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

  mall <- DescTools::Abind(
    est = do.call(DescTools::MultMerge, lapply(lst,
                                    function(x) SetNames(x$coef[,c("est"), drop=FALSE], rownames=x$coef$name))),
    lci = do.call(DescTools::MultMerge, lapply(lst,
                                    function(x) SetNames(x$coef[,c("lci"), drop=FALSE], rownames=x$coef$name))),
    uci = do.call(DescTools::MultMerge, lapply(lst,
                                    function(x) SetNames(x$coef[,c("uci"), drop=FALSE], rownames=x$coef$name))),
    along=3)

  dimnames(mall)[[2]] <- modname

  if(!identical(order, NA)){
    # get order
    ordm <- rbind(m, SetNames(mm, colnames=colnames(m)))
    j <- order(unlist(ordm[match(order, ordm[, 1]), -1]))
    m <- m[, c(1, j+1)]
    mm <- mm[, c(1, j+1)]
  }

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

  m[, -1] <- apply(m[, -1, drop=FALSE], 2, DescTools::StrAlign, sep=".")
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
    xx <- DescTools::SplitToCol(as.data.frame(lapply(x[[1]], DescTools::StrTrim))[, -1],
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

  DescTools::ToWrd(as.matrix(ttt),
        font=font,
        align=align)

}


