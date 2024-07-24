



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

