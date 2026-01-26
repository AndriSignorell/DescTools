


.nctCI <- function(tval, df, conf.level = 0.95, tol = 1e-8) {
  
  stopifnot(
    is.numeric(tval), length(tval) == 1,
    is.numeric(df), df > 0,
    is.numeric(conf.level), conf.level > 0, conf.level < 1
  )
  
  t_abs <- abs(tval)
  alpha <- 1 - conf.level
  probs <- c(alpha / 2, 1 - alpha / 2)
  
  f <- function(delta, p) {
    suppressWarnings(
      pt(t_abs, df = df, ncp = delta) - p
    )  
  }
  
  find_root <- function(p) {
    search <- max(1, t_abs)
    for (i in 1:50) {
      lo <- -search
      hi <-  search
      if (f(lo, p) * f(hi, p) < 0)
        return(uniroot(f, c(lo, hi), p = p, tol = tol)$root)
      search <- search * 2
    }
    stop("Could not bracket root for noncentral t CI.")
  }
  
  lower <- find_root(probs[2])
  upper <- find_root(probs[1])
  
  if (tval < 0) {
    tmp <- -upper
    upper <- -lower
    lower <- tmp
  }
  
  return(  c(lci = lower, uci = upper)  )
}



# # tests for .nctCI 
# library(testthat)
# 
# test_that(".nctCI returns numeric CI with correct structure", {
#   
#   res <- .nctCI(tval = 2.5, df = 20, conf.level = 0.95)
#   
#   expect_type(res, "double")
#   expect_length(res, 2)
#   expect_named(res, c("lci", "uci"))
#   expect_true(res["lci"] < res["uci"])
# })
# 
# 
# test_that(".nctCI correctly inverts noncentral t distribution", {
#   tval <- 2.3
#   df   <- 28
#   conf.level <- 0.95
#   alpha <- 1 - conf.level
#   
#   ci <- .nctCI(tval, df, conf.level)
#   
#   p_lower <- pt(abs(tval), df = df, ncp = ci["lci"])
#   p_upper <- pt(abs(tval), df = df, ncp = ci["uci"])
#   
#   expect_equal(p_lower, 1 - alpha / 2, tolerance = 1e-6)
#   expect_equal(p_upper, alpha / 2,     tolerance = 1e-6)
# })
# 
# 
# 
# test_that(".nctCI is symmetric with respect to sign of t", {
#   df   <- 15
#   conf.level <- 0.95
#   
#   ci_pos <- unname(.nctCI( 2.0, df, conf.level))
#   ci_neg <- unname(.nctCI(-2.0, df, conf.level))
#   
#   expect_equal(ci_neg["lci"], -ci_pos["uci"], tolerance = 1e-6)
#   expect_equal(ci_neg["uci"], -ci_pos["lci"], tolerance = 1e-6)
# })
# 
# 
# 
# 
# test_that(".nctCI matches MBESS across representative settings", {
#   skip_if_not_installed("MBESS")
#   
#   cases <- list(
#     list(t = 1.8,  df = 28,  conf = 0.95),
#     list(t = -2.5, df = 10,  conf = 0.90)
#   )
#   
#   for (x in cases) {
#     ref <- MBESS::conf.limits.nct(
#       x$t, x$df, conf.level = x$conf
#     )
#     ci <- .nctCI(x$t, x$df, x$conf)
#     
#     expect_equal(
#       unname(ci[c("lci", "uci")]),
#       as.numeric(ref[c(1, 3)]),
#       tolerance = 1e-6
#     )
#   }
# })
# 
# 
# test_that(".nctCI works for very small t-values", {
#   ci <- .nctCI(tval = 0.05, df = 50, conf.level = 0.95)
#   
#   expect_true(is.finite(ci["lci"]))
#   expect_true(is.finite(ci["uci"]))
#   expect_true(ci["lci"] <= 0)
#   expect_true(ci["uci"] >= 0)
# })
# 
# 
# test_that(".nctCI works for large degrees of freedom", {
#   ci <- .nctCI(tval = 2.0, df = 1000, conf.level = 0.95)
#   
#   expect_true(is.finite(ci["lci"]))
#   expect_true(is.finite(ci["uci"]))
# })
# 
# 
# test_that(".nctCI works for high confidence levels", {
#   ci <- .nctCI(tval = 2.0, df = 30, conf.level = 0.99)
#   
#   expect_true(ci["uci"] - ci["lci"] > 0)
# })
# 
# 
# test_that(".nctCI fails gracefully on invalid input", {
#   expect_error(.nctCI("a", 10), "numeric")
#   expect_error(.nctCI(2, -1),  "df")
#   expect_error(.nctCI(2, 10, conf.level = 1.5))
# })
# 





# interactive only

# test_that(".nctCI regression snapshot", {
#   expect_snapshot(.nctCI(2.5, 20))
#   expect_snapshot(.nctCI(1.2, 50))
# })
# 


# bad solution!!
# .nctCI <- function(t, df, conf.level) {
#   
#   alpha <- 1 - conf.level
#   probs <- c(alpha/2, 1 - alpha/2)
#   
#   ncp <- suppressWarnings(optim(par = 1.1 * rep(t, 2), fn = function(x) {
#     p <- pt(q = t, df = df, ncp = x)
#     abs(max(p) - probs[2]) + abs(min(p) - probs[1])
#   }, control = list(abstol = 0.000000001)))
#   
#   t_ncp <- unname(sort(ncp$par))
#   
#   return(t_ncp)
#   
# }




# .nctCI <- function(tval.1, df, conf.level) {
# 
#   # Function for finding the upper and lower conf.levelidence limits for the noncentrality from noncentral t distributions.
#   # Especially helpful when forming conf.levelidence intervals around the standardized effect size, Cohen's d.
# 
#   ###################################################################################################################
#   # The following code was adapted from code written by Michael Smithson:
#   # Australian National University, sometime around the early part of October, 2001
#   # Adapted by Joe Rausch & Ken Kelley: University of Notre Dame, in January 2002.
#   # Available at: JRausch@nd.edu & KKelley@nd.edu
#   ###################################################################################################################
# 
# 
#   # tval.1 is the observed t value, df is the degrees of freedom (group size need not be equal), and conf.level is simply 1 - alpha
# 
#   #         Result <- matrix(NA,1,4)
#   tval <- abs(tval.1)
# 
# 
#   ############################This part Finds the Lower bound for the conf.levelidence interval###########################
#   ulim <- 1 - (1-conf.level)/2
# 
#   # This first part finds a lower value from which to start.
#   lc <- c(-tval,tval/2,tval)
#   while(pt(tval, df, lc[1])<ulim)    {
#     lc <- c(lc[1]-tval,lc[1],lc[3])
#   }
# 
#   # This next part finds the lower limit for the ncp.
#   diff <- 1
#   while(diff > .00000001)    {
#     if(pt(tval, df, lc[2]) <ulim)
#       lc <- c(lc[1],(lc[1]+lc[2])/2,lc[2])
#     else lc <- c(lc[2],(lc[2]+lc[3])/2,lc[3])
#     diff <- abs(pt(tval,df,lc[2]) - ulim)
#     ucdf <- pt(tval,df,lc[2])
#   }
#   res.1 <- ifelse(tval.1 >= 0,lc[2],-lc[2])
# 
#   ############################This part Finds the Upper bound for the conf.levelidence interval###########################
#   llim <- (1-conf.level)/2
# 
#   # This first part finds an upper value from which to start.
#   uc <- c(tval,1.5*tval,2*tval)
#   while(pt(tval,df,uc[3])>llim)   {
#     uc <- c(uc[1],uc[3],uc[3]+tval)
#   }
# 
#   # This next part finds the upper limit for the ncp.
#   diff <- 1
#   while(diff > .00000001)         {
#     if(pt(tval,df,uc[2])<llim)
#       uc <- c(uc[1],(uc[1]+uc[2])/2,uc[2])
#     else uc <- c(uc[2],(uc[2]+uc[3])/2,uc[3])
#     diff <- abs(pt(tval,df,uc[2]) - llim)
#     lcdf <- pt(tval,df,uc[2])
#   }
#   res <- ifelse(tval.1 >= 0,uc[2],-uc[2])
# 
# 
#   #################################This part Compiles the results into a matrix#####################################
# 
#   return(c(lwr.ci=min(res, res.1), lprob=ucdf, upr.ci=max(res, res.1), uprob=lcdf))
# 
#   #        Result[1,1] <- min(res,res.1)
#   #         Result[1,2] <- ucdf
#   #         Result[1,3] <- max(res,res.1)
#   #         Result[1,4] <- lcdf
#   # dimnames(Result) <- list("Values", c("Lower.Limit", "Prob.Low.Limit", "Upper.Limit", "Prob.Up.Limit"))
#   #         Result
# }
