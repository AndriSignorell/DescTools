
#' Gwet's AC1 for Multiple Raters (Nominal)

#' @description
#' Computes Gwet's AC1 agreement coefficient for \emph{m >= 2} raters
#' and nominal categories, allowing missing ratings. The default method 
#' accepts a subject-by-rater rating matrix or as alternative two vectors.
#' Use formula interface \code{y ~ id | rater} 
#' (long format) via \code{\link{RaterFrame}()}. 
#'
#' It returns Gwet's AC1 along with an optional confidence interval based on
#' (i) an asymptotic delta-method variance or (ii) nonparametric bootstrap
#' over subjects.
#'
#' @name GwetAC1
#' 
#' @details
#' Let \eqn{n} be the number of subjects and \eqn{m_s} the number of valid
#' ratings for subject \eqn{s}. For each subject, the observed pairwise
#' agreement is
#' \deqn{A_s = \frac{\sum_c {n_{sc} \choose 2}}{ {m_s \choose 2} },}
#' where \eqn{n_{sc}} is the number of raters assigning category \eqn{c} to
#' subject \eqn{s}. The overall observed agreement is
#' \eqn{P_o = \frac{1}{n}\sum_s A_s}.
#'
#' Let \eqn{p_c} denote the overall prevalence of category \eqn{c} across all
#' ratings (all raters and subjects), and
#' \eqn{P_e = \sum_c p_c(1-p_c)}. Then Gwet's AC1 for multiple raters is
#' \deqn{AC1 = \frac{P_o - P_e}{1 - P_e}.}
#'
#' For the asymptotic variance, we use a subject-level delta-method:
#' define the subject contribution vector
#' \eqn{g_s = \big(A_s,\ n_{s1}/\bar m,\dots,n_{sK}/\bar m\big)^\top},
#' where \eqn{\bar m} is the average number of ratings per subject and
#' \eqn{K} is the number of categories. The empirical covariance of
#' \eqn{g_s} yields (via delta-method) an estimate of \eqn{Var(AC1)}.
#'
#' Bootstrap CIs resample subjects with replacement.
#'
#' @param x For the default method: an \eqn{n \times m} matrix (or data.frame)
#'   of ratings where rows are subjects and columns are raters. Entries are
#'   nominal categories (factor, character, or integers); \code{NA} allowed.
#' @param y For the default method: an \eqn{n \times m} matrix (or data.frame)
#'   of ratings where rows are subjects and columns are raters. Entries are
#'   nominal categories (factor, character, or integers); \code{NA} allowed.
#' @param ci.type Type of confidence interval: one of
#'   \code{"none"}, \code{"asymptotic"} (delta method), or \code{"bootstrap"}.
#' @param conf.level Confidence level for the interval (default \code{0.95}).
#' 
#' @param out either \code{"def"} or \code{"ext"} defining the set of results. 
#' If \code{"def"} is selected, only the value of the statistic is 
#' returned, supplemented with the confidence interval 
#' if necessary. If the argument is set to \code{"ext"}, an 
#' extended result set with various intermediate results is returned.
#' 
#' @param ... Further arguments are passed to \code{BootCI()}  to control
#' type and parameters of the bootstrap confidence intervals.
#'
#' @return An object of class \code{"htest"} with elements:
#' \item{statistic}{Named numeric: \code{AC1}.}
#' \item{estimate}{Named vector with \code{Po} (observed agreement) and
#'   \code{Pe} (expected agreement).}
#' \item{parameter}{Named vector with \code{subjects} and \code{raters.eff}
#'   (average number of valid ratings per subject).}
#' \item{conf.int}{Confidence interval for AC1 if requested.}
#' \item{method}{Method string.}
#' \item{data.name}{Description of the data.}
#'
#' @examples
#' # Wide example (5 subjects x 4 raters), nominal categories V/N/P:
#' d.ratings <- data.frame(
#'   subj = c("1","2","3","4","5"),
#'   rtr1 = factor(c("V","V","V","V","P"), levels=c("V","N","P")),
#'   rtr2 = factor(c("V","N","V","V","P"), levels=c("V","N","P")),
#'   rtr3 = factor(c("V","P","V","V","P"), levels=c("V","N","P")),
#'   rtr4 = factor(c("V","V","V","V","N"), levels=c("V","N","P"))
#' )
#' d.long <- reshape(d.ratings, varying=2:5, idvar="subj",
#'                   times=names(d.ratings)[2:5],
#'                   v.names="rat", timevar="rater",
#'                   direction="long",
#'                   new.row.names=seq(prod(dim(d.ratings))))
#'
#' # Formula interface (long data):
#' # AC1(y ~ id | rater)
#' GwetAC1(RaterFrame(rat ~ subj | rater, data = d.long), ci = "asymptotic")
#'
#' # Default interface (matrix/data.frame):
#' m <- as.matrix(d.ratings[,-1])
#' GwetAC1(m, ci = "bootstrap", B = 500, seed = 123)
#'
#'@concept{ ~irr }


# https://support.sas.com/resources/papers/proceedings/proceedings/forum2007/186-2007.pdf
# https://www.rama.mahidol.ac.th/ceb/sites/default/files/public/pdf/stata_journal/sj18-4.pdf
# S. 871


#' @rdname GwetAC1
#' @export
GwetAC1 <- function(x, y = NULL,
                   conf.level = 0.95,
                   ci.type = c("asymptotic","bootstrap"),
                   out = c("def", "ext"), ...) {
  
  m <- .NormalizeToConfusion(x, y)
  
  res <- .GwetAC1(m, conf.level=conf.level) 
  
  if(match.arg(ci.type) == "bootstrap"){
    ci <- BootCI(x,y, 
                 FUN=function(x,y) GwetAC1(x, y, 
                                          conf.level = NA), 
                 ...)
    
    res[["ci"]] <- ci
  }

  if(out == "def"){
    if(!is.na(conf.level)) {
      res <- SetNames(unlist(c(res["ac1"], res["ci"])), names=c("est","lci","uci"))
    } else {
      res <- res["ac1"]
    }
  } else {
    # res is what comes back from .GwetAC
  }
    
  return (res)
}


.GwetAC1 <- function(m, conf.level=NULL){
  
  # counts -> proportions
  n <- sum(m)
  if (!is.finite(n) || n <= 0) stop("Non-positive total.")
  P <- m / n
  k <- nrow(P)
  
  # observed agreement
  Po <- sum(diag(P))
  
  # margins and e_i
  pi_dot  <- rowSums(P)
  p_dot_i <- colSums(P)
  e <- (pi_dot + p_dot_i) / 2
  
  # Pe for AC1
  Pe <- sum(e * (1 - e) / (k - 1))
  
  # handle degenerate case
  if (abs(1 - Pe) < .Machine$double.eps^0.5) {
    ac1 <- NA_real_
    se  <- NA_real_
    ci  <- c(NA_real_, NA_real_)
    return(list(ac1 = ac1, Po = Po, Pe = Pe, se = se, ci = ci, n = n, k = k))
  }
  
  ac1 <- (Po - Pe) / (1 - Pe)
  
  # Variance (SAS PROC FREQ formula)
  A <- sum(diag(P) * (1 - e) / (k - 1)) - Po * Pe
  # expand B
  E_sum <- outer(e, e, function(ei, ej) (1 - (ei + ej)/2)^2)
  B <- sum(P * (E_sum / (k - 1)^2)) - Pe^2
  
  var_ac1 <- (Po * (1 - Po) - 4 * (1 - ac1) * A + 4 * (1 - ac1)^2 * B) /
    (n * (1 - Pe)^2)
  var_ac1 <- max(var_ac1, 0)  # numerical guard
  se <- sqrt(var_ac1)
  
  if (is.null(conf.level)) {
    ci <- c(NA_real_, NA_real_)
  } else {
    z <- qnorm(1 - (1 - conf.level)/2)
    ci <- c(ac1 - z * se, ac1 + z * se)
  }
  
  list(ac1 = ac1, Po = Po, Pe = Pe, se = se, ci = ci, n = n, k = k)
  
}



# 
# .GwetAC1Core <- function(x, conf.level = NA) {
# 
#   # counts -> proportions
#   n <- sum(x)
#   if (!is.finite(n) || n <= 0) stop("Non-positive total.")
#   P <- x / n
#   k <- nrow(P)
#   
#   # observed agreement
#   Po <- sum(diag(P))
#   
#   # margins and e_i
#   pi_dot  <- rowSums(P)
#   p_dot_i <- colSums(P)
#   e <- (pi_dot + p_dot_i) / 2
#   
#   # Pe for AC1
#   Pe <- sum(e * (1 - e) / (k - 1))
#   
#   # handle degenerate case
#   if (abs(1 - Pe) < .Machine$double.eps^0.5) {
#     ac1 <- NA_real_
#     se  <- NA_real_
#     ci  <- c(NA_real_, NA_real_)
#     return(list(ac1 = ac1, Po = Po, Pe = Pe, se = se, ci = ci, n = n, k = k))
#   }
#   
#   ac1 <- (Po - Pe) / (1 - Pe)
#   
#   # Variance (SAS PROC FREQ formula)
#   A <- sum(diag(P) * (1 - e) / (k - 1)) - Po * Pe
#   # expand B
#   E_sum <- outer(e, e, function(ei, ej) (1 - (ei + ej)/2)^2)
#   B <- sum(P * (E_sum / (k - 1)^2)) - Pe^2
#   
#   var_ac1 <- (Po * (1 - Po) - 4 * (1 - ac1) * A + 4 * (1 - ac1)^2 * B) /
#     (n * (1 - Pe)^2)
#   var_ac1 <- max(var_ac1, 0)  # numerical guard
#   se <- sqrt(var_ac1)
#   
#   if (is.null(conf.level)) {
#     ci <- c(NA_real_, NA_real_)
#   } else {
#     z <- qnorm(1 - (1 - conf.level)/2)
#     ci <- c(ac1 - z * se, ac1 + z * se)
#   }
#   
#   list(ac1 = ac1, Po = Po, Pe = Pe, se = se, ci = ci, n = n, k = k)
# }
# 
# 
# 
# 
# # 
# # ci <- match.arg(ci)
# # X <- as.matrix(x)
# # 
# # core <- .AC1_core(X)
# # 
# # # Build htest-like result
# # est <- core$AC1
# # out <- list(
# #   statistic = c(AC1 = est),
# #   estimate  = c(Po = core$Po, Pe = core$Pe),
# #   parameter = c(subjects = core$n, raters.eff = core$m_eff),
# #   method    = "Gwet's AC1 (multi-rater, nominal)",
# #   data.name = deparse(substitute(x))
# # )
# # 
# # # Confidence intervals
# # if (ci == "asymptotic") {
# #   se <- sqrt(max(core$var_asym, 0))
# #   alpha <- 1 - conf.level
# #   z <- stats::qnorm(1 - alpha/2)
# #   ci_lo <- est - z * se
# #   ci_hi <- est + z * se
# #   out$conf.int <- c(max(-1, ci_lo), min(1, ci_hi))
# #   attr(out$conf.int, "conf.level") <- conf.level
# #   out$method <- paste0(out$method, ", asymptotic CI")
# # } else if (ci == "bootstrap") {
# #   if (!is.null(seed)) set.seed(seed)
# #   n <- nrow(X)
# #   AC1_boot <- numeric(B)
# #   for (b in seq_len(B)) {
# #     idx <- sample.int(n, n, replace = TRUE)
# #     AC1_boot[b] <- .AC1_core(X[idx, , drop = FALSE])$AC1
# #   }
# #   alpha <- 1 - conf.level
# #   qs <- stats::quantile(AC1_boot, probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE, names = FALSE)
# #   out$conf.int <- c(max(-1, qs[1]), min(1, qs[2]))
# #   attr(out$conf.int, "conf.level") <- conf.level
# #   out$method <- paste0(out$method, ", bootstrap CI (subjects)")
# # }
# # 
# # class(out) <- "htest"
# # out
