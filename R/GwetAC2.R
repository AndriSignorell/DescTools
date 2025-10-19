#' #' Gwet's AC2 for Multiple Raters (Ordinal / Weighted)
#' #'
#' #' @description
#' #' Computes Gwet's AC2 agreement coefficient for \emph{m >= 2} raters with
#' #' ordinal (or more generally, weighted) categories, allowing missing ratings.
#' #' Supports a formula interface \code{y ~ id | rater} (long format) via
#' #' \code{LongToSquare()} and a default method that accepts a subject-by-rater
#' #' matrix. Confidence intervals are available via an asymptotic delta-method
#' #' variance and via nonparametric bootstrap over subjects.
#' #'
#' #' @details
#' #' Let \eqn{n} be the number of subjects and \eqn{m_s} the number of valid
#' #' ratings for subject \eqn{s}. With a \eqn{K \times K} weight matrix
#' #' \eqn{W=(w_{cd})} (diagonal 1, off-diagonal in \eqn{[0,1]}), the subject-wise
#' #' weighted observed agreement is
#' #' \deqn{A_s^{(w)} = \frac{\sum_{c,d} w_{cd} n_{sc} n_{sd} - \sum_c w_{cc} n_{sc}}
#' #'                        {m_s (m_s - 1)} ,}
#' #' where \eqn{n_{sc}} is the number of raters assigning category \eqn{c} to
#' #' subject \eqn{s}. The overall observed agreement is
#' #' \eqn{P_o^{(w)} = \frac{1}{n}\sum_s A_s^{(w)}}.
#' #'
#' #' Let \eqn{p} be the vector of overall category prevalences across all ratings
#' #' (all raters and subjects). Define \eqn{S = p^\top W p} and
#' #' \eqn{P_e^{(w)} = 1 - S}. Then the AC2 coefficient is
#' #' \deqn{AC2 = \frac{P_o^{(w)} - P_e^{(w)}}{1 - P_e^{(w)}} = \frac{P_o^{(w)} + S - 1}{S}.}
#' #' For \eqn{W = I_K} (unweighted), this reduces to AC1.
#' #'
#' #' The asymptotic variance uses a subject-level delta method based on the
#' #' contributions \eqn{(A_s^{(w)}, n_{s1}/\bar m, \dots, n_{sK}/\bar m)} with
#' #' \eqn{\bar m} the average number of ratings per subject.
#' #'
#' #' @param x For the default method: an \eqn{n \times m} matrix (or data.frame)
#' #'   of ratings where rows are subjects and columns are raters. Entries are
#' #'   ordinal categories (factor/character/integers); \code{NA} allowed.
#' #' @param weights Type of weights: one of \code{"linear"}, \code{"quadratic"},
#' #'   \code{"unweighted"} (i.e., identity), or \code{"custom"}.
#' #' @param w For \code{weights="custom"}: a \eqn{K \times K} numeric matrix with
#' #'   entries in \eqn{[0,1]} and unit diagonal; otherwise ignored.
#' #' @param ci One of \code{"none"}, \code{"asymptotic"} (delta method), or \code{"bootstrap"}.
#' #' @param conf.level Confidence level for the interval (default \code{0.95}).
#' #' @param B Number of bootstrap replicates when \code{ci = "bootstrap"} (default \code{1000}).
#' #' @param seed Optional random seed for bootstrap reproducibility.
#' #' @param ... Further arguments passed to \code{LongToSquare()} in the formula method.
#' #'
#' #' @return An object of class \code{"htest"} with elements:
#' #' \item{statistic}{Named numeric: \code{AC2}.}
#' #' \item{estimate}{Named vector with \code{Po.w} (weighted observed) and
#' #'   \code{S} (weighted prevalence term \eqn{p^\top W p}).}
#' #' \item{parameter}{Named vector with \code{subjects} and \code{raters.eff}
#' #'   (average number of valid ratings per subject).}
#' #' \item{conf.int}{Confidence interval for AC2 if requested.}
#' #' \item{method}{Method string.}
#' #' \item{data.name}{Description of the data.}
#' #'
#' #' @examples
#' #' # Beispiel: 5 Subjekte x 4 Rater, Ordnung V < N < P
#' #' d.ratings <- data.frame(
#' #'   subj = c("1","2","3","4","5"),
#' #'   rtr1 = factor(c("V","V","V","V","P"), levels=c("V","N","P"), ordered=TRUE),
#' #'   rtr2 = factor(c("V","N","V","V","P"), levels=c("V","N","P"), ordered=TRUE),
#' #'   rtr3 = factor(c("V","P","V","V","P"), levels=c("V","N","P"), ordered=TRUE),
#' #'   rtr4 = factor(c("V","V","V","V","N"), levels=c("V","N","P"), ordered=TRUE)
#' #' )
#' #' d.long <- reshape(d.ratings, varying=2:5, idvar="subj",
#' #'                   times=names(d.ratings)[2:5],
#' #'                   v.names="rat", timevar="rater",
#' #'                   direction="long",
#' #'                   new.row.names=seq(prod(dim(d.ratings))))
#' #'
#' #' # Formel-Interface (linear/quadratisch)
#' #' AC2(rat ~ subj | rater, data = d.long, weights="linear", ci="asymptotic")
#' #' AC2(rat ~ subj | rater, data = d.long, weights="quadratic",
#' #'     ci="bootstrap", B=500, seed=1)
#' #'
#' #' # Custom-Gewichte
#' #' X <- as.matrix(d.ratings[,-1])
#' #' K <- length(levels(X[,1]))
#' #' D <- as.matrix(dist(seq_len(K), method="manhattan"))
#' #' W <- 1 - (D / max(D))     # lineare Gewichte als Beispiel
#' #' diag(W) <- 1
#' #' AC2(X, weights="custom", w=W, ci="asymptotic")
#' #'
#' 
#' 
#' 
#' # ---- interne Hilfen ----
#' .AC_make_weights <- function(K, type = c("linear","quadratic","unweighted"), w = NULL) {
#'   type <- match.arg(type)
#'   if (type == "unweighted") {
#'     W <- diag(1, K)
#'   } else {
#'     # Positionen 1..K sind geordnet
#'     idx <- seq_len(K)
#'     D <- as.matrix(stats::dist(idx, method = "manhattan"))
#'     if (type == "linear") {
#'       W <- 1 - D / max(D)
#'     } else { # quadratic
#'       W <- 1 - (D / max(D))^2
#'     }
#'     diag(W) <- 1
#'   }
#'   if (!is.null(w)) {
#'     if (!is.matrix(w) || any(dim(w) != K))
#'       stop("'w' must be a K x K matrix.")
#'     if (any(w < 0 | w > 1)) stop("Custom weights must be in [0,1].")
#'     if (any(abs(diag(w) - 1) > 1e-12)) stop("Diagonal of custom weights must be 1.")
#'     W <- w
#'   }
#'   W
#' }
#' 
#' .AC2_core <- function(X, weights = c("linear","quadratic","unweighted","custom"), w = NULL) {
#'   X <- as.matrix(X)
#'   # Kategorien zu Integern 1..K harmonisieren (NA erlaubt)
#'   if (!is.numeric(X)) {
#'     X <- apply(X, 2, function(col) {
#'       if (is.factor(col)) as.character(col) else as.character(col)
#'     })
#'     levs <- sort(unique(na.omit(as.vector(X))))
#'     K <- length(levs)
#'     map <- setNames(seq_len(K), levs)
#'     X <- apply(X, 2, function(col) unname(map[col]))
#'   } else {
#'     levs <- sort(unique(na.omit(as.vector(X))))
#'     K <- length(levs)
#'   }
#'   
#'   m_s <- rowSums(!is.na(X))
#'   if (any(m_s < 2))
#'     stop("Each subject must have at least two non-missing ratings.")
#'   
#'   # Gewichtsmatrix
#'   weights <- match.arg(weights)
#'   W <- .AC_make_weights(K,
#'                         type = if (weights %in% c("linear","quadratic","unweighted")) weights else "linear",
#'                         w = if (weights == "custom") w else NULL)
#'   
#'   n <- nrow(X)
#'   choose2 <- function(v) ifelse(v >= 2, v*(v-1)/2, 0)
#'   
#'   # A_s^(w) berechnen
#'   A_s_w <- numeric(n)
#'   n_sc_mat <- matrix(0, nrow = n, ncol = K)
#'   for (s in seq_len(n)) {
#'     row <- X[s, ]
#'     row <- row[!is.na(row)]
#'     ms <- length(row)
#'     tab <- tabulate(row, nbins = K)
#'     n_sc_mat[s, ] <- tab
#'     if (ms < 2) { A_s_w[s] <- NA_real_; next }
#'     # gewichtete Summe über geordnete Paare minus Eigenpaare:
#'     num <- sum((W %*% tab) * tab) - sum(diag(W) * tab)
#'     den <- ms * (ms - 1)
#'     A_s_w[s] <- num / den
#'   }
#'   if (anyNA(A_s_w))
#'     stop("Internal error: NA in A_s_w after requiring m_s >= 2.")
#'   
#'   Po_w <- mean(A_s_w)
#'   
#'   # Prävalenzen p_c
#'   total_m <- sum(m_s)
#'   p_c <- colSums(n_sc_mat) / total_m
#'   
#'   # S = p' W p  ;  Pe^(w) = 1 - S  ;  AC2 = (Po_w + S - 1)/S
#'   S <- as.numeric(t(p_c) %*% W %*% p_c)
#'   AC2 <- (Po_w + S - 1) / S
#'   
#'   # Asymptotische Varianz (Delta-Methode)
#'   m_bar <- mean(m_s)
#'   g_mat <- cbind(A_s_w, n_sc_mat / m_bar)  # n x (1+K)
#'   Sigma_hat <- stats::cov(g_mat)
#'   
#'   # Gradienten:
#'   # d(AC2)/dPo = 1/S
#'   d_dPo <- 1 / S
#'   # dS/dp = 2 W p  (W symmetrisch)
#'   dS_dp <- as.numeric(2) * as.vector(W %*% p_c)
#'   # d(AC2)/dp = - (Po_w - 1) / S^2 * dS/dp
#'   d_dp <- - (Po_w - 1) * dS_dp / (S^2)
#'   grad <- c(d_dPo, d_dp)
#'   var_asym <- as.numeric( (t(grad) %*% Sigma_hat %*% grad) / n )
#'   
#'   list(AC2 = AC2, Po_w = Po_w, S = S,
#'        n = n, m_eff = m_bar, K = K,
#'        var_asym = var_asym,
#'        levels = levs, W = W)
#' }
#' 
#' #' @rdname GwetAC2
#' #' @export
#' GwetAC2 <- function(x,
#'                         weights = c("linear","quadratic","unweighted","custom"),
#'                         w = NULL,
#'                         ci = c("none","asymptotic","bootstrap"),
#'                         conf.level = 0.95,
#'                         B = 1000,
#'                         seed = NULL) {
#'   ci <- match.arg(ci)
#'   core <- .AC2_core(x, weights = weights, w = w)
#'   
#'   est <- core$AC2
#'   out <- list(
#'     statistic = c(AC2 = est),
#'     estimate  = c(Po.w = core$Po_w, S = core$S),
#'     parameter = c(subjects = core$n, raters.eff = core$m_eff),
#'     method    = paste0("Gwet's AC2 (multi-rater, weighted: ",
#'                        if (missing(weights)) "linear" else match.arg(weights), ")"),
#'     data.name = deparse(substitute(x))
#'   )
#'   
#'   if (ci == "asymptotic") {
#'     se <- sqrt(max(core$var_asym, 0))
#'     z  <- stats::qnorm(1 - (1 - conf.level)/2)
#'     ci_lo <- est - z * se
#'     ci_hi <- est + z * se
#'     out$conf.int <- c(max(-1, ci_lo), min(1, ci_hi))
#'     attr(out$conf.int, "conf.level") <- conf.level
#'     out$method <- paste0(out$method, ", asymptotic CI")
#'   } else if (ci == "bootstrap") {
#'     if (!is.null(seed)) set.seed(seed)
#'     X <- as.matrix(x)
#'     n <- nrow(X)
#'     AC_boot <- numeric(B)
#'     for (b in seq_len(B)) {
#'       idx <- sample.int(n, n, replace = TRUE)
#'       AC_boot[b] <- .AC2_core(X[idx, , drop = FALSE],
#'                               weights = weights, w = w)$AC2
#'     }
#'     qs <- stats::quantile(AC_boot,
#'                           probs = c((1 - conf.level)/2, 1 - (1 - conf.level)/2),
#'                           na.rm = TRUE, names = FALSE)
#'     out$conf.int <- c(max(-1, qs[1]), min(1, qs[2]))
#'     attr(out$conf.int, "conf.level") <- conf.level
#'     out$method <- paste0(out$method, ", bootstrap CI (subjects)")
#'   }
#'   
#'   class(out) <- "htest"
#'   out
#' }
#' 
#' 
