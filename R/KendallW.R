


#' Kendall's Coefficient of Concordance W
#' 
#' Computes Kendall's coefficient of concordance, a popular measure of
#' association. It is an index of interrater reliability of ordinal data. The
#' coefficient could be corrected for ties within raters.
#' 
#' The test for Kendall's W is completely equivalent to
#' \code{\link[stats]{friedman.test}}. The only advantage of this test over
#' Friedman's is that Kendall's W has an interpretation as the coefficient of
#' concordance. The test itself is only valid for large samples.\cr Kendall's W
#' should be corrected for ties, if raters did not use a true ranking order for
#' the subjects.\cr
#' The function warns if ties are present and no correction has been required.\cr\cr
#' 
#' In the presence of NAs the algorithm is switched to a generalized form for 
#' randomly incomplete datasets introduced in Brueckl (2011).
#' This uses the mean Spearman's rho of all pairwise comparisons, 
#' (see Kendall, 1962):
#' W = (1+mean(rho)*(k-1)) / k
#' where k is the mean number of (pairwise) ratings per object and mean(rho) is 
#' calculated weighted, according to Taylor (1987), since the pairwise are 
#' possibly based on a different number of ratings, what must be reflected 
#' in weights.
#' In case of complete datasets, it yields the same results 
#' as usual implementations of Kendall's W, except for tied ranks. In case 
#' of tied ranks, the (pairwise) correction of s used, which (already with 
#' complete datasets) results in slightly different values than the tie 
#' correction explicitly specified for W.
#' 
#' @param x \eqn{n \times m}{k x m} matrix or dataframe, k subjects (in rows) m
#' raters (in columns).
#' @param correct a logical indicating whether the coefficient should be
#' corrected for ties within raters.
#' @param test a logical indicating whether the test statistic and p-value
#' should be reported.

#' @return Either a single value if test is set to \code{FALSE} or else \cr
#' 
#' a list with class \dQuote{htest} containing the following components:
#' \item{statistic}{the value of the chi-square statistic.} \item{p.value }{the
#' p-value for the test.} \item{method}{the character string \dQuote{Kendall's
#' coefficient of concordance W}.} \item{data.name}{a character string giving
#' the name(s) of the data.} \item{estimate}{the coefficient of concordance.}
#' \item{parameter}{the degrees of freedom df, the number of subjects examined
#' and the number of raters.}

#' @note This function was previously published as \code{kendall()} in the
#' \pkg{irr} package and has been integrated here without logical changes, but
#' with some adaptations in the result structure.

#' @author Andri Signorell <andri@signorell.net> based on code by Matthias Gamer <m.gamer@@uke.uni-hamburg.de>
#' and Markus Brueckl <markus.brueckl@tu-berlin.de>

#' @seealso \code{\link[stats]{cor}}, \code{\link{KappaM}},
#' \code{\link{CronbachAlpha}}, \code{\link{ICC}},
#' \code{\link[stats]{friedman.test}}

#' @references Kendall, M.G. (1948) \emph{Rank correlation methods}. London:
#' Griffin.
#' Kendall, M.G. (1962). Rank correlation methods (3rd ed.). London: Griffin.
#' Brueckl, M. (2011). Statistische Verfahren zur Ermittlung der 
#' Urteileruebereinstimmung. in: Altersbedingte Veraenderungen der 
#' Stimme und Sprechweise von Frauen, Berlin: Logos, 88–103.
#' Taylor, J.M.G. (1987). Kendall's and Spearman's correlation coefficients in the presence of a blocking variable. \emph{Biometrics}, 43, 409–416.


#' @keywords multivar

#' @examples
#' 
#' anxiety <- data.frame(rater1=c(3,3,3,4,5,5,2,3,5,2,2,6,1,5,2,2,1,2,4,3),
#'                       rater2=c(3,6,4,6,2,4,2,4,3,3,2,3,3,3,2,2,1,3,3,4),
#'                       rater3=c(2,1,4,4,3,2,1,6,1,1,1,2,3,3,1,1,3,3,2,2))
#' 
#' KendallW(anxiety, TRUE)
#' 
#' # with test results
#' KendallW(anxiety, TRUE, test=TRUE)
#' 
#' # example from Siegel and Castellan (1988)
#' d.att <- data.frame(
#'   id        = c(4,21,11),
#'   airfare   = c(5,1,4),
#'   climate   = c(6,7,5),
#'   season    = c(7,6,1),
#'   people    = c(1,2,3),
#'   program   = c(2,3,2),
#'   publicity = c(4,5,7),
#'   present   = c(3,4,6),
#'   interest  = c(8,8,8)
#' )
#' 
#' KendallW(t(d.att[, -1]), test = TRUE)
#' 
#' # which is perfectly the same as
#' friedman.test(y=as.matrix(d.att[,-1]), groups = d.att$id)
 
 
KendallW <- function(x, correct=FALSE, test=FALSE) {
  
  # see also old Jim Lemon function kendall.w
  # other solution: library(irr);  kendall(ratings, correct = TRUE)
  # http://www.real-statistics.com/reliability/kendalls-w/
  
  
  dname <- deparse(substitute(x))
  
  ratings <- as.matrix(x)
  ns <- nrow(ratings)  # number of subjects
  nr <- ncol(ratings)  # number of raters
  
  # check for NAs and escalate to
  # Brueckl, M. (2011). Statistische Verfahren zur Ermittlung der Urteileruebereinstimmung. in: Altersbedingte Veraenderungen der Stimme und Sprechweise von Frauen, Berlin: Logos, 88–103.
  if(sum(is.na(ratings)) > 0){

    # no correction required
    TIES <- FALSE
        
    N <- nrow(ratings)
    m <- ncol(ratings)
    
    rho <- ZeroIfNA(stats::cor(ratings, method = "spearman", use = "pairwise.complete"))
    
    w <- t(!is.na(ratings)) %*% (!is.na(ratings)) -1 
    w <- pmax(0, w[lower.tri(w)])
    wsum <- sum(w, na.rm = TRUE)

    kq <- mean(apply(!is.na(ratings), 1, sum), na.rm = TRUE)

    wmean_rho <- sum(rho[lower.tri(rho)] * w, na.rm = TRUE) / wsum

    coeff.name <- "W (generalized)"
    coeff <- (1 + wmean_rho * (kq - 1))/kq
    #test statistic
    stat <- kq * (N - 1) * coeff
    
  }  else {
    
    ratings.rank <- apply(ratings,2,rank)
    
    #Without correction for ties
    if (!correct) {
      
      #Test for ties
      TIES = FALSE
      testties <- apply(ratings, 2, unique)
      if (!is.matrix(testties)) 
        TIES = TRUE
      else { 
        if (length(testties) < length(ratings)) 
          TIES = TRUE 
        }
      
      coeff.name <- "W"
      coeff <- (12*var(apply(ratings.rank,1,sum))*(ns-1))/(nr^2*(ns^3-ns))
    }
    else { #With correction for ties
      
      Tj <- 0
      for (i in 1:nr) {
        rater <- table(ratings.rank[,i])
        ties  <- rater[rater>1]
        l 	  <- as.numeric(ties)
        Tj	  <- Tj + sum(l^3-l)
      }
      
      coeff.name <- "W (with ties correction)"
      coeff <- (12*var(apply(ratings.rank,1,sum))*(ns-1))/(nr^2*(ns^3-ns)-nr*Tj)
    }
    
    #test statistic
    stat  <- nr * (ns-1) * coeff
    
  }
  
  if(test){
    p.value <- pchisq(stat, ns-1, lower.tail = FALSE)
    method <- paste("Kendall's coefficient of concordance", coeff.name)
    alternative <- paste(coeff.name, "is greater 0")
    names(coeff) <- coeff.name
    
    rval <- list(
      estimate = coeff, 
      parameter=c(df=ns-1, subjects=ns, raters=nr),
      statistic = SetNames(stat, "Kendall chi-squared"), 
      p.value = p.value,
      alternative = alternative, method = method, 
      data.name = dname)
    
    class(rval) <- "htest"
    
  } else {
    rval <- coeff
  }
  
  if (!correct && TIES) warning("Coefficient may be incorrect due to ties")
  
  return(rval)
  
}
