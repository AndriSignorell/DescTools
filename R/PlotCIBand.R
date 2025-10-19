
#' Confidence band and estimator line (base R)
#'
#' Draws a confidence band (lower/upper limits) and an estimator line
#' into an existing plot or, if needed, sets up a new plot.
#'
#' @param x Numeric vector of x values.
#' @param y 3-column matrix of estimator values (e.g., mean/predicted) and
#' lower/upper confidence interval.
#' @param col_band Fill color for the confidence band.
#'   Default: \code{grDevices::adjustcolor("grey", alpha.f = 0.35)}.
#' @param band_border Border color for the band polygon (use \code{NA} for no border).
#' @param col_line Line color for the estimator. Default: \code{"blue"}.
#' @param lwd Line width for the estimator line. Default: \code{2}.
#' @param lty Line type for the estimator line. Default: \code{1}.
#' @param add Logical; if \code{TRUE}, draw into an existing plot and do not
#'   set up a new one. Default: \code{FALSE}.
#' @param sort_x Logical; if \code{TRUE}, data are sorted by \code{x} so that
#'   the polygon does not self-intersect. Default: \code{TRUE}.
#' @param ... Additional graphical parameters that (only when \code{add = FALSE})
#'   are passed to \code{plot()} (e.g., \code{xlab}, \code{ylab}, \code{main},
#'   \code{xlim}, \code{ylim}, \code{axes}).
#'
#' @details
#' If \code{add = FALSE}, an empty plot is created with \code{type = "n"},
#' then the band is drawn via \code{polygon()} and the estimator via \code{lines()}.
#' The y-axis limits (\code{ylim}) are set to \code{range(low, high)} only if
#' no \code{ylim} was supplied via \code{...}. Missing values in \code{x},
#' \code{yhat}, \code{low}, \code{high} are removed; if fewer than two points
#' remain, the function aborts.
#'
#' If \code{add = TRUE}, axis limits are not modified; drawing is done on the
#' current device.
#'
#' @return
#' Invisibly returns a list with the (possibly sorted and NA-filtered) vectors:
#' \code{list(x = x, fit = yhat, lower = low, upper = high)}.
#'
#' @seealso \code{\link{PlotDotCI}} for plotting confidence intervals
#'
#' @examples
#' set.seed(1)
#' x <- 1:50
#' fit <- sin(x/8) + rnorm(50, 0, 0.05)
#' se  <- 0.15
#' low <- fit - 1.96 * se
#' high <- fit + 1.96 * se
#'
#' # 1) Standalone (sets up the plot):
#' PlotCIBand(x, cbind(fit, low, high),
#'   col_band = grDevices::adjustcolor("grey70", 0.4),
#'   col_line = "black", lwd = 2,
#'   xlab = "x", ylab = "y", main = "Estimator with 95% CI")
#'
#' # 2) Add to an existing plot:
#' plot(x, fit, type = "n", xlab = "x", ylab = "y")
#' PlotCIBand(cbind(fit, low, high), add = TRUE)
#'


#' @export
PlotCIBand <- function(x, y = NULL,
                       col_band = SetAlpha("grey"),
                       col_line = hred, lwd = 2, lty = 1,
                       band_border = NA, sort_x = TRUE, add=FALSE, ...) {
  # Mode detection:
  # - PlotCIBand(x, y): x = Vector, y = 3-column Matrix
  # - PlotCIBand(y):    y = 3-column Matrix, x = 1:nrow(y)
  
  if (is.null(y)) {
    ymat <- x
    if (is.null(dim(ymat)) || ncol(ymat) != 3)
      stop("When called with one argument, this must be a 3-column matrix: estimator, lowerCI, upperCI.")
    xvec <- seq_len(nrow(ymat))
  } else {
    ymat <- y
    xvec <- x
  }
  
  # Check input ...
  if (is.null(dim(ymat)) || ncol(ymat) != 3)
    stop("'y' must be a 3-column matrix: estimator, lowerCI, upperCI.")
  if (!is.numeric(xvec))
    stop("'x' must be numeric.")
  if (length(xvec) != nrow(ymat))
    stop("The length of 'x' must match nrow(y).")
  
  yhat <- ymat[, 1]
  low  <- ymat[, 2]
  high <- ymat[, 3]
  
  # Optionally sort x (for correct polygon)
  if (isTRUE(sort_x)) {
    o <- order(xvec)
    xvec <- xvec[o]; yhat <- yhat[o]; low <- low[o]; high <- high[o]
  }
  
  # Remove NAs
  keep <- stats::complete.cases(xvec, yhat, low, high)
  xvec <- xvec[keep]; yhat <- yhat[keep]; low <- low[keep]; high <- high[keep]
  
  if (length(xvec) < 2)
    stop("Too few valid points after filter NAs.")
  
  if(!add){
    # Plot (further parameters via ...)
    dots <- list(...)
    # Only set ylim if not explicitly present in ...
    if (!("ylim" %in% names(dots))) {
      dots$ylim <- range(low, high, na.rm = TRUE)
    }
    if (!("ylab" %in% names(dots))) {
      dots$ylab <- "x"
    }
    
    do.call(plot, c(list(x = xvec, y = yhat, type = "n"), dots))
    
  }
  
  # Band and line
  polygon(c(xvec, rev(xvec)), c(low, rev(high)),
          col = col_band, border = band_border)
  lines(xvec, yhat, col = col_line, lwd = lwd, lty = lty)
  
  invisible(list(x = xvec, fit = yhat, lower = low, upper = high))
  
}



# 
# set.seed(1)
# x <- 1:50
# fit  <- sin(x/8) + rnorm(50, 0, 0.05)
# se   <- 0.15
# y <- cbind(fit, fit - 1.96*se, fit + 1.96*se)
# 
# PlotCIBand(x,y,
#              col_band = adjustcolor("grey70", 0.4),
#              xlab = "x", ylab = "y", main = "Schaetzer mit 95%-KI",
#            panel.first=quote(grid()), las=1)
# 
# 
# PlotCIBand(y+0.2,
#              col_band = adjustcolor("lightblue", 0.35),
#              col_line = "navy", lwd = 2,
#              xlab = "Index", ylab = "y", main = "Automatische x-Achse", add=TRUE)

