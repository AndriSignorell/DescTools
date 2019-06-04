
# The BoutrosLab.statistics.general package is copyright (c) 2012 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

ks.test.critical.value <- function(n, conf, alternative = "two.sided") {

  if(alternative == "one-sided") conf <- 1- (1-conf)*2;

  # for the small sample size

  if (n < 50) {
    # use the exact distribution from the C code in R
    exact.kolmogorov.pdf <- function(x) {
      p <- .Call("pKolmogorov2x", p = as.double(x), as.integer(n), PACKAGE = "DescTools");
      return(p - conf);
    }

    critical.value <- uniroot(exact.kolmogorov.pdf, lower = 0, upper = 1)$root;
  }

  # if the sample size is large(>50), under the null hypothesis, the absolute value of the difference
  # of the empirical cdf and the theoretical cdf should follow a kolmogorov distribution

  if (n >= 50) {
    # pdf of the kolmogorov distribution minus the confidence level
    kolmogorov.pdf <- function(x) {
      i <- 1:10^4;
      sqrt(2*pi) / x * sum(exp(-(2*i - 1)^2*pi^2/(8*x^2))) - conf;
    }

    # the root of the function above
    # is the critical value for a specific confidence level multiplied by sqrt(n);
    critical.value <- uniroot(kolmogorov.pdf , lower = 10^(-6), upper = 3)$root / sqrt(n);
  }

  return(critical.value);
}



# The BoutrosLab.statistics.general package is copyright (c) 2011 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

create.qqplot.fit.confidence.interval <- function(x, distribution = qnorm,
                                                  conf = 0.95, conf.method = "both",
                                                  reference.line.method = "quartiles") {

  # remove the NA and sort the sample
  # the QQ plot is the plot of the sorted sample against the corresponding quantile from the theoretical distribution
  sorted.sample <- sort(x[!is.na(x)]);

  # the corresponding probabilities, should be (i - 0.5)/n, where i = 1,2,3,...,n
  probabilities <- ppoints(length(sorted.sample));

  # the corresponding quantile under the theoretical distribution
  theoretical.quantile <- distribution(probabilities);

  if(reference.line.method == "quartiles") {
    # get the numbers of the 1/4 and 3/4 quantile in order to draw a reference line
    quantile.x.axis <- quantile(sorted.sample, c(0.25, 0.75));
    quantile.y.axis <- distribution(c(0.25, 0.75));

    # the intercept and slope of the reference line
    b <- (quantile.x.axis[2] - quantile.x.axis[1]) / (quantile.y.axis[2] - quantile.y.axis[1]);
    a <- quantile.x.axis[1] - b * quantile.y.axis[1];
  }
  if(reference.line.method == "diagonal") {
    a <- 0;
    b <- 1;
  }
  if(reference.line.method == "robust") {
    coef.linear.model <- coef(lm(sorted.sample ~ theoretical.quantile));
    a <- coef.linear.model[1];
    b <- coef.linear.model[2];
  }


  # the reference line
  fit.value <- a + b * theoretical.quantile;

  # create some vectors to store the returned values
  upper.pw <- NULL;
  lower.pw <- NULL;
  upper.sim <- NULL;
  lower.sim <- NULL;
  u <- NULL;	# a vector of logical value of whether the probabilities are in the interval [0,1] for the upper band
  l <- NULL;	# a vector of logical value of whether the probabilities are in the interval [0,1] for the lower band

  ### pointwise method
  if (conf.method == "both" | conf.method == "pointwise") {

    # create the numeric derivative of the theoretical quantile distribution
    numeric.derivative <- function(p) {
      # set the change in independent variable
      h <- 10^(-6);
      if (h * length(sorted.sample) > 1) { h <- 1 / (length(sorted.sample) + 1); }
      # the function
      return((distribution(p + h/2) - distribution(p - h/2)) / h);
    }

    # the standard errors of pointwise method
    data.standard.error <- b * numeric.derivative(probabilities) * qnorm(1 - (1 - conf)/2) * sqrt(probabilities * (1 - probabilities) / length(sorted.sample));

    # confidence interval of pointwise method
    upper.pw <- fit.value + data.standard.error;
    lower.pw <- fit.value - data.standard.error;
  }

  ### simultaneous method
  if (conf.method == "both" | conf.method == "simultaneous") {

    # get the threshold value for the statistics---the absolute difference of the empirical cdf and the theoretical cdf
    # Note that this statistics should follow a kolmogorov distribution when the sample size is large

    # the critical value from the Kolmogorov-Smirnov Test
    critical.value <- ks.test.critical.value(length(sorted.sample), conf);

    # under the null hypothesis, get the CI for the probabilities
    # the probabilities of the fitted value under the empirical cdf
    expected.prob <- ecdf(sorted.sample)(fit.value);

    # the probability should be in the interval [0, 1]
    u <- (expected.prob + critical.value) >= 0 & (expected.prob + critical.value) <= 1;
    l <- (expected.prob - critical.value) >= 0 & (expected.prob - critical.value) <= 1;

    # get the corresponding quantiles from the theoretical distribution
    z.upper <- distribution((expected.prob + critical.value)[u]);
    z.lower <- distribution((expected.prob - critical.value)[l]);

    # confidence interval of simultaneous method
    upper.sim <- a + b * z.upper;
    lower.sim <- a + b * z.lower;
  }


  # return the values for constructing the Confidence Bands of one sample QQ plot
  # the list to store the returned values
  returned.values <- list(
    a = a,
    b = b,
    z = theoretical.quantile,
    upper.pw = upper.pw,
    lower.pw = lower.pw,
    u = u,
    l = l,
    upper.sim = upper.sim,
    lower.sim = lower.sim
  );
  return (returned.values);
}


