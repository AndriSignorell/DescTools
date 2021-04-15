\name{TMod}
\alias{TMod}
\alias{ModSummary}
\alias{ModSummary.lm}
\alias{ModSummary.glm}
\alias{plot.TMod}
\alias{print.TMod}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Comparison Table For Linear Models
%%  ~~function to do ... ~~
}
\description{Collect the coefficients and some qualifying statistics of linear models and organize it in a table for comparison and reporting. The function supports linear and general linear models.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
TMod(..., FUN = NULL)

ModSummary(x, ...)

\method{ModSummary}{lm}(x, conf.level = 0.95, ...)
\method{ModSummary}{glm}(x, conf.level = 0.95, use.profile = TRUE, ...)

\method{plot}{TMod}(x, terms = NULL, intercept = FALSE, ...)
\method{print}{TMod}(x, digits = 3, na.form = "-", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{a (general) linear model object.}
\item{\dots}{a list of (general) linear models.}
\item{conf.level}{the level for the confidence intervals.}
\item{FUN}{function with arguments \code{est}, \code{se}, \code{tval}, \code{pval}, \code{lci}, \code{uci} to display the coefficients. The default function will display the coefficient and significance stars for the p-values.}
\item{terms}{a vector with the terms of the model formula to be plotted. By default this will be all of them.}
\item{use.profile}{logical. Defines if profile approach should be used, which normally is a good choice for small datasets. Calculating profile can however take ages for large datasets and not be necessary there. So we can fallback to normal confidence intervals. }

\item{intercept}{logical, defining whether the intercept should be plotted (default is \code{FALSE}).}
  \item{digits}{integer, the desired (fixed) number of digits after the decimal point. Unlike \code{\link{formatC}} you will always get this number of digits even if the last digit is 0.
%%     ~~Describe \code{digits} here~~
}

  \item{na.form}{character, string specifying how \code{NA}s should be specially formatted.
  If set to \code{NULL} (default) no special action will be taken.
%%     ~~Describe \code{pad} here~~
}
}
\details{In order to compare the coefficients of linear models, the user is left to his own devices. R offers no support in this respect. \code{TMod()} jumps into the breach and displays the coefficients of several models in tabular form. For this purpose, different quality indicators for the models are displayed, so that a comprehensive comparison of the models is possible. In particular, it is easy to see the effect that adding or omitting variables has on forecast quality.

A plot function for a \code{TMod} object will produce a dotchart with the coefficients and their confidence intervals.
}
\value{character table
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{help}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
r.full <- lm(Fertility ~ . , swiss)
r.nox <- lm(Fertility ~ . -Examination - Catholic, swiss)
r.grp <- lm(Fertility ~ . -Education - Catholic + CutQ(Catholic), swiss)
r.gam <- glm(Fertility ~ . , swiss, family=Gamma(link="identity"))
r.gama <- glm(Fertility ~ .- Agriculture , swiss, family=Gamma(link="identity"))
r.gaml <- glm(Fertility ~ . , swiss, family=Gamma(link="log"))

TMod(r.full, r.nox, r.grp, r.gam, r.gama, r.gaml)

# display confidence intervals
TMod(r.full, r.nox, r.gam, FUN = function(est, se, tval, pval, lci, uci){
  gettextf("\%s [\%s, \%s]",
           Format(est, fmt=Fmt("num")),
           Format(lci, digits=3),
           Format(uci, digits=2)
           )
})


# cbind interface is not supported!!
# d.titanic <- reshape(as.data.frame(Titanic),
#                       idvar = c("Class","Sex","Age"),
#                       timevar="Survived",
#                       direction = "wide")
#
# r.glm0 <- glm(cbind(Freq.Yes, Freq.No) ~ 1, data=d.titanic, family="binomial")
# r.glm1 <- glm(cbind(Freq.Yes, Freq.No) ~ Class, data=d.titanic, family="binomial")
# r.glm2 <- glm(cbind(Freq.Yes, Freq.No) ~ ., data=d.titanic, family="binomial")

d.titanic <- Untable(Titanic)

r.glm0 <- glm(Survived ~ 1, data=d.titanic, family="binomial")
r.glm1 <- glm(Survived ~ Class, data=d.titanic, family="binomial")
r.glm2 <- glm(Survived ~ ., data=d.titanic, family="binomial")

TMod(r.glm0, r.glm1, r.glm2)

# plot OddsRatios
d.pima <- MASS::Pima.tr2

r.a <- glm(type ~ npreg + bp + skin + bmi + ped + age, data=d.pima, family=binomial)
r.b <- glm(type ~ npreg + glu + bp + skin, data=d.pima, family=binomial)
r.c <- glm(type ~ npreg + age, data=d.pima, family=binomial)

or.a <- OddsRatio(r.a)
or.b <- OddsRatio(r.b)
or.c <- OddsRatio(r.c)


# create the model table
tm <- TMod(m_A=or.a, m_B=or.b, m_C=or.c)
# .. and plotit
plot(tm, main="ORs for Models A, B, C", intercept=FALSE,
     pch=15, col=c(hred, hblue, horange), 
     panel.first=abline(v=1, col="grey30"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ mod }% use one of  RShowDoc("KEYWORDS")
