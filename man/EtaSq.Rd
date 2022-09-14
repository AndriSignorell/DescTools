\name{EtaSq}
\alias{EtaSq}
\alias{EtaSq.lm}
\alias{EtaSq.aovlist}

\alias{aovlDetails}
\alias{aovlErrorTerms}

\title{Effect Size Calculations for ANOVAs}

\description{
Calculates eta-squared, partial eta-squared and generalized eta-squared
}

\usage{
EtaSq(x, type = 2, anova = FALSE)

\method{EtaSq}{lm}(x, type = 2, anova = FALSE)

\method{EtaSq}{aovlist}(x, type = 2, anova = FALSE)
}

\arguments{
  \item{x}{An analysis of variance (\code{aov}, \code{aovlist}) object.}
  \item{type}{What type of sum of squares to calculate? \code{EtaSq.aovlist} requires \code{type=1}.}
  \item{anova}{Should the full ANOVA table be printed out in addition to the effect sizes?}
}

\details{
Calculates the eta-squared, partial eta-squared, and generalized eta-squared measures of effect size that are commonly used in analysis of variance. The input \code{x} should be the analysis of variance object itself. For between-subjects designs, generalized eta-squared equals partial eta-squared. The reported generalized eta-squared for repeated-measures designs assumes that all factors are manipulated, i.e., that there are no measured factors like gender (see references).

For unbalanced designs, the default in \code{EtaSq} is to compute Type II sums of squares (\code{type=2}), in keeping with the \code{Anova} function in the \code{car} package. It is possible to revert to the Type I SS values (\code{type=1}) to be consistent with \code{anova}, but this rarely tests hypotheses of interest. Type III SS values (\code{type=3}) can also be computed. \code{EtaSq.aovlist} requires \code{type=1}.
}

\value{
If \code{anova=FALSE}, the output for \code{EtaSq.lm} is an M x 2 matrix, for \code{EtaSq.aovlist} it is an M x 3 matrix. Each of the M rows corresponds to one of the terms in the ANOVA (e.g., main effect 1, main effect 2, interaction, etc), and each of the columns corresponds to a different measure of effect size. Column 1 contains the eta-squared values, and column 2 contains partial eta-squared values. Column 3 contains the generalized eta-squared values. If \code{anova=TRUE}, the output contains additional columns containing the sums of squares, mean squares, degrees of freedom, F-statistics and p-values. For \code{EtaSq.aovlist}, additional columns contain the error sum of squares and error degrees of freedom corresponding to an effect term.
}

\references{
Bakeman, R. (2005). Recommended effect size statistics for repeated measures designs. Behavior Research Methods 37(3), 379-384.

Olejnik, S. and Algina, J. (2003). Generalized Eta and Omega Squared Statistics: Measures of Effect Size for Some Common Research Designs. Psychological Methods 8(4), 434-447.
}

\author{Danielle Navarro <djnavarro@protonmail.com>, Daniel Wollschlaeger <dwoll@psychologie.uni-kiel.de>}

\seealso{
\code{\link{aov}},	\code{\link{anova}}, \code{\link[car]{Anova}}
}
\examples{
#### Example 1: one-way ANOVA ####

outcome <- c(1.4,2.1,3.0,2.1,3.2,4.7,3.5,4.5,5.4)    # data
treatment1 <- factor(c(1,1,1,2,2,2,3,3,3))           # grouping variable
anova1 <- aov(outcome ~ treatment1)                  # run the ANOVA
summary(anova1)                                      # print the ANOVA table
EtaSq(anova1)                                        # effect size

#### Example 2: two-way ANOVA ####

treatment2 <- factor(c(1,2,3,1,2,3,1,2,3))       # second grouping variable
anova2 <- aov(outcome ~ treatment1 + treatment2) # run the ANOVA
summary(anova2)                                  # print the ANOVA table
EtaSq(anova2)                                    # effect size

#### Example 3: two-way ANOVA unbalanced cell sizes ####
#### data from Maxwell & Delaney, 2004              ####
#### Designing experiments and analyzing data       ####

dfMD <- data.frame(IV1=factor(rep(1:3, c(3+5+7, 5+6+4, 5+4+6))),
                   IV2=factor(rep(rep(1:3, 3), c(3,5,7, 5,6,4, 5,4,6))),
                   DV=c(c(41, 43, 50), c(51, 43, 53, 54, 46), c(45, 55, 56, 60, 58, 62, 62),
                        c(56, 47, 45, 46, 49), c(58, 54, 49, 61, 52, 62), c(59, 55, 68, 63),
                        c(43, 56, 48, 46, 47), c(59, 46, 58, 54), c(55, 69, 63, 56, 62, 67)))

# use contr.sum for correct sum of squares type 3
dfMD$IV1s <- C(dfMD$IV1, "contr.sum")
dfMD$IV2s <- C(dfMD$IV2, "contr.sum")
dfMD$IV1t <- C(dfMD$IV1, "contr.treatment")
dfMD$IV2t <- C(dfMD$IV2, "contr.treatment")

EtaSq(aov(DV ~ IV1s*IV2s, data=dfMD), type=3)
EtaSq(aov(DV ~ IV1t*IV2t, data=dfMD), type=1)

#### Example 4: two-way split-plot ANOVA -> EtaSq.aovlist ####

DV_t1 <- round(rnorm(3*10, -0.5, 1), 2)
DV_t2 <- round(rnorm(3*10,  0,   1), 2)
DV_t3 <- round(rnorm(3*10,  0.5, 1), 2)
dfSPF <- data.frame(id=factor(rep(1:(3*10), times=3)),
                    IVbtw=factor(rep(LETTERS[1:3], times=3*10)),
					IVwth=factor(rep(1:3, each=3*10)),
					DV=c(DV_t1, DV_t2, DV_t3))
spf <- aov(DV ~ IVbtw*IVwth + Error(id/IVwth), data=dfSPF)
EtaSq(spf, type=1, anova=TRUE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
