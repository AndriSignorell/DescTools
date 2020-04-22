
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version-last-release/DescTools)](https://CRAN.R-project.org/package=DescTools)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/DescTools)](https://CRAN.R-project.org/package=DescTools)
[![downloads](http://cranlogs.r-pkg.org/badges/last-week/DescTools)](https://CRAN.R-project.org/package=DescTools)
[![License: GPL
v2+](https://img.shields.io/badge/License-GPL%20v2+-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Documentation](https://img.shields.io/badge/Updated%20on-2020--04--22-yellowgreen.svg)](/commits/master)
[![R build
status](https://github.com/AndriSignorell/DescTools/workflows/R-CMD-check/badge.svg)](https://github.com/AndriSignorell/DescTools/actions)
<!-- badges: end -->

# Tools for Descriptive Statistics and Exploratory Data Analysis

**DescTools** is an extensive collection of miscellaneous basic
statistics functions and comfort wrappers not available in the R basic
system for efficient description of data. The author’s intention was to
create a toolbox, which facilitates the (notoriously time consuming)
first descriptive tasks in data analysis, consisting of calculating
descriptive statistics, drawing graphical summaries and reporting the
results. The package contains furthermore functions to produce documents
using MS Word (or PowerPoint) and functions to import data from Excel. 

A considerable part of the included functions can be found scattered in
other packages and other sources written partly by Titans of R. The
reason for collecting them here, was primarily to have them consolidated
in ONE instead of dozens of packages (which themselves might depend on
other packages, which are not needed at all), and to provide a common
and consistent interface as far as function and arguments naming, `NA`
handling, recycling rules etc. are concerned. Google style guides were
used as naming rules (in absence of convincing alternatives). The
‘CamelStyle’ was consequently applied to functions borrowed from
contributed R packages as well. 

Feedback, feature requests, bugreports and other suggestions are
welcome\! Please report problems to Stack Overflow mentioning DescTools
or directly to the maintainer.

## Installation

You can install the released version of DescTools from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("DescTools")
```

And the development version from [GitHub](https://github.com/) with:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("AndriSignorell/DescTools")
```

<!--
# A list of the functions in the package

A grouped list of the functions in the package.


## Operators, calculus, transformations

------------------------------------ --------------------------------------------------------------------------------------
[%()%]                               \tab Between operators determine if a value lies within a range[a,b] 

[%)(%]                               \tab Outside operators: \%)(\%, \%](\%, \%)[\%, \%][\% 

[%nin%]                              \tab "not in" operator 

[%overlaps%]                         \tab Do two collections have common elements? 

[%like%],[%like any%]                \tab Simple operator to search for a specified pattern 

[%^%]                                \tab Powers of matrices 

[Interval]                           \tab The number of days of the overlapping part 
                                     \tab of two date periods 

[AUC]                                \tab Area under the curve 

[Primes]                             \tab Find all primes less than n 

[Factorize]                          \tab Prime factorization of integers 

[Divisors]                           \tab All divisors of an integer 

[GCD]                                \tab Greatest common divisor 

[LCM]                                \tab Least common multiple 

[Permn]                              \tab Determine all possible permutations of a set 

[Fibonacci]                          \tab Generates single Fibonacci numbers or a Fibonacci sequence 

[DigitSum]                           \tab Digit sum of a number 

[Frac]                               \tab Return the fractional part of a numeric value 

[Ndec]                               \tab Count decimal places of a number 

[MaxDigits]                          \tab Maximum used digits for a vector of numbers 

[Prec]                               \tab Precision of a number 

[BoxCox],[BoxCoxInv]                 \tab Box Cox transformation and its inverse transformation 

[BoxCoxLambda]                       \tab Return the optimal lambda for a BoxCox transformation 

[LogSt],[LogStInv]                   \tab Calculate started logarithmic transformation and it's inverse 

[Logit],[LogitInv]                   \tab Generalized logit and inverse logit function 

[LinScale]                           \tab Simple linear scaling of a vector x 

[Winsorize]                          \tab Data cleaning by winsorization 

[Trim]                               \tab Trim data by omitting outlying observations 

[CutQ]                               \tab Cut a numeric variable into quartiles or other quantiles 

[Recode]                             \tab Recode a factor with altered levels 

[Rename]                             \tab Change name(s) of a named object 

[Sort]                               \tab Sort extension for matrices and data.frames 

[SortMixed],[OrderMixed]             \tab Mixed sort order 

[DenseRank]                          \tab Calculate ranks in consecutive order (no ties) 

[PercentRank]                        \tab Calculate the percent rank 

[RoundTo]                            \tab Round to a multiple 

[Large],[Small]                      \tab Returns the kth largest, resp. smallest values 

[HighLow]                            \tab Combines `Large` and `Small`. 

[Rev]                                \tab Reverses the order of rows and/or columns of a matrix or a data.frame 

[Untable]                            \tab Recreates original list based on a n-dimensional frequency table 

[CollapseTable]                      \tab Collapse some rows/columns in a table. 

[Dummy]                              \tab Generate dummy codes for a factor 

[FisherZ],[FisherZInv]               \tab Fisher's z-transformation and its inverse 

[Midx]                               \tab Calculate sequentially the midpoints of the elements of a vector 

[Unwhich]                            \tab Inverse function to[which()], create a logical vector/matrix from indices 

[Vigenere]                           \tab Implements a Vigenere cypher, both encryption and decryption 

[BinTree],[PlotBinTree]              \tab Create and plot a binary tree structure with a given length 
------------------------------------ --------------------------------------------------------------------------------------
  

## Information and manipulation functions

------------------------------------ --------------------------------------------------------------------------------------
[AllDuplicated]                      \tab Find all values involved in ties 

[Closest]                            \tab Return the value in a vector being closest to a given one 

[Coalesce]                           \tab Return the first value in a vector not being `NA` 

[ZeroIfNA],[NAIfZero]                \tab Replace NAs by 0, resp. vice versa 

[Impute]                             \tab Replace NAs by the median or another value 

[LOCF]                               \tab Imputation of datapoints following the "last observation 
                                     \tab carried forward" rule 

[CombN]                              \tab Returns the number of subsets out of a list of elements 

[CombSet]                            \tab Generates all possible subsets out of a list of elements 

[CombPairs]                          \tab Generates all pairs out of one or two sets of elements 

[SampleTwins]                        \tab Create sample using stratifying groups 

[RndPairs]                           \tab Create pairs of correlated random numbers 

[RndWord]                            \tab Produce random combinations of characters 

[IsNumeric]                          \tab Check a vector for being numeric, zero Or a whole number 

[IsWhole]                            \tab Is x a whole number? 

[IsDichotomous]                      \tab Check if x contains exactly 2 values 

[IsOdd]                              \tab Is x even or odd? 

[IsPrime]                            \tab Is x a prime number? 

[IsZero]                             \tab Is numeric(x) == 0, say x < machine.eps? 

[IsEuclid]                           \tab Check if a distance matrix is euclidean 

[Label],[Unit]                       \tab Get or set the `label`, resp. `unit`, attribute of an object 

[Abind]                              \tab Bind matrices to n-dimensional arrays 

[Append]                             \tab Append elements to several classes of objects 

[VecRot],[VecShift]                  \tab Shift the elements of a vector in a circular mode to the right 
                                     \tab or to the left by n characters.  

[Clockwise]                          \tab Transform angles from counter clock into clockwise mode 

[split.formula]                      \tab A formula interface for the base function split 

[reorder.factor]                     \tab Reorder the levels of a factor 

[ToLong],[ToWide]                    \tab Simple reshaping of a vector 

[SetNames]                           \tab Set the names, rownames or columnnames in an object and return it 

[Some]                               \tab Return some randomly chosen elements of an object 

[SplitAt]                            \tab Split a vector into several pieces at given positions 

[SplitPath]                          \tab Split a path string in drive, path, filename 

[Str]                                \tab Compactly display the structure of any R object 

[TextToTable]                        \tab Converts a string to a table 
------------------------------------ --------------------------------------------------------------------------------------
  

## String functions

------------------------------------ --------------------------------------------------------------------------------------
[StrCountW]                          \tab Count the words in a string 

[StrTrim]                            \tab Delete white spaces from a string 

[StrTrunc]                           \tab Truncate string on a given length and add ellipses if it really 
                                     \tab was truncated 

[StrLeft],[StrRight]                 \tab Returns the left/right part or the a string. 

[StrAlign]                           \tab Align strings to the left/right/center or to a given character 

[StrAbbr]                            \tab Abbreviates a string 

[StrCap]                             \tab Capitalize the first letter of a string 

[StrPad]                             \tab Fill a string with defined characters to fit a given length 

[StrRev]                             \tab Reverse a string 

[StrChop]                            \tab Split a string by a fixed number of characters. 

[StrExtract]                         \tab Extract a part of a string, defined as regular expression. 

[StrVal]                             \tab Extract numeric values from a string 

[StrIsNumeric]                       \tab Check whether a string does only contain numeric data 

[StrPos]                             \tab Find position of first occurrence of a string in another one 

[StrDist]                            \tab Compute Levenshtein or Hamming distance between strings 

[FixToTable]                         \tab Create table out of a running text, by using columns of spaces as delimiter 
------------------------------------ --------------------------------------------------------------------------------------
  

## Conversion functions

------------------------------------ --------------------------------------------------------------------------------------
[AscToChar],[CharToAsc]              \tab Converts ASCII codes to characters and vice versa 

[DecToBin],[BinToDec]                \tab Converts numbers from binmode to decimal and vice versa 

[DecToHex],[HexToDec]                \tab Converts numbers from hexmode to decimal and vice versa 

[DecToOct],[OctToDec]                \tab Converts numbers from octmode to decimal and vice versa 

[DegToRad],[RadToDeg]                \tab Convert degrees to radians and vice versa 

[CartToPol],[PolToCart]              \tab Transform cartesian to polar coordinates and vice versa 

[CartToSph],[SphToCart]              \tab Transform cartesian to spherical coordinates and vice versa 

[RomanToInt]                         \tab Convert roman numerals to integers 

[RgbToLong],[LongToRgb]              \tab Convert a rgb color to a long number and vice versa 

[ColToGray],[ColToGrey]              \tab Convert colors to gcrey/grayscale 

[ColToHex],[HexToCol]                \tab Convert a color into hex string 

[HexToRgb]                           \tab Convert a hexnumber to an RGB-color 

[ColToHsv]                           \tab R color to HSV conversion 

[ColToRgb],[RgbToCol]                \tab Color to RGB conversion and back 

[ConvUnit]                           \tab Return the most common unit conversions 

------------------------------------ --------------------------------------------------------------------------------------
  

## Colors

------------------------------------ --------------------------------------------------------------------------------------
[SetAlpha]                           \tab Add transperancy (alpha channel) to a color.  

[ColorLegend]                        \tab Add a color legend to a plot 

[FindColor]                          \tab Get color on a defined color range 

[MixColor]                           \tab Get the mix of two colors 

[TextContrastColor]                  \tab Choose textcolor depending on background color 

[Pal]                                \tab Some custom color palettes 

------------------------------------ --------------------------------------------------------------------------------------
  

## Plots (low level)

------------------------------------ --------------------------------------------------------------------------------------
[Canvas]                             \tab Canvas for geometric plotting 

[Mar]                                \tab Set margins more comfortably.  

[Asp]                                \tab Return aspect ratio of the current plot 

[LineToUser]                         \tab Convert line coordinates to user coordinates 

[lines.loess]                        \tab Add a loess smoother and its CIs to an existing plot 

[lines.lm]                           \tab Add the prediction of linear model and its CIs to a plot 

[lines.smooth.spline]                \tab Add the prediction of a smooth.spline and its CIs to a plot 

[BubbleLegend]                       \tab Add a legend for bubbles to a bubble plot 

[TitleRect]                          \tab Add a main title to a plot surrounded by a rectangular box 

[BarText]                            \tab Add the value labels to a barplot 

[ErrBars]                            \tab Add horizontal or vertical error bars to an existing plot 

[DrawArc],[DrawRegPolygon]           \tab Draw elliptic, circular arc(s) or regular polygon(s) 

[DrawCircle],[DrawEllipse]           \tab Draw a circle, a circle annulus or a sector or an annulus 

[DrawBezier]                         \tab Draw a Bezier curve 

[DrawBand]                           \tab Draw confidence band 

[BoxedText]                          \tab Add text surrounded by a box to a plot 

[Rotate]                             \tab Rotate a geometric structure 

[SpreadOut]                          \tab Spread out a vector of numbers so that there is a minimum 
                                     \tab interval between any two elements. This can be used 
                                     \tab to place textlabels in a plot so that they do not overlap. 

[IdentifyA]                          \tab Helps identifying all the points in a specific area.  

[identify.formula]                   \tab Formula interface for[identify()].  

[PtInPoly]                           \tab Identify all the points within a polygon. 

[ConnLines]                          \tab Calculate and insert connecting lines in a barplot 

[AxisBreak]                          \tab Place a break mark on an axis 

[Shade]                              \tab Produce a shaded curve 

[Stamp]                              \tab Stamp the current plot with Date/Time/Directory or any other expression 
------------------------------------ --------------------------------------------------------------------------------------
  

## Plots (high level) 

------------------------------------ --------------------------------------------------------------------------------------
[PlotACF],[PlotGACF]                 \tab Create a combined plot of a time series including its 
                                     \tab autocorrelation and partial autocorrelation 

[PlotMonth]                          \tab Plot seasonal effects of a univariate time series 

[PlotArea]                           \tab Create an area plot 

[PlotBag]                            \tab Create a two-dimensional boxplot 

[PlotBagPairs]                       \tab Produce pairwise 2-dimensional boxplots (bagplot) 

[PlotBubble]                         \tab Draw a bubble plot 

[PlotCandlestick]                    \tab Plot candlestick chart 

[PlotCirc]                           \tab Create a circular plot 

[PlotCorr]                           \tab Plot a correlation matrix 

[PlotDot]                            \tab Plot a dotchart with confidence intervals 

[PlotFaces]                          \tab Produce a plot of Chernoff faces 

[PlotFdist]                          \tab Frequency distribution plot, combination of histogram, 
                                     \tab boxplot and ecdf.plot 

[PlotMarDens]                        \tab Scatterplot with marginal densities 

[PlotMultiDens]                      \tab Plot multiple density curves 

[PlotPolar]                          \tab Plot values on a circular grid 

[PlotFun]                            \tab Plot mathematical expression or a function 

[PolarGrid]                          \tab Plot a grid in polar coordinates 

[PlotPyramid]                        \tab Pyramid plot (back-back histogram) 

[PlotTreemap]                        \tab Plot of a treemap. 

[PlotVenn]                           \tab Plot a Venn diagram 

[PlotViolin]                         \tab Plot violins instead of boxplots 

[PlotQQ]                             \tab QQ-plot for an optional distribution 

[PlotWeb]                            \tab Create a web plot 

[PlotTernary]                        \tab Create a triangle or ternary plot 

[PlotMiss]                           \tab Plot missing values 

[PlotDev]                            \tab Simple convenience wrapper for producing TIF-Files 

[PlotECDF]                           \tab Plot empirical cumulative distribution function 

[PlotLinesA]                         \tab Plot the columns of one matrix against the columns of another 

[PlotLog]                            \tab Create a plot with logarithmic axis and log grid 

[PlotMosaic]                         \tab Plots a mosaic describing a contingency table in array form 

------------------------------------ --------------------------------------------------------------------------------------
  

## Distributions

------------------------------------ --------------------------------------------------------------------------------------
 _Benf                               \tab Benford distribution, including[qBenf],[dBenf],[rBenf] 

 _ExtrVal                            \tab Extreme value distribution ([dExtrVal]) 

 _Frechet                            \tab Frechet distribution ([dFrechet]) 

 _GenExtrVal                         \tab Generalized Extreme Value Distribution ([dGenExtrVal]) 

 _GenPareto                          \tab Generalized Pareto Distribution ([dGenPareto]) 

 _Gompertz                           \tab Gompertz distribution ([dGompertz]) 

 _Gumbel                             \tab Gumbel distribution ([dGumbel]) 

 _NegWeibull                         \tab Negative Weibull distribution ([dNegWeibull]) 

 _Order                              \tab Distributions of Order Statistics ([dOrder]) 

 _RevGumbel                          \tab Reverse Gumbel distribution ([dRevGumbel]), 

 _RevGumbelExp                       \tab Expontial reverse Gumbel distribution (quantile only) 

 _RevWeibull                         \tab Reverse Weibull distribution ([dRevWeibull]) 
------------------------------------ --------------------------------------------------------------------------------------
  

## Statistics

------------------------------------ --------------------------------------------------------------------------------------
[Freq]                               \tab Univariate frequency table 

[PercTable]                          \tab Bivariate percentage table 

[Margins]                            \tab (Extended) margin tables of a table 

[ExpFreq]                            \tab Expected frequencies of a n-dimensional table 

[Mode]                               \tab Mode, the most frequent value 

[Gmean],[Gsd]                        \tab Geometric mean and geometric standard deviation 

[Hmean]                              \tab Harmonic Mean 

[Median]                             \tab Extended median function supporting weights and ordered factors 

[HuberM],[TukeyBiweight]             \tab Huber M-estimator of location and Tukey's biweight robust mean 

[HodgesLehmann]                      \tab the Hodges-Lehmann estimator 

[HoeffD]                             \tab Hoeffding's D statistic 

[MeanSE]                             \tab Standard error of mean 

[MeanCI],[MedianCI]                  \tab Confidence interval for the mean and median 

[MeanDiffCI]                         \tab Confidence interval for the difference of two means 

[MoveAvg]                            \tab Moving average 

[MeanAD]                             \tab Mean absolute deviation 

[VarCI]                              \tab Confidence interval for the variance 

[CoefVar]                            \tab Coefficient of variation and its confidence interval 

[RobScale]                           \tab Robust data standardization 

[Range]                              \tab (Robust) range 

[BinomCI],[MultinomCI]               \tab Confidence intervals for binomial and multinomial proportions 

[BinomDiffCI]                        \tab Calculate confidence interval for a risk difference 

[BinomRatioCI]                       \tab Calculate confidence interval for the ratio of binomial proportions.  

[PoissonCI]                          \tab Confidence interval for a Poisson lambda 

[Skew],[Kurt]                        \tab Skewness and kurtosis 

[YuleQ],[YuleY]                      \tab Yule's Q and Yule's Y 

[TschuprowT]                         \tab Tschuprow's T 

[Phi],[ContCoef],[CramerV]           \tab Phi, Pearson's Contingency Coefficient and Cramer's V 

[GoodmanKruskalGamma]                \tab Goodman Kruskal's gamma 

[KendallTauA]                        \tab Kendall's tau-a 

[KendallTauB]                        \tab Kendall's tau-b 

[StuartTauC]                         \tab Stuart's tau-c 

[SomersDelta]                        \tab Somers' delta 

[Lambda]                             \tab Goodman Kruskal's lambda 

[GoodmanKruskalTau]                  \tab Goodman Kruskal's tau 

[UncertCoef]                         \tab Uncertainty coefficient 

[Entropy],[MutInf]                   \tab Shannon's entropy, mutual information 

[DivCoef],[DivCoefMax]               \tab Rao's diversity coefficient ("quadratic entropy") 

[TheilU]                             \tab Theil's U1 and U2 coefficient 

[Assocs]                             \tab Combines the association measures above.  

[OddsRatio],[RelRisk]                \tab Odds ratio and relative risk 

[ORToRelRisk]                        \tab Transform odds ratio to relative risk 

[CohenKappa],[KappaM]                \tab Cohen's Kappa, weighted Kappa and Kappa for 
                                     \tab more than 2 raters 

[CronbachAlpha]                      \tab Cronbach's alpha 

[ICC]                                \tab Intraclass correlations 

[KrippAlpha]                         \tab Return Kripp's alpha coefficient 

[KendallW]                           \tab Compute the Kendall coefficient of concordance 

[Lc]                                 \tab Calculate and plot Lorenz curve 

[Gini],[Atkinson]                    \tab Gini- and Atkinson coefficient 

[Herfindahl],[Rosenbluth]            \tab Herfindahl- and Rosenbluth coefficient 

[GiniSimpson]                        \tab Compute Gini-Simpson Coefficient 

[CorCI]                              \tab Confidence interval for Pearson's correlation coefficient 

[CorPart]                            \tab Find the correlations for a set x of variables with set y removed 

[CorPolychor]                        \tab Polychoric correlation coefficient 

[SpearmanRho]                        \tab Spearman rank correlation and its confidence intervals 

[ConDisPairs]                        \tab Return concordant and discordant pairs of two vectors 

[FindCorr]                           \tab Determine highly correlated variables 

[CohenD]                             \tab Cohen's Effect Size 

[EtaSq]                              \tab Effect size calculations for ANOVAs 

[Contrasts]                          \tab Generate pairwise contrasts for using in a post-hoc test 

[Strata]                             \tab Stratified sampling with equal/unequal probabilities 

[Outlier]                            \tab Outliers following Tukey's boxplot definition 

[LOF]                                \tab Local outlier factor 

[BrierScore]                         \tab Brier score, assessing the quality of predictions of binary events 

[Cstat]                              \tab C statistic, equivalent to the area under the ROC curve) 

[CCC]                                \tab Lin's concordance correlation coef for agreement on a continuous measure 

[MAE]                                \tab Mean absolute error 

[MAPE],[SMAPE]                       \tab Mean absolute and symmetric mean absolute percentage error 

[MSE],[RMSE]                         \tab Mean squared error and root mean squared error 

[NMAE],[NMSE]                        \tab Normalized mean absolute and mean squared error 

[Conf]                               \tab Confusion matrix, a cross-tabulation of observed and predicted classes 
                                     \tab with associated statistics 

[Sens],[Spec]                        \tab Sensitivity and specificity 

[PseudoR2]                           \tab Variants of pseudo R squared statistics: McFadden, Aldrich-Nelson, 
                                     \tab Nagelkerke, CoxSnell, Effron, McKelvey-Zavoina, Tjur 

[Mean],[SD],[Var]                    \tab Variants of base statistics, allowing to define weights: Mean, 

[Quantile],[MAD],[Cor]               \tab standard deviation, variance, quantile, mad, correlation 

[VIF],[StdCoef]                      \tab Variance inflation factors and standardised coefficents for linear models 
------------------------------------ --------------------------------------------------------------------------------------

## Tests
------------------------------------ --------------------------------------------------------------------------------------
[SignTest]                           \tab Signtest to test whether two groups are equally sized 

[ZTest]                              \tab Z--test for known population variance 

[TTestA]                             \tab Student's t-test based on sample statistics 

[JonckheereTerpstraTest]             \tab Jonckheere-Terpstra trend test for medians 

[PageTest]                           \tab Page test for ordered alternatives 

[CochranQTest]                       \tab Cochran's Q-test to find differences in matched sets 
                                     \tab of three or more frequencies or proportions. 

[VarTest]                            \tab ChiSquare test for one variance and F test for two variances 

[SiegelTukeyTest]                    \tab Siegel-Tukey test for equality in variability 

[SiegelTukeyRank]                    \tab Calculate Siegel-Tukey's ranks (auxiliary function) 

[LeveneTest]                         \tab Levene's test for homogeneity of variance 

[MosesTest]                          \tab Moses Test of extreme reactions 

[RunsTest]                           \tab Runs test for detecting non-randomness 

[DurbinWatsonTest]                   \tab Durbin-Watson test for autocorrelation 

[BartelsRankTest]                    \tab Bartels rank test for randomness 

[JarqueBeraTest]                     \tab Jarque-Bera Test for normality 

[AndersonDarlingTest]                \tab Anderson-Darling test for normality 

[CramerVonMisesTest]                 \tab Cramer-von Mises test for normality 

[LillieTest]                         \tab Lilliefors (Kolmogorov-Smirnov) test for normality 

[PearsonTest]                        \tab Pearson chi-square test for normality 

[ShapiroFranciaTest]                 \tab Shapiro-Francia test for normality 

[MHChisqTest]                        \tab Mantel-Haenszel Chisquare test 

[StuartMaxwellTest]                  \tab Stuart-Maxwell marginal homogeneity test 

[LehmacherTest]                      \tab Lehmacher marginal homogeneity test 

[CochranArmitageTest]                \tab Cochran-Armitage test for trend in binomial proportions 

[BreslowDayTest],[WoolfTest]         \tab Test for homogeneity on 2x2xk tables over strata 

[PostHocTest]                        \tab Post hoc tests by Scheffe, LSD, Tukey for a aov-object 

[ScheffeTest]                        \tab Multiple comparisons Scheffe test 

[DunnTest]                           \tab Dunn's test of multiple comparisons 

[DunnettTest]                        \tab Dunnett's test of multiple comparisons 

[ConoverTest]                        \tab Conover's test of multiple comparisons (following a kruskal test) 

[NemenyiTest]                        \tab Nemenyi's test of multiple comparisons 

[HotellingsT2Test]                   \tab Hotelling's T2 test for the one and two sample case 

[YuenTTest]                          \tab Yuen's robust t-Test with trimmed means and winsorized variances 

[BarnardTest]                        \tab Barnard's test for 2x2 tables 

[BreuschGodfreyTest]                 \tab Breusch-Godfrey test for higher-order serial correlation. 

[GTest]                              \tab Chi-squared contingency table test and goodness-of-fit test 

[HosmerLemeshowTest]                 \tab Hosmer-Lemeshow goodness of fit tests 

[VonNeumannTest]                     \tab Von Neumann's successive difference test 
------------------------------------ --------------------------------------------------------------------------------------
  

## Date functions
------------------------------------ --------------------------------------------------------------------------------------
[day.name],[day.abb]                 \tab Defined names of the days 

[AddMonths],[AddMonthsYM]            \tab Add a number of months to a given date 

[IsDate]                             \tab Check whether x is a date object 

[IsWeekend]                          \tab Check whether x falls on a weekend 

[IsLeapYear]                         \tab Check whether x is a leap year 

[LastDayOfMonth]                     \tab Return the last day of the month of the date x 

[DiffDays360]                        \tab Calculate the difference of two dates using the 360-days system 

[Date]                               \tab Create a date from numeric representation of year, month, day 

[Day],[Month],[Year]                 \tab Extract part of a date 

[Hour],[Minute],[Second]             \tab Extract part of time 

[Week],[Weekday]                     \tab Returns ISO week and weekday of a date 

[Quarter]                            \tab Quarter of a date 

[Timezone]                           \tab Timezone of a POSIXct/POSIXlt date 

[YearDay],[YearMonth]                \tab The day in the year of a date 

[Now],[Today]                        \tab Get current date or date-time 

[HmsToSec],[SecToHms]                \tab Convert `h:m:s` times to seconds and vice versa 

[Overlap]                            \tab Determine if and how extensively two date ranges overlap 

[Zodiac]                             \tab The zodiac sign of a date :-) 
------------------------------------ --------------------------------------------------------------------------------------
  

## Finance functions
------------------------------------ --------------------------------------------------------------------------------------
[OPR]                                \tab One period returns (simple and log returns) 

[NPV]                                \tab Net present value 

[NPVFixBond]                         \tab Net present value for fix bonds 

[IRR]                                \tab Internal rate of return 

[YTM]                                \tab Return yield to maturity for a bond 

[SLN],[DB],[SYD]                     \tab Several methods of depreciation of an asset 
------------------------------------ --------------------------------------------------------------------------------------
  

## GUI-Helpers
------------------------------------ --------------------------------------------------------------------------------------
[PasswordDlg]                        \tab Display a dialog containing an edit field, showing only ***. 
------------------------------------ --------------------------------------------------------------------------------------
  

## Reporting, InOut
------------------------------------ --------------------------------------------------------------------------------------
[CatTable]                           \tab Print a table with the option to have controlled linebreaks 

[Format],[Fmt]                       \tab Easy format for numbers and dates 

[Desc]                               \tab Produce a rich description of an object 

[Abstract]                           \tab Display compact overview of the structure of a data frame 

[TMod]                               \tab Create comparison table for (general) linear models 

[TOne]                               \tab Create "Table One"" describing baseline characteristics 

[GetNewWrd],[GetNewXL],[GetNewPP]    \tab Create a new Word, Excel or PowerPoint Instance 

[GetCurrWrd],[GetCurrXL],[GetCurrPP] \tab Get a handle to a running Word, Excel or PowerPoint instance 

[WrdKill],[XLKill]                   \tab Ends a (possibly hidden) Word/Excel process 

[IsValidWrd]                         \tab Check if the handle to a Word instance is valid or outdated 

[WrdCaption]                         \tab Insert a title in Word 

[WrdFont]                            \tab Get and set the font for the current selection in Word 

[WrdParagraphFormat]                 \tab Get and set the paragraph format 

[WrdTable]                           \tab Create a table in Word 

[WrdCellRange]                       \tab Select a cell range of a table in Word 

[WrdMergeCells]                      \tab Merge cells of a table in Word 

[WrdFormatCells]                     \tab Format selected cells of a table in word 

[WrdTableBorders]                    \tab Set or edit table border style of a table in Word 

[ToWrd],[ToXL]                       \tab Mord flexible wrapper to send diverse objects to Word, resp. Excel 

[WrdPlot]                            \tab Insert the active plot to Word 

[WrdInsertBookmark]                  \tab Insert a new bookmark in a Word document 

[WrdGoto]                            \tab Place cursor to a specific bookmark, or another text position. 

[WrdUpdateBookmark]                  \tab Update the text of a bookmark's range 

[WrdSaveAs]                          \tab Saves documents in Word 

[WrdStyle]                           \tab Get and set the style of a paragraph in Word 

[XLDateToPOSIXct]                    \tab Convert XL-Date format to POSIXct format 

[XLGetRange]                         \tab Get the values of one or several cell range(s) in Excel 

[XLGetWorkbook]                      \tab Get the values of all sheets of an Excel workbook 

[XLView]                             \tab Use Excel as viewer for a data.frame 

[PpPlot]                             \tab Insert active plot to PowerPoint 

[PpAddSlide]                         \tab Adds a slide to a PowerPoint presentation 

[PpText]                             \tab Adds a textbox with text to a PP-presentation 

[ParseSASDatalines]                  \tab Parse a SAS "datalines" statement to read data 
------------------------------------ --------------------------------------------------------------------------------------
  

## Tools
------------------------------------ --------------------------------------------------------------------------------------
[PairApply]                          \tab Helper for calculating functions pairwise 

[LsFct],[LsObj]                      \tab List the functions (or the data, all objects) of a package 

[FctArgs]                            \tab Retrieve the arguments of a functions 

[InDots]                             \tab Check if an argument is contained in ... argument and return it's value 

[ParseFormula]                       \tab Parse a formula and return the splitted parts of if 

[Recycle]                            \tab Recycle a list of elements to the maximal found dimension 

[Keywords]                           \tab Get the keywords of a man page 

[SysInfo]                            \tab Get some more information about system and environment 

[DescToolsOptions]                   \tab Get the DescTools specific options 

[PDFManual]                          \tab Get the pdf-manual of any package on CRAN and open it 
------------------------------------ --------------------------------------------------------------------------------------
  

## Data
------------------------------------ --------------------------------------------------------------------------------------
[d.pizza]                            \tab Synthetic dataset created for testing the description 

[d.whisky]                           \tab of Scotch Single Malts 
------------------------------------ --------------------------------------------------------------------------------------
  

## Reference Data
------------------------------------ --------------------------------------------------------------------------------------
[d.units],[d.prefix]                 \tab Unit conversion factors and metric prefixes 

[d.periodic]                         \tab Periodic table of elements 

[d.countries]                        \tab ISO 3166-1 country codes 

[roulette],[cards],[tarot]           \tab Datasets for probabilistic simulation 
------------------------------------ --------------------------------------------------------------------------------------

-->

# Warning

**Warning:** This package is still under development. Although the code
seems meanwhile quite stable, until release of version 1.0 (which is
expected in hmm: spring 2019? … you don’t believe it anymore?) you
should be aware that everything in the package might be subject to
change. Backward compatibility is not yet guaranteed. Functions may be
deleted or renamed and new syntax may be inconsistent with earlier
versions. By release of version 1.0 the “deprecated-defunct process”
will be installed.

# Authors

Andri Signorell  
Helsana Versicherungen AG, Health Sciences, Zurich  
HWZ University of Applied Sciences in Business Administration Zurich.

R is a community project. This can be seen from the fact that this
package includes R source code and/or documentation previously published
by: [the list of
authors/contibutors](https://github.com/AndriSignorell/DescTools)

Special thanks go to Beat Bruengger, Mathias Frueh, Daniel Wollschlaeger
for their valuable contributions and testing.

The good things come from all these guys, any problems are likely due to
my tweaking. Thank you all\!

**Maintainer:** Andri Signorell

<!-- @keywords package -->

# Examples

``` r
library(DescTools)
```

## Demo “describe”

``` r
demo(describe, package = "DescTools")
#> 
#> 
#>  demo(describe)
#>  ---- ~~~~~~~~
#> 
#> > ## -----------------------------------------------------------------------------------
#> > ## Demo file for DescTools; start with 'demo(DescTools)'
#> > ## -----------------------------------------------------------------------------------
#> > 
#> > 
#> > # Descriptions **************
#> > # use a subset of built-in data.frame
#> > d.sub <- d.pizza[,c("temperature","driver","delivery_min","count","operator","date","wine_ordered")]
#> 
#> > # all univariate descriptions
#> > Desc(d.sub, plotit=TRUE)
#> ------------------------------------------------------------------------------ 
#> Describe d.sub (data.frame):
#> 
#> data frame:  1209 obs. of  7 variables
#>      1116 complete cases (92.3%)
#> 
#>   Nr  ColName       Class    NAs        Levels                           
#>   1   temperature   numeric  39 (3.2%)                                   
#>   2   driver        factor    5 (0.4%)  (7): 1-Butcher, 2-Carpenter,     
#>                                         3-Carter, 4-Farmer, 5-Hunter, ...
#>   3   delivery_min  numeric   .                                          
#>   4   count         integer  12 (1.0%)                                   
#>   5   operator      factor    8 (0.7%)  (3): 1-Allanah, 2-Maria, 3-Rhonda
#>   6   date          Date     32 (2.6%)                                   
#>   7   wine_ordered  integer  12 (1.0%)                                   
#> 
#> 
#> ------------------------------------------------------------------------------ 
#> 1 - temperature (numeric)
#> 
#>   length       n     NAs  unique      0s    mean  meanCI'
#>    1'209   1'170      39     375       0  47.937  47.367
#>            96.8%    3.2%            0.0%          48.507
#>                                                         
#>      .05     .10     .25  median     .75     .90     .95
#>   26.700  33.290  42.225  50.000  55.300  58.800  60.500
#>                                                         
#>    range      sd   vcoef     mad     IQR    skew    kurt
#>   45.500   9.938   0.207   9.192  13.075  -0.842   0.051
#>                                                         
#> lowest : 19.3, 19.4, 20.0, 20.2 (2), 20.35
#> highest: 63.8, 64.1, 64.6, 64.7, 64.8
#> 
#> ' 95%-CI (classic)
```

<img src="man/figures/README-demo-describe-1.png" width="100%" />

    #> ------------------------------------------------------------------------------ 
    #> 2 - driver (factor)
    #> 
    #>   length      n    NAs unique levels  dupes
    #>    1'209  1'204      5      7      7      y
    #>           99.6%   0.4%                     
    #> 
    #>        level  freq   perc  cumfreq  cumperc
    #> 1  Carpenter   272  22.6%      272    22.6%
    #> 2     Carter   234  19.4%      506    42.0%
    #> 3     Taylor   204  16.9%      710    59.0%
    #> 4     Hunter   156  13.0%      866    71.9%
    #> 5     Miller   125  10.4%      991    82.3%
    #> 6     Farmer   117   9.7%    1'108    92.0%
    #> 7    Butcher    96   8.0%    1'204   100.0%

<img src="man/figures/README-demo-describe-2.png" width="100%" />

    #> ------------------------------------------------------------------------------ 
    #> 3 - delivery_min (numeric)
    #> 
    #>   length       n    NAs  unique     0s   mean  meanCI'
    #>    1'209   1'209      0     384      0  25.65   25.04
    #>           100.0%   0.0%           0.0%          26.26
    #>                                                      
    #>      .05     .10    .25  median    .75    .90     .95
    #>    10.40   11.60  17.40   24.40  32.50  40.42   45.20
    #>                                                      
    #>    range      sd  vcoef     mad    IQR   skew    kurt
    #>    56.80   10.84   0.42   11.27  15.10   0.61    0.10
    #>                                                      
    #> lowest : 8.8 (3), 8.9, 9.0 (3), 9.1 (5), 9.2 (3)
    #> highest: 61.9, 62.7, 62.9, 63.2, 65.6
    #> 
    #> ' 95%-CI (classic)

<img src="man/figures/README-demo-describe-3.png" width="100%" />

    #> ------------------------------------------------------------------------------ 
    #> 4 - count (integer)
    #> 
    #>   length      n    NAs  unique    0s  mean  meanCI'
    #>    1'209  1'197     12       8     0  3.44    3.36
    #>           99.0%   1.0%          0.0%          3.53
    #>                                                   
    #>      .05    .10    .25  median   .75   .90     .95
    #>     1.00   2.00   2.00    3.00  4.00  6.00    6.00
    #>                                                   
    #>    range     sd  vcoef     mad   IQR  skew    kurt
    #>     7.00   1.56   0.45    1.48  2.00  0.45   -0.36
    #>                                                   
    #> 
    #>    level  freq   perc  cumfreq  cumperc
    #> 1      1   108   9.0%      108     9.0%
    #> 2      2   259  21.6%      367    30.7%
    #> 3      3   300  25.1%      667    55.7%
    #> 4      4   240  20.1%      907    75.8%
    #> 5      5   152  12.7%    1'059    88.5%
    #> 6      6    97   8.1%    1'156    96.6%
    #> 7      7    34   2.8%    1'190    99.4%
    #> 8      8     7   0.6%    1'197   100.0%
    #> 
    #> heap(?): remarkable frequency (25.1%) for the mode(s) (= 3)
    #> 
    #> ' 95%-CI (classic)

<img src="man/figures/README-demo-describe-4.png" width="100%" />

    #> ------------------------------------------------------------------------------ 
    #> 5 - operator (factor)
    #> 
    #>   length      n    NAs unique levels  dupes
    #>    1'209  1'201      8      3      3      y
    #>           99.3%   0.7%                     
    #> 
    #>      level  freq   perc  cumfreq  cumperc
    #> 1   Rhonda   446  37.1%      446    37.1%
    #> 2    Maria   388  32.3%      834    69.4%
    #> 3  Allanah   367  30.6%    1'201   100.0%

<img src="man/figures/README-demo-describe-5.png" width="100%" />

    #> ------------------------------------------------------------------------------ 
    #> 6 - date (Date)
    #> 
    #>   length      n    NAs unique
    #>    1'209  1'177     32     31
    #>           97.4%   2.6%       
    #> 
    #> lowest : 2014-03-01 (42), 2014-03-02 (46), 2014-03-03 (26), 2014-03-04 (19)
    #> highest: 2014-03-28 (46), 2014-03-29 (53), 2014-03-30 (43), 2014-03-31 (34)
    #> 
    #> 
    #> Weekday:
    #> 
    #> Pearson's Chi-squared test (1-dim uniform):
    #>   X-squared = 78.879, df = 6, p-value = 6.09e-15
    #> 
    #>        level  freq   perc  cumfreq  cumperc
    #> 1     Monday   144  12.2%      144    12.2%
    #> 2    Tuesday   117   9.9%      261    22.2%
    #> 3  Wednesday   134  11.4%      395    33.6%
    #> 4   Thursday   147  12.5%      542    46.0%
    #> 5     Friday   171  14.5%      713    60.6%
    #> 6   Saturday   244  20.7%      957    81.3%
    #> 7     Sunday   220  18.7%    1'177   100.0%
    #> 
    #> Months:
    #> 
    #> Pearson's Chi-squared test (1-dim uniform):
    #>   X-squared = 12947, df = 11, p-value < 2.2e-16
    #> 
    #>         level   freq    perc  cumfreq  cumperc
    #> 1     January      0    0.0%        0     0.0%
    #> 2    February      0    0.0%        0     0.0%
    #> 3       March  1'177  100.0%    1'177   100.0%
    #> 4       April      0    0.0%    1'177   100.0%
    #> 5         May      0    0.0%    1'177   100.0%
    #> 6        June      0    0.0%    1'177   100.0%
    #> 7        July      0    0.0%    1'177   100.0%
    #> 8      August      0    0.0%    1'177   100.0%
    #> 9   September      0    0.0%    1'177   100.0%
    #> 10    October      0    0.0%    1'177   100.0%
    #> 11   November      0    0.0%    1'177   100.0%
    #> 12   December      0    0.0%    1'177   100.0%
    #> 
    #> By days :
    #> 
    #>          level  freq  perc  cumfreq  cumperc
    #> 1   2014-03-01    42  3.6%       42     3.6%
    #> 2   2014-03-02    46  3.9%       88     7.5%
    #> 3   2014-03-03    26  2.2%      114     9.7%
    #> 4   2014-03-04    19  1.6%      133    11.3%
    #> 5   2014-03-05    33  2.8%      166    14.1%
    #> 6   2014-03-06    39  3.3%      205    17.4%
    #> 7   2014-03-07    44  3.7%      249    21.2%
    #> 8   2014-03-08    55  4.7%      304    25.8%
    #> 9   2014-03-09    42  3.6%      346    29.4%
    #> 10  2014-03-10    26  2.2%      372    31.6%
    #> 11  2014-03-11    34  2.9%      406    34.5%
    #> 12  2014-03-12    36  3.1%      442    37.6%
    #> 13  2014-03-13    35  3.0%      477    40.5%
    #> 14  2014-03-14    38  3.2%      515    43.8%
    #> 15  2014-03-15    48  4.1%      563    47.8%
    #> 16  2014-03-16    47  4.0%      610    51.8%
    #> 17  2014-03-17    30  2.5%      640    54.4%
    #> 18  2014-03-18    32  2.7%      672    57.1%
    #> 19  2014-03-19    31  2.6%      703    59.7%
    #> 20  2014-03-20    36  3.1%      739    62.8%
    #> 21  2014-03-21    43  3.7%      782    66.4%
    #> 22  2014-03-22    46  3.9%      828    70.3%
    #> 23  2014-03-23    42  3.6%      870    73.9%
    #> 24  2014-03-24    28  2.4%      898    76.3%
    #> 25  2014-03-25    32  2.7%      930    79.0%
    #> 26  2014-03-26    34  2.9%      964    81.9%
    #> 27  2014-03-27    37  3.1%    1'001    85.0%
    #> 28  2014-03-28    46  3.9%    1'047    89.0%
    #> 29  2014-03-29    53  4.5%    1'100    93.5%
    #> 30  2014-03-30    43  3.7%    1'143    97.1%
    #> 31  2014-03-31    34  2.9%    1'177   100.0%

<img src="man/figures/README-demo-describe-6.png" width="100%" /><img src="man/figures/README-demo-describe-7.png" width="100%" /><img src="man/figures/README-demo-describe-8.png" width="100%" />

    #> ------------------------------------------------------------------------------ 
    #> 7 - wine_ordered (integer - dichotomous)
    #> 
    #>   length      n    NAs unique
    #>    1'209  1'197     12      2
    #>           99.0%   1.0%       
    #> 
    #>     freq   perc  lci.95  uci.95'
    #> 0  1'010  84.4%   82.2%   86.3%
    #> 1    187  15.6%   13.7%   17.8%
    #> 
    #> ' 95%-CI (Wilson)

<img src="man/figures/README-demo-describe-9.png" width="100%" />

    #> 
    #> > # just a few groupwise descriptions on the console
    #> > Desc(temperature ~ driver, d.pizza, plotit=TRUE)
    #> ------------------------------------------------------------------------------ 
    #> temperature ~ driver (d.pizza)
    #> 
    #> Summary: 
    #> n pairs: 1'209, valid: 1'166 (96.4%), missings: 43 (3.6%), groups: 7
    #> 
    #>                                                                         
    #>           Butcher  Carpenter     Carter     Farmer     Hunter     Miller
    #> mean       49.617     43.493     50.419     50.937     52.141     47.524
    #> median     51.400     44.800     51.750     54.100     55.100     49.600
    #> sd          8.787      9.407      8.467      9.024      8.885      8.935
    #> IQR        11.975     12.500     11.325     11.200     11.575      8.800
    #> n              96        253        226        117        156        121
    #> np         8.233%    21.698%    19.383%    10.034%    13.379%    10.377%
    #> NAs             0         19          8          0          0          4
    #> 0s              0          0          0          0          0          0
    #>                  
    #>            Taylor
    #> mean       45.091
    #> median     48.500
    #> sd         11.442
    #> IQR        18.400
    #> n             197
    #> np        16.895%
    #> NAs             7
    #> 0s              0
    #> 
    #> Kruskal-Wallis rank sum test:
    #>   Kruskal-Wallis chi-squared = 141.93, df = 6, p-value < 2.2e-16
    #> Warning:
    #>   Grouping variable contains 5 NAs (0.414%).

<img src="man/figures/README-demo-describe-10.png" width="100%" />

    #> 
    #> > Desc(driver ~ temperature, d.pizza, plotit=TRUE)
    #> ------------------------------------------------------------------------------ 
    #> driver ~ temperature (d.pizza)
    #> 
    #> Summary: 
    #> n pairs: 1'209, valid: 1'166 (96.4%), missings: 43 (3.6%), groups: 7
    #> 
    #>                                                                         
    #>           Butcher  Carpenter     Carter     Farmer     Hunter     Miller
    #> mean       49.617     43.493     50.419     50.937     52.141     47.524
    #> median     51.400     44.800     51.750     54.100     55.100     49.600
    #> sd          8.787      9.407      8.467      9.024      8.885      8.935
    #> IQR        11.975     12.500     11.325     11.200     11.575      8.800
    #> n              96        253        226        117        156        121
    #> np         8.233%    21.698%    19.383%    10.034%    13.379%    10.377%
    #> NAs             0         19          8          0          0          4
    #> 0s              0          0          0          0          0          0
    #>                  
    #>            Taylor
    #> mean       45.091
    #> median     48.500
    #> sd         11.442
    #> IQR        18.400
    #> n             197
    #> np        16.895%
    #> NAs             7
    #> 0s              0
    #> 
    #> Kruskal-Wallis rank sum test:
    #>   Kruskal-Wallis chi-squared = 141.93, df = 6, p-value < 2.2e-16
    #> Warning:
    #>   Grouping variable contains 5 NAs (0.414%).
    #> 
    #> 
    #> 
    #> Proportions of driver in the quantiles of temperature:
    #>            
    #>                  Q1      Q2      Q3      Q4
    #>   Butcher      6.8%    8.1%    7.3%   10.7%
    #>   Carpenter   34.9%   28.8%   15.9%    6.9%
    #>   Carter      13.7%   18.3%   21.1%   24.5%
    #>   Farmer       6.5%    4.7%   14.9%   14.1%
    #>   Hunter       7.5%    9.5%   11.8%   24.8%
    #>   Miller       9.2%   12.9%   13.1%    6.2%
    #>   Taylor      21.2%   17.6%   15.9%   12.8%

<img src="man/figures/README-demo-describe-11.png" width="100%" />

    #> 
    #> > Desc(temperature ~ delivery_min, d.pizza, plotit=TRUE)
    #> ------------------------------------------------------------------------------ 
    #> temperature ~ delivery_min (d.pizza)
    #> 
    #> Summary: 
    #> n pairs: 1'209, valid: 1'170 (96.8%), missings: 39 (3.2%)
    #> 
    #> 
    #> Pearson corr. : -0.575
    #> Spearman corr.: -0.573
    #> Kendall corr. : -0.422
    #> 
    #> > Desc(quality ~ driver, d.pizza, plotit=TRUE, rfrq=("111")) # show all rel. frequencies
    #> ------------------------------------------------------------------------------ 
    #> quality ~ driver (d.pizza)
    #> Error in strsplit(rfrq, NULL): non-character argument

<img src="man/figures/README-demo-describe-12.png" width="100%" />

## Demo “plots”

``` r
demo(plots, package = "DescTools")
#> 
#> 
#>  demo(plots)
#>  ---- ~~~~~
#> 
#> > ## -----------------------------------------------------------------------------------
#> > ## Demo file for plots; start with 'demo(plots)'
#> > ## -----------------------------------------------------------------------------------
#> > 
#> > 
#> > tab <- matrix(c(2,5,8,3,10,12,5,7,15), nrow=3, byrow=FALSE,
#> +               dimnames = list(c("A","B","C"), c("D","E","F")) )
#> 
#> > par(mfrow=c(1,1), xpd=TRUE)
#> 
#> > PlotCirc( tab,
#> +           acol = c("dodgerblue","seagreen2","limegreen","olivedrab2","goldenrod2","tomato2"),
#> +           rcol = SetAlpha(c("red","orange","olivedrab1"), 0.5)
#> + )
```

<img src="man/figures/README-demo-plots-1.png" width="100%" />

    #> 
    #> > # distribution plot (combination of histogram, densitycurve, boxplot and ecdf.plot)
    #> > old.par <- par(no.readonly=TRUE)
    #> 
    #> > PlotFdist(x=d.pizza$delivery_min, na.rm=TRUE)

<img src="man/figures/README-demo-plots-2.png" width="100%" />

    #> 
    #> > # plot multiple density curves
    #> > par(old.par)
    #> 
    #> > PlotMultiDens( split(d.pizza$delivery_min, d.pizza$driver), na.rm=TRUE
    #> +                , main="delivery time ~ driver", xlab="delivery time [min]", ylab="density"
    #> +                , panel.first=grid())

<img src="man/figures/README-demo-plots-3.png" width="100%" />

    #> 
    #> > # areaplot with stapled areas
    #> > tab <- table( d.pizza$date, d.pizza$driver )
    #> 
    #> > PlotArea(x=as.Date(rownames(tab)), y=tab, xaxt="n", xlab="Date", ylab="Pizzas delivered" )

<img src="man/figures/README-demo-plots-4.png" width="100%" />

    #> 
    #> > # add x-axis and some text labels
    #> > xrng <- pretty(range(as.Date(rownames(tab))))
    #> 
    #> > axis(side=1, at=xrng, labels=xrng)
    #> 
    #> > text( x=min(d.pizza$date + .5, na.rm=TRUE), y=cumsum(tab[2,])-2.5,
    #> +       label=levels(d.pizza$driver), adj=c(0,0.5), col=TextContrastColor( gray.colors(7)))
    #> 
    #> > # dotchart with confidence intervals
    #> > x <- do.call("rbind", tapply( d.pizza$temperature, d.pizza$driver, MeanCI, na.rm=TRUE))
    #> 
    #> > rownames(x) <- levels(d.pizza$driver)
    #> 
    #> > PlotDot(x)

<img src="man/figures/README-demo-plots-5.png" width="100%" />

    #> 
    #> > # Plot pyramid
    #> > xy.pop <- c(3.2,3.5,3.6,3.6,3.5,3.5,3.9,3.7,3.9,3.5,3.2,2.8,2.2,1.8,1.5,1.3,0.7,0.4)
    #> 
    #> > xx.pop <- c(3.2,3.4,3.5,3.5,3.5,3.7,4,3.8,3.9,3.6,3.2,2.5,2,1.7,1.5,1.3,1,0.8)
    #> 
    #> > agelabels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
    #> +                "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-44","85+")
    #> 
    #> > PlotPyramid( xy.pop, xx.pop, ylab=agelabels, lxlab="men", rxlab="women",
    #> +              main="Australian population pyramid 2002", col=PalHelsana()[c(6,1)])
    #> Error in PalHelsana(): could not find function "PalHelsana"

<img src="man/figures/README-demo-plots-6.png" width="100%" />

<!-- # ****************************************************** -->

<!-- # There are no examples defined here. But see the demos: -->

<!-- # -->

<!-- # demo(describe) -->

<!-- # demo(plots) -->

<!-- # -->

<!-- # ****************************************************** -->
