\name{DescTools-package}
\alias{DescTools-package}
\alias{DescTools}
\docType{package}
\title{ Tools for Descriptive Statistics and Exploratory Data Analysis
}
\description{
DescTools is an extensive collection of miscellaneous basic statistics functions and comfort wrappers not available in the R basic system for efficient description of data.
The author's intention was to create a toolbox, which facilitates the (notoriously time consuming) first descriptive tasks in data analysis, consisting of calculating descriptive statistics, drawing graphical summaries and reporting the results. Special attention was paid to the integration of various approaches to the calculation of confidence intervals. For most basic statistics functions, variants are included that allow the use of weights. The package contains furthermore functions to produce documents using MS Word (or PowerPoint) and functions to import data from Excel. \cr
A considerable part of the included functions can be found scattered in other packages and other sources written partly by Titans of R. The reason for collecting them here, was primarily to have them consolidated in ONE instead of dozens of packages (which themselves might depend on other packages which are not needed at all), and to provide a common and consistent interface as far as function and arguments naming, \code{NA} handling, recycling rules etc. are concerned. Google style guides were used as naming rules (in absence of convincing alternatives). The 'CamelStyle' was consequently applied to functions borrowed from contributed R packages as well.
\cr

Feedback, feature requests, bugreports and other suggestions are welcome! Please report problems to Stack Overflow using tag [desctools] or directly to the maintainer.
}

\section{Warning}{This package is still under development. Although the code seems meanwhile quite stable, until release of version 1.0 (which is expected in hmm: near future?) you should be aware that everything in the package might be subject to change. Backward compatibility is not yet guaranteed. Functions may be deleted or renamed and new syntax may be inconsistent with earlier versions. By release of version 1.0 the "deprecated-defunct process" will be installed.
}

\details{

A grouped list of the functions:

\tabular{ll}{
\cr\cr
\strong{Operators, calculus, transformations:}\tab\cr
\link{\%()\%}    \tab      Between operators determine if a value lies within a range [a,b] \cr
\link{\%)(\%}    \tab      Outside operators: \%)(\%, \%](\%, \%)[\%, \%][\% \cr
\link{\%nin\%}    \tab     "not in" operator \cr
\link{\%overlaps\%}    \tab     Do two collections have common elements? \cr
\link{\%like\%}, \link{\%like any\%}    \tab     Simple operator to search for a specified pattern \cr
\link{\%^\%}    \tab     Powers of matrices \cr
\link{Interval}       \tab    The number of days of the overlapping part \cr
             \tab of two date periods \cr
\link{AUC}        \tab      Area under the curve \cr
\link{Primes}     \tab      Find all primes less than n \cr
\link{Factorize}  \tab      Prime factorization of integers \cr
\link{Divisors}  \tab      All divisors of an integer\cr
\link{GCD}      \tab    Greatest common divisor\cr
\link{LCM}      \tab    Least common multiple\cr
\link{Permn}      \tab      Determine all possible permutations of a set \cr
\link{Fibonacci}  \tab      Generates single Fibonacci numbers or a Fibonacci sequence\cr
\link{DigitSum}  \tab      Digit sum of a number\cr
\link{Frac}       \tab      Return the fractional part of a numeric value \cr
\link{Ndec}       \tab      Count decimal places of a number \cr
\link{MaxDigits}       \tab      Maximum used digits for a vector of numbers \cr
\link{Prec}       \tab      Precision of a number \cr
\link{BoxCox}, \link{BoxCoxInv}     \tab      Box Cox transformation and its inverse transformation \cr
\link{BoxCoxLambda}       \tab      Return the optimal lambda for a BoxCox transformation \cr
\link{LogSt}, \link{LogStInv}      \tab      Calculate started logarithmic transformation and it's inverse\cr
\link{Logit}, \link{LogitInv}      \tab      Generalized logit and inverse logit function \cr
\link{LinScale}       \tab      Simple linear scaling of a vector x \cr
\link{Winsorize}  \tab      Data cleaning by winsorization \cr
\link{Trim}  \tab      Trim data by omitting outlying observations \cr
\link{CutQ}       \tab     Cut a numeric variable into quartiles or other quantiles \cr
\link{Recode}      \tab            Recode a factor with altered levels  \cr
\link{Rename}      \tab            Change name(s) of a named object \cr
\link{Sort}      \tab            Sort extension for matrices and data.frames \cr
\link{SortMixed}, \link{OrderMixed}    \tab Mixed sort order \cr
\link{Rank}       \tab      Calculate ranks including dense type for ties  \cr
\link{PercentRank}       \tab      Calculate the percent rank \cr
\link{RoundTo}       \tab Round to a multiple \cr
\link{Large}, \link{Small}  \tab    Returns the kth largest, resp. smallest values\cr
\link{HighLow} \tab Combines \code{Large} and \code{Small}.\cr
\link{Rev}      \tab            Reverses the order of rows and/or columns of a matrix or a data.frame \cr
\link{Untable}      \tab            Recreates original list based on a n-dimensional frequency table \cr
\link{CollapseTable}       \tab      Collapse some rows/columns in a table. \cr
\link{Dummy}      \tab      Generate dummy codes for a factor \cr
\link{FisherZ}, \link{FisherZInv}      \tab      Fisher's z-transformation and its inverse \cr
\link{Midx}       \tab Calculate sequentially the midpoints of the elements of a vector\cr
\link{Unwhich}       \tab   Inverse function to \code{\link{which}}, create a logical vector/matrix from indices \cr
\link{Vigenere}       \tab      Implements a Vigenere cypher, both encryption and decryption  \cr
\link{BinTree}, \link{PlotBinTree}       \tab      Create and plot a binary tree structure with a given length \cr

\cr\cr
\bold{Information and manipulation functions:}\tab\cr
\link{AllDuplicated}       \tab Find all values involved in ties \cr
\link{Closest}       \tab Return the value in a vector being closest to a given one\cr
\link{Coalesce}       \tab      Return the first value in a vector not being \code{NA} \cr
\link{ZeroIfNA}, \link{NAIfZero}       \tab   Replace NAs by 0, resp. vice versa \cr
\link{Impute}       \tab   Replace NAs by the median or another value \cr
\link{LOCF}      \tab         Imputation of datapoints following the "last observation \cr
\tab carried forward" rule \cr
\link{CombN}       \tab      Returns the number of subsets out of a list of elements \cr
\link{CombSet}       \tab      Generates all possible subsets out of a list of elements \cr
\link{CombPairs}      \tab            Generates all pairs out of one or two sets of elements \cr
\link{SampleTwins}       \tab   Create sample using stratifying groups \cr
\link{RndPairs}       \tab      Create pairs of correlated random numbers \cr
\link{RndWord}       \tab      Produce random combinations of characters \cr
\link{IsNumeric}       \tab      Check a vector for being numeric, zero Or a whole number \cr
\link{IsWhole}      \tab            Is x a whole number? \cr
\link{IsDichotomous}       \tab   Check if x contains exactly 2 values \cr
\link{IsOdd}       \tab    Is x even or odd? \cr
\link{IsPrime}       \tab  Is x a prime number? \cr
\link{IsZero}       \tab   Is numeric(x) == 0, say x < machine.eps? \cr
\link{IsEuclid}       \tab     Check if a distance matrix is euclidean\cr
\link{Label}, \link{Unit}    \tab            Get or set the \code{label}, resp. \code{unit}, attribute of an object \cr
\link{Abind}  \tab  Bind matrices to n-dimensional arrays     \cr
\link{Append}  \tab  Append elements to several classes of objects     \cr
\link{VecRot}, \link{VecShift}  \tab  Shift the elements of a vector in a circular mode to the right \cr
\tab or to the left by n characters.     \cr
\link{Clockwise}       \tab      Transform angles from counter clock into clockwise mode \cr
\link{split.formula}       \tab      A formula interface for the base function split \cr
\link{reorder.factor}       \tab      Reorder the levels of a factor \cr
\link{ToLong}, \link{ToWide}     \tab Simple reshaping of a vector \cr
\link{SetNames}       \tab      Set the names, rownames or columnnames in an object and return it\cr
\link{Some}       \tab      Return some randomly chosen elements of an object \cr
\link{SplitAt}       \tab      Split a vector into several pieces at given positions \cr
\link{SplitToCol}       \tab      Splits the columns of a data frame using a split character\cr
\link{SplitPath}       \tab      Split a path string in drive, path, filename \cr
\link{Str}       \tab      Compactly display the structure of any R object \cr
\link{TextToTable}       \tab      Converts a string to a table \cr
\cr\cr
\bold{String functions:}\tab\cr
\link{StrCountW}      \tab            Count the words in a string \cr
\link{StrTrim}       \tab      Delete white spaces from a string \cr
\link{StrTrunc}   \tab      Truncate string on a given length and add ellipses if it really \cr
\tab was truncated  \cr
\link{StrLeft}, \link{StrRight}      \tab      Returns the left/right part or the a string. \cr
\link{StrAlign}       \tab      Align strings to the left/right/center or to a given character \cr
\link{StrAbbr}       \tab      Abbreviates a string \cr
\link{StrCap}      \tab            Capitalize the first letter of a string  \cr
\link{StrPad}       \tab      Fill a string with defined characters to fit a given length\cr
\link{StrRev}      \tab            Reverse a string \cr
\link{StrChop}      \tab          Split a string by a fixed number of characters. \cr
\link{StrExtract}       \tab      Extract a part of a string, defined as regular expression. \cr
\link{StrVal}      \tab          Extract numeric values from a string \cr
\link{StrIsNumeric}       \tab    Check whether a string does only contain numeric data \cr
\link{StrPos}       \tab      Find position of first occurrence of a string in another one \cr
\link{StrDist}      \tab            Compute Levenshtein or Hamming distance between strings \cr
\link{FixToTable}       \tab      Create table out of a running text, by using columns of spaces as delimiter \cr
\cr\cr
\bold{Conversion functions:}\tab\cr
\link{AscToChar}, \link{CharToAsc}       \tab      Converts ASCII codes to characters and vice versa  \cr
\link{DecToBin}, \link{BinToDec}   \tab      Converts numbers from binmode to decimal and vice versa  \cr
\link{DecToHex}, \link{HexToDec}   \tab      Converts numbers from hexmode to decimal and vice versa  \cr
\link{DecToOct}, \link{OctToDec}   \tab      Converts numbers from octmode to decimal and vice versa  \cr
\link{DegToRad}, \link{RadToDeg}      \tab            Convert degrees to radians and vice versa  \cr
\link{CartToPol}, \link{PolToCart}     \tab            Transform cartesian to polar coordinates and vice versa \cr
\link{CartToSph}, \link{SphToCart}     \tab            Transform cartesian to spherical coordinates and vice versa \cr
\link{RomanToInt}    \tab    Convert roman numerals to integers\cr
\link{RgbToLong}, \link{LongToRgb}       \tab Convert a rgb color to a long number and vice versa\cr
\link{ColToGray}, \link{ColToGrey}  \tab  Convert colors to gcrey/grayscale     \cr
\link{ColToHex}, \link{HexToCol}  \tab   Convert a color into hex string    \cr
\link{HexToRgb} \tab  Convert a hexnumber to an RGB-color\cr
\link{ColToHsv}  \tab   R color to HSV conversion  \cr
\link{ColToRgb}, \link{RgbToCol}  \tab    Color to RGB conversion and back   \cr
\link{ConvUnit}       \tab      Return the most common unit conversions \cr

\cr\cr
\bold{Colors:}\tab\cr
\link{SetAlpha}   \tab  Add transperancy (alpha channel) to a color.     \cr
\link{ColorLegend}  \tab   Add a color legend to a plot    \cr
\link{FindColor}  \tab   Get color on a defined color range    \cr
\link{MixColor}  \tab   Get the mix of two colors  \cr
\link{TextContrastColor} \tab   Choose textcolor depending on background color    \cr
\link{Pal} \tab   Some custom color palettes    \cr
\cr\cr
\bold{Plots (low level):}\tab\cr
\link{Canvas}  \tab    Canvas for geometric plotting   \cr
\link{Mar}  \tab    Set margins more comfortably.  \cr
\link{Asp}       \tab      Return aspect ratio of the current plot \cr
\link{LineToUser}       \tab      Convert line coordinates to user coordinates \cr
\link{lines.loess}   \tab    Add a loess smoother and its CIs to an existing plot   \cr
\link{lines.lm}       \tab    Add the prediction of linear model and its CIs to a plot\cr
\link{lines.smooth.spline}       \tab  Add the prediction of a smooth.spline and its CIs to a plot   \cr
\link{BubbleLegend}       \tab      Add a legend for bubbles to a bubble plot \cr
\link{TitleRect}       \tab      Add a main title to a plot surrounded by a rectangular box \cr
\link{BarText}       \tab      Add the value labels to a barplot \cr
\link{ErrBars}   \tab    Add horizontal or vertical error bars to an existing plot   \cr
\link{DrawArc}, \link{DrawRegPolygon}  \tab  Draw elliptic, circular arc(s) or regular polygon(s)     \cr
\link{DrawCircle}, \link{DrawEllipse}  \tab  Draw a circle, a circle annulus or a sector or an annulus   \cr
\link{DrawBezier}  \tab    Draw a Bezier curve   \cr
\link{DrawBand}    \tab  Draw confidence band  \cr
\link{BoxedText}  \tab   Add text surrounded by a box to a plot    \cr
\link{Rotate}  \tab  Rotate a geometric structure     \cr
\link{SpreadOut}  \tab  Spread out a vector of numbers so that there is a minimum \cr
\tab interval between any two elements. This can be used \cr
\tab to place textlabels in a plot so that they do not overlap.\cr
\link{IdentifyA}  \tab  Helps identifying all the points in a specific area.     \cr
\link{identify.formula}  \tab  Formula interface for \code{\link{identify}}.     \cr
\link{PtInPoly}  \tab  Identify all the points within a polygon. \cr
\link{ConnLines}       \tab   Calculate and insert connecting lines in a barplot \cr
\link{AxisBreak}       \tab      Place a break mark on an axis \cr
\link{Shade}       \tab      Produce a shaded curve \cr
\link{Stamp}       \tab      Stamp the current plot with Date/Time/Directory or any other expression \cr
\cr\cr
\bold{Plots (high level):}\tab\cr
\link{PlotACF}, \link{PlotGACF} \tab  Create a combined plot of a time series including its \cr
\tab autocorrelation and partial autocorrelation\cr
\link{PlotMonth} \tab     Plot seasonal effects of a univariate time series\cr
\link{PlotArea}    \tab  Create an area plot \cr
\link{PlotBag}    \tab  Create a two-dimensional boxplot \cr
\link{PlotBagPairs}       \tab      Produce pairwise 2-dimensional boxplots (bagplot) \cr
\link{PlotBubble}    \tab  Draw a bubble plot \cr
\link{PlotCandlestick}    \tab  Plot candlestick chart \cr
\link{PlotCirc}    \tab  Create a circular plot \cr
\link{PlotCorr}    \tab Plot a correlation matrix  \cr
\link{PlotDot}    \tab  Plot a dotchart with confidence intervals \cr
\link{PlotFaces}    \tab  Produce a plot of Chernoff faces  \cr
\link{PlotFdist}    \tab  Frequency distribution plot, combination of histogram, \cr
\tab boxplot and ecdf.plot \cr
\link{PlotMarDens}    \tab Scatterplot with marginal densities  \cr
\link{PlotMultiDens}    \tab  Plot multiple density curves \cr
\link{PlotPolar}    \tab Plot values on a circular grid \cr
\link{PlotFun}    \tab Plot mathematical expression or a function \cr
\link{PolarGrid}    \tab  Plot a grid in polar coordinates \cr
\link{PlotPyramid}    \tab  Pyramid plot (back-back histogram) \cr
\link{PlotTreemap}    \tab   Plot of a treemap.\cr
\link{PlotVenn}    \tab  Plot a Venn diagram \cr
\link{PlotViolin}    \tab   Plot violins instead of boxplots \cr
\link{PlotQQ}    \tab  QQ-plot for an optional distribution\cr
\link{PlotWeb}    \tab  Create a web plot \cr
\link{PlotTernary}    \tab  Create a triangle or ternary plot \cr
\link{PlotMiss}    \tab  Plot missing values \cr
\link{PlotECDF}       \tab     Plot empirical cumulative distribution function \cr
\link{PlotLinesA}       \tab      Plot the columns of one matrix against the columns of another \cr
\link{PlotLog}       \tab     Create a plot with logarithmic axis and log grid  \cr
\link{PlotMosaic}       \tab    Plots a mosaic describing a  contingency table in array form \cr
\cr\cr
\bold{Distributions:}\tab\cr
_Benf   \tab      Benford distribution, including \link{qBenf}, \link{dBenf}, \link{rBenf}\cr
_ExtrVal       \tab      Extreme value distribution (\link{dExtrVal})\cr
_Frechet       \tab      Frechet distribution (\link{dFrechet})\cr
_GenExtrVal       \tab       Generalized Extreme Value Distribution (\link{dGenExtrVal})\cr
_GenPareto       \tab       Generalized Pareto Distribution (\link{dGenPareto})\cr
_Gompertz       \tab      Gompertz distribution (\link{dGompertz})\cr
_Gumbel       \tab      Gumbel distribution (\link{dGumbel})\cr
_NegWeibull   \tab      Negative Weibull distribution (\link{dNegWeibull})\cr
_Order       \tab      Distributions of Order Statistics (\link{dOrder})\cr
_RevGumbel       \tab      Reverse Gumbel distribution (\link{dRevGumbel}),\cr
_RevGumbelExp   \tab      Expontial reverse Gumbel distribution (quantile only)\cr
_RevWeibull   \tab      Reverse Weibull distribution (\link{dRevWeibull})\cr
\cr\cr
\bold{Statistics:}\tab\cr
\link{Freq}  \tab    Univariate frequency table   \cr
\link{PercTable}  \tab    Bivariate percentage table   \cr
\link{Margins}       \tab      (Extended) margin tables of a table \cr
\link{ExpFreq}       \tab      Expected frequencies of a n-dimensional table \cr
\link{Mode}  \tab     Mode, the most frequent value (including frequency)   \cr
\link{Gmean}, \link{Gsd}  \tab  Geometric mean and geometric standard deviation     \cr
\link{Hmean}  \tab    Harmonic Mean   \cr
\link{Median}       \tab   Extended median function supporting weights and ordered factors \cr
\link{HuberM}, \link{TukeyBiweight}  \tab    Huber M-estimator of location and Tukey's biweight robust mean   \cr
\link{HodgesLehmann}       \tab the Hodges-Lehmann estimator\cr
\link{HoeffD}       \tab   Hoeffding's D statistic \cr
\link{MeanSE}  \tab   Standard error of mean    \cr
\link{MeanCI}, \link{MedianCI}  \tab    Confidence interval for the mean and median   \cr
\link{MeanDiffCI}  \tab    Confidence interval for the difference of two means \cr
\link{MoveAvg}       \tab  Moving average \cr
\link{MeanAD}  \tab    Mean absolute deviation   \cr
\link{VarCI}  \tab    Confidence interval for the variance   \cr
\link{CoefVar} \tab Coefficient of variation and its confidence interval \cr
\link{RobScale}  \tab   Robust data standardization  \cr
\link{Range}  \tab   (Robust) range  \cr
\link{BinomCI}, \link{MultinomCI}  \tab   Confidence intervals for binomial and multinomial proportions    \cr
\link{BinomDiffCI}  \tab   Calculate confidence interval for a risk difference  \cr
\link{BinomRatioCI}  \tab   Calculate confidence interval for the ratio of binomial proportions.  \cr
\link{PoissonCI}       \tab  Confidence interval for a Poisson lambda \cr
\link{Skew}, \link{Kurt} \tab   Skewness and kurtosis    \cr
\link{YuleQ}, \link{YuleY}  \tab  Yule's Q and Yule's Y      \cr
\link{TschuprowT}  \tab  Tschuprow's T     \cr
\link{Phi}, \link{ContCoef}, \link{CramerV}  \tab   Phi, Pearson's Contingency Coefficient and Cramer's V    \cr
\link{GoodmanKruskalGamma}  \tab  Goodman Kruskal's gamma     \cr
\link{KendallTauA}  \tab  Kendall's tau-a     \cr
\link{KendallTauB}  \tab  Kendall's tau-b     \cr
\link{StuartTauC}  \tab  Stuart's tau-c     \cr
\link{SomersDelta}  \tab  Somers' delta     \cr
\link{Lambda}  \tab  Goodman Kruskal's lambda     \cr
\link{GoodmanKruskalTau}  \tab  Goodman Kruskal's tau    \cr
\link{UncertCoef}  \tab  Uncertainty coefficient    \cr
\link{Entropy}, \link{MutInf}  \tab  Shannon's entropy, mutual information     \cr
\link{DivCoef}, \link{DivCoefMax}    \tab   Rao's diversity coefficient ("quadratic entropy")\cr
\link{TheilU} \tab  Theil's U1 and U2 coefficient     \cr
\link{Assocs}       \tab  Combines the association measures above.  \cr
\link{OddsRatio}, \link{RelRisk}  \tab   Odds ratio and relative risk    \cr
\link{ORToRelRisk}       \tab      Transform odds ratio to relative risk \cr
\link{CohenKappa}, \link{KappaM}  \tab Cohen's Kappa, weighted Kappa and Kappa for \cr
\tab more than 2 raters      \cr
\link{CronbachAlpha}  \tab  Cronbach's alpha     \cr
\link{ICC}  \tab  Intraclass correlations   \cr
\link{KrippAlpha}   \tab  Return Kripp's alpha coefficient \cr
\link{KendallW}     \tab  Compute the Kendall coefficient of concordance \cr
\link{Lc}  \tab   Calculate and plot Lorenz curve    \cr
\link{Gini}, \link{Atkinson}  \tab   Gini- and Atkinson coefficient    \cr
\link{Herfindahl}, \link{Rosenbluth}  \tab  Herfindahl- and Rosenbluth coefficient      \cr
\link{GiniSimpson}       \tab      Compute Gini-Simpson Coefficient \cr
\link{CorCI}  \tab    Confidence interval for Pearson's correlation coefficient \cr
\link{CorPart}       \tab      Find the correlations for a set x of variables with set y removed \cr
\link{CorPolychor}  \tab    Polychoric correlation coefficient \cr
\link{SpearmanRho}       \tab      Spearman rank correlation and its confidence intervals \cr
\link{ConDisPairs}       \tab  Return concordant and discordant pairs of two vectors \cr
\link{FindCorr}       \tab      Determine highly correlated variables \cr
\link{CohenD}       \tab      Cohen's Effect Size \cr
\link{EtaSq}       \tab      Effect size calculations for ANOVAs \cr
\link{Contrasts}       \tab      Generate pairwise contrasts for using in a post-hoc test \cr
\link{Strata}       \tab      Stratified sampling with equal/unequal probabilities \cr
\link{Outlier}  \tab   Outliers following Tukey's boxplot definition  \cr
\link{LOF}  \tab  Local outlier factor  \cr
\link{BrierScore}       \tab       Brier score, assessing the quality of predictions of binary events \cr
\link{Cstat}       \tab       C statistic, equivalent to the area under the ROC curve) \cr
\link{CCC}       \tab      Lin's concordance correlation coef for agreement on a continuous measure \cr
\link{MAE}       \tab      Mean absolute error  \cr
\link{MAPE}, \link{SMAPE}       \tab      Mean absolute and symmetric mean absolute percentage error  \cr
\link{MSE}, \link{RMSE}       \tab       Mean squared error and root mean squared error\cr
\link{NMAE}, \link{NMSE}     \tab      Normalized mean absolute and mean squared error \cr
\link{Conf}       \tab      Confusion matrix, a cross-tabulation of observed and predicted classes\cr
\tab with associated statistics \cr
\link{Sens}, \link{Spec}       \tab     Sensitivity and specificity \cr
\link{PseudoR2}       \tab Variants of pseudo R squared statistics: McFadden, Aldrich-Nelson,\cr
\tab Nagelkerke, CoxSnell, Effron, McKelvey-Zavoina, Tjur \cr
\link{Mean}, \link{SD}, \link{Var}, \link{IQRw}  \tab      Variants of base statistics, allowing to define weights: Mean, \cr
\link{Quantile}, \link{MAD}, \link{Cor}    \tab standard deviation, variance, quantile, mad, correlation \cr
\link{VIF}, \link{StdCoef}       \tab      Variance inflation factors and standardised coefficents for linear models \cr
\cr\cr
\bold{Tests:}\tab\cr
\link{SignTest}    \tab  Signtest to test whether two groups are equally sized\cr
\link{ZTest}    \tab   Z--test for known population variance \cr
\link{TTestA}       \tab      Student's t-test based on sample statistics \cr
\link{JonckheereTerpstraTest}    \tab  Jonckheere-Terpstra trend test for medians\cr
\link{PageTest}    \tab  Page test for ordered alternatives\cr
\link{CochranQTest}    \tab  Cochran's Q-test to find differences in matched sets \cr
\tab of three or more frequencies or proportions.\cr
\link{VarTest}       \tab      ChiSquare test for one variance and F test for two variances \cr
\link{SiegelTukeyTest}    \tab  Siegel-Tukey test for equality in variability \cr
\link{SiegelTukeyRank}     \tab Calculate Siegel-Tukey's ranks (auxiliary function)\cr
\link{LeveneTest}    \tab  Levene's test for homogeneity of variance\cr
\link{MosesTest}    \tab  Moses Test of extreme reactions\cr
\link{RunsTest}    \tab  Runs test for detecting non-randomness\cr
\link{DurbinWatsonTest}    \tab  Durbin-Watson test for autocorrelation\cr
\link{BartelsRankTest}    \tab  Bartels rank test for randomness\cr
\link{JarqueBeraTest}    \tab  Jarque-Bera Test for normality\cr
\link{AndersonDarlingTest}    \tab  Anderson-Darling test for normality\cr
\link{CramerVonMisesTest}    \tab  Cramer-von Mises test for normality\cr
\link{LillieTest}    \tab  Lilliefors (Kolmogorov-Smirnov) test for normality\cr
\link{PearsonTest}    \tab  Pearson chi-square test for normality\cr
\link{ShapiroFranciaTest}    \tab  Shapiro-Francia test for normality\cr
\link{MHChisqTest}    \tab  Mantel-Haenszel Chisquare test\cr
\link{StuartMaxwellTest}    \tab  Stuart-Maxwell marginal homogeneity test\cr
\link{LehmacherTest}    \tab  Lehmacher marginal homogeneity test\cr
\link{CochranArmitageTest}    \tab  Cochran-Armitage test for trend in binomial proportions\cr
\link{BreslowDayTest}, \link{WoolfTest}   \tab  Test for homogeneity on 2x2xk tables over strata\cr
\link{PostHocTest}    \tab  Post hoc tests by Scheffe, LSD, Tukey for a aov-object\cr
\link{ScheffeTest}   \tab  Multiple comparisons Scheffe test\cr
\link{DunnTest}   \tab  Dunn's test of multiple comparisons\cr
\link{DunnettTest}   \tab  Dunnett's test of multiple comparisons\cr
\link{ConoverTest}       \tab      Conover's test of multiple comparisons (following a kruskal test)\cr
\link{NemenyiTest}       \tab      Nemenyi's test of multiple comparisons \cr
\link{HotellingsT2Test}   \tab  Hotelling's T2 test for the one and two sample case\cr
\link{YuenTTest}       \tab Yuen's robust t-Test with trimmed means and winsorized variances\cr
\link{BarnardTest}       \tab Barnard's test for 2x2 tables\cr
\link{BreuschGodfreyTest}       \tab      Breusch-Godfrey test for higher-order serial correlation. \cr
\link{GTest}       \tab      Chi-squared contingency table test and goodness-of-fit test \cr
\link{HosmerLemeshowTest}       \tab      Hosmer-Lemeshow goodness of fit tests \cr
\link{VonNeumannTest}       \tab      Von Neumann's successive difference test \cr
\cr\cr
\bold{Date functions:}\tab\cr
\link{day.name}, \link{day.abb} \tab Defined names of the days \cr
\link{AddMonths}, \link{AddMonthsYM}  \tab  Add a number of months to a given date     \cr
\link{IsDate} \tab  Check whether x is a date object     \cr
\link{IsWeekend} \tab  Check whether x falls on a weekend     \cr
\link{IsLeapYear} \tab  Check whether x is a leap year     \cr
\link{LastDayOfMonth}       \tab Return the last day of the month of the date x \cr
\link{DiffDays360}       \tab  Calculate the difference of two dates using the 360-days system \cr
\link{Date}  \tab     Create a date from numeric representation of year, month, day   \cr
\link{Day}, \link{Month}, \link{Year}  \tab     Extract part of a date  \cr
\link{Hour}, \link{Minute}, \link{Second}  \tab     Extract part of time   \cr
\link{Week}, \link{Weekday}  \tab    Returns ISO week and weekday of a date   \cr
\link{Quarter}  \tab  Quarter of a date     \cr
\link{Timezone}  \tab  Timezone of a POSIXct/POSIXlt date     \cr
\link{YearDay}, \link{YearMonth}  \tab  The day in the year of a date     \cr
\link{Now}, \link{Today}  \tab     Get current date or date-time  \cr
\link{HmsToSec}, \link{SecToHms}  \tab   Convert h:m:s times to seconds and vice versa  \cr
\link{Overlap}       \tab      Determine if and how extensively two date ranges overlap \cr
\link{Zodiac}  \tab  The zodiac sign of a date :-)   \cr
\cr\cr
\bold{Finance functions:}\tab\cr
\link{OPR}  \tab  One period returns (simple and log returns)\cr
\link{NPV}  \tab  Net present value\cr
\link{NPVFixBond}       \tab      Net present value for fix bonds\cr
\link{IRR} \tab  Internal rate of return\cr
\link{YTM}       \tab     Return yield to maturity for a bond \cr
\link{SLN}, \link{DB}, \link{SYD}       \tab     Several methods of depreciation of an asset \cr
\cr\cr
\bold{GUI-Helpers:}\tab\cr
\link{PasswordDlg}    \tab  Display a dialog containing an edit field, showing only ***. \cr
\cr\cr
\bold{Reporting, InOut:}\tab\cr
\link{CatTable}  \tab  Print a table with the option to have controlled linebreaks \cr
\link{Format}, \link{Fmt}  \tab  Easy format for numbers and dates      \cr
\link{Desc}  \tab   Produce a rich description of an object    \cr
\link{Abstract}  \tab   Display compact overview of the structure of a data frame   \cr
\link{TMod}  \tab  Create comparison table for (general) linear models    \cr
\link{TOne}  \tab   Create "Table One"" describing baseline characteristics   \cr
\link{GetNewWrd}, \link{GetNewXL}, \link{GetNewPP}     \tab   Create a new Word, Excel or PowerPoint Instance  \cr
\link{GetCurrWrd}, \link{GetCurrXL}, \link{GetCurrPP}    \tab  Get a handle to a running Word, Excel or PowerPoint instance \cr
\link{WrdKill}, \link{XLKill}       \tab      Ends a (possibly hidden) Word/Excel process \cr
\link{IsValidHwnd}       \tab  Check if the handle to a MS Office application is valid or outdated \cr
\link{WrdCaption}    \tab Insert a title in Word   \cr
\link{WrdFont}   \tab  Get and set the font for the current selection in Word \cr
\link{WrdParagraphFormat}    \tab Get and set the paragraph format \cr
\link{WrdTable}       \tab      Create a table in Word \cr
\link{WrdCellRange}       \tab   Select a cell range of a table in Word \cr
\link{WrdMergeCells}       \tab      Merge cells of a table in Word \cr
\link{WrdFormatCells}       \tab      Format selected cells of a table in word \cr
\link{WrdTableBorders}       \tab    Set or edit table border style of a table in Word \cr
\link{ToWrd}, \link{ToXL}    \tab  Mord flexible wrapper to send diverse objects to Word, resp. Excel \cr
\link{WrdPlot}    \tab   Insert the active plot to Word \cr
\link{WrdInsertBookmark}    \tab Insert a new bookmark in a Word document \cr
\link{WrdDeleteBookmark}    \tab Delete an existing bookmark in a Word document \cr
\link{WrdGoto}    \tab Place cursor to a specific bookmark, or another text position. \cr
\link{WrdUpdateBookmark}    \tab Update the text of a bookmark's range\cr
\link{WrdSaveAs}     \tab      Saves documents in Word \cr
\link{WrdStyle}     \tab      Get and set the style of a paragraph in Word \cr
\link{XLDateToPOSIXct}       \tab      Convert XL-Date format to POSIXct format \cr
\link{XLGetRange}    \tab  Get the values of one or several cell range(s) in Excel \cr
\link{XLGetWorkbook}    \tab  Get the values of all sheets of an Excel workbook \cr
\link{XLView}    \tab   Use Excel as viewer for a data.frame \cr
\link{PpPlot}    \tab   Insert active plot to PowerPoint \cr
\link{PpAddSlide}       \tab      Adds a slide to a PowerPoint presentation \cr
\link{PpText}       \tab      Adds a textbox with text to a PP-presentation \cr
\link{ParseSASDatalines}       \tab      Parse a SAS "datalines" statement to read data \cr
\cr\cr
\bold{Tools:}\tab\cr
\link{PairApply}       \tab  Helper for calculating functions pairwise \cr
\link{LsFct}, \link{LsObj}      \tab  List the functions (or the data, all objects) of a package \cr
\link{FctArgs}      \tab            Retrieve the arguments of a functions \cr
\link{InDots}       \tab  Check if an argument is contained in ... argument and return it's value \cr
\link{ParseFormula}      \tab     Parse a formula and return the splitted parts of if \cr
\link{Recycle}       \tab      Recycle a list of elements to the maximal found dimension \cr
\link{Keywords}    \tab  Get the keywords of a man page\cr
\link{SysInfo}    \tab  Get some more information about system and environment\cr
\link{DescToolsOptions}    \tab  Get the DescTools specific options\cr
\link{PDFManual}       \tab      Get the pdf-manual of any package on CRAN and open it\cr
\cr\cr
\bold{Data:}\tab\cr
\link{d.pizza}       \tab  Synthetic dataset created for testing the description \cr
\link{d.whisky}      \tab   of Scotch Single Malts\cr
\cr\cr
\bold{Reference Data:}\tab\cr
\link{d.units},  \link{d.prefix}    \tab  Unit conversion factors and metric prefixes\cr
\link{d.periodic}      \tab  Periodic table of elements\cr
\link{d.countries}      \tab ISO 3166-1 country codes \cr
\link{roulette}, \link{cards},  \link{tarot}     \tab   Datasets for probabilistic simulation\cr
}
}



\section{MS-Office}{To make use of MS-Office features you must have Office in one of its variants installed, as well as the package \bold{RDCOMClient}. This package uses the COM interface to control the Office applications. There is no direct equivalent to COM interface for Mac or Linux, hence the use of these functions is restricted to Windows systems. All \code{Wrd*}, \code{XL*} and \code{Pp*} functions require this basis to run.


\bold{RDCOMClient} can be installed with: \cr
\preformatted{install.packages("RDCOMClient", repos="http://www.omegahat.net/R")}
The omegahat repository does not benefit from the same update service as CRAN. So you may be forced to install a package compiled with an earlier version, which usually is no problem. Use e.g. for R 3.6x/R 4.0\cr
\preformatted{
url <- "http://www.omegahat.net/R/bin/windows/contrib/3.5.1/RDCOMClient_0.93-0.zip"
url <- "http://www.omegahat.net/R/bin/windows/contrib/4.0/RDCOMClient_0.94-0.zip"
install.packages(url, repos=NULL, type="binary")}
\bold{RDCOMClient} does not exist for Mac or Linux, sorry.
}


\author{
Andri Signorell\cr
Helsana Versicherungen AG, Health Sciences, Zurich\cr
HWZ University of Applied Sciences in Business Administration Zurich.\cr

R is a community project. This can also be seen in this package, which contains R source code and/or documentation previously published elsewhere by (in alphabetical order): \cr

Ken Aho, Andreas Alfons, Nanina Anderegg, Tomas Aragon, Antti Arppe, Adrian Baddeley, Kamil Barton, Ben Bolker, Hans W. Borchers, Frederico Caeiro, Stephane Champely, Daniel Chessel, Leanne Chhay, Clint Cummins, Michael Dewey, Harold C. Doran, Stephane Dray, Charles Dupont, Dirk Eddelbuettel, Jeff Enos, Claus Ekstrom, Martin Elff, Kamil Erguler, Richard W. Farebrother, John Fox, Romain Francois, Michael Friendly, Tal Galili, Matthias Gamer, Joseph L. Gastwirth, Yulia R. Gel, Juergen Gross, Gabor Grothendieck, Frank E. Harrell Jr, Richard Heiberger, Michael Hoehle, Christian W. Hoffmann, Soeren Hojsgaard, Torsten Hothorn, Markus Huerzeler, Wallace W. Hui, Pete Hurd, Rob J. Hyndman, Pablo J. Villacorta Iglesias, Christopher Jackson, Matthias Kohl, Mikko Korpela, Max Kuhn, Detlew Labes, Duncan Temple Lang, Friederich Leisch, Jim Lemon, Dong Li, Martin Maechler, Arni Magnusson, Daniel Malter, George Marsaglia, John Marsaglia, Alina Matei, David Meyer, Weiwen Miao, Giovanni Millo, Yongyi Min, David Mitchell, Franziska Mueller, Markus Naepflin, Danielle Navarro, Henric Nilsson, Klaus Nordhausen, Derek Ogle, Hong Ooi, Nick Parsons, Sandrine Pavoine, Tony Plate, Roland Rapold, William Revelle, Tyler Rinker, Brian D. Ripley, Caroline Rodriguez, Nathan Russell, Nick Sabbe, Venkatraman E. Seshan, Greg Snow, Michael Smithson, Karline Soetaert, Werner A. Stahel, Alec Stephenson, Mark Stevenson, Matthias Templ, Terry Therneau, Yves Tille, Adrian Trapletti, Joshua Ulrich, Kevin Ushey, Jeremy VanDerWal, Bill Venables, John Verzani, Gregory R. Warnes, Stefan Wellek, Hadley Wickham, Rand R. Wilcox, Peter Wolf, Daniel Wollschlaeger, Thomas Yee, Achim Zeileis

Special thanks go to Beat Bruengger, Mathias Frueh, Daniel Wollschlaeger, Vilmantas Gegzna for their valuable contributions and testing.

The good things come from all these guys, any problems are likely due to my tweaking.
Thank you all! \cr


Maintainer: Andri Signorell <andri@signorell.net>

}

\examples{
# ******************************************************
# There are no examples defined here. But see the demos:
#
# demo(describe)
# demo(plots))
#
# ******************************************************
}
\keyword{ package }


