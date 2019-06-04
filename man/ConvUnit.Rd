\name{ConvUnit}
\alias{ConvUnit}
\alias{d.units}
\alias{d.prefix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Unit Conversion and Metrix Prefixes
%%  ~~function to do ... ~~
}
\description{
Converts a numerical vector from one measurement system to another.
Metric prefixes (as unit prefixes that precede a basic unit of measure to indicate a multiple or fraction of the unit) are respected.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ConvUnit(x, from, to)

data(d.units)
data(d.prefix)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the numeric to be converted.
%%     ~~Describe \code{x} here~~
}
  \item{from}{a character defining the original unit.
%%     ~~Describe \code{from_unit} here~~
}
  \item{to}{a character defining the target unit.
%%     ~~Describe \code{to_unit} here~~
}
}
\details{
The units as defined by the International System of Units (SI) (m, g, s, A, K, mol, cd, Hz, rad, sr, N, Pa, J, W, C, V, F, Ohm, S, Wb, T, H, lm, lx, Bq, Gy, Sv, kat, l) can be used to convert between different prefixes. The following non SI-units can be chosen for conversion between different systems. \code{NA} will be returned if a conversion can't be found. \cr
The function is using the conversion factors stored in the dataset \code{d.units}.



\tabular{lll}{
\cr\cr
\bold{Weight and mass}\tab \tab\cr
Gram  \tab g \tab metric\cr
Pound mass (avoirdupois)  \tab lb \cr
Ounce mass (avoirdupois)  \tab oz \cr
Metric ton  \tab ton (or tn) \cr
\cr\bold{Distance}\tab \cr
Meter  \tab  m \tab metric \cr
Statute mile  \tab mi \cr
Nautical mile  \tab nmi \cr
Inch  \tab in \cr
Foot  \tab ft \cr
Yard  \tab yd \cr
Angstrom  \tab AA \tab (accepted) metric \cr
Astronomical unit  \tab au \tab \cr
  \tab  \cr
\cr\bold{Time}\tab \cr
Year  \tab a \cr
Day  \tab d \cr
Hour  \tab h \cr
Minute  \tab min \cr
Second  \tab s \cr
\cr\bold{Pressure}\tab \cr
Pascal  \tab Pa \cr
Atmosphere  \tab atm \cr
mm of Mercury  \tab mmHg \cr
bar  \tab bar \cr
Pound-force per quare inch  \tab psi \cr
  \tab  \cr
%%\cr\bold{Force}\tab \cr
%%Newton  \tab N \tab metric \cr
%%Dyne  \tab dyn (or dy) \cr
%%Pound force  \tab lbf \cr
\cr\bold{Energy}\tab \cr
Joule  \tab J \tab metric \cr
IT calorie  \tab cal \tab (accepted) metric \cr
Electron volt  \tab eV (or ev) \tab \cr
%%Horsepower-hour  \tab HPh (or hh) \cr
%%Watt-hour  \tab Wh (or wh) \tab metric \cr
%%Foot-pound  \tab flb \cr
%%BTU  \tab BTU (or btu) \cr
  \tab  \cr
\cr\bold{Power}\tab \cr
Horsepower (mechanical)  \tab hp \cr
Horsepower (metric)  \tab HP\cr
Watt  \tab W (or w) \tab metric \cr
  \tab  \cr
\cr\bold{Temperature}\tab \cr
Degree Celsius  \tab C \cr
Degree Fahrenheit  \tab F\cr
Kelvin  \tab K \tab metric \cr
\cr\bold{Liquid measure}\tab \cr
%% Teaspoon  \tab tsp \cr
%% Tablespoon  \tab tbs \cr
Fluid ounce  \tab fl oz \cr
%% Cup  \tab cup \cr
%% U.S. pint  \tab pt (or us_pt) \cr
%% U.K. pint  \tab uk_pt \cr
%% Quart  \tab qt \cr
Gallon  \tab gal \cr
Liter  \tab l (or lt) \tab (accepted) metric \cr
}


Additional details can be found in the \code{d.units} data.frame.

%%  ~~ If necessary, more details than the description above ~~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\examples{
ConvUnit(c(1.2, 5.4, 6.7), "in", "m")

# from kilometers to pico meters
ConvUnit(1, from="km", to="pm")

# from miles to kilometers
ConvUnit(1, from="mi", to="km")
# nautical miles
ConvUnit(1, from="nmi", to="km")
# from kilo Kelvin to Fahrenheit
ConvUnit(10, from="kK", to="F")
# from metric to more quirky units
ConvUnit(c(10, 1), from="hl", to="gal")
ConvUnit(500, from="ml", to="fl oz")

# conversion between non-SI units
ConvUnit(1000, "yd", "mi")
# ... must be the same as
ConvUnit(ConvUnit(1000, "yd", "m"), "m", "mi")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }
