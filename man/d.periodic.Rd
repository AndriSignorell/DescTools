\name{d.periodic}
\alias{d.periodic}
\docType{data}
\title{Periodic Table of Elements
%%   ~~ data name/kind ... ~~
}
\description{This data.frame contains the most important properties of the periodic table of the elements.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(d.periodic)}
\format{
  A data frame with 110 observations on the following 24 variables.
  \describe{
    \item{\code{symbol}}{symbol of an element.}
    \item{\code{nr}}{atomic number of an atomic symbol. }
    \item{\code{name}}{name of an element.}
    \item{\code{group}}{group of an element. Possible results are: Alkali Earth, Alkali Met., Halogen, Metal, Noble Gas, Non-Metal, Rare Earth and Trans. Met. }
    \item{\code{weight}}{atomic weight of an element. The values are based upon carbon-12. () indicates the most stable or best known isotope.}
    \item{\code{meltpt}}{melting point of an element in [K].}
    \item{\code{boilpt}}{boiling point of an element in Kelvin [K].}
    \item{\code{dens}}{density of an element in [g/cm3] at 300K and 1 atm.}
    \item{\code{elconf}}{electron configuration of an element.}
    \item{\code{oxstat}}{oxidation states of an element. The most stable is indicated by a "!".}
    \item{\code{struct}}{crystal structure of an element. Possible results are: Cubic, Cubic body centered, Cubic face centered, Hexagonal, Monoclinic, Orthorhombic, Rhombohedral, Tetragonal}
    \item{\code{covrad}}{covalent radius of an element in Angstroem [A].}
    \item{\code{arad}}{atomic radius of an element in Angstroem.}
    \item{\code{avol}}{atomic volume of an element in [cm3/mol].}
    \item{\code{spheat}}{specific heat of an element in [J/(g K)].}
    \item{\code{eneg}}{electronegativity (Pauling's) of an element.}
    \item{\code{fusheat}}{heat of fusion of an element in [kJ/mol].}
    \item{\code{vapheat}}{heat of vaporization of an element in [kJ/mol].}
    \item{\code{elcond}}{electrical conductivity of an element in [1/(Ohm cm].}
    \item{\code{thermcond}}{thermal conductivity of an element in [W/(cm K)].}
    \item{\code{ionpot1}}{first ionization potential of an element in [V].}
    \item{\code{ionpot2}}{second ionization potential of an element in [V].}
    \item{\code{ionpot3}}{third ionization potential of an element in [V].}
    \item{\code{discyear}}{year of discovery of the element}
  }
}
\references{\url{http://en.wikipedia.org/wiki/Periodic_table}}
\keyword{datasets}
