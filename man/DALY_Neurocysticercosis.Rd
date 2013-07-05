\name{DALY_Neurocysticercosis}
\alias{DALY_Neurocysticercosis}

\docType{data}

\title{
  Example Dataset 1: Neurocysticercosis in West-Cameroon
}

\description{
  The \code{Neurocysticercosis} dataset consists of a nested list
  containing data needed to calculate DALYs lost due to
  Neurocysticercosis in West Cameroon, according to Praet et al. (2009).
}

\usage{data(DALY_Neurocysticercosis)}

\format{
  \code{List of 3} \cr\cr
    \code{$ model : List of 2} \cr
      \code{..$ diseaseName : chr "Neurocysticercosis"} \cr
      \code{..$ outcomeNames : List of 8} \cr\cr
    \code{$ settings : List of 4} \cr
      \code{..$ pop : num [1:5, 1:2] 397229 686600 1073342 210474 129081 ...} \cr
      \code{..$ LE : num [1:21, 1:2] 80 79.4 75.4 70.4 65.4 ...} \cr
      \code{..$ aw : chr "Yes"} \cr
      \code{..$ dr : num 3} \cr\cr
    \code{$ data : List of 8} \cr
      \code{..$ : List of 8} \cr
        \code{.. ..$ inc : List of 3} \cr
          \code{.. .. ..$ dist : chr "Gamma"} \cr
          \code{.. .. ..$ strat : chr "Age and Sex"} \cr
          \code{.. .. ..$ param : num [1:5, 1:4] 47.3 47.3 47.3 47.3 47.3 ...} \cr
        \code{.. ..$ trt : List of 3} \cr
          \code{.. .. ..$ dist : chr "Beta"} \cr
          \code{.. .. ..$ strat : chr "None"} \cr
          \code{.. .. ..$ param : num [1, 1:2] 267 733} \cr
        \code{.. ..$ ons : List of 3} \cr
          \code{.. .. ..$ dist : chr "Fixed"} \cr
          \code{.. .. ..$ strat : chr "Age"} \cr
          \code{.. .. ..$ param : num [1:5, 1] 2.5 9.95 26.99 51.94 73.6} \cr
        \code{.. ..$ dur : List of 3} \cr
          \code{.. .. ..$ dist : chr "Fixed"} \cr
          \code{.. .. ..$ strat : chr "Age and Sex"} \cr
          \code{.. .. ..$ param : num [1:5, 1:2] 1.4 2 3.6 2.8 1.6 1.6 3.1 5.9 6 2.8} \cr
        \code{.. ..$ DWt : List of 3} \cr
          \code{.. .. ..$ dist : chr "Beta"} \cr
          \code{.. .. ..$ strat : chr "Age"} \cr
          \code{.. .. ..$ param : num [1:5, 1:2] 1.5 1.5 1.5 1.5 1.5 35 21.6 21.6 21.6 21.6} \cr
        \code{.. ..$ DWn : List of 3} \cr
          \code{.. .. ..$ dist : chr "Beta"} \cr
          \code{.. .. ..$ strat : chr "Age"} \cr
          \code{.. .. ..$ param : num [1:5, 1:2] 3 3 3 3 3 27.3 17 17 17 17} \cr
        \code{.. ..$ mrt : List of 3} \cr
          \code{.. .. ..$ dist : chr "Gamma"} \cr
          \code{.. .. ..$ strat : chr "None"} \cr
          \code{.. .. ..$ param : num [1, 1:2] 3.05 12.32} \cr
        \code{.. ..$ lxp : List of 3} \cr
          \code{.. .. ..$ dist : chr "Fixed"} \cr
          \code{.. .. ..$ strat : chr "Age"} \cr
          \code{.. .. ..$ param : num [1:5, 1] 2.5 10 30 52.5 77.5} \cr\cr
	  \code{[...]}
}

\details{
  \emph{Taenia solium} cysticercosis is a zoonotic disease occurring in many
  developing countries. A relatively high prevalence in humans and pigs has
  been reported in several parts of the world, but insufficient data are available
  on the disease burden. Disease impact assessment needs detailed information on
  well-defined epidemiological and economic parameters. Our work conducted in West
  Cameroon over several years allowed us to collect the necessary information to
  estimate the impact of the parasite on the human and animal populations in this
  area using both cost and Disability Adjusted Life Year (DALY) estimations. This
  study identified the professional inactivity caused by the disease as the major
  loss factor in comparison to the cost of health care and losses due to infected
  pigs. These findings should allow a simpler estimation of the global disease
  burden based on information on salary levels and human cysticercosis prevalence
  in endemic areas of the world. In addition, the number of DALYs lost was higher
  than estimates already available for some other neglected tropical diseases in
  sub-Saharan Africa.
  
  The average number of DALYs lost was 9.0 per thousand persons per year
  (95\% CR 2.8-20.4).
}

\source{
  Praet N, Speybroeck N, Manzanedo R, Berkvens D, Nforninwe DN,
  Zoli A, Quet F, Preux P-M, Carabin H, Geerts S (2009).
  The disease burden of \emph{Taenia solium} cysticercosis in Cameroon.
  \emph{PLoS Neglected Tropical Diseases 3(3),} e406.
}

\seealso{
  \code{\link{setDALYexample}}\cr

  \code{\link{DALYcalculator}}
  (for a brief description of the \emph{DALY Calculator})\cr
  \code{\link{DALYmanual}}
  (for a more comprehensive overview)
}

\keyword{example}
