\name{DALY_Toxoplasmosis}
\alias{DALY_Toxoplasmosis}

\docType{data}

\title{Example Dataset 2: Congenital toxoplasmosis in the Netherlands}

\description{
  The \code{Toxoplasmosis} dataset consists of a nested list
  containing data needed to calculate DALYs lost due to
  congenital toxoplasmosis in the Netherlands, according to Kortbeek et al. (2009).
}

\usage{data(DALY_Toxoplasmosis)}

\format{
  \code{List of 3} \cr\cr
    \code{$ model : List of 2} \cr
      \code{..$ diseaseName : chr "Toxoplasmosis"} \cr
      \code{..$ outcomeNames : List of 8} \cr\cr
    \code{$ settings : List of 4} \cr
      \code{..$ pop : num [1:5, 1:2] 97000 NA NA NA NA 97000 NA NA NA NA} \cr
      \code{..$ LE : num [1:21, 1:2] 79 0 0 0 0 0 0 0 0 0 ...} \cr
      \code{..$ aw : chr "No"} \cr
      \code{..$ dr : num 0} \cr\cr
    \code{$ data : List of 8} \cr
      \code{..$ : List of 8} \cr
        \code{.. ..$ inc : List of 3} \cr
          \code{.. .. ..$ dist : chr "Beta-Pert"} \cr
          \code{.. .. ..$ strat : chr "Age"} \cr
          \code{.. .. ..$ param : logi [1:5, 1:3] NA NA NA NA NA NA ...} \cr
        \code{.. ..$ trt : List of 3} \cr
          \code{.. .. ..$ dist : chr "Fixed"} \cr
          \code{.. .. ..$ strat : chr "Age"} \cr
          \code{.. .. ..$ param : logi [1:5, 1] NA NA NA NA NA} \cr
        \code{.. ..$ ons : List of 3} \cr
          \code{.. .. ..$ dist : chr "Fixed"} \cr
          \code{.. .. ..$ strat : chr "Age"} \cr
          \code{.. .. ..$ param : logi [1:5, 1] NA NA NA NA NA} \cr
        \code{.. ..$ dur : List of 3} \cr
          \code{.. .. ..$ dist : chr "Fixed"} \cr
          \code{.. .. ..$ strat : chr "Age"} \cr
          \code{.. .. ..$ param : logi [1:5, 1] NA NA NA NA NA} \cr
        \code{.. ..$ DWt : List of 3} \cr
          \code{.. .. ..$ dist : chr "Fixed"} \cr
          \code{.. .. ..$ strat : chr "Age"} \cr
          \code{.. .. ..$ param : logi [1:5, 1] NA NA NA NA NA} \cr
        \code{.. ..$ DWn : List of 3} \cr
          \code{.. .. ..$ dist : chr "Fixed"} \cr
          \code{.. .. ..$ strat : chr "Age"} \cr
          \code{.. .. ..$ param : logi [1:5, 1] NA NA NA NA NA} \cr
        \code{.. ..$ mrt : List of 3} \cr
          \code{.. .. ..$ dist : chr "Beta-Pert"} \cr
          \code{.. .. ..$ strat : chr "Age"} \cr
          \code{.. .. ..$ param : num [1:5, 1:3] 0.0323 NA NA NA NA ...} \cr
        \code{.. ..$ lxp : List of 3} \cr
          \code{.. .. ..$ dist : chr "Fixed"} \cr
          \code{.. .. ..$ strat : chr "Age"} \cr
          \code{.. .. ..$ param : num [1:5, 1] 0 NA NA NA NA} \cr\cr
      \code{[...]}
}

\details{
  The calculation of disability-adjusted life years (DALYs) enables public
  health policy makers to compare the burden of disease of a specific disease
  with that of other (infectious) diseases. The incidence of a disease is
  important for the calculation of DALYs. To estimate the incidence of
  congenital toxoplasmosis (CT), a random sample of 10,008 dried blood spot
  filter paper cards from babies born in 2006 in the Netherlands were tested
  for \emph{Toxoplasma gondii}-specific IgM antibodies. Eighteen samples were
  confirmed as positive for IgM, resulting in an observed birth incidence of
  CT of 1.8 cases per 1,000 live-born children in 2006 and an adjusted
  incidence of 2.0 cases per 1,000. This means that 388 infected children were
  born in 2006. The most likely burden of disease is estimated to be 2,300
  DALYs (range 820-6,710 DALYs). In the previous calculations, using data from
  a regional study from 1987, this estimate was 620 DALYs (range 220-1,900
  DALYs). The incidence of CT in the Netherlands is much higher than
  previously reported; it is 10 times higher than in Denmark and 20 times
  higher than in Ireland, based on estimates obtained using the same methods.
  There is no screening program in the Netherlands; most children will be born
  asymptomatic and therefore will not be detected or treated.
}

\source{
  Kortbeek LM, Hofhuis A, Nijhuis CDM, Havelaar AH (2009).
  Congenital toxoplasmosis and DALYs in the Netherlands.
  \emph{Memorias de Instituto Oswaldo Cruz 104(2),} 370-373.
}

\seealso{
  \code{\link{setDALYexample}}\cr

  \code{\link{DALYcalculator}}
  (for a brief description of the \emph{DALY Calculator})\cr
  \code{\link{DALYmanual}}
  (for a more comprehensive overview)
}

\keyword{example}
