\name{setStdLE}
\alias{setStdLE}

\title{Set the \sQuote{Life Expectancy} table to a standard life expectancy table}

\description{
  This function sets the \sQuote{Life Expectancy} table to a standard life expectancy for DALY calculation. The default standard life expectancy table is the synthetic life table introduced for the GBD 2010 study. Other options include the Coale and Demeny model life table West (introduced for GBD 1990 study), and the frontier life expectancy table used for the WHO Global Health Estimates.
}

\usage{setStdLE(table = NULL)}

\arguments{
  \item{table}{The required standard life expectancy table -- i.e., one of: \code{c("GBD2010", "GBD1990", "WHO/GHE")}; when set to \code{NULL}, the value is taken from the GUI.}
}

\references{
  \itemize{
        \item{Devleesschauwer B, et al. (2014). Calculating Disability-Adjusted
          Life Years to quantify burden of disease. \emph{International Journal
          of Public Health 59,} 565-569. \url{http://dx.doi.org/10.1007/s00038-014-0552-z}}
  }
}

\details{
The DALY Calculator provides three standard life expectancy tables:
\enumerate{
  \item \strong{GBD1990}: the Coale and Demeny model life-table West, level 26 and 25, which has a life expectancy at birth of 80 for males and 82.5 for females (Murray, 1994);
  \item \strong{GBD2010} (\emph{default}): the synthetic standard life expectancy introduced for the GBD 2010 study, with a life expectancy at birth of 86 for both males and females;
  \item \strong{WHO/GHE}: the projected frontier life expectancy for the year 2050, used for the WHO Global Health Estimates, with a life expectancy at birth of 92 for both males and females.
}
}

\author{\email{brechtdv@gmail.com}}

\seealso{
  \code{\link{setLifeExp}}\cr
  
  \code{\link{DALYcalculator}}
  (for a brief description of the \emph{DALY Calculator})\cr
  \code{\link{DALYmanual}}
  (for a more comprehensive overview)
}
