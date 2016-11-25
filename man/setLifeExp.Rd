\name{setLifeExp}
\alias{setLifeExp}

\title{Open the \sQuote{Life Expectancy} window}

\description{
 This function opens the \sQuote{Life Expectancy} window, where the life expectancy table can be customized. The default life expectancy table is the GBD2010 standard life expectancy table. Alternative standard life expectancy tables may be selected via the \sQuote{Life Expectancy} window or via function \code{\link{setStdLE}}.
}

\usage{setLifeExp()}

\author{\email{brechtdv@gmail.com}}

\details{
The DALY Calculator provides three standard life expectancy tables:
\enumerate{
  \item \strong{GBD1990}: the Coale and Demeny model life-table West, level 26 and 25, which has a life expectancy at birth of 80 for males and 82.5 for females (Murray, 1994);
  \item \strong{GBD2010} (\emph{default}): the synthetic standard life expectancy introduced for the GBD 2010 study, with a life expectancy at birth of 86 for both males and females;
  \item \strong{WHO/GHE}: the projected frontier life expectancy for the year 2050, used for the WHO Global Health Estimates, with a life expectancy at birth of 92 for both males and females.
}
}

\seealso{
  \code{\link{setStdLE}}\cr\cr
  \code{\link{DALYcalculator}}
  (for a brief description of the \emph{DALY Calculator})\cr
  \code{\link{DALYmanual}}
  (for a more comprehensive overview)
}
