\name{setStdLE}
\alias{setStdLE}

\title{Set the \sQuote{Life Expectancy} table to the standard life expectancy table}

\description{
  This function sets the \sQuote{Life Expectancy} table to the
  standard life expectancy for DALY calculation,
  i.e., the Coale and Demeny model life table West, level 25 and 26,
  which has a maximum life expectancy of 80 years for males
  and 82.5 years for females.
}

\usage{setStdLE()}

\references{
  \itemize{
    \item{Murray CJL (1994). Quantifying the burden of disease:
          the technical basis for disability-adjusted life years.
          \emph{Bulletin of the World Health Organization 72(3),} 429-445.}
  }
}

\author{\email{Brecht.Devleesschauwer@UGent.be}}

\seealso{
  \code{\link{setLifeExp}}\cr
  
  \code{\link{DALYcalculator}}
  (for a brief description of the \emph{DALY Calculator})\cr
  \code{\link{DALYmanual}}
  (for a more comprehensive overview)
}
