\name{setDALYexample}
\alias{setDALYexample}

\title{Load one of the two built-in DALY calculation examples}

\description{
  This function resets the \emph{DALY Calculator}
  (through \code{\link{reset}}),
  and loads one of the two built-in DALY calculation examples
  (through \code{\link{readDALYdata}}):
  \enumerate{
    \item{Neurocysticercosis in West-Cameroon}
    \item{Congenital Toxoplasmosis in the Netherlands}
  }
}

\usage{setDALYexample(example)}

\arguments{
  \item{example}{This argument takes value \sQuote{\code{1}} for the
                 Neurocysticercosis example, and value \sQuote{\code{2}} for the
				 Toxoplasmosis example}
}

\author{\email{Brecht.Devleesschauwer@UGent.be}}

\seealso{
  \code{\link{DALY_Neurocysticercosis}},
  \code{\link{DALY_Toxoplasmosis}},
  \code{\link{readDALYdata}}\cr\cr
  \code{\link{DALYcalculator}}
  (for a brief description of the \emph{DALY Calculator})\cr
  \code{\link{DALYmanual}}
  (for a more comprehensive overview)
}
