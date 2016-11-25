\name{readDALYdata}
\alias{readDALYdata}

\title{Read in DALY data stored in an .RData image file}

\description{
  This function loads data from an .RData image file into the
  \emph{DALY Calculator}. The file can be an external file, previously saved
  through \code{saveDALYdata()}, or one of the two built-in example datasets.
}

\usage{
  readDALYdata(file = NULL, example = NULL)
}

\arguments{
  \item{file}{Character string, denoting the name of the .RData file}
  \item{example}{Integer, denoting the example to be loaded
                 (any of \code{c(1, 2)})}
}

\details{NA}

\author{\email{brechtdv@gmail.com}}

\seealso{
  \code{\link{saveDALYdata}},
  \code{\link{setDALYexample}},
  \code{\link{DALY_Neurocysticercosis}},
  \code{\link{DALY_Toxoplasmosis}}\cr
  
  \code{\link{DALYcalculator}}
  (for a brief description of the \emph{DALY Calculator})\cr
  \code{\link{DALYmanual}}
  (for a more comprehensive overview)
}

\keyword{import/export}
\keyword{example}
