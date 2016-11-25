\name{summary.DALY}
\alias{summary.DALY}

\title{Summary method for class 'DALY'}

\description{
  Print summary of \emph{DALY Calculator} output per age/sex class.
}

\usage{
\method{summary}{DALY}(object, relative = FALSE, outcomes = FALSE,
        digits = 0, \dots)
}

\arguments{
  \item{object }{Object of class \code{'DALY'}}
  \item{relative }{Show results per 1000 population? See details below}
  \item{outcomes }{Show results per outcome? See details below}
  \item{digits }{Number of decimals to be printed; defaults to \code{0}}
  \item{\dots }{Additional arguments to be passed to \code{summary}}
}

\details{
  The standard \code{summary} method gives the \emph{absolute} number of
  DALYs, YLDs, YLLs, cases and deaths. By specifying \code{relative = TRUE},
  the number of DALYs, YLDs, YLLs, cases and deaths per 1,000 population will
  be shown.\cr

  The standard \code{summary} method gives the number of DALYs, YLDs, YLLs,
  cases and deaths \emph{aggregated over all health outcomes}. By specifying
  \code{outcomes = TRUE}, the number of DALYs, YLDs, YLLs, cases and deaths
  for each outcome will be shown as well.\cr
}

\value{
  \code{summary.DALY} returns a list containg the following elements:
  \item{total }{Matrix containing mean, median and \code{prob}
                credible interval of total DALYs, YLDs, YLLs, cases and deaths}
  \item{outcomes }{Named list, containing matrices of DALYs, YLDs, YLLs,
                   cases and deaths for each health outcome}
  \item{pct }{Vector of contribution of YLDs and YLLs to overall DALYs}
}

\author{\email{brechtdv@gmail.com}}

\seealso{
  \code{\link{print.DALY}},
  \code{\link{aggregate.DALY}}\cr
  
  \code{\link{DALYcalculator}}
  (for a brief description of the \emph{DALY Calculator})\cr
  \code{\link{DALYmanual}}
  (for a more comprehensive overview)
}

\examples{
\dontrun{

##= load the NCC example ==================================
setDALYexample(1)

##= perform DALY calculation, store results in 'x' ========
x <- getDALY()

##= view the DALY calculation results =====================
summary(x)                   # absolute, total ============
summary(x, relative = TRUE)  # relative (ie, per 1000) ====
summary(x, outcomes = TRUE)  # outcome-wise ===============
}}

\keyword{output}
