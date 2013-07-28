\name{print.DALY}
\alias{print.DALY}

\title{Print method for class 'DALY'}

\description{
  Print summary of \emph{DALY Calculator} output aggregated by age/sex class.
}

\usage{
\method{print}{DALY}(x, relative = FALSE, outcomes = FALSE,
     prob = 0.95, digits = 0, \dots)
}

\arguments{
  \item{x }{Object of class \code{'DALY'}}
  \item{relative }{Show results per 1000 population? See details below}
  \item{outcomes }{Show results per outcome? See details below}
  \item{prob }{Probability span by printed credibility intervals;
               defaults to \code{0.95}}
  \item{digits }{Number of decimals to be printed; defaults to \code{0}}
  \item{\dots }{Additional arguments to be passed to \code{print}}
}

\details{
  The standard \code{print} method gives the absolute number of DALYs, YLDs,
  YLLs, Cases and Deaths. By specifying \code{relative = TRUE}, the number of
  DALYs, YLDs, YLLs, cases and deaths per 1,000 population will be shown.\cr

  The standard \code{print} method gives the number of DALYs, YLDs, YLLs,
  cases and deaths aggregated over all health outcomes. By specifying
  \code{outcomes = TRUE}, the number of DALYs, YLDs, YLLs, cases and deaths
  for each outcome will be shown as well.\cr
}

\value{
  \code{print.DALY} returns a list containg the following elements:
  \item{total }{Matrix containing mean, median and \code{prob}
                credible interval of total DALYs, YLDs, YLLs, cases and deaths}
  \item{outcomes }{Named list, containing matrices of DALYs, YLDs, YLLs,
                   cases and deaths for each health outcome}
  \item{pct }{Vector of contribution of YLDs and YLLs to overall DALYs}
}

\author{\email{Brecht.Devleesschauwer@UGent.be}}

\seealso{
  \code{\link{summary.DALY}},
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
print(x)                   # absolute, total ==============
print(x, prob = .90)       # absolute, total (90\% CrI) ====
print(x, relative = TRUE)  # relative (ie, per 1000 pop) ==
print(x, outcomes = TRUE)  # outcome-wise =================
}}

\keyword{output}
