\name{aggregate.DALY}
\alias{aggregate.DALY}

\title{Aggregate method for class 'DALY'}

\description{
  Aggregate \emph{DALY Calculator} output by outcome, age/sex class, or both.
}

\usage{
\method{aggregate}{DALY}(x, by = c("total", "class", "outcome"), \dots)
}

\arguments{
  \item{x }{Object of class \code{'DALY'}}
  \item{by }{Any of \code{c("total", "outcome", "class")}}
  \item{\dots }{Additional arguments to be passed to \code{aggregate}}
}

\value{
  For \code{by = "total"}:
  \item{DALY }{Vector of DALYs aggregated by outcome and age/sex class}
  \item{YLD }{Vector of YLDs aggregated by outcome and age/sex class}
  \item{YLL }{Vector of YLLs aggregated by outcome and age/sex class}
  \item{Cases }{Vector of cases aggregated by outcome and age/sex class}
  \item{Deaths }{Vector of deaths aggregated by outcome and age/sex class}
  \item{pop }{Population matrix}
  \item{name }{Name of the disease}
  
  For \code{by = "class"}:
  \item{DALY }{Vector of DALYs aggregated by age/sex class}
  \item{YLD }{Vector of YLDs aggregated by age/sex class}
  \item{YLL }{Vector of YLLs aggregated by age/sex class}
  \item{Cases }{Vector of cases aggregated by age/sex class}
  \item{Deaths }{Vector of deaths aggregated by age/sex class}
  \item{pop }{Population matrix}
  \item{name }{Name of the disease}
  
  For \code{by = "outcome"}:
  \item{\emph{i} }{List containing vectors of DALYs, YLDs, YLLs, cases and
                   deaths for each outcome \emph{\code{i}}}
  \item{pop }{Population matrix}
  \item{name }{Name of the disease}
}

\author{\email{Brecht.Devleesschauwer@UGent.be}}

\seealso{
  \code{\link{print.DALY}},
  \code{\link{summary.DALY}}\cr
  
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

##= store simulations aggregated by =======================
##= outcome and age/sex class     =========================
y <- aggregate(x, by = "total")

summary(y$DALY)  # default summary of total DALYs =========
hist(y$deaths)   # default histogram of total deaths ======
}}

\keyword{output}
