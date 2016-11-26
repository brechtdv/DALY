\name{getDALY}
\alias{getDALY}

\title{Initiate the DALY calculation process}

\description{
  This function initiates the Monte Carlo DALY calculation process,
  based on the entered data, and returns the simulated results.
}

\usage{getDALY(button.call = FALSE, aw, dr)}

\arguments{
  \item{button.call}{Gives information on the origin of the call
  (i.e., through the GUI button (\code{TRUE}) or the \R Console (\code{FALSE}))}
  \item{aw}{Logical flag, indicating if age weighting should be applied;
            if \code{aw} is not specified, the value from the GUI will be used}
  \item{dr}{Time discount rate (numeric value between 0 and 1);
            if \code{dr} is not specified, the value from the GUI will be used}
}

\value{
  \code{getDALY} returns an object of class \code{'DALY'},
    which inherits from class \code{'list'}.\cr
  
  The generic functions \code{print} and \code{summary} may be used to
    obtain and print the DALY calculation results.
  The generic function \code{aggregate} extracts from the \code{'DALY'} object
    results by outcome, age/sex class, or both.\cr

  An object of class \code{'DALY'} is a list containing the following elements:
  \item{\emph{i}}{For each outcome \emph{\code{i}},
                  an unnamed list containing simulated results (see next paragraph)}
  \item{pop}{Population}
  \item{name}{Name of the disease}
  
  For each outcome \emph{\code{i}}, \code{DALY[[}\emph{\code{i}}\code{]]} is a list
    containing the following elements:
  \item{DALY}{Vector of simulated DALYs}
  \item{YLD}{Vector of simulated YLDs}
  \item{YLL}{Vector of simulated YLLs}
  \item{INC}{Vector of simulated incident cases}
  \item{MRT}{Vector of simulated deaths}
  \item{name}{Name of the health outcome}
  
  The vectors containing simulated DALYs, YLDs, YLLs, incident cases and
    deaths each have three dimensions:
  \item{1}{Iterations}
  \item{2}{Age groups}
  \item{3}{Sex}
}

\author{\email{brechtdv@gmail.com}}

\seealso{
  The numerical output methods for \code{'DALY'}: \code{\link{print.DALY}}, \code{\link{summary.DALY}}\cr
  The graphical output method for \code{'DALY'}: \code{\link{hist.DALY}}\cr
  The aggregator method for \code{'DALY'}: \code{\link{aggregate.DALY}}\cr
  DALY sensitivity analysis : \code{\link{sensitivity}}\cr\cr
  \code{\link{DALYcalculator}}
  (for a brief description of the \emph{DALY Calculator})\cr
  \code{\link{DALYmanual}}
  (for a more comprehensive overview)
}

\examples{
\dontrun{

##= load the NCC example ==================================
setDALYexample(1)
setStdLE("GBD1990")

##= perform DALY calculation  =============================
##= without age weighting and time discounting ============
getDALY(aw = FALSE, dr = 0)

##= perform DALY calculation, store results in 'x' ========
##= (with age weighting and a 3\% time discount rate) ======
x <- getDALY(aw = TRUE, dr = 0.03)

##= view the structure of 'x' =============================
str(x)

##= view the DALY calculation results =====================
print(x)                   # absolute, total ==============
print(x, relative = TRUE)  # relative (ie, per 1000 pop) ==
print(x, outcomes = TRUE)  # outcome-wise =================

##= obtain minimum & maximum simulated DALY ===============
min(x[[1]]$DALY)
max(x[[1]]$DALY)

##= standardized DALY histogram ===========================
hist(x)

##= DALY sensitivity analysis =============================
sensitivity(x)
}}

\keyword{output}
