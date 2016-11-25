\name{DALYoptions}
\alias{DALYoptions}

\title{Open the \sQuote{Options} window}

\description{
  This function opens the \sQuote{Options} window of the \emph{DALY Calculator}.
}

\usage{DALYoptions()}

\details{
  The following settings can be modified in the \sQuote{options} window:
  \itemize{
    \item{\strong{Iterations:} the default number of iterations is 20,000}
    \item{\strong{Numerical output:} the standard output contains a summary of
          DALYs, YLDs, YLLs, incident cases and deaths. Different options are available:
	\itemize{
	  \item{\dQuote{Summed over age/sex classes} OR \dQuote{Per age/sex class}}
	  \item{\dQuote{Summed over outcomes} OR \dQuote{Per outcome}}
	  \item{\dQuote{Absolute} OR \dQuote{Relative (per 1000 pop)}}
	}}
    \item{\strong{Graphical output:} the user can specficy whether or not to view a
		  standardized DALY histogram (see \code{\link{hist.DALY}}).}
  }
}

\author{\email{brechtdv@gmail.com}}

\seealso{
  The numerical output methods for class \code{'DALY'}: \code{\link{print.DALY}}, \code{\link{summary.DALY}}\cr
  The graphical output method for class \code{'DALY'}: \code{\link{hist.DALY}}\cr
  The aggregator method for class \code{'DALY'}: \code{\link{aggregate.DALY}}\cr\cr
  \code{\link{DALYcalculator}}
  (for a brief description of the \emph{DALY Calculator})\cr
  \code{\link{DALYmanual}}
  (for a more comprehensive overview)
}
