\name{hist.DALY}
\alias{hist.DALY}

\title{Histogram method for class 'DALY'}

\description{
  Plot a standardized histogram of \emph{DALY Calculator} output.
}

\usage{
\method{hist}{DALY}(x, xval = c("DALY", "YLD", "YLL", "cases", "deaths"),
     prob = 0.95, central = c("mean", "median"),
     breaks = 25, fill = "grey95", \dots)
}

\arguments{
  \item{x }{Object of class \code{'DALY'}}
  \item{xval }{Value to plot on x-axis;
               must be any of \code{c("DALY", "YLD", "YLL", "cases", "deaths")}}
  \item{prob }{Probability span by plotted credibility interval;
              defaults to \code{0.95}}
  \item{central }{Plotted central value of distribution;
                 must be any of \code{c("mean", "median")}}
  \item{breaks }{See \code{\link{hist}}; the default is 25 bars}
  \item{fill }{The colour to be used to fill the bars}
  \item{\dots }{Additional arguments to be passed to \code{\link{gpar}}}
}

\details{
  This function plots a standardized histogram of \emph{DALY Calculator}
  output. The histogram contains by default 25 bars. The limits of the
  credible interval are denoted by vertical lines; the credible interval
  limits and central tendency are printed above the histogram.
}

\author{\email{Brecht.Devleesschauwer@UGent.be}}

\seealso{
  \code{\link{aggregate.DALY}}\cr
  
  \code{\link{DALYcalculator}}
  (for a brief description of the \emph{DALY Calculator})\cr
  \code{\link{DALYmanual}}
  (for a more comprehensive overview)
}

\examples{
\dontrun{

##= load NCC example ======================================
setDALYexample(1)

##= perform DALY calculation, store results in 'x'  =======
x <- getDALY()

##= plot histogram of total deaths ========================
##= + show 90\% credible interval & median =================
hist(x, xval = "deaths", prob = 0.90, central = "median")
}}

\keyword{output}
