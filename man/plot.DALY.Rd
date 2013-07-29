\name{plot.DALY}
\alias{plot.DALY}

\title{Plot method for class 'DALY'}

\description{
  Plot a stacked barplot of YLLs/YLDs with a DALY error bar.
}

\usage{
\method{plot}{DALY}(x, prob = 0.95, sort = TRUE, names = NULL,
     bars = TRUE, col = c("grey90", "white"),
     error_bars = TRUE, eb_col = "black",
     grid = TRUE, \dots)
}

\arguments{
  \item{x }{Object of class \code{'DALY'}}
  \item{prob }{Probability span by DALY error bar;
              defaults to \code{0.95}}
  \item{sort }{Should values be sorted from high to low?
               defaults to \code{TRUE}}
  \item{names }{Vector of names to be plotted on the y-axis. If this argument
                is \code{NULL}, the default, then the names are taken from the
                \code{name} elements of the outcomes}
  \item{bars }{Should YLL/YLD bars be plotted?
               defaults to \code{TRUE}}
  \item{col }{Color of YLL and YLD barplot;
              defaults to \code{c("grey90", "white")}}
  \item{error_bars }{Should DALY error bars be plotted?
               defaults to \code{TRUE}}
  \item{eb_col }{Color of DALY error bars;
                 defaults to black}
  \item{grid }{Should a vertical grid be plotted?
               defaults to \code{TRUE}}
  \item{\dots }{Additional arguments to be passed to \code{\link{barplot}}}
}

\details{
  This function plots the results of the DALY calculation aggregated by
  outcome. By default, both a stacked barplot of YLLs/YLDs and an error bar
  of total DALYs are shown.
}

\author{\email{Brecht.Devleesschauwer@UGent.be}}

\seealso{
  \code{\link{hist.DALY}}\cr

  \code{\link{DALYcalculator}}
  (for a brief description of the \emph{DALY Calculator})\cr
  \code{\link{DALYmanual}}
  (for a more comprehensive overview)
}

\examples{
\dontrun{

##= load toxoplasmosis example ============================
setDALYexample(2)

##= perform DALY calculation, store results in 'x'  =======
x <- getDALY()

##= plot results ==========================================
par(mar = c(4, 7, 4, 1))
plot(x)

plot(x, bars = FALSE)        # only error bars
plot(x, error_bars = FALSE)  # only barplots
}}

\keyword{output}
