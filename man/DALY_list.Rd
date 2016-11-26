\name{DALY_list}
\alias{DALY_list}
\alias{print.DALY_list}
\alias{plot.DALY_list}

\title{Make list of \sQuote{DALY} objects}

\description{
  This function produces a list of DALY objects and assigns class
  \code{'DALY_list'} to it. Methods are available to print and plot the
  resulting object.
}

\usage{
DALY_list(\dots)

\method{print}{DALY_list}(x, \dots)

\method{plot}{DALY_list}(x, prob = 0.95, sort = TRUE, names = NULL,
     bars = TRUE, col = c("grey90", "white"),
     error_bars = TRUE, eb_col = "black",
     grid = TRUE, \dots)
}

\arguments{
  \item{\dots}{
  In \code{DALY_list}: Multiple objects of class \code{'DALY'}; \cr
  In \code{print.DALY_list}: Additional arguments to be passed to \code{\link{print.DALY}}; \cr
  In \code{plot.DALY_list}: Additional arguments to be passed to \code{\link{plot.DALY}}}

  \item{x }{Object of class \code{'DALY_list'}}
  \item{prob }{Probability span by DALY error bar;
              defaults to \code{0.95}}
  \item{sort }{Should values be sorted from high to low?
               defaults to \code{TRUE}}
  \item{names }{Vector of names to be plotted on the y-axis. If this argument
                is \code{NULL}, the default, then the names are taken from the
                \code{name} element of the constituting \code{DALY} objects}
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
}

\value{
  An object of S3 class \code{'DALY_list'}.
}

\author{\email{brechtdv@gmail.com}}

\seealso{
  \code{\link{getDALY}}\cr
  \code{\link{print.DALY}}\cr
  \code{\link{plot.DALY}}\cr
  \code{\link{scatterplot}}\cr

  \code{\link{DALYcalculator}}
  (for a brief description of the \emph{DALY Calculator})\cr
  \code{\link{DALYmanual}}
  (for a more comprehensive overview)
}

\examples{
\dontrun{

##= load NCC example ======================================
setDALYexample(1)
setStdLE("GBD1990")

##= calculate DALYs under different scenarios =============
ncc_00 <- getDALY(aw = FALSE, dr = 0)
ncc_03 <- getDALY(aw = FALSE, dr = 0.03)
ncc_13 <- getDALY(aw = TRUE,  dr = 0.03)

##= store results as 'DALY_list' ==========================
ncc <- DALY_list(ncc_00, ncc_03, ncc_13)

##= barplot of different scenarios ========================
par(mar = c(4, 4, 1, 1) + .5)
plot(ncc, names = c("DALY[0,0]", "DALY[0,0.03]", "DALY[1,0.03]"))
}}

\keyword{output}
\keyword{methods}
