\name{scatterplot}
\alias{scatterplot}
\alias{scatterplot.DALY}
\alias{scatterplot.DALY_list}

\title{Scatterplot method for class 'DALY' and 'DALY_list'}

\description{
  Generate a scatterplot of population versus patient level burden. The method
  is available for objects of class 'DALY' and 'DALY_list'.
}

\usage{
\method{scatterplot}{DALY}(x, plot = c("DALY", "YLD", "YLL"), outcomes = TRUE,
            per = 1000, samples = 1000, pch = 16, col = NULL, legend = NULL,
            legend_pos = c("topright", "topleft", "bottomright", "bottomleft"), \dots)

\method{scatterplot}{DALY_list}(x, plot = c("DALY", "YLD", "YLL"),
            per = 1000, samples = 1000, pch = 16, col = NULL, legend = NULL,
            legend_pos = c("topright", "topleft", "bottomright", "bottomleft"), \dots)
}

\arguments{
  \item{x }{Object of class \code{'DALY'} or \code{'DALY_list'}}
  \item{plot }{Which estimates should be plotted? Must be either \code{"DALY"},
              \code{"YLD"} or \code{"YLL"}}
  \item{outcomes }{Should different outcomes be plotted? Defaults to \code{TRUE}.
                   If \code{FALSE}, aggregated estimates are plotted}
  \item{per }{Denominator for population level burden. Defaults to 1000}
  \item{samples }{Number of samples to plotted. Defaults to 1000}
  \item{pch }{Plotting symbol. Defaults to 16 (= solid circle).
              See \code{\link{pch}} for more info}
  \item{col }{Plotting color. Defaults to \code{NULL}, in which case the
              function tries to generate distinctive rainbow colors}
  \item{legend }{A character or \code{\link{expression}} vector of length
                 \eqn{\ge 1} to appear in the legend. Defaults to \code{NULL},
                 in which case the legend names are derived from the plotted
                 objects; if \code{FALSE}, no legend is plotted}
  \item{legend_pos }{The position of the legend, either \code{"topright"},
                     \code{"topleft"}, \code{"bottomright"} or
                     \code{"bottomleft"}}
  \item{\dots }{Additional arguments to be passed to \code{\link{plot}}}
}

\details{
  This function generates a scatterplot of the estimated burden at the
  population level (x-axis) versus the patient level (y-axis). Scatterplots can
  be generated of \code{'DALY'} objects, to visualize the overall or
  outcome-specific burden; and of \code{'DALY_list'} objects, to compare the
  overall burden of, e.g., different diseases or different DALY calculation
  scenarios.
}

\author{\email{Brecht.Devleesschauwer@UGent.be}}

\seealso{
  \code{\link{DALY_list}}\cr

  \code{\link{plot.DALY}}, \code{\link{hist.DALY}}\cr

  \code{\link{DALYcalculator}}
  (for a brief description of the \emph{DALY Calculator})\cr
  \code{\link{DALYmanual}}
  (for a more comprehensive overview)
}

\examples{
\dontrun{

##= load toxoplasmosis example ============================
setDALYexample(2)

##= perform DALY calculation for different scenarios ======
toxo_00 <- getDALY(aw = FALSE, dr = 0)
toxo_03 <- getDALY(aw = FALSE, dr = 0.03)
toxo_13 <- getDALY(aw = TRUE,  dr = 0.03)

##= combine scenarios in 'DALY_list' ======================
toxo <- DALY_list(toxo_00, toxo_03, toxo_13)

##= plot YLL and YLL for toxo_00 ==========================
par(mar = c(4, 4, 1, 1) + .5)
par(mfrow = c(1, 2))
scatterplot(toxo_00, plot = "YLL",
            outcomes = FALSE, legend = FALSE)
scatterplot(toxo_00, plot = "YLD",
            outcomes = FALSE, legend = FALSE)

##= plot all three scenarios ==============================
par(mfrow = c(1, 1))
scatterplot(toxo,
            legend = c("DALY{0,0}",
                       "DALY{0,0.03}",
                       "DALY{1,0.03}"),
            legend_pos = "topleft",
            log = "xy",
            main = "Scenario analysis")
}}

\keyword{output}
