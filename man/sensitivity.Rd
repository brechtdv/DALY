\name{sensitivity}
\alias{sensitivity}

\title{Perform DALY sensitivity analysis}

\description{
  This function performs a probabilistic global sensitivity analysis
  of the overall DALY estimate. Three methods are available, i.e.,
  based on mapped regression coefficients, standardized regression
  coefficients, and Spearman's rank correlation coefficients.
  See below for more details.
}

\usage{
sensitivity(x, method = c("regression", "rank"), mapped = TRUE,
            plot = TRUE, main = "Sensitivity analysis", ...)
}

\arguments{
  \item{x}{Object of class \code{'DALY'},
           typically obtained from a call to \code{\link{getDALY}}}
  \item{method}{Sensitivity analysis method: \code{"regression"} or
                \code{"rank"}. See below for details}
  \item{mapped}{Should mapped regression coefficients be presented?
                Defaults to \code{TRUE}}
  \item{plot}{Should a barplot of ranked significant estimates be shown?
              Defaults to \code{TRUE}}
  \item{main}{Main title of the barplot}
  \item{\dots}{Additional arguments to be passed to \code{barplot}}
}

\details{
  Sensitivity analysis studies how the uncertainty in the overall DALY estimate
  can be apportioned to the different sources of uncertainty in the input
  parameters. These results can therefore help to identify those input
  parameters that cause significant uncertainty in the overall DALY estimate
  and that therefore may be the focus of further research if one wishes to
  reduce the uncertainty in the overall estimate.
  
  The \code{sensitivity} function implements a \emph{probabilistic global}
  sensitivity analysis, in which the analysis is conducted over the full range
  of plausible input values (hence \emph{global}), determined by the specified
  uncertainty distributions (hence \emph{probabilistic}).
  
  Specifying \code{method = "regression"} will perform a linear
  regression-based sensitivity analysis. Here, the simulated overall DALY
  estimates will be regressed against the simulated values for the stochastic
  input parameters (using \code{\link{lm}}). To facilitate comparison, the
  independent terms are standardized such that they are normally distributed
  with mean zero and standard deviation one (using \code{\link{scale}}).
  
  If \code{mapped = TRUE}, the dependent term is not standardized, such that
  the resulting \emph{mapped} regression coefficients correspond to the change
  in overall DALY given one standard deviation change in the corresponding
  input parameter. If \code{mapped = FALSE}, the dependent term is
  standardized, such that the resulting \emph{standardized} regression
  coefficients correspond to the number of standard deviations change in
  overall DALY given one standard deviation change in the corresponding input
  parameter.
  
  Specifying \code{method = "rank"} will perform a rank correlation-based
  sensitivity analysis. Here, Spearman's rank correlation coefficients will be
  calculated between the simulated overall DALY estimates and the simulated
  values for each of the stochastic input parameters (using
  \code{\link{cor.test}}).
}

\value{
  If \code{method = "regression"}, the output of \code{\link{summary.lm}};
  
  If \code{method = "rank"}, a matrix with columns for Spearman's rank
  correlation coefficient (\emph{rho}) and corresponding p-value.
}

\references{
  \itemize{
    \item{Vose D (2000).
	      \emph{Risk analysis: a quantitative guide}.
	      John Wiley & Sons.}
  }
}

\author{\email{Brecht.Devleesschauwer@UGent.be}}

\seealso{
  \code{\link{getDALY}}\cr
  \code{\link{lm}}\cr
  \code{\link{cor.test}}
}

\examples{
\dontrun{

##= load NCC example ======================================
setDALYexample(1)

##= perform DALY calculation, store results in 'x' ========
x <- getDALY()

##= perform DALY sensitivity analysis =====================

# mapped regression coefficients
sensitivity(x, method = "regression", mapped = TRUE)

# standardized regression coefficients
sensitivity(x, method = "regression", mapped = FALSE)

# Spearman's rank correlation coefficients
sensitivity(x, method = "rank")
}}

\keyword{output}
\keyword{sensitivity}
