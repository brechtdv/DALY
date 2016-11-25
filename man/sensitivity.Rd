\name{sensitivity}
\alias{sensitivity}
\alias{print.DALY_sensitivity}
\alias{plot.DALY_sensitivity}

\title{DALY sensitivity analysis}

\description{
  This function performs a probabilistic global sensitivity analysis of the
  overall DALY estimate, based on standardized regression coefficients
  (method \code{src}) or partial correlation coefficients (method \code{pcc}).
  Actual or ranked values may be used, and for method \code{src}, coefficients
  or mapped values may be reported. See below for more details.
}

\usage{
sensitivity(x, method = c("src", "pcc"), rank = FALSE, mapped = TRUE)

\method{print}{DALY_sensitivity}(x, digits = 3, signif_stars = getOption("show.signif.stars"), \dots)

\method{plot}{DALY_sensitivity}(x, alpha = 0.05, main = "Sensitivity analysis",
     show_values = FALSE, value_digits = 3, value_cex = 0.6, \dots)
}

\arguments{
  \item{x}{Object of class \code{'DALY'},
           typically obtained from a call to \code{\link{getDALY}}}
  \item{method}{Sensitivity analysis method: \code{"src"} or \code{"pcc"}.
                See below for details}
  \item{rank}{Should ranked values be used? Defaults to \code{FALSE}}
  \item{mapped}{Should mapped regression coefficients be presented?
                Defaults to \code{TRUE}}

  \item{digits }{Number of decimals to be printed; defaults to \code{0}}
  \item{signif_stars }{Should significance stars be printed?}

  \item{alpha }{Significance level for selecting significant variables; defaults to 0.05}
  \item{main }{Main title of tornado plot; defaults to "Sensitivity analysis"}
  \item{show_values }{Should values be plotted next to bars?; defaults to FALSE}
  \item{value_digits }{Number of significant digits for plotted values; defaults to 3}
  \item{value_cex }{Size of plotted values; defaults to 0.6}
  
  \item{\ldots }{Additional arguments to be passed to methods}
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
  
  Specifying \code{method = "src"} will perform a linear regression-based
  sensitivity analysis. Here, the simulated overall DALY estimates will be
  regressed against the simulated values for the stochastic input parameters
  (using \code{\link{lm}}). To facilitate comparison, the independent terms
  are standardized such that they are normally distributed with mean zero and
  standard deviation one (using \code{\link{scale}}). The resulting regression
  coefficients are therefore referred to as \emph{standardized} regression
  coefficients.
  
  Argument \code{rank} specifies whether the regression should be performed on
  the actual values (\code{rank = FALSE}; default) or on the ranked values
  (\code{rank = TRUE}). Rank-based regression may be preferred when the
  relation between output and inputs is non-linear. R^2 values smaller than
  0.60 may be indicative of a poor fit of the default linear regression model.
  
  If \code{mapped = TRUE}, the dependent term is not standardized, such that
  the resulting \emph{mapped} regression coefficients correspond to the change
  in overall DALY given one standard deviation change in the corresponding
  input parameter. If \code{mapped = FALSE}, the dependent term is
  standardized, such that the resulting \emph{standardized} regression
  coefficients correspond to the number of standard deviations change in
  overall DALY given one standard deviation change in the corresponding input
  parameter.
  
  Specifying \code{method = "pcc"} will calculate partial correlation
  coefficients for each of the input variables. Partial correlation
  coefficients represent the correlation between two variables when adjusting
  for other variables. In the presence of important interactions between input
  variables, partial correlation coefficients may be preferred over
  standardized regression coefficients.

  Argument \code{rank} specifies whether the correlation should be calculated
  between the actual values (\code{rank = FALSE}; default) or between the
  ranked values (\code{rank = TRUE}).
  
  
  Method \code{plot} can be used to generate a tornado plot of the significant
  input variables.
}

\value{
  An object of S3 class \code{DALY_sensitivity}, containing the following two
  elements:\cr
  
  \item{method}{List containing the specified values for \code{method},
               \code{rank} and \code{mapped}}
  \item{out}{Either the output of \code{\link{summary.lm}} (for method
	         \code{src}), or a matrix with columns for the partial
			 correlation coefficients (\emph{rho}) and corresponding p-values
			 (for method \code{pcc}).}
}

\references{
  \itemize{
    \item{Vose D (2000).
	      \emph{Risk analysis: a quantitative guide}.
	      John Wiley & Sons.}
    \item{Saltelli A, Chan K, Scott EM (2000).
	      \emph{Sensitivity analysis}.
	      John Wiley & Sons.}
  }
}

\author{\email{brechtdv@gmail.com}}

\seealso{
  Package \pkg{sensitivity} (\url{https://cran.r-project.org/package=sensitivity}), for more advanced sensitivity analysis methods.\cr
  
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

## default sensitivity analysis
sa <- sensitivity(x)
print(sa)

## tornado plot of default sensitivity analysis
plot(sa, show_values = TRUE)

## standardized regression coefficients -------------------
sensitivity(x, method = "src", rank = FALSE, mapped = TRUE)
sensitivity(x, method = "src", rank = FALSE, mapped = FALSE)

## standardized rank regression coefficients --------------
sensitivity(x, method = "src", rank = TRUE, mapped = TRUE)
sensitivity(x, method = "src", rank = TRUE, mapped = FALSE)

## partial correlation coefficients -----------------------
sensitivity(x, method = "pcc", rank = FALSE)

## partial rank correlation coefficients ------------------
sensitivity(x, method = "pcc", rank = FALSE)
}}

\keyword{output}
\keyword{sensitivity}
