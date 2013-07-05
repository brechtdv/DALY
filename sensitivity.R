## sensitivity analysis for DALY estimate

sensitivity <-
function(x, method = c("regression", "rank"), mapped = TRUE,
         plot = TRUE, main = "Sensitivity analysis", ...){
  ## evaluate method
  method <- match.arg(method)

  ## aggregate DALYs
  y <- aggregate(x, by = "total")
  daly <- y$DALY

  ## max number of parameters
  ## n_outcomes * 8
  n_outcomes <- length(x) - 2

  ## merge unique columns
  listNames <- c("inc", "trt", "ons", "dur", "DWt", "DWn", "mrt", "dth")
  data <- numeric(length(daly))
  for (i in seq(n_outcomes)){
    for (j in seq(8)){
      data <-
        cbind(data,
              as_column(x[[i]]$input[[j]], i, listNames[j]))
    }
  }

  ## remove fixed values
  fixed <- apply(data, 2, var) == 0
  data <- data[, !fixed]

  if (method == "regression"){
    ## scale variables
    data <- as.data.frame(apply(data, 2, scale))
    if (!mapped) daly <- scale(daly)

    ## linear regression
    out <- summary(lm(daly ~ ., data = data))

    ## ranked estimates
    cf <- coef(out)[-1, ]
    signif <- cf[, 4] < .05
    order <- order(abs(cf[signif, 1]))
    est <- cf[signif, 1][order]
    xlab <- ifelse(mapped,
                   "mapped value",
                   "standardized regression coefficient")

  } else {
    ## rank correlation coefficients
    out <- matrix(ncol = 2, nrow = ncol(data))
    colnames(out) <- c("rho", "p")
    rownames(out) <- colnames(data)
    for (i in seq(ncol(data))){
      cor <- cor.test(daly, data[, i], method = "spearman")
      out[i, ] <- unname(c(cor[[4]], cor[[3]]))
    }

    ## ranked estimates
    signif <- out[, 2] < .05
    order <- order(abs(out[signif, 1]))
    est <- out[signif, 1][order]
    xlab = "Spearman's rank correlation coefficient"
  }

  ## plot coefficients
  if (plot){
    par(mar = c(4, 6, 2, 2) + .5)
    barplot(est, horiz = T, las = 1,
            main = main, xlab = xlab, ...)
  }

  ## return output
  return(out)
}