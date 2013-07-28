## PLOT method for class 'DALY'
## generates stacked YLL/YLD barplot
## with overall DALY credibility interval

plot.DALY <-
function(x, prob = 0.95, sort = TRUE, 
         bars = TRUE, col = c("grey90", "white"),
         error_bars = TRUE, eb_col = "black",
         grid = TRUE, ...){
  ## check input values
  if (!bars & !error_bars)
    stop("'bars' and 'error_bars' cannot both be FALSE")

  ## create empty vectors
  n <- length(x) - 2
  YLD <- numeric(n)
  YLL <- numeric(n)
  error <- matrix(ncol = 2, nrow = n)
  name <- numeric(n)

  ## obtain summary statistics
  y <- aggregate(x, by = "outcome")
  for (i in seq(n)){
    YLD[i] <- mean(y[[i]]$YLD)
    YLL[i] <- mean(y[[i]]$YLL)
    error[i, 1] <- quantile(y[[i]]$DALY, (1 - prob) / 2)
    error[i, 2] <- quantile(y[[i]]$DALY, prob + (1 - prob) / 2)
    name[i] <- y[[i]]$name
  }

  ## sort by total DALY
  if (sort){
    order <- order(YLD + YLL)
    YLD <- YLD[order]
    YLL <- YLL[order]
    name <- name[order]
    error <- matrix(error[order, ], ncol = 2)
  }

  ## calculate total DALY
  DALY <- YLL + YLD

  ## calculate 'xlim' values
  if (error_bars){
    xlim <- c(0, 1.04 * max(error))
  } else {
    xlim <- c(0, 1.04 * max(DALY))
  }

  ## plot YLL/YLD bars
  if (bars){
    bp <-
      barplot(DALY, horiz = TRUE, col = col[2],
              xlim = xlim,
              names.arg = name, las = 1, cex.names = .6,
              xlab = "DALY", main = y$name)
    if (grid){
      xaxp <- par("xaxp")
      abline(v = seq(xaxp[1], xaxp[2], length.out = xaxp[3] + 1),
             col = "lightgray", lty = "dotted")
      barplot(DALY, horiz = TRUE, col = col[2],
              axes = FALSE, add = TRUE)
    }
    barplot(YLL, horiz = TRUE, col = col[1],
            axes = FALSE, add = TRUE)
    legend("bottomright", legend = c("YLL", "YLD"),
           fill = col, cex = .8, bg = "white")
  } else {
    bp <-
      barplot(DALY, horiz = TRUE, col = "white", border = "white",
              xlim = c(0, 1.04 * max(error)),
              names.arg = name, las = 1, cex.names = .6,
              xlab = "DALY", main = y$name)
  }

  ## plot DALY error bars
  if (error_bars){
    segments(x0 = error[, 1], x1 = error[, 2], y0 = bp,
             col = eb_col)
    points(x = DALY, y = bp,
           pch = 16, col = eb_col)
  }

  ## put a box around it
  box()
}