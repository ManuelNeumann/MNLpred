#' Multinomial First Differences Prediction (Observed Value Approach)
#'
#' @return
#' @export
#'
#' @examples
#'

mnl_fd_ova <- function(model,
                       data,
                       xvari,
                       scenname,
                       scenvalues,
                       by = 1,
                       nsim = 1000,
                       seed = "random",
                       probs = c(0.025, 0.975)){
  output <- list()

  # Predictions for first scenario
  pred1 <- mnl_pred_ova(model = model,
                        data = data,
                        xvari = xvari,
                        scenname = scenname,
                        scenvalue = scenvalues[1],
                        by = by, nsim = nsim, seed = seed,
                        probs = probs)
  output[["Prediction1"]] <- pred1

  # Predictions for second scenario
  pred2 <- mnl_pred_ova(model = model,
                        data = data,
                        xvari = xvari,
                        scenname = scenname,
                        scenvalue = scenvalues[2],
                        by = by, nsim = nsim, seed = seed,
                        probs = probs)

  output[["Prediction2"]] <- pred2


  # First differences
  P_fd <- array(NA, dim = c(nsim, pred1$nchoices, pred1$nvariation))

  for (i in 1:pred1$nchoices) {
    P_fd[, i,] <- pred2$P[, i,] - pred1$P[, i,]
  }

  output[["P"]] <- P_fd

  # Plotdata
  plotdata_fd <- pred1$plotdata[, c(1:2,4:6)]
  plotdata_fd[, c(4:6)] <- NA

  start <- 1
  for (i in 1:pred1$nchoices){
    end <- i*pred1$nvariation
    plotdata_fd[c(start:end), "mean"] <- apply(P_fd[, i, ], 2, mean)
    plotdata_fd[c(start:end), "lower"] <- apply(P_fd[, i, ], 2, quantile, probs = probs[1])
    plotdata_fd[c(start:end), "upper"] <- apply(P_fd[, i, ], 2, quantile, probs = probs[2])
    start <- end+1
  }

  output[["plotdata"]] <- plotdata_fd

  return(output)

}
