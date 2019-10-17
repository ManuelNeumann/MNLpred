#' Multinomial First Differences Prediction (Observed Value Approach)
#'
#' This function predicts values for two different scenarios over a range of
#' values. It then takes the differences between the different simulations to
#' return first differences for each value.
#'
#' The function uses the \code{\link{mnl_pred_ova}} function for each scenario.
#' The results of these predictions are also returned and can therefore be
#' easily accessed. If you need predictions for multiple scenarios, you can use
#' this function to both plot the predictions for each scenario and the
#' differences between them.
#'
#' @param model
#'
#' @return The function returns a list with several elements.
#' @export
#'
#' @examples
#'

mnl_fd_ova <- function(model,
                       data,
                       xvari,
                       scenname,
                       scenvalues,
                       by = NULL,
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
  P_fd <- array(NA, dim = c(nsim, pred1$nChoices, pred1$nVariation))

  for (i in 1:pred1$nChoices) {
    P_fd[, i,] <- pred2$P[, i,] - pred1$P[, i,]
  }

  output[["P"]] <- P_fd

  # Plotdata
  plotdata_fd <- pred1$plotdata[, c(1:2,4:6)]
  plotdata_fd[, c(4:6)] <- NA

  start <- 1
  for (i in 1:pred1$nChoices){
    end <- i*pred1$nVariation
    plotdata_fd[c(start:end), "mean"] <- apply(P_fd[, i, ], 2, mean)
    plotdata_fd[c(start:end), "lower"] <- apply(P_fd[, i, ], 2, quantile, probs = probs[1])
    plotdata_fd[c(start:end), "upper"] <- apply(P_fd[, i, ], 2, quantile, probs = probs[2])
    start <- end+1
  }

  output[["plotdata"]] <- plotdata_fd

  return(output)

}
