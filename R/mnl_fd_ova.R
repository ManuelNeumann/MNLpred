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
#' @param model the multinomial model, from a \code{\link{multinom}}()-function call (see the \code{\link{nnet}} package)
#' @param data the data with which the model was estimated
#' @param xvari the name of the variable that should be varied (the x-axis variable in prediction plots)
#' @param scenname if you want to hold a specific variable stable over all scenarios, you can name it here (optional).
#' @param scenvalues determine the two values at which value you want to fix the scenario (\code{scenname}). The first differences will be computed by subtracting the values of the first supplied scenario from the second one.
#' @param by define the steps of the \code{xvari}.
#' @param nsim numbers of simulations
#' @param seed set a seed for replication purposes.
#' @param probs a vector with two numbers, defining the significance levels. Default to 5\% significance level: \code{c(0.025, 0.975)}
#'
#' @return The function returns a list with several elements. Most importantly the list includes the simulated draws `S`, the simulated predictions `P`, and a data set for plotting `plotdata`.
#' @export
#'
#' @examples
#' library(foreign)
#' library(nnet)
#' library(MASS)
#'
#' ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
#'
#' ml$prog2 <- relevel(ml$prog, ref = "academic")
#' ml$female2 <- as.numeric(ml$female == "female")
#'
#' mod1 <- multinom(prog2 ~ female2 + read + write + math + science,
#'                  Hess = TRUE, data = ml)
#'
#' fdif <- mnl_fd_ova(model = mod1, data = ml, xvari = "math", by = 1,
#'                    scenname = "female2", scenvalues = c(0,1),
#'                    nsim = 10)
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
  # Errors:
  if (is.null(model) == TRUE) {
    stop("Please supply a model")
  }

  if (sum(grepl("multinom", model$call)) == 0) {
    stop("Please supply a multinom()-model")
  }

  if (is.null(data) == TRUE) {
    stop("Please supply a data set")
  }

  if (is.null(xvari) == TRUE | is.character(xvari) == FALSE) {
    stop("Please supply a character of your x-variable of interest")
  }

  if (is.null(scenname) == TRUE | is.character(scenname) == FALSE) {
    stop("Please supply a character of the scenario variable of interest")
  }

  if (is.null(scenvalues) == TRUE |
      length(scenvalues) != 2 |
      is.vector(scenvalues, mode = "numeric") == FALSE) {
    stop("Please two numeric values that are used for the different scenarios")
  }


  # Prepare output:
  output <- list()

  if (seed == "random") {
    seed <- sample(1:10000, 1)
  }

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

  plotdat <- rbind(pred1$plotdata,
                   pred2$plotdata)

  output[["plotdata"]] <- plotdat


  # First differences
  P_fd <- array(NA, dim = c(nsim, pred1$nChoices, pred1$nVariation))

  for (i in 1:pred1$nChoices) {
    P_fd[, i,] <- pred2$P[, i,] - pred1$P[, i,]
  }

  output[["P"]] <- P_fd

  # Plotdata
  plotdata_fd <- pred1$plotdata[, c(1:2,4:6)]
  plotdata_fd[, c("mean", "lower", "upper")] <- NA

  start <- 1
  for (i in 1:pred1$nChoices){
    end <- i*pred1$nVariation
    plotdata_fd[c(start:end), "mean"] <- apply(P_fd[, i, ], 2, mean)
    plotdata_fd[c(start:end), "lower"] <- apply(P_fd[, i, ], 2, quantile, probs = probs[1])
    plotdata_fd[c(start:end), "upper"] <- apply(P_fd[, i, ], 2, quantile, probs = probs[2])
    start <- end+1
  }

  output[["plotdata_fd"]] <- plotdata_fd

  return(output)

}
