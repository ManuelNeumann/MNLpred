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
#' @param x the name of the variable that should be varied (the x-axis variable in prediction plots)
#' @param z define the variable for which you want to compute the difference.
#' @param z_values determine the two values at which value you want to fix the scenario (\code{z}). The first differences will be computed by subtracting the values of the first supplied scenario from the second one.
#' @param xvari former argument for \code{x} (deprecated).
#' @param scenname former argument for \code{z} (deprecated).
#' @param scenvalues former argument for \code{z_values} (deprecated).
#' @param by define the steps of \code{x}.
#' @param nsim numbers of simulations
#' @param seed set a seed for replication purposes.
#' @param probs a vector with two numbers, defining the significance levels. Default to 5\% significance level: \code{c(0.025, 0.975)}
#'
#' @return The function returns a list with several elements. Most importantly the list includes the simulated draws `S`, the simulated predictions `P`, and a data set for plotting `plotdata`.
#' @export
#'
#' @examples
#' library(nnet)
#' library(MASS)
#'
#' dataset <- data.frame(y = c(rep("a", 10), rep("b", 10), rep("c", 10)),
#'                       x1 = rnorm(30),
#'                       x2 = rnorm(30, mean = 1),
#'                       x3 = sample(1:10, 30, replace = TRUE))
#'
#' mod <- multinom(y ~ x1 + x2 + x3, data = dataset, Hess = TRUE)
#'
#' fdif <- mnl_fd_ova(model = mod, data = dataset,
#'                    x = "x1", z = "x3",
#'                    z_values = c(min(dataset$x3), max(dataset$x3)),
#'                    nsim = 10)
#'

mnl_fd_ova <- function(model,
                       data,
                       x,
                       z,
                       z_values,
                       xvari,
                       scenname,
                       scenvalues,
                       by = NULL,
                       nsim = 1000,
                       seed = "random",
                       probs = c(0.025, 0.975)){

  # Prepare output:
  output <- list()

  # Warnings for deprecated arguments
  if (!missing(xvari)) {
    warning("The argument 'xvari' is deprecated; please use 'x' instead.\n\n",
            call. = FALSE)
    x <- xvari
  }

  if (!missing(scenname)) {
    warning("The argument 'scenname' is deprecated; please use 'z' instead.\n\n",
            call. = FALSE)
    z <- scenname
  }

  if (!missing(scenvalues)) {
    warning("The argument 'scenvalues' is deprecated; please use 'z_values' instead.\n\n",
            call. = FALSE)
    z_values <- scenvalues
  }

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

  if (is.null(x) == TRUE | is.character(x) == FALSE) {
    stop("Please supply a character of your x-variable of interest")
  }

  if (is.null(z) == TRUE | is.character(z) == FALSE) {
    stop("Please supply a character of the scenario variable of interest")
  }

  if (is.null(z_values) == TRUE |
      length(z_values) != 2 |
      is.vector(z_values, mode = "numeric") == FALSE) {
    stop("Please two numeric values that are used for the different scenarios")
  }


  if (seed == "random") {
    seed <- sample(1:10000, 1)
  }

  # Predictions for first scenario
  cat("First scenario:\n")

  pred1 <- mnl_pred_ova(model = model,
                        data = data,
                        x = x,
                        z = z,
                        z_value = z_values[1],
                        by = by, nsim = nsim, seed = seed,
                        probs = probs)
  output[["Prediction1"]] <- pred1

  # Predictions for second scenario
  cat("Second scenario:\n")

  pred2 <- mnl_pred_ova(model = model,
                        data = data,
                        x = x,
                        z = z,
                        z_value = z_values[2],
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
    plotdata_fd[c(start:end), "mean"] <- colMeans(P_fd[, i, ])
    plotdata_fd[c(start:end), "lower"] <- apply(P_fd[, i, ], 2, quantile, probs = probs[1])
    plotdata_fd[c(start:end), "upper"] <- apply(P_fd[, i, ], 2, quantile, probs = probs[2])
    start <- end+1
  }

  output[["plotdata_fd"]] <- plotdata_fd

  return(output)

}
