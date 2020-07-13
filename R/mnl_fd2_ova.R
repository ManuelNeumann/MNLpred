#' Multinomial First Differences Predictions For Two Values (Observed Value Approach)
#'
#' @param model the multinomial model, from a \code{\link{multinom}}()-function call (see the \code{\link{nnet}} package)
#' @param data the data with which the model was estimated
#' @param xvari the name of the variable that should be varied
#' @param value1 first value for the difference
#' @param value2 second value for the difference
#' @param nsim numbers of simulations
#' @param seed set a seed for replication purposes.
#' @param probs a vector with two numbers, defining the significance levels. Default to 5\% significance level: \code{c(0.025, 0.975)}
#'
#' @return The function returns a list with several elements. Most importantly the list includes the simulated draws `S`, the simulated predictions `P`, the first differences of the predictions `P_fd`, a data set for plotting `plotdata` the predicted probabilities, and one for the first differences `plotdata_fd`.
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
#' fdi1 <- mnl_fd2_ova(model = mod, data = dataset,
#'                     xvari = "x1",
#'                     value1 = min(dataset$x1),
#'                     value2 = max(dataset$x1),
#'                     nsim = 10)
#'
#'
#'
#' @importFrom stats coef na.omit quantile
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom MASS mvrnorm


mnl_fd2_ova <- function(model,
                        data,
                        xvari,
                        value1,
                        value2,
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

  if (is.null(value1) == TRUE | is.null(value2) == TRUE) {
    stop("Please supply values to compute differences")
  }

  if (is.null(model$Hessian) == TRUE) {
    stop("There is no Hessian matrix. Please specify Hess = TRUE in your multinom() call.")
  }

  # Names of variables in model (without the "list" character in the vector)
  variables <- as.character(attr(model$terms, "variables"))[-1]

  if (!(xvari %in% variables) == TRUE){
    stop("x-variable is not an independent variable in the model. There might be a typo.")
  }


  # Create list that is returned in the end.
  output <- list()

  # Get matrix of coefficients out of the model
  coefmatrix <- coef(model)

  # Number of coefficients
  ncoef <- ncol(coefmatrix)

  # Model coefficients as a vector
  mu <- as.vector(t(coef(model)))

  # Variance-covariance matrix of estimates
  varcov <- solve(model$Hessian)

  # Set seed if needed:
  if (seed != "random") {
    set.seed(seed = seed)
  }

  # Simulate a sampling distribution
  S <- mvrnorm(nsim, mu, varcov)
  output[["S"]] <- S

  # Artificial variation ov independent variable of interest
  variation <- c(value1, value2)

  output[["ScenarioValues"]] <- variation

  nseq <- length(variation)

  # Name of independent variables
  iv <- variables[2:length(variables)]
  output[["IV"]] <- iv

  # Variables have to be numeric
  if (sum(apply(data[, iv], 2, class) %in% c("numeric", "integer")) < ncol(data[, iv])) {
    stop("Please supply data that consists of numeric values. The package can not handle factor or character variables, yet. For workarounds, please take a look at the github issues (https://github.com/ManuelNeumann/MNLpred/issues/1). The problem will be fixed with the 0.1.0 release.")
  }

  # Name of dependent variable
  dv <- variables[1]
  output[["DV"]] <- dv

  # Full observations (listwise deletion:
  data_redux <- na.omit(data[, c(dv, iv)])

  # Number of full observations
  obs <- nrow(data_redux)
  output[["Observations"]] <- obs

  # Choice categories of the dependent variable
  categories <- sort(unique(eval(parse(text = paste0("data$", dv)))))
  J <- length(categories)
  output[["ChoiceCategories"]] <- categories
  output[["nChoices"]] <- J

  # Numbers of interactions
  ninteraction <- sum(grepl(":", model$coefnames))

  # Matrix of observations
  X <- matrix(NA, ncol = ncoef, nrow = obs)
  colnames(X) <- model$coefnames
  # 1 for the Intercept
  X[, 1] <- 1
  # Values of the independent variables
  X[, 2:(length(iv)+1)] <- as.matrix(data_redux[, iv])


  # Prepare array to fill in the matrix with the observed values
  ovacases <- array(NA, c(dim(X), nseq))
  # Fill in the matrices:
  ovacases[,,] <- X

  # Select the position of the variable which should vary:
  if (is.null(xvari) == FALSE) {
    varidim <- which(colnames(X) == xvari)
  }

  # Artificially alter the variable in each dimension according to
  # the preferred sequence:
  if (is.null(xvari) == FALSE) {
    for (i in 1:nseq) {
      ovacases[, varidim, i] <- variation[i]
    }
  }


  # Compute interactions:
  if (ninteraction != 0) {

    # Get position of interaction terms
    interactionterms <- which(grepl(":", model$coefnames) == TRUE)

    # Compute the terms:
    for (i in c(interactionterms)) {
      # First variable name of the interaction:
      firstint <- gsub(":.*", "", model$coefnames[i])
      # Second variable name of the interaction:
      secondint <- gsub(".*:", "", model$coefnames[i])

      # Get position in matrix:
      intdim1 <- which(colnames(X) == firstint)
      intdim2 <- which(colnames(X) == secondint)

      # Compute interaction term:
      for(j in 1:nseq) {
        ovacases[, i, j] <- ovacases[, intdim1, j]*ovacases[, intdim2, j]
      }
    }
  }

  # Prepare array of observed values:
  ovaV <- array(NA, c(obs, nsim, nseq, J))

  # Add progress bar
  pb_multiplication <- txtProgressBar(min = 0, max = nseq, initial = 0)

  # Loop over all scenarios
  cat("Multiplying values with simulated estimates:\n")

  for(i in 1:nseq){
    ovaV[, , i, 1] <- apply(matrix(0,
                                   nrow = nsim,
                                   ncol = ncol(X)), 1, function(s) ovacases[, , i] %*% s)
    # ^ This will be zero because of the baseline category  ^

    # For each choice, the cases will now be multiplied with the simulated estimates
    for (k in 2:J) {
      coefstart <- (k-2)*ncoef+1
      coefend <- (k-1)*ncoef
      element <- parse(text = paste0("ovaV[,, i,", k, "] <- apply(S[, ",
                                     coefstart, ":", coefend,
                                     "], 1, function(s) ovacases[,, i] %*% s)"))
      eval(element)
    }

    # Progress bar:
    setTxtProgressBar(pb_multiplication, i)
  }

  # Multinomial link function:

  pb_link <- txtProgressBar(min = 0, max = nseq, initial = 0)
  cat("\nApplying link function:\n")

  # 1. Part: Sum over cases
  Sexp <- apply(ovaV, c(1, 2, 3), function(x) sum(exp(x)))

  # Create P (array with predictions)
  P <- array(NA, c(nsim, J, nseq))

  # 2. Part: take the exponent and divide through the sum of all (Sexp)
  for (l in 1:nseq) {
    for (m in 1:J) {
      P[, m, l] <- apply(exp(ovaV[, , l, m])/Sexp[, , l], 2, mean)
    }

    setTxtProgressBar(pb_link, l)
  }

  output[["P"]] <- P

  # Aggregate the simulations
  # Create tibble for plot
  # plotdat <- tibble::tibble(iv = rep(variation, J),
  #                           categories = rep(categories, each = length(variation)),
  #                           mean = NA,
  #                           lower = NA,
  #                           upper = NA)
  plotdat <- data.frame(iv = rep(variation, J),
                        categories = rep(categories, each = length(variation)),
                        mean = NA,
                        lower = NA,
                        upper = NA)



  # Aggregate

  start <- 1

  for (i in 1:J) {
    end <- i*length(variation)
    plotdat[c(start:end), "mean"] <- apply(P[, i,], 2, mean)
    plotdat[c(start:end), "lower"] <- apply(P[, i,], 2, quantile, probs = probs[1])
    plotdat[c(start:end), "upper"] <- apply(P[, i,], 2, quantile, probs = probs[2])
    start <- end+1
  }

  # Rename the variables in the plot data
  colnames(plotdat)[1:2] <- c(xvari, dv)


  # Put the data in the output
  output[["plotdata"]] <- plotdat

  # First differences
  P_fd <- array(NA, dim = c(nsim, J))

  for (i in 1:J) {
    P_fd[, i] <- P[, i, 2] - P[, i, 1]
  }

  output[["P_fd"]] <- P_fd

  # Plotdata
  plotdat_fd <- data.frame(categories = categories,
                           mean = NA,
                           lower = NA,
                           upper = NA)

  start <- 1
  for (i in 1:J){
    end <- i
    plotdat_fd[c(start:end), "mean"] <- mean(P_fd[, i])
    plotdat_fd[c(start:end), "lower"] <- quantile(P_fd[, i], probs = probs[1])
    plotdat_fd[c(start:end), "upper"] <- quantile(P_fd[, i], probs = probs[2])
    start <- end+1
  }

  output[["plotdata_fd"]] <- plotdat_fd

  cat("\nDone!\n\n")
  return(output)
}
