#' Multinomial Prediction Function (Observed Value Approach)
#'
#' This function predicts probabilities for all choices of a multinomial logit
#' model over a specified span of values.
#'
#' @param model the multinomial model, from a \code{\link{multinom}}()-function call (see the \code{\link{nnet}} package)
#' @param data the data with which the model was estimated
#' @param x the name of the variable that should be varied (the x-axis variable in prediction plots)
#' @param z if you want to hold a specific variable stable over all scenarios, you can name it here (optional).
#' @param z_value determine at which value you want to fix the \code{z}.
#' @param xvari former argument for \code{x} (deprecated).
#' @param scenname former argument for \code{z} (deprecated).
#' @param scenvalue former argument for \code{z_value} (deprecated).
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
#' pred <- mnl_pred_ova(model = mod, data = dataset,
#'                      x = "x1",
#'                      nsim = 10)
#'
#'
#' @importFrom stats coef na.omit quantile
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom MASS mvrnorm


mnl_pred_ova <- function(model,
                         data,
                         x,
                         by = NULL,
                         z = NULL,
                         z_value = NULL,
                         xvari,
                         scenname,
                         scenvalue,
                         nsim = 1000,
                         seed = "random",
                         probs = c(0.025, 0.975)){

  # Create list that is returned in the end.
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

  if (!missing(scenvalue)) {
    warning("The argument 'scenvalue' is deprecated; please use 'z_value' instead.\n\n",
            call. = FALSE)
    z_value <- scenvalue
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
    stop("Please supply a character string of your x-variable of interest")
  }

  if (is.null(model$Hessian) == TRUE) {
    stop("There is no Hessian matrix. Please specify Hess = TRUE in your multinom() call.")
  }

  # Names of variables in model (without the "list" character in the vector)
  variables <- as.character(attr(model$terms, "variables"))[-1]

  if(!(x %in% variables) == TRUE){
    stop("x-variable is not an independent variable in the model. There might be a typo.")
  }

  # Check if scenario is supplied correctly
  if (is.null(z) == FALSE & is.character(z) == FALSE) {
    stop("Please supply a character string of your scenario of interest")
  }

  if(is.null(z) == FALSE){
    if (!(z %in% variables) == TRUE) {
      stop("The scenario variable is not an independent variable in the model. There might be a typo.")
    }
  }

  # > Handeling the IVs --------------------------------------------------------
  # Name of independent variables
  iv <- variables[2:length(variables)]
  output[["IV"]] <- iv

  # Variables have to be numeric
  if (length(iv) > 1) {
    if (sum(apply(data[, iv], 2, class) %in% c("numeric", "integer")) < ncol(data[, iv])) {
      stop("Please supply data that consists of numeric values. The package can not handle factor or character variables, yet. For workarounds, please take a look at the github issues (https://github.com/ManuelNeumann/MNLpred/issues/1). The problem will hopefully be fixed with the 0.1.0 release.")
    }
  } else {
    if (class(eval(parse(text = paste0("data$", iv)))) %in% c("numeric", "integer") == FALSE) {
      stop("Please supply data that consists of numeric values. The package can not handle factor or character variables, yet. For workarounds, please take a look at the github issues (https://github.com/ManuelNeumann/MNLpred/issues/1). The problem will hopefully be fixed with the 0.1.0 release.")
    }
  }

  # > Handeling the DVs --------------------------------------------------------
  # Name of dependent variable
  dv <- variables[1]
  output[["DV"]] <- dv


  # > Full observations (listwise deletion) ------------------------------------
  data_redux <- na.omit(data[, c(dv, iv)])

  # Number of full observations
  obs <- nrow(data_redux)
  output[["Observations"]] <- obs

  # > Working with the model ---------------------------------------------------
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

  # Artificial variation of independent variable of interest (x)
  if (is.null(by) == TRUE) {
    by <- abs(min(eval(parse(text = paste0("data$", x))), na.rm = TRUE) -
      max(eval(parse(text = paste0("data$", x))), na.rm = TRUE))
  }

  variation <- seq(from = min(eval(parse(text = paste0("data$", x))),
                              na.rm = TRUE),
                   to = max(eval(parse(text = paste0("data$", x))),
                            na.rm = TRUE),
                   by = by)

  output[["Variation"]] <- variation

  # Length of sequence
  nseq <- length(variation)

  if (nseq == 1) {
    stop("Please supply a dataset or a x-variable with variation")
  }

  output[["nVariation"]] <- nseq

  # Choice categories of the dependent variable
  categories <- sort(unique(eval(parse(text = paste0("data$", dv)))))
  J <- length(categories)

  if (J < 3) {
    stop("Please supply a dataset with a dependent variable that has a sufficient number of outcomes (> 2)")
  }

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
  varidim <- which(colnames(X) == x)


  # Artificially alter the variable in each dimension according to
  # the preferred sequence:
  for (i in 1:nseq) {
    ovacases[, varidim, i] <- variation[i]
  }

  # Hold a second variable steady (if need be)
  if(is.null(z) == FALSE) {
    scendim <- which(colnames(X) == z)

    for (i in 1:nseq) {
      ovacases[, scendim, i] <- z_value
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

  # Loop over all scenarios
  for(i in 1:nseq){
    ovaV[, , i, 1] <- apply(matrix(0,
                                   nrow = nsim,
                                   ncol = ncol(X)),
                            1,
                            function(s) ovacases[, , i] %*% s)
    # ^ This will be zero because of the baseline category  ^

    # For each choice, the cases will now be multiplied with the simulated estimates
    for (k in 2:J) {
      coefstart <- (k-2)*ncoef+1
      coefend <- (k-1)*ncoef
      element <- parse(text = paste0("ovaV[,, i,", k, "] <- apply(S[, ",
                                     coefstart,
                                     ":",
                                     coefend,
                                     "], 1, function(s) ovacases[,, i] %*% s)"))
      eval(element)
    }

    # Progress bar:
    setTxtProgressBar(pb_multiplication, i)
  }

  # Multinomial link function:

  # 1. Part: Sum over cases
  Sexp <- rowSums(exp(ovaV), dims = 3L)

  # Create P (array with predictions)
  P <- array(NA, c(nsim, J, nseq))

  # 2. Part: take the exponent and divide through the sum of all (Sexp)
  # Add progress bar
  pb_link <- txtProgressBar(min = 0, max = nseq, initial = 0)

  # Loop over all scenarios
  cat("\nApplying link function:\n")

  for (l in 1:nseq) {
    for (m in 1:J) {
      P[, m, l] <- colMeans(exp(ovaV[, , l, m]) / Sexp[, , l])
      if (sum(is.na(P[, m, l])) != 0) {
        stop("Stop")
      }
    }

    # Progress bar:
    setTxtProgressBar(pb_link, l)
  }


  output[["P"]] <- P

  # Aggregate the simulations
  # Create tibble for plot
  # if (is.null(z_value) == TRUE) {
  #   plotdat <- tibble::tibble(iv = rep(variation, J),
  #                             categories = rep(categories, each = length(variation)),
  #                             mean = NA,
  #                             lower = NA,
  #                             upper = NA)
  # } else {
  #   plotdat <- tibble::tibble(iv = rep(variation, J),
  #                             categories = rep(categories, each = length(variation)),
  #                             scen = rep(z_value, each = length(categories)),
  #                             mean = NA,
  #                             lower = NA,
  #                             upper = NA)
  # }

  if (is.null(z_value) == TRUE) {
    plotdat <- data.frame(iv = rep(variation, J),
                          categories = rep(categories, each = length(variation)),
                          mean = NA,
                          lower = NA,
                          upper = NA)
  } else {
    plotdat <- data.frame(iv = rep(variation, J),
                          categories = rep(categories, each = length(variation)),
                          scen = rep(z_value, each = length(variation)),
                          mean = NA,
                          lower = NA,
                          upper = NA)
  }



  # Aggregate
  start <- 1

  for (i in 1:J) {
    end <- i*length(variation)
    plotdat[c(start:end), "mean"] <- colMeans(P[, i,])
    plotdat[c(start:end), "lower"] <- apply(P[, i,], 2, quantile, probs = probs[1])
    plotdat[c(start:end), "upper"] <- apply(P[, i,], 2, quantile, probs = probs[2])
    start <- end+1
  }

  # Rename the variables in the plot data
  if (is.null(z) == TRUE) {
    colnames(plotdat)[1:2] <- c(x, dv)
  } else {
    colnames(plotdat)[1:3] <- c(x, dv, z)
  }


  # Put the data in the output
  output[["plotdata"]] <- plotdat

  cat("\nDone!\n\n")

  return(output)
}
