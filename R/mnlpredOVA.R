#' Multinomial Prediction Function (Observed Value Approach)
#'
#' @param model the multinomial model, from a \code{\link{multinom}}()-function call (see the \code{\link{nnet}} package)
#' @param data the data with which the model was estimated
#' @param xvari the name of the variable that should be varied (the x-axis variable in prediction plots)
#' @param scenname if you want to hold a specific variable stable over all scenarios, you can name it here (optional).
#' @param scenvalue determine at which value you want to fix the \code{scenname}.
#' @param by define the steps of the \code{xvari}.
#' @param nsim numbers of simulations
#' @param seed set a seed for replication purposes.
#' @param probs a vector with two numbers, defining the significance levels. Default to 5\% siginficance level (\code{c(0.025, 0.975)})
#'
#' @return The function returns a list with several elements.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom stats coef na.omit quantile


mnlpredOVA <- function(model,
                       data,
                       xvari,
                       scenname = NULL,
                       scenvalue = NULL,
                       by = 1,
                       nsim = 1000,
                       seed = "random",
                       probs = c(0.025, 0.975)){

  # Create list that is returned in the end.
  output <- list()

  # Get matrix of coefficients out of the model
  coefmatrix <- coef(model)

  # Number of coefficients
  ncoef <- ncol(coefmatrix)

  # Model coefficients as a vector
  mu <- coef(model) %>% t() %>% as.vector()

  # Variance-covariance matrix of estimates
  varcov <- solve(model$Hessian)

  # Set seed if needed:
  if (seed != "random") {
    set.seed(seed = seed)
  }

  # Simulate a sampling distribution
  S <- mvrnorm(nsim, mu, varcov)
  output$S <- S

  # Artificial variation ov independent variable of interest
  variation <- seq(from = min(eval(parse(text = paste0("data$", xvari))), na.rm = TRUE),
                   to = max(eval(parse(text = paste0("data$", xvari))), na.rm = TRUE),
                   by = by)
  output[["variation"]] <- variation

  # Length of sequence
  nseq <- length(variation)

  # Names of variables in model (without the "list" character in the vector)
  variables <- as.character(attr(model$terms, "variables"))[-1]

  # Name of independent variables
  iv <- variables[2:length(variables)]
  output[["IV"]] <- iv

  # Name of dependent variable
  dv <- variables[1]
  output[["DV"]] <- dv

  # Full observations (listwise deletion:
  data_redux <- data[, c(dv, iv)] %>% na.omit()

  # Number of full observations
  obs <- nrow(data_redux)
  output[["obs"]] <- obs

  # Choice categories of the dependent variable
  categories <- sort(unique(eval(parse(text = paste0("data$", dv)))))
  J <- length(categories)
  output[["ChoiceCategories"]] <- categories
  output[["J"]] <- J

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
  varidim <- which(colnames(X) == xvari)

  # Artificially alter the variable in each dimension according to
  # the preferred sequence:
  for (i in 1:nseq) {
    ovacases[, varidim, i] <- variation[i]
  }

  # Hold a second variable steady (if need be)
  if(is.null(scenname) == FALSE) {
    scendim <- which(colnames(X) == scenname)

    for (i in 1:nseq) {
      ovacases[, scendim, i] <- scenvalue
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

  # Loop over all scenarios
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
  }

  # Multinomial link function:

  # 1. Part: Sum over cases
  Sexp <- apply(ovaV, c(1, 2, 3), function(x) sum(exp(x)))

  # Create P (array with predictions)
  P <- array(NA, c(nsim, J, nseq))

  # 2. Part: take the exponent and divide through the sum of all (Sexp)
  for (l in 1:nseq) {
    for (m in 1:J) {
      P[, m, l] <- apply(exp(ovaV[, , l, m])/Sexp[, , l], 2, mean)
    }
  }

  output[["P"]] <- P

  # Aggregate the simulations
  # Create tibble for plot
  plotdat <- tibble::tibble(iv = rep(variation, J),
                            categories = rep(categories, each = length(variation)),
                            scen = rep(scenvalue, each = length(categories)),
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

  colnames(plotdat)[1:3] <- c(xvari, dv, scenname)

  return(output)
}
