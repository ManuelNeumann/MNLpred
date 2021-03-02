context("Variations in the input")
library(MNLpred)
library(nnet)
library(MASS)

mod1 <- multinom(vote ~ egoposition_immigration +
                   political_interest +
                   income + gender + ostwest,
                 data = gles,
                 Hess = TRUE)

mod2 <- multinom(vote ~ egoposition_immigration +
                   political_interest +
                   income + gender + ostwest,
                 data = gles)

# Add factors:
gles$factor <- as.factor(sample(c("A", "B", "C"), nrow(gles), replace = T))

mod3 <- multinom(vote ~ egoposition_immigration +
                   political_interest +
                   income + gender + ostwest +
                   factor,
                 data = gles,
                 Hess = TRUE)

# Just one IV
mod4 <- multinom(vote ~ egoposition_immigration,
                 data = gles,
                 Hess = TRUE)

# Tests
test_that("mnl_pred_ova() returns two predictions when by = NULL", {

  expect_equal(mnl_pred_ova(model = mod1,
                            data = gles,
                            x = "egoposition_immigration",
                            by = NULL,
                            seed = "random", # default
                            nsim = 100, # faster
                            probs = c(0.025, 0.975))$nVariation, 2)
})

test_that("mnl_pred_ova() returns dataframes with correct number of rows", {

  pred1 <- mnl_pred_ova(model = mod1,
                        data = gles,
                        x = "egoposition_immigration",
                        by = 1,
                        seed = "random", # default
                        nsim = 2, # faster
                        probs = c(0.025, 0.975))
  pred2 <- mnl_pred_ova(model = mod1,
                        data = gles,
                        x = "egoposition_immigration",
                        z = "gender",
                        z_value = 1,
                        by = 1,
                        seed = "random", # default
                        nsim = 2, # faster
                        probs = c(0.025, 0.975))
  expect_equal(nrow(pred1$plotdata), length(unique(gles$vote)) * length(seq(min(gles$egoposition_immigration),
                                                                            max(gles$egoposition_immigration),
                                                                            by = 1)))
  })

test_that("mnl_pred_ova() returns error message when variables contain typos", {

  expect_error(mnl_pred_ova(model = mod1,
                        data = gles,
                        x = "immigration",
                        by = 1,
                        seed = "random", # default
                        nsim = 2, # faster
                        probs = c(0.025, 0.975)),
               regexp = "There might be a typo.")
  })

test_that("mnl_pred_ova() returns error message when variables contain typos", {

  expect_error(mnl_pred_ova(model = mod1,
                            data = gles,
                            x = "egoposition_immigration",
                            by = 1,
                            z = "gndr",
                            z_value = 1,
                            seed = "random", # default
                            nsim = 2, # faster
                            probs = c(0.025, 0.975)),
               regexp = "There might be a typo.")
})

test_that("mnl_pred_ova() returns error message when there is no Hessian matrix", {

  expect_error(mnl_pred_ova(model = mod2,
                            data = gles,
                            x = "egoposition_immigration",
                            by = 1,
                            seed = "random", # default
                            nsim = 2, # faster
                            probs = c(0.025, 0.975)),
               regexp = "Hess = TRUE")
})

test_that("mnl_pred_ova() stops if non-numeric variables are supplied with the data", {

  expect_error(mnl_pred_ova(model = mod3,
                            data = gles,
                            x = "egoposition_immigration",
                            nsim = 2),
               regexp = "Please supply data that consists of numeric values.")

})


# Fixing bug with apply() and one IV (v0.0.6)
test_that("mnl_pred_ov() works with just one iv", {

  expect_type(mnl_pred_ova(model = mod4,
                           data = gles,
                           x = "egoposition_immigration",
                           nsim = 2), "list")
})
