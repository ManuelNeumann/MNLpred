context("Variations in the input")
library(MNLpred)
library(foreign)
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

test_that("mnl_pred_ova() returns two predictions when by = NULL", {

  expect_equal(mnl_pred_ova(model = mod1,
                            data = gles,
                            xvari = "egoposition_immigration",
                            by = NULL,
                            seed = "random", # default
                            nsim = 100, # faster
                            probs = c(0.025, 0.975))$nVariation, 2)
})

test_that("mnl_pred_ova() returns dataframes with correct number of rows", {

  pred1 <- mnl_pred_ova(model = mod1,
                        data = gles,
                        xvari = "egoposition_immigration",
                        by = 1,
                        seed = "random", # default
                        nsim = 2, # faster
                        probs = c(0.025, 0.975))
  pred2 <- mnl_pred_ova(model = mod1,
                        data = gles,
                        xvari = "egoposition_immigration",
                        scenname = "gender",
                        scenvalue = 1,
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
                        xvari = "immigration",
                        by = 1,
                        seed = "random", # default
                        nsim = 2, # faster
                        probs = c(0.025, 0.975)),
               regexp = "There might be a typo.")
  })

test_that("mnl_pred_ova() returns error message when variables contain typos", {

  expect_error(mnl_pred_ova(model = mod1,
                            data = gles,
                            xvari = "egoposition_immigration",
                            by = 1,
                            scenname = "gndr",
                            scenvalue = 1,
                            seed = "random", # default
                            nsim = 2, # faster
                            probs = c(0.025, 0.975)),
               regexp = "There might be a typo.")
})

test_that("mnl_pred_ova() returns error message when there is no Hessian matrix", {

  expect_error(mnl_pred_ova(model = mod2,
                            data = gles,
                            xvari = "immigration",
                            by = 1,
                            seed = "random", # default
                            nsim = 2, # faster
                            probs = c(0.025, 0.975)),
               regexp = "Hess = TRUE")
})
