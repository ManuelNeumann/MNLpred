context("Variations in the input")
library(MNLpred)
library(foreign)
library(nnet)
library(MASS)

test_that("mnl_pred_ova() returns two predictions when by = NULL", {

  ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
  ml$prog2 <- relevel(ml$prog, ref = "academic")
  ml$female2 <- as.numeric(ml$female == "female")

  mod1 <- multinom(prog2 ~ female2 + read + write + math + science,
                   Hess = TRUE,
                   data = ml)

  expect_equal(mnl_pred_ova(model = mod1,
                            data = ml,
                            xvari = "math",
                            by = NULL,
                            seed = "random", # default
                            nsim = 2,
                            probs = c(0.025, 0.975))$nVariation, 2)
})

test_that("mnl_pred_ova() returns dataframes with correct number of rows", {

  ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
  ml$prog2 <- relevel(ml$prog, ref = "academic")
  ml$female2 <- as.numeric(ml$female == "female")

  mod1 <- multinom(prog2 ~ female2 + read + write + math + science,
                   Hess = TRUE,
                   data = ml)

  pred1 <- mnl_pred_ova(model = mod1,
                        data = ml,
                        xvari = "math",
                        by = 10,
                        seed = "random", # default
                        nsim = 2,
                        probs = c(0.025, 0.975))
  pred2 <- mnl_pred_ova(model = mod1,
                        data = ml,
                        xvari = "math",
                        by = 10,
                        scenname = "female2",
                        scenvalue = 1,
                        seed = "random", # default
                        nsim = 2,
                        probs = c(0.025, 0.975))

  expect_equal(nrow(pred1$plotdata), length(unique(ml$prog2)) * length(seq(min(ml$math),
                                                                           max(ml$math),
                                                                           by = 10)))
  })
