devtools::load_all()

test_that("HyperparamsNN Default works properly", {

  hyper_nn_default = HyperparamsNN$new()

  hyp_ranges = hyper_nn_default$hyperparams_ranges

  expect_equal(hyp_ranges$learn_rate$range, list(lower = -3, upper = -1))
  expect_equal(hyp_ranges$hidden_units$range, list(lower = 5, upper = 20))
  expect_equal(hyp_ranges$activation$values, c("relu", "tanh", "sigmoid"))
  expect_equal(hyper_nn_default$tuning, T)

})

test_that("HyperparamsNN constant works properly", {

  hyper_nn_notune = HyperparamsNN$new(list(learn_rate = -2,
                                           hidden_units = 10,
                                           activation = "sigmoid"
                                           )
                                        )

  hyperparams = hyper_nn_notune$hyperparams_constant

  expect_equal(hyperparams$learn_rate, -2)
  expect_equal(hyperparams$hidden_units, 10)
  expect_equal(hyperparams$activation, "sigmoid")
  expect_equal(hyper_nn_notune$tuning, F)

})

test_that("HyperparamsNN mixed works properly", {

  hyper_nn_tune = HyperparamsNN$new(list(learn_rate = c(-2, -1),
                                         hidden_units = 15)
                                    )

  hyp_ranges = hyper_nn_tune$hyperparams_ranges
  hyp_const = hyper_nn_tune$hyperparams_constant

  expect_equal(hyp_ranges$learn_rate$range, list(lower = -2, upper = -1))
  expect_equal(hyp_const$hidden_units, 15)
  expect_equal(hyp_ranges$activation$values, c("relu", "tanh", "sigmoid"))
  expect_equal(hyper_nn_tune$tuning, T)

})


