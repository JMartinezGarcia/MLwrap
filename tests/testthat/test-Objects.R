devtools::load_all()

test_that("HyperparamsNN works properly", {

  hyper_nn_default = HyperparamsNN$new()

  hyperparams = hyper_nn_default$hyperparams_dials

  expect_equal(hyperparams$object[[1]]$range, list(lower = 1e-3, upper = 1e-1))
  expect_equal(hyperparams$object[[2]]$range, list(lower = 5, upper = 20))
  expect_equal(hyperparams$object[[3]]$values, c("relu", "tanh", "sigmoid"))
  expect_equal(hyper_nn_default$tuning, T)

  hyper_nn_notune = HyperparamsNN$new(list(learning_rate = 1e-2,
                                           n_neurons = 10,
                                           activation_func = "sigmoid"
                                           )
                                      )

  hyperparams = hyper_nn_notune$hyperparams_constant

  expect_equal(hyperparams$learning_rate, 1e-2)
  expect_equal(hyperparams$n_neurons, 10)
  expect_equal(hyperparams$activation_func, "sigmoid")
  expect_equal(hyper_nn_notune$tuning, F)

  hyper_nn_tune = HyperparamsNN$new(list(learning_rate = c(1e-2, 1e-1),
                                         n_neurons = 15)
                                    )

  hyperparams_dials = hyper_nn_tune$hyperparams_dials
  hyperparams_const = hyper_nn_tune$hyperparams_constant

  expect_equal(hyperparams_dials$object[[1]]$range, list(lower = 1e-2, upper = 1e-1))
  expect_equal(hyperparams_const$n_neurons, 15)
  expect_equal(hyperparams_dials$object[[2]]$values, c("relu", "tanh", "sigmoid"))
  expect_equal(hyper_nn_tune$tuning, T)

  }

)


