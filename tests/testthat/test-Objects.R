test_that("HyperparamsNN works properly", {

  hyper_nn = HyperparamsNN$new()

  expect_equal(hyper_nn$hyperparams$learning_rate, c(1e-3, 1e-1))
  expect_equal(hyper_nn$hyperparams$n_neurons, c(5, 20))
  expect_equal(hyper_nn$hyperparams$activation_func, c("relu", "tanh", "sigmoid"))
  expect_equal(hyper_nn$tuneable, T)

  hyper_nn_notune = HyperparamsNN$new(list(learning_rate = 1e-2,
                                           n_neurons = 10,
                                           activation_func = "sigmoid"
                                           )
                                      )
  expect_equal(hyper_nn_notune$hyperparams$learning_rate, 1e-2)
  expect_equal(hyper_nn_notune$hyperparams$n_neurons, 10)
  expect_equal(hyper_nn_notune$hyperparams$activation_func, "sigmoid")
  expect_equal(hyper_nn_notune$tuneable, F)

  hyper_nn_tune = HyperparamsNN$new(list(learning_rate = c(1e-2, 1e-1),
                                         n_neurons = 15)
                                    )

  expect_equal(hyper_nn_tune$hyperparams$learning_rate, c(1e-2, 1e-1))
  expect_equal(hyper_nn_tune$hyperparams$n_neurons, 15)
  expect_equal(hyper_nn_tune$hyperparams$activation_func, c("relu", "tanh", "sigmoid"))
  expect_equal(hyper_nn_tune$tuneable, T)

})
