test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

tidy_object <- TidyMLObject$new(0.5, 0.6)

hyper_nn_tune = HyperparamsNN$new(list(
                                       learning_rate = c(1e-2, 1e-1),
                                       n_neurons = 15)
                                )

test_that("Check create_nn works properly",{

    nn_model = create_nn(hyper_nn_tune, "regression", 10)

})


