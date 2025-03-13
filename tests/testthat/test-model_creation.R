devtools::load_all()

tidy_object <- TidyMLObject$new(0.5, 0.6)

hyper_nn_tune_list = list(
           learning_rate = c(1e-2, 1e-1),
           n_neurons = 15
           )


test_that("Check create_nn works properly",{

    hyper_nn_tune = HyperparamsNN$new(hyper_nn_tune_list)
    nn_model = create_nn(hyper_nn_tune, "regression", 10)

    expect_equal(nn_model$mode, "regression")
    expect_equal(class(nn_model), c("mlp", "model_spec"))

})

test_that("Check create_models works properly",{

    model_object = create_models(tidy_object = tidy_object,
                             model_names = "Neural Network",
                             hyperparameters = hyper_nn_tune_list,
                             task = "classification")

    expect_equal(model_object$task, "classification")
    expect_equal(model_object$models_names, "Neural Network")
    expect_equal(class(model_object$models), c("mlp", "model_spec"))
    expect_equal(model_object$hyperparameters$tuning, T)
    expect_equal(model_object$hyperparameters$learning_rate_tune, T)
    expect_equal(model_object$hyperparameters$n_neurons_tune, F)
    expect_equal(model_object$hyperparameters$activation_func_tune, T)
    expect_equal(model_object$hyperparameters$hyperparams_constant$n_neurons, 15)
    expect_equal(model_object$hyperparameters$hyperparams_dials$object[[1]]$range, list("lower" = 0.01, "upper" = 0.1))
    expect_equal(model_object$hyperparameters$hyperparams_dials$object[[2]]$values, c("relu", "tanh", "sigmoid"))

})

