devtools::load_all()

tidy_object <- TidyMLObject$new(0.5, 0.6)

hyper_nn_tune_list = list(
           learn_rate = c(-2, -1),
           hidden_units = 15
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
    expect_equal(model_object$hyperparameters$learn_rate_tune, T)
    expect_equal(model_object$hyperparameters$hidden_units_tune, F)
    expect_equal(model_object$hyperparameters$activation_tune, T)
    expect_equal(model_object$hyperparameters$hyperparams_constant$hidden_units, 15)
    expect_equal(model_object$hyperparameters$hyperparams_ranges$learn_rate$range, list("lower" = -2, "upper" = -1))
    expect_equal(model_object$hyperparameters$hyperparams_ranges$activation$values, c("relu", "tanh", "sigmoid"))

})

