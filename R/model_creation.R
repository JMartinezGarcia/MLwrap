create_nn <- function(hyperparams, task, epochs){

  hidden_units <- if (hyperparams$hidden_units_tune) tune::tune() else as.integer(hyperparams$hyperparams_constant$hidden_units)
  learn_rate <- if (hyperparams$learn_rate_tune) tune::tune() else hyperparams$hyperparams_constant$learn_rate
  activation <- if (hyperparams$activation_tune) tune::tune() else hyperparams$hyperparams_constant$activation

  model = parsnip::mlp(
    hidden_units = !!hidden_units,
    epochs = !!epochs,
    learn_rate = !!learn_rate,
    activation = !!activation,
  ) %>%
    parsnip::set_engine("brulee") %>%
    parsnip::set_mode(task)

  return(model)
}




create_models <- function(tidy_object, model_names, hyperparameters = NULL, task = "regression"){

                    ### EXCEPTION HANDLING
                    #if (is.null(hyperparams) ){

                    #tidy_object$add_hyperparameters(hyperparams)

                    tidy_object$add_task(task)

                    if (model_names == "Neural Network"){

                      hyperparams_nn = HyperparamsNN$new(hyperparameters)

                      tidy_object$add_hyperparameters(hyperparams_nn)

                      model = create_nn(hyperparams = hyperparams_nn, task = task, epochs = 10)

                      tidy_object$add_models_names(model_names)

                      tidy_object$add_models(model)

                    } else if (model_names == "XGBOOST"){

                      tidy_objet$add_models(model_names)

                    } else if (model_names == "SVM"){


                    }

                    return(tidy_object)
}
