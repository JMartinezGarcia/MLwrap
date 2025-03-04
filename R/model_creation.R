create_nn <- function(hyperparams, task, epochs){

  model = parsnip::mlp(
    hidden_units = hyperparams$n_neurons_tune,
    epochs = epochs,
    learn_rate = hyperparams$learning_rate_tune,
    activation = hyperparams$activation_func_tune
  ) %>%
    parsnip::set_engine("brulee") %>%
    parsnip::set_mode(task) %>%
    parsnip::translate()

  return(model)
}




create_models <- function(tidy_object, model_names, hyperparams = NULL, task = "regression"){

                    ### EXCEPTION HANDLING
                    #if (is.null(hyperparams) ){

                    tidy_object$add_hyperparameters(hyperparams)

                    tidy_object$add_task(task)

                    if (model_names == "Neural Network"){

                      model = create_nn(hyperparams = hyperparams, task = task, epochs = 10)

                      tidy_object$add_models(model)

                    } else if (model_names == "XGBOOST"){

                      tidy_objet$add_models(model_names)

                    } else if (model_names == "SVM"){


                    }

                    return(tidy_object)
}
