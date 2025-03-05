create_nn <- function(hyperparams, task, epochs){

  model = parsnip::mlp(
    hidden_units = if (length(hyperparams$n_neurons) > 1) tune::tune else hyperparams$n_neurons,
    epochs = epochs,
    learn_rate = if (length(hyperparams$learning_rate) > 1) tune::tune else hyperparams$learning_rate,
    activation = if (length(hyperparams$activation_func) > 1) tune::tune else hyperparams$activation_func,
  ) %>%
    parsnip::set_engine("brulee") %>%
    parsnip::set_mode(task) %>%
    parsnip::translate()

  return(model)
}




create_models <- function(tidy_object, model_names, hyperparams = NULL, task = "regression"){

                    ### EXCEPTION HANDLING
                    #if (is.null(hyperparams) ){

                    #tidy_object$add_hyperparameters(hyperparams)

                    tidy_object$add_task(task)

                    if (model_names == "Neural Network"){

                      hyperparams_nn = HyperparamsNN(hyperparams)

                      tidy_object$add_hyperparameters(hyperarams_nn)

                      model = create_nn(hyperparams = hyperparams_nn, task = task, epochs = 10)

                      tidy_object$add_models(model)

                    } else if (model_names == "XGBOOST"){

                      tidy_objet$add_models(model_names)

                    } else if (model_names == "SVM"){


                    }

                    return(tidy_object)
}
