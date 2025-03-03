model_selection <- function(tidy_object, model_names, hyperparams = NULL, task = "regression"){

                    ### EXCEPTION HANDLING
                    if (!is.null(hyperparams) ){

                      tidy_object$add_hyperparameters(hyperparams)

                    } else {

                      #### DEFAULT HYPERPARAMS FOR EACH MODEL

                    }

                    if (model_names == "Neural Network"){

                      model = parsnip::mlp(
                        hidden_units = tune::tune(),
                        epochs = 10,
                        learn_rate = tune::tune(),
                        activation = tune::tune()
                      ) %>%
                        set_engine("brulee") %>%
                        set_mode("regression") %>%
                        translate()

                      tidy_object$add_models(model)

                    } else if (model_names == "XGBOOST"){

                      tidy_objet$add_models(model_names)

                    } else if (model_names == "SVM"){



                    }



}
