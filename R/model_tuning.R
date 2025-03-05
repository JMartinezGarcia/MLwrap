create_workflow <- function(tidy_object){

    workflow = workflows::workflow() %>%
                      workflows::add_recipe(tidy_object$transformer) %>%
                      workflows::add_model(tidy_object$models)

    return(workflow)
}

hyperparams_grid_nn <- function(hyperparams, tuner, workflow){


  if (tuner == "Grid Search CV"){

      grid_params = list()

      i = 1

     if (length(hyperparams$n_neurons) > 1){

        grid_params[[i]] = dials::hidden_units() %>% dials::range_set(hyperparams$n_neurons)

         i = i + 1

      }

    if (length(hyperparams$learning_rate) > 1){

        grid_params[[i]] = dials::learn_rate() %>% dials::range_set(hyperparams$learning_rate)

        i = i + 1

      }

    if (length(hyperparams$activation_func) > 1){

        grid_params[[i]] = dials::activation() %>% dials::value_set(hyperparams$activation_func)

        i = i + 1

      }

    grid_params = dials::grid_regular(grid_params)

  } else if (tuner == "Bayesian Optimization"){

     grid_params = workflow %>%
                        workflows::extract_parameter_set_dials() %>%
                        {if (length(hyperparams$n_neurons) > 1) dials::update(.,
                                                                    hidden_units = dials::hidden_units(hyperparams$n_neurons)) else .} %>%
                        {if (length(hyperparams$learning_rate > 1)) dials::update(.,
                                                                    learn_rate = dials::learn_rate(hyperparams$learning_rate)) else .} %>%
                        {if (length(hyperparams$activation_func > 1)) dials::update(.,
                                                                        activation = dials::activation(hyperparams$activation_func)) else .}

  }

  return(grid_params)
}

define_metrics <- function(metrics){

  if (metrics == "Root Mean Squared"){

    metric = yardstick:: rmse

  } else if (metrics == "Mean Absolute Value"){

    metric = yardstick::mae

  } else {

    #### ERROR

    metric = yardstick::accuracy

  }

  return(metric)

}

tune_models <- function(tidy_object, tuner, sampling_method, hyperparams_grid){

        set.seed(123)

        if (tuner == "Bayesian Optimization"){


          tuner_object <- tune::tune_bayes(

            object = tidy_object$workflow,
            resamples = sampling_method,
            metrics = yardstick::metric_set(tidy_object$metrics),
            control = tune::control_bayes(save_pred = TRUE),
            param_info = hyperparams_grid

          )

          tuner_fit <- tuner_object



        } else if (tuner == "Grid Search CV"){

          tuner_object <- tune::tune_grid(

            object = tidy_object$workflow,
            resamples = sampling_method,
            metrics = yardstick::metric_set(tidy_object$metrics),
            control = tune::control_grid(save_pred = TRUE),
            grid = hyperparams_grid

          )


        } else {


          #### ERROR

        }

  return(tuner_object)

}

model_tuning <- function(tidy_object, tuner, metrics){

            tidy_object$add_workflow(create_workflow(tidy_object))

            tidy_object$add_metrics(define_metrics(metrics))

            if (tidy_object$models == "Neural Network"){

              set.seed(123)

              validation_split = rsample::initial_validation_split(tidy_object$full_data, prop = c(0.7, 0.1))

              tidy_object$add_train_data(rsample::training(validation_split))
              tidy_object$add_validation_data(rsample::validation(validation_split))
              tidy_object$add_test_data(rsample::testing(validation_split))

              val_set <- validation_set(validation_split)

              hyperparams = tidy_object$hyperparameters


              if (hyperparams$tuning == TRUE){

                hyperparam_grid = hyperparams_grid_nn(hyperparams)

                tuner_fit = tune_models(tidy_object, "Bayesian Optimization", val_set, hyperparam_grid)

                tidy_object$add_tuner_fit(tuner_fit)

                # ENTRENAMIENTO FINAL
                # =============================================================================

                best_hyper <- workflows::select_best(tuner_fit, metric = metrics)


                final_workflow = workflows::update_model(create_nn(best_hyper, task = task, epochs = 100))

                tidy_object$add_workflow(final_workflow)

                final_model <- workflows::finalize_workflow(
                  x = tidy_object$workflow,
                  parameters = best_hyper
                )

                final_model <- final_model %>%
                  fit(
                    data = tidy_object$train
                  )

                tidy_object$add_final_models(final_model)

              } else{

                ### NO TUNING


              }

            } else{

              ##### OTHER MODELS

              set.seed(123)

  train_test_split <- rsample::initial_split(
    data   = df,
    prop   = 0.8
  )

  train_data <- rsample::training(train_test_split)
  test_data  <- rsample::testing(train_test_split)
              ###### IF HYPS are all length 1, else

}

}
