model_tuning <- function(tidy_object, tuner, metrics, verbose = TRUE){

            tidy_object$modify("workflow", create_workflow(tidy_object))

            tidy_object$modify("metrics", metrics)

            set_metrics <- create_metric_set(tidy_object$metrics)

            set.seed(123)

            split_final_data <- split_data(tidy_object, model = tidy_object$models_names)
            sampling_method = split_final_data$sampling_method
            final_data = split_final_data$final_split

            if (tidy_object$hyperparameters$tuning == TRUE){

                tuner_fit = tune_models(tidy_object,
                                        tuner,
                                        sampling_method,
                                        metrics = set_metrics,
                                        verbose = verbose)

                tidy_object$modify("tuner_fit", tuner_fit)

                # ENTRENAMIENTO FINAL
                # ============================================================================

                best_hyper <- tune::select_best(tuner_fit, metric = tidy_object$metrics)

                final_hyperparams <- c(as.list(best_hyper),
                                       tidy_object$hyperparameters$hyperparams_constant
                                       )

            } else{

                ### NO TUNING

                final_hyperparams <- tidy_object$hyperparameters$hyperparams_constant

            }

            if (tidy_object$models_names == "Neural Network"){

              final_hyperparams <- c(final_hyperparams, epochs = 100)

            }

            final_model <- tidy_object$workflow %>%

              tune::finalize_workflow(final_hyperparams) %>%

              fit(final_data)

            tidy_object$modify("final_models", final_model)

            return(tidy_object)

}

tune_models <- function(tidy_object, tuner, sampling_method, metrics, verbose = TRUE){

  if (tuner == "Bayesian Optimization"){

    tuner_object <- tune_models_bayesian(tidy_object, sampling_method, metrics = metrics,  verbose = verbose)

  } else if (tuner == "Grid Search CV"){

    tuner_object <- tune_models_grid_search_cv(tidy_object, sampling_method, metrics = metrics, verbose = verbose)

  }

  else {

    ##### ERRRORRRR

  }

  return(tuner_object)

}


