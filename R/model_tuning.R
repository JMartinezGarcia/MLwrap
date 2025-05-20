#' Fine Tune ML Model
#'
#' @param tidy_object Tidy_Object created from build_model function.
#' @param tuner Name of the Hyperparameter Tuner. A string of the tuner name: "Bayesian Optimization" or
#'     "Grid Search CV".
#' @param metrics Metric used for Model Selection. A string of the name of metric (see Metrics).
#' @param plot_results Whether to plot the tuning results. Boolean TRUE or FALSE (default).
#' @param verbose Whether to show tuning process. Boolean TRUE or FALSE (default).
#' @returns Updated tidy_object
#'
#' @section Metrics:
#'
#' ## Regression Metrics
#'
#' * rmse
#' * mae
#' * mpe
#' * mape
#' * ccc
#' * smape
#' * rpiq
#' * rsq
#'
#' ## Classification Metrics
#'
#' * accuracy
#' * bal_accuracy
#' * recall
#' * sensitivity
#' * specificity
#' * kap
#' * f_meas
#' * mcc
#' * j_index
#' * detection_prevelance
#' * roc_auc
#' * pr_auc
#' * gain_capture
#' * brier_class
#' * roc_aunp
#'
#'
#' @export
fine_tuning <- function(analysis_object, tuner, metrics, plot_results = F, verbose = FALSE){

            check_args_fine_tuning(analysis_object = analysis_object, tuner = tuner, metrics = metrics,
                                   plot_results = plot_results, verbose = verbose)

            analysis_object$modify("workflow", create_workflow(analysis_object))

            if (!all(metrics %in% names(metrics_info))) {
              invalid_metrics <- metrics[!(metrics %in% names(metrics_info))]
              stop(paste0(
                "Unrecognized metric(s):\n ", paste(invalid_metrics, collapse = ", "),
                ". \n\nChoose from:\n ", paste(names(metrics_info), collapse = ", ")
              ))
            }

            analysis_object$modify("metrics", metrics)

            analysis_object$modify("tuner", tuner)

            set_metrics <- create_metric_set(analysis_object$metrics)

            set.seed(123)

            split_final_data <- split_data(analysis_object)

            sampling_method = split_final_data$sampling_method

            final_data = split_final_data$final_split

            if (analysis_object$hyperparameters$tuning == TRUE){

                print("Commencing Tuning...")

                tuner_fit = tune_models(analysis_object,
                                        tuner,
                                        sampling_method,
                                        metrics = set_metrics,
                                        verbose = verbose)

                print("Tuning Finalized")

                analysis_object$modify("tuner_fit", tuner_fit)

                if (plot_results == T){

                  plot_tuning_results(analysis_object)

                }

                # FINAL TRAINING
                # ============================================================================

                best_hyper <- tune::select_best(tuner_fit, metric = analysis_object$metrics[1])

                final_hyperparams <- c(as.list(best_hyper),
                                       analysis_object$hyperparameters$hyperparams_constant
                                       )

            } else{

                ### NO TUNING

                final_hyperparams <- analysis_object$hyperparameters$hyperparams_constant

            }

            if (analysis_object$model_name == "Neural Network"){

              new_hyperparams_nn = HyperparamsNN$new(final_hyperparams[!names(final_hyperparams) %in% ".config"])

              new_mlp_model = create_nn(hyperparams = new_hyperparams_nn, task = analysis_object$task, epochs = 100)

              new_workflow <- analysis_object$workflow %>%
                workflows::update_model(new_mlp_model)

              analysis_object$modify("workflow", new_workflow)

            }

            set.seed(123)

            final_model <- analysis_object$workflow %>%

              tune::finalize_workflow(final_hyperparams)  %>%

              fit(final_data)

            analysis_object$modify("final_model", final_model)

            if (analysis_object$model_name == "Neural Network"){

                model_parsnip <- tune::extract_fit_parsnip(final_model)

                print("############# Loss Curve")

                p <- autoplot(model_parsnip) +
                     ggplot2::labs(title = "Neural Network Loss Curve")

                print(p)

                p <- graph_nn(model_parsnip)

                print(p)

            }

            analysis_object$modify("stage", "fit_model")

            return(analysis_object)

}

tune_models <- function(analysis_object, tuner, sampling_method, metrics, verbose = TRUE){

  if (tuner == "Bayesian Optimization"){

    tuner_object <- tune_models_bayesian(analysis_object, sampling_method, metrics = metrics,  verbose = verbose)

  } else if (tuner == "Grid Search CV"){

    tuner_object <- tune_models_grid_search_cv(analysis_object, sampling_method, metrics = metrics, verbose = verbose)

  }

  else {

    ##### ERRRORRRR

    stop("Unrecognized Tuner. Select from: 'Bayesian Optimization', 'Grid Search CV'.")

  }

  return(tuner_object)

}


