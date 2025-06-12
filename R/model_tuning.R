#' Fine Tune ML Model
#'
#' The **fine_tuning()** function performs automated hyperparameter optimization for ML workflows encapsulated
#' within an AnalysisObject. It supports different tuning strategies, such as **Bayesian Optimization** and
#' **Grid Search Cross-Validation**, allowing the user to specify evaluation metrics and whether to visualize
#' tuning results. The function first validates arguments and updates the workflow and metric settings within
#' the AnalysisObject. If hyperparameter tuning is enabled, it executes the selected tuning procedure,
#' identifies the best hyperparameter configuration based on the specified metrics, and updates the workflow
#' accordingly. For neural network models, it also manages the creation and integration of new model instances
#' and provides additional visualization of training dynamics. Finally, the function fits the optimized model to
#' the training data and updates the AnalysisObject, ensuring a reproducible and efficient model selection process
#' (Bartz et al., 2023).
#'
#' @param analysis_object analysis_object created from build_model function.
#' @param tuner Name of the Hyperparameter Tuner. A string of the tuner name: "Bayesian Optimization" or
#'     "Grid Search CV".
#' @param metrics Metric used for Model Selection. A string of the name of metric (see Metrics). By default
#'    either "rmse" (regression) or "roc_auc" (classification).
#' @param plot_results Whether to plot the tuning results. Boolean TRUE or FALSE (default).
#' @param verbose Whether to show tuning process. Boolean TRUE or FALSE (default).
#'
#' @section Tuners:
#'
#' ## Bayesian Optimization
#'
#' * Initial data points: 20
#' * Maximum number of iterations: 25
#' * Convergence after 5 iterations without improvement
#' * Train / Validation / Test : 0.6 / 0.2 / 0.2
#'
#' ## Grid Search CV
#'
#' * Number of Folds: 5
#' * Maximum levels per hyperparameter: 10
#' * Train / Test : 0.75 / 0.25
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
#' @returns An updated analysis_object containing the fitted model with optimized hyperparameters,
#' the tuning results, and all relevant workflow modifications. This object includes the final trained
#' model, the best hyperparameter configuration, tuning diagnostics, and, if applicable, plots of the
#' tuning process. It can be used for further model evaluation, prediction, or downstream analysis within
#' the package workflow.
#' @examples
#' # Example 1: Fine tuning function applied to a regression task
#'
#' library(TidyML)
#'
#' data(sim_data) # sim_data is a simulated dataset wtih psychological variables
#'
#' tidy_object <- preprocessing(
#'            df = sim_data,
#'            formula = psych_well ~ depression + emot_intel + resilience + life_sat,
#'            task = "regression"
#'            )
#'
#' tidy_object <- build_model(
#'                analysis_object = tidy_object,
#'                model_name = "Random Forest",
#'                hyperparameters = list(
#'                      mtry = 3,
#'                      trees = 50
#'                      )
#'                  )
#'
#' tidy_object <- fine_tuning(tidy_object,
#'                 tuner = "Grid Search CV",
#'                 metrics = c("rmse"),
#'                 plot_results = TRUE
#'                )
#'
#' @references
#' Bartz, E., Bartz-Beielstein, T., Zaefferer, M., & Mersmann, O. (2023). *Hyperparameter tuner
#' for Machine and Deep Learning with R. A Practical Guide*. Springer, Singapore.
#' https://doi.org/10.1007/978-981-19-5170-1
#' @export
fine_tuning <- function(analysis_object, tuner, metrics = NULL, plot_results = F, verbose = FALSE){

            check_args_fine_tuning(analysis_object = analysis_object, tuner = tuner, metrics = metrics,
                                   plot_results = plot_results, verbose = verbose)

            analysis_object = analysis_object$clone()

            if (is.null(metrics)){

              if (analysis_object$task == "regression"){metrics = "rmse"}
              else {metrics = "roc_auc"}

            }

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

            split_final_data <- split_data(analysis_object)

            sampling_method = split_final_data$sampling_method

            final_data = split_final_data$final_split

            if (analysis_object$hyperparameters$tuning == TRUE){

                cat("Commencing Tuning...")

                tuner_fit = tune_models(analysis_object,
                                        tuner,
                                        sampling_method,
                                        metrics = set_metrics,
                                        verbose = verbose)

                cat("Tuning Finalized \n")

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

              #torch::torch_manual_seed(123)

              new_mlp_model = create_nn(hyperparams = new_hyperparams_nn, task = analysis_object$task, epochs = 100)

              new_workflow <- analysis_object$workflow %>%
                workflows::update_model(new_mlp_model)

              analysis_object$modify("workflow", new_workflow)

            }

            final_model <-  tune::finalize_workflow(analysis_object$workflow ,final_hyperparams)

            final_model <- fit(final_model, final_data)

            analysis_object$modify("final_model", final_model)

            if (analysis_object$model_name == "Neural Network"){

                model_parsnip <- tune::extract_fit_parsnip(final_model)

                cat("###### Loss Curve ######")
                cat("\n")

                p <- brulee::autoplot(model_parsnip) +
                     ggplot2::labs(title = "Neural Network Loss Curve")

                plot_ob = analysis_object$plots

                plot_ob$nn_loss_curve = p

                analysis_object$modify("plots", plot_ob)

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


