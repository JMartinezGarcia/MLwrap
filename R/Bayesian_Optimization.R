tune_models_bayesian <- function(analysis_object, sampling_method, metrics, verbose, seed = 123){

  bayes_control <-
    tune::control_bayes(
      no_improve    = 5L,
      time_limit    = 20,
      save_pred     = TRUE,
      save_workflow = TRUE,
      verbose = verbose,
      verbose_iter = verbose
    )
  
  n_combinations <- count_possible_combinations(analysis_object)
  
  n_initial <- min(20L, n_combinations %/% 2L)
  
  n_iter <- min(25L, n_combinations - n_initial)
  
  extracted_hyperparams <- extract_hyperparams(analysis_object)
  
  tuner_object <- tune::tune_bayes(
    object     = analysis_object$workflow,
    resamples  = sampling_method,
    iter       = n_iter,
    control    = bayes_control,
    initial    = n_initial,
    param_info = extracted_hyperparams,
    metrics    = metrics
  )

  # tuner_object <- tryCatch(
  #   suppressMessages(suppressWarnings(
  #     tune::tune_bayes(
  #       object     = analysis_object$workflow,
  #       resamples  = sampling_method,
  #       iter       = 25L,
  #       control    = bayes_control,
  #       initial    = 20,
  #       param_info = extracted_hyperparams,
  #       metrics    = metrics
  #     )
  #   )),
  #   error = function(e) {
  #     msg <- conditionMessage(e)
  #     if (grepl("\\$ operator is invalid for atomic vectors", msg, fixed = TRUE)) {
  #       if (base::interactive()) {
  #         cli::cli_alert_warning("Error interno de metricas suprimido; el tuning continua.")
  #       }
  #       return(tibble::tibble(.config = NA)[0, ])
  #     }
  #     stop(e)
  #   }
  # )

  return(tuner_object)
}

