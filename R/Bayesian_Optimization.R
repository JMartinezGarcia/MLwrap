tune_models_bayesian <- function(tidy_object, sampling_method, metrics, seed = 123, verbose = TRUE){

  set.seed(seed)

  bayes_control <-
    tune::control_bayes(
      no_improve    = 5L,
      time_limit    = 20,
      verbose = verbose,
      verbose_iter  = verbose,
      save_pred     = TRUE,
      save_workflow = TRUE

    )

  extracted_hyperparams <- extract_hyperparams(tidy_object)

  tuner_object <-
    tidy_object$workflow %>%
    tune::tune_bayes(
      resamples = sampling_method,
      iter      = 10L,
      control   = bayes_control,
      initial   = 10,
      param_info = extracted_hyperparams,
      metrics = metrics
    )

  return(tuner_object)

}

