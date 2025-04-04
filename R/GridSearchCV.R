tune_models_grid_search_cv <- function(tidy_object, sampling_method, metrics, seed = 123,  verbose = TRUE){

  sampling_method <- rsample::vfold_cv(tidy_object$train_data, v = 5)


  grid_hyperparams = hyperparams_grid(tidy_object$hyperparameters)

  grid_control <- tune::control_grid(

    allow_par     = TRUE,
    save_pred     = TRUE,
    save_workflow = TRUE,
    parallel_over = NULL

  )

  tuner_object <- tune::tune_grid(

    object = tidy_object$workflow,
    resamples = sampling_method,
    metrics = metrics,
    control = grid_control,
    grid = grid_hyperparams

  )

  return(tuner_object)


}
