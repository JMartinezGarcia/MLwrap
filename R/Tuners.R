create_workflow <- function(tidy_object){

  workflow = workflows::workflow() %>%
    workflows::add_recipe(tidy_object$transformer) %>%
    workflows::add_model(tidy_object$models)

  return(workflow)
}

split_data <- function(tidy_object, prop_train = 0.6, prop_val = 0.2, model_name = "Other"){

  if (model_name == "Neural Network"){

  validation_split = rsample::initial_validation_split(tidy_object$full_data, prop = c(prop_train, prop_val))

  tidy_object$modify("train_data", rsample::training(validation_split))
  tidy_object$modify("validation_data", rsample::validation(validation_split))
  tidy_object$modify("test_data", rsample::testing(validation_split))

  sampling_method <- rsample::validation_set(validation_split)

  final_split = rbind(tidy_object$train_data, tidy_object$validation_data)

  }

  else{

    train_test_split = rsample::initial_split(tidy_object$full_data, prop = prop_train)

    tidy_object$modify("train_data", rsample::training(train_test_split))
    tidy_object$modify("test_data", rsample::testing(train_test_split))

    sampling_method <- rsample::vfold_cv(tidy_object$train_data, v = 5)

    final_split <- tidy_object$train_data

  }

  return(list(sampling_method = sampling_method, final_split = final_split))

}

create_metric_set <- function(metrics){

  set_metrics <- yardstick::metric_set(!!!rlang::syms(metrics))

  return(set_metrics)

}

extract_hyperparams <- function(tidy_object){

  extracted_hyperparams <-
    tidy_object$workflow %>%
    workflows::extract_parameter_set_dials() %>%
    update(!!!tidy_object$hyperparameters$hyperparams_ranges)

  return(extracted_hyperparams)

}


hyperparams_grid <- function(hyperparams, levels = 5){

  grid = dials::grid_regular(hyperparams$hyperparams_ranges, levels = levels)

  return(grid)

}

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

  print("COMMENCING BAYESIAN OPTIMIZATION")

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

  print("FINISHED BAYESIAN OPTIMIZATION")

  return(tuner_object)

}

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
