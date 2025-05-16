create_workflow <- function(tidy_object){

  workflow = workflows::workflow() %>%
    workflows::add_recipe(tidy_object$transformer) %>%
    workflows::add_model(tidy_object$model)

  return(workflow)
}

split_data <- function(tidy_object, prop_train = 0.6, prop_val = 0.2){

  model_name = tidy_object$model_name

  if (model_name == "Neural Network"){

  validation_split = rsample::initial_validation_split(tidy_object$full_data, prop = c(prop_train, prop_val))

  tidy_object$modify("train_data", rsample::training(validation_split))
  tidy_object$modify("validation_data", rsample::validation(validation_split))
  tidy_object$modify("test_data", rsample::testing(validation_split))

  sampling_method <- rsample::validation_set(validation_split)

  final_split = rbind(tidy_object$train_data, tidy_object$validation_data)

  }

  else{

    train_test_split = rsample::initial_split(tidy_object$full_data, prop = 0.75)

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

plot_tuning_results <- function(tidy_object){

  print("############# Hyperparameter Tuning Results")

  if (tidy_object$tuner == "Bayesian Optimization"){

    p <- tidy_object$tuner_fit %>%
      tune::autoplot(type = "performance") +
      ggplot2::labs(title = "Bayesian Optimization Iteration Loss")
      print(p)

    p <- tidy_object$tuner_fit %>%
      tune::autoplot(., search_res, type = "parameters") +
      ggplot2::labs(x = "Iterations", y = NULL, title = "Bayesian Optimization Iteration Results")
      print(p)


  }

  p <- tidy_object$tuner_fit %>%
       tune::autoplot() +
       ggplot2::labs(title = paste0(tidy_object$tuner, " Search Results"))

       print(p)

  print("############# Best Hyperparameters Found:")

  tidy_object$tuner_fit %>%
    tune::show_best(metric = tidy_object$metrics[1], n = 1) %>%
    print()

}
