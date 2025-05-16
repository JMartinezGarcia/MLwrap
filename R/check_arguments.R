########## Preprocessing

check_args_preprocessing <- function(df, formula, task, num_vars, cat_vars,
                         norm_num_vars, encode_cat_vars){

  df_cols = names(df)

  ## Check formula

  model.frame(formula, data = df)

  ## Check task

  check_args_list(arg = task, arg_list = c("regression", "classification"), arg_name = "task", null_valid = F)

  #### OTHER checks

  if (any(!(num_vars %in% df_cols)) && !(is.null(num_vars))) {
    message("num_vars doesn't coincide with data columns")
  }

  if (any(!(cat_vars %in% df_cols)) && !(is.null(cat_vars))) {
    message("cat_vars doesn't coincide with data columns")
  }

  if (any(!(norm_num_vars %in% df_cols)) && !(is.null(norm_num_vars)) && !(norm_num_vars == "all")) {
    message("norm_num_vars doesn't coincide with data columns")
  }

  if (any(!(encode_cat_vars %in% df_cols)) && !(is.null(encode_cat_vars)) && !(encode_cat_vars == "all")) {
    message("encode_cat_vars doesn't coincide with data columns")
  }

}

########## Model building

check_args_build_model <- function(tidy_object, model_names){

  ## Check tidy_object stage

  if (!(tidy_object$stage == "preprocessing")){

    stop(paste0("You must first add a preprocessing step using preprocessing() !!"))

  }

  ## Check model_names

  check_args_list(arg = model_names, arg_list = c("Neural Network",
                                                  "Random Forest",
                                                  "XGBOOST",
                                                  "SVM"
                                                  ),
                  arg_name = "model_names", null_valid = F)

}

########## Check fine_tuning

check_args_fine_tuning <- function(tidy_object, tuner, metrics, plot_results = F, verbose = FALSE){

  ## Check tidy_object stage

  if (!(tidy_object$stage == "build_model")){

    stop("You must first add a model with build_model() !!")

  }

  ## Check tuner

  check_args_list(arg = tuner, arg_list = c("Bayesian Optimization", "Grid Search CV"), arg_name = "tuner",
                  null_valid = F)

  ## Check metrics

  check_args_list(arg = metrics, arg_list = names(metrics_info), arg_name = "metrics", null_valid = F)

  ## Check plot_results

  check_boolean(arg = plot_results, arg_name = "plot_results")

  ## Check verbose

  check_boolean(arg = verbose, arg_name = "verbose")


}

############ Check Results

check_args_show_results <- function(tidy_object,
                                    summary, roc_curve, pr_curve,
                                    gain_curve, lift_curve,
                                    dist_by_class, reliability_plot, confusion_matrix,
                                    scatter_residuals, scatter_predictions, residuals_dist,
                                    new_data){

  ## Check tidy_object stage

  if (!(tidy_object$stage == "fit_model")){

    stop("You must first fit a model with fine_tuning() !!")

  }

  ## Check new_data

  check_args_list(arg = new_data, arg_list = c("train", "test"), arg_name = "new_data", null_valid = F)

  ## Check booleans

  check_boolean(arg = summary, arg_name = "summary")

  check_boolean(arg = roc_curve, arg_name = "roc_curve")

  check_boolean(arg = pr_curve, arg_name = "pr_curve")

  check_boolean(arg = gain_curve, arg_name = "gain_curve")

  check_boolean(arg = lift_curve, arg_name = "lift_curve")

  check_boolean(arg = dist_by_class, arg_name = "dist_by_class")

  check_boolean(arg = reliability_plot, arg_name = "reliability_plot")

  check_boolean(arg = confusion_matrix, arg_name = "confusion_matrix")

  check_boolean(arg = scatter_residuals, arg_name = "scatter_residuals")

  check_boolean(arg = scatter_predictions, arg_name = "scatter_predictions")

  check_boolean(arg = residuals_dist, arg_name = "residuals_dist")

  ## Check classification plots

  regression_plots = c(scatter_residuals, scatter_predictions, residuals_dist)

  if (any(regression_plots) && tidy_object$task == "classification"){

    stop("scatter_residuals, scatter_predictions and residuals_dist are for regression taks only!")

  }

  ## Check regression plot

  classification_plots = c(roc_curve, pr_curve, gain_curve, lift_curve,
                           dist_by_class, reliability_plot, confusion_matrix)

  if(any(classification_plots) && tidy_object$task == "regression"){

    stop("roc_curve, pr_curve, gain_curve, lift_curve, dist_by_class, reliability_plot and confusion
         matrix are for classification task only!")

  }


}

############ Check sensitivity_analysis

check_args_sensitivity_analysis <- function(tidy_object, type, metric){

  ## Check tidy_object stage

  if (!(tidy_object$stage == "fit_model")){

    stop("You must first fit a model with fine_tuning() !!")

  }

  ## Check type

  check_args_list(arg = type, arg_list = c("PFI", "SHAP", "Integrated Gradients", "Olden"),
                  arg_name = "type", null_valid = F)

  if (!(tidy_object$models_names == "Neural Network") && (type == "Integrated Gradients" || type == "Olden")){

    stop("Integrated Gradients and Olden's method are for Neural Networks only!!")
  }
  ## Check metric

  check_args_list(arg = metric, arg_list = names(metrics_info), arg_name = "metric")

}


########## UTILS

check_args_list <- function(arg, arg_list, arg_name, null_valid = T){

  if (!is.null(arg)){

      if (!(arg %in% arg_list)){

        stop(paste0(arg_name, " option not valid. Valid options are: ", paste(arg_list, collapse = ",")))

      }
  } else if (null_valid == F){

    stop(paste0("NULL value for ", arg_name, " is not allowed!!! Valid options are: ",
                paste(arg_list, collapse = ",")))

  }

}

check_boolean <- function(arg, arg_name){

  if (!(class(arg) == "logical")){

    stop(paste0(arg_name, " must be boolean! (TRUE or FALSE)"))

  }

}

