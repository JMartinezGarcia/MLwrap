### Tuner Plots ###

#' Plotting Tuner Search Results
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#'
#' @export
plot_tuning_results <- function(analysis_object){

  if ((analysis_object$stage != "fit_model") && (analysis_object$stage != "evaluated_model")){

    stop("You must first fit a model with 'fine_tuning()'!")

  }

  if (analysis_object$hyperparameters$tuning == FALSE){

    stop("All hyperparameters are fixed values, no tuning was performed!")

  }

  plots <- analysis_object$plots

  if (analysis_object$tuner == "Bayesian Optimization"){

    plot(plots$bayesian_opt_iter_loss)

    plot(plots$bayesian_opt_iter_results)

  }

  plot(plots$tuner_search_results)

  invisible(analysis_object)

}


#' Plot Neural Network Loss Curve
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#'
#' @export
plot_loss_curve <- function(analysis_object){

  if ((analysis_object$stage != "fit_model") && (analysis_object$stage != "evaluated_model")){

    stop("You must first fit a Neural Network model with 'fine_tuning()'!")

  }

  if (analysis_object$model_name != "Neural Network"){

    stop("Loss curve is only available for Neural Network models!")

  }

  plots <- analysis_object$plots

  p <- plots$nn_loss_curve

  plot(p)

}




### Regression Plots ###

#' Plotting Residuals Distribution
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#'
#' @export
plot_residuals_distribution <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_regression_plot(analysis_object)

  p_train <- plots$residuals_dist_train

  p_test <- plots$residuals_dist_test

  plot(patchwork::wrap_plots(p_train, p_test, nrow = 2))

  invisible(analysis_object)

  }

#' Plotting Residuals vs Predictions
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#'
#' @export
plot_scatter_residuals <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_regression_plot(analysis_object)

  p_train <- plots$scatter_residuals_train

  p_test <- plots$scatter_residuals_test

  plot(patchwork::wrap_plots(p_train, p_test, nrow = 2))
  invisible(analysis_object)

}

#' Plotting Observed vs Predictions
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#'
#' @export
plot_scatter_predicitions <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_regression_plot(analysis_object)

  p_train <- plots$scatter_predictions_train

  p_test <- plots$scatter_predictions_test

  plot(patchwork::wrap_plots(p_train, p_test, ncol = 2))

  invisible(analysis_object)

}

### Classification Plots ###

#' Plotting Confusion Matrix
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#'
#' @export
plot_confusion_matrix <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_classification_plot(analysis_object)

  p_train <- plots$confusion_matrix_train

  p_test <- plots$confusion_matrix_test

  plot(patchwork::wrap_plots(p_train, p_test, nrow = 2))

  invisible(analysis_object)

}

#' Plotting ROC Curve
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#'
#' @export
plot_roc_curve <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_classification_plot(analysis_object)

  p <- plots$roc_curve

  plot(p)

  invisible(analysis_object)

}

#' Plotting Precision-Recall Curve
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#'
#' @export
plot_pr_curve <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_classification_plot(analysis_object)

  p <- plots$pr_curve

  plot(p)

  invisible(analysis_object)

}

#' Plotting Gain Curve
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#'
#' @export
plot_gain_curve <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_classification_plot(analysis_object)

  p <- plots$gain_curve

  plot(p)

  invisible(analysis_object)

}

#' Plotting Lift Curve
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#'
#' @export
plot_lift_curve <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_classification_plot(analysis_object)

  p <- plots$lift_curve

  plot(p)

  invisible(analysis_object)

}

#' Plotting Output Distribution By Class
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#'
#' @export
plot_distribution_by_class <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_classification_plot(analysis_object)

  p_train <- plots$dist_by_class_train

  p_test <- plots$dist_by_class_test

  plot(patchwork::wrap_plots(p_train, p_test, nrow = 2))

  invisible(analysis_object)

}

#' Plotting Calibration Curve
#'
#' Binary Classification Only
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#'
#' @export
plot_calibration_curve <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_classification_plot(analysis_object)

  if (analysis_object$outcome_levels > 2){

    stop("Calibration Curve is only implemented for binary classification!")

  }

  p_train <- plots$reliability_plot_train

  p_test <- plots$reliability_plot_test

  plot(patchwork::wrap_plots(p_train, p_test, nrow = 2))

  invisible(analysis_object)

}

### Sensitivity Analysis

#' Plotting Permutation Feature Importance Barplot
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "PFI")'.
#'
#' @param show_table Boolean. Whether to print PFI results table.
#'
#' @returns analysis_object
#'
#' @export
plot_pfi <- function(analysis_object, show_table = FALSE){

  plots <- analysis_object$plots

  pfi_names   <- grep("^PFI", names(plots), value = TRUE)

  if (length(pfi_names) == 0){

    stop("You need to calculate PFI values first with 'sensitivity_analysis()'!")

  }

  pfi_plots   <- plots[pfi_names]

  combined <- patchwork::wrap_plots(pfi_plots)

  if (base::interactive()){

    if (show_table == TRUE){

      tables <- table_pfi_results(analysis_object, show_table = TRUE)

    }

    plot(combined)

  }

  invisible(analysis_object)

}

#' Plotting SHAP Plots
#'
#' Mean Abs Barplot, Directional Barplot, Boxplot, Swarmplot
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "SHAP")'.
#'
#' @param show_table Boolean. Whether to print SHAP summarized results table.
#'
#' @returns analysis_object
#'
#' @export
plot_shap <- function(analysis_object, show_table = FALSE){

  plots <- analysis_object$plots

  shap_names   <- grep("^SHAP", names(plots), value = TRUE)

  if (length(shap_names) == 0){

    stop("You need to calculate SHAP values first with 'sensitivity_analysis()'!")

  }

  mean_shap_names <- grep("barplot$", shap_names, value = TRUE)

  dir_shap_names <- grep("directional$", shap_names, value = TRUE)

  box_shap_names <- grep("boxplot$", shap_names, value = TRUE)

  swarm_shap_names <- grep("swarmplot$", shap_names, value = TRUE)

  mean_shap_plots <- plots[mean_shap_names]

  dir_shap_plots <- plots[dir_shap_names]

  box_shap_plots <- plots[box_shap_names]

  swarm_shap_plots <- plots[swarm_shap_names]

  combined_mean <- patchwork::wrap_plots(mean_shap_plots)
  combined_dir <- patchwork::wrap_plots(dir_shap_plots)
  combined_box <- patchwork::wrap_plots(box_shap_plots)
  combined_swarm <- patchwork::wrap_plots(swarm_shap_plots)

  if (base::interactive()){

    if (show_table == TRUE){

    tables <- table_shap_results(analysis_object, show_table = TRUE)

    }

    plot(combined_mean)
    plot(combined_dir)
    plot(combined_box)
    plot(combined_swarm)

    }

  invisible(analysis_object)

}

#' Plotting Integrated Gradients Plots
#'
#' Mean Abs Barplot, Directional Barplot, Boxplot, Swarmplot
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "Integrated Gradients")'.
#'
#' @param show_table Boolean. Whether to print Integrated Gradients summarized results table.
#'
#' @returns analysis_object
#'
#' @export
plot_integrated_gradients <- function(analysis_object, show_table = FALSE){

  plots <- analysis_object$plots

  ig_names   <- grep("^Integrated", names(plots), value = TRUE)

  if (length(ig_names) == 0){

    stop("You need to calculate Integrated Gradients values first with 'sensitivity_analysis()'!")

  }

  mean_ig_names <- grep("barplot$", ig_names, value = TRUE)

  dir_ig_names <- grep("directional$", ig_names, value = TRUE)

  box_ig_names <- grep("boxplot$", ig_names, value = TRUE)

  swarm_ig_names <- grep("swarmplot$", ig_names, value = TRUE)

  mean_ig_plots <- plots[mean_ig_names]

  dir_ig_plots <- plots[dir_ig_names]

  box_ig_plots <- plots[box_ig_names]

  swarm_ig_plots <- plots[swarm_ig_names]

  combined_mean <- patchwork::wrap_plots(mean_ig_plots)
  combined_dir <- patchwork::wrap_plots(dir_ig_plots)
  combined_box <- patchwork::wrap_plots(box_ig_plots)
  combined_swarm <- patchwork::wrap_plots(swarm_ig_plots)

  if (base::interactive()){

    if (show_table){

      tables <- table_integrated_gradients_results(analysis_object, show_table = TRUE)

    }

    plot(combined_mean)
    plot(combined_dir)
    plot(combined_box)
    plot(combined_swarm)

  }

  invisible(analysis_object)

}

#' Plotting Olden Values Barplot
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "Olden")'.
#'
#' @param show_table Boolean. Whether to print Olden results table.
#'
#' @returns analysis_object
#'
#' @export
plot_olden <- function(analysis_object, show_table = FALSE){

  plots <- analysis_object$plots

  olden_names   <- grep("^Olden", names(plots), value = TRUE)

  if (length(olden_names) == 0){

    stop("You need to calculate Olden values first with 'sensitivity_analysis()'!")

  }

  olden_plots <- plots[olden_names]

  combined <- patchwork::wrap_plots(olden_plots)

  if (base::interactive()){

    if (show_table == TRUE){

      tables <- table_olden_results(analysis_object, show_table = TRUE)

    }

    plot(combined)

  }

  invisible(analysis_object)

}

#' Plotting Sobol-Jansen Values Barplot
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "Sobol_Jansen")'.
#'
#' @param show_table Boolean. Whether to print Sobol-Jansen results table.
#'
#' @returns analysis_object
#'
#' @export
plot_sobol_jansen <- function(analysis_object, show_table = FALSE){

  plots <- analysis_object$plots

  if (is.null(plots$Sobol_Jansen)){

    stop("You need to calculate Sobol_Jansen values first with 'sensitivity_analysis()'!")

  }

  if (base::interactive()){

    if (show_table == TRUE){

      tables <- table_sobol_jansen_results(analysis_object, show_table = TRUE)

    }

    plot(plots$Sobol_Jansen)

  }

  invisible(analysis_object)

}
