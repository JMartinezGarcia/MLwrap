######################################################
#         get_results                                #
######################################################

#' Showcase Summary Results and Plots
#'
#' @param tidy_object Tidy_Object created from fine_tuning function.
#' @param summary Whether to plot summary results table. Boolean (FALSE by default).
#' @param roc_curve Whether to plot ROC Curve (Classification task only). Boolean (FALSE by default).
#' @param pr_curve Whether to plot ROC Curve (Classification task only). Boolean (FALSE by default).
#' @param gain_curve Whether to plot ROC Curve (Classification task only). Boolean (FALSE by default).
#' @param lift_curve Whether to plot ROC Curve (Classification task only). Boolean (FALSE by default).
#' @param dist_by_class Whether to plot distribution of output probability by class (Classification task only).
#'  Boolean (FALSE by default).
#' @param reliability_plot Whether to plot Reliability Plot (Binary Classification task only). Boolean (FALSE by default).
#' @param confusion_matrix Whether to Confusion Matrix (Classification task only). Boolean (FALSE by default).
#' @param scatter_residuals Whether to plot Residuals vs Predictions (Regression task only). Boolean (FALSE by default).
#' @param scatter_predictions Whether to plot Predictions vs Observed (Regression task only). Boolean (FALSE by defaut).
#' @param residuals_dist Whether to plot Residuals Distribution (Regression task only). Boolean (FALSE by default).
#' @param new_data Data to be used for Confusion Matrix, Reliability Plot, Distribution by Class Plot,
#'        Residuals vs Predictions Plot, Predictions vs Observed Plot and Residuals Distribution Plot.
#'        A string with the name of the data_set: "train", "validation", "test" (default) or "all".
#' @returns Updated tidy_object
#' @export
show_results <- function(analysis_object,
                        summary = FALSE, roc_curve = FALSE, pr_curve = FALSE,
                        gain_curve = FALSE, lift_curve = FALSE,
                        dist_by_class = FALSE, reliability_plot = FALSE, confusion_matrix = FALSE,
                        scatter_residuals = FALSE, scatter_predictions = FALSE, residuals_dist = FALSE,
                        new_data = "test"){

  check_args_show_results(analysis_object = analysis_object,
                          summary = summary, roc_curve = roc_curve, pr_curve = pr_curve,
                          gain_curve = gain_curve, lift_curve = lift_curve,
                          dist_by_class = dist_by_class, reliability_plot = reliability_plot,
                          confusion_matrix = confusion_matrix, scatter_residuals = scatter_residuals,
                          scatter_predictions = scatter_predictions, residuals_dist = residuals_dist,
                          new_data = new_data)

  predictions = get_predictions(analysis_object, "all")

  analysis_object$modify("predictions", predictions)

  pred_test = predictions %>% filter(data_set == "test")

  summary_results = summary_results(analysis_object, pred_test)

  analysis_object$modify("fit_summary",summary_results)

  print("############# Showing Results")

  if (summary == T){

    # Multiclass classification case

    if (analysis_object$outcome_levels > 2){

      summary_results %>%
        dplyr::mutate(across(where(is.numeric), ~ signif(., 3))) %>%
        dplyr::select(-c(Class)) %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column("Metric") %>%
        ggpubr::ggtexttable(rows = NULL) %>%
        print()

    } else {

    summary_results %>%
      dplyr::mutate(across(where(is.numeric), ~ signif(., 3))) %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Metric") %>%
      dplyr::rename(Value = 2) %>%
      ggpubr::ggtexttable(rows = NULL) %>%
      print()

    }

  }

  if (roc_curve == T){

    if (analysis_object$outcome_levels == 2){

        p <- predictions %>%
        plot_roc_curve_binary(new_data = "all") %>%
        autoplot() +
        ggplot2::labs(title = "ROC Curve")

        print(p)


    } else {

      p <-  predictions %>%
            plot_roc_curve_multiclass(new_data = "all") %>%
            autoplot() +
            ggplot2::labs(title = "ROC Curve")

      print(p)

    }

  }

  if (pr_curve == T){

    if (analysis_object$outcome_levels == 2){

    p <- predictions %>%
         plot_pr_curve_binary(new_data = "all") %>%
         autoplot() +
         ggplot2::labs(title = "Precision Recall Curve")

    print(p)

    } else {

      p <- predictions %>%
           plot_pr_curve_multlicass(new_data = "all") %>%
           autoplot() +
           ggplot2::labs(title = "Precision Recall Curve")

      print(p)

    }

  }

  if (gain_curve == T){

    if (analysis_object$outcome_levels == 2){

     p <- predictions %>%
          plot_gain_curve_binary() %>%
          autoplot() +
          ggplot2::labs(title = "Gain Curve")

    print(p)

    } else {

    p <-predictions %>%
        plot_gain_curve_multiclass() %>%
        autoplot() +
        ggplot2::labs(title = "Gain Curve")

    print(p)

    }

  }

  if (lift_curve == T){

    if (analysis_object$outcome_levels == 2){

      p <- predictions %>%
           plot_lift_curve_binary(new_data = "all") %>%
           autoplot() +
           ggplot2::labs(title = "Lift Curve")

      print(p)

    } else{


      p <- predictions %>%
           plot_lift_curve_multiclass(new_data = "all") %>%
           autoplot() +
           ggplot2::labs(title = "Lift Curve")

      print(p)

    }

  }

  if (dist_by_class == T){

    if (analysis_object$outcome_levels == 2){

      pred_test %>%
        plot_dist_probs_binary(new_data) %>%
        print()

    } else {

      pred_test %>%
        plot_dist_probs_multiclass() %>%
        print()

    }
  }

  if (reliability_plot == T){

    if (analysis_object$outcome_levels > 2){stop("Reliability Plot not implemented for Multiclass Classification!")}

    pred_test %>%
      plot_calibration_curve_binary(new_data = new_data) %>%
      print()
  }

  if (confusion_matrix == T){


     p <- pred_test %>%
      plot_conf_mat(new_data = new_data) %>%
      autoplot(type = "heatmap") +
      ggplot2::labs(title = "Confusion Matrix")

      print(p)

  }

  if (scatter_residuals == T){

    pred_test %>%
      plot_scatter(new_data = new_data, error = T) %>%
      print()

  }

  if (scatter_predictions == T){

    pred_test %>%
      plot_scatter(new_data = new_data, error = F) %>%
      print()

  }

  if (residuals_dist == T){

    pred_test %>%
      plot_residuals_density(new_data = new_data) %>%
      print()

  }

  return(analysis_object)




}

modify_datasets <- function(analysis_object){

  rec <- recipes::prep(analysis_object$transformer,
                       training = analysis_object$train_data,
                       strings_as_factors = T)

  new_train <- recipes::bake(rec, new_data = analysis_object$train_data)
  new_test <- recipes::bake(rec, new_data = tidy_object$test_data)

  tidy_object$modify("train_data", new_train)
  tidy_object$modify("test_data", new_test)

  if (!is.null(tidy_object$validation_data)){

    new_validation <- recipes::bake(rec, new_data = tidy_object$validation_data)
    tidy_object$modify("validation_data", new_validation)

  }

  print(new_train)

  return(tidy_object)

}

######################################################
#         get_predictions                           #
######################################################

get_predictions <- function(analysis_object, new_data = "test"){

  if (analysis_object$task == "regression"){

    predictions = get_predictions_regression(analysis_object, new_data = new_data)

  } else if (analysis_object$task == "classification"){

    predictions = get_predictions_binary(analysis_object, new_data = new_data)

    if (analysis_object$outcome_levels == 2){

      predictions = get_predictions_binary(analysis_object, new_data = new_data)

    } else {

      predictions = get_predictions_multiclass(analysis_object, new_data = new_data)

    }

  }

  return(predictions)


}

######################################################
#         SUMMARY                                    #
######################################################

summary_results <- function(analysis_object, predictions, new_data = "test"){

  if (analysis_object$task == "regression"){

    return(summary_regression(predictions, new_data))

  } else if (analysis_object$task == "classification"){

    if (analysis_object$outcome_levels == 2){

      return(summary_binary(predictions, new_data))

    } else {

      return(summary_multiclass_per_class(predictions, new_data))

    }

  }

}



