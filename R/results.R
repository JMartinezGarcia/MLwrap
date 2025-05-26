######################################################
#         get_results                                #
######################################################

#' Showcase Summary Results and Plots
#'
#' The **show_results()** function is a central component of the ML workflow established by the package,
#' following the stages of data preprocessing, model construction (build_model), and hyperparameter optimization
#' (fine_tuning). After a model has been trained and tuned, show_results() enables users to generate comprehensive
#' **visualizations and summaries of model performance**, including metrics tables, ROC and PR curves, gain and lift
#' curves, confusion matrices, calibration plots, and regression diagnostics, tailored to both regression and
#' classification tasks. This function provides a thorough and interpretable assessment of the fitted model,
#' supporting informed evaluation and communication of results. Importantly, show_results() is not the final step
#' of the workflow, as further analyses such as sensitivity analysis can be performed subsequently to deepen the
#' understanding of model robustness and behavior (Molnar, 2025).
#'
#' @param analysis_object analysis_object created from fine_tuning function.
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
#' @param scatter_predictions Whether to plot Predictions vs Observed (Regression task only). Boolean (FALSE by default).
#' @param residuals_dist Whether to plot Residuals Distribution (Regression task only). Boolean (FALSE by default).
#' @param new_data Data to be used for Confusion Matrix, Reliability Plot, Distribution by Class Plot,
#'        Residuals vs Predictions Plot, Predictions vs Observed Plot and Residuals Distribution Plot.
#'        A string with the name of the data_set: "train", "validation", "test" (default) or "all".
#' @returns An updated analysis_object containing the generated predictions, summary statistics, and any
#' visualizations or diagnostic outputs selected by the user. This object reflects the results of model
#' evaluation and can be further used for reporting, interpretation, or additional analyses within the
#' ML tidyML workflow.
#' @examples
#' # Example 1: Classification Task
#' # Display summary metrics, ROC curve, and confusion matrix for a classification model with test partition
#'
#' library(TidyML)
#'
#' data(sim_data) # sim_data is a simulated dataset wtih psychological variables
#'
#' tidy_object <- preprocessing(
#'                              df = sim_data,
#'                              formula = psych_well_bin ~ depression + emot_intel + resilience + life_sat,
#'                              task = "classification"
#'                              )
#'
#' tidy_object <- build_model(
#'                analysis_object = tidy_object,
#'                model_name = "SVM",
#'                hyperparameters = list(
#'                                  type = "rbf",
#'                                  cost = 1,
#'                                  margin = 0.1,
#'                                  rbf_sigma = 0.05
#'                                  )
#'                            )
#'
#' tidy_object <- fine_tuning(tidy_object,
#'                              tuner = "Grid Search CV",
#'                              metrics = c("roc_auc", "f_meas"),
#'                              plot_results = TRUE
#'                              )
#'
#' tidy_object<-show_results(tidy_object,
#'                           summary = TRUE,
#'                           roc_curve = TRUE,
#'                           confusion_matrix = TRUE,
#'                           new_data = "test")
#'
#' # Example 2: Regression Task
#' # Display summary metrics, scatter plot of predictions, and residuals distribution for a regression model
#' # with train partition
#'
#' data(sim_data) # sim_data is a simulated dataset wtih psychological variables
#'
#' tidy_object <- preprocessing(
#'                              df = sim_data,
#'                              formula = psych_well ~ depression + emot_intel + resilience + life_sat,
#'                              task = "regression"
#'                              )
#'
#' tidy_object <- build_model(
#'                analysis_object = tidy_object,
#'                model_name = "Neural Network",
#'                hyperparameters = list(
#'                                  hidden_units = 10,
#'                                  activation = "relu",
#'                                  learn_rate = 0.01
#'                                  )
#'                            )
#'
#' tidy_object <- fine_tuning(tidy_object,
#'                              tuner = "Bayesian Optimization",
#'                              metrics = c("rmse", "mape"),
#'                              plot_results = TRUE
#'                              )
#'
#' tidy_object<-show_results(tidy_object,
#'                           summary = TRUE,
#'                           scatter_predictions = TRUE,
#'                           residuals_dist = TRUE,
#'                           new_data = "train")
#' @references
#' Molnar, C. (2025). *Interpretable Machine Learning: A Guide for Making Black Box Models Explainable (3rd. ed.)*.
#' cristophm.github.io/interpretable-ml-book/
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

  analysis_object = analysis_object$clone()

  predictions = get_predictions(analysis_object, "all")

  analysis_object$modify("predictions", predictions)

  pred_test = predictions %>% dplyr::filter(data_set == "test")

  summary_results = summary_results(analysis_object, pred_test)

  analysis_object$modify("fit_summary",summary_results)

  print("############# Showing Results #############")

  if (summary == T){

    print("###### Summary ######")
    cat("\n")
    print(summary_results)
    cat("\n")

    # Multiclass classification case

    if (analysis_object$outcome_levels > 2){

      summary_results %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ signif(., 3))) %>%
        dplyr::select(-c(Class)) %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column("Metric") %>%
        ggpubr::ggtexttable(rows = NULL) %>%
        print()

    } else {

    summary_results %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ signif(., 3))) %>%
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
        ggplot2::autoplot() +
        ggplot2::labs(title = "ROC Curve")


    } else {

      p <-  predictions %>%
            plot_roc_curve_multiclass(new_data = "all") %>%
            ggplot2::autoplot() +
            ggplot2::labs(title = "ROC Curve")

    }

    plot_ob = analysis_object$plots

    plot_ob$roc_curve = p

    analysis_object$modify("plots", plot_ob)

    print(p)

  }

  if (pr_curve == T){

    if (analysis_object$outcome_levels == 2){

    p <- predictions %>%
         plot_pr_curve_binary(new_data = "all") %>%
         ggplot2::autoplot() +
         ggplot2::labs(title = "Precision Recall Curve")


    } else {

      p <- predictions %>%
           plot_pr_curve_multiclass(new_data = "all") %>%
           ggplot2::autoplot() +
           ggplot2::labs(title = "Precision Recall Curve")


    }

    plot_ob = analysis_object$plots

    plot_ob$pr_curve = p

    analysis_object$modify("plots", plot_ob)

    print(p)


  }

  if (gain_curve == T){

    if (analysis_object$outcome_levels == 2){

     p <- predictions %>%
          plot_gain_curve_binary() %>%
          ggplot2::autoplot() +
          ggplot2::labs(title = "Gain Curve")


    } else {

    p <-predictions %>%
        plot_gain_curve_multiclass() %>%
        ggplot2::autoplot() +
        ggplot2::labs(title = "Gain Curve")

    }

    plot_ob = analysis_object$plots

    plot_ob$gain_curve = p

    analysis_object$modify("plots", plot_ob)

    print(p)

  }

  if (lift_curve == T){

    if (analysis_object$outcome_levels == 2){

      p <- predictions %>%
           plot_lift_curve_binary(new_data = "all") %>%
           ggplot2::autoplot() +
           ggplot2::labs(title = "Lift Curve")


    } else{


      p <- predictions %>%
           plot_lift_curve_multiclass(new_data = "all") %>%
           ggplot2::autoplot() +
           ggplot2::labs(title = "Lift Curve")


    }

    plot_ob = analysis_object$plots

    plot_ob$lift_curve = p

    analysis_object$modify("plots", plot_ob)

    print(p)

  }

  if (dist_by_class == T){

    if (analysis_object$outcome_levels == 2){

   p <- pred_test %>%
        plot_dist_probs_binary(new_data)

    } else {

   p <- pred_test %>%
        plot_dist_probs_multiclass()

    }

    plot_ob = analysis_object$plots

    plot_ob$dist_by_class = p

    analysis_object$modify("plots", plot_ob)

    print(p)

  }

  if (reliability_plot == T){

    if (analysis_object$outcome_levels > 2){stop("Reliability Plot not implemented for Multiclass Classification!")}

     p <- pred_test %>%
          plot_calibration_curve_binary(new_data = new_data)

     plot_ob = analysis_object$plots

     plot_ob$reliability_plot = p

     analysis_object$modify("plots", plot_ob)

     print(p)

  }

  if (confusion_matrix == T){

    # y = all.vars(analysis_object$formula)[1]
    # y_levels <- levels(analysis_object$full_data[[y]])
    #
    # print(y_levels)
    #
    #
    # pred_test <- pred_test %>%
    #   mutate(
    #     y = factor(y, levels = y_levels)
    #   )

     cm <- pred_test %>%
          plot_conf_mat(new_data = new_data)

    p <- cm %>% ggplot2::autoplot(type = "heatmap") +
          ggplot2::labs(title = "Confusion Matrix")

     plot_ob = analysis_object$plots

     plot_ob$confusion_matrix = p

     analysis_object$modify("plots", plot_ob)

     print(p)

  }

  if (scatter_residuals == T){

     p <- pred_test %>%
          plot_scatter(new_data = new_data, error = T)

     plot_ob = analysis_object$plots

     plot_ob$scatter_residuals = p

     analysis_object$modify("plots", plot_ob)

     print(p)


  }

  if (scatter_predictions == T){

    p <- pred_test %>%
      plot_scatter(new_data = new_data, error = F)

    plot_ob = analysis_object$plots

    plot_ob$scatter_predicitons = p

    analysis_object$modify("plots", plot_ob)

    print(p)

  }

  if (residuals_dist == T){

     p <- pred_test %>%
          plot_residuals_density(new_data = new_data)

     plot_ob = analysis_object$plots

     plot_ob$residuals_dist = p

     analysis_object$modify("plots", plot_ob)

     print(p)


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



