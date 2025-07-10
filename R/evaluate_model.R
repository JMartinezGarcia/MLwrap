# ######################################################
# #         get_results                                #
# ######################################################
#
# #' Showcase Summary Results and Plots
# #'
# #' The **show_results()** function is a central component of the ML workflow established by the package,
# #' following the stages of data preprocessing, model construction (build_model), and hyperparameter optimization
# #' (fine_tuning). After a model has been trained and tuned, show_results() enables users to generate comprehensive
# #' **visualizations and summaries of model performance**, including metrics tables, ROC and PR curves, gain and lift
# #' curves, confusion matrices, calibration plots, and regression diagnostics, tailored to both regression and
# #' classification tasks. This function provides a thorough and interpretable assessment of the fitted model,
# #' supporting informed evaluation and communication of results. Importantly, show_results() is not the final step
# #' of the workflow, as further analyses such as sensitivity analysis can be performed subsequently to deepen the
# #' understanding of model robustness and behavior (Molnar, 2025).
# #'
# #' @param analysis_object analysis_object created from fine_tuning function.
# #' @param summary Whether to plot summary results table. Boolean (FALSE by default).
# #' @param roc_curve Whether to plot ROC Curve (Classification task only). Boolean (FALSE by default).
# #' @param pr_curve Whether to plot ROC Curve (Classification task only). Boolean (FALSE by default).
# #' @param gain_curve Whether to plot ROC Curve (Classification task only). Boolean (FALSE by default).
# #' @param lift_curve Whether to plot ROC Curve (Classification task only). Boolean (FALSE by default).
# #' @param dist_by_class Whether to plot distribution of output probability by class (Classification task only).
# #'  Boolean (FALSE by default).
# #' @param reliability_plot Whether to plot Reliability Plot (Binary Classification task only). Boolean (FALSE by default).
# #' @param confusion_matrix Whether to Confusion Matrix (Classification task only). Boolean (FALSE by default).
# #' @param scatter_residuals Whether to plot Residuals vs Predictions (Regression task only). Boolean (FALSE by default).
# #' @param scatter_predictions Whether to plot Predictions vs Observed (Regression task only). Boolean (FALSE by default).
# #' @param residuals_dist Whether to plot Residuals Distribution (Regression task only). Boolean (FALSE by default).
# #' @param new_data Data to be used for Confusion Matrix, Reliability Plot, Distribution by Class Plot,
# #'        Residuals vs Predictions Plot, Predictions vs Observed Plot and Residuals Distribution Plot.
# #'        A string with the name of the data_set: "train", "validation", "test" (default) or "all".
# #' @returns An updated analysis_object containing the generated predictions, summary statistics, and any
# #' visualizations or diagnostic outputs selected by the user. This object reflects the results of model
# #' evaluation and can be further used for reporting, interpretation, or additional analyses within the
# #' ML TidyML workflow.
# #' @examples
# #' # Example 1: Classification Task
# #' # Display summary metrics, ROC curve, and confusion matrix for
# #' # a classification model with test partition
# #'
# #' library(TidyML)
# #'
# #' data(sim_data) # sim_data is a simulated dataset with psychological variables
# #'
# #' tidy_object <- preprocessing(
# #'        df = sim_data,
# #'        formula = psych_well_bin ~ depression + emot_intel + resilience + life_sat,
# #'        task = "classification"
# #'       )
# #'
# #' tidy_object <- build_model(
# #'          analysis_object = tidy_object,
# #'          model_name = "SVM",
# #'          hyperparameters = list(
# #'              type = "rbf",
# #'              cost = 1,
# #'              margin = 0.1,
# #'              rbf_sigma = 0.05
# #'              )
# #'           )
# #'
# #' tidy_object <- fine_tuning(tidy_object,
# #'              tuner = "Grid Search CV",
# #'              metrics = c("roc_auc", "f_meas"),
# #'              plot_results = FALSE
# #'              )
# #'
# #' tidy_object<-show_results(tidy_object,
# #'          summary = TRUE,
# #'          roc_curve = TRUE,
# #'          confusion_matrix = TRUE,
# #'          new_data = "test")
# #'
# #' @references
# #' Molnar, C. (2025). *Interpretable Machine Learning: A Guide for Making Black Box Models Explainable (3rd. ed.)*.
# #' cristophm.github.io/interpretable-ml-book/
# #' @export
evaluate_model <- function(analysis_object){

  analysis_object = analysis_object$clone()

  predictions = get_predictions(analysis_object, "all")

  task = analysis_object$task

  analysis_object$modify("predictions", predictions)

  pred_train = predictions %>% dplyr::filter(data_set == "train")

  pred_test = predictions %>% dplyr::filter(data_set == "test")

  summary_train = summary_results(analysis_object, pred_train, new_data = "Train")

  summary_test = summary_results(analysis_object, pred_test, new_data = "Test")

  tables <- analysis_object$tables

  if (analysis_object$outcome_levels > 2){

    tables$summary_train <- summary_train

    tables$summary_test <- summary_test

  } else {

    summary_total <- bind_rows(summary_train, summary_test)

    tables$summary_results <- summary_total

  }

  analysis_object$modify("tables", tables)

  if (task == "classification"){

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

  }

  if (task == "classification"){

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

  }

  if (task == "classification"){

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

  }

  if (task == "classification"){

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

  }

  if (task == "classification"){

    if (analysis_object$outcome_levels == 2){

      p_train <- pred_train %>%
        plot_dist_probs_binary("train")

      p_test <- pred_test %>%
        plot_dist_probs_binary("test")

    } else {

      p_train <- pred_train %>%
        plot_dist_probs_multiclass(data_set = "train")

      p_test <- pred_test %>%
        plot_dist_probs_multiclass(data_set = "test")

    }

    plot_ob = analysis_object$plots

    plot_ob$dist_by_class_train = p_train

    plot_ob$dist_by_class_test = p_test

    analysis_object$modify("plots", plot_ob)

  }

  if (task == "classification"){

    if (analysis_object$outcome_levels == 2){

      p_train <- pred_train %>%
        plot_calibration_curve_binary(new_data = "train")

      p_test <- pred_test %>%
        plot_calibration_curve_binary(new_data = "test")

      plot_ob = analysis_object$plots

      plot_ob$reliability_plot_train = p_train

      plot_ob$reliability_plot_test = p_test

      analysis_object$modify("plots", plot_ob)

    }

  }

  if (task == "classification"){

    cm_train <- pred_train %>%
      plot_conf_mat(new_data = "train")

    cm_test <- pred_test %>%
      plot_conf_mat(new_data = "test")

    p_train <- cm_train %>% ggplot2::autoplot(type = "heatmap") +
      ggplot2::labs(title = "Confusion Matrix Train Data")

    p_test <- cm_test %>% ggplot2::autoplot(type = "heatmap") +
      ggplot2::labs(title = "Confusion Matrix Test Data")

    plot_ob = analysis_object$plots

    plot_ob$confusion_matrix_train = p_train

    plot_ob$confusion_matrix_test = p_test

    analysis_object$modify("plots", plot_ob)

  }

  if (task == "regression"){

    p_train <- pred_train %>%
      plot_scatter(new_data = "train", error = TRUE)

    p_test <- pred_test %>%
      plot_scatter(new_data = "test", error = TRUE)

    plot_ob = analysis_object$plots

    plot_ob$scatter_residuals_train = p_train

    plot_ob$scatter_residuals_test = p_test

    analysis_object$modify("plots", plot_ob)

  }

  if (task == "regression"){

    p_train <- pred_train %>%
      plot_scatter(new_data = "train", error = F)

    p_test <- pred_test %>%
      plot_scatter(new_data = "test", error = F)

    plot_ob = analysis_object$plots

    plot_ob$scatter_predictions_train = p_train

    plot_ob$scatter_predictions_test = p_test

    analysis_object$modify("plots", plot_ob)

  }

  if (task == "regression"){

    p_train <- pred_train %>%
      plot_residuals_density(new_data = "train")

    p_test <- pred_test %>%
      plot_residuals_density(new_data = "test")

    plot_ob = analysis_object$plots

    plot_ob$residuals_dist_train = p_train

    plot_ob$residuals_dist_test = p_test

    analysis_object$modify("plots", plot_ob)

  }

  analysis_object$modify("stage", "evaluated_model")

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


#############################
#                           #
#      Plot Functions       #
#                           #
#############################


graph_nn <- function(){

  if ((analysis_object$model_name == "Neural Network") && (analysis_object$model$engine == "brulee")){

  model_parsnip <- tune::extract_fit_parsnip(final_model)

  message("###### Loss Curve ######\n")

  p <- brulee::autoplot(model_parsnip) +
    ggplot2::labs(title = "Neural Network Loss Curve")

  plot_ob = analysis_object$plots

  plot_ob$nn_loss_curve = p

  analysis_object$modify("plots", plot_ob)

  print(p)

  p <- graph_nn(model_parsnip)

  print(p)

  }

}

plot_results <- function(){

  if (plot_results == T){

    plot_tuning_results(analysis_object)

  }
}





