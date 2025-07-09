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
#' ML TidyML workflow.
#' @examples
#' # Example 1: Classification Task
#' # Display summary metrics, ROC curve, and confusion matrix for
#' # a classification model with test partition
#'
#' library(TidyML)
#'
#' data(sim_data) # sim_data is a simulated dataset with psychological variables
#'
#' tidy_object <- preprocessing(
#'        df = sim_data,
#'        formula = psych_well_bin ~ depression + emot_intel + resilience + life_sat,
#'        task = "classification"
#'       )
#'
#' tidy_object <- build_model(
#'          analysis_object = tidy_object,
#'          model_name = "SVM",
#'          hyperparameters = list(
#'              type = "rbf",
#'              cost = 1,
#'              margin = 0.1,
#'              rbf_sigma = 0.05
#'              )
#'           )
#'
#' tidy_object <- fine_tuning(tidy_object,
#'              tuner = "Grid Search CV",
#'              metrics = c("roc_auc", "f_meas"),
#'              plot_results = FALSE
#'              )
#'
#' tidy_object<-show_results(tidy_object,
#'          summary = TRUE,
#'          roc_curve = TRUE,
#'          confusion_matrix = TRUE,
#'          new_data = "test")
#'
#' @references
#' Molnar, C. (2025). *Interpretable Machine Learning: A Guide for Making Black Box Models Explainable (3rd. ed.)*.
#' cristophm.github.io/interpretable-ml-book/
#' @export
