######################################
###         Classification         ###
######################################

# roc_auc <- function(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
#   yardstick::metric_summarizer(
#     "roc_auc",
#     yardstick::roc_auc_vec,
#     data = data,
#     truth = {{truth}},
#     estimate = {{estimate}},
#     estimator = estimator,
#     na_rm = na_rm,
#     ...
#   )
# }
#
# accuracy <- function(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
#   yardstick::metric_summarizer(
#     "accuracy",
#     yardstick::accuracy_vec,
#     data = data,
#     truth = {{truth}},
#     estimate = {{estimate}},
#     estimator = estimator,
#     na_rm = na_rm,
#     ...
#   )
# }
#
#
# mcc <- function(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
#   yardstick::metric_summarizer(
#     "mcc",
#     yardstick::mcc_vec,
#     data = data,
#     truth = {{truth}},
#     estimate = {{estimate}},
#     estimator = estimator,
#     na_rm = na_rm,
#     ...
#   )
# }
#
#
# f_meas <- function(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
#   yardstick::metric_summarizer(
#     "f_meas",
#     yardstick::f_meas_vec,
#     data = data,
#     truth = {{truth}},
#     estimate = {{estimate}},
#     estimator = estimator,
#     na_rm = na_rm,
#     ...
#   )
# }
#
#
# kap <- function(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
#   yardstick::metric_summarizer(
#     "kap",
#     yardstick::kap_vec,
#     data = data,
#     truth = {{truth}},
#     estimate = {{estimate}},
#     estimator = estimator,
#     na_rm = na_rm,
#     ...
#   )
# }
#
# pr_auc <- function(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
#   yardstick::metric_summarizer(
#     "pr_auc",
#     yardstick::pr_auc_vec,
#     data = data,
#     truth = {{truth}},
#     estimate = {{estimate}},
#     estimator = estimator,
#     na_rm = na_rm,
#     ...
#   )
# }
#
# brier_class <- function(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
#   yardstick::metric_summarizer(
#     "brier_class",
#     yardstick::brier_class_vec,
#     data = data,
#     truth = {{truth}},
#     estimate = {{estimate}},
#     estimator = estimator,
#     na_rm = na_rm,
#     ...
#   )
# }
#
# recall <- function(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
#   yardstick::metric_summarizer(
#     "recall",
#     yardstick::recall_vec,
#     data = data,
#     truth = {{truth}},
#     estimate = {{estimate}},
#     estimator = estimator,
#     na_rm = na_rm,
#     ...
#   )
# }
#
# precision <- function(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
#   yardstick::metric_summarizer(
#     "precision",
#     yardstick::precision_vec,
#     data = data,
#     truth = {{truth}},
#     estimate = {{estimate}},
#     estimator = estimator,
#     na_rm = na_rm,
#     ...
#   )
# }
#
# specificity <- function(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
#   yardstick::metric_summarizer(
#     "specificity",
#     yardstick::specificity_vec,
#     data = data,
#     truth = {{truth}},
#     estimate = {{estimate}},
#     estimator = estimator,
#     na_rm = na_rm,
#     ...
#   )
# }
#
# sensitivity <- function(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
#   yardstick::metric_summarizer(
#     "sensitivity",
#     yardstick::sensitivity_vec,
#     data = data,
#     truth = {{truth}},
#     estimate = {{estimate}},
#     estimator = estimator,
#     na_rm = na_rm,
#     ...
#   )
# }
#
# bal_accuracy <- function(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
#   yardstick::metric_summarizer(
#     "bal_accuracy",
#     yardstick::bal_accuracy_vec,
#     data = data,
#     truth = {{truth}},
#     estimate = {{estimate}},
#     estimator = estimator,
#     na_rm = na_rm,
#     ...
#   )
# }
#
# detection_prevalence <- function(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
#   yardstick::metric_summarizer(
#     "detection_prevalence",
#     yardstick::detection_prevalence_vec,
#     data = data,
#     truth = {{truth}},
#     estimate = {{estimate}},
#     estimator = estimator,
#     na_rm = na_rm,
#     ...
#   )
# }
#
# nombre <- function(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
#   yardstick::metric_summarizer(
#     "nombre",
#     yardstick::nombre_vec,
#     data = data,
#     truth = {{truth}},
#     estimate = {{estimate}},
#     estimator = estimator,
#     na_rm = na_rm,
#     ...
#   )
# }
#
# roc_auc <- yardstick::new_prob_metric(roc_auc, "maximize")
# accuracy <- yardstick::new_class_metric(accuracy, "maximize")

######################################
###         Regression             ###
######################################



