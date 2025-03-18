roc_auc <- function(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
  yardstick::metric_summarizer(
    "roc_auc",
    yardstick::roc_auc_vec,
    data = data,
    truth = {{truth}},
    estimate = {{estimate}},
    estimator = estimator,
    na_rm = na_rm,
    ...
  )
}

accuracy <- function(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
  yardstick::metric_summarizer(
    "accuracy",
    yardstick::accuracy_vec,
    data = data,
    truth = {{truth}},
    estimate = {{estimate}},
    estimator = estimator,
    na_rm = na_rm,
    ...
  )
}

roc_auc <- yardstick::new_prob_metric(roc_auc, "maximize")
accuracy <- yardstick::new_class_metric(accuracy, "maximize")
