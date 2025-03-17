roc_auc_vec <- function(truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
  yardstick::roc_auc_vec(
    truth = truth,
    estimate = estimate,
    estimator = estimator,
    na_rm = na_rm,
    ...
  )
}

roc_auc2 <- function(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...) {
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

