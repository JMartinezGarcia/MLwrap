create_svm_rbf <- function(hyperparams, task, epochs){

  cost <- if (hyperparams$cost_tune) tune::tune() else hyperparams$hyperparams_constant$cost
  margin <- if (hyperparams$margin_tune) tune::tune() else hyperparams$hyperparams_constant$margin
  rbf_sigma <- if (hyperparams$rbf_sigma_tune) tune::tune() else hyperparams$hyperparams_constant$rbf_sigma

  model = parsnip::svm_rbf(
    cost = !!cost,
    margin = !!margin,
    rbf_sigma = !!rbf_sigma
  ) %>%
    parsnip::set_engine("kernlab") %>%
    parsnip::set_mode(task)

  return(model)
}
