###########################
#       Interpretable ML
###########################

sensitivity_analysis <- function(tidy_object, new_data = "test", shap_plots = F, PFI_plots = T, metric = NULL){

  y = all.vars(tidy_object$formula)[1]

  dat = tidy_object$transformer %>%
    recipes::prep(training = tidy_object$train_data) %>%
    recipes::bake(new_data = tidy_object[[paste0(new_data, "new_data")]])


  model_parsnip <- tidy_object$final_models %>%
    tune::extract_fit_parsnip()

  if (PFI_plots == T){

    pfi_plot(tidy_object, new_data = new_data, metric = metric)

  }

  if (shap_plots == T){

    shap_plot(tidy_object, new_data = new_data)

  }

}
