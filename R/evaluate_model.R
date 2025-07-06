# modify_datasets <- function(analysis_object){
#
#   rec <- recipes::prep(analysis_object$transformer,
#                        training = analysis_object$train_data,
#                        strings_as_factors = T)
#
#   new_train <- recipes::bake(rec, new_data = analysis_object$train_data)
#   new_test <- recipes::bake(rec, new_data = tidy_object$test_data)
#
#   tidy_object$modify("train_data", new_train)
#   tidy_object$modify("test_data", new_test)
#
#   if (!is.null(tidy_object$validation_data)){
#
#     new_validation <- recipes::bake(rec, new_data = tidy_object$validation_data)
#     tidy_object$modify("validation_data", new_validation)
#
#   }
#
#   print(new_train)
#
#   return(tidy_object)
#
# }
#
# ######################################################
# #         get_predictions                           #
# ######################################################
#
# get_predictions <- function(analysis_object, new_data = "test"){
#
#   if (analysis_object$task == "regression"){
#
#     predictions = get_predictions_regression(analysis_object, new_data = new_data)
#
#   } else if (analysis_object$task == "classification"){
#
#     predictions = get_predictions_binary(analysis_object, new_data = new_data)
#
#     if (analysis_object$outcome_levels == 2){
#
#       predictions = get_predictions_binary(analysis_object, new_data = new_data)
#
#     } else {
#
#       predictions = get_predictions_multiclass(analysis_object, new_data = new_data)
#
#     }
#
#   }
#
#   return(predictions)
#
#
# }
#
# ######################################################
# #         SUMMARY                                    #
# ######################################################
#
# summary_results <- function(analysis_object, predictions, new_data = "test"){
#
#   if (analysis_object$task == "regression"){
#
#     return(summary_regression(predictions, new_data))
#
#   } else if (analysis_object$task == "classification"){
#
#     if (analysis_object$outcome_levels == 2){
#
#       return(summary_binary(predictions, new_data))
#
#     } else {
#
#       return(summary_multiclass_per_class(predictions, new_data))
#
#     }
#
#   }
#
# }
#
#
# #############################
# #                           #
# #      Plot Functions       #
# #                           #
# #############################
#
#
# graph_nn <- function(){
#
#   if ((analysis_object$model_name == "Neural Network") && (analysis_object$model$engine == "brulee")){
#
#   model_parsnip <- tune::extract_fit_parsnip(final_model)
#
#   message("###### Loss Curve ######\n")
#
#   p <- brulee::autoplot(model_parsnip) +
#     ggplot2::labs(title = "Neural Network Loss Curve")
#
#   plot_ob = analysis_object$plots
#
#   plot_ob$nn_loss_curve = p
#
#   analysis_object$modify("plots", plot_ob)
#
#   print(p)
#
#   p <- graph_nn(model_parsnip)
#
#   print(p)
#
#   }
#
# }
#
# plot_results <- function(){
#
#   if (plot_results == T){
#
#     plot_tuning_results(analysis_object)
#
#   }
# }

plot_residuals_distribution <- function(analysis_object, data_set = "test"){

  check_args_regression_plot(analysis_object, data_set)

  predictions <- analysis_object$predictions %>% filter(data_set == data_set)

  p <- predictions %>%
      dplyr::mutate(error = y - .pred) %>%
      ggplot2::ggplot(ggplot2::aes(x = error)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                              bins = 30, fill = "lightgray", color = "white", alpha = 0.5) +
      ggplot2::geom_density(color = "steelblue", size = 1.2, alpha = 0.6) +
      ggplot2::labs(title = paste("Residual Density -", new_data, "set"),
                    x = "Residuals", y = "Density") +
      ggplot2::theme_minimal()

  plots <- analysis_object$plots

  plots$residuals_distribution <- p

  analysis_object$modify("plots", plots)

  if (base::interactive()){plot(p)}

  invisible(analysis_object)

}




