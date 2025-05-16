    ########################################
    #             Predictions              #
    ########################################

get_predictions_regression <- function(tidy_object, new_data = "test"){

  model_workflow <- tidy_object$final_model

  y = all.vars(tidy_object$formula)[1]

  if (new_data == "all"){

    data_sets = c("train", "validation", "test")

    temp = list()

    for (data_set in data_sets){

      dat = tidy_object[[paste0(data_set, "_data")]]

      predictions = predict(model_workflow, new_data = dat)
      predictions = cbind(predictions, y = dat[[y]])
      predictions$data_set = data_set

      temp[[data_set]] = predictions
    }

    predictions = rbind(temp[["train"]], temp[["validation"]], temp[["test"]])

  } else {

    dat = tidy_object[[paste0(new_data, "_data")]]

    predictions = predict(model_workflow, new_data = dat)
    predictions = cbind(predictions, y = dat[[y]])
    predictions$data_set = new_data

  }

  return (predictions)
}



    ########################################
    #             SUMMARY                  #
    ########################################

summary_regression <- function(predictions, new_data = "test"){

  metric_funcs <- list(

    RMSE = function(data) rmse(data, y, .pred),
    MAE = function(data) mae(data, y, .pred),
    MAPE = function(data) mape(data, y, .pred),
    MPE = function(data) mpe(data, y, .pred),
    CCC = function(data) ccc(data, y, .pred),
    SMAPE = function(data) smape(data, y, .pred),
    RPIQ = function(data) rpiq(data, y, .pred),
    RSQ = function(data) rsq(data, y, .pred)

  )

  results <- lapply(metric_funcs, function(f) f(predictions)$.estimate)

  results <- as.data.frame(results)

  return(results)


}

    ########################################
    #             PLOTS                    #
    ########################################


plot_scatter <- function(predictions, new_data = "test", error = F){

  if (error == T){

    predictions %>%
      dplyr::mutate(error = y - .pred) %>%

      ggplot2::ggplot(ggplot2::aes(x = .pred, y = error)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Residuals vs Predictions", x = "Predictions", y = "Residuals") +
      ggplot2::theme_minimal()

  } else {

    predictions %>%
      ggplot2::ggplot(ggplot2::aes(x = .pred, y = y)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Observed vs Predictions", x = "Predictions", y = "Observed") +
      ggplot2::theme_minimal()
  }

}

plot_residuals_density <- function(predictions, new_data = "test"){

  predictions %>%
    dplyr::mutate(error = y - .pred) %>%

    ggplot2::ggplot(ggplot2::aes(x = error))+
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::labs(title = "Residuals vs Predictions", x = "Predictions", y = "Residuals") +
    ggplot2::theme_minimal()

}
