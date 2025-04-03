######################################################
#         get_results                                #
######################################################

get_results <- function(tidy_object,
                        summary = FALSE, roc_curve = FALSE, pr_curve = FALSE,
                        gain_curve = FALSE, lift_curve = FALSE,
                        dist_by_class = FALSE, reliability_plot = FALSE, confusion_matrix = FALSE,
                        scatter_residuals = FALSE, scatter_predictions = FALSE, residuals_dist = FALSE,
                        new_data = "test"){

  predictions = get_predictions(tidy_object, "all")

  pred_test = predictions %>% filter(data_set == "test")

  summary_results = summary_results(tidy_object)

  tidy_object$modify("fit_summary",summary_results)

  if (summary == T){

    print(summary_results)

  }

  if (roc_curve == T){

    predictions %>%
      plot_roc_curve_binary(new_data = "all") %>%
      autoplot() %>%
      print()

  }

  if (pr_curve == T){

    predictions %>%
      plot_pr_curve_binary(new_data = "all") %>%
      autoplot() %>%
      print()

  }

  if (gain_curve == T){

    predictions %>%
      plot_gain_curve_binary() %>%
      autoplot() %>%
      print()

  }

  if (lift_curve == T){

    predictions %>%
      plot_lift_curve_binary(new_data = "all") %>%
      autoplot() %>%
      print()

  }

  if (dist_by_class == T){

    pred_test %>%
      plot_dist_probs_binary() %>%
      print()

  }

  if (reliability_plot == T){

    pred_test %>%
      plot_calibration_curve_binary() %>%
      print()
  }

  if (confusion_matrix == T){


    pred_test %>%
      plot_conf_mat_binary(new_data = new_data) %>%
      autoplot(type = "heatmap") %>%
      print()

  }

  if (scatter_residuals == T){

    pred_test %>%
      plot_scatter(new_data = "all", erro = T) %>%
      print()

  }

  if (scatter_predictions == T){

    pred_test %>%
      plot_scatter(new_data = "all", error = F) %>%
      print()

  }

  if (residuals_dist == T){

    pred_test %>%
      plot_residuals_density(new_data = "all") %>%
      print()

  }

  return(tidy_object)




}

######################################################
#         get_predictions                           #
######################################################

get_predictions <- function(tidy_object, new_data = "test"){

  if (tidy_object$task == "regression"){

    predictions = get_predictions_regression(tidy_object, new_data = new_data)

  } else if (tidy_object$task == "classification"){

    predictions = get_predictions_binary(tidy_object, new_data = new_data)

  }

  return(predictions)


}

get_predictions_regression <- function(tidy_object, new_data = "test"){

  model_workflow <- tidy_object$final_models

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

get_predictions_binary <- function(tidy_object, new_data = "test"){

  model_workflow <- tidy_object$final_models

  y = all.vars(tidy_object$formula)[1]

  if (new_data == "all"){

    data_sets = c("train", "validation", "test")

    temp = list()

    for (data_set in data_sets){

      dat = tidy_object[[paste0(data_set, "_data")]]

      predictions_class = predict(model_workflow, new_data = dat)
      predictions_prob = predict(model_workflow, new_data = dat, type = "prob")
      predictions = cbind(predictions_class, predictions_prob, y = as.factor(dat[[y]]))
      predictions$data_set = data_set

      temp[[data_set]] = predictions
    }

    predictions = rbind(temp[["train"]], temp[["validation"]], temp[["test"]])

  } else {

  dat = tidy_object[[paste0(new_data, "_data")]]

  predictions_class = predict(model_workflow, new_data = dat)
  predictions_prob = predict(model_workflow, new_data = dat, type = "prob")
  predictions = cbind(predictions_class, predictions_prob, y = as.factor(dat[[y]]))
  predictions$data_set = new_data

  }

  return (predictions)

}

######################################################
#         SUMMARY                                    #
######################################################

summary_results <- function(tidy_object, new_data = "test"){

  if (tidy_object$task == "regression"){

    return(summary_regression(tidy_object, new_data))

  } else if (tidy_object$task == "classification"){

    return(summary_binary(tidy_object, new_data))

  }

}

summary_binary <- function(tidy_object, new_data = "test"){

  metric_funcs <- list(

    Accuracy = function(data) accuracy(data, y, .pred_class),
    Balanced_Accuracy = function(data) bal_accuracy(data, y, .pred_class),
    Precision = function(data) precision(data, y, .pred_class),
    Recall = function(data) recall(data, y, .pred_class),
    Specificity = function(data) specificity(data, y, .pred_class),
    Sensitivity = function(data) sensitivity(data, y, .pred_class),
    Kappa = function(data) kap(data, y, .pred_class),
    F1_score = function(data) f_meas(data, y, .pred_class),
    MCC = function(data) mcc(data, y, .pred_class),
    J_index = function(data) j_index(data, y, .pred_class),
    Detection_Prevalence = function(data) detection_prevalence(data, y, .pred_class, event_level = "second"),
    AUC_ROC = function(data) roc_auc(data, y, data[[predicted]], event_level = "second"),
    AUC_PR = function(data) pr_auc(data, y, data[[predicted]], event_level = "second"),
    Gain_Capture = function(data) gain_capture(data, y, data[[predicted]], event_level = "second"),
    Brier_Score = function(data) brier_class(data, y, data[[predicted]], event_level = "second")

  )

  predictions <- get_predictions_binary(tidy_object, new_data = new_data)

  positive_class = levels(predictions$y)[2]

  predicted = paste0(".pred_", positive_class)

  results <- lapply(metric_funcs, function(f) f(predictions)$.estimate)

  results <- as.data.frame(results)

  return(results)

}

summary_regression <- function(tidy_object, new_data = "test"){

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

  predictions <- get_predictions_regression(tidy_object, new_data = new_data)

  results <- lapply(metric_funcs, function(f) f(predictions)$.estimate)

  results <- as.data.frame(results)

  return(results)


}

######################################################
#         PLOTS                                      #
######################################################


plot_roc_curve_binary <- function(predictions, new_data = "all"){

  positive_class = levels(predictions$y)[2]

  predicted = paste0(".pred_", positive_class)

  if (new_data == "all"){

      curve_plot <- predictions %>%
        dplyr::group_by(data_set) %>%
        yardstick::roc_curve(y, predicted, event_level = "second")

      return(curve_plot)

  }

}

plot_pr_curve_binary <- function(predictions, new_data = "all"){

  positive_class = levels(predictions$y)[2]

  predicted = paste0(".pred_", positive_class)

  if (new_data == "all"){

    curve_plot <- predictions %>%
      dplyr::group_by(data_set) %>%
      yardstick::pr_curve(y, predicted, event_level = "second")

    return(curve_plot)

  }


}

plot_gain_curve_binary <- function(predictions, new_data = "all"){

  positive_class = levels(predictions$y)[2]

  predicted = paste0(".pred_", positive_class)

  if (new_data == "all"){

    curve_plot <- predictions %>%
      dplyr::group_by(data_set) %>%
      yardstick::gain_curve(y, predicted, event_level = "second")

    return(curve_plot)

  }


}

plot_lift_curve_binary <- function(predictions, new_data = "all"){

  positive_class = levels(predictions$y)[2]

  predicted = paste0(".pred_", positive_class)

  if (new_data == "all"){

    curve_plot <- predictions %>%
      dplyr::group_by(data_set) %>%
      yardstick::lift_curve(y, predicted, event_level = "second")

    return(curve_plot)

  }


}

plot_dist_probs_binary <- function(predictions, new_data = "test"){

  positive_class = levels(predictions$y)[2]

  predicted = paste0(".pred_", positive_class)

  predictions %>%
    dplyr::group_by(y) %>%
    ggplot2::ggplot(ggplot2::aes_string(x = predicted, fill = "y")) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::labs(title = "Probability Distribution by Class",
         x = "Predicted Probability",
         y = "Density",
         fill = "Class") +
    ggplot2::theme_minimal()

}

plot_calibration_curve_binary <- function(predictions, new_data = "test"){

  positive_class = levels(predictions$y)[2]

  predicted = sym(paste0(".pred_", positive_class))

  predictions %>%
    dplyr::mutate(y = sapply(y, function(x) if(x == positive_class) 1 else 0)) %>%
    dplyr::mutate(pred_bin = cut(predictions[[predicted]],
                                 breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) %>%  # Crear bins de probabilidad
    dplyr::group_by(pred_bin) %>%
    dplyr::summarise(
      prob_pred = mean({{predicted}}),  # Promedio de las probabilidades predichas en cada bin
      prob_observed = mean(y)  # Promedio de 1s observados (probabilidad observada)
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = prob_pred, y = prob_observed)) +
    ggplot2::geom_point() +  # Graficar los puntos
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Línea de calibración ideal
    ggplot2::labs(title = "Reliability Plot", x = "Predicted Probability", y = "Observed Probability") +
    ggplot2::theme_minimal()

}


plot_conf_mat_binary <- function(predictions, new_data = "test"){

      confusion_matrix = yardstick::conf_mat(predictions, truth = y, estimate = .pred_class)

      return(confusion_matrix)

}

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

plot_residuals_density <- function(predictions, new_data = "test")

 predictions %>%
      dplyr::mutate(error = y - .pred) %>%

      ggplot2::ggplot(ggplot2::aes(x = error))+
      ggplot2::geom_density(alpha = 0.5) +
      ggplot2::labs(title = "Residuals vs Predictions", x = "Predictions", y = "Residuals") +
      ggplot2::theme_minimal()



radar_diagram <- function(tidy_object, new_data = "test"){

    #predictions <- get_predictions_regression(tidy_object, new_data = new_data)

    summary_results = summary_results(tidy_object, new_data = new_data)

    fmsb::radarchart(summary_results)


}


###########################
#       Interpretable ML
###########################

permutation_feature_importance <- function(tidy_object, new_data = "test"){

  pfun <- function(object, newdata){

   pred = predict(object, new_data = newdata, type = "prob")

   return(pred[[2]])

}

  model_parsnip <- tidy_object$final_models %>%
                      tune::extract_workflow() %>%
                      tune::extract_fit_parsnip()


  dat = tidy_object$transformer %>%
    recipes::prep(training = tidy_object$train) %>%
    recipes::bake(new_data = tidy_object[[new_data]])

  vis <- vip::vi(model_parsnip,
                 method = "permute",
                 nsim = 10,
                 metric = "roc_auc",
                 train = dat,
                 target = "Species",
                 pred_wrapper = pfun,
                 event_level = "second")

  vip::vip(vis, include_type = TRUE, all_permutations = TRUE,
           geom = "boxplot", aesthetics = list(color = "lightblue", width = 0.3))


}

