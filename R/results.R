######################################################
#         get_results                                #
######################################################

get_results <- function(tidy_object, roc_curve = FALSE, confusion_matrix = FALSE, summary = FALSE, new_data = "test"){

  predictions = get_predictions(tidy_object, new_data = "test")

  summary_results = summary_binary(tidy_object, new_data = new_data)

  tidy_object$add_summary(summary_results)

  if (summary == T){

    print(summary_results)

  }

  if (roc_curve == T){

    tidy_object %>%
      plot_roc_curve_binary(new_data = "all") %>%
      autoplot() %>%
      print()

  }

  if (confusion_matrix == T){


    tidy_object %>%
      plot_conf_mat_binary(new_data = new_data) %>%
      autoplot(type = "heatmap") %>%
      print()

  }

  return(tidy_object)


}

######################################################
#         get_predicitions                           #
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

  return(1)

}

get_predictions_binary <- function(tidy_object, new_data = "test"){

  model_workflow <- tidy_object$final_models %>% tune::extract_workflow()

  y = all.vars(tidy_object$formula)[1]

  if (new_data == "all"){

    data_sets = c("train", "validation", "test")

    temp = list()

    for (data_set in data_sets){

      dat = tidy_object[[data_set]]

      predictions_class = predict(model_workflow, new_data = dat)
      predictions_prob = predict(model_workflow, new_data = dat, type = "prob")
      predictions = cbind(predictions_class, predictions_prob, y = dat[[y]])
      predictions$data_set = data_set

      temp[[data_set]] = predictions
    }

    predictions = rbind(temp[["train"]], temp[["validation"]], temp[["test"]])

  } else {

  dat = tidy_object[[new_data]]

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

summary_binary <- function(tidy_object, new_data = "test"){

  predictions <- get_predictions_binary(tidy_object, new_data = new_data)

  confusion_matrix = yardstick::conf_mat(predictions, truth = y, estimate = .pred_class)

  return(tidyr::pivot_wider(summary(confusion_matrix), names_from = .metric, values_from = .estimate))

}

######################################################
#         PLOTS                                      #
######################################################

box_plot_binary <- function(tidy_object, new_data = "test"){

  predictions <- get_predictions_binary(tidy_object, new_data)

  positive_class = levels(predictions$y)[2]

  predicted = paste0(".pred_", positive_class)

  ggplot2::ggplot(predictions) +
    ggplot2::aes(
          x = y,
          y = .pred_class #predicted
    ) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(alpha = 0.1)
}

plot_roc_curve_binary <- function(tidy_object, new_data = "all"){

  predictions <- get_predictions_binary(tidy_object, new_data)

  positive_class = levels(predictions$y)[2]

  predicted = paste0(".pred_", positive_class)

  if (new_data == "all"){

      curve_roc <- tidy_object %>%
        get_predictions_binary(new_data = new_data) %>%
        dplyr::group_by(data_set) %>%
        yardstick::roc_curve(y, predicted, event_level = "second")

      return(curve_roc)

  }

}

plot_conf_mat_binary <- function(tidy_object, new_data = "test"){

      predictions <- get_predictions_binary(tidy_object, new_data = new_data)

      confusion_matrix = yardstick::conf_mat(predictions, truth = y, estimate = .pred_class)

      return(confusion_matrix)


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

