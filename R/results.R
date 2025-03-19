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
  predictions = cbind(predictions_class, predictions_prob, y = dat[[y]])
  predictions$data_set = new_data

  }

  return (predictions)

}

box_plot_binary <- function(tidy_object, new_data = "test"){

  predictions <- get_predictions_binary(tidy_object, new_data)

  ggplot2::ggplot(predictions) +
    ggplot2::aes(
          x = y,
          y = .pred_1
    ) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(alpha = 0.1)
}

plot_roc_curve_binary <- function(tidy_object, new_data = "all"){

  if (new_data == "all"){

      tidy_object %>%
        get_predictions_binary(new_data = new_data) %>%
        dplyr::group_by(data_set) %>%
        yardstick::roc_curve(y, .pred_1, event_level = "second") %>%
        autoplot()

  }

}

# rocky = yardstick::roc_curve(pred, y, .pred_1, event_level = "second")
#
# samps <- create_sim_samples(10, 100, 100, "good_er")
# mdat <- mmdata(samps[["scores"]], samps[["labels"]],
#                modnames = samps[["modnames"]],
#                dsids = samps[["dsids"]]
# )
