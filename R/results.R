######################################################
#         get_results                                #
######################################################

show_results <- function(tidy_object,
                        summary = FALSE, roc_curve = FALSE, pr_curve = FALSE,
                        gain_curve = FALSE, lift_curve = FALSE,
                        dist_by_class = FALSE, reliability_plot = FALSE, confusion_matrix = FALSE,
                        scatter_residuals = FALSE, scatter_predictions = FALSE, residuals_dist = FALSE,
                        new_data = "test"){

  predictions = get_predictions(tidy_object, "all")

  tidy_object$modify("predictions", predictions)

  pred_test = predictions %>% filter(data_set == "test")

  summary_results = summary_results(tidy_object, pred_test)

  tidy_object$modify("fit_summary",summary_results)

  print("############# Showing Results")

  if (summary == T){

    # summary_results %>%
    #   dplyr::mutate(across(where(is.numeric), ~ signif(., 3))) %>%
    # ggpubr::ggtexttable(rows = NULL) %>%
    #   print()

    summary_results %>%
      dplyr::mutate(across(where(is.numeric), ~ signif(., 3))) %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Metric") %>%
      dplyr::rename(Value = 2) %>%
      ggpubr::ggtexttable(rows = NULL) %>%
      print()

  }

  if (roc_curve == T){

    if (tidy_object$outcome_levels == 2){

        p <- predictions %>%
        plot_roc_curve_binary(new_data = "all") %>%
        autoplot() +
        ggplot2::labs(title = "ROC Curve")

        print(p)


    } else {

      p <-  predictions %>%
            plot_roc_curve_multiclass(new_data = "all") %>%
            autoplot() +
            ggplot2::labs(title = "ROC Curve")

      print(p)

    }

  }

  if (pr_curve == T){

    if (tidy_object$outcome_levels == 2){

    p <- predictions %>%
         plot_pr_curve_binary(new_data = "all") %>%
         autoplot() +
         ggplot2::labs(title = "Precision Recall Curve")

    print(p)

    } else {

      p <- predictions %>%
           plot_pr_curve_multlicass(new_data = "all") %>%
           autoplot() +
           ggplot2::labs(title = "Precision Recall Curve")

      print(p)

    }

  }

  if (gain_curve == T){

    if (tidy_object$outcome_levels == 2){

     p <- predictions %>%
          plot_gain_curve_binary() %>%
          autoplot() +
          ggplot2::labs(title = "Gain Curve")

    print(p)

    } else {

    p <-predictions %>%
        plot_gain_curve_multiclass() %>%
        autoplot() +
        ggplot2::labs(title = "Gain Curve")

    print(p)

    }

  }

  if (lift_curve == T){

    if (tidy_object$outcome_levels == 2){

      p <- predictions %>%
           plot_lift_curve_binary(new_data = "all") %>%
           autoplot() +
           ggplot2::labs(title = "Lift Curve")

      print(p)

    } else{


      p <- predictions %>%
           plot_lift_curve_multiclass(new_data = "all") %>%
           autoplot() +
           ggplot2::labs(title = "Lift Curve")

      print(p)

    }

  }

  if (dist_by_class == T){

    if (tidy_object$outcome_levels == 2){

      pred_test %>%
        plot_dist_probs_binary() %>%
        print()

    } else {

      pred_test %>%
        plot_dist_probs_multiclass() %>%
        print()

    }
  }

  if (reliability_plot == T){

    pred_test %>%
      plot_calibration_curve_binary() %>%
      print()
  }

  if (confusion_matrix == T){


     p <- pred_test %>%
      plot_conf_mat(new_data = new_data) %>%
      autoplot(type = "heatmap") +
      ggplot2::labs(title = "Confusion Matrix")

      print(p)

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

modify_datasets <- function(tidy_object){

  rec <- recipes::prep(tidy_object$transformer,
                       training = tidy_object$train_data,
                       strings_as_factors = T)

  new_train <- recipes::bake(rec, new_data = tidy_object$train_data)
  new_test <- recipes::bake(rec, new_data = tidy_object$test_data)

  tidy_object$modify("train_data", new_train)
  tidy_object$modify("test_data", new_test)

  if (!is.null(tidy_object$validation_data)){

    new_validation <- recipes::bake(rec, new_data = tidy_object$validation_data)
    tidy_object$modify("validation_data", new_validation)

  }

  print(new_train)

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

    if (tidy_object$outcome_levels == 2){

      predictions = get_predictions_binary(tidy_object, new_data = new_data)

    } else {

      predictions = get_predictions_multiclass(tidy_object, new_data = new_data)

    }

  }

  return(predictions)


}

######################################################
#         SUMMARY                                    #
######################################################

summary_results <- function(tidy_object, predictions, new_data = "test"){

  if (tidy_object$task == "regression"){

    return(summary_regression(predictions, new_data))

  } else if (tidy_object$task == "classification"){

    if (tidy_object$outcome_levels == 2){

      return(summary_binary(predictions, new_data))

    } else {

      return(summary_multiclass_per_class(predictions, new_data))

    }

  }

}

###########################
#       Interpretable ML
###########################


