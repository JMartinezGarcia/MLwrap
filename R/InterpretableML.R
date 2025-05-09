###########################
#       Interpretable ML
###########################
#' @export
sensitivity_analysis <- function(tidy_object, type="PFI", metric = NULL){

  task = tidy_object$task

  y = all.vars(tidy_object$formula)[1]

  rec =  tidy_object$transformer %>%
    recipes::prep(training = tidy_object$train_data)

  bake_train = recipes::bake(rec, new_data = tidy_object$train_data)
  bake_test = recipes::bake(rec, new_data = tidy_object$test_data)

  model_parsnip <- tidy_object$final_models %>%
    tune::extract_fit_parsnip()

  if (is.null(tidy_object$sensitivity_analysis)){

    sensitivity_analysis_list = list()

  } else {

    sensitivity_analysis_list = tidy_object$sensitivity_analysis

  }

  feature_names <- names(bake_train)[which(names(bake_test) != y)]

  if (type == "PFI"){

    pfi_plot(tidy_object, new_data = new_data, metric = metric)

  }

  else if (type == "SHAP"){

    results <- shap_calc(model = model_parsnip, train = bake_train, test = bake_test, y = y, task = task)

    sensitivity_analysis_list[["SHAP"]] <- results

    test <- bake_test[which(names(bake_test) != y)]



    plot_barplot(results, func = function(x) mean(abs(x)),
                 func_se = function(x) sd(abs(x)),
                 x_label = "Mean |SHAP|",
                 title = "Mean |SHAP| value")

    plot_boxplot(results, y_label = "SHAP value", title = "SHAP Value Distribution")

    plot_beeswarm(results, X_orig = test, x_label = "SHAP value", title = "SHAP Swarm Plot")

  }

  else if (type == "Integrated Gradients"){

    results <- IntGrad_calc(model = model_parsnip, train = bake_train, test = bake_test, y, task)

    sensitivity_analysis_list[["IntegratedGradients"]] <- results

    test <- bake_test[which(names(bake_test) != y)]

    plot_barplot(results, func = function(x) mean(abs(x)),
                 func_se = function(x) sd(abs(x)),
                 x_label = "Mean |Integrated Gradient|",
                 title = "Mean |Integrated Gradient| value")

    plot_boxplot(results, y_label = "Integrated Gradient value", title = "Integrated Gradient Distribution")

    plot_beeswarm(results, X_orig = test, x_label = "Integrated Gradient value",
                  title = "Integrated Gradient Swarm Plot")

  }

  else if (type == "Olden"){

    results = olden_calc(model = model_parsnip, task)

    sensitivity_analysis_list[["Olden"]] <- results

    olden_barplot(results, feature_names)

  }

  else {

    stop("Method not recognized. Recognized methods are: 'PFI', 'SHAP', 'Integrated Gradients'")

  }

  tidy_object$modify("sensitivity_analysis", sensitivity_analysis_list)

  return(tidy_object)

}

####################
#   Global Plots   #
####################

#### plot_global

plot_barplot <- function(X, func = base::mean, func_se = stats::sd, title, x_label) {

  X <- base::as.data.frame(X)

  summary_df <- tibble::tibble(
      variable = base::colnames(X),
      value = base::sapply(X, func),
      se = base::sapply(X, func_se)
    )

  summary_df$variable <- factor(summary_df$variable,
                                levels = summary_df$variable[order(summary_df$value, decreasing = F)])

    p <- ggplot2::ggplot(summary_df, ggplot2::aes(x = value, y = variable)) +
      ggplot2::geom_col(fill = "steelblue", width = 0.7) +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = value - se, xmax = value + se), width = 0.2) +
      ggplot2::geom_text(aes(label = paste0(round(value, 2), " ± ", round(se, 2))),
                vjust =  -0.5,
                hjust = -0.2) +
      ggplot2::labs(
        x_label = x_label,
        y = "Feature",
        title = title
        ) +
      ggplot2::theme_grey()

    print(p)

  }

plot_boxplot <- function(X, title, y_label){

  X <- base::as.data.frame(X)

  summary_df <- tibble::tibble(
    variable = base::colnames(X),
    value = base::sapply(X, function(x) mean(abs(x)))
  )

    X_long <- tidyr::pivot_longer(
      data = X,
      cols = tidyselect::everything(),
      names_to = "variable",
      values_to = "value"
    )

    X_long$variable <- factor(X_long$variable,
                                  levels = summary_df$variable[order(summary_df$value, decreasing = T)])

    p <- ggplot2::ggplot(X_long, ggplot2::aes(x = variable, y = value)) +
      ggplot2::geom_boxplot(fill = "lightgray") +
      ggplot2::labs(
        x = "Feature",
        y = y_label,
        title = title
      ) +
      ggplot2::theme_grey() +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

    print(p)

}

###### Beeswarm

plot_beeswarm <- function(X_vals, X_orig, title, x_label){

  order_df <- tibble::tibble(
    variable = base::colnames(X_vals),
    value = base::sapply(X_vals, function(x) mean(abs(x)))
  )


  summary_df <- X_vals %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "value")

  summary_df$variable <- factor(summary_df$variable,
                            levels = order_df$variable[order(order_df$value, decreasing = F)])

  X <- X_orig %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "val_color")

  X$variable <- factor(X$variable,
                                 levels = order_df$variable[order(order_df$value, decreasing = F)])

  summary_df["val_color"] = X["val_color"]


  p <- ggplot2::ggplot(summary_df, aes(x=value, y=variable, color = val_color)) +
    ggbeeswarm::geom_quasirandom(bandwidth = 0.2, method = "pseudorandom", cex = 2, orientation = 'x') +
    ggplot2::labs(x = x_label, y = "Feature" ,title = title) +
    ggplot2::theme_grey() +
    ggplot2::scale_color_viridis_c(option = "A")

  print(p)

}

###########################
#   Prediction Wrappers   #
###########################

pred_reg <- function(object, newdata){

  return(predict(object,new_data= newdata)$.pred)

}

pred_bin <- function(object, newdata){

  return(predict(object, new_data = newdata, type = "prob")[,2])

}



