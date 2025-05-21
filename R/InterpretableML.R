###########################
#       Interpretable ML
###########################

#' Perform Sensitivity Analysis and Interpretable ML methods
#'
#' @param analysis_object analysis_object created from fine_tuning function.
#' @param type Type of method used. A string of the method name: "PFI" (Permutation Feature Importance),
#'     "SHAP" (SHapley Additive exPlanations), "Integrated Gradients" (Neural Network only) or
#'     "Olden" (Neural Network only).
#' @param  metric Metric used for "PFI" method (Permutation Feature Importance).
#'  A string of the name of metric (see Metrics).
#' @returns Updated analysis_object
#' @export
sensitivity_analysis <- function(analysis_object, methods = c("PFI"), metric = NULL){

  check_args_sensitivity_analysis(analysis_object = analysis_object, methods = methods, metric = metric)

  task = analysis_object$task

  y = all.vars(analysis_object$formula)[1]

  rec =  analysis_object$transformer %>%
    recipes::prep(training = analysis_object$train_data)

  bake_train = recipes::bake(rec, new_data = analysis_object$train_data)
  bake_test = recipes::bake(rec, new_data = analysis_object$test_data)

  model_parsnip <- analysis_object$final_model %>%
    tune::extract_fit_parsnip()

  if (is.null(analysis_object$sensitivity_analysis)){

    sensitivity_analysis_list = list()

  } else {

    sensitivity_analysis_list = analysis_object$sensitivity_analysis

  }

  feature_names <- names(bake_train)[which(names(bake_test) != y)]

  if ("PFI" %in% methods){

    if (is.null(metric)){

      if (task == "regression"){

        metric = "rmse"

      } else{

        metric = "roc_auc"

      }
    }

    results <- pfi_calc(model = model_parsnip, train = bake_train, test = bake_test, y = y,
                        task = task, metric = metric, outcome_levels = analysis_object$outcome_levels)

    sensitivity_analysis_list[["PFI"]] <- results

    if (analysis_object$outcome_levels > 2){

    y_classes <- levels(bake_train[[y]])

    for (target_class in y_classes){

      plot_barplot(results[[target_class]], func = NULL, title = paste0("Permutation Feature Importance for class ",
                                                        target_class), x_label = "Importance")
      }

    } else{

      plot_barplot(results, func = NULL, title = "Permutation Feature Importance", x_label = "Importance")

    }

  }

  if ("SHAP" %in% methods){

    results <- shap_calc(model = model_parsnip, train = bake_train, test = bake_test, y = y,
                         task = task, outcome_levels = analysis_object$outcome_levels)

    sensitivity_analysis_list[["SHAP"]] <- results

    test <- bake_test[which(names(bake_test) != y)]


    if (analysis_object$outcome_levels > 2){

      y_classes = levels(bake_train[[y]])

      for (target_class in y_classes){

        plot_barplot(results[[target_class]], func = function(x) mean(abs(x)),
                     func_se = function(x) sd(abs(x)) / sqrt(length(x)),
                     x_label = "Mean |SHAP|",
                     title = paste0("Mean |SHAP| value for class ", target_class)
                     )

        plot2(results[[target_class]], test, func = function(x) mean(x),
              func_se = function(x) sd(x),
              x_label = "Mean (SHAP * sign(X))",
              title = paste0("Mean (SHAP * sign(X)) value for class ", target_class))

        plot_boxplot(results[[target_class]], y_label = "SHAP value",
                     title = paste0("SHAP Value Distribution for class ", target_class))

        plot_beeswarm(results[[target_class]], X_orig = test, x_label = "SHAP value",
                      title = paste0("SHAP Swarm Plot for class ", target_class))

      }

    } else{

    plot_barplot(results, func = function(x) mean(abs(x)),
                 func_se = function(x) sd(abs(x)) / sqrt(length(x)),
                 x_label = "Mean |SHAP|",
                 title = "Mean |SHAP| value")

      plot2(results, test, func = function(x) mean(x),
            func_se = function(x) sd(x),
            x_label = "Mean (SHAP * sign(X))",
            title = "Mean (SHAP * sign(X)) value")

    plot_boxplot(results, y_label = "SHAP value", title = "SHAP Value Distribution")

    plot_beeswarm(results, X_orig = test, x_label = "SHAP value", title = "SHAP Swarm Plot")

    }

  }

  if ("Integrated Gradients" %in% methods){

    results <- IntGrad_calc(model = model_parsnip, train = bake_train, test = bake_test, y = y,
                            task = task, outcome_levels = analysis_object$outcome_levels)

    sensitivity_analysis_list[["IntegratedGradients"]] <- results

    test <- bake_test[which(names(bake_test) != y)]

    if (analysis_object$outcome_levels > 2){

      y_classes = levels(bake_train[[y]])

      for (target_class in y_classes){

        plot_barplot(results[[target_class]], func = function(x) mean(abs(x)),
                     func_se = function(x) sd(abs(x)) / sqrt(length(x)),
                     x_label = "Mean |Integrated Gradient|",
                     title = paste0("Mean |Integrated Gradient| value for class ", target_class)
        )

        plot2(results[[target_class]], test, func = function(x) mean(x),
              func_se = function(x) sd(x),
              x_label = "Mean (Integrated Gradient * sign(X))",
              title = paste0("Mean (Integrated Gradient * sign(X)) value for class ", target_class))

        plot_boxplot(results[[target_class]], y_label = "Integrated Gradient value",
                     title = paste0("Integrated Gradient Value Distribution for class ", target_class))

        plot_beeswarm(results[[target_class]], X_orig = test, x_label = "SHAP value",
                      title = paste0("Integrated Gradient Swarm Plot for class ", target_class))

      }

    } else{

      plot_barplot(results, func = function(x) mean(abs(x)),
                   func_se = function(x) sd(abs(x)) / sqrt(length(x)),
                   x_label = "Mean |Integrated Gradient|",
                   title = "Mean |Integrated Gradient| value")

      plot2(results, test, func = function(x) mean(x),
            func_se = function(x) sd(x),
            x_label = "Mean (Integrated Gradient * sign(X))",
            title = "Mean (Integrated Gradient * sign(X)) value")



      plot_boxplot(results, y_label = "Integrated Gradient value", title = "Integrated Gradient Distribution")

      plot_beeswarm(results, X_orig = test, x_label = "Integrated Gradient value",
                    title = "Integrated Gradient Swarm Plot")

    }

  }

  if ("Olden" %in% methods){

    y_classes = levels(bake_train[[y]])

    results = olden_calc(model = model_parsnip, task,
                         outcome_levels = analysis_object$outcome_levels, y_classes = y_classes)

    df_results <- as.data.frame(t(results))

    names(df_results) <- feature_names

    sensitivity_analysis_list[["Olden"]] <- df_results

    if (analysis_object$outcome_levels > 2){

      olden_barplot_mul(results, feature_names, outcome_levels = analysis_object$outcome_levels,
                        y_classes = y_classes)

    } else{

      olden_barplot(results, feature_names)

    }

  }

  analysis_object$modify("sensitivity_analysis", sensitivity_analysis_list)

  return(analysis_object)

}

####################
#   Global Plots   #
####################

#### plot_global

plot_barplot <- function(X, func = NULL, func_se = stats::sd, title, x_label) {

  X <- base::as.data.frame(X)

  if (!is.null(func)){

  summary_df <- tibble::tibble(
      Variable = base::colnames(X),
      Importance = base::sapply(X, func),
      StDev = base::sapply(X, func_se)
    )

  } else{summary_df <- X}



  summary_df$Variable <- factor(summary_df$Variable,
                                levels = summary_df$Variable[order(summary_df$Importance, decreasing = F)])

    p <- ggplot2::ggplot(summary_df, ggplot2::aes(x = Importance, y = Variable)) +
      ggplot2::geom_col(fill = "steelblue", width = 0.7) +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = Importance - StDev, xmax = Importance + StDev), width = 0.2) +
      ggplot2::geom_text(aes(label = paste0(round(Importance, 3), " ± ", round(StDev, 3))),
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


plot2 <- function(X, test, func = NULL, func_se = stats::sd, title, x_label) {

  X <- base::as.data.frame(X)

  #sign_results = as.data.frame(as.matrix(X) * sign(as.matrix(test)))

  X_mat <- as.matrix(X)
  test_mat <- as.matrix(test)

  # Paso 1: multiplicación elemento a elemento
  product <- X_mat * test_mat

  # Paso 2: divisor por columna = media del valor absoluto de test
  denominator <- colMeans(abs(test_mat))

  # Paso 3: dividir cada columna por su media
  sign_results <- sweep(product, 2, denominator, "/")

  # Si quieres data.frame al final:
  sign_results <- as.data.frame(sign_results)

  names(sign_results) <- names(X)

   df <- tibble::tibble(
    variable = base::colnames(sign_results),
    importance = as.numeric(base::sapply(sign_results, func))
  )

  # Order decreasing
  df$variable <- factor(df$variable, levels = df$variable[order(df$importance, decreasing = T)])

  # Plot
  p <- ggplot2::ggplot(df, aes(x = variable, y = importance, fill = importance > 0)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(aes(label = round(importance, 3)),
                       vjust = ifelse(df$importance >= 0, -0.5, 1.2)) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick")) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title = title,
      x = "Feature",
      y = "Olden Feature Importance"
    ) +
    ggplot2::theme_grey() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

  return(predict(object, new_data = newdata, type = "prob")[,2][[1]])

}

pred_bin_class <- function(object, newdata){

  return(predict(object, new_data = newdata, type = "class")$.pred_class)

}



