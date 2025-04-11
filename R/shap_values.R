shap_plot <- function(tidy_object, new_data = "test"){

  if (tidy_object$task == "regression"){

    shap_reg(tidy_object, new_data = new_data)

  } else if (tidy_object$task == "classification"){

    shap_bin(tidy_object, new_data = new_data)

  }

}



###########################
#     Regression          #
###########################

shap_reg <- function(tidy_object, new_data = "test"){

  y = all.vars(tidy_object$formula)[1]

  model_parsnip <- tidy_object$final_models %>%
    tune::extract_fit_parsnip()

  dat = tidy_object$transformer %>%
    recipes::prep(training = tidy_object$train_data) %>%
    recipes::bake(new_data = tidy_object[[paste0(new_data, "new_data")]])

  shaps = shap_global(dat, y, model_parsnip)

  plot_shap_global(shaps$shap_vals)

  plot_shap_bee(shaps$shap_vals, dat, y)

}

##################################
#     Binary Classification      #
##################################

shap_bin <- function(tidy_object, new_data = "test"){

  y = all.vars(tidy_object$formula)[1]

  model_parsnip <- tidy_object$final_models %>%
    tune::extract_fit_parsnip()

  dat = tidy_object$transformer %>%
    recipes::prep(training = tidy_object$train_data) %>%
    recipes::bake(new_data = tidy_object[[paste0(new_data, "new_data")]])

  shaps = shap_global(dat, y, model_parsnip, pfun_bin)

  plot_shap_global(shaps$shap_vals)

  plot_shap_bee(shaps$shap_vals, dat, y)

}

###########################
#     Utilities          #
###########################

pfun_bin <- function(object, newdata){

  pred = predict(object, new_data = newdata, type = "prob")

  return(pred[[2]])

}

shap_global <- function(dat, y, model, pfun = NULL){

  X <- dat[which(names(dat) != y)]
  Y <- dat[[y]]

  if (!is.null(pfun)){

    predictor <- iml::Predictor$new(model, data = X, y = Y, predict.function = pfun_bin)

  } else {

    predictor <- iml::Predictor$new(model, data = X, y = Y)

  }

  calc_shap <- function(i, val, predictor, X){

    shapley <- iml::Shapley$new(predictor, x.interest = X[i,], sample.size = 50)
    results <- shapley$results[[val]]
    names(results) <- names(X)

    return(results)

  }

  shap_vals_list <- lapply(1:nrow(X), function(i) as.list(calc_shap(i, "phi", predictor, X)))
  shap_vals_df <- rbindlist(shap_vals_list)

  shap_std_list <- lapply(1:nrow(X), function(i) as.list(calc_shap(i, "phi.var", predictor, X)))
  shap_std_df <- rbindlist(shap_std_list)

  results_shap = list(

    shap_vals = shap_vals_df,
    shap_std = shap_std_df

  )

  return(results_shap)

}

plot_shap_global <- function(shap_vals){

  summary_df <- shap_vals %>%
    pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "value") %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(
      mean = mean(abs(value)),
      std = sd(abs(value))
    )

  p <- ggplot2::ggplot(summary_df, aes(x=mean, y=variable)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    ggplot2::geom_errorbar(aes(xmin = mean - std, xmax = mean + std), width = 0.2) +
    ggplot2::labs(x = "Mean ± SD", y = "Variable") +
    ggplot2::theme_minimal()

  print(p)
}

plot_shap_bee <- function(shap_vals, dat, y){

  X <- dat[which(names(dat) != y)]

  summary_df <- shap_vals %>%
    pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "value")

  X <- X %>%
    pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "val_color")

 summary_df["val_color"] = X["val_color"]


  p <- ggplot2::ggplot(summary_df, aes(x=value, y=variable, color = val_color)) +
    ggbeeswarm::geom_beeswarm(cex = 0.4) +
    ggplot2::labs(x = "Mean ± SD", y = "Variable") +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_gradient(low = "blue", high = "red")

  print(p)

}

