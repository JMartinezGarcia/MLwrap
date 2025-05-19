shap_calc <- function(model, train, test, y, task, outcome_levels){

  if (task == "regression"){

    shap_reg(model, train, test, y)

  } else if (task == "classification"){

    if (outcome_levels == 2){

      shap_bin(model, train, test, y)

    } else {

      shap_mul(model, train, test, y)

    }

  }

}



###########################
#     Regression          #
###########################

shap_reg <- function(model, train, test, y){

  y_vals = train[[y]]

  train <- train[which(names(train) != y)]
  test <- test[which(names(test) != y)]

  shap_vals = shapr::explain(model, phi0 = mean(y_vals),
                  approach = "empirical",
                  x_train = train,
                  x_explain = test,
                  predict_model = pred_reg,
                  max_n_coalitions = 40,
                  n_MC_samples = 1e2,
                  iterative = T)

  shap_vals = shap_vals$shapley_values_est %>% select(names(train))

  return(shap_vals)

}

##################################
#     Binary Classification      #
##################################

shap_bin <- function(model, train, test, y){

  target_class = levels(train[[y]])[1]

  y_vals = factor(ifelse(train[[y]] == target_class, 1, 0), levels = c(0,1))

  phi0 = mean(y_vals == levels(y_vals)[1])

  train <- train[which(names(train) != y)]
  test <- test[which(names(test) != y)]

  shap_vals = shapr::explain(model, phi0 = phi0,
                             approach = "empirical",
                             x_train = train,
                             x_explain = test,
                             predict_model = pred_bin,
                             max_n_coalitions = 40,
                             n_MC_samples = 1e3,
                             iterative = T,
                             verbose = NULL)

  shap_vals = shap_vals$shapley_values_est %>% select(names(train))

  return(shap_vals)

}

######################################
#     Multiclass Classification      #
######################################

shap_mul <- function(model, train, test, y){

  results = list()

  y_classes = levels(train[[y]])

  new_train <- train[which(names(train) != y)]
  new_test <- test[which(names(test) != y)]

  for (target_class in y_classes){

    y_vals = factor(ifelse(test[[y]] == target_class, 1, 0), levels = c(0,1))

    pred_class = paste0(".pred_", target_class)

    phi0 = mean(y_vals == 1)

    pred_func <- function(object, newdata){return(predict(model, newdata, type = "prob")[[pred_class]])}

    shap_vals = shapr::explain(model, phi0 = phi0,
                             approach = "empirical",
                             x_train = new_train,
                             x_explain = new_test,
                             predict_model = pred_func,
                             max_n_coalitions = 40,
                             n_MC_samples = 50,
                             iterative = T)

    shap_vals = shap_vals$shapley_values_est %>% select(names(new_train))

    results[[target_class]] <- shap_vals

  }

  return(results)

}

###########################
#     Utilities          #
###########################

shap_global <- function(dat, y, model, pfun = NULL, class = NULL){

  X <- dat[which(names(dat) != y)]
  Y <- dat[[y]]

  if (!is.null(class)){

    class = paste0(".pred_", class)

    predictor <- iml::Predictor$new(model, data = X, y = Y, type = "prob", class = class)

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
  shap_vals_df <- do.call(rbind, shap_vals_list)

  shap_std_list <- lapply(1:nrow(X), function(i) as.list(calc_shap(i, "phi.var", predictor, X)))
  shap_std_df <- do.call(rbind, shap_std_list)

  results_shap = list(

    shap_vals = as.data.frame(shap_vals_df),
    shap_std = as.data.frame(shap_std_df)

  )

  return(results_shap)

}

plot_shap_global <- function(shap_vals){

  # summary_df <- shap_vals %>%
  #   tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "value") %>%
  #   dplyr::group_by(variable) %>%
  #   dplyr::summarise(
  #     mean = mean(abs(value)),
  #     std = sd(abs(value))
  #   )

  summary_df <- rbind(
    mean = sapply(shap_vals, function(x) mean(abs(unlist(x)))),
    sd   = sapply(shap_vals, function(x) sd(abs(unlist(x))))
  ) %>%
    as.data.frame() %>%
  tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "value")

  p <- ggplot2::ggplot(summary_df, aes(x=mean, y = variable)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    ggplot2::geom_errorbar(aes(xmin = mean - std, xmax = mean + std), width = 0.2) +
    ggplot2::labs(x = "Mean ± SD", y = "Variable") +
    ggplot2::theme_minimal()

  print(p)
}

plot_shap_bee <- function(shap_vals, dat, y){

  X <- dat[which(names(dat) != y)]

  summary_df <- shap_vals %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "value")

  X <- X %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "val_color")

 summary_df["val_color"] = X["val_color"]


  p <- ggplot2::ggplot(summary_df, aes(x=value, y=variable, color = val_color)) +
    ggbeeswarm::geom_beeswarm(cex = 0.4) +
    ggplot2::labs(x = "Mean ± SD", y = "Variable") +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_gradient(low = "blue", high = "red")

  print(p)

}

