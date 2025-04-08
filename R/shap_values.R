###########################
#     Regression          #
###########################

shap_reg <- function(tidy_object, new_data = "test", metric = NULL){

  if (is.null(metric)){metric = tidy_object$metrics[1]}

  y = all.vars(tidy_object$formula)[1]

  pfun <- function(object, newdata){

    pred = predict(object, new_data = newdata)

    return(pred$.pred)

  }

  model_parsnip <- tidy_object$final_models %>%
    tune::extract_fit_parsnip()


  dat = tidy_object$transformer %>%
    recipes::prep(training = tidy_object$train_data) %>%
    recipes::bake(new_data = tidy_object[[paste0(new_data, "new_data")]])

  feat_names <- names(dat[names(dat) != y])

  vis <- vip::vi(model_parsnip,
                 method = "shap",
                 feature_names = feat_names,
                 train = dat,
                 pred_wrapper = pfun,
                 nsim = 10,
                 keep = F)

  print(vis)


  vip::vip(vis, include_type = TRUE, all_permutations = TRUE,
           geom = "boxplot", aesthetics = list(color = "lightblue", width = 0.3))



}

pfun <- function(object, newdata){

  pred = predict(object, new_data = newdata)

  return(pred$.pred)

}

# model_parsnip <- model_fit$final_models %>%
#   tune::extract_fit_parsnip()
#
#
# dat = model_fit$transformer %>%
#   recipes::prep(training = model_fit$train_data) %>%
#   recipes::bake(new_data = model_fit[[paste0("test", "new_data")]])
#
# y = all.vars(model_fit$formula)[1]
#
# explainer <- DALEX::explain(model_parsnip,
#                                     data = dat,
#                                     y = dat[[y]],
#                                     predict_function = pfun,
#                                     label = "RF",
#                                     colorize = FALSE,
#                                     verbose = FALSE)
#
# vi_regr_bt <- DALEX::model_parts(explainer, loss_function = DALEX::loss_root_mean_square)
#
# plot(vi_regr_bt)
#
# pepe = DALEX::predict_parts(explainer, new_observation = dat[1,], type = "shap")
# plot(pepe)

shap_global <- function(dat, y, model){

  X <- dat[which(names(dat) != y)]
  Y <- dat[[y]]

  calc_shap <- function(i, val){

    shapley <- iml::Shapley$new(predictor, x.interest = X[i,], sample.size = 50)
    results <- shapley$results[[val]]
    names(results) <- names(X)

    return(results)

  }

  predictor <- iml::Predictor$new(model, data = X, y = Y)

  shap_vals_list <- lapply(1:nrow(X), function(i) as.list(calc_shap(i, "phi")))
  shap_vals_df <- rbindlist(shap_vals_list)

  shap_std_list <- lapply(1:nrow(X), function(i) as.list(calc_shap(i, "phi.var")))
  shap_std_df <- rbindlist(shap_std_list)

  results_shap = list(

    shap_vals = shap_vals_df,
    shap_std = shap_std_df

  )

  return(results_shap)

}

plot_global_shap <- function(shap_vals){

  summary_df <- shap_vals %>%
    pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "value") %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(
      mean = mean(abs(value)),
      std = sd(abs(value))
    )

  ggplot2::ggplot(summary_df, aes(x=mean, y=variable)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    ggplot2::geom_errorbar(aes(xmin = mean - std, xmax = mean + std), width = 0.2) +
    ggplot2::labs(x = "Mean ± SD", y = "Variable") +
    ggplot2::theme_minimal()

}

plot_shap_bee <- function(shap_vals, dat, y){

  X <- dat[which(names(dat) != y)]

  summary_df <- shap_vals %>%
    pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "value")

  X <- X %>%
    pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "val_color")

 summary_df["val_color"] = X["val_color"]


  ggplot2::ggplot(summary_df, aes(x=value, y=variable, color = val_color)) +
    ggbeeswarm::geom_beeswarm(cex = 0.4) +
    ggplot2::labs(x = "Mean ± SD", y = "Variable") +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_gradient(low = "blue", high = "red")

}

