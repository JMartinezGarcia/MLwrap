###########################
#       Interpretable ML
###########################

permutation_feature_importance <- function(tidy_object, new_data = "test"){

  y = all.vars(tidy_object$formula)[1]



  pfun <- function(object, newdata){

    pred = predict(object, new_data = newdata, type = "prob")

    return(pred[[2]])

  }

  model_parsnip <- tidy_object$final_models


  dat = tidy_object$transformer %>%
    recipes::prep(training = tidy_object$train_data) %>%
    recipes::bake(new_data = tidy_object[[paste0(new_data, "new_data")]])

  vis <- vip::vi(model_parsnip,
                 method = "permute",
                 nsim = 10,
                 metric = metric,
                 train = dat,
                 target = y,
                 pred_wrapper = pfun) #, event_level = "second")

  vip::vip(vis, include_type = TRUE, all_permutations = TRUE,
           geom = "boxplot", aesthetics = list(color = "lightblue", width = 0.3))

}
