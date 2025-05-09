pfi_plot <- function(tidy_object, new_data = "test", metric = NULL){

  if (tidy_object$task == "regression"){

    pfi_reg(tidy_object, new_data = new_data, metric = metric)

  } else if (tidy_object$task == "classification"){

    pfi_bin(tidy_object, new_data = new_data, metric = metric)

  }

}


                                       ###########################
                                        #     Regression          #
                                        ###########################

pfi_calc <- function(model, train, test, y, task){

  if (task == "regression"){

    pfi_reg(model, train, test, y)

  } else {

    pfi_bin(model, train, test, y)

  }

}

pfi_reg <- function(model, test, y, metric, pred_func){

  vis <- vip::vi(model,
                 method = "permute",
                 nsim = 25,
                 metric = metric,
                 train = test,
                 target = y,
                 pred_wrapper = pred_func)

  plot <- vip::vip(vis, include_type = TRUE, all_permutations = TRUE,
                geom = "boxplot", aesthetics = list(color = "lightblue", width = 0.3)) +

          ggplot2::geom_text(aes(label = paste0(round(Importance, 2), " ± ", round(StDev, 2))),
                                                    vjust =  -1.5,
                                                    hjust = 0.5)

  return(plot)


}

pfi_reg <- function(tidy_object, new_data = "test", metric = NULL){

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

  vis <- vip::vi(model_parsnip,
                 method = "permute",
                 nsim = 10,
                 metric = metric,
                 train = dat,
                 target = y,
                 pred_wrapper = pfun) #, event_level = "second")

  p <- vip::vip(vis, include_type = TRUE, all_permutations = TRUE,
           geom = "boxplot", aesthetics = list(color = "lightblue", width = 0.3))

  print(p)

}

                                #####################################
                                #     Binary Classification         #
                                #####################################

pfi_bin <- function(tidy_object, new_data = "test", metric = NULL){

  if (is.null(metric)){metric = tidy_object$metrics[1]}

  y = all.vars(tidy_object$formula)[1]

  if (metrics_info[[metric]][1] == "prob"){

    positive_class = levels(as.factor(tidy_object[["test_data"]][[y]]))[2]

    predicted = paste0(".pred_", positive_class)

    type = "prob"

  } else {

    predicted = ".pred_class"

    type = "class"


  }

  pred_func_wrapper <- function(type = type){

    function(object, new_data){

    predict(object, new_data,  type = type)

    }

  }

  pred_func <- pred_func_wrapper(type = type)

  force(predicted)

  pfun <- function(object, newdata){

      pred = pred_func(object = object, new_data = newdata)

      return(pred[[predicted]])

    }

  model_parsnip <- tidy_object$final_models %>%
    tune::extract_fit_parsnip()

  dat = tidy_object$transformer %>%
    recipes::prep(training = tidy_object$train_data) %>%
    recipes::bake(new_data = tidy_object[[paste0(new_data, "new_data")]])

  vis <- vip::vi(model_parsnip,
                 method = "permute",
                 nsim = 10,
                 metric = metric,
                 train = dat,
                 target = y,
                 pred_wrapper = pfun,
                 event_level = "second")

  p <- vip::vip(vis, include_type = TRUE, all_permutations = TRUE,
           geom = "boxplot", aesthetics = list(color = "lightblue", width = 0.3))

  print(p)

}

#########################################
#     Multiclass Classification         #
#########################################

pfi_multiclass <- function(tidy_object, new_data = "test", metric = NULL){

  if (is.null(metric)){metric = yardstick::roc_auc}#tidy_object$metrics[1]}

  metric2 = "roc_auc"

  y = all.vars(tidy_object$formula)[1]

  y_classes = levels(tidy_object$full_data[[y]])

  if (metrics_info[[metric2]][1] == "prob"){

    predicted = unlist(lapply(y_classes, function(target_class) paste0(".pred_", target_class)))

    type = "prob"

  } else {

    predicted = ".pred_class"

    type = "class"

  }

  pred_func_wrapper <- function(type = type){

    function(object, new_data){

      pred = predict(object, new_data,  type = type)

      if (type == "class"){

        return(pred[[predicted]])

      } else {

        return(pred)

        }

    }

  }

  pred_func <- pred_func_wrapper(type = type)

  force(predicted)

  pfun <- function(object, newdata){

    pred = pred_func(object = object, new_data = newdata)

    print(pred)

    return(pred)

  }

  model_parsnip <- tidy_object$final_models %>%
    tune::extract_fit_parsnip()

  dat = tidy_object$transformer %>%
    recipes::prep(training = tidy_object$train_data) %>%
    recipes::bake(new_data = tidy_object[[paste0(new_data, "new_data")]])

  vis <- vip::vi(model_parsnip,
                 method = "permute",
                 nsim = 10,
                 metric = function(truth, estimate){yardstick::roc_auc_vec(truth, estimate)},
                 train = dat,
                 target = y,
                 pred_wrapper = pfun,
                 smaller_is_better = F)

  p <- vip::vip(vis, include_type = TRUE, all_permutations = TRUE,
                geom = "boxplot", aesthetics = list(color = "lightblue", width = 0.3))

  print(p)

}


