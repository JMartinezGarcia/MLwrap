                                        ###########################
                                        #     Regression          #
                                        ###########################

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

  vip::vip(vis, include_type = TRUE, all_permutations = TRUE,
           geom = "boxplot", aesthetics = list(color = "lightblue", width = 0.3))

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

  vip::vip(vis, include_type = TRUE, all_permutations = TRUE,
           geom = "boxplot", aesthetics = list(color = "lightblue", width = 0.3))

}

