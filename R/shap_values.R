shap_calc <- function(model, train, test, y, task, outcome_levels, use_test = FALSE){
  
  
  if (task == "regression"){
    
    shap_reg(model, train, test, y, use_test = use_test)
    
  } else if (task == "classification"){
    
    if (outcome_levels == 2){
      
      shap_bin(model, train, test, y, use_test = use_test)
      
    } else {
      
      shap_mul(model, train, test, y, use_test = use_test)
      
    }
    
  }
  
}

###########################
#     Regression          #
###########################

shap_reg <- function(model, train, test, y, use_test){
  
  x_train <- train[which(names(train) != y)]
  x_test <- test[which(names(test) != y)]
  
  features <- names(x_train)
  
  y_total = rbind(train[y], test[y])
  
  phi0 = mean(y_total[[y]])
  
  shap_exp <- shapr::explain(
    model = model,
    x_explain = if (use_test) x_test else x_train,
    x_train = rbind(x_train, x_test),
    approach = "gaussian",
    phi0 = phi0,
    seed = 42,
    verbose = NULL,
    predict_model = pred_reg,
    n_MC_samples = 300,
    iterative = TRUE,
    iterative_args = list(
      initial_n_coalitions = min(20, 2^(ncol(x_train) - 1)), 
      fixed_n_coalitions_per_iter = min(20, 2^(ncol(x_train))),
      max_iter = 20,
      convergence_tol = 0.02
    )
  )
  
  shap_vals <- as.data.frame(shap_exp$shapley_values_est)[features]
  
  return(shap_vals)
  
}

#####################################
#     Binary Classification         #
#####################################

shap_bin <- function(model, train, test, y, use_test){
  
  x_train <- train[which(names(train) != y)]
  x_test <- test[which(names(test) != y)]
  
  features <- names(x_train)
  
  y_total = rbind(train[y], test[y])
  
  y_levels = levels(train[[y]])
  
  target_class <- y_levels[[1]]
  
  target_pred <- paste0(".pred_", target_class)
  
  y_bin = as.integer(y_total == target_class)
  
  phi0 = mean(y_bin)
  
  pred_func = function(model, newdata){
    
    preds = predict(model, new_data = newdata, type = "prob")
    
    return(preds[[target_pred]])
    
  }
  
  shap_exp <- shapr::explain(
    model = model,
    x_explain = if (use_test) x_test else x_train,
    x_train = rbind(x_train, x_test),
    approach = "gaussian",
    phi0 = phi0,
    seed = 42,
    verbose = NULL,
    predict_model = pred_func,
    n_MC_samples = 300,
    iterative = TRUE,
    iterative_args = list(
      initial_n_coalitions = min(20, 2^(ncol(x_train) - 1)), 
      fixed_n_coalitions_per_iter = min(20, 2^(ncol(x_train))),
      max_iter = 20,
      convergence_tol = 0.02
    )
  )
  
  shap_vals <- as.data.frame(shap_exp$shapley_values_est)[features]
  
  return(shap_vals)
  
}

#########################################
#     Multiclass Classification         #
#########################################

shap_mul <- function(model, train, test, y, use_test){
  
  results = list()
  
  y_classes = levels(train[[y]])
  
  x_train <- train[which(names(train) != y)]
  x_test <- test[which(names(test) != y)]
  
  features <- names(x_train)
  
  y_total = rbind(train[y], test[y])
  
  for (target_class in y_classes){
    
    y_bin = as.integer(y_total == target_class)
    
    pred_class = paste0(".pred_", target_class)
    
    phi0 = mean(y_bin)
    
    pred_func <- function(object, newdata){return(predict(model, newdata, type = "prob")[[pred_class]])}
    
    shap_exp <- shapr::explain(
      model = model,
      x_explain = if (use_test) x_test else x_train,
      x_train = rbind(x_train, x_test),
      approach = "gaussian",
      phi0 = phi0,
      seed = 42,
      verbose = NULL,
      predict_model = pred_func,
      n_MC_samples = 300,
      iterative = TRUE,
      iterative_args = list(
        initial_n_coalitions = min(20, 2^(ncol(x_train) - 1)), 
        fixed_n_coalitions_per_iter = min(20, 2^(ncol(x_train))),
        max_iter = 20,
        convergence_tol = 0.02
      )
    )
    
    results[[target_class]]  <- as.data.frame(shap_exp$shapley_values_est)[features]
    
  }
  
  return(results)
  
}

