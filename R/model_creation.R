
#' @export
build_model <- function(tidy_object, model_names, hyperparameters = NULL){

                    task = tidy_object$task

                    if (model_names == "Neural Network"){

                      hyperparams_nn = HyperparamsNN$new(hyperparameters)

                      tidy_object$modify("hyperparameters", hyperparams_nn)

                      model = create_nn(hyperparams = hyperparams_nn, task = task, epochs = 25)

                    } else if (model_names == "XGBOOST"){

                      hyperparams_xgboost = HyperparamsXGBoost$new(hyperparameters)

                      tidy_object$modify("hyperparameters", hyperparams_xgboost)

                      model = create_xgboost(hyperparams = hyperparams_xgboost, task = task)

                    } else if (model_names == "Random Forest"){


                      hyperparams_rf = HyperparamsRF$new(hyperparameters)

                      tidy_object$modify("hyperparameters", hyperparams_rf)

                      model = create_rf(hyperparams = hyperparams_rf, task = task)


                    } else if (model_names == "SVM"){

                      type = hyperparameters$type

                      hyperparams_svm = HyperparamsSVM$new(hyperparameters)

                      tidy_object$modify("hyperparameters", hyperparams_svm)

                      if (type == "rbf"){

                        model = create_svm_rbf(hyperparams = hyperparams_svm, task = task)

                      } else if (type == "poly"){

                        model = create_svm_poly(hyperparams = hyperparams_svm, task = task)

                      } else if (type == "linear"){

                        model = create_svm_linear(hyperparams = hyperparams_svm, task = task)

                      }

                    } else {

                      stop("
                      Unrecognized Model. Select from: 'Neural Network', 'XGBOOST', 'Random Forest',
                      'SVM'.
                           ")

                    }

                    tidy_object$modify("models_names", model_names)

                    tidy_object$modify("models", model)

                    return(tidy_object)
}
