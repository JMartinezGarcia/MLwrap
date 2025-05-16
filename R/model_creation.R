
#' Create ML Model
#'
#' @param tidy_object Tidy_Object created from preprocessing function.
#' @param model_names Name of the ML Model. A string of the model name: "Neural Network",
#'     "Random Forest", "SVM" or "XGBOOST".
#' @param hyperparameters Hyperparameters of the ML model. List containing the name of the hyperparameter
#'  and its value or range of values.
#' @returns Updated tidy_object
#' @export

build_model <- function(tidy_object, model_name, hyperparameters = NULL){

                    check_args_build_model(tidy_object = tidy_object, model_name = model_name)

                    task = tidy_object$task

                    if (model_name == "Neural Network"){

                      hyperparams_nn = HyperparamsNN$new(hyperparameters)

                      tidy_object$modify("hyperparameters", hyperparams_nn)

                      model = create_nn(hyperparams = hyperparams_nn, task = task, epochs = 25)

                    } else if (model_name == "XGBOOST"){

                      hyperparams_xgboost = HyperparamsXGBoost$new(hyperparameters)

                      tidy_object$modify("hyperparameters", hyperparams_xgboost)

                      model = create_xgboost(hyperparams = hyperparams_xgboost, task = task)

                    } else if (model_name == "Random Forest"){


                      hyperparams_rf = HyperparamsRF$new(hyperparameters)

                      tidy_object$modify("hyperparameters", hyperparams_rf)

                      model = create_rf(hyperparams = hyperparams_rf, task = task)


                    } else if (model_name == "SVM"){

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

                    tidy_object$modify("model_name", model_name)

                    tidy_object$modify("model", model)

                    tidy_object$modify("stage", "build_model")

                    return(tidy_object)
}
