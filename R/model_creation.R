
#' Create ML Model
#'
#' @param tidy_object Tidy_Object created from preprocessing function.
#' @param model_names Name of the ML Model. A string of the model name: "Neural Network",
#'     "Random Forest", "SVM" or "XGBOOST".
#' @param hyperparameters Hyperparameters of the ML model. List containing the name of the hyperparameter
#'  and its value or range of values.
#'
#'
#' @returns Updated tidy_object
#'
#' @section Hyperparameters:
#'
#' ## Neural Network
#'
#' * **hidden_units**: Number of Hidden Neurons.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range.
#'
#' * **activation**: Activation Function.
#' A vector with any of ("relu", "sigmoid", "tanh") or NULL for default values.
#'
#' * **learn_rate**: Learning Rate.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range.
#'
#' ## Random Forest
#'
#' * **trees**: Number of Trees.
#' A single value, a vector with range values `c(min_val, max_val)`. Default range ().
#'
#' * **mtry**: Number of variables randomly selected as candidates at each split.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range.
#'
#' * **min_n**: Minimum Number of samples to split at each node.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range.
#'
#' ## XGBOOST
#'
#' * **trees**: Number of Trees.
#' A single value, a vector with range values `c(min_val, max_val)`. Default range ().
#'
#' * **mtry**: Number of variables randomly selected as candidates at each split.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range.
#'
#' * **min_n**: Minimum Number of samples to split at each node.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range.
#'
#' * **tree_depth**: Maximum tree depth.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range.
#'
#' * **learn_rate**: Learning Rate.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range.
#'
#' * **loss_reduction**: Minimum loss reduction required to make a further partition on a leaf node.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range.
#'
#' ## SVM
#'
#' * **cost**: Penalty parameter that regulates model complexity and misclassification tolerance.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range.
#'
#' * **margin**: Distance between the separating hyperplane and the nearest data points.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range.
#'
#' * **type**: Kernel to be used.
#' A single value from ("linear", "rbf", "polynomial")
#'
#' * **rbf_sigma**:
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range.
#'
#' * **degree**: Polynomial Degree (polynomial kernel only).
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range.
#'
#' * **scale_factor**: Scaling coefficient applied to inputs. (polynomial kernel only)
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range.
#'
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
