
#' Create ML Model
#'
#' @param analysis_object analysis_object created from preprocessing function.
#' @param model_names Name of the ML Model. A string of the model name: "Neural Network",
#'     "Random Forest", "SVM" or "XGBOOST".
#' @param hyperparameters Hyperparameters of the ML model. List containing the name of the hyperparameter
#'  and its value or range of values.
#'
#'
#' @returns Updated analysis_object
#'
#' @section Hyperparameters:
#'
#' ## Neural Network
#'
#' Parsnip model using **brulee** engine. Hyperparameters:
#'
#' * **hidden_units**: Number of Hidden Neurons.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(5, 20).
#'
#' * **activation**: Activation Function.
#' A vector with any of ("relu", "sigmoid", "tanh") or NULL for default values c("relu", "sigmoid", "tanh").
#'
#' * **learn_rate**: Learning Rate.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(-3, -1) in log10 scale.
#'
#' ## Random Forest
#'
#' Parsnip model using **ranger** engine. Hyperparameters:
#'
#' * **trees**: Number of Trees.
#' A single value, a vector with range values `c(min_val, max_val)`. Default range c(100, 300).
#'
#' * **mtry**: Number of variables randomly selected as candidates at each split.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(3, 8).
#'
#' * **min_n**: Minimum Number of samples to split at each node.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(2, 25).
#'
#' ## XGBOOST
#'
#' Parsnip model using **xgboost** engine. Hyperparameters:
#'
#' * **trees**: Number of Trees.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(100, 300).
#'
#' * **mtry**: Number of variables randomly selected as candidates at each split.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(3, 8).
#'
#' * **min_n**: Minimum Number of samples to split at each node.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(5, 25).
#'
#' * **tree_depth**: Maximum tree depth.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(3, 10).
#'
#' * **learn_rate**: Learning Rate.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(-4, -1) in log10 scale.
#'
#' * **loss_reduction**: Minimum loss reduction required to make a further partition on a leaf node.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(-5, 1.5) in log10 scale.
#'
#' ## SVM
#'
#' Parsnip model using **kernlab** engine. Hyperparameters:
#'
#' * **cost**: Penalty parameter that regulates model complexity and misclassification tolerance.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(-3, 3) in log10 scale.
#'
#' * **margin**: Distance between the separating hyperplane and the nearest data points.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(0, 0.2).
#'
#' * **type**: Kernel to be used.
#' A single value from ("linear", "rbf", "polynomial")
#'
#' * **rbf_sigma**:
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(-5, 0) in log10 scale.
#'
#' * **degree**: Polynomial Degree (polynomial kernel only).
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(1, 3).
#'
#' * **scale_factor**: Scaling coefficient applied to inputs. (polynomial kernel only)
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(-5, -1) in log10 scale.
#'
#' @export

build_model <- function(analysis_object, model_name, hyperparameters = NULL){

                    check_args_build_model(analysis_object = analysis_object, model_name = model_name)

                    task = analysis_object$task

                    if (model_name == "Neural Network"){

                      hyperparams_nn = HyperparamsNN$new(hyperparameters)

                      analysis_object$modify("hyperparameters", hyperparams_nn)

                      model = create_nn(hyperparams = hyperparams_nn, task = task, epochs = 25)

                    } else if (model_name == "XGBOOST"){

                      hyperparams_xgboost = HyperparamsXGBoost$new(hyperparameters)

                      analysis_object$modify("hyperparameters", hyperparams_xgboost)

                      model = create_xgboost(hyperparams = hyperparams_xgboost, task = task)

                    } else if (model_name == "Random Forest"){

                      hyperparams_rf = HyperparamsRF$new(hyperparameters)

                      analysis_object$modify("hyperparameters", hyperparams_rf)

                      model = create_rf(hyperparams = hyperparams_rf, task = task)


                    } else if (model_name == "SVM"){

                      type = hyperparameters$type

                      hyperparams_svm = HyperparamsSVM$new(hyperparameters)

                      analysis_object$modify("hyperparameters", hyperparams_svm)

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

                    analysis_object$modify("model_name", model_name)

                    analysis_object$modify("model", model)

                    analysis_object$modify("stage", "build_model")

                    return(analysis_object)
}
