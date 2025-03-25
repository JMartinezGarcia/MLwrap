TidyMLObject <- R6::R6Class("TidyMLObject",

  active = list(

    full_data = function(value) {
      if (missing(value)) {
        private$.full_data
      } else {
        stop("`$full_data` is read only", call. = FALSE)
      }
    },

    train_data = function(value) {
      if (missing(value)) {
        private$.train_data
      } else {
        stop("`$train_data` is read only", call. = FALSE)
      }
    },

    test_data = function(value) {
      if (missing(value)) {
        private$.test_data
      } else {
        stop("`$test_data` is read only", call. = FALSE)
      }
    },

    validation_data = function(value) {
      if (missing(value)) {
        private$.full_data
      } else {
        stop("`$validation_data` is read only", call. = FALSE)
      }
    },

    transformer = function(value) {
      if (missing(value)) {
        private$.transformer
      } else {
        stop("`$transformer` is read only", call. = FALSE)
      }
    },

    task = function(value) {
      if (missing(value)) {
        private$.task
      } else {
        stop("`$task` is read only", call. = FALSE)
      }
    },

    hyperparameters = function(value) {
      if (missing(value)) {
        private$.hyperparameters
      } else {
        stop("`$hyperparameters` is read only", call. = FALSE)
      }
    },

    models = function(value) {
      if (missing(value)) {
        private$.models
      } else {
        stop("`$models` is read only", call. = FALSE)
      }
    },

    models_names = function(value) {
      if (missing(value)) {
        private$.models_names
      } else {
        stop("`$models_names` is read only", call. = FALSE)
      }
    },

    workflow= function(value) {
      if (missing(value)) {
        private$.workflow
      } else {
        stop("`$workflow` is read only", call. = FALSE)
      }
    },

    metrics = function(value) {
      if (missing(value)) {
        private$.metrics
      } else {
        stop("`$metrics` is read only", call. = FALSE)
      }
    },

    tuner = function(value) {
      if (missing(value)) {
        private$.tuner
      } else {
        stop("`$tuner` is read only", call. = FALSE)
      }
    },

    tuner_fit = function(value) {
      if (missing(value)) {
        private$.tuner_fit
      } else {
        stop("`$tuner_fit` is read only", call. = FALSE)
      }
    },

    final_models = function(value) {
      if (missing(value)) {
        private$.final_models
      } else {
        stop("`$final_models` is read only", call. = FALSE)
      }
    },

    formula = function(value) {
      if (missing(value)) {
        private$.formula
      } else {
        stop("`$formula` is read only", call. = FALSE)
      }
    },

    fit_summaery = function(value) {
      if (missing(value)) {
        private$.fit_summary
      } else {
        stop("`$fit_summary` is read only", call. = FALSE)
      }
    }
  ),

  private = list(

    .full_data = NULL,
    .train_data = NULL,
    .validation_data = NULL,
    .test_data = NULL,
    .transformer = NULL,
    .hyperparameters = NULL,
    .models = NULL,
    .models_names = NULL,
    .workflow = NULL,
    .metrics = NULL,
    .tuner = NULL,
    .tuner_fit = NULL,
    .final_models = NULL,
    .task = NULL,
    .formula = NULL,
    .fit_summary = NULL,

    add_formula = function(formula){

      private$.formula <- formula

    },

    add_train_data = function(train_data){

      private$.train_data <- train_data

    },

    add_validation_data = function(validation_data){

      private$.validation_data <- validation_data

    },

    add_test_data = function(test_data){

      private$.test_data <- test_data

    },

    add_models = function(models){

      private$.models <- models

    },

    add_models_names = function(models_names){

      private$.models_names <- models_names

    },

    add_hyperparameters = function(hyperparameters){

      private$.hyperparameters <- hyperparameters

    },

    add_workflow = function(workflow){

      private$.workflow <- workflow

    },

    add_tuner = function(tuner){

      private$.tuner <- tuner

    },

    add_metrics = function(metrics){

      #### CHECK METRICS!!!!

      private$.metrics <- metrics

    },

    add_tuner_fit = function(tuner_fit){

      private$.tuner_fit <- tuner_fit

    },

    add_final_models = function(final_models){

      private$.final_models = final_models

    },

    add_task = function(task){

      private$.task = task

    },

    add_fit_summary = function(fit_summary){

      private$.fit_summary = fit_summary

    }
  ),

  public = list(

    initialize = function(full_data, transformer){

      private$.full_data <- full_data
      private$.transformer <- transformer

    },

    modify = function(type, value) {

      private_names <- names(private)

      # Filtrar solo los atributos privados que comienzan con "."
      auxiliary_funcs <- grep("^\\.", private_names, value = TRUE)
      auxiliary_funcs <- sub("^\\.", "", auxiliary_funcs)

      if (!(type %in% auxiliary_funcs)){
        stop("Tipo no válido. Usa uno de los siguientes: ", paste(auxiliary_funcs), collapse = ", ")
      }

      method_name <- paste0("add_", type)  # "add_train", "add_test", "add_hyperparameters"
      private_method <- get(method_name, envir = private)  # Obtener la función privada

      private_method(value)
    }
  )
)



HyperparametersBase <- R6::R6Class("HyperparametersBase",
                               public = list(

                                 tuning = NULL,
                                 hyperparams_dials = NULL,
                                 hyperparams_constant = NULL,
                                 hyperparams_ranges = NULL,

                                 initialize = function(hyperparams = NULL){

                                   self$tuning = FALSE
                                   self$hyperparams_dials = NULL
                                   self$hyperparams_constant = NULL

                                   hyperparameters <- self$set_hyperparams(hyperparams)



                                     # List values are converted to dials::value_set, else a single value


                                   # Change hyperparams to dials::parameters

                                   # Convertir a objetos dials::parameters

                                   hyperparams_ranges <- Filter(function(x) inherits(x, "param"), hyperparameters)

                                   hyperparams_constant <- Filter(function(x) !inherits(x, "param"), hyperparameters)

                                   self$hyperparams_ranges <- hyperparams_ranges

                                   self$hyperparams_constant = hyperparams_constant

                                   if (length(hyperparams_ranges) > 0){self$tuning <- TRUE}

                                   # if (length(hyperparams_dials) > 0) {
                                   #   self$hyperparams_dials <- do.call(dials::parameters, unname(hyperparams_dials))
                                   #   self$tuning <- TRUE
                                   # }

                                  },


                                 set_hyperparams = function(hyperparams){
                                   stop("Must be implemented in the subclass")
                                 },

                                 default_hyperparams = function() {
                                   stop("Must be implemented in the subclass")
                                 }

                               )
                  )

HyperparamsNN <- R6::R6Class("Neural Network Hyperparameters",
                   inherit = HyperparametersBase,
                   public = list(

                     hidden_units_tune = TRUE,
                     learn_rate_tune = TRUE,
                     activation_tune = TRUE,
                     epochs = 10,

                     default_hyperparams = function() {
                       list(learn_rate = dials::learn_rate(range = c(-3, -1)),
                            hidden_units = dials::hidden_units(range = c(5, 20)),
                            activation = dials::activation(values = c("relu", "tanh", "sigmoid"))
                       )
                     },

                     set_hyperparams = function(hyperparams = NULL) {

                       default_hyperparameters <- self$default_hyperparams()

                       # Actualizar solo los valores proporcionados

                       if (!is.null(hyperparams)) {

                           if ("learn_rate" %in% names(hyperparams)) {

                             if (length(hyperparams$learn_rate) > 1){

                             default_hyperparameters$learn_rate <- dials::learn_rate(range = hyperparams$learn_rate)

                             } else {

                               default_hyperparameters$learn_rate <- hyperparams$learn_rate

                               self$learn_rate_tune = F

                             }

                           }

                           if ("hidden_units" %in% names(hyperparams)) {

                             if (length(hyperparams$hidden_units) > 1){

                               default_hyperparameters$hidden_units <- dials::hidden_units(range = hyperparams$hidden_units)

                             } else {

                               default_hyperparameters$hidden_units <- hyperparams$hidden_units

                               self$hidden_units_tune = F
                             }

                           }

                           if ("activation" %in% names(hyperparams)) {

                             if (length(hyperparams$activation) > 1){

                               default_hyperparameters$activation <- dials::activation(values = hyperparams$activation)

                             } else {

                               default_hyperparameters$activation <- hyperparams$activation

                               self$activation_tune = F

                             }

                           }

                       }

                       return(default_hyperparameters)

                       }

                   )

                )




