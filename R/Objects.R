TidyMLObject <- R6::R6Class("TidyMLObject",

  public = list(

    full_data = NULL,
    train = NULL,
    validation = NULL,
    test = NULL,
    transformer = NULL,
    hyperparameters = NULL,
    models = NULL,
    models_names = NULL,
    workflow = NULL,
    metrics = NULL,
    tuner = NULL,
    tuner_fit = NULL,
    final_models = NULL,
    task = NULL,
    formula = NULL,

    initialize = function(full_data, transformer){

      self$full_data = full_data
      self$transformer = transformer
      self$train = NULL
      self$validation = NULL
      self$test = NULL
      self$hyperparameters = NULL
      self$models = NULL
      self$models_names = NULL
      self$workflow = NULL
      self$metrics = NULL
      self$tuner = NULL
      self$tuner_fit = NULL
      self$final_models = NULL
      self$task = NULL
    },

    add_formula = function(formula){

      self$formula <- formula

    },

    add_train_data = function(train_data){

      self$train <- train_data

    },

    add_validation_data = function(validation_data){

      self$validation <- validation_data

    },

    add_test_data = function(test_data){

      self$test <- test_data

    },

    add_models = function(models){

      self$models <- models

    },

    add_models_names = function(models_names){

      self$models_names <- models_names

    },

    add_hyperparameters = function(hyperparameters){

      self$hyperparameters <- hyperparameters

    },

    add_workflow = function(workflow){

      self$workflow <- workflow

    },

    add_tuner = function(tuner){

      self$tuner <- tuner

    },

    add_metrics = function(metrics){

      self$metrics <- metrics

    },

    add_tuner_fit = function(tuner_fit){

      self$tuner_fit <- tuner_fit

    },

    add_final_models = function(final_models){

      self$final_models = final_models

    },

    add_task = function(task){

      self$task = task

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

                                   #self$hyperparams <- dials::parameters(!!!hyperparameters)

                                   # Convertir a objetos dials::parameters

                                   hyperparams_dials <- Filter(function(x) inherits(x, "param"), hyperparameters)

                                   self$hyperparams_ranges <- hyperparams_dials

                                   hyperparams_constant <- Filter(function(x) !inherits(x, "param"), hyperparameters)

                                   if (length(hyperparams_dials) > 0) {
                                     self$hyperparams_dials <- do.call(dials::parameters, unname(hyperparams_dials))
                                     self$tuning <- TRUE
                                   }

                                   self$hyperparams_constant = hyperparams_constant

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

                        default_hyperparameters$epochs <- dials::epochs(range= c(5L, 50L))

                       }

                       return(default_hyperparameters)

                       }

                   )

                )




