TidyMLObject <- R6::R6Class("TidyMLObject",

  public = list(

    full_data = NULL,
    train = NULL,
    test = NULL,
    transformer = NULL,
    hyperparameters = NULL,
    models = NULL,
    workflow = NULL,
    metrics = NULL,
    tuner = NULL,

    initialize = function(full_data, train_data, test_data, transformer){

      self$full_data = full_data
      self$train = train_data
      self$test = test_data
      self$transformer = transformer
      self$hyperparameters = NULL
      self$models = NULL
      self$workflow = NULL
      self$metrics = NULL
      self$tuner = NULL
    },

    add_models = function(models){

      self$models <- models
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
    }

  )

)

HyperparametersNN <- R6::R6Class("Neural Network Hyperparameters",

      public = list(

          n_neurons = NULL,
          learning_rate = NULL,
          activation_func = NULL,
          n_neurons_tune = NULL,
          learning_rate_tune = NULL,
          activation_func_tune = NULL,

          initialize = function(n_neurons = NULL, learning_rate = NULL, activation_func = NULL){

            self$n_neurons = n_neurons
            self$learning_rate = learning_rate
            self$activation_func = activation_func

            self$check_n_neurons()
            self$check_learning_rate()
            self$check_activation_function()

          },

          #### Check n_neurons

          check_n_neurons = function(){

            if (is.null(self$n_neurons)){

              #### DEFAULT VALUES
              self$n_neurons = c(5,20)
              self$n_neurons_tune = tune::tune()

            } else if (length(self$n_neurons) == 1) {

              self$n_neurons_tune = self$n_neurons

            } else {

              self$n_neurons_tune = tune::tune()

            }

          },

          #### Check n_layers

          check_learning_rate = function(){

            if (is.null(self$learning_rate)){

              #### DEFAULT VALUES
              self$learning_rate = c(1e-3, 1e-1)
              self$learning_rate_tune = tune::tune()

            } else if (length(self$learning_rate) == 1) {

              self$learning_rate_tune = self$learning_rate

            } else {

              self$learning_rate_tune = tune::tune()

            }
          },

            #### Check Activation Function

            check_activation_function = function(){

              if (is.null(self$activation_func)){

                #### DEFAULT VALUES
                self$activation_func = c("tanh", "relu", "sigmoid")
                self$activation_func_tune = tune::tune()

              } else if (length(self$activation_func) == 1) {

                self$activation_func_tune = self$activation_func

              } else {

                self$activation_func_tune = tune::tune()

              }

            }
  )
)



