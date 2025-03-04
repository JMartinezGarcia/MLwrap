TidyMLObject <- R6::R6Class("TidyMLObject",

  public = list(

    full_data = NULL,
    train = NULL,
    validation = NULL,
    test = NULL,
    transformer = NULL,
    hyperparameters = NULL,
    models = NULL,
    workflow = NULL,
    metrics = NULL,
    tuner = NULL,
    tuner_fit = NULL,
    final_models = NULL,
    task = NULL,

    initialize = function(full_data, transformer){

      self$full_data = full_data
      self$train = train
      self$validation = validation
      self$test = test
      self$transformer = transformer
      self$hyperparameters = NULL
      self$models = NULL
      self$workflow = NULL
      self$metrics = NULL
      self$tuner = NULL
      self$tuner_fit = NULL
      self$final_models = NULL
      self$task = NULL
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

    add_final_models = function(models){

      self$final_models = models

    },

    add_task = function(task){

      self$task = task

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
          tuning = FALSE,

          initialize = function(n_neurons = NULL, learning_rate = NULL, activation_func = NULL){

            self$n_neurons = n_neurons
            self$learning_rate = learning_rate
            self$activation_func = activation_func

            self$tuning = tuning

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
              self$tuning = TRUE

            } else if (length(self$n_neurons) == 1) {

              self$n_neurons_tune = self$n_neurons

            } else {

              self$n_neurons_tune = tune::tune()
              self$tuning = TRUE

            }

          },

          #### Check n_layers

          check_learning_rate = function(){

            if (is.null(self$learning_rate)){

              #### DEFAULT VALUES
              self$learning_rate = c(1e-3, 1e-1)
              self$learning_rate_tune = tune::tune()
              self$tuning = TRUE

            } else if (length(self$learning_rate) == 1) {

              self$learning_rate_tune = self$learning_rate

            } else {

              self$learning_rate_tune = tune::tune()
              self$tuning = TRUE

            }
          },

            #### Check Activation Function

            check_activation_function = function(){

              if (is.null(self$activation_func)){

                #### DEFAULT VALUES
                self$activation_func = c("tanh", "relu", "sigmoid")
                self$activation_func_tune = tune::tune()
                self$tuning = TRUE

              } else if (length(self$activation_func) == 1) {

                self$activation_func_tune = self$activation_func

              } else {

                self$activation_func_tune = tune::tune()
                self$tuning = TRUE

              }

            }
  )
)



