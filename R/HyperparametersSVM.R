HyperparamsSVM <- R6::R6Class("Neural Network Hyperparameters",
                             inherit = HyperparametersBase,
                             public = list(

                               cost_tune = TRUE,
                               margin_tune = TRUE,
                               rbf_sigma_tune = FALSE,
                               degree_tune = FALSE,
                               scale_factor_tune = FALSE,

                               default_hyperparams = function(){
                                 list(

                                      cost = dials::cost(range = c(-3, 3)),
                                      margin = dials::svm_margin(range = c(0, 0.2))

                                 )

                               },

                               set_hyperparams = function(hyperparams = NULL){

                                 def_hyperparams = self$default_hyperparams()

                                 if (hyperparams$type == "rbf"){

                                   self$rbf_sigma_tune = TRUE
                                   def_hyperparams$rbf_sigma = dials::rbf_sigma(range = c(-5, 0))

                                 } else if (hyperparams$type == "poly"){

                                   selfdegree_tune = TRUE
                                   self$scale_factor_tune = TRUE


                                   def_hyperparams$degree = dials::degree(range = c(1,3))
                                   def_hyperparams$scale_factor = dials::scale_factor(range = c(-5, -1))

                                 }

                                 hyperparams$type  <- NULL

                                 if (!is.null(hyperparams)) {

                                   def_hyperparams[names(hyperparams)] <- Map(function(name, value) {

                                     if (length(value) > 1) {

                                       func <- get(name, envir = asNamespace("dials"))
                                       func(range = value)

                                     } else {

                                       self[[paste0(name, "_tune")]] <- FALSE
                                       value
                                     }
                                   }, names(hyperparams), hyperparams)
                                 }

                                 return(def_hyperparams)

                               }
                             )
                          )













