### Tuning Results
#' Best Hyperparameter Configuration
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @param show_table Boolean. Whether to print the table.
#'
#' @returns Tibble with best hyperparameter configuration.
#'
#' @export
table_best_hyperparameters <- function(analysis_object, show_table = FALSE){

  if (analysis_object$stage != "fit_model"){

    stop("You must first fit a model with 'train_model()'!")

  }

  if (is.null(analysis_object$tuner_fit)){

    stop("All hyperparameters had fixed values, no hyperparameter tuning was performed!")

  }

  best_hyper <- tune::show_best(analysis_object$tuner_fit, metric = analysis_object$metrics[1], n = 1)

  best_hyper <- c(analysis_object$hyperparameters$hyperparams_constant,
                         as.list(best_hyper))

  best_hyp <- tibble::as_tibble(best_hyper)

  if (base::interactive() && show_table == TRUE){

    cli::cat_line()

    cli::cli_h1("Best Hyperparameter Configuration")

    cli::cat_line()

    print(best_hyp)

  }

  invisible(best_hyp)

}


### Results

#' Evaluation Results
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @param show_table Boolean. Whether to print the table.
#'
#' @returns Tibble or list of tibbles (multiclass classification) with evaluation results.
#'
#' @export
table_evaluation_results <- function(analysis_object, show_table = FALSE){

  if (analysis_object$stage != "fit_model"){

    stop("You must first fit a model with 'train_model()'!")

  }

  tables <- analysis_object$tables

  result = list()

  if (analysis_object$outcome_levels > 2){

    result$summary_train <- tibble::as_tibble(tables$summary_train)

    result$summary_test <- tibble::as_tibble(tables$summary_test)

  } else {

    result <- tibble::as_tibble(tables$summary_results)

  }

  if (base::interactive() && show_table){

    cli::cli_h1("Evaluation Results")

    cli::cat_line()

    if (analysis_object$outcome_levels > 2){

      cli::cli_h2("Train Data Evaluation Results")

      print(tables$summary_train)

      cli::cat_line()

      cli::cli_h2("Test Data Evaluation Results")

      print(tables$summary_test)

    } else {

      print(tables$summary_results)


    }

  }

  invisible(result)

}

#### Sensitivity Analysis

#' Permutation Feature Importance Results Table
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "PFI")'.
#'
#' @param show_table Boolean. Whether to print the table.
#'
#' @returns Tibble or list of tibbles (multiclass classification) with PFI results.
#'
#' @export
table_pfi_results <- function(analysis_object, show_table = FALSE){

  tables <- analysis_object$tables

  pfi_names   <- grep("^PFI", names(tables), value = TRUE)

  if (length(pfi_names) == 0){

    stop("You first need to compute PFI values using 'sensitivity_analysis()'!")

  }

  pfi_tables <- tables[pfi_names]

  if (base::interactive() && show_table){

    cli::cat_line()

    cli::cli_h1("Permutation Feature Importance Results")

    if (analysis_object$outcome_levels > 2){

      N <- length(pfi_names)

      for (i in 1:N){

        cli::cli_h2(sub(".*_", "", pfi_names[[i]]))

        print(pfi_tables[[i]])

        cli::cat_line()

      }

    } else{

      print(pfi_tables)

    }

  }

  invisible(pfi_tables)

}

#' SHAP Summarized Results Table
#'
#' @description
#'
#' To summarize the SHAP values calculated, three different metrics are computed:
#'
#' * **Mean Absolute Value**
#' * **Standard Deviation of Mean Absolute Value**
#' * **Directional Sensitivity Value** (Cov(Feature values, SHAP values) / Var(Feature values))
#'
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "SHAP")'.
#'
#' @param show_table Boolean. Whether to print the table.
#'
#' @returns Tibble or list of tibbles (multiclass classification) with SHAP summarized results.
#'
#' @export
table_shap_results <- function(analysis_object, show_table = FALSE){

  tables <- analysis_object$tables

  shap_names   <- grep("^SHAP", names(tables), value = TRUE)

  if (length(shap_names) == 0){

    stop("You first need to compute SHAP values using 'sensitivity_analysis()'!")

  }

  shap_tables <- tables[shap_names]

  if (base::interactive() && show_table){

    cli::cat_line()

    cli::cli_h1("SHAP Importance Results")

    if (analysis_object$outcome_levels > 2){

      N <- length(shap_names)

      for (i in 1:N){

        cli::cli_h2(sub(".*_", "", shap_names[[i]]))

        print(shap_tables[[i]])

        cli::cat_line()

      }

    } else{

      print(shap_tables)

    }

  }

  invisible(shap_tables)

}

#' Integrated Gradients Summarized Results Table
#'
#' @description
#'
#' To summarize the Integrated Gradients values calculated, three different metrics are computed:
#'
#' * **Mean Absolute Value**
#' * **Standard Deviation of Mean Absolute Value**
#' * **Directional Sensitivity Value** (Cov(Feature values, IG values) / Var(Feature values))
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "Integrated Gradients")'.
#'
#' @param show_table Boolean. Whether to print the table.
#'
#'
#' @returns Tibble or list of tibbles (multiclass classification) with Integrated Gradient summarized results.
#'
#' @export
table_integrated_gradients_results <- function(analysis_object, show_table = FALSE){

  tables <- analysis_object$tables

  ig_names   <- grep("^Integrated", names(tables), value = TRUE)

  if (length(ig_names) == 0){

    stop("You first need to compute Integrated Gradients values using 'sensitivity_analysis()'!")

  }

  ig_tables <- tables[ig_names]

  if (base::interactive() && show_table){

    cli::cat_line()

    cli::cli_h1("Integrated Gradients Importance Results")

    if (analysis_object$outcome_levels > 2){

      N <- length(ig_names)

      for (i in 1:N){

        cli::cli_h2(sub(".*_", "", ig_names[[i]]))

        print(ig_tables[[i]])

        cli::cat_line()

      }

    } else{

      print(ig_tables)

    }

  }

  invisible(ig_tables)

}

#' Olden Results Table
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "Olden")'.
#'
#' @param show_table Boolean. Whether to print the table.
#'
#' @returns Tibble with Olden results.
#'
#' @export
table_olden_results <- function(analysis_object, show_table = FALSE){

  olden <- analysis_object$tables$Olden

  if (is.null(olden)){

    stop("You first need to compute Olden values using 'sensitivity_analysis()'!")

  }

  if (base::interactive() && show_table){

    cli::cat_line()

    cli::cli_h1("Olden Importance Results")

    print(olden)

    cli::cat_line()

  }

  invisible(olden)

}

#' Sobol-Jansen Results Table
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "Sobol_Jansen")'.
#'
#' @param show_table Boolean. Whether to print the table.
#'
#' @returns Tibble with Sobol-Jansen results.
#'
#' @export
table_sobol_jansen_results <- function(analysis_object, show_table = FALSE){

  ### Check_args

  sobol <- analysis_object$tables$Sobol_Jansen

  if (is.null(pfi_names)){

    stop("You first need to compute sobol_jansen values using 'sensitivity_analysis()'!")

  }

  if (base::interactive() && show_table){

    cli::cli_h1("Sobol_Jansen Importance Results")

    print(sobol)

    cli::cat_line()

  }


  invisible(sobol)

}
