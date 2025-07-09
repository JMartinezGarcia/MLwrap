### Tuning Results


show_best_hyperparameters <- function(analysis_object){
  analysis_object$tuner_fit %>%
    tune::show_best(metric = analysis_object$metrics[1], n = 1) %>%
    print()
}


### Results

table_evaluation_results <- function(analysis_object, show_table = FALSE){

  if (analysis_object$stage != "evaluated_model"){

    stop("You must first evaluate a model with 'evaluate_model()'!")

  }

  tables <- analysis_object$tables

  result = list()

  if (analysis_object$outcome_levels > 2){

    result$summary_train <- tibble::as_tibble(tables$summary_train)

    result$summary_test <- tibble::as_tibble(tables$summary_test)

  } else {

    result <- tibble::as_tibble(tables$summary_results)

  }

  if (interactive() && show_table){

    cli::cli_h1("Evaluation Results")

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

table_pfi_results <- function(analysis_object, show_table = FALSE){

  ##check args

  tables <- analysis_object$tables

  pfi_names   <- grep("^PFI", names(tables), value = TRUE)

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

  return(pfi_tables)

}

table_shap_results <- function(analysis_object, show_table = FALSE){

  ##check_args

  tables <- analysis_object$tables

  shap_names   <- grep("^SHAP", names(tables), value = TRUE)

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

  return(shap_tables)

}

table_integrated_gradients_results <- function(analysis_object, show_table = FALSE){

  tables <- analysis_object$tables

  ig_names   <- grep("^Integrated", names(tables), value = TRUE)

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

  return(ig_tables)

}

table_olden_results <- function(analysis_object, show_table = FALSE){

  olden <- analysis_object$tables$Olden

  if (base::interactive() && show_table){

    cli::cat_line()

    cli::cli_h1("Olden Importance Results")

    print(olden)

    cli::cat_line()

  }

  return(olden)

}

table_sobol_jansen_results <- function(analysis_object, show_table = FALSE){

  ### Check_args

  sobol <- analysis_object$tables$Sobol_Jansen

  if (base::interactive() && show_table){

    cli::cli_h1("Sobol_Jansen Importance Results")

    print(sobol)

    cli::cat_line()

  }


  return(sobol)

}
