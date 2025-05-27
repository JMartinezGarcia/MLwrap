#' @export
summary.AnalysisObject <- function(object, ...) {

  y = all.vars(object$formula)[1]
  features = all.vars(object$formula)[-1]

  cat("######## \n")
  cat("Summary of AnalysisObject:\n")
  cat("Stage: ", object$stage, "\n")
  cat("Outcome Variable: ", y, "\n")
  cat("Features: ", features, "\n")
  cat("Task: ", object$task, "\n")
  cat("######## \n")
  cat("Preprocessor Steps: \n")
  print(object$transformer$steps)

  if (object$stage == "build_model"){

    cat("######## \n")
    cat("Model Specification \n")
    cat("Model Name: ", object$model_name, "\n")
    cat("Model: \n")
    print(object$model)

  }

  if (object$stage == "fit_model"){

    cat("######## \n")
    cat("Final Model Fit: \n")
    print(tune::extract_fit_parsnip(object$final_model))

  }

  if (!is.null(object$fit_summary)){

    cat("######## \n")
    cat("Summary Results: \n")
    print(tibble::as_tibble(object$fit_summary))

  }

  invisible(object)
}
