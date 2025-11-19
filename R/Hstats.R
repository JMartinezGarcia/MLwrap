calc_hstats <- function(analysis_object){

  task <- analysis_object$task
  outcome_levels <- analysis_object$outcome_levels

  if (task == "regression"){

    h2_tables <- calc_hstats_regression(analysis_object)

  } else if (outcome_levels == 2){

    h2_tables <- calc_hstats_binary(analysis_object)

  } else {

    h2_tables <- calc_hstats_multiclass(analysis_object)

  }

  return(h2_tables)

}


calc_hstats_regression <- function(analysis_object){

  train_data <- analysis_object$data$raw$train_data %>%
    dplyr::select(-dplyr::all_of(analysis_object$dep_var))

  hstats_object <- hstats::hstats(analysis_object$final_model,
                                  X = train_data,
                                  verbose = FALSE)

  # Total H2

  num   <- hstats_object$h2_overall$num
  denom <- hstats_object$h2_overall$denom
  h2_normalized <- as.data.frame(round(sweep(num, 2, denom, "/"), 5))
  colnames(h2_normalized) <- "H^2 Normalized"

  h2_table <- h2_normalized %>%
    dplyr::mutate(Feature = rownames(.)) %>%
    dplyr::relocate(Feature, .before = 1) %>%
    dplyr::arrange(dplyr::desc(rowMeans(dplyr::select(., -Feature))))

  rownames(h2_table) <- NULL

  # Pairwise H2

  num   <- hstats_object$h2_pairwise$num
  denom <- hstats_object$h2_pairwise$denom

  h2_pairwise_norm <- round(num / denom, 5)
  h2_pairwise_norm <- h2_pairwise_norm[order(-h2_pairwise_norm[,1]), ]
  h2_pairwise_norm_table <- as.data.frame(h2_pairwise_norm)
  colnames(h2_pairwise_norm_table) <- "H^2 Normalized"

  h2_pairwise_raw <- round(num, 5)
  h2_pairwise_raw <- h2_pairwise_raw[order(-h2_pairwise_raw[,1]), ]
  h2_pairwise_raw_table <- as.data.frame(h2_pairwise_raw)
  colnames(h2_pairwise_raw_table) <- "H^2 Raw"

  h2_pairwise_norm_table <- h2_pairwise_norm_table %>%
    dplyr::mutate("Pairwise Interaction" = rownames(.)) %>%
    dplyr::relocate("Pairwise Interaction", .before = 1)

  h2_pairwise_raw_table <- h2_pairwise_raw_table %>%
    dplyr::mutate("Pairwise Interaction" = rownames(.)) %>%
    dplyr::relocate("Pairwise Interaction", .before = 1)

  rownames(h2_pairwise_norm_table) <- NULL
  rownames(h2_pairwise_raw_table) <- NULL

  return(list(h2_total = h2_table,
              h2_pairwise_norm = h2_pairwise_norm_table,
              h2_pairwise_raw = h2_pairwise_raw_table))

}


calc_hstats_binary <- function(analysis_object){

  train_data <- analysis_object$data$raw$train_data %>%
              dplyr::select(-dplyr::all_of(analysis_object$dep_var))

  hstats_object <- hstats::hstats(analysis_object$final_model,
                                  X = train_data,
                                  verbose = FALSE,
                                  type = "prob")

  # Total H2

  num   <- hstats_object$h2_overall$num
  denom <- hstats_object$h2_overall$denom
  h2_normalized <- as.data.frame(round(sweep(num, 2, denom, "/"), 5)[,1])
  colnames(h2_normalized) <- "H^2 Normalized"

  h2_table <- h2_normalized %>%
    dplyr::mutate(Feature = rownames(.)) %>%
    dplyr::relocate(Feature, .before = 1) %>%
    dplyr::arrange(dplyr::desc(rowMeans(dplyr::select(., -Feature))))

  rownames(h2_table) <- NULL

  # Pairwise H2

  num   <- hstats_object$h2_pairwise$num
  denom <- hstats_object$h2_pairwise$denom

  h2_pairwise_norm <- round(num / denom, 5)
  h2_pairwise_norm <- h2_pairwise_norm[order(-h2_pairwise_norm[,1]), ]
  h2_pairwise_norm_table <- as.data.frame(h2_pairwise_norm[,1])
  colnames(h2_pairwise_norm_table) <- "H^2 Normalized"

  h2_pairwise_raw <- round(num, 5)
  h2_pairwise_raw <- h2_pairwise_raw[order(-h2_pairwise_raw[,1]), ]
  h2_pairwise_raw_table <- as.data.frame(h2_pairwise_raw[,1])
  colnames(h2_pairwise_raw_table) <- "H^2 Raw"

  h2_pairwise_norm_table <- h2_pairwise_norm_table %>%
    dplyr::mutate("Pairwise Interaction" = rownames(.)) %>%
    dplyr::relocate("Pairwise Interaction", .before = 1) %>%

  h2_pairwise_raw_table <- h2_pairwise_raw_table %>%
    dplyr::mutate("Pairwise Interaction" = rownames(.)) %>%
    dplyr::relocate("Pairwise Interaction", .before = 1) %>%

  rownames(h2_pairwise_norm_table) <- NULL
  rownames(h2_pairwise_raw_table) <- NULL

  return(list(h2_total = h2_table,
              h2_pairwise_norm = h2_pairwise_norm_table,
              h2_pairwise_raw = h2_pairwise_raw_table))

}


calc_hstats_multiclass <- function(analysis_object){

  train_data <- analysis_object$data$raw$train_data %>%
    dplyr::select(-dplyr::all_of(analysis_object$dep_var))

  hstats_object <- hstats::hstats(analysis_object$final_model,
                                  X = train_data,
                                  verbose = FALSE,
                                  type = "prob")

  # Total H2

  num   <- hstats_object$h2_overall$num
  denom <- hstats_object$h2_overall$denom
  h2_normalized <- as.data.frame(round(sweep(num, 2, denom, "/"), 5))
  colnames(h2_normalized) <- gsub("^\\.pred_", "", colnames(h2_normalized))

  h2_table <- h2_normalized %>%
    dplyr::mutate(Feature = rownames(.)) %>%
    dplyr::relocate(Feature, .before = 1) %>%
    dplyr::arrange(dplyr::desc(rowMeans(dplyr::select(., -Feature))))

  rownames(h2_table) <- NULL

  # Pairwise H2

  num   <- hstats_object$h2_pairwise$num
  denom <- hstats_object$h2_pairwise$denom
  h2_pairwise_norm <- as.data.frame(round(num / denom, 5))
  colnames(h2_pairwise_norm) <- gsub("^\\.pred_", "",
                                     colnames(h2_pairwise_norm))

  h2_pairwise_raw <- as.data.frame(round(num, 5))
  colnames(h2_pairwise_raw) <- gsub("^\\.pred_", "",
                                     colnames(h2_pairwise_raw))

  h2_pairwise_norm_table <- h2_pairwise_norm %>%
    dplyr::mutate("Pairwise Interaction" = rownames(.)) %>%
    dplyr::relocate("Pairwise Interaction", .before = 1) %>%
    dplyr::arrange(dplyr::desc(rowMeans(dplyr::select(., -"Pairwise Interaction"))))

  h2_pairwise_raw_table <- h2_pairwise_raw %>%
    dplyr::mutate("Pairwise Interaction" = rownames(.)) %>%
    dplyr::relocate("Pairwise Interaction", .before = 1) %>%
    dplyr::arrange(dplyr::desc(rowMeans(dplyr::select(., -"Pairwise Interaction"))))

  rownames(h2_pairwise_norm_table) <- NULL
  rownames(h2_pairwise_raw_table) <- NULL

  return(list(h2_total = h2_table,
              h2_pairwise_norm = h2_pairwise_norm_table,
              h2_pairwise_raw = h2_pairwise_raw_table))

}

hstat_total_plot <- function(h2_total, outcome_levels){

  if (outcome_levels <= 2){

    p <- ggplot2::ggplot(h2_total, ggplot2::aes(x = .data[["H^2 Normalized"]],
                                      y = stats::reorder(Feature, .data[["H^2 Normalized"]]))) +
      ggplot2::geom_col(orientation = "y", fill = "orange") +
      ggplot2::labs(x = expression(H^2~"Normalized"), y = NULL,
           title = "Friedman's H-statistic")

  } else {

    h2_long <- h2_total %>%
      tidyr::pivot_longer(
        cols = -Feature,
        names_to = "Class",
        values_to = "H2"
      )

    p <- ggplot2::ggplot(h2_long, ggplot2::aes(x = H2,
                                               y = stats::reorder(Feature, H2),
                                               fill = Class)) +
        ggplot2::geom_col(orientation = "y", position = "dodge") +
      ggplot2::labs(x = expression(H^2~"Normalized"), y = NULL,
           title = "Friedman's H-statistic per Class")
  }

  return(p)

}

hstat_pairwise_plot <- function(h2_pairwise, outcome_levels, normalized = TRUE){

    if (normalized){

      h2_col <- "H^2 Normalized"

    } else {

      h2_col <- "H^2 Raw"

    }

    if (outcome_levels <= 2){

      p <- ggplot2::ggplot(h2_pairwise,
                        ggplot2::aes(x = .data[[h2_col]],
                        y = stats::reorder(.data[["Pairwise Interaction"]],
                                    .data[[h2_col]]))) +
        ggplot2::geom_col(orientation = "y", fill = "orange")

    }

    else {

      h2_long <- h2_pairwise %>%
        tidyr::pivot_longer(
          cols = -'Pairwise Interaction',
          names_to = "Class",
          values_to = "H2"
        )

      p <- ggplot2::ggplot(h2_long,
                        ggplot2::aes(x = H2,
                                  y = stats::reorder(.data[["Pairwise Interaction"]], H2),
                                  fill = Class)) +
        ggplot2::geom_col(orientation = "y") +
        ggplot2::facet_wrap("Class")


    }

    if (normalized){

      p <- p + ggplot2::labs(x = expression(H^2~"Normalized"), y = NULL,
                            title = "Feature Interaction")

    } else {

      p <- p + ggplot2::labs(x = expression(H^2~"Unnormalized"), y = NULL,
                             title = "Feature Interaction")

    }

    return(p)

}
