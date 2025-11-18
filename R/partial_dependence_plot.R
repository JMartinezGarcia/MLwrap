#' Plotting Partial Dependence Plot
#'
#' @description
#'
#' The **plot_pfi()** function generates feature importance estimates via
#' Permutation Feature Importance measuring performance degradation when each
#' feature's values are randomly permuted while holding all other features
#' constant. Provides model-agnostic importance ranking independent of
#' feature-target correlation patterns, capturing both linear and non-linear
#' predictive contributions to model performance.
#'
#' @param analysis_object Fitted analysis_object with
#' 'sensitivity_analysis(methods = "PFI")'.
#' @param show_table Boolean. Whether to print PFI results table.
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the PFI plot results the user needs to complete till
#' # sensitivity_analysis( ) function of the MLwrap pipeline using the PFI
#' # method.
#' # See the full pipeline example under sensitivity_analysis()
#' # (Requires sensitivity_analysis(methods = "PFI"))
#' # Final call signature:
#' # plot_pfi(wrap_object)
#' @seealso \code{\link{sensitivity_analysis}}
#' @export

plot_partial_dependence_plot <- function(analysis_object, feature,
                         group_by = NULL,
                         grid_size = 25,
                         show_ice = TRUE, ice_n = 50,
                         pdp_line_size = 1.1,
                         plot = TRUE){

  model <- analysis_object$final_model
  data <- analysis_object$data$raw$test_data
  task <- analysis_object$task
  outcome_levels <- analysis_object$outcome_levels

  # 1) Full ICE (no sampling) using your ice_data() with trimmed grid
  ice_full <- ice_data(
    model = model,
    data = data,
    task = task,
    outcome_levels = outcome_levels,
    feature   = feature,
    grid_size = grid_size,
    group_by  = group_by
  )

  # 2) PDP from full ICE
  if ("pred_class" %in% names(ice_full)) {
    # Multiclass: group by class
    if (is.null(group_by)) {
      pdp_df <- ice_full %>%
        dplyr::group_by(pred_class, feature_value) %>%
        dplyr::summarise(prediction = mean(prediction, na.rm = TRUE), .groups = "drop")
    } else {
      pdp_df <- ice_full %>%
        dplyr::group_by(pred_class, .data[[group_by]], feature_value) %>%
        dplyr::summarise(prediction = mean(prediction, na.rm = TRUE), .groups = "drop")
    }
  } else {
    # Regression or binary classification
    if (is.null(group_by)) {
      pdp_df <- ice_full %>%
        dplyr::group_by(feature_value) %>%
        dplyr::summarise(prediction = mean(prediction, na.rm = TRUE), .groups = "drop")
    } else {
      pdp_df <- ice_full %>%
        dplyr::group_by(.data[[group_by]], feature_value) %>%
        dplyr::summarise(prediction = mean(prediction, na.rm = TRUE), .groups = "drop")
    }
  }

  # 3) If plotting ICE, sample IDs from the full ICE (same dataframe)
  if (isTRUE(show_ice)) {
    sampled_ids <- sample(unique(ice_full$id), size = min(ice_n, length(unique(ice_full$id))))
    ice_plot <- ice_full[ice_full$id %in% sampled_ids, , drop = FALSE]
  }

  # Multiclass PDP + ICE facet version
  if ("pred_class" %in% names(ice_full)) {
    p <- ggplot2::ggplot()

    if (isTRUE(show_ice)) {
      p <- p +
        ggplot2::geom_line(data = ice_plot,
                  ggplot2::aes(x = feature_value, y = prediction,
                      group = interaction(id, pred_class),
                      color = if (!is.null(group_by)) .data[[group_by]] else "dodgerblue2"),
                  alpha = 0.3, linewidth = 0.4)
    }

    p <- p +
      ggplot2::geom_line(data = pdp_df,
                ggplot2::aes(x = feature_value, y = prediction,
                    color = if (!is.null(group_by)) .data[[group_by]] else "dodgerblue2"),
                linewidth = pdp_line_size) +
      ggplot2::facet_wrap(~ pred_class, scales = "free_y") +
      ggplot2::scale_color_viridis_d(option = "plasma", begin = 0.1, end = 0.9) +
      ggplot2::labs(
        title = if (is.null(group_by)) glue::glue("PD Plot {feature}") else glue::glue("PD Plot {feature} by {group_by}"),
        x = feature, y = "Predicted probability",
        color = if (!is.null(group_by)) group_by else NULL
      ) +
      ggplot2::theme_gray()

    if (is.null(group_by)) {
      p <- p + ggplot2::guides(color = "none")
    }

  } else {
    # Regression or binary
    p <- ggplot2::ggplot()

    if (isTRUE(show_ice)) {
      p <- p +
        ggplot2::geom_line(data = ice_plot,
                  ggplot2::aes(x = feature_value, y = prediction, group = id,
                      color = if (!is.null(group_by)) .data[[group_by]] else NULL),
                  alpha = 0.3, linewidth = 0.4
                  )
    }

    p <- p +
      ggplot2::geom_line(data = pdp_df,
                ggplot2::aes(x = feature_value, y = prediction,
                    color = if (!is.null(group_by)) .data[[group_by]] else NULL),
                linewidth = pdp_line_size) +
      ggplot2::scale_color_viridis_d(option = "plasma", begin = 0.1, end = 0.9) +
      ggplot2::labs(
        title = if (is.null(group_by)) glue::glue("PD Plot {feature}") else glue::glue("PD Plot {feature} by {group_by}"),
        x = feature, y = "Prediction",
        color = group_by
      ) +
      ggplot2::theme_gray()
  }

  if (plot){

        plot(p)

        invisible(analysis_object)

  } else{

    return(p)

  }
}

ice_data <- function(model, data, task, feature,
                     outcome_levels = NULL, group_by = NULL, grid_size = 25) {

  # 1) Build trimmed numeric grid (1%–99%)
  x <- data[[feature]]
  q <- stats::quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  grid <- seq(q[1], q[2], length.out = grid_size)

  # 2) Prepare grouping vector (with numeric -> quartiles)
  group_vec <- NULL
  if (!is.null(group_by)) {
    stopifnot(group_by %in% names(data))
    g <- data[[group_by]]
    if (is.numeric(g)) {
      r <- rank(g, na.last = "keep", ties.method = "average") / sum(!is.na(g))
      group_vec <- cut(r, breaks = c(0, .25, .50, .75, 1), include.lowest = TRUE,
                       labels = paste0("Q", 1:4))
    } else {
      group_vec <- g
    }
  }

  # 3) Loop over grid, predict, and build rows
  ice_list <- lapply(grid, function(val) {
    xtemp <- data
    xtemp[[feature]] <- val

    if (identical(task, "regression")) {
      preds <- predict(model, new_data = xtemp)[[1]]

      df <- data.frame(
        id = seq_len(nrow(data)),
        feature_value = val,
        prediction = preds
      )

    } else if (!is.null(outcome_levels) && outcome_levels == 2) {
      # Binary classification: take prob of 2nd level as in your setup
      preds <- predict(model, new_data = xtemp, type = "prob")[[2]]

      df <- data.frame(
        id = seq_len(nrow(data)),
        feature_value = val,
        prediction = preds
      )

    } else {
      # Multiclass: get prob tibble and pivot longer
      prob_tbl <- predict(model, new_data = xtemp, type = "prob")
      # ensure it's a data.frame
      prob_df <- as.data.frame(prob_tbl)

      # names typically like ".pred_classA", ".pred_classB", ...
      cls_names <- sub("^\\.pred_", "", colnames(prob_df))

      # build long data (base-R, no tidyr required)
      id_vec <- rep(seq_len(nrow(prob_df)), times = ncol(prob_df))
      class_vec <- rep(cls_names, each = nrow(prob_df))
      pred_vec <- as.numeric(as.matrix(prob_df))

      df <- data.frame(
        id = id_vec,
        feature_value = val,
        pred_class = class_vec,
        prediction = pred_vec
      )
    }

    # attach grouping if requested
    if (!is.null(group_by)) {
      df[[group_by]] <- rep(group_vec, each = if ("class" %in% names(df)) 1 else 1)
    }

    df
  })

  ice_df <- do.call(rbind, ice_list)
  rownames(ice_df) <- NULL
  ice_df
}
