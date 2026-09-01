# Internal reporting/orchestration helpers for definitive Study 1.
# Migrated from the validated data-raw definitive runner during Phase 6D-B2A.
# These functions do not change the frozen DGP, estimands, methods, or inference.

study1d_find_project_root <- function(path = getwd()) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  repeat {
    if (file.exists(file.path(path, "DESCRIPTION"))) {
      return(path)
    }

    parent <- dirname(path)

    if (identical(parent, path)) {
      stop(
        "Could not locate the mmiCATs project root.",
        call. = FALSE
      )
    }

    path <- parent
  }
}

study1d_mean_or_na <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  mean(x)
}

study1d_max_or_na <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  max(x)
}

study1d_min_or_na <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  min(x)
}

study1d_sum_or_zero <- function(x) {
  if (length(x) == 0L || all(is.na(x))) {
    return(0)
  }

  sum(x, na.rm = TRUE)
}

study1d_has_text <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

study1d_column_or_default <- function(data,
                              name,
                              default) {
  if (name %in% names(data)) {
    return(data[[name]])
  }

  rep(default, nrow(data))
}

study1d_select_existing <- function(data,
                            columns) {
  data[
    ,
    columns[columns %in% names(data)],
    drop = FALSE
  ]
}

study1d_add_condition_columns <- function(data,
                                  condition) {
  if (is.null(data) || nrow(data) == 0L) {
    return(data)
  }

  condition_rows <- condition[
    rep(1L, nrow(data)),
    ,
    drop = FALSE
  ]

  out <- cbind(condition_rows, data)
  rownames(out) <- NULL
  out
}

study1d_rbind_fill <- function(data_list) {
  data_list <- data_list[
    vapply(
      data_list,
      function(x) {
        is.data.frame(x) && nrow(x) > 0L
      },
      logical(1)
    )
  ]

  if (length(data_list) == 0L) {
    return(data.frame())
  }

  all_names <- unique(unlist(
    lapply(data_list, names),
    use.names = FALSE
  ))

  aligned <- lapply(data_list, function(data) {
    missing_names <- setdiff(all_names, names(data))

    for (name in missing_names) {
      data[[name]] <- NA
    }

    data[
      ,
      all_names,
      drop = FALSE
    ]
  })

  out <- do.call(rbind, aligned)
  rownames(out) <- NULL
  out
}

study1d_method_labels <- function() {
  c(
    ri = "Random intercept (KR)",
    cr2 = "OLS with CR2",
    cats = "CATs",
    cats_trunc = "Truncated CATs",
    cats_robust = "Robust CATs: lmRob",
    cats_robustbase = "Robust CATs: lmrob",
    robust_ri = "Robust random intercept"
  )
}

study1d_add_method_labels <- function(data) {
  labels <- study1d_method_labels()
  method_column <- if ("model" %in% names(data)) {
    "model"
  } else {
    "method"
  }

  data$method_label <- unname(
    labels[as.character(data[[method_column]])]
  )
  data$method_order <- match(
    as.character(data[[method_column]]),
    names(labels)
  )

  data
}

study1d_extract_flagged_cluster_diagnostics <- function(replicates,
                                                condition) {
  if (!"cluster_diagnostics" %in% names(replicates)) {
    return(data.frame())
  }

  warning_count <- study1d_column_or_default(
    replicates,
    "cluster_warning_count",
    0
  )
  error_count <- study1d_column_or_default(
    replicates,
    "cluster_error_count",
    0
  )
  dropped_count <- study1d_column_or_default(
    replicates,
    "dropped_cluster_count",
    0
  )

  flagged <- which(
    (!is.na(warning_count) & warning_count > 0) |
      (!is.na(error_count) & error_count > 0) |
      (!is.na(dropped_count) & dropped_count > 0)
  )

  if (length(flagged) == 0L) {
    return(data.frame())
  }

  diagnostics <- lapply(flagged, function(row_index) {
    cluster_data <- replicates$cluster_diagnostics[[row_index]]

    if (!is.data.frame(cluster_data) ||
        nrow(cluster_data) == 0L) {
      return(NULL)
    }

    prefix <- data.frame(
      condition_id = condition$condition_id,
      n_clusters = condition$n_clusters,
      beta = condition$beta,
      contamination = condition$contamination,
      contamination_label = condition$contamination_label,
      replicate = replicates$replicate[row_index],
      replicate_seed = replicates$replicate_seed[row_index],
      method = replicates$method[row_index],
      stringsAsFactors = FALSE
    )

    prefix <- prefix[
      rep(1L, nrow(cluster_data)),
      ,
      drop = FALSE
    ]

    out <- cbind(prefix, cluster_data)
    rownames(out) <- NULL
    out
  })

  study1d_rbind_fill(diagnostics)
}

study1d_prepare_replicates_for_storage <- function(replicates,
                                           settings,
                                           condition) {
  if (!"replicate" %in% names(replicates)) {
    stop(
      "Replicate-level results do not contain a 'replicate' column.",
      call. = FALSE
    )
  }

  replicate_ids <- as.integer(replicates$replicate)

  if (any(
    is.na(replicate_ids) |
      replicate_ids < 1L |
      replicate_ids > length(settings$replicate_seeds)
  )) {
    stop(
      "Could not map replicate identifiers to replicate seeds.",
      call. = FALSE
    )
  }

  replicates$replicate_seed <-
    settings$replicate_seeds[replicate_ids]

  flagged_diagnostics <- study1d_extract_flagged_cluster_diagnostics(
    replicates = replicates,
    condition = condition
  )

  if ("cluster_diagnostics" %in% names(replicates)) {
    replicates$cluster_diagnostics <- NULL
  }

  replicates <- study1d_add_condition_columns(
    data = replicates,
    condition = condition
  )

  list(
    replicates = replicates,
    flagged_diagnostics = flagged_diagnostics
  )
}

study1d_make_status_snapshot <- function(checkpoint_dir,
                                 design) {
  checkpoint_paths <- list.files(
    checkpoint_dir,
    pattern = "^condition_S1C[0-9]{3}[.]rds$",
    full.names = TRUE
  )

  checkpoint_map <- stats::setNames(
    checkpoint_paths,
    sub(
      "^condition_(S1C[0-9]{3})[.]rds$",
      "\\1",
      basename(checkpoint_paths)
    )
  )

  rows <- lapply(
    seq_len(nrow(design)),
    function(index) {
      condition <- design[index, , drop = FALSE]
      checkpoint_path <- checkpoint_map[
        condition$condition_id
      ]

      if (length(checkpoint_path) == 0L ||
          is.na(checkpoint_path) ||
          !file.exists(checkpoint_path)) {
        return(data.frame(
          condition,
          status = "not_started",
          condition_error = NA_character_,
          started_at = NA_character_,
          completed_at = NA_character_,
          elapsed_sec = NA_real_,
          stringsAsFactors = FALSE
        ))
      }

      checkpoint <- tryCatch(
        readRDS(checkpoint_path),
        error = function(e) e
      )

      if (inherits(checkpoint, "error")) {
        return(data.frame(
          condition,
          status = "unreadable",
          condition_error = conditionMessage(checkpoint),
          started_at = NA_character_,
          completed_at = NA_character_,
          elapsed_sec = NA_real_,
          stringsAsFactors = FALSE
        ))
      }

      data.frame(
        condition,
        status = checkpoint$status,
        condition_error = checkpoint$error,
        started_at = as.character(checkpoint$started_at),
        completed_at = as.character(checkpoint$completed_at),
        elapsed_sec = checkpoint$elapsed_sec,
        stringsAsFactors = FALSE
      )
    }
  )

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

study1d_summarize_diagnostics <- function(replicates) {
  condition_ids <- unique(replicates$condition_id)

  condition_rows <- lapply(condition_ids, function(condition_id) {
    condition_results <- replicates[
      replicates$condition_id == condition_id,
      ,
      drop = FALSE
    ]

    methods <- unique(condition_results$method)

    method_rows <- lapply(methods, function(method) {
      method_results <- condition_results[
        condition_results$method == method,
        ,
        drop = FALSE
      ]

      warning_values <- study1d_column_or_default(
        method_results,
        "warning",
        NA_character_
      )
      error_values <- study1d_column_or_default(
        method_results,
        "error",
        NA_character_
      )
      singular_values <- study1d_column_or_default(
        method_results,
        "singular",
        NA
      )
      template_warning <- study1d_column_or_default(
        method_results,
        "template_warning",
        NA_character_
      )
      cluster_warning_count <- study1d_column_or_default(
        method_results,
        "cluster_warning_count",
        NA_real_
      )
      cluster_error_count <- study1d_column_or_default(
        method_results,
        "cluster_error_count",
        NA_real_
      )
      dropped_cluster_count <- study1d_column_or_default(
        method_results,
        "dropped_cluster_count",
        NA_real_
      )

      observed_singular <- singular_values[
        !is.na(singular_values)
      ]

      condition_columns <- condition_results[
        1L,
        c(
          "condition_id",
          "n_clusters",
          "cluster_size",
          "beta",
          "effect_label",
          "contamination",
          "contamination_label",
          "contamination_prop",
          "contamination_size",
          "leverage_size",
          "reps",
          "condition_seed"
        ),
        drop = FALSE
      ]

      data.frame(
        condition_columns,
        model = method,
        requested_reps = method_results$reps[1L],
        observed_reps = nrow(method_results),
        successful_reps = sum(method_results$fit_success),
        fit_success_rate = 100 * mean(
          method_results$fit_success
        ),
        failure_rate = 100 * mean(
          !method_results$fit_success
        ),
        overall_warning_field_rate = 100 * mean(
          study1d_has_text(warning_values)
        ),
        error_rep_rate = 100 * mean(
          study1d_has_text(error_values)
        ),
        singular_rate = if (
          length(observed_singular) == 0L
        ) {
          NA_real_
        } else {
          100 * mean(observed_singular)
        },
        template_warning_rate = 100 * mean(
          study1d_has_text(template_warning)
        ),
        cluster_warning_rep_rate = if (
          all(is.na(cluster_warning_count))
        ) {
          NA_real_
        } else {
          100 * mean(
            cluster_warning_count > 0,
            na.rm = TRUE
          )
        },
        cluster_error_rep_rate = if (
          all(is.na(cluster_error_count))
        ) {
          NA_real_
        } else {
          100 * mean(
            cluster_error_count > 0,
            na.rm = TRUE
          )
        },
        dropped_cluster_rep_rate = if (
          all(is.na(dropped_cluster_count))
        ) {
          NA_real_
        } else {
          100 * mean(
            dropped_cluster_count > 0,
            na.rm = TRUE
          )
        },
        mean_cluster_warning_count = study1d_mean_or_na(
          cluster_warning_count
        ),
        maximum_cluster_warning_count = study1d_max_or_na(
          cluster_warning_count
        ),
        mean_cluster_error_count = study1d_mean_or_na(
          cluster_error_count
        ),
        maximum_cluster_error_count = study1d_max_or_na(
          cluster_error_count
        ),
        mean_dropped_cluster_count = study1d_mean_or_na(
          dropped_cluster_count
        ),
        maximum_dropped_cluster_count = study1d_max_or_na(
          dropped_cluster_count
        ),
        mean_retained_clusters = study1d_mean_or_na(
          method_results$retained_clusters
        ),
        minimum_retained_clusters = study1d_min_or_na(
          method_results$retained_clusters
        ),
        mean_runtime_sec = study1d_mean_or_na(
          method_results$runtime_sec
        ),
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, method_rows)
  })

  out <- do.call(rbind, condition_rows)
  rownames(out) <- NULL
  study1d_add_method_labels(out)
}

study1d_count_text_values <- function(data,
                              column,
                              source) {
  if (!column %in% names(data)) {
    return(data.frame())
  }

  values <- trimws(as.character(data[[column]]))
  values <- values[study1d_has_text(values)]

  if (length(values) == 0L) {
    return(data.frame())
  }

  counts <- sort(table(values), decreasing = TRUE)

  data.frame(
    source = source,
    message = names(counts),
    count = as.integer(counts),
    stringsAsFactors = FALSE
  )
}

study1d_make_message_frequency <- function(replicates,
                                   flagged_diagnostics) {
  outputs <- list(
    study1d_count_text_values(
      replicates,
      "warning",
      "overall_warning_field"
    ),
    study1d_count_text_values(
      replicates,
      "error",
      "overall_error"
    ),
    study1d_count_text_values(
      replicates,
      "template_warning",
      "robust_template_warning"
    )
  )

  if (is.data.frame(flagged_diagnostics) &&
      nrow(flagged_diagnostics) > 0L) {
    warning_columns <- names(flagged_diagnostics)[
      grepl("warning", names(flagged_diagnostics),
            ignore.case = TRUE)
    ]
    error_columns <- names(flagged_diagnostics)[
      grepl("error", names(flagged_diagnostics),
            ignore.case = TRUE)
    ]

    for (column in warning_columns) {
      outputs[[length(outputs) + 1L]] <- study1d_count_text_values(
        flagged_diagnostics,
        column,
        paste0("cluster_", column)
      )
    }

    for (column in error_columns) {
      outputs[[length(outputs) + 1L]] <- study1d_count_text_values(
        flagged_diagnostics,
        column,
        paste0("cluster_", column)
      )
    }
  }

  out <- study1d_rbind_fill(outputs)

  if (nrow(out) == 0L) {
    return(data.frame(
      source = character(),
      message = character(),
      count = integer(),
      stringsAsFactors = FALSE
    ))
  }

  out <- out[
    order(out$source, -out$count, out$message),
    ,
    drop = FALSE
  ]
  rownames(out) <- NULL
  out
}

study1d_make_negative_control_comparison <- function(replicates) {
  keys <- c(
    "condition_id",
    "replicate"
  )
  values <- c(
    "estimate",
    "std_error",
    "df",
    "p_value",
    "conf_low",
    "conf_high",
    "reject",
    "cover",
    "fit_success",
    "retained_clusters"
  )

  cats <- replicates[
    replicates$method == "cats",
    c(keys, values),
    drop = FALSE
  ]

  cats_trunc <- replicates[
    replicates$method == "cats_trunc",
    c(keys, values),
    drop = FALSE
  ]

  names(cats)[names(cats) %in% values] <- paste0(
    values,
    "_cats"
  )
  names(cats_trunc)[
    names(cats_trunc) %in% values
  ] <- paste0(
    values,
    "_cats_trunc"
  )

  merged <- merge(
    cats,
    cats_trunc,
    by = keys,
    all = TRUE,
    sort = FALSE
  )

  condition_ids <- unique(merged$condition_id)

  rows <- lapply(condition_ids, function(condition_id) {
    x <- merged[
      merged$condition_id == condition_id,
      ,
      drop = FALSE
    ]

    estimate_difference <- x$estimate_cats_trunc -
      x$estimate_cats
    p_value_difference <- x$p_value_cats_trunc -
      x$p_value_cats
    retained_difference <-
      x$retained_clusters_cats_trunc -
      x$retained_clusters_cats

    data.frame(
      condition_id = condition_id,
      reps_compared = nrow(x),
      estimate_exact_match_rate = 100 * mean(
        x$estimate_cats_trunc == x$estimate_cats,
        na.rm = TRUE
      ),
      maximum_absolute_estimate_difference = study1d_max_or_na(
        abs(estimate_difference)
      ),
      standard_error_exact_match_rate = 100 * mean(
        x$std_error_cats_trunc == x$std_error_cats,
        na.rm = TRUE
      ),
      maximum_absolute_p_value_difference = study1d_max_or_na(
        abs(p_value_difference)
      ),
      rejection_exact_match_rate = 100 * mean(
        x$reject_cats_trunc == x$reject_cats,
        na.rm = TRUE
      ),
      coverage_exact_match_rate = 100 * mean(
        x$cover_cats_trunc == x$cover_cats,
        na.rm = TRUE
      ),
      fit_success_exact_match_rate = 100 * mean(
        x$fit_success_cats_trunc ==
          x$fit_success_cats,
        na.rm = TRUE
      ),
      mean_retained_cluster_difference = study1d_mean_or_na(
        retained_difference
      ),
      minimum_truncated_retained_clusters = study1d_min_or_na(
        x$retained_clusters_cats_trunc
      ),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

study1d_make_robust_vs_cats <- function(summary_results) {
  robust_methods <- c(
    "cats_robust",
    "cats_robustbase"
  )

  cats <- summary_results[
    summary_results$model == "cats",
    ,
    drop = FALSE
  ]

  robust <- summary_results[
    summary_results$model %in% robust_methods,
    ,
    drop = FALSE
  ]

  cats_columns <- c(
    "condition_id",
    "bias",
    "rmse",
    "rejection_rate",
    "coverage",
    "avg_ci_width",
    "failure_rate"
  )

  cats <- cats[
    ,
    cats_columns[cats_columns %in% names(cats)],
    drop = FALSE
  ]

  cats_value_columns <- setdiff(
    names(cats),
    "condition_id"
  )
  names(cats)[
    names(cats) %in% cats_value_columns
  ] <- paste0(cats_value_columns, "_cats")

  out <- merge(
    robust,
    cats,
    by = "condition_id",
    all.x = TRUE,
    sort = FALSE
  )

  if (all(c("rmse", "rmse_cats") %in% names(out))) {
    out$rmse_ratio_vs_cats <- out$rmse / out$rmse_cats
  }

  if (all(c(
    "avg_ci_width",
    "avg_ci_width_cats"
  ) %in% names(out))) {
    out$ci_width_ratio_vs_cats <-
      out$avg_ci_width / out$avg_ci_width_cats
  }

  if (all(c(
    "coverage",
    "coverage_cats"
  ) %in% names(out))) {
    out$coverage_difference_vs_cats <-
      out$coverage - out$coverage_cats
  }

  if (all(c(
    "rejection_rate",
    "rejection_rate_cats"
  ) %in% names(out))) {
    out$rejection_difference_vs_cats <-
      out$rejection_rate - out$rejection_rate_cats
  }

  if (all(c("bias", "bias_cats") %in% names(out))) {
    out$absolute_bias_difference_vs_cats <-
      abs(out$bias) - abs(out$bias_cats)
  }

  study1d_add_method_labels(out)
}

study1d_make_primary_performance_table <- function(summary_results) {
  columns <- c(
    "condition_id",
    "n_clusters",
    "cluster_size",
    "beta",
    "effect_label",
    "contamination",
    "contamination_label",
    "model",
    "method_label",
    "method_order",
    "mean_coef",
    "bias",
    "rejection_rate",
    "rejection_rate_se",
    "rmse",
    "coverage",
    "coverage_se",
    "avg_ci_width",
    "success",
    "failure_rate",
    "singular_rate",
    "mean_retained_clusters",
    "mean_runtime_sec"
  )

  out <- study1d_select_existing(
    summary_results,
    columns
  )

  out$rejection_metric <- ifelse(
    out$beta == 0,
    "Type I error",
    "Power"
  )

  out <- out[
    order(
      out$n_clusters,
      out$beta,
      match(
        out$contamination,
        c("none", "vertical", "bad_leverage")
      ),
      out$method_order
    ),
    ,
    drop = FALSE
  ]

  rownames(out) <- NULL
  out
}

study1d_make_mcse_summary <- function(summary_results) {
  rejection_mcse <- summary_results[
    ,
    c(
      "condition_id",
      "model",
      "rejection_rate_se"
    ),
    drop = FALSE
  ]
  names(rejection_mcse)[3L] <- "mcse"
  rejection_mcse$metric <- ifelse(
    summary_results$beta == 0,
    "Type I error",
    "Power"
  )

  coverage_mcse <- summary_results[
    ,
    c(
      "condition_id",
      "model",
      "coverage_se"
    ),
    drop = FALSE
  ]
  names(coverage_mcse)[3L] <- "mcse"
  coverage_mcse$metric <- "Coverage"

  long <- rbind(
    rejection_mcse,
    coverage_mcse
  )

  split_metrics <- split(long, long$metric)

  rows <- lapply(
    split_metrics,
    function(x) {
      data.frame(
        metric = x$metric[1L],
        minimum_mcse = study1d_min_or_na(x$mcse),
        mean_mcse = study1d_mean_or_na(x$mcse),
        maximum_mcse = study1d_max_or_na(x$mcse),
        stringsAsFactors = FALSE
      )
    }
  )

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
