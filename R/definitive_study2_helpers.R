# Internal reporting/orchestration helpers for definitive Study 2.
# Migrated from the validated data-raw definitive runner during Phase 6D-B2A.
# These functions do not change the frozen DGP, estimands, methods, or inference.

study2d_find_project_root <- function(path = getwd()) {
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

study2d_mean_or_na <- function(x) {
  x <- x[is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  mean(x)
}

study2d_min_or_na <- function(x) {
  x <- x[is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  min(x)
}

study2d_max_or_na <- function(x) {
  x <- x[is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  max(x)
}

study2d_quantile_or_na <- function(x,
                           probability) {
  x <- x[is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  unname(stats::quantile(
    x,
    probs = probability,
    names = FALSE,
    type = 7
  ))
}

study2d_has_text <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

study2d_column_or_default <- function(data,
                              name,
                              default) {
  if (name %in% names(data)) {
    return(data[[name]])
  }

  rep(default, nrow(data))
}

study2d_select_existing <- function(data,
                            columns) {
  data[
    ,
    columns[columns %in% names(data)],
    drop = FALSE
  ]
}

study2d_add_condition_columns <- function(data,
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

study2d_rbind_fill <- function(data_list) {
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

study2d_method_labels <- function() {
  c(
    rs = "Random slope (KR)",
    ri = "Random intercept (KR)",
    cr2 = "OLS with CR2",
    cats = "CATs",
    cats_trunc = "Truncated CATs",
    cats_robust = "Robust CATs: lmRob",
    cats_robustbase = "Robust CATs: lmrob",
    robust_ri = "Robust random intercept",
    robust_rs = "Robust random slope"
  )
}

study2d_add_method_labels <- function(data) {
  labels <- study2d_method_labels()
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

study2d_extract_flagged_cluster_diagnostics <- function(replicates,
                                                condition) {
  if (!"cluster_diagnostics" %in% names(replicates)) {
    return(data.frame())
  }

  warning_count <- study2d_column_or_default(
    replicates,
    "cluster_warning_count",
    0
  )
  error_count <- study2d_column_or_default(
    replicates,
    "cluster_error_count",
    0
  )
  dropped_count <- study2d_column_or_default(
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
      random_slope_sd = condition$random_slope_sd,
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

  study2d_rbind_fill(diagnostics)
}

study2d_prepare_replicates_for_storage <- function(replicates,
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

  flagged_diagnostics <- study2d_extract_flagged_cluster_diagnostics(
    replicates = replicates,
    condition = condition
  )

  if ("cluster_diagnostics" %in% names(replicates)) {
    replicates$cluster_diagnostics <- NULL
  }

  replicates <- study2d_add_condition_columns(
    data = replicates,
    condition = condition
  )

  list(
    replicates = replicates,
    flagged_diagnostics = flagged_diagnostics
  )
}

study2d_make_status_snapshot <- function(checkpoint_dir,
                                 design) {
  checkpoint_paths <- list.files(
    checkpoint_dir,
    pattern = "^condition_S2C[0-9]{3}[.]rds$",
    full.names = TRUE
  )

  checkpoint_map <- stats::setNames(
    checkpoint_paths,
    sub(
      "^condition_(S2C[0-9]{3})[.]rds$",
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

study2d_summarize_diagnostics <- function(replicates) {
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

      warning_values <- study2d_column_or_default(
        method_results,
        "warning",
        NA_character_
      )
      optimizer_warning <- study2d_column_or_default(
        method_results,
        "optimizer_warning",
        NA_character_
      )
      optimizer_code <- study2d_column_or_default(
        method_results,
        "optimizer_code",
        NA_real_
      )
      error_values <- study2d_column_or_default(
        method_results,
        "error",
        NA_character_
      )
      converged_values <- study2d_column_or_default(
        method_results,
        "converged",
        NA
      )
      singular_values <- study2d_column_or_default(
        method_results,
        "singular",
        NA
      )
      template_warning <- study2d_column_or_default(
        method_results,
        "template_warning",
        NA_character_
      )
      cluster_warning_count <- study2d_column_or_default(
        method_results,
        "cluster_warning_count",
        NA_real_
      )
      cluster_error_count <- study2d_column_or_default(
        method_results,
        "cluster_error_count",
        NA_real_
      )
      dropped_cluster_count <- study2d_column_or_default(
        method_results,
        "dropped_cluster_count",
        NA_real_
      )
      estimated_ri_sd <- study2d_column_or_default(
        method_results,
        "estimated_random_intercept_sd",
        NA_real_
      )
      estimated_rs_sd <- study2d_column_or_default(
        method_results,
        "estimated_random_slope_sd",
        NA_real_
      )

      observed_converged <- converged_values[
        !is.na(converged_values)
      ]
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
          "random_slope_sd",
          "random_slope_variance",
          "contamination",
          "contamination_label",
          "contamination_prop",
          "contamination_size",
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
          study2d_has_text(warning_values)
        ),
        optimizer_warning_rep_rate = 100 * mean(
          study2d_has_text(optimizer_warning)
        ),
        nonzero_optimizer_code_rate = if (
          all(is.na(optimizer_code))
        ) {
          NA_real_
        } else {
          100 * mean(
            optimizer_code != 0,
            na.rm = TRUE
          )
        },
        error_rep_rate = 100 * mean(
          study2d_has_text(error_values)
        ),
        convergence_failure_rate = if (
          length(observed_converged) == 0L
        ) {
          NA_real_
        } else {
          100 * mean(!observed_converged)
        },
        singular_rate = if (
          length(observed_singular) == 0L
        ) {
          NA_real_
        } else {
          100 * mean(observed_singular)
        },
        template_warning_rate = 100 * mean(
          study2d_has_text(template_warning)
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
        mean_cluster_warning_count = study2d_mean_or_na(
          cluster_warning_count
        ),
        maximum_cluster_warning_count = study2d_max_or_na(
          cluster_warning_count
        ),
        mean_cluster_error_count = study2d_mean_or_na(
          cluster_error_count
        ),
        maximum_cluster_error_count = study2d_max_or_na(
          cluster_error_count
        ),
        mean_dropped_cluster_count = study2d_mean_or_na(
          dropped_cluster_count
        ),
        maximum_dropped_cluster_count = study2d_max_or_na(
          dropped_cluster_count
        ),
        mean_retained_clusters = study2d_mean_or_na(
          method_results$retained_clusters
        ),
        minimum_retained_clusters = study2d_min_or_na(
          method_results$retained_clusters
        ),
        mean_estimated_random_intercept_sd = study2d_mean_or_na(
          estimated_ri_sd
        ),
        mean_estimated_random_slope_sd = study2d_mean_or_na(
          estimated_rs_sd
        ),
        mean_runtime_sec = study2d_mean_or_na(
          method_results$runtime_sec
        ),
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, method_rows)
  })

  out <- do.call(rbind, condition_rows)
  rownames(out) <- NULL
  study2d_add_method_labels(out)
}

study2d_count_text_values <- function(data,
                              column,
                              source) {
  if (!column %in% names(data)) {
    return(data.frame())
  }

  values <- trimws(as.character(data[[column]]))
  values <- values[study2d_has_text(values)]

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

study2d_make_message_frequency <- function(replicates,
                                   flagged_diagnostics) {
  outputs <- list(
    study2d_count_text_values(
      replicates,
      "warning",
      "overall_warning_field"
    ),
    study2d_count_text_values(
      replicates,
      "optimizer_warning",
      "optimizer_warning"
    ),
    study2d_count_text_values(
      replicates,
      "error",
      "overall_error"
    ),
    study2d_count_text_values(
      replicates,
      "template_warning",
      "robust_template_warning"
    )
  )

  if (is.data.frame(flagged_diagnostics) &&
      nrow(flagged_diagnostics) > 0L) {
    warning_columns <- names(flagged_diagnostics)[
      grepl(
        "warning",
        names(flagged_diagnostics),
        ignore.case = TRUE
      )
    ]
    error_columns <- names(flagged_diagnostics)[
      grepl(
        "error",
        names(flagged_diagnostics),
        ignore.case = TRUE
      )
    ]

    for (column in warning_columns) {
      outputs[[length(outputs) + 1L]] <- study2d_count_text_values(
        flagged_diagnostics,
        column,
        paste0("cluster_", column)
      )
    }

    for (column in error_columns) {
      outputs[[length(outputs) + 1L]] <- study2d_count_text_values(
        flagged_diagnostics,
        column,
        paste0("cluster_", column)
      )
    }
  }

  out <- study2d_rbind_fill(outputs)

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

study2d_make_negative_control_comparison <- function(replicates) {
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
      maximum_absolute_estimate_difference = study2d_max_or_na(
        abs(estimate_difference)
      ),
      standard_error_exact_match_rate = 100 * mean(
        x$std_error_cats_trunc == x$std_error_cats,
        na.rm = TRUE
      ),
      maximum_absolute_p_value_difference = study2d_max_or_na(
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
      mean_retained_cluster_difference = study2d_mean_or_na(
        retained_difference
      ),
      minimum_truncated_retained_clusters = study2d_min_or_na(
        x$retained_clusters_cats_trunc
      ),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

study2d_make_method_vs_reference <- function(summary_results,
                                     comparison_methods,
                                     reference_method,
                                     comparison_name) {
  reference <- summary_results[
    summary_results$model == reference_method,
    ,
    drop = FALSE
  ]

  comparison <- summary_results[
    summary_results$model %in% comparison_methods,
    ,
    drop = FALSE
  ]

  reference_columns <- c(
    "condition_id",
    "bias",
    "rmse",
    "rejection_rate",
    "coverage",
    "avg_ci_width",
    "failure_rate",
    "singular_rate"
  )

  reference <- reference[
    ,
    reference_columns[
      reference_columns %in% names(reference)
    ],
    drop = FALSE
  ]

  reference_value_columns <- setdiff(
    names(reference),
    "condition_id"
  )
  names(reference)[
    names(reference) %in% reference_value_columns
  ] <- paste0(
    reference_value_columns,
    "_reference"
  )

  out <- merge(
    comparison,
    reference,
    by = "condition_id",
    all.x = TRUE,
    sort = FALSE
  )

  if (all(c(
    "rmse",
    "rmse_reference"
  ) %in% names(out))) {
    out$rmse_ratio_vs_reference <-
      out$rmse / out$rmse_reference
  }

  if (all(c(
    "avg_ci_width",
    "avg_ci_width_reference"
  ) %in% names(out))) {
    out$ci_width_ratio_vs_reference <-
      out$avg_ci_width /
      out$avg_ci_width_reference
  }

  if (all(c(
    "coverage",
    "coverage_reference"
  ) %in% names(out))) {
    out$coverage_difference_vs_reference <-
      out$coverage - out$coverage_reference
  }

  if (all(c(
    "rejection_rate",
    "rejection_rate_reference"
  ) %in% names(out))) {
    out$rejection_difference_vs_reference <-
      out$rejection_rate -
      out$rejection_rate_reference
  }

  if (all(c(
    "bias",
    "bias_reference"
  ) %in% names(out))) {
    out$absolute_bias_difference_vs_reference <-
      abs(out$bias) - abs(out$bias_reference)
  }

  if (all(c(
    "failure_rate",
    "failure_rate_reference"
  ) %in% names(out))) {
    out$failure_difference_vs_reference <-
      out$failure_rate -
      out$failure_rate_reference
  }

  out$comparison <- comparison_name
  study2d_add_method_labels(out)
}

study2d_summarize_rs_subset <- function(data,
                                subset_label,
                                condition_id,
                                beta,
                                attempted_reps) {
  usable <- data[
    data$fit_success &
      is.finite(data$estimate) &
      is.finite(data$conf_low) &
      is.finite(data$conf_high),
    ,
    drop = FALSE
  ]

  n <- nrow(usable)

  rejection <- if (n > 0L) {
    mean(usable$reject)
  } else {
    NA_real_
  }
  coverage <- if (n > 0L) {
    mean(usable$cover)
  } else {
    NA_real_
  }

  data.frame(
    condition_id = condition_id,
    subset = subset_label,
    attempted_reps = attempted_reps,
    usable_reps = n,
    mean_coef = if (n > 0L) {
      mean(usable$estimate)
    } else {
      NA_real_
    },
    bias = if (n > 0L) {
      mean(usable$estimate) - beta
    } else {
      NA_real_
    },
    rejection_rate = 100 * rejection,
    rejection_rate_se = if (
      n > 0L && is.finite(rejection)
    ) {
      100 * sqrt(
        rejection * (1 - rejection) / n
      )
    } else {
      NA_real_
    },
    rmse = if (n > 0L) {
      sqrt(mean((usable$estimate - beta)^2))
    } else {
      NA_real_
    },
    coverage = 100 * coverage,
    coverage_se = if (
      n > 0L && is.finite(coverage)
    ) {
      100 * sqrt(
        coverage * (1 - coverage) / n
      )
    } else {
      NA_real_
    },
    avg_ci_width = if (n > 0L) {
      mean(usable$conf_high - usable$conf_low)
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )
}

study2d_make_rs_singularity_sensitivity <- function(replicates) {
  rs <- replicates[
    replicates$method == "rs",
    ,
    drop = FALSE
  ]

  condition_ids <- unique(rs$condition_id)

  rows <- lapply(condition_ids, function(condition_id) {
    x <- rs[
      rs$condition_id == condition_id,
      ,
      drop = FALSE
    ]

    all_usable <- study2d_summarize_rs_subset(
      data = x,
      subset_label = "All usable fits",
      condition_id = condition_id,
      beta = x$beta[1L],
      attempted_reps = nrow(x)
    )

    nonsingular_data <- x[
      !is.na(x$singular) & !x$singular,
      ,
      drop = FALSE
    ]

    nonsingular <- study2d_summarize_rs_subset(
      data = nonsingular_data,
      subset_label = "Nonsingular usable fits",
      condition_id = condition_id,
      beta = x$beta[1L],
      attempted_reps = nrow(nonsingular_data)
    )

    condition_columns <- x[
      1L,
      c(
        "condition_id",
        "n_clusters",
        "beta",
        "effect_label",
        "random_slope_sd",
        "contamination",
        "contamination_label"
      ),
      drop = FALSE
    ]

    out <- rbind(all_usable, nonsingular)
    out <- merge(
      condition_columns,
      out,
      by = "condition_id",
      all.y = TRUE,
      sort = FALSE
    )
    out$singular_rate <- 100 * mean(
      x$singular,
      na.rm = TRUE
    )
    out
  })

  long <- do.call(rbind, rows)
  rownames(long) <- NULL

  all_rows <- long[
    long$subset == "All usable fits",
    ,
    drop = FALSE
  ]
  nonsingular_rows <- long[
    long$subset == "Nonsingular usable fits",
    ,
    drop = FALSE
  ]

  value_columns <- c(
    "usable_reps",
    "mean_coef",
    "bias",
    "rejection_rate",
    "rejection_rate_se",
    "rmse",
    "coverage",
    "coverage_se",
    "avg_ci_width"
  )

  all_wide <- all_rows[
    ,
    c(
      "condition_id",
      value_columns,
      "singular_rate"
    ),
    drop = FALSE
  ]
  nonsingular_wide <- nonsingular_rows[
    ,
    c(
      "condition_id",
      value_columns
    ),
    drop = FALSE
  ]

  names(all_wide)[
    names(all_wide) %in% value_columns
  ] <- paste0(value_columns, "_all")
  names(nonsingular_wide)[
    names(nonsingular_wide) %in% value_columns
  ] <- paste0(value_columns, "_nonsingular")

  wide <- merge(
    all_wide,
    nonsingular_wide,
    by = "condition_id",
    all = TRUE,
    sort = FALSE
  )

  condition_columns <- all_rows[
    ,
    c(
      "condition_id",
      "n_clusters",
      "beta",
      "effect_label",
      "random_slope_sd",
      "contamination",
      "contamination_label"
    ),
    drop = FALSE
  ]

  wide <- merge(
    condition_columns,
    wide,
    by = "condition_id",
    all.y = TRUE,
    sort = FALSE
  )

  wide$bias_difference_nonsingular_minus_all <-
    wide$bias_nonsingular - wide$bias_all
  wide$rmse_ratio_nonsingular_vs_all <-
    wide$rmse_nonsingular / wide$rmse_all
  wide$coverage_difference_nonsingular_minus_all <-
    wide$coverage_nonsingular - wide$coverage_all
  wide$rejection_difference_nonsingular_minus_all <-
    wide$rejection_rate_nonsingular -
    wide$rejection_rate_all
  wide$ci_width_ratio_nonsingular_vs_all <-
    wide$avg_ci_width_nonsingular /
    wide$avg_ci_width_all

  list(
    long = long,
    comparison = wide
  )
}

study2d_make_dgp_diagnostics <- function(replicates) {
  one_row <- replicates[
    !duplicated(
      replicates[
        ,
        c("condition_id", "replicate"),
        drop = FALSE
      ]
    ),
    ,
    drop = FALSE
  ]

  condition_ids <- unique(one_row$condition_id)

  rows <- lapply(condition_ids, function(condition_id) {
    x <- one_row[
      one_row$condition_id == condition_id,
      ,
      drop = FALSE
    ]

    data.frame(
      condition_id = condition_id,
      n_clusters = x$n_clusters[1L],
      beta = x$beta[1L],
      random_slope_sd = x$random_slope_sd[1L],
      contamination = x$contamination[1L],
      contamination_label = x$contamination_label[1L],
      reps = nrow(x),
      mean_realized_mean_slope = study2d_mean_or_na(
        x$realized_mean_slope
      ),
      sd_realized_mean_slope = stats::sd(
        x$realized_mean_slope,
        na.rm = TRUE
      ),
      minimum_realized_mean_slope = study2d_min_or_na(
        x$realized_mean_slope
      ),
      maximum_realized_mean_slope = study2d_max_or_na(
        x$realized_mean_slope
      ),
      mean_realized_random_slope_sd = study2d_mean_or_na(
        x$realized_random_slope_sd
      ),
      sd_realized_random_slope_sd = stats::sd(
        x$realized_random_slope_sd,
        na.rm = TRUE
      ),
      minimum_realized_random_slope_sd = study2d_min_or_na(
        x$realized_random_slope_sd
      ),
      maximum_realized_random_slope_sd = study2d_max_or_na(
        x$realized_random_slope_sd
      ),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

study2d_make_crn_audit <- function(replicates,
                           tolerance = 1e-12) {
  one_row <- replicates[
    !duplicated(
      replicates[
        ,
        c("condition_id", "replicate"),
        drop = FALSE
      ]
    ),
    c(
      "condition_id",
      "n_clusters",
      "beta",
      "random_slope_sd",
      "contamination",
      "replicate",
      "replicate_seed",
      "realized_mean_slope",
      "realized_random_slope_sd"
    ),
    drop = FALSE
  ]

  groups <- unique(
    one_row[
      ,
      c("n_clusters", "replicate"),
      drop = FALSE
    ]
  )
  groups <- groups[
    order(groups$n_clusters, groups$replicate),
    ,
    drop = FALSE
  ]

  rows <- lapply(seq_len(nrow(groups)), function(index) {
    n_clusters <- groups$n_clusters[index]
    replicate_id <- groups$replicate[index]

    x <- one_row[
      one_row$n_clusters == n_clusters &
        one_row$replicate == replicate_id,
      ,
      drop = FALSE
    ]

    contamination_differences <- numeric(0)

    for (beta in c(0, 0.10)) {
      for (random_slope_sd in c(0.05, 0.10)) {
        y <- x[
          x$beta == beta &
            x$random_slope_sd == random_slope_sd,
          ,
          drop = FALSE
        ]

        if (nrow(y) == 2L) {
          contamination_differences <- c(
            contamination_differences,
            diff(range(y$realized_mean_slope)),
            diff(range(y$realized_random_slope_sd))
          )
        } else {
          contamination_differences <- c(
            contamination_differences,
            Inf,
            Inf
          )
        }
      }
    }

    beta_shift_differences <- numeric(0)

    for (random_slope_sd in c(0.05, 0.10)) {
      for (contamination in c("none", "vertical")) {
        y <- x[
          x$random_slope_sd == random_slope_sd &
            x$contamination == contamination,
          ,
          drop = FALSE
        ]
        y <- y[order(y$beta), , drop = FALSE]

        if (nrow(y) == 2L) {
          beta_shift_differences <- c(
            beta_shift_differences,
            abs(
              (
                y$realized_mean_slope[2L] -
                  y$realized_mean_slope[1L]
              ) - 0.10
            )
          )
        } else {
          beta_shift_differences <- c(
            beta_shift_differences,
            Inf
          )
        }
      }
    }

    mean_rescaling_differences <- numeric(0)
    sd_rescaling_differences <- numeric(0)

    for (beta in c(0, 0.10)) {
      for (contamination in c("none", "vertical")) {
        y <- x[
          x$beta == beta &
            x$contamination == contamination,
          ,
          drop = FALSE
        ]
        y <- y[order(y$random_slope_sd), , drop = FALSE]

        if (nrow(y) == 2L) {
          mean_rescaling_differences <- c(
            mean_rescaling_differences,
            abs(
              (
                y$realized_mean_slope[2L] - beta
              ) -
                2 * (
                  y$realized_mean_slope[1L] - beta
                )
            )
          )
          sd_rescaling_differences <- c(
            sd_rescaling_differences,
            abs(
              y$realized_random_slope_sd[2L] -
                2 * y$realized_random_slope_sd[1L]
            )
          )
        } else {
          mean_rescaling_differences <- c(
            mean_rescaling_differences,
            Inf
          )
          sd_rescaling_differences <- c(
            sd_rescaling_differences,
            Inf
          )
        }
      }
    }

    unique_seed_count <- length(unique(
      x$replicate_seed
    ))
    max_contamination_difference <- max(
      contamination_differences
    )
    max_beta_shift_difference <- max(
      beta_shift_differences
    )
    max_mean_rescaling_difference <- max(
      mean_rescaling_differences
    )
    max_sd_rescaling_difference <- max(
      sd_rescaling_differences
    )

    data.frame(
      n_clusters = n_clusters,
      replicate = replicate_id,
      condition_count = nrow(x),
      unique_seed_count = unique_seed_count,
      max_contamination_difference =
        max_contamination_difference,
      max_beta_shift_difference =
        max_beta_shift_difference,
      max_slope_mean_rescaling_difference =
        max_mean_rescaling_difference,
      max_slope_sd_rescaling_difference =
        max_sd_rescaling_difference,
      passed = nrow(x) == 8L &&
        unique_seed_count == 1L &&
        max_contamination_difference <= tolerance &&
        max_beta_shift_difference <= tolerance &&
        max_mean_rescaling_difference <= tolerance &&
        max_sd_rescaling_difference <= tolerance,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

study2d_make_primary_performance_table <- function(summary_results) {
  columns <- c(
    "condition_id",
    "n_clusters",
    "cluster_size",
    "beta",
    "effect_label",
    "random_slope_sd",
    "random_slope_variance",
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

  out <- study2d_select_existing(
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
      out$random_slope_sd,
      match(
        out$contamination,
        c("none", "vertical")
      ),
      out$method_order
    ),
    ,
    drop = FALSE
  ]

  rownames(out) <- NULL
  out
}

study2d_make_mcse_summary <- function(summary_results) {
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
        minimum_mcse = study2d_min_or_na(x$mcse),
        mean_mcse = study2d_mean_or_na(x$mcse),
        maximum_mcse = study2d_max_or_na(x$mcse),
        stringsAsFactors = FALSE
      )
    }
  )

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
