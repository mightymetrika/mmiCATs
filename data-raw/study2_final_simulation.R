# Study 2 definitive manuscript-version simulation
#
# This script runs the frozen Study 2 design comparing:
#
#   1. a correctly specified independent random-intercept and random-slope
#      model with Kenward-Roger inference;
#   2. the Study 1 random-intercept model with Kenward-Roger inference;
#   3. ordinary least squares with CR2 and Satterthwaite inference;
#   4. ordinary cluster-adjusted t statistics (CATs);
#   5. truncated CATs, retained as a negative control;
#   6. robust CATs using robust::lmRob();
#   7. robust CATs using robustbase::lmrob();
#   8. a robust random-intercept mixed model; and
#   9. a correctly specified robust independent random-intercept and
#      random-slope mixed model, both using robust Satterthwaite inference.
#
# Frozen design:
#
#   - clusters: 10, 20, and 40;
#   - observations per cluster: 40;
#   - population mean slope: 0 and 0.10;
#   - random-slope SD: 0.05 and 0.10;
#   - random-intercept SD: 1;
#   - residual SD: 1;
#   - predictor SD: 1;
#   - intercept-slope covariance: 0;
#   - contamination regimes:
#       a. none; and
#       b. vertical outcome contamination of 6 residual SDs;
#   - contamination proportion: 0.05 within every cluster;
#   - 2,000 attempted replications per condition.
#
# The primary estimand is the superpopulation mean slope beta. The realized
# mean cluster slope is retained as a diagnostic but is not used as the truth.
#
# Conditions with the same number of clusters use common random numbers across
# beta, random-slope SD, and contamination conditions. The definitive-study seeds
# are distinct from the numerical-validation, plumbing-pilot, and
# random-slope convergence-diagnostic seeds.
#
# A finite singular random-slope fit remains usable when there is no genuine
# optimizer or gradient convergence failure. Singularity and convergence are
# reported separately. The standard boundary-singularity console messages are
# suppressed during the long run because their information is preserved in
# the replicate-level diagnostics.
#
# Each 2,000-replication condition is executed as deterministic small shards.
# Completed shards are skipped on restart, and a completed condition checkpoint
# is reconstructed only after every frozen shard is present and valid. No new
# shard begins when free disk space is below the frozen safety threshold.
# Rerunning the script also rebuilds combined outputs from completed conditions.
#
# Run this script from the mmiCATs project. The project root is located
# automatically, so the script can also be sourced while the working directory
# is data-raw.

find_project_root <- function(path = getwd()) {
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

save_rds_atomic <- function(object,
                            path,
                            compress = "gzip") {
  temp_path <- tempfile(
    pattern = "study2_",
    tmpdir = dirname(path),
    fileext = ".rds"
  )

  saveRDS(
    object,
    temp_path,
    version = 3,
    compress = compress
  )

  if (file.exists(path) && !file.remove(path)) {
    stop(
      paste("Could not replace existing file:", path),
      call. = FALSE
    )
  }

  if (!file.rename(temp_path, path)) {
    stop(
      paste("Could not save file:", path),
      call. = FALSE
    )
  }

  invisible(path)
}

write_csv_atomic <- function(object,
                             path) {
  temp_path <- tempfile(
    pattern = "study2_",
    tmpdir = dirname(path),
    fileext = ".csv"
  )

  utils::write.csv(
    object,
    temp_path,
    row.names = FALSE,
    na = ""
  )

  if (file.exists(path) && !file.remove(path)) {
    stop(
      paste("Could not replace existing file:", path),
      call. = FALSE
    )
  }

  if (!file.rename(temp_path, path)) {
    stop(
      paste("Could not save file:", path),
      call. = FALSE
    )
  }

  invisible(path)
}

mean_or_na <- function(x) {
  x <- x[is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  mean(x)
}

min_or_na <- function(x) {
  x <- x[is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  min(x)
}

max_or_na <- function(x) {
  x <- x[is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  max(x)
}

quantile_or_na <- function(x,
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

has_text <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

column_or_default <- function(data,
                              name,
                              default) {
  if (name %in% names(data)) {
    return(data[[name]])
  }

  rep(default, nrow(data))
}

select_existing <- function(data,
                            columns) {
  data[
    ,
    columns[columns %in% names(data)],
    drop = FALSE
  ]
}

add_condition_columns <- function(data,
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

rbind_fill <- function(data_list) {
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

method_labels <- function() {
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

add_method_labels <- function(data) {
  labels <- method_labels()
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

extract_flagged_cluster_diagnostics <- function(replicates,
                                                condition) {
  if (!"cluster_diagnostics" %in% names(replicates)) {
    return(data.frame())
  }

  warning_count <- column_or_default(
    replicates,
    "cluster_warning_count",
    0
  )
  error_count <- column_or_default(
    replicates,
    "cluster_error_count",
    0
  )
  dropped_count <- column_or_default(
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

  rbind_fill(diagnostics)
}

prepare_replicates_for_storage <- function(replicates,
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

  flagged_diagnostics <- extract_flagged_cluster_diagnostics(
    replicates = replicates,
    condition = condition
  )

  if ("cluster_diagnostics" %in% names(replicates)) {
    replicates$cluster_diagnostics <- NULL
  }

  replicates <- add_condition_columns(
    data = replicates,
    condition = condition
  )

  list(
    replicates = replicates,
    flagged_diagnostics = flagged_diagnostics
  )
}

make_status_snapshot <- function(checkpoint_dir,
                                 design) {
  checkpoint_paths <- list.files(
    checkpoint_dir,
    pattern = "^condition_S2C[0-9]{3}[.]rds$",
    full.names = TRUE
  )

  checkpoint_map <- setNames(
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

summarize_diagnostics <- function(replicates) {
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

      warning_values <- column_or_default(
        method_results,
        "warning",
        NA_character_
      )
      optimizer_warning <- column_or_default(
        method_results,
        "optimizer_warning",
        NA_character_
      )
      optimizer_code <- column_or_default(
        method_results,
        "optimizer_code",
        NA_real_
      )
      error_values <- column_or_default(
        method_results,
        "error",
        NA_character_
      )
      converged_values <- column_or_default(
        method_results,
        "converged",
        NA
      )
      singular_values <- column_or_default(
        method_results,
        "singular",
        NA
      )
      template_warning <- column_or_default(
        method_results,
        "template_warning",
        NA_character_
      )
      cluster_warning_count <- column_or_default(
        method_results,
        "cluster_warning_count",
        NA_real_
      )
      cluster_error_count <- column_or_default(
        method_results,
        "cluster_error_count",
        NA_real_
      )
      dropped_cluster_count <- column_or_default(
        method_results,
        "dropped_cluster_count",
        NA_real_
      )
      estimated_ri_sd <- column_or_default(
        method_results,
        "estimated_random_intercept_sd",
        NA_real_
      )
      estimated_rs_sd <- column_or_default(
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
          has_text(warning_values)
        ),
        optimizer_warning_rep_rate = 100 * mean(
          has_text(optimizer_warning)
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
          has_text(error_values)
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
          has_text(template_warning)
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
        mean_cluster_warning_count = mean_or_na(
          cluster_warning_count
        ),
        maximum_cluster_warning_count = max_or_na(
          cluster_warning_count
        ),
        mean_cluster_error_count = mean_or_na(
          cluster_error_count
        ),
        maximum_cluster_error_count = max_or_na(
          cluster_error_count
        ),
        mean_dropped_cluster_count = mean_or_na(
          dropped_cluster_count
        ),
        maximum_dropped_cluster_count = max_or_na(
          dropped_cluster_count
        ),
        mean_retained_clusters = mean_or_na(
          method_results$retained_clusters
        ),
        minimum_retained_clusters = min_or_na(
          method_results$retained_clusters
        ),
        mean_estimated_random_intercept_sd = mean_or_na(
          estimated_ri_sd
        ),
        mean_estimated_random_slope_sd = mean_or_na(
          estimated_rs_sd
        ),
        mean_runtime_sec = mean_or_na(
          method_results$runtime_sec
        ),
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, method_rows)
  })

  out <- do.call(rbind, condition_rows)
  rownames(out) <- NULL
  add_method_labels(out)
}

count_text_values <- function(data,
                              column,
                              source) {
  if (!column %in% names(data)) {
    return(data.frame())
  }

  values <- trimws(as.character(data[[column]]))
  values <- values[has_text(values)]

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

make_message_frequency <- function(replicates,
                                   flagged_diagnostics) {
  outputs <- list(
    count_text_values(
      replicates,
      "warning",
      "overall_warning_field"
    ),
    count_text_values(
      replicates,
      "optimizer_warning",
      "optimizer_warning"
    ),
    count_text_values(
      replicates,
      "error",
      "overall_error"
    ),
    count_text_values(
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
      outputs[[length(outputs) + 1L]] <- count_text_values(
        flagged_diagnostics,
        column,
        paste0("cluster_", column)
      )
    }

    for (column in error_columns) {
      outputs[[length(outputs) + 1L]] <- count_text_values(
        flagged_diagnostics,
        column,
        paste0("cluster_", column)
      )
    }
  }

  out <- rbind_fill(outputs)

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

make_negative_control_comparison <- function(replicates) {
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
      maximum_absolute_estimate_difference = max_or_na(
        abs(estimate_difference)
      ),
      standard_error_exact_match_rate = 100 * mean(
        x$std_error_cats_trunc == x$std_error_cats,
        na.rm = TRUE
      ),
      maximum_absolute_p_value_difference = max_or_na(
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
      mean_retained_cluster_difference = mean_or_na(
        retained_difference
      ),
      minimum_truncated_retained_clusters = min_or_na(
        x$retained_clusters_cats_trunc
      ),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

make_method_vs_reference <- function(summary_results,
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
  add_method_labels(out)
}

summarize_rs_subset <- function(data,
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

make_rs_singularity_sensitivity <- function(replicates) {
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

    all_usable <- summarize_rs_subset(
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

    nonsingular <- summarize_rs_subset(
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

make_dgp_diagnostics <- function(replicates) {
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
      mean_realized_mean_slope = mean_or_na(
        x$realized_mean_slope
      ),
      sd_realized_mean_slope = stats::sd(
        x$realized_mean_slope,
        na.rm = TRUE
      ),
      minimum_realized_mean_slope = min_or_na(
        x$realized_mean_slope
      ),
      maximum_realized_mean_slope = max_or_na(
        x$realized_mean_slope
      ),
      mean_realized_random_slope_sd = mean_or_na(
        x$realized_random_slope_sd
      ),
      sd_realized_random_slope_sd = stats::sd(
        x$realized_random_slope_sd,
        na.rm = TRUE
      ),
      minimum_realized_random_slope_sd = min_or_na(
        x$realized_random_slope_sd
      ),
      maximum_realized_random_slope_sd = max_or_na(
        x$realized_random_slope_sd
      ),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

make_crn_audit <- function(replicates,
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

make_primary_performance_table <- function(summary_results) {
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

  out <- select_existing(
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

make_mcse_summary <- function(summary_results) {
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
        minimum_mcse = min_or_na(x$mcse),
        mean_mcse = mean_or_na(x$mcse),
        maximum_mcse = max_or_na(x$mcse),
        stringsAsFactors = FALSE
      )
    }
  )

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

make_source_checksums <- function(project_root) {
  paths <- c(
    DESCRIPTION = file.path(
      project_root,
      "DESCRIPTION"
    ),
    pwr_func_study1 = file.path(
      project_root,
      "R",
      "pwr_func_study1.R"
    ),
    pwr_func_study1_helpers = file.path(
      project_root,
      "R",
      "pwr_func_study1_helpers.R"
    ),
    pwr_func_study2 = file.path(
      project_root,
      "R",
      "pwr_func_study2.R"
    ),
    pwr_func_study2_helpers = file.path(
      project_root,
      "R",
      "pwr_func_study2_helpers.R"
    ),
    robust_mixed_models = file.path(
      project_root, "R", "robust_mixed_models.R"
    ),
    definitive_sharding_helpers = file.path(
      project_root, "data-raw", "definitive_sharding_helpers.R"
    ),
    study2_final_simulation = file.path(
      project_root,
      "data-raw",
      "study2_final_simulation.R"
    )
  )

  exists <- file.exists(paths)
  paths <- paths[exists]

  if (length(paths) == 0L) {
    return(data.frame())
  }

  data.frame(
    source = names(paths),
    path = normalizePath(
      paths,
      winslash = "/",
      mustWork = TRUE
    ),
    md5 = unname(tools::md5sum(paths)),
    stringsAsFactors = FALSE
  )
}

project_root <- find_project_root()

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop(
    "The pkgload package is required to run this data-raw script.",
    call. = FALSE
  )
}

if (!requireNamespace("pbkrtest", quietly = TRUE)) {
  stop(
    paste(
      "The pbkrtest package is required because the final simulation",
      "includes mixed models with Kenward-Roger inference."
    ),
    call. = FALSE
  )
}

pkgload::load_all(project_root, quiet = TRUE)

if (!requireNamespace("robustlmm", quietly = TRUE)) {
  stop(
    "The robustlmm package is required for the definitive robust mixed-model comparators.",
    call. = FALSE
  )
}

source(
  file.path(
    project_root,
    "data-raw",
    "definitive_sharding_helpers.R"
  )
)

# -------------------------------------------------------------------------
# Frozen definitive-study configuration
# -------------------------------------------------------------------------

final_reps <- 2000L
alpha <- 0.05
minimum_usable_reps <- 1900L
final_seed_base <- 20260905L
shard_size <- 10L
minimum_free_gb <- 2.0
retain_completed_shards <- FALSE
overwrite_completed <- FALSE

# Set to a character vector such as c("S2C001", "S2C002") to run only
# selected conditions. Leave NULL for the complete study.
condition_ids_to_run <- NULL

methods <- c(
  "rs",
  "ri",
  "cr2",
  "cats",
  "cats_trunc",
  "cats_robust",
  "cats_robustbase",
  "robust_ri",
  "robust_rs"
)

output_dir <- file.path(
  project_root,
  "data-raw",
  "study2-results",
  "definitive-study"
)

checkpoint_dir <- file.path(
  output_dir,
  "conditions"
)

dir.create(
  checkpoint_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

shard_dir <- file.path(output_dir, "shards")
shard_status_dir <- file.path(output_dir, "shard-status")

dir.create(shard_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(shard_status_dir, recursive = TRUE, showWarnings = FALSE)

contamination_specifications <- data.frame(
  contamination = c(
    "none",
    "vertical"
  ),
  contamination_label = c(
    "Clean",
    "Vertical outliers"
  ),
  contamination_size = c(
    0,
    6
  ),
  stringsAsFactors = FALSE
)

design_rows <- list()
design_index <- 0L

for (n_clusters in c(10L, 20L, 40L)) {
  for (beta in c(0, 0.10)) {
    for (random_slope_sd in c(0.05, 0.10)) {
      for (
        contamination_index in
        seq_len(nrow(contamination_specifications))
      ) {
        design_index <- design_index + 1L

        design_rows[[design_index]] <- data.frame(
          n_clusters = n_clusters,
          beta = beta,
          random_slope_sd = random_slope_sd,
          contamination_specifications[
            contamination_index,
            ,
            drop = FALSE
          ],
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

final_design <- do.call(rbind, design_rows)
rownames(final_design) <- NULL

final_design$condition_id <- sprintf(
  "S2C%03d",
  seq_len(nrow(final_design))
)
final_design$cluster_size <- 40L
final_design$effect_label <- ifelse(
  final_design$beta == 0,
  "Null",
  "Alternative"
)
final_design$intercept <- 0
final_design$random_intercept_sd <- 1
final_design$random_slope_variance <-
  final_design$random_slope_sd^2
final_design$residual_sd <- 1
final_design$x_sd <- 1
final_design$contamination_prop <- 0.05
final_design$reps <- final_reps
final_design$alpha <- alpha
final_design$shard_size <- shard_size
final_design$minimum_free_gb <- minimum_free_gb
final_design$retain_completed_shards <- retain_completed_shards
final_design$method_set <- paste(
  methods,
  collapse = ","
)

cluster_seed_index <- match(
  final_design$n_clusters,
  c(10L, 20L, 40L)
)

final_design$condition_seed <- as.integer(
  final_seed_base + cluster_seed_index - 1L
)
final_design$common_random_number_group <- paste0(
  "G",
  final_design$n_clusters
)

final_design <- final_design[
  ,
  c(
    "condition_id",
    "n_clusters",
    "cluster_size",
    "beta",
    "effect_label",
    "intercept",
    "random_intercept_sd",
    "random_slope_sd",
    "random_slope_variance",
    "residual_sd",
    "x_sd",
    "contamination",
    "contamination_label",
    "contamination_prop",
    "contamination_size",
    "reps",
    "alpha",
    "shard_size",
    "minimum_free_gb",
    "retain_completed_shards",
    "method_set",
    "condition_seed",
    "common_random_number_group"
  )
]

design_path <- file.path(
  output_dir,
  "study2_final_design.rds"
)

if (file.exists(design_path)) {
  existing_design <- readRDS(design_path)

  if (!identical(existing_design, final_design)) {
    if (!overwrite_completed) {
      stop(
        paste(
          "The saved definitive-study design differs from the current design.",
          "Set overwrite_completed <- TRUE only if the frozen design is",
          "intentionally being replaced."
        ),
        call. = FALSE
      )
    }

    old_checkpoints <- list.files(
      checkpoint_dir,
      pattern = "^condition_S2C[0-9]{3}[.]rds$",
      full.names = TRUE
    )

    if (length(old_checkpoints) > 0L) {
      file.remove(old_checkpoints)
    }
  }
}

save_rds_atomic(
  final_design,
  design_path
)
write_csv_atomic(
  final_design,
  file.path(output_dir, "study2_final_design.csv")
)

source_checksums <- make_source_checksums(
  project_root
)

package_description <- read.dcf(
  file.path(project_root, "DESCRIPTION")
)

metadata <- list(
  study = "mmiCATs Study 2 definitive manuscript-version simulation",
  created_at = Sys.time(),
  project_root = project_root,
  package_version = unname(
    package_description[1L, "Version"]
  ),
  r_version = R.version.string,
  final_reps = final_reps,
  alpha = alpha,
  minimum_usable_reps = minimum_usable_reps,
  final_seed_base = final_seed_base,
  shard_size = shard_size,
  minimum_free_gb = minimum_free_gb,
  retain_completed_shards = retain_completed_shards,
  methods = methods,
  primary_estimand = paste(
    "The superpopulation mean cluster slope beta. The realized",
    "sample mean cluster slope is retained only as a diagnostic."
  ),
  common_random_numbers = paste(
    "All eight conditions with the same number of clusters use",
    "the same condition seed. Seeds differ across cluster counts."
  ),
  calibration_independence = paste(
    "The definitive-study seeds differ from numerical validation,",
    "plumbing-pilot, and random-slope convergence-diagnostic seeds."
  ),
  frozen_parameters = list(
    n_clusters = c(10L, 20L, 40L),
    cluster_size = 40L,
    beta = c(0, 0.10),
    random_intercept_sd = 1,
    random_slope_sd = c(0.05, 0.10),
    random_slope_variance = c(0.0025, 0.0100),
    intercept_slope_covariance = 0,
    residual_sd = 1,
    x_sd = 1,
    contamination_prop = 0.05,
    vertical_contamination_size = 6
  ),
  correctly_specified_model = paste(
    "out ~ x + (1 + x || cluster), fitted by REML with",
    "Kenward-Roger inference and default lmerTest/lme4 optimizer settings."
  ),
  singularity_rule = paste(
    "A singular fit remains usable when fixed-effect inference is",
    "complete and finite and there is no genuine convergence failure."
  ),
  failed_fit_rule = paste(
    "Failed fits are not replaced. Method-specific usable denominators",
    "and failure rates are reported."
  ),
  computational_adequacy_rule = paste(
    "At least 1,900 usable fits among 2,000 attempts are required",
    "for every method-condition combination."
  ),
  truncated_cats_role = paste(
    "Truncated CATs is retained as a negative control because",
    "contamination is distributed similarly within every cluster."
  ),
  no_contamination_placeholder = paste(
    "pwr_func_study2 requires a positive contamination_size.",
    "The script passes 1 under contamination = 'none'; the value is unused."
  ),
  source_checksums = source_checksums,
  overwrite_completed = overwrite_completed,
  condition_ids_to_run = condition_ids_to_run,
  session_info = utils::sessionInfo(),
  system_info = Sys.info()
)

save_rds_atomic(
  metadata,
  file.path(output_dir, "study2_final_metadata.rds")
)

write_csv_atomic(
  source_checksums,
  file.path(output_dir, "study2_source_checksums.csv")
)

writeLines(
  capture.output(utils::sessionInfo()),
  con = file.path(output_dir, "session_info.txt"),
  useBytes = TRUE
)

# -------------------------------------------------------------------------
# Run definitive-study conditions in deterministic shards
# -------------------------------------------------------------------------

run_design <- final_design

if (!is.null(condition_ids_to_run)) {
  invalid_ids <- setdiff(
    condition_ids_to_run,
    final_design$condition_id
  )

  if (length(invalid_ids) > 0L) {
    stop(
      paste(
        "Unknown condition_ids_to_run:",
        paste(invalid_ids, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  run_design <- final_design[
    final_design$condition_id %in% condition_ids_to_run,
    ,
    drop = FALSE
  ]
}

for (condition_index in seq_len(nrow(run_design))) {
  condition <- run_design[
    condition_index,
    ,
    drop = FALSE
  ]

  checkpoint_path <- file.path(
    checkpoint_dir,
    paste0("condition_", condition$condition_id, ".rds")
  )

  if (file.exists(checkpoint_path) && !overwrite_completed) {
    existing_checkpoint <- tryCatch(
      readRDS(checkpoint_path),
      error = function(e) NULL
    )

    if (!is.null(existing_checkpoint) &&
        identical(existing_checkpoint$status, "complete")) {
      message(sprintf("Skipping completed condition %s.", condition$condition_id))
      next
    }
  }

  replicate_seed_vector <- definitive_make_replicate_seeds(
    condition_seed = condition$condition_seed,
    total_reps = condition$reps
  )

  shard_plan <- definitive_make_shard_plan(
    total_reps = condition$reps,
    shard_size = condition$shard_size
  )

  message(sprintf(
    paste0(
      "Running %s of %s: G = %s, beta = %s, random-slope SD = %s, ",
      "condition = %s; %s shards of up to %s reps."
    ),
    condition$condition_id,
    nrow(final_design),
    condition$n_clusters,
    format(condition$beta, trim = TRUE),
    format(condition$random_slope_sd, trim = TRUE),
    condition$contamination_label,
    nrow(shard_plan),
    condition$shard_size
  ))

  condition_failed <- FALSE

  for (shard_index in seq_len(nrow(shard_plan))) {
    shard_row <- shard_plan[
      shard_index,
      ,
      drop = FALSE
    ]

    shard_result <- tryCatch(
      definitive_run_shard_checkpoint(
        study = "study2",
        condition = condition,
        shard_row = shard_row,
        replicate_seed_vector = replicate_seed_vector,
        methods = methods,
        shard_dir = shard_dir,
        minimum_free_gb = condition$minimum_free_gb,
        overwrite_completed = overwrite_completed
      ),
      error = function(e) e
    )

    if (inherits(shard_result, "error")) {
      message(sprintf(
        "Condition %s stopped before/at shard %s: %s",
        condition$condition_id,
        shard_row$shard_id,
        conditionMessage(shard_result)
      ))
      condition_failed <- TRUE
      break
    }

    if (identical(shard_result$action, "error")) {
      message(sprintf(
        "Condition %s shard %s returned a caught error: %s",
        condition$condition_id,
        shard_row$shard_id,
        shard_result$checkpoint$error
      ))
      condition_failed <- TRUE
      break
    }

    current <- definitive_collect_condition_shards(
      condition = condition,
      shard_plan = shard_plan,
      replicate_seed_vector = replicate_seed_vector,
      methods = methods,
      shard_dir = shard_dir
    )

    write_csv_atomic(
      current$status,
      file.path(
        shard_status_dir,
        paste0("condition_", condition$condition_id, "_shard_status.csv")
      )
    )

    completed_shards <- sum(current$status$status == "complete")

    message(sprintf(
      "  %s %s: %s of %s shards complete.",
      condition$condition_id,
      shard_row$shard_id,
      completed_shards,
      nrow(shard_plan)
    ))
  }

  collected <- definitive_collect_condition_shards(
    condition = condition,
    shard_plan = shard_plan,
    replicate_seed_vector = replicate_seed_vector,
    methods = methods,
    shard_dir = shard_dir
  )

  write_csv_atomic(
    collected$status,
    file.path(
      shard_status_dir,
      paste0("condition_", condition$condition_id, "_shard_status.csv")
    )
  )

  if (condition_failed || !collected$complete) {
    next
  }

  full_settings <- collected$checkpoints[[1L]]$settings
  full_settings$reps <- condition$reps
  full_settings$seed <- condition$condition_seed
  full_settings$replicate_seeds <- replicate_seed_vector
  full_settings$methods <- methods

  prepared <- prepare_replicates_for_storage(
    replicates = collected$replicates,
    settings = full_settings,
    condition = condition
  )

  condition_summary <- mmiCATs:::study1_summarize_results(
    replicate_results = collected$replicates,
    methods = methods,
    reps = condition$reps
  )
  rownames(condition_summary) <- NULL
  condition_summary <- add_condition_columns(condition_summary, condition)
  condition_summary <- add_method_labels(condition_summary)

  shard_elapsed <- vapply(
    collected$checkpoints,
    function(x) x$elapsed_sec,
    numeric(1)
  )
  shard_started <- do.call(
    c,
    lapply(collected$checkpoints, function(x) x$started_at)
  )
  shard_completed <- do.call(
    c,
    lapply(collected$checkpoints, function(x) x$completed_at)
  )

  condition_checkpoint <- list(
    status = "complete",
    condition = condition,
    result = list(
      summary = condition_summary,
      replicates = prepared$replicates,
      settings = full_settings
    ),
    flagged_cluster_diagnostics = prepared$flagged_diagnostics,
    error = NA_character_,
    started_at = min(shard_started),
    completed_at = max(shard_completed),
    elapsed_sec = sum(shard_elapsed, na.rm = TRUE),
    shard_plan = shard_plan
  )

  save_rds_atomic(condition_checkpoint, checkpoint_path)

  # The complete condition checkpoint is the durable artifact. Shard files are
  # temporary restart units and are removed after the condition checkpoint has
  # been written atomically and verified by the save helper. This prevents the
  # long local run from accumulating avoidable disk usage.
  if (!isTRUE(condition$retain_completed_shards)) {
    completed_shard_paths <- vapply(
      seq_len(nrow(shard_plan)),
      function(i) {
        definitive_shard_checkpoint_path(
          shard_dir = shard_dir,
          condition_id = condition$condition_id,
          shard_id = shard_plan$shard_id[i]
        )
      },
      character(1)
    )

    existing_shard_paths <- completed_shard_paths[
      file.exists(completed_shard_paths)
    ]

    if (length(existing_shard_paths) > 0L) {
      removed <- file.remove(existing_shard_paths)
      if (any(!removed)) {
        warning(
          sprintf(
            "Could not remove %s completed temporary shard file(s) for %s.",
            sum(!removed),
            condition$condition_id
          ),
          call. = FALSE
        )
      }
    }
  }

  status_snapshot <- make_status_snapshot(
    checkpoint_dir = checkpoint_dir,
    design = final_design
  )
  write_csv_atomic(
    status_snapshot,
    file.path(output_dir, "study2_condition_status.csv")
  )

  message(sprintf(
    "Completed %s: %s of %s conditions complete.",
    condition$condition_id,
    sum(status_snapshot$status == "complete"),
    nrow(final_design)
  ))
}

# -------------------------------------------------------------------------
# Combine completed definitive condition checkpoints
# -------------------------------------------------------------------------

final_status <- make_status_snapshot(
  checkpoint_dir = checkpoint_dir,
  design = final_design
)

write_csv_atomic(
  final_status,
  file.path(
    output_dir,
    "study2_condition_status.csv"
  )
)
save_rds_atomic(
  final_status,
  file.path(
    output_dir,
    "study2_condition_status.rds"
  )
)

complete_ids <- final_status$condition_id[
  final_status$status == "complete"
]

if (length(complete_ids) == 0L) {
  stop(
    "No definitive-study conditions completed successfully.",
    call. = FALSE
  )
}

checkpoint_paths <- file.path(
  checkpoint_dir,
  paste0(
    "condition_",
    complete_ids,
    ".rds"
  )
)

checkpoints <- lapply(
  checkpoint_paths,
  readRDS
)

final_summary <- do.call(
  rbind,
  lapply(
    checkpoints,
    function(checkpoint) {
      checkpoint$result$summary
    }
  )
)

final_replicates <- do.call(
  rbind,
  lapply(
    checkpoints,
    function(checkpoint) {
      checkpoint$result$replicates
    }
  )
)

flagged_cluster_diagnostics <- rbind_fill(
  lapply(
    checkpoints,
    function(checkpoint) {
      checkpoint$flagged_cluster_diagnostics
    }
  )
)

rownames(final_summary) <- NULL
rownames(final_replicates) <- NULL

final_summary <- final_summary[
  order(
    final_summary$n_clusters,
    final_summary$beta,
    final_summary$random_slope_sd,
    match(
      final_summary$contamination,
      c("none", "vertical")
    ),
    final_summary$method_order
  ),
  ,
  drop = FALSE
]

final_replicates <- final_replicates[
  order(
    final_replicates$n_clusters,
    final_replicates$beta,
    final_replicates$random_slope_sd,
    match(
      final_replicates$contamination,
      c("none", "vertical")
    ),
    final_replicates$replicate,
    match(
      final_replicates$method,
      methods
    )
  ),
  ,
  drop = FALSE
]

rownames(final_summary) <- NULL
rownames(final_replicates) <- NULL

final_diagnostics <- summarize_diagnostics(
  final_replicates
)

primary_performance <- make_primary_performance_table(
  final_summary
)

negative_control_comparison <-
  make_negative_control_comparison(
    final_replicates
  )

robust_vs_cats <- make_method_vs_reference(
  summary_results = final_summary,
  comparison_methods = c(
    "cats_robust",
    "cats_robustbase"
  ),
  reference_method = "cats",
  comparison_name = "Robust CATs versus ordinary CATs"
)

random_slope_vs_ri <- make_method_vs_reference(
  summary_results = final_summary,
  comparison_methods = "rs",
  reference_method = "ri",
  comparison_name = paste(
    "Correct random-slope model versus",
    "misspecified random-intercept model"
  )
)

rs_sensitivity <- make_rs_singularity_sensitivity(
  final_replicates
)

dgp_diagnostics <- make_dgp_diagnostics(
  final_replicates
)

crn_audit <- make_crn_audit(
  final_replicates
)

message_frequency <- make_message_frequency(
  replicates = final_replicates,
  flagged_diagnostics =
    flagged_cluster_diagnostics
)

mcse_summary <- make_mcse_summary(
  final_summary
)

flagged_mixed_model_replicates <- final_replicates[
  final_replicates$method %in% c("rs", "ri") &
    (
      !final_replicates$fit_success |
        (
          !is.na(final_replicates$singular) &
            final_replicates$singular
        ) |
        has_text(final_replicates$optimizer_warning) |
        (
          !is.na(final_replicates$optimizer_code) &
            final_replicates$optimizer_code != 0
        ) |
        has_text(final_replicates$error)
    ),
  ,
  drop = FALSE
]

# -------------------------------------------------------------------------
# Final validation
# -------------------------------------------------------------------------

expected_summary_rows <-
  nrow(final_design) * length(methods)
expected_replicate_rows <-
  nrow(final_design) * final_reps * length(methods)

observed_counts <- stats::aggregate(
  rep(1L, nrow(final_replicates)),
  by = list(
    condition_id = final_replicates$condition_id,
    method = final_replicates$method
  ),
  FUN = sum
)
names(observed_counts)[3L] <- "observed_reps"

usable_counts <- stats::aggregate(
  as.integer(final_replicates$fit_success),
  by = list(
    condition_id = final_replicates$condition_id,
    method = final_replicates$method
  ),
  FUN = sum
)
names(usable_counts)[3L] <- "usable_reps"

condition_method_counts <- merge(
  observed_counts,
  usable_counts,
  by = c("condition_id", "method"),
  all = TRUE,
  sort = FALSE
)

condition_method_counts <- condition_method_counts[
  order(
    match(
      condition_method_counts$condition_id,
      final_design$condition_id
    ),
    match(
      condition_method_counts$method,
      methods
    )
  ),
  ,
  drop = FALSE
]
rownames(condition_method_counts) <- NULL

condition_method_counts$meets_usable_criterion <-
  condition_method_counts$usable_reps >=
    minimum_usable_reps

completed_conditions <- sum(
  final_status$status == "complete"
)

all_conditions_completed <-
  completed_conditions == nrow(final_design)

combined_dimensions_correct <-
  nrow(final_summary) == expected_summary_rows &&
    nrow(final_replicates) == expected_replicate_rows

condition_method_dimensions_correct <-
  nrow(condition_method_counts) ==
    expected_summary_rows &&
    all(
      condition_method_counts$observed_reps ==
        final_reps
    )

all_crn_checks_passed <-
  nrow(crn_audit) ==
    length(unique(final_design$n_clusters)) *
      final_reps &&
    all(crn_audit$passed)

all_method_conditions_meet_usability <-
  nrow(condition_method_counts) ==
    expected_summary_rows &&
    all(
      condition_method_counts$meets_usable_criterion
    )

final_validation <- data.frame(
  check = c(
    "all_conditions_completed",
    "combined_dimensions_correct",
    "condition_method_dimensions_correct",
    "all_crn_checks_passed",
    "all_method_conditions_meet_1900_usable_fits"
  ),
  passed = c(
    all_conditions_completed,
    combined_dimensions_correct,
    condition_method_dimensions_correct,
    all_crn_checks_passed,
    all_method_conditions_meet_usability
  ),
  details = c(
    sprintf(
      "%s of %s conditions complete",
      completed_conditions,
      nrow(final_design)
    ),
    sprintf(
      paste0(
        "%s summary rows and %s replicate rows; ",
        "expected %s and %s"
      ),
      nrow(final_summary),
      nrow(final_replicates),
      expected_summary_rows,
      expected_replicate_rows
    ),
    sprintf(
      paste0(
        "%s condition-method rows; all completed rows ",
        "should contain %s attempts"
      ),
      nrow(condition_method_counts),
      final_reps
    ),
    sprintf(
      "%s of %s cluster-by-replication CRN checks passed",
      sum(crn_audit$passed),
      nrow(crn_audit)
    ),
    sprintf(
      "%s of %s method-condition combinations have at least %s usable fits",
      sum(
        condition_method_counts$meets_usable_criterion
      ),
      nrow(condition_method_counts),
      minimum_usable_reps
    )
  ),
  stringsAsFactors = FALSE
)

final_results <- list(
  design = final_design,
  status = final_status,
  validation = final_validation,
  condition_method_counts =
    condition_method_counts,
  summary = final_summary,
  primary_performance = primary_performance,
  replicates = final_replicates,
  diagnostics = final_diagnostics,
  dgp_diagnostics = dgp_diagnostics,
  common_random_number_audit = crn_audit,
  flagged_mixed_model_replicates =
    flagged_mixed_model_replicates,
  flagged_cluster_diagnostics =
    flagged_cluster_diagnostics,
  message_frequency = message_frequency,
  cats_trunc_negative_control =
    negative_control_comparison,
  robust_vs_cats = robust_vs_cats,
  random_slope_vs_random_intercept =
    random_slope_vs_ri,
  random_slope_singularity_sensitivity =
    rs_sensitivity,
  mcse_summary = mcse_summary,
  metadata = metadata
)

# -------------------------------------------------------------------------
# Save combined outputs
# -------------------------------------------------------------------------

save_rds_atomic(
  final_validation,
  file.path(
    output_dir,
    "study2_final_validation.rds"
  )
)
save_rds_atomic(
  condition_method_counts,
  file.path(
    output_dir,
    "study2_condition_method_counts.rds"
  )
)
save_rds_atomic(
  final_summary,
  file.path(
    output_dir,
    "study2_final_summary.rds"
  )
)
save_rds_atomic(
  primary_performance,
  file.path(
    output_dir,
    "study2_primary_performance.rds"
  )
)
save_rds_atomic(
  final_replicates,
  file.path(
    output_dir,
    "study2_final_replicates.rds"
  )
)
save_rds_atomic(
  final_diagnostics,
  file.path(
    output_dir,
    "study2_final_diagnostics.rds"
  )
)
save_rds_atomic(
  dgp_diagnostics,
  file.path(
    output_dir,
    "study2_dgp_diagnostics.rds"
  )
)
save_rds_atomic(
  crn_audit,
  file.path(
    output_dir,
    "study2_crn_audit.rds"
  )
)
save_rds_atomic(
  flagged_mixed_model_replicates,
  file.path(
    output_dir,
    "study2_flagged_mixed_model_replicates.rds"
  )
)
save_rds_atomic(
  flagged_cluster_diagnostics,
  file.path(
    output_dir,
    "study2_flagged_cluster_diagnostics.rds"
  )
)
save_rds_atomic(
  negative_control_comparison,
  file.path(
    output_dir,
    "study2_cats_trunc_negative_control.rds"
  )
)
save_rds_atomic(
  robust_vs_cats,
  file.path(
    output_dir,
    "study2_robust_vs_cats.rds"
  )
)
save_rds_atomic(
  random_slope_vs_ri,
  file.path(
    output_dir,
    "study2_random_slope_vs_random_intercept.rds"
  )
)
save_rds_atomic(
  rs_sensitivity,
  file.path(
    output_dir,
    "study2_random_slope_singularity_sensitivity.rds"
  )
)
save_rds_atomic(
  message_frequency,
  file.path(
    output_dir,
    "study2_message_frequency.rds"
  )
)
save_rds_atomic(
  mcse_summary,
  file.path(
    output_dir,
    "study2_mcse_summary.rds"
  )
)
save_rds_atomic(
  final_results,
  file.path(
    output_dir,
    "study2_final_results.rds"
  )
)

write_csv_atomic(
  final_validation,
  file.path(
    output_dir,
    "study2_final_validation.csv"
  )
)
write_csv_atomic(
  condition_method_counts,
  file.path(
    output_dir,
    "study2_condition_method_counts.csv"
  )
)
write_csv_atomic(
  final_summary,
  file.path(
    output_dir,
    "study2_final_summary.csv"
  )
)
write_csv_atomic(
  primary_performance,
  file.path(
    output_dir,
    "study2_primary_performance.csv"
  )
)
write_csv_atomic(
  final_diagnostics,
  file.path(
    output_dir,
    "study2_final_diagnostics.csv"
  )
)
write_csv_atomic(
  dgp_diagnostics,
  file.path(
    output_dir,
    "study2_dgp_diagnostics.csv"
  )
)
write_csv_atomic(
  crn_audit,
  file.path(
    output_dir,
    "study2_crn_audit.csv"
  )
)
write_csv_atomic(
  flagged_mixed_model_replicates,
  file.path(
    output_dir,
    "study2_flagged_mixed_model_replicates.csv"
  )
)
# write_csv_atomic(
#   flagged_cluster_diagnostics,
#   file.path(
#     output_dir,
#     "study2_flagged_cluster_diagnostics.csv"
#   )
# )
write_csv_atomic(
  message_frequency,
  file.path(
    output_dir,
    "study2_message_frequency.csv"
  )
)
write_csv_atomic(
  negative_control_comparison,
  file.path(
    output_dir,
    "study2_cats_trunc_negative_control.csv"
  )
)
write_csv_atomic(
  robust_vs_cats,
  file.path(
    output_dir,
    "study2_robust_vs_cats.csv"
  )
)
write_csv_atomic(
  random_slope_vs_ri,
  file.path(
    output_dir,
    "study2_random_slope_vs_random_intercept.csv"
  )
)
write_csv_atomic(
  rs_sensitivity$long,
  file.path(
    output_dir,
    "study2_random_slope_singularity_sensitivity_long.csv"
  )
)
write_csv_atomic(
  rs_sensitivity$comparison,
  file.path(
    output_dir,
    "study2_random_slope_singularity_sensitivity_comparison.csv"
  )
)
write_csv_atomic(
  mcse_summary,
  file.path(
    output_dir,
    "study2_mcse_summary.csv"
  )
)

# -------------------------------------------------------------------------
# Console summary
# -------------------------------------------------------------------------

total_conditions <- nrow(final_design)
total_elapsed_hours <- sum(
  final_status$elapsed_sec[
    final_status$status == "complete"
  ],
  na.rm = TRUE
) / 3600

message("")
message("Study 2 final simulation processing complete.")
message(
  sprintf(
    "Completed conditions: %s of %s.",
    completed_conditions,
    total_conditions
  )
)
message(
  sprintf(
    "Total elapsed time across completed conditions: %.2f hours.",
    total_elapsed_hours
  )
)
message(paste("Results saved to:", output_dir))

message("")
message("Final validation checks:")
print(
  final_validation,
  row.names = FALSE
)

message("")
message("Monte Carlo standard-error summary:")
print(
  mcse_summary,
  row.names = FALSE
)

diagnostic_problems <- final_diagnostics[
  final_diagnostics$failure_rate > 0 |
    final_diagnostics$optimizer_warning_rep_rate > 0 |
    final_diagnostics$error_rep_rate > 0 |
    (
      !is.na(final_diagnostics$singular_rate) &
        final_diagnostics$singular_rate > 0
    ) |
    (
      !is.na(
        final_diagnostics$cluster_error_rep_rate
      ) &
        final_diagnostics$cluster_error_rep_rate > 0
    ) |
    (
      !is.na(
        final_diagnostics$dropped_cluster_rep_rate
      ) &
        final_diagnostics$dropped_cluster_rep_rate > 0
    ),
  ,
  drop = FALSE
]

message("")

if (nrow(diagnostic_problems) == 0L) {
  message(
    paste(
      "No fit failures, genuine optimizer warnings, errors,",
      "singular fits, or dropped robust-CATs clusters were detected."
    )
  )
} else {
  message("Diagnostic events detected:")
  print(
    diagnostic_problems[
      ,
      c(
        "condition_id",
        "model",
        "successful_reps",
        "failure_rate",
        "optimizer_warning_rep_rate",
        "error_rep_rate",
        "convergence_failure_rate",
        "singular_rate",
        "cluster_error_rep_rate",
        "dropped_cluster_rep_rate",
        "minimum_retained_clusters"
      ),
      drop = FALSE
    ],
    row.names = FALSE
  )
}

message("")
message("Truncated CATs negative-control summary:")
print(
  negative_control_comparison,
  row.names = FALSE
)

if (completed_conditions < total_conditions) {
  incomplete <- final_status[
    final_status$status != "complete",
    c(
      "condition_id",
      "n_clusters",
      "beta",
      "random_slope_sd",
      "contamination_label",
      "status",
      "condition_error"
    ),
    drop = FALSE
  ]

  message("")
  message(
    paste(
      "The combined outputs are partial because not all conditions",
      "are complete. Rerun this script to resume."
    )
  )
  print(
    incomplete,
    row.names = FALSE
  )
} else {
  message("")

  if (all(final_validation$passed)) {
    message(
      "All 24 frozen Study 2 conditions and validation checks passed."
    )
  } else {
    message(
      paste(
        "All Study 2 conditions completed, but one or more",
        "post-run validation criteria require review."
      )
    )
  }
}


