# Study 1 definitive manuscript-version simulation
#
# This script runs the frozen Study 1 design comparing:
#   1. a correctly specified random-intercept model with Kenward-Roger
#      inference;
#   2. ordinary least squares with CR2 and Satterthwaite inference;
#   3. ordinary cluster-adjusted t statistics (CATs);
#   4. truncated CATs, retained as a negative control;
#   5. robust CATs using robust::lmRob();
#   6. robust CATs using robustbase::lmrob(); and
#   7. a correctly specified robust random-intercept mixed model using
#      robustlmm with robust Satterthwaite inference.
#
# Frozen design:
#   - clusters: 10, 20, and 40;
#   - observations per cluster: 40;
#   - true slope: 0 and 0.10;
#   - contamination regimes:
#       a. none;
#       b. vertical outcome contamination of 6 residual SDs; and
#       c. bad leverage with x at 4 predictor SDs and an outcome displacement
#          of 0.375 residual SDs;
#   - contamination proportion: 0.05 within every cluster;
#   - 2,000 replications per condition.
#
# The script uses a new definitive-study seed that was not used for parameter
# calibration. Conditions with the same number of clusters use common random
# numbers to improve the precision of comparisons across slopes and
# contamination regimes.
#
# Each 2,000-replication condition is executed as deterministic small shards.
# Completed shards are skipped on restart, and a completed condition checkpoint
# is reconstructed only after every frozen shard is present and valid. No new
# shard begins when free disk space is below the frozen safety threshold.
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
    pattern = "study1_",
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
    pattern = "study1_",
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
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  mean(x)
}

max_or_na <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  max(x)
}

min_or_na <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  min(x)
}

sum_or_zero <- function(x) {
  if (length(x) == 0L || all(is.na(x))) {
    return(0)
  }

  sum(x, na.rm = TRUE)
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
    ri = "Random intercept (KR)",
    cr2 = "OLS with CR2",
    cats = "CATs",
    cats_trunc = "Truncated CATs",
    cats_robust = "Robust CATs: lmRob",
    cats_robustbase = "Robust CATs: lmrob",
    robust_ri = "Robust random intercept"
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
    pattern = "^condition_S1C[0-9]{3}[.]rds$",
    full.names = TRUE
  )

  checkpoint_map <- setNames(
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
      error_values <- column_or_default(
        method_results,
        "error",
        NA_character_
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
          has_text(warning_values)
        ),
        error_rep_rate = 100 * mean(
          has_text(error_values)
        ),
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
      grepl("warning", names(flagged_diagnostics),
            ignore.case = TRUE)
    ]
    error_columns <- names(flagged_diagnostics)[
      grepl("error", names(flagged_diagnostics),
            ignore.case = TRUE)
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

make_robust_vs_cats <- function(summary_results) {
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

  add_method_labels(out)
}

make_primary_performance_table <- function(summary_results) {
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
    robust_mixed_models = file.path(
      project_root, "R", "robust_mixed_models.R"
    ),
    definitive_sharding_helpers = file.path(
      project_root, "data-raw", "definitive_sharding_helpers.R"
    ),
    study1_final_simulation = file.path(
      project_root, "data-raw", "study1_final_simulation.R"
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
      "includes the random-intercept method with Kenward-Roger inference."
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
final_seed_base <- 20260815L
shard_size <- 10L
minimum_free_gb <- 2.0
retain_completed_shards <- FALSE
overwrite_completed <- FALSE

# Set to a character vector such as c("S1C001", "S1C002") to run only
# selected conditions. Leave NULL for the complete study.
condition_ids_to_run <- NULL

methods <- c(
  "ri",
  "cr2",
  "cats",
  "cats_trunc",
  "cats_robust",
  "cats_robustbase",
  "robust_ri"
)

output_dir <- file.path(
  project_root,
  "data-raw",
  "study1-results",
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
    "vertical",
    "bad_leverage"
  ),
  contamination_label = c(
    "Clean",
    "Vertical outliers",
    "Bad leverage"
  ),
  contamination_size = c(
    0,
    6,
    0.375
  ),
  leverage_size = c(
    0,
    0,
    4
  ),
  stringsAsFactors = FALSE
)

design_base <- expand.grid(
  n_clusters = c(10L, 20L, 40L),
  beta = c(0, 0.10),
  contamination_index = seq_len(
    nrow(contamination_specifications)
  ),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

design_base <- design_base[
  order(
    design_base$n_clusters,
    design_base$beta,
    design_base$contamination_index
  ),
  ,
  drop = FALSE
]

rownames(design_base) <- NULL

final_design <- cbind(
  design_base[
    ,
    c("n_clusters", "beta"),
    drop = FALSE
  ],
  contamination_specifications[
    design_base$contamination_index,
    ,
    drop = FALSE
  ]
)

rownames(final_design) <- NULL

final_design$condition_id <- sprintf(
  "S1C%03d",
  seq_len(nrow(final_design))
)
final_design$cluster_size <- 40L
final_design$intercept <- 0
final_design$random_intercept_sd <- 1
final_design$residual_sd <- 1
final_design$x_sd <- 1
final_design$contamination_prop <- 0.05
final_design$reps <- final_reps
final_design$alpha <- alpha
final_design$shard_size <- shard_size
final_design$minimum_free_gb <- minimum_free_gb
final_design$retain_completed_shards <- retain_completed_shards
final_design$effect_label <- ifelse(
  final_design$beta == 0,
  "Null",
  "Alternative"
)
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
    "residual_sd",
    "x_sd",
    "contamination",
    "contamination_label",
    "contamination_prop",
    "contamination_size",
    "leverage_size",
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
  "study1_final_design.rds"
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
      pattern = "^condition_S1C[0-9]{3}[.]rds$",
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
  file.path(output_dir, "study1_final_design.csv")
)

source_checksums <- make_source_checksums(
  project_root
)

package_description <- read.dcf(
  file.path(project_root, "DESCRIPTION")
)

metadata <- list(
  study = "mmiCATs Study 1 definitive manuscript-version simulation",
  created_at = Sys.time(),
  project_root = project_root,
  package_version = unname(
    package_description[1L, "Version"]
  ),
  r_version = R.version.string,
  final_reps = final_reps,
  alpha = alpha,
  final_seed_base = final_seed_base,
  shard_size = shard_size,
  minimum_free_gb = minimum_free_gb,
  retain_completed_shards = retain_completed_shards,
  methods = methods,
  common_random_numbers = paste(
    "Conditions with the same number of clusters use the same",
    "condition seed. Seeds differ across cluster counts."
  ),
  calibration_independence = paste(
    "The definitive-study seeds differ from all calibration seeds."
  ),
  frozen_parameters = list(
    n_clusters = c(10L, 20L, 40L),
    cluster_size = 40L,
    beta = c(0, 0.10),
    random_intercept_sd = 1,
    residual_sd = 1,
    x_sd = 1,
    contamination_prop = 0.05,
    vertical_contamination_size = 6,
    bad_leverage_x_size = 4,
    bad_leverage_outcome_size = 0.375
  ),
  truncated_cats_role = paste(
    "Truncated CATs is retained as a negative control because",
    "contamination is distributed similarly within every cluster."
  ),
  source_checksums = source_checksums,
  overwrite_completed = overwrite_completed,
  session_info = utils::sessionInfo(),
  system_info = Sys.info()
)

save_rds_atomic(
  metadata,
  file.path(output_dir, "study1_final_metadata.rds")
)

write_csv_atomic(
  source_checksums,
  file.path(output_dir, "study1_source_checksums.csv")
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
      "Running %s of %s: G = %s, beta = %s, condition = %s; ",
      "%s shards of up to %s reps."
    ),
    condition$condition_id,
    nrow(final_design),
    condition$n_clusters,
    format(condition$beta, trim = TRUE),
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
        study = "study1",
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
    file.path(output_dir, "study1_condition_status.csv")
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
    "study1_condition_status.csv"
  )
)
save_rds_atomic(
  final_status,
  file.path(
    output_dir,
    "study1_condition_status.rds"
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
    match(
      final_summary$contamination,
      c("none", "vertical", "bad_leverage")
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
    match(
      final_replicates$contamination,
      c("none", "vertical", "bad_leverage")
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

robust_vs_cats <- make_robust_vs_cats(
  final_summary
)

message_frequency <- make_message_frequency(
  replicates = final_replicates,
  flagged_diagnostics =
    flagged_cluster_diagnostics
)

mcse_summary <- make_mcse_summary(
  final_summary
)

final_results <- list(
  design = final_design,
  status = final_status,
  summary = final_summary,
  primary_performance = primary_performance,
  replicates = final_replicates,
  diagnostics = final_diagnostics,
  flagged_cluster_diagnostics =
    flagged_cluster_diagnostics,
  message_frequency = message_frequency,
  cats_trunc_negative_control =
    negative_control_comparison,
  robust_vs_cats = robust_vs_cats,
  mcse_summary = mcse_summary,
  metadata = metadata
)

# -------------------------------------------------------------------------
# Save combined outputs
# -------------------------------------------------------------------------

save_rds_atomic(
  final_summary,
  file.path(
    output_dir,
    "study1_final_summary.rds"
  )
)
save_rds_atomic(
  primary_performance,
  file.path(
    output_dir,
    "study1_primary_performance.rds"
  )
)
save_rds_atomic(
  final_replicates,
  file.path(
    output_dir,
    "study1_final_replicates.rds"
  )
)
save_rds_atomic(
  final_diagnostics,
  file.path(
    output_dir,
    "study1_final_diagnostics.rds"
  )
)
save_rds_atomic(
  flagged_cluster_diagnostics,
  file.path(
    output_dir,
    "study1_flagged_cluster_diagnostics.rds"
  )
)
save_rds_atomic(
  negative_control_comparison,
  file.path(
    output_dir,
    "study1_cats_trunc_negative_control.rds"
  )
)
save_rds_atomic(
  robust_vs_cats,
  file.path(
    output_dir,
    "study1_robust_vs_cats.rds"
  )
)
save_rds_atomic(
  mcse_summary,
  file.path(
    output_dir,
    "study1_mcse_summary.rds"
  )
)
save_rds_atomic(
  final_results,
  file.path(
    output_dir,
    "study1_final_results.rds"
  )
)

write_csv_atomic(
  final_summary,
  file.path(
    output_dir,
    "study1_final_summary.csv"
  )
)
write_csv_atomic(
  primary_performance,
  file.path(
    output_dir,
    "study1_primary_performance.csv"
  )
)
write_csv_atomic(
  final_diagnostics,
  file.path(
    output_dir,
    "study1_final_diagnostics.csv"
  )
)
write_csv_atomic(
  flagged_cluster_diagnostics,
  file.path(
    output_dir,
    "study1_flagged_cluster_diagnostics.csv"
  )
)
write_csv_atomic(
  message_frequency,
  file.path(
    output_dir,
    "study1_message_frequency.csv"
  )
)
write_csv_atomic(
  negative_control_comparison,
  file.path(
    output_dir,
    "study1_cats_trunc_negative_control.csv"
  )
)
write_csv_atomic(
  robust_vs_cats,
  file.path(
    output_dir,
    "study1_robust_vs_cats.csv"
  )
)
write_csv_atomic(
  mcse_summary,
  file.path(
    output_dir,
    "study1_mcse_summary.csv"
  )
)

# -------------------------------------------------------------------------
# Console summary
# -------------------------------------------------------------------------

completed_conditions <- sum(
  final_status$status == "complete"
)
total_conditions <- nrow(final_design)
total_elapsed_hours <- sum(
  final_status$elapsed_sec[
    final_status$status == "complete"
  ],
  na.rm = TRUE
) / 3600

message("")
message("Study 1 final simulation processing complete.")
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
message("Monte Carlo standard-error summary:")
print(
  mcse_summary,
  row.names = FALSE
)

diagnostic_problems <- final_diagnostics[
  final_diagnostics$failure_rate > 0 |
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
      "No fit failures, errors, singular fits, or dropped",
      "robust-CATs clusters were detected in completed conditions."
    )
  )
} else {
  message("Diagnostic problems detected:")
  print(
    diagnostic_problems[
      ,
      c(
        "condition_id",
        "model",
        "failure_rate",
        "error_rep_rate",
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
  message(
    "All 18 frozen Study 1 conditions completed successfully."
  )
}


