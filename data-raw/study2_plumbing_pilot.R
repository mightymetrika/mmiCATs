# Study 2 plumbing pilot
#
# This script runs a small plumbing, diagnostic, and runtime pilot for the
# frozen Study 2 design. It follows the conventions used for Study 1:
#
#   - one checkpoint per simulation condition;
#   - completed conditions are skipped on rerun;
#   - atomic RDS and CSV writes;
#   - common random numbers within each cluster-count block;
#   - replicate-level results and method-specific diagnostics;
#   - status files updated after every condition;
#   - a compact combined results object; and
#   - session information and package metadata.
#
# Frozen Study 2 design checked by this pilot:
#
#   - clusters: 10, 20, and 40;
#   - observations per cluster: 40;
#   - population mean slope: 0 and 0.10;
#   - random-slope SD: 0.05 and 0.10;
#   - random-intercept SD: 1;
#   - residual SD: 1;
#   - predictor SD: 1;
#   - intercept-slope covariance: 0;
#   - contamination: none and vertical outcome contamination of 6 residual SDs;
#   - contamination proportion: 0.05 within every cluster;
#   - methods:
#       1. correctly specified independent random-slope model with
#          Kenward-Roger inference;
#       2. random-intercept model with Kenward-Roger inference;
#       3. ordinary least squares with CR2 and Satterthwaite inference;
#       4. ordinary CATs;
#       5. truncated CATs;
#       6. robust CATs using robust::lmRob(); and
#       7. robust CATs using robustbase::lmrob();
#   - five pilot replications per condition.
#
# The pilot is intended to validate plumbing and computational behavior only.
# Its statistical performance results must not be used to recalibrate the
# frozen random-slope SDs or contamination magnitude.
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

add_condition_columns <- function(x,
                                  condition) {
  condition_rows <- condition[
    rep(1L, nrow(x)),
    ,
    drop = FALSE
  ]

  out <- cbind(condition_rows, x)
  rownames(out) <- NULL
  out
}

mean_or_na <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  mean(x)
}

min_or_na <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  min(x)
}

max_or_na <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  max(x)
}

range_or_na <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  max(x) - min(x)
}

has_text <- function(x) {
  !is.na(x) & nzchar(x)
}

column_or_default <- function(data,
                              name,
                              default) {
  if (name %in% names(data)) {
    return(data[[name]])
  }

  rep(default, nrow(data))
}

same_value <- function(x,
                       y,
                       tolerance = 1e-12) {
  isTRUE(all.equal(
    x,
    y,
    tolerance = tolerance,
    check.attributes = FALSE
  ))
}

make_study2_pilot_design <- function(pilot_reps,
                                     cluster_seeds,
                                     methods) {
  design_rows <- list()
  row_index <- 0L

  for (n_clusters in c(10L, 20L, 40L)) {
    for (beta in c(0, 0.10)) {
      for (random_slope_sd in c(0.05, 0.10)) {
        for (contamination in c("none", "vertical")) {
          row_index <- row_index + 1L

          design_rows[[row_index]] <- data.frame(
            condition_id = sprintf("S2P%03d", row_index),
            n_clusters = n_clusters,
            cluster_size = 40L,
            beta = beta,
            intercept = 0,
            random_intercept_sd = 1,
            random_slope_sd = random_slope_sd,
            random_slope_variance = random_slope_sd^2,
            residual_sd = 1,
            x_sd = 1,
            contamination = contamination,
            contamination_label = if (
              contamination == "none"
            ) {
              "Clean"
            } else {
              "Vertical outliers"
            },
            contamination_prop = 0.05,
            contamination_size = 6,
            reps = pilot_reps,
            alpha = 0.05,
            method_set = paste(methods, collapse = ","),
            condition_seed = unname(
              cluster_seeds[as.character(n_clusters)]
            ),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  design <- do.call(rbind, design_rows)
  rownames(design) <- NULL
  design
}

checkpoint_path_for <- function(checkpoint_dir,
                                condition_id) {
  file.path(
    checkpoint_dir,
    paste0("condition_", condition_id, ".rds")
  )
}

read_checkpoint_safely <- function(path) {
  tryCatch(
    readRDS(path),
    error = function(e) {
      structure(
        list(
          status = "unreadable",
          condition = NULL,
          result = NULL,
          error = conditionMessage(e),
          started_at = as.POSIXct(NA),
          completed_at = as.POSIXct(NA),
          elapsed_sec = NA_real_
        ),
        class = "study2_unreadable_checkpoint"
      )
    }
  )
}

collect_checkpoints <- function(checkpoint_dir) {
  checkpoint_paths <- sort(list.files(
    checkpoint_dir,
    pattern = "^condition_S2P[0-9]{3}[.]rds$",
    full.names = TRUE
  ))

  if (length(checkpoint_paths) == 0L) {
    return(list())
  }

  lapply(checkpoint_paths, read_checkpoint_safely)
}

checkpoint_status_row <- function(checkpoint) {
  if (inherits(checkpoint, "study2_unreadable_checkpoint")) {
    return(data.frame(
      condition_id = NA_character_,
      n_clusters = NA_integer_,
      cluster_size = NA_integer_,
      beta = NA_real_,
      intercept = NA_real_,
      random_intercept_sd = NA_real_,
      random_slope_sd = NA_real_,
      random_slope_variance = NA_real_,
      residual_sd = NA_real_,
      x_sd = NA_real_,
      contamination = NA_character_,
      contamination_label = NA_character_,
      contamination_prop = NA_real_,
      contamination_size = NA_real_,
      reps = NA_integer_,
      alpha = NA_real_,
      method_set = NA_character_,
      condition_seed = NA_integer_,
      status = "unreadable",
      condition_error = checkpoint$error,
      started_at = NA_character_,
      completed_at = NA_character_,
      elapsed_sec = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    checkpoint$condition,
    status = checkpoint$status,
    condition_error = checkpoint$error,
    started_at = as.character(checkpoint$started_at),
    completed_at = as.character(checkpoint$completed_at),
    elapsed_sec = checkpoint$elapsed_sec,
    stringsAsFactors = FALSE
  )
}

collect_status <- function(checkpoints) {
  if (length(checkpoints) == 0L) {
    return(data.frame())
  }

  status <- do.call(
    rbind,
    lapply(checkpoints, checkpoint_status_row)
  )
  rownames(status) <- NULL
  status
}

save_status_outputs <- function(checkpoint_dir,
                                output_dir) {
  checkpoints <- collect_checkpoints(checkpoint_dir)
  status <- collect_status(checkpoints)

  if (nrow(status) > 0L) {
    save_rds_atomic(
      status,
      file.path(output_dir, "study2_pilot_status.rds")
    )
    write_csv_atomic(
      status,
      file.path(output_dir, "study2_pilot_status.csv")
    )
  }

  invisible(status)
}

condition_diagnostics <- function(replicates) {
  condition_ids <- unique(replicates$condition_id)

  diagnostics <- lapply(condition_ids, function(condition_id) {
    condition_results <- replicates[
      replicates$condition_id == condition_id,
      ,
      drop = FALSE
    ]

    methods <- unique(condition_results$method)

    method_diagnostics <- lapply(methods, function(method) {
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
      template_warning <- column_or_default(
        method_results,
        "template_warning",
        NA_character_
      )
      estimated_random_intercept_sd <- column_or_default(
        method_results,
        "estimated_random_intercept_sd",
        NA_real_
      )
      estimated_random_slope_sd <- column_or_default(
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
        attempted_reps = nrow(method_results),
        usable_reps = sum(method_results$fit_success),
        usable_fit_rate = 100 * mean(method_results$fit_success),
        failure_rate = 100 * mean(!method_results$fit_success),
        warning_rep_rate = 100 * mean(has_text(warning_values)),
        error_rep_rate = 100 * mean(has_text(error_values)),
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
        cluster_warning_rep_rate = if (
          all(is.na(cluster_warning_count))
        ) {
          NA_real_
        } else {
          100 * mean(cluster_warning_count > 0, na.rm = TRUE)
        },
        cluster_error_rep_rate = if (
          all(is.na(cluster_error_count))
        ) {
          NA_real_
        } else {
          100 * mean(cluster_error_count > 0, na.rm = TRUE)
        },
        dropped_cluster_rep_rate = if (
          all(is.na(dropped_cluster_count))
        ) {
          NA_real_
        } else {
          100 * mean(dropped_cluster_count > 0, na.rm = TRUE)
        },
        template_warning_rate = 100 * mean(
          has_text(template_warning)
        ),
        mean_cluster_warning_count = mean_or_na(
          cluster_warning_count
        ),
        mean_cluster_error_count = mean_or_na(
          cluster_error_count
        ),
        mean_dropped_cluster_count = mean_or_na(
          dropped_cluster_count
        ),
        mean_retained_clusters = mean_or_na(
          method_results$retained_clusters
        ),
        minimum_retained_clusters = min_or_na(
          method_results$retained_clusters
        ),
        mean_estimated_random_intercept_sd = mean_or_na(
          estimated_random_intercept_sd
        ),
        mean_estimated_random_slope_sd = mean_or_na(
          estimated_random_slope_sd
        ),
        mean_runtime_sec = mean_or_na(
          method_results$runtime_sec
        ),
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, method_diagnostics)
  })

  out <- do.call(rbind, diagnostics)
  rownames(out) <- NULL
  out
}

condition_dgp_diagnostics <- function(replicates) {
  one_row_per_replication <- replicates[
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

  condition_ids <- unique(one_row_per_replication$condition_id)

  rows <- lapply(condition_ids, function(condition_id) {
    x <- one_row_per_replication[
      one_row_per_replication$condition_id == condition_id,
      ,
      drop = FALSE
    ]

    data.frame(
      condition_id = condition_id,
      n_clusters = x$n_clusters[1L],
      beta = x$beta[1L],
      random_slope_sd = x$random_slope_sd[1L],
      contamination = x$contamination[1L],
      reps = nrow(x),
      mean_realized_mean_slope = mean_or_na(
        x$realized_mean_slope
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

validate_checkpoint_schema <- function(checkpoint,
                                       methods) {
  required_summary_columns <- c(
    "model",
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
    "mean_retained_clusters",
    "mean_runtime_sec"
  )

  required_replicate_columns <- c(
    "replicate",
    "method",
    "true_beta",
    "realized_mean_slope",
    "realized_random_slope_sd",
    "estimate",
    "std_error",
    "df",
    "p_value",
    "conf_low",
    "conf_high",
    "reject",
    "cover",
    "fit_success",
    "converged",
    "singular",
    "retained_clusters",
    "estimated_random_intercept_sd",
    "estimated_random_slope_sd",
    "warning",
    "error",
    "runtime_sec",
    "cluster_diagnostics"
  )

  required_settings <- c(
    "n_clusters",
    "cluster_size",
    "beta",
    "intercept",
    "random_intercept_sd",
    "random_slope_sd",
    "random_slope_variance",
    "residual_sd",
    "x_sd",
    "contamination",
    "contamination_prop",
    "contamination_size",
    "reps",
    "alpha",
    "methods",
    "seed",
    "replicate_seeds"
  )

  if (!identical(checkpoint$status, "complete") ||
      is.null(checkpoint$result)) {
    return(data.frame(
      condition_id = if (
        !is.null(checkpoint$condition)
      ) {
        checkpoint$condition$condition_id
      } else {
        NA_character_
      },
      status_complete = FALSE,
      summary_rows_correct = FALSE,
      replicate_rows_correct = FALSE,
      methods_correct = FALSE,
      unique_method_replicate_rows = FALSE,
      summary_columns_complete = FALSE,
      replicate_columns_complete = FALSE,
      settings_complete = FALSE,
      settings_match_condition = FALSE,
      all_checks_passed = FALSE,
      missing_summary_columns = NA_character_,
      missing_replicate_columns = NA_character_,
      missing_settings = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  condition <- checkpoint$condition
  result <- checkpoint$result
  summary_results <- result$summary
  replicate_results <- result$replicates
  settings <- result$settings

  missing_summary <- setdiff(
    required_summary_columns,
    names(summary_results)
  )
  missing_replicates <- setdiff(
    required_replicate_columns,
    names(replicate_results)
  )
  missing_settings <- setdiff(
    required_settings,
    names(settings)
  )

  expected_replicate_rows <-
    condition$reps * length(methods)

  methods_correct <- setequal(
    as.character(summary_results$model),
    methods
  ) &&
    setequal(
      as.character(unique(replicate_results$method)),
      methods
    )

  duplicate_method_replicate <- duplicated(
    replicate_results[
      ,
      c("replicate", "method"),
      drop = FALSE
    ]
  )

  settings_match <- length(missing_settings) == 0L &&
    same_value(settings$n_clusters, condition$n_clusters) &&
    same_value(settings$cluster_size, condition$cluster_size) &&
    same_value(settings$beta, condition$beta) &&
    same_value(settings$intercept, condition$intercept) &&
    same_value(
      settings$random_intercept_sd,
      condition$random_intercept_sd
    ) &&
    same_value(
      settings$random_slope_sd,
      condition$random_slope_sd
    ) &&
    same_value(
      settings$random_slope_variance,
      condition$random_slope_variance
    ) &&
    same_value(settings$residual_sd, condition$residual_sd) &&
    same_value(settings$x_sd, condition$x_sd) &&
    identical(
      as.character(settings$contamination),
      as.character(condition$contamination)
    ) &&
    same_value(
      settings$contamination_prop,
      condition$contamination_prop
    ) &&
    same_value(
      settings$contamination_size,
      condition$contamination_size
    ) &&
    same_value(settings$reps, condition$reps) &&
    same_value(settings$alpha, condition$alpha) &&
    identical(as.character(settings$methods), methods) &&
    same_value(settings$seed, condition$condition_seed) &&
    length(settings$replicate_seeds) == condition$reps

  checks <- c(
    status_complete = TRUE,
    summary_rows_correct =
      nrow(summary_results) == length(methods),
    replicate_rows_correct =
      nrow(replicate_results) == expected_replicate_rows,
    methods_correct = methods_correct,
    unique_method_replicate_rows =
      !any(duplicate_method_replicate),
    summary_columns_complete = length(missing_summary) == 0L,
    replicate_columns_complete =
      length(missing_replicates) == 0L,
    settings_complete = length(missing_settings) == 0L,
    settings_match_condition = settings_match
  )

  data.frame(
    condition_id = condition$condition_id,
    status_complete = checks["status_complete"],
    summary_rows_correct = checks["summary_rows_correct"],
    replicate_rows_correct = checks["replicate_rows_correct"],
    methods_correct = checks["methods_correct"],
    unique_method_replicate_rows =
      checks["unique_method_replicate_rows"],
    summary_columns_complete =
      checks["summary_columns_complete"],
    replicate_columns_complete =
      checks["replicate_columns_complete"],
    settings_complete = checks["settings_complete"],
    settings_match_condition =
      checks["settings_match_condition"],
    all_checks_passed = all(checks),
    missing_summary_columns = paste(
      missing_summary,
      collapse = ","
    ),
    missing_replicate_columns = paste(
      missing_replicates,
      collapse = ","
    ),
    missing_settings = paste(
      missing_settings,
      collapse = ","
    ),
    stringsAsFactors = FALSE
  )
}

make_schema_validation <- function(checkpoints,
                                   methods) {
  rows <- lapply(
    checkpoints,
    validate_checkpoint_schema,
    methods = methods
  )

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

make_crn_audit <- function(checkpoints,
                           replicates,
                           tolerance = 1e-12) {
  complete_checkpoints <- checkpoints[
    vapply(
      checkpoints,
      function(checkpoint) {
        identical(checkpoint$status, "complete")
      },
      logical(1)
    )
  ]

  seed_rows <- do.call(
    rbind,
    lapply(complete_checkpoints, function(checkpoint) {
      data.frame(
        condition_id = checkpoint$condition$condition_id,
        n_clusters = checkpoint$condition$n_clusters,
        replicate = seq_along(
          checkpoint$result$settings$replicate_seeds
        ),
        replicate_seed =
          checkpoint$result$settings$replicate_seeds,
        stringsAsFactors = FALSE
      )
    })
  )

  one_row_per_replication <- replicates[
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
      "realized_mean_slope",
      "realized_random_slope_sd"
    ),
    drop = FALSE
  ]

  groups <- unique(
    seed_rows[
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

  rows <- lapply(seq_len(nrow(groups)), function(group_index) {
    n_clusters <- groups$n_clusters[group_index]
    replicate_id <- groups$replicate[group_index]

    seed_group <- seed_rows[
      seed_rows$n_clusters == n_clusters &
        seed_rows$replicate == replicate_id,
      ,
      drop = FALSE
    ]

    dgp_group <- one_row_per_replication[
      one_row_per_replication$n_clusters == n_clusters &
        one_row_per_replication$replicate == replicate_id,
      ,
      drop = FALSE
    ]

    contamination_ranges <- list()
    contamination_index <- 0L

    for (beta in c(0, 0.10)) {
      for (random_slope_sd in c(0.05, 0.10)) {
        x <- dgp_group[
          dgp_group$beta == beta &
            dgp_group$random_slope_sd == random_slope_sd,
          ,
          drop = FALSE
        ]

        contamination_index <- contamination_index + 1L
        contamination_ranges[[contamination_index]] <- c(
          range_or_na(x$realized_mean_slope),
          range_or_na(x$realized_random_slope_sd)
        )
      }
    }

    contamination_ranges <- unlist(contamination_ranges)

    beta_shift_differences <- numeric(0)

    for (random_slope_sd in c(0.05, 0.10)) {
      for (contamination in c("none", "vertical")) {
        x <- dgp_group[
          dgp_group$random_slope_sd == random_slope_sd &
            dgp_group$contamination == contamination,
          ,
          drop = FALSE
        ]

        x <- x[order(x$beta), , drop = FALSE]

        if (nrow(x) == 2L) {
          beta_shift_differences <- c(
            beta_shift_differences,
            abs(
              (x$realized_mean_slope[2L] -
                 x$realized_mean_slope[1L]) -
                0.10
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

    slope_mean_rescaling_differences <- numeric(0)
    slope_sd_rescaling_differences <- numeric(0)

    for (beta in c(0, 0.10)) {
      for (contamination in c("none", "vertical")) {
        x <- dgp_group[
          dgp_group$beta == beta &
            dgp_group$contamination == contamination,
          ,
          drop = FALSE
        ]

        x <- x[order(x$random_slope_sd), , drop = FALSE]

        if (nrow(x) == 2L) {
          slope_mean_rescaling_differences <- c(
            slope_mean_rescaling_differences,
            abs(
              (x$realized_mean_slope[2L] - beta) -
                2 * (x$realized_mean_slope[1L] - beta)
            )
          )
          slope_sd_rescaling_differences <- c(
            slope_sd_rescaling_differences,
            abs(
              x$realized_random_slope_sd[2L] -
                2 * x$realized_random_slope_sd[1L]
            )
          )
        } else {
          slope_mean_rescaling_differences <- c(
            slope_mean_rescaling_differences,
            Inf
          )
          slope_sd_rescaling_differences <- c(
            slope_sd_rescaling_differences,
            Inf
          )
        }
      }
    }

    unique_seed_count <- length(unique(
      seed_group$replicate_seed
    ))
    condition_count <- nrow(dgp_group)
    max_contamination_difference <- max(
      contamination_ranges,
      na.rm = TRUE
    )
    max_beta_shift_difference <- max(
      beta_shift_differences,
      na.rm = TRUE
    )
    max_slope_mean_rescaling_difference <- max(
      slope_mean_rescaling_differences,
      na.rm = TRUE
    )
    max_slope_sd_rescaling_difference <- max(
      slope_sd_rescaling_differences,
      na.rm = TRUE
    )

    data.frame(
      n_clusters = n_clusters,
      replicate = replicate_id,
      condition_count = condition_count,
      unique_seed_count = unique_seed_count,
      max_contamination_difference =
        max_contamination_difference,
      max_beta_shift_difference =
        max_beta_shift_difference,
      max_slope_mean_rescaling_difference =
        max_slope_mean_rescaling_difference,
      max_slope_sd_rescaling_difference =
        max_slope_sd_rescaling_difference,
      passed = condition_count == 8L &&
        unique_seed_count == 1L &&
        max_contamination_difference <= tolerance &&
        max_beta_shift_difference <= tolerance &&
        max_slope_mean_rescaling_difference <= tolerance &&
        max_slope_sd_rescaling_difference <= tolerance,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

make_runtime_projection <- function(status,
                                    pilot_reps,
                                    final_reps) {
  complete_status <- status[
    status$status == "complete",
    ,
    drop = FALSE
  ]

  projection_factor <- final_reps / pilot_reps

  condition_projection <- data.frame(
    condition_id = complete_status$condition_id,
    n_clusters = complete_status$n_clusters,
    beta = complete_status$beta,
    random_slope_sd = complete_status$random_slope_sd,
    contamination = complete_status$contamination,
    pilot_reps = pilot_reps,
    pilot_elapsed_sec = complete_status$elapsed_sec,
    mean_elapsed_sec_per_rep =
      complete_status$elapsed_sec / pilot_reps,
    final_reps = final_reps,
    projected_final_elapsed_sec =
      complete_status$elapsed_sec * projection_factor,
    projected_final_elapsed_min =
      complete_status$elapsed_sec *
        projection_factor / 60,
    projected_final_elapsed_hours =
      complete_status$elapsed_sec *
        projection_factor / 3600,
    stringsAsFactors = FALSE
  )

  by_cluster <- do.call(
    rbind,
    lapply(
      split(condition_projection, condition_projection$n_clusters),
      function(x) {
        data.frame(
          n_clusters = x$n_clusters[1L],
          completed_pilot_conditions = nrow(x),
          mean_pilot_sec_per_condition =
            mean(x$pilot_elapsed_sec),
          projected_final_hours =
            sum(x$projected_final_elapsed_hours),
          stringsAsFactors = FALSE
        )
      }
    )
  )
  rownames(by_cluster) <- NULL

  total_projection <- data.frame(
    completed_pilot_conditions = nrow(condition_projection),
    total_pilot_elapsed_min =
      sum(condition_projection$pilot_elapsed_sec) / 60,
    projected_final_elapsed_hours =
      sum(condition_projection$projected_final_elapsed_hours),
    projected_final_elapsed_days =
      sum(condition_projection$projected_final_elapsed_hours) / 24,
    stringsAsFactors = FALSE
  )

  list(
    condition = condition_projection,
    by_cluster = by_cluster,
    total = total_projection
  )
}

make_message_frequency <- function(replicates) {
  message_rows <- list()
  row_index <- 0L

  for (message_type in c(
    "warning",
    "error",
    "template_warning"
  )) {
    if (!(message_type %in% names(replicates))) {
      next
    }

    values <- replicates[[message_type]]
    keep <- has_text(values)

    if (!any(keep)) {
      next
    }

    x <- replicates[
      keep,
      c("condition_id", "method"),
      drop = FALSE
    ]
    x$message_type <- message_type
    x$message <- values[keep]

    counts <- stats::aggregate(
      rep(1L, nrow(x)),
      by = x,
      FUN = sum
    )
    names(counts)[names(counts) == "x"] <- "frequency"

    row_index <- row_index + 1L
    message_rows[[row_index]] <- counts
  }

  if (length(message_rows) == 0L) {
    return(data.frame(
      condition_id = character(0),
      method = character(0),
      message_type = character(0),
      message = character(0),
      frequency = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, message_rows)
  out <- out[
    order(
      out$condition_id,
      out$method,
      out$message_type,
      -out$frequency
    ),
    ,
    drop = FALSE
  ]
  rownames(out) <- NULL
  out
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
      "The pbkrtest package is required because the pilot includes",
      "random-slope and random-intercept models with Kenward-Roger",
      "inference."
    ),
    call. = FALSE
  )
}

pkgload::load_all(project_root, quiet = TRUE)

# -------------------------------------------------------------------------
# Pilot configuration
# -------------------------------------------------------------------------

pilot_reps <- 5L
final_reps <- 2000L
overwrite_completed <- FALSE

# Set to a character vector such as c("S2P001", "S2P002") to run only a
# subset. NULL runs every condition that does not already have a complete
# checkpoint.
condition_ids_to_run <- NULL

methods <- c(
  "rs",
  "ri",
  "cr2",
  "cats",
  "cats_trunc",
  "cats_robust",
  "cats_robustbase"
)

# These seeds are reserved for the Study 2 plumbing pilot. A distinct seed
# base should be used for the final simulation.
cluster_seeds <- c(
  "10" = 20260825L,
  "20" = 20260826L,
  "40" = 20260827L
)

output_dir <- file.path(
  project_root,
  "data-raw",
  "study2-results",
  "plumbing-pilot"
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

pilot_design <- make_study2_pilot_design(
  pilot_reps = pilot_reps,
  cluster_seeds = cluster_seeds,
  methods = methods
)

if (!is.null(condition_ids_to_run)) {
  unknown_condition_ids <- setdiff(
    condition_ids_to_run,
    pilot_design$condition_id
  )

  if (length(unknown_condition_ids) > 0L) {
    stop(
      paste(
        "Unknown condition IDs:",
        paste(unknown_condition_ids, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

design_path <- file.path(
  output_dir,
  "study2_pilot_design.rds"
)

if (file.exists(design_path)) {
  existing_design <- readRDS(design_path)

  if (!identical(existing_design, pilot_design)) {
    if (!overwrite_completed) {
      stop(
        paste(
          "The saved Study 2 pilot design differs from the current",
          "design. Set overwrite_completed <- TRUE to replace prior",
          "pilot checkpoints."
        ),
        call. = FALSE
      )
    }

    old_checkpoints <- list.files(
      checkpoint_dir,
      pattern = "^condition_S2P[0-9]{3}[.]rds$",
      full.names = TRUE
    )

    if (length(old_checkpoints) > 0L) {
      file.remove(old_checkpoints)
    }
  }
}

save_rds_atomic(pilot_design, design_path)
write_csv_atomic(
  pilot_design,
  file.path(output_dir, "study2_pilot_design.csv")
)

package_description <- read.dcf(
  file.path(project_root, "DESCRIPTION")
)

metadata <- list(
  study = "mmiCATs Study 2 plumbing pilot",
  purpose = paste(
    "Validate all 24 frozen Study 2 conditions, checkpointing,",
    "common random numbers, schemas, diagnostics, and runtime.",
    "Pilot performance estimates are not used for calibration."
  ),
  created_at = Sys.time(),
  project_root = project_root,
  package_version = unname(
    package_description[1L, "Version"]
  ),
  r_version = R.version.string,
  pilot_reps = pilot_reps,
  final_reps = final_reps,
  methods = methods,
  cluster_seeds = cluster_seeds,
  common_random_numbers = TRUE,
  common_random_number_scope =
    "All eight conditions within each cluster-count block.",
  overwrite_completed = overwrite_completed,
  condition_ids_to_run = condition_ids_to_run,
  session_info = utils::sessionInfo()
)

save_rds_atomic(
  metadata,
  file.path(output_dir, "study2_pilot_metadata.rds")
)

writeLines(
  capture.output(utils::sessionInfo()),
  con = file.path(output_dir, "session_info.txt"),
  useBytes = TRUE
)

# -------------------------------------------------------------------------
# Run each condition
# -------------------------------------------------------------------------

pilot_started_at <- Sys.time()

for (condition_index in seq_len(nrow(pilot_design))) {
  condition <- pilot_design[
    condition_index,
    ,
    drop = FALSE
  ]

  if (!is.null(condition_ids_to_run) &&
      !(condition$condition_id %in% condition_ids_to_run)) {
    next
  }

  checkpoint_path <- checkpoint_path_for(
    checkpoint_dir = checkpoint_dir,
    condition_id = condition$condition_id
  )

  if (file.exists(checkpoint_path) && !overwrite_completed) {
    existing_checkpoint <- read_checkpoint_safely(
      checkpoint_path
    )

    if (!inherits(
      existing_checkpoint,
      "study2_unreadable_checkpoint"
    ) &&
        identical(existing_checkpoint$status, "complete")) {
      message(
        sprintf(
          "Skipping completed condition %s.",
          condition$condition_id
        )
      )
      next
    }
  }

  message(
    sprintf(
      paste0(
        "Running %s of %s: G = %s, beta = %s, ",
        "random-slope SD = %s, condition = %s, reps = %s."
      ),
      condition$condition_id,
      nrow(pilot_design),
      condition$n_clusters,
      format(condition$beta, trim = TRUE),
      format(condition$random_slope_sd, trim = TRUE),
      condition$contamination_label,
      condition$reps
    )
  )

  started_at <- Sys.time()

  simulation_result <- tryCatch(
    mmiCATs::pwr_func_study2(
      n_clusters = condition$n_clusters,
      cluster_size = condition$cluster_size,
      beta = condition$beta,
      intercept = condition$intercept,
      random_intercept_sd =
        condition$random_intercept_sd,
      random_slope_sd = condition$random_slope_sd,
      residual_sd = condition$residual_sd,
      x_sd = condition$x_sd,
      contamination = condition$contamination,
      contamination_prop =
        condition$contamination_prop,
      contamination_size =
        condition$contamination_size,
      reps = condition$reps,
      alpha = condition$alpha,
      methods = methods,
      seed = condition$condition_seed,
      keep_replicates = TRUE
    ),
    error = function(e) e
  )

  completed_at <- Sys.time()
  elapsed_sec <- as.numeric(
    difftime(
      completed_at,
      started_at,
      units = "secs"
    )
  )

  if (inherits(simulation_result, "error")) {
    checkpoint <- list(
      status = "error",
      condition = condition,
      result = NULL,
      error = conditionMessage(simulation_result),
      started_at = started_at,
      completed_at = completed_at,
      elapsed_sec = elapsed_sec
    )

    save_rds_atomic(checkpoint, checkpoint_path)
    save_status_outputs(
      checkpoint_dir = checkpoint_dir,
      output_dir = output_dir
    )

    message(
      sprintf(
        "Condition %s failed: %s",
        condition$condition_id,
        conditionMessage(simulation_result)
      )
    )

    next
  }

  simulation_result$summary <- add_condition_columns(
    simulation_result$summary,
    condition
  )

  simulation_result$replicates <- add_condition_columns(
    simulation_result$replicates,
    condition
  )

  checkpoint <- list(
    status = "complete",
    condition = condition,
    result = simulation_result,
    error = NA_character_,
    started_at = started_at,
    completed_at = completed_at,
    elapsed_sec = elapsed_sec
  )

  save_rds_atomic(checkpoint, checkpoint_path)
  current_status <- save_status_outputs(
    checkpoint_dir = checkpoint_dir,
    output_dir = output_dir
  )

  completed_count <- sum(
    current_status$status == "complete",
    na.rm = TRUE
  )
  elapsed_total_hours <- as.numeric(
    difftime(
      Sys.time(),
      pilot_started_at,
      units = "hours"
    )
  )

  message(
    sprintf(
      paste0(
        "Completed %s in %.2f seconds. Overall progress: ",
        "%s of %s conditions; %.2f elapsed hours."
      ),
      condition$condition_id,
      elapsed_sec,
      completed_count,
      nrow(pilot_design),
      elapsed_total_hours
    )
  )
}

# -------------------------------------------------------------------------
# Combine checkpoints
# -------------------------------------------------------------------------

checkpoints <- collect_checkpoints(checkpoint_dir)
pilot_status <- collect_status(checkpoints)

if (nrow(pilot_status) == 0L) {
  stop(
    "No Study 2 pilot checkpoints were found.",
    call. = FALSE
  )
}

complete_checkpoints <- checkpoints[
  vapply(
    checkpoints,
    function(checkpoint) {
      identical(checkpoint$status, "complete")
    },
    logical(1)
  )
]

if (length(complete_checkpoints) == 0L) {
  stop(
    "No Study 2 pilot conditions completed successfully.",
    call. = FALSE
  )
}

pilot_summary <- do.call(
  rbind,
  lapply(
    complete_checkpoints,
    function(checkpoint) checkpoint$result$summary
  )
)

pilot_replicates <- do.call(
  rbind,
  lapply(
    complete_checkpoints,
    function(checkpoint) checkpoint$result$replicates
  )
)

rownames(pilot_summary) <- NULL
rownames(pilot_replicates) <- NULL

pilot_diagnostics <- condition_diagnostics(
  pilot_replicates
)

pilot_dgp_diagnostics <- condition_dgp_diagnostics(
  pilot_replicates
)

schema_validation <- make_schema_validation(
  checkpoints = checkpoints,
  methods = methods
)

crn_audit <- make_crn_audit(
  checkpoints = checkpoints,
  replicates = pilot_replicates
)

runtime_projection <- make_runtime_projection(
  status = pilot_status,
  pilot_reps = pilot_reps,
  final_reps = final_reps
)

message_frequency <- make_message_frequency(
  pilot_replicates
)

pilot_results <- list(
  design = pilot_design,
  status = pilot_status,
  summary = pilot_summary,
  replicates = pilot_replicates,
  diagnostics = pilot_diagnostics,
  dgp_diagnostics = pilot_dgp_diagnostics,
  schema_validation = schema_validation,
  common_random_number_audit = crn_audit,
  runtime_projection = runtime_projection,
  message_frequency = message_frequency,
  metadata = metadata
)

# -------------------------------------------------------------------------
# Save combined outputs
# -------------------------------------------------------------------------

save_rds_atomic(
  pilot_status,
  file.path(output_dir, "study2_pilot_status.rds")
)
save_rds_atomic(
  pilot_summary,
  file.path(output_dir, "study2_pilot_summary.rds")
)
save_rds_atomic(
  pilot_replicates,
  file.path(output_dir, "study2_pilot_replicates.rds")
)
save_rds_atomic(
  pilot_diagnostics,
  file.path(output_dir, "study2_pilot_diagnostics.rds")
)
save_rds_atomic(
  pilot_dgp_diagnostics,
  file.path(
    output_dir,
    "study2_pilot_dgp_diagnostics.rds"
  )
)
save_rds_atomic(
  schema_validation,
  file.path(
    output_dir,
    "study2_pilot_schema_validation.rds"
  )
)
save_rds_atomic(
  crn_audit,
  file.path(output_dir, "study2_pilot_crn_audit.rds")
)
save_rds_atomic(
  runtime_projection,
  file.path(
    output_dir,
    "study2_pilot_runtime_projection.rds"
  )
)
save_rds_atomic(
  message_frequency,
  file.path(
    output_dir,
    "study2_pilot_message_frequency.rds"
  )
)
save_rds_atomic(
  pilot_results,
  file.path(output_dir, "study2_plumbing_pilot.rds")
)

write_csv_atomic(
  pilot_status,
  file.path(output_dir, "study2_pilot_status.csv")
)
write_csv_atomic(
  pilot_summary,
  file.path(output_dir, "study2_pilot_summary.csv")
)
write_csv_atomic(
  pilot_diagnostics,
  file.path(output_dir, "study2_pilot_diagnostics.csv")
)
write_csv_atomic(
  pilot_dgp_diagnostics,
  file.path(
    output_dir,
    "study2_pilot_dgp_diagnostics.csv"
  )
)
write_csv_atomic(
  schema_validation,
  file.path(
    output_dir,
    "study2_pilot_schema_validation.csv"
  )
)
write_csv_atomic(
  crn_audit,
  file.path(output_dir, "study2_pilot_crn_audit.csv")
)
write_csv_atomic(
  runtime_projection$condition,
  file.path(
    output_dir,
    "study2_pilot_runtime_projection_by_condition.csv"
  )
)
write_csv_atomic(
  runtime_projection$by_cluster,
  file.path(
    output_dir,
    "study2_pilot_runtime_projection_by_clusters.csv"
  )
)
write_csv_atomic(
  runtime_projection$total,
  file.path(
    output_dir,
    "study2_pilot_runtime_projection_total.csv"
  )
)
write_csv_atomic(
  message_frequency,
  file.path(
    output_dir,
    "study2_pilot_message_frequency.csv"
  )
)

# -------------------------------------------------------------------------
# Pilot validation decision
# -------------------------------------------------------------------------

all_conditions_completed <-
  sum(pilot_status$status == "complete") ==
    nrow(pilot_design)

all_schema_checks_passed <-
  nrow(schema_validation) == nrow(pilot_design) &&
    all(schema_validation$all_checks_passed)

all_crn_checks_passed <-
  nrow(crn_audit) ==
    length(cluster_seeds) * pilot_reps &&
    all(crn_audit$passed)

expected_summary_rows <-
  nrow(pilot_design) * length(methods)

expected_replicate_rows <-
  nrow(pilot_design) *
    pilot_reps *
    length(methods)

combined_dimensions_correct <-
  nrow(pilot_summary) == expected_summary_rows &&
    nrow(pilot_replicates) == expected_replicate_rows

pilot_validation <- data.frame(
  check = c(
    "all_conditions_completed",
    "all_schema_checks_passed",
    "all_crn_checks_passed",
    "combined_dimensions_correct"
  ),
  passed = c(
    all_conditions_completed,
    all_schema_checks_passed,
    all_crn_checks_passed,
    combined_dimensions_correct
  ),
  details = c(
    sprintf(
      "%s of %s conditions complete",
      sum(pilot_status$status == "complete"),
      nrow(pilot_design)
    ),
    sprintf(
      "%s of %s checkpoint schemas passed",
      sum(schema_validation$all_checks_passed),
      nrow(schema_validation)
    ),
    sprintf(
      "%s of %s cluster-by-replication CRN checks passed",
      sum(crn_audit$passed),
      nrow(crn_audit)
    ),
    sprintf(
      "%s summary rows and %s replicate rows; expected %s and %s",
      nrow(pilot_summary),
      nrow(pilot_replicates),
      expected_summary_rows,
      expected_replicate_rows
    )
  ),
  stringsAsFactors = FALSE
)

save_rds_atomic(
  pilot_validation,
  file.path(output_dir, "study2_pilot_validation.rds")
)
write_csv_atomic(
  pilot_validation,
  file.path(output_dir, "study2_pilot_validation.csv")
)

# -------------------------------------------------------------------------
# Console summary
# -------------------------------------------------------------------------

message("")
message("Study 2 plumbing pilot processing complete.")
message(
  sprintf(
    "Completed conditions: %s of %s.",
    sum(pilot_status$status == "complete"),
    nrow(pilot_design)
  )
)
message(
  sprintf(
    "Total elapsed time across completed conditions: %.2f minutes.",
    sum(
      pilot_status$elapsed_sec[
        pilot_status$status == "complete"
      ],
      na.rm = TRUE
    ) / 60
  )
)
message(paste("Results saved to:", output_dir))

message("")
message("Pilot validation checks:")
print(pilot_validation, row.names = FALSE)

message("")
message("Projected final Study 2 runtime:")
print(runtime_projection$total, row.names = FALSE)

message("")
message("Projected runtime by cluster count:")
print(runtime_projection$by_cluster, row.names = FALSE)

diagnostic_problems <- pilot_diagnostics[
  pilot_diagnostics$failure_rate > 0 |
    pilot_diagnostics$warning_rep_rate > 0 |
    pilot_diagnostics$error_rep_rate > 0 |
    (
      !is.na(pilot_diagnostics$convergence_failure_rate) &
        pilot_diagnostics$convergence_failure_rate > 0
    ) |
    (
      !is.na(pilot_diagnostics$singular_rate) &
        pilot_diagnostics$singular_rate > 0
    ) |
    (
      !is.na(pilot_diagnostics$cluster_warning_rep_rate) &
        pilot_diagnostics$cluster_warning_rep_rate > 0
    ) |
    (
      !is.na(pilot_diagnostics$cluster_error_rep_rate) &
        pilot_diagnostics$cluster_error_rep_rate > 0
    ) |
    (
      !is.na(pilot_diagnostics$dropped_cluster_rep_rate) &
        pilot_diagnostics$dropped_cluster_rep_rate > 0
    ),
  ,
  drop = FALSE
]

message("")

if (nrow(diagnostic_problems) == 0L) {
  message(
    paste(
      "No failures, warnings, errors, convergence problems,",
      "singular fits, or dropped clusters were detected."
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
        "failure_rate",
        "warning_rep_rate",
        "error_rep_rate",
        "convergence_failure_rate",
        "singular_rate",
        "cluster_warning_rep_rate",
        "cluster_error_rep_rate",
        "dropped_cluster_rep_rate",
        "minimum_retained_clusters",
        "mean_runtime_sec"
      )
    ],
    row.names = FALSE
  )
}

message("")

if (all(pilot_validation$passed)) {
  message(
    paste(
      "All Study 2 plumbing checks passed.",
      "The pilot is ready for diagnostic review."
    )
  )
} else {
  message(
    paste(
      "One or more Study 2 plumbing checks failed.",
      "Review the saved validation, schema, status, and CRN files",
      "before proceeding."
    )
  )
}
