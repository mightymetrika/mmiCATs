# Study 1 bad-leverage follow-up calibration
#
# This script evaluates three milder bad-leverage conditions after the initial
# calibration showed that an outcome displacement of 1 residual standard
# deviation was too severe.
#
# All six Study 1 methods are retained. Truncated CATs remain in the design as
# a negative-control method.

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

save_rds_atomic <- function(object, path) {
  temp_path <- tempfile(
    pattern = "study1_",
    tmpdir = dirname(path),
    fileext = ".rds"
  )

  saveRDS(object, temp_path, version = 3)

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

add_condition_columns <- function(x, condition) {
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

column_or_default <- function(data, name, default) {
  if (name %in% names(data)) {
    return(data[[name]])
  }

  rep(default, nrow(data))
}

has_text <- function(x) {
  !is.na(x) & nzchar(x)
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

      observed_singular <- singular_values[!is.na(singular_values)]

      condition_columns <- condition_results[
        1L,
        c(
          "condition_id",
          "candidate_id",
          "n_clusters",
          "cluster_size",
          "beta",
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
        warning_rep_rate = 100 * mean(has_text(warning_values)),
        error_rep_rate = 100 * mean(has_text(error_values)),
        failure_rate = 100 * mean(!method_results$fit_success),
        singular_rate = if (length(observed_singular) == 0L) {
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
        mean_cluster_warning_count = mean_or_na(
          cluster_warning_count
        ),
        mean_cluster_error_count = mean_or_na(
          cluster_error_count
        ),
        mean_dropped_cluster_count = mean_or_na(
          dropped_cluster_count
        ),
        template_warning_rate = 100 * mean(
          has_text(template_warning)
        ),
        mean_retained_clusters = mean_or_na(
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
  out
}

make_selection_aid <- function(summary_results) {
  nonrobust_methods <- c("ri", "cr2", "cats")

  nonrobust_results <- summary_results[
    summary_results$model %in% nonrobust_methods,
    ,
    drop = FALSE
  ]

  split_results <- split(
    nonrobust_results,
    nonrobust_results$candidate_id
  )

  rows <- lapply(split_results, function(x) {
    data.frame(
      condition_id = x$condition_id[1L],
      candidate_id = x$candidate_id[1L],
      leverage_size = x$leverage_size[1L],
      contamination_size = x$contamination_size[1L],
      average_nonrobust_type1_error = mean_or_na(
        x$rejection_rate
      ),
      minimum_nonrobust_type1_error = min_or_na(
        x$rejection_rate
      ),
      maximum_nonrobust_type1_error = max_or_na(
        x$rejection_rate
      ),
      average_nonrobust_coverage = mean_or_na(x$coverage),
      minimum_nonrobust_coverage = min_or_na(x$coverage),
      average_nonrobust_bias = mean_or_na(x$bias),
      average_nonrobust_absolute_bias = mean_or_na(
        abs(x$bias)
      ),
      average_nonrobust_rmse = mean_or_na(x$rmse),
      maximum_nonrobust_failure_rate = max_or_na(
        x$failure_rate
      ),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  out$distance_from_30 <- abs(
    out$average_nonrobust_type1_error - 30
  )
  out$stable <- out$maximum_nonrobust_failure_rate < 1
  out$within_type1_ceiling <-
    out$average_nonrobust_type1_error <= 50
  out$eligible <- out$stable & out$within_type1_ceiling
  out$recommended <- FALSE

  eligible_rows <- which(out$eligible)

  if (length(eligible_rows) > 0L) {
    eligible_order <- eligible_rows[
      order(
        out$distance_from_30[eligible_rows],
        out$contamination_size[eligible_rows]
      )
    ]

    out$recommended[eligible_order[1L]] <- TRUE
  }

  out <- out[
    order(out$contamination_size),
    ,
    drop = FALSE
  ]
  rownames(out) <- NULL
  out
}

make_negative_control_comparison <- function(replicates) {
  keys <- c("condition_id", "candidate_id", "replicate")
  values <- c(
    "estimate",
    "p_value",
    "reject",
    "cover",
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
  names(cats_trunc)[names(cats_trunc) %in% values] <- paste0(
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
    retained_difference <- x$retained_clusters_cats_trunc -
      x$retained_clusters_cats

    data.frame(
      condition_id = condition_id,
      candidate_id = x$candidate_id[1L],
      reps = nrow(x),
      estimate_exact_match_rate = 100 * mean(
        x$estimate_cats_trunc == x$estimate_cats
      ),
      maximum_absolute_estimate_difference = max(
        abs(estimate_difference),
        na.rm = TRUE
      ),
      rejection_exact_match_rate = 100 * mean(
        x$reject_cats_trunc == x$reject_cats
      ),
      coverage_exact_match_rate = 100 * mean(
        x$cover_cats_trunc == x$cover_cats
      ),
      maximum_absolute_p_value_difference = max(
        abs(p_value_difference),
        na.rm = TRUE
      ),
      mean_retained_cluster_difference = mean(
        retained_difference,
        na.rm = TRUE
      ),
      minimum_truncated_retained_clusters = min(
        x$retained_clusters_cats_trunc,
        na.rm = TRUE
      ),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
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
      "The pbkrtest package is required because the calibration includes",
      "the random-intercept method with Kenward-Roger inference."
    ),
    call. = FALSE
  )
}

pkgload::load_all(project_root, quiet = TRUE)

# -------------------------------------------------------------------------
# Follow-up configuration
# -------------------------------------------------------------------------

followup_reps <- 250L
common_seed <- 20260802L
overwrite_completed <- FALSE

methods <- c(
  "ri",
  "cr2",
  "cats",
  "cats_trunc",
  "cats_robust",
  "cats_robustbase"
)

output_dir <- file.path(
  project_root,
  "data-raw",
  "study1-results",
  "bad-leverage-followup"
)

checkpoint_dir <- file.path(output_dir, "conditions")

dir.create(
  checkpoint_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

followup_design <- data.frame(
  condition_id = sprintf("BLF%03d", 1:3),
  candidate_id = sprintf("BL%02d", 1:3),
  n_clusters = 20L,
  cluster_size = 40L,
  beta = 0,
  intercept = 0,
  random_intercept_sd = 1,
  residual_sd = 1,
  x_sd = 1,
  contamination = "bad_leverage",
  contamination_prop = 0.05,
  contamination_size = c(0.25, 0.50, 0.75),
  leverage_size = 4,
  reps = followup_reps,
  alpha = 0.05,
  method_set = paste(methods, collapse = ","),
  condition_seed = common_seed,
  stringsAsFactors = FALSE
)

design_path <- file.path(
  output_dir,
  "bad_leverage_followup_design.rds"
)

if (file.exists(design_path)) {
  existing_design <- readRDS(design_path)

  if (!identical(existing_design, followup_design)) {
    if (!overwrite_completed) {
      stop(
        paste(
          "The saved bad-leverage follow-up design differs from the",
          "current design. Set overwrite_completed <- TRUE to replace",
          "prior results."
        ),
        call. = FALSE
      )
    }

    old_checkpoints <- list.files(
      checkpoint_dir,
      pattern = "^condition_BLF[0-9]{3}[.]rds$",
      full.names = TRUE
    )

    if (length(old_checkpoints) > 0L) {
      file.remove(old_checkpoints)
    }
  }
}

save_rds_atomic(followup_design, design_path)

package_description <- read.dcf(
  file.path(project_root, "DESCRIPTION")
)

metadata <- list(
  study = "mmiCATs Study 1 bad-leverage follow-up calibration",
  created_at = Sys.time(),
  project_root = project_root,
  package_version = unname(package_description[1L, "Version"]),
  r_version = R.version.string,
  followup_reps = followup_reps,
  common_seed = common_seed,
  methods = methods,
  common_random_numbers = TRUE,
  selection_rule = paste(
    "Select the stable candidate with average Type I error across",
    "ri, cr2, and cats closest to 30 percent, provided that the",
    "average does not exceed 50 percent."
  ),
  overwrite_completed = overwrite_completed,
  session_info = utils::sessionInfo()
)

save_rds_atomic(
  metadata,
  file.path(output_dir, "bad_leverage_followup_metadata.rds")
)

writeLines(
  capture.output(utils::sessionInfo()),
  con = file.path(output_dir, "session_info.txt"),
  useBytes = TRUE
)

# -------------------------------------------------------------------------
# Run each condition
# -------------------------------------------------------------------------

for (condition_index in seq_len(nrow(followup_design))) {
  condition <- followup_design[
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
        "Running condition %s of %s: leverage size = %s, ",
        "contamination size = %s"
      ),
      condition$condition_id,
      nrow(followup_design),
      format(condition$leverage_size, trim = TRUE),
      format(condition$contamination_size, trim = TRUE)
    )
  )

  started_at <- Sys.time()

  simulation_result <- tryCatch(
    mmiCATs::pwr_func_study1(
      n_clusters = condition$n_clusters,
      cluster_size = condition$cluster_size,
      beta = condition$beta,
      intercept = condition$intercept,
      random_intercept_sd = condition$random_intercept_sd,
      residual_sd = condition$residual_sd,
      x_sd = condition$x_sd,
      contamination = condition$contamination,
      contamination_prop = condition$contamination_prop,
      contamination_size = condition$contamination_size,
      leverage_size = condition$leverage_size,
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
    difftime(completed_at, started_at, units = "secs")
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

  message(
    sprintf(
      "Completed condition %s in %.2f seconds.",
      condition$condition_id,
      elapsed_sec
    )
  )
}

# -------------------------------------------------------------------------
# Combine checkpoints
# -------------------------------------------------------------------------

checkpoint_paths <- list.files(
  checkpoint_dir,
  pattern = "^condition_BLF[0-9]{3}[.]rds$",
  full.names = TRUE
)

checkpoints <- lapply(checkpoint_paths, readRDS)

followup_status <- do.call(
  rbind,
  lapply(checkpoints, function(checkpoint) {
    data.frame(
      checkpoint$condition,
      status = checkpoint$status,
      condition_error = checkpoint$error,
      started_at = as.character(checkpoint$started_at),
      completed_at = as.character(checkpoint$completed_at),
      elapsed_sec = checkpoint$elapsed_sec,
      stringsAsFactors = FALSE
    )
  })
)

rownames(followup_status) <- NULL

complete_checkpoints <- checkpoints[
  vapply(
    checkpoints,
    function(checkpoint) identical(checkpoint$status, "complete"),
    logical(1)
  )
]

if (length(complete_checkpoints) == 0L) {
  stop(
    "No bad-leverage follow-up conditions completed successfully.",
    call. = FALSE
  )
}

followup_summary <- do.call(
  rbind,
  lapply(
    complete_checkpoints,
    function(checkpoint) checkpoint$result$summary
  )
)

followup_replicates <- do.call(
  rbind,
  lapply(
    complete_checkpoints,
    function(checkpoint) checkpoint$result$replicates
  )
)

rownames(followup_summary) <- NULL
rownames(followup_replicates) <- NULL

followup_diagnostics <- summarize_diagnostics(
  followup_replicates
)

selection_aid <- make_selection_aid(
  followup_summary
)

negative_control_comparison <- make_negative_control_comparison(
  followup_replicates
)

followup_results <- list(
  design = followup_design,
  status = followup_status,
  summary = followup_summary,
  replicates = followup_replicates,
  diagnostics = followup_diagnostics,
  selection_aid = selection_aid,
  negative_control_comparison = negative_control_comparison,
  metadata = metadata
)

# -------------------------------------------------------------------------
# Save outputs
# -------------------------------------------------------------------------

save_rds_atomic(
  followup_status,
  file.path(output_dir, "bad_leverage_followup_status.rds")
)
save_rds_atomic(
  followup_summary,
  file.path(output_dir, "bad_leverage_followup_summary.rds")
)
save_rds_atomic(
  followup_replicates,
  file.path(output_dir, "bad_leverage_followup_replicates.rds")
)
save_rds_atomic(
  followup_diagnostics,
  file.path(output_dir, "bad_leverage_followup_diagnostics.rds")
)
save_rds_atomic(
  selection_aid,
  file.path(output_dir, "bad_leverage_followup_selection.rds")
)
save_rds_atomic(
  negative_control_comparison,
  file.path(
    output_dir,
    "cats_trunc_negative_control_comparison.rds"
  )
)
save_rds_atomic(
  followup_results,
  file.path(output_dir, "study1_bad_leverage_followup.rds")
)

utils::write.csv(
  followup_design,
  file.path(output_dir, "bad_leverage_followup_design.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  followup_status,
  file.path(output_dir, "bad_leverage_followup_status.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  followup_summary,
  file.path(output_dir, "bad_leverage_followup_summary.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  followup_diagnostics,
  file.path(output_dir, "bad_leverage_followup_diagnostics.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  selection_aid,
  file.path(output_dir, "bad_leverage_followup_selection.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  negative_control_comparison,
  file.path(
    output_dir,
    "cats_trunc_negative_control_comparison.csv"
  ),
  row.names = FALSE,
  na = ""
)

# -------------------------------------------------------------------------
# Console summary
# -------------------------------------------------------------------------

message("")
message("Study 1 bad-leverage follow-up calibration complete.")
message(
  sprintf(
    "Completed conditions: %s of %s.",
    sum(followup_status$status == "complete"),
    nrow(followup_design)
  )
)
message(
  sprintf(
    "Total elapsed time across saved conditions: %.2f minutes.",
    sum(
      followup_status$elapsed_sec[
        followup_status$status == "complete"
      ],
      na.rm = TRUE
    ) / 60
  )
)
message(paste("Results saved to:", output_dir))

message("")
message("Bad-leverage selection aid:")
print(selection_aid, row.names = FALSE)

message("")
message("Truncated CATs negative-control comparison:")
print(negative_control_comparison, row.names = FALSE)

diagnostic_problems <- followup_diagnostics[
  followup_diagnostics$warning_rep_rate > 0 |
    followup_diagnostics$error_rep_rate > 0 |
    followup_diagnostics$failure_rate > 0 |
    (
      !is.na(followup_diagnostics$singular_rate) &
        followup_diagnostics$singular_rate > 0
    ) |
    (
      !is.na(followup_diagnostics$cluster_warning_rep_rate) &
        followup_diagnostics$cluster_warning_rep_rate > 0
    ) |
    (
      !is.na(followup_diagnostics$cluster_error_rep_rate) &
        followup_diagnostics$cluster_error_rep_rate > 0
    ) |
    (
      !is.na(followup_diagnostics$dropped_cluster_rep_rate) &
        followup_diagnostics$dropped_cluster_rep_rate > 0
    ),
  ,
  drop = FALSE
]

message("")

if (nrow(diagnostic_problems) == 0L) {
  message(
    paste(
      "No warnings, errors, failures, singular fits,",
      "or dropped clusters were detected."
    )
  )
} else {
  message("Diagnostic issues detected:")
  print(
    diagnostic_problems[
      ,
      c(
        "condition_id",
        "model",
        "warning_rep_rate",
        "error_rep_rate",
        "failure_rate",
        "singular_rate",
        "cluster_warning_rep_rate",
        "cluster_error_rep_rate",
        "dropped_cluster_rep_rate",
        "mean_retained_clusters",
        "mean_runtime_sec"
      )
    ],
    row.names = FALSE
  )
}
