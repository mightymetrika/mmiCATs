# Study 1 bad-leverage midpoint refinement
#
# This script evaluates one midpoint candidate after the first follow-up
# calibration bracketed the target Type I error:
#   contamination size 0.25 produced an average nonrobust Type I error below
#   30 percent, while contamination size 0.50 produced an average above
#   30 percent.
#
# The midpoint candidate uses:
#   - leverage size = 4 predictor standard deviations;
#   - outcome contamination size = 0.375 residual standard deviations;
#   - contamination proportion = 0.05 within every cluster;
#   - 20 clusters with 40 observations per cluster;
#   - beta = 0;
#   - 250 replications; and
#   - all six Study 1 methods.
#
# Truncated CATs remains in the design as a negative control. This is the final
# bad-leverage calibration refinement.

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

has_text <- function(x) {
  !is.na(x) & nzchar(x)
}

summarize_refinement_diagnostics <- function(replicates) {
  methods <- unique(replicates$method)

  rows <- lapply(methods, function(method) {
    x <- replicates[
      replicates$method == method,
      ,
      drop = FALSE
    ]

    cluster_warning_count <- if (
      "cluster_warning_count" %in% names(x)
    ) {
      x$cluster_warning_count
    } else {
      rep(NA_real_, nrow(x))
    }

    cluster_error_count <- if (
      "cluster_error_count" %in% names(x)
    ) {
      x$cluster_error_count
    } else {
      rep(NA_real_, nrow(x))
    }

    dropped_cluster_count <- if (
      "dropped_cluster_count" %in% names(x)
    ) {
      x$dropped_cluster_count
    } else {
      rep(NA_real_, nrow(x))
    }

    template_warning <- if (
      "template_warning" %in% names(x)
    ) {
      x$template_warning
    } else {
      rep(NA_character_, nrow(x))
    }

    error_values <- if ("error" %in% names(x)) {
      x$error
    } else {
      rep(NA_character_, nrow(x))
    }

    singular_values <- if ("singular" %in% names(x)) {
      x$singular
    } else {
      rep(NA, nrow(x))
    }

    observed_singular <- singular_values[
      !is.na(singular_values)
    ]

    data.frame(
      condition_id = x$condition_id[1L],
      candidate_id = x$candidate_id[1L],
      model = method,
      reps = nrow(x),
      fit_success_rate = 100 * mean(x$fit_success),
      failure_rate = 100 * mean(!x$fit_success),
      error_rep_rate = 100 * mean(has_text(error_values)),
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
      maximum_cluster_warning_count = if (
        all(is.na(cluster_warning_count))
      ) {
        NA_real_
      } else {
        max(cluster_warning_count, na.rm = TRUE)
      },
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
        x$retained_clusters
      ),
      mean_runtime_sec = mean_or_na(x$runtime_sec),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

make_selection_table <- function(summary_results) {
  nonrobust_methods <- c("ri", "cr2", "cats")

  x <- summary_results[
    summary_results$model %in% nonrobust_methods,
    ,
    drop = FALSE
  ]

  candidate_keys <- unique(
    x[
      ,
      c(
        "condition_id",
        "candidate_id",
        "contamination_size"
      ),
      drop = FALSE
    ]
  )

  rows <- lapply(
    seq_len(nrow(candidate_keys)),
    function(i) {
      key <- candidate_keys[i, , drop = FALSE]

      candidate_results <- x[
        x$condition_id == key$condition_id &
          x$candidate_id == key$candidate_id,
        ,
        drop = FALSE
      ]

      data.frame(
        condition_id = key$condition_id,
        candidate_id = key$candidate_id,
        leverage_size = candidate_results$leverage_size[1L],
        contamination_size = key$contamination_size,
        average_nonrobust_type1_error = mean_or_na(
          candidate_results$rejection_rate
        ),
        minimum_nonrobust_type1_error = min_or_na(
          candidate_results$rejection_rate
        ),
        maximum_nonrobust_type1_error = max_or_na(
          candidate_results$rejection_rate
        ),
        average_nonrobust_coverage = mean_or_na(
          candidate_results$coverage
        ),
        minimum_nonrobust_coverage = min_or_na(
          candidate_results$coverage
        ),
        average_nonrobust_bias = mean_or_na(
          candidate_results$bias
        ),
        average_nonrobust_absolute_bias = mean_or_na(
          abs(candidate_results$bias)
        ),
        average_nonrobust_rmse = mean_or_na(
          candidate_results$rmse
        ),
        maximum_nonrobust_failure_rate = max_or_na(
          candidate_results$failure_rate
        ),
        stringsAsFactors = FALSE
      )
    }
  )

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
    selected <- eligible_rows[
      order(
        out$distance_from_30[eligible_rows],
        out$contamination_size[eligible_rows]
      )
    ][1L]

    out$recommended[selected] <- TRUE
  }

  out <- out[
    order(out$contamination_size),
    ,
    drop = FALSE
  ]
  rownames(out) <- NULL
  out
}

make_negative_control_check <- function(replicates) {
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

  data.frame(
    condition_id = merged$condition_id[1L],
    candidate_id = merged$candidate_id[1L],
    reps = nrow(merged),
    estimate_exact_match_rate = 100 * mean(
      merged$estimate_cats == merged$estimate_cats_trunc
    ),
    maximum_absolute_estimate_difference = max(
      abs(
        merged$estimate_cats -
          merged$estimate_cats_trunc
      ),
      na.rm = TRUE
    ),
    rejection_exact_match_rate = 100 * mean(
      merged$reject_cats == merged$reject_cats_trunc
    ),
    coverage_exact_match_rate = 100 * mean(
      merged$cover_cats == merged$cover_cats_trunc
    ),
    maximum_absolute_p_value_difference = max(
      abs(
        merged$p_value_cats -
          merged$p_value_cats_trunc
      ),
      na.rm = TRUE
    ),
    mean_retained_cluster_difference = mean(
      merged$retained_clusters_cats_trunc -
        merged$retained_clusters_cats,
      na.rm = TRUE
    ),
    minimum_truncated_retained_clusters = min(
      merged$retained_clusters_cats_trunc,
      na.rm = TRUE
    ),
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
      "The pbkrtest package is required because the refinement includes",
      "the random-intercept method with Kenward-Roger inference."
    ),
    call. = FALSE
  )
}

pkgload::load_all(project_root, quiet = TRUE)

# -------------------------------------------------------------------------
# Refinement configuration
# -------------------------------------------------------------------------

refinement_reps <- 250L
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
  "bad-leverage-refinement"
)

checkpoint_dir <- file.path(output_dir, "conditions")

dir.create(
  checkpoint_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

refinement_design <- data.frame(
  condition_id = "BLR001",
  candidate_id = "BLR01",
  n_clusters = 20L,
  cluster_size = 40L,
  beta = 0,
  intercept = 0,
  random_intercept_sd = 1,
  residual_sd = 1,
  x_sd = 1,
  contamination = "bad_leverage",
  contamination_prop = 0.05,
  contamination_size = 0.375,
  leverage_size = 4,
  reps = refinement_reps,
  alpha = 0.05,
  method_set = paste(methods, collapse = ","),
  condition_seed = common_seed,
  stringsAsFactors = FALSE
)

design_path <- file.path(
  output_dir,
  "bad_leverage_refinement_design.rds"
)

if (file.exists(design_path)) {
  existing_design <- readRDS(design_path)

  if (!identical(existing_design, refinement_design)) {
    if (!overwrite_completed) {
      stop(
        paste(
          "The saved bad-leverage refinement design differs from the",
          "current design. Set overwrite_completed <- TRUE to replace",
          "the prior refinement result."
        ),
        call. = FALSE
      )
    }

    old_checkpoints <- list.files(
      checkpoint_dir,
      pattern = "^condition_BLR[0-9]{3}[.]rds$",
      full.names = TRUE
    )

    if (length(old_checkpoints) > 0L) {
      file.remove(old_checkpoints)
    }
  }
}

save_rds_atomic(refinement_design, design_path)

# -------------------------------------------------------------------------
# Run the midpoint condition
# -------------------------------------------------------------------------

condition <- refinement_design[1L, , drop = FALSE]

checkpoint_path <- file.path(
  checkpoint_dir,
  paste0("condition_", condition$condition_id, ".rds")
)

run_condition <- TRUE

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
    run_condition <- FALSE
  }
}

if (run_condition) {
  message(
    sprintf(
      paste0(
        "Running %s: leverage size = %s, ",
        "contamination size = %s."
      ),
      condition$condition_id,
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

    stop(
      paste(
        "The midpoint refinement failed:",
        conditionMessage(simulation_result)
      ),
      call. = FALSE
    )
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
      "Completed %s in %.2f seconds.",
      condition$condition_id,
      elapsed_sec
    )
  )
}

checkpoint <- readRDS(checkpoint_path)

if (!identical(checkpoint$status, "complete")) {
  stop(
    "The midpoint refinement checkpoint is not complete.",
    call. = FALSE
  )
}

refinement_summary <- checkpoint$result$summary
refinement_replicates <- checkpoint$result$replicates
refinement_diagnostics <- summarize_refinement_diagnostics(
  refinement_replicates
)
negative_control_check <- make_negative_control_check(
  refinement_replicates
)

# -------------------------------------------------------------------------
# Combine with the previous bracketing candidates
# -------------------------------------------------------------------------

followup_dir <- file.path(
  project_root,
  "data-raw",
  "study1-results",
  "bad-leverage-followup"
)

followup_rds <- file.path(
  followup_dir,
  "bad_leverage_followup_summary.rds"
)

followup_csv <- file.path(
  followup_dir,
  "bad_leverage_followup_summary.csv"
)

previous_summary <- NULL

if (file.exists(followup_rds)) {
  previous_summary <- readRDS(followup_rds)
} else if (file.exists(followup_csv)) {
  previous_summary <- utils::read.csv(
    followup_csv,
    stringsAsFactors = FALSE
  )
}

if (is.null(previous_summary)) {
  stop(
    paste(
      "Could not locate the previous bad-leverage follow-up summary",
      "needed to compare 0.25, 0.375, and 0.50."
    ),
    call. = FALSE
  )
}

bracketing_summary <- previous_summary[
  previous_summary$contamination_size %in% c(0.25, 0.50),
  ,
  drop = FALSE
]

combined_summary <- rbind(
  bracketing_summary,
  refinement_summary
)

selection_table <- make_selection_table(combined_summary)

# -------------------------------------------------------------------------
# Save outputs
# -------------------------------------------------------------------------

metadata <- list(
  study = "mmiCATs Study 1 bad-leverage midpoint refinement",
  created_at = Sys.time(),
  common_seed = common_seed,
  methods = methods,
  selection_rule = paste(
    "Among stable candidates with average Type I error no greater",
    "than 50 percent, select the condition whose average Type I error",
    "across ri, cr2, and cats is closest to 30 percent."
  ),
  final_refinement = TRUE,
  session_info = utils::sessionInfo()
)

result_object <- list(
  design = refinement_design,
  summary = refinement_summary,
  replicates = refinement_replicates,
  diagnostics = refinement_diagnostics,
  negative_control_check = negative_control_check,
  combined_candidate_summary = combined_summary,
  selection = selection_table,
  metadata = metadata
)

save_rds_atomic(
  refinement_summary,
  file.path(output_dir, "bad_leverage_refinement_summary.rds")
)
save_rds_atomic(
  refinement_replicates,
  file.path(output_dir, "bad_leverage_refinement_replicates.rds")
)
save_rds_atomic(
  refinement_diagnostics,
  file.path(output_dir, "bad_leverage_refinement_diagnostics.rds")
)
save_rds_atomic(
  combined_summary,
  file.path(output_dir, "bad_leverage_combined_candidate_summary.rds")
)
save_rds_atomic(
  selection_table,
  file.path(output_dir, "bad_leverage_final_selection.rds")
)
save_rds_atomic(
  result_object,
  file.path(output_dir, "study1_bad_leverage_refinement.rds")
)

utils::write.csv(
  refinement_design,
  file.path(output_dir, "bad_leverage_refinement_design.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  refinement_summary,
  file.path(output_dir, "bad_leverage_refinement_summary.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  refinement_diagnostics,
  file.path(output_dir, "bad_leverage_refinement_diagnostics.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  negative_control_check,
  file.path(output_dir, "bad_leverage_refinement_negative_control.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  combined_summary,
  file.path(output_dir, "bad_leverage_combined_candidate_summary.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  selection_table,
  file.path(output_dir, "bad_leverage_final_selection.csv"),
  row.names = FALSE,
  na = ""
)

writeLines(
  capture.output(utils::sessionInfo()),
  con = file.path(output_dir, "session_info.txt"),
  useBytes = TRUE
)

# -------------------------------------------------------------------------
# Console summary
# -------------------------------------------------------------------------

message("")
message("Study 1 bad-leverage midpoint refinement complete.")
message(paste("Results saved to:", output_dir))

message("")
message("Final bad-leverage selection:")
print(selection_table, row.names = FALSE)

message("")
message("Method-specific midpoint results:")
print(
  refinement_summary[
    ,
    c(
      "model",
      "mean_coef",
      "bias",
      "rejection_rate",
      "rmse",
      "coverage",
      "avg_ci_width",
      "failure_rate",
      "mean_retained_clusters"
    )
  ],
  row.names = FALSE
)

message("")
message("Midpoint diagnostics:")
print(refinement_diagnostics, row.names = FALSE)

message("")
message("Truncated CATs negative-control check:")
print(negative_control_check, row.names = FALSE)
