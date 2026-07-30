# Study 1 parameter calibration
#
# This script calibrates:
#   1. the nonzero slope used for power comparisons;
#   2. the magnitude of vertical contamination; and
#   3. the magnitude of bad leverage contamination.
#
# All six Study 1 methods are retained, including truncated CATs as a
# negative-control method. One checkpoint is saved per condition.
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

      warning_present <- !is.na(method_results$warning) &
        nzchar(method_results$warning)
      error_present <- !is.na(method_results$error) &
        nzchar(method_results$error)
      singular_values <- method_results$singular
      singular_values <- singular_values[!is.na(singular_values)]

      condition_columns <- condition_results[
        1L,
        c(
          "condition_id",
          "calibration_stage",
          "candidate_id",
          "n_clusters",
          "cluster_size",
          "beta",
          "contamination",
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
        warning_count = sum(warning_present),
        warning_rate = 100 * mean(warning_present),
        error_count = sum(error_present),
        error_rate = 100 * mean(error_present),
        failure_count = sum(!method_results$fit_success),
        failure_rate = 100 * mean(!method_results$fit_success),
        singular_rate = if (length(singular_values) == 0L) {
          NA_real_
        } else {
          100 * mean(singular_values)
        },
        mean_retained_clusters = mean_or_na(
          method_results$retained_clusters
        ),
        mean_runtime_sec = mean_or_na(method_results$runtime_sec),
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, method_diagnostics)
  })

  out <- do.call(rbind, diagnostics)
  rownames(out) <- NULL
  out
}

make_contamination_comparison <- function(summary_results) {
  clean_results <- summary_results[
    summary_results$calibration_stage == "contamination_reference",
    c(
      "model",
      "mean_coef",
      "bias",
      "rejection_rate",
      "rmse",
      "coverage",
      "avg_ci_width",
      "failure_rate"
    ),
    drop = FALSE
  ]

  names(clean_results)[-1L] <- paste0(
    "clean_",
    names(clean_results)[-1L]
  )

  contaminated_results <- summary_results[
    summary_results$calibration_stage %in%
      c("vertical_severity", "bad_leverage_severity"),
    ,
    drop = FALSE
  ]

  comparison <- merge(
    contaminated_results,
    clean_results,
    by = "model",
    all.x = TRUE,
    sort = FALSE
  )

  comparison$rmse_difference <- comparison$rmse -
    comparison$clean_rmse
  comparison$rmse_ratio <- comparison$rmse /
    comparison$clean_rmse
  comparison$ci_width_difference <- comparison$avg_ci_width -
    comparison$clean_avg_ci_width
  comparison$ci_width_ratio <- comparison$avg_ci_width /
    comparison$clean_avg_ci_width
  comparison$absolute_bias_difference <- abs(comparison$bias) -
    abs(comparison$clean_bias)
  comparison$rejection_rate_difference <- comparison$rejection_rate -
    comparison$clean_rejection_rate
  comparison$coverage_difference <- comparison$coverage -
    comparison$clean_coverage

  comparison
}

make_vertical_selection_aid <- function(comparison) {
  nonrobust_methods <- c("ri", "cr2", "cats")

  vertical_results <- comparison[
    comparison$calibration_stage == "vertical_severity" &
      comparison$model %in% nonrobust_methods,
    ,
    drop = FALSE
  ]

  split_results <- split(
    vertical_results,
    vertical_results$candidate_id
  )

  rows <- lapply(split_results, function(x) {
    data.frame(
      candidate_id = x$candidate_id[1L],
      contamination_size = x$contamination_size[1L],
      mean_nonrobust_rmse_ratio = mean_or_na(x$rmse_ratio),
      minimum_nonrobust_rmse_ratio = min_or_na(x$rmse_ratio),
      mean_nonrobust_ci_width_ratio = mean_or_na(x$ci_width_ratio),
      minimum_nonrobust_ci_width_ratio = min_or_na(
        x$ci_width_ratio
      ),
      maximum_nonrobust_failure_rate = max_or_na(
        x$failure_rate
      ),
      maximum_nonrobust_absolute_bias = max_or_na(abs(x$bias)),
      maximum_nonrobust_rejection_rate = max_or_na(
        x$rejection_rate
      ),
      minimum_nonrobust_coverage = min_or_na(x$coverage),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  out$stable <- out$maximum_nonrobust_failure_rate <= 1
  out$meaningful_precision_change <-
    out$mean_nonrobust_rmse_ratio >= 1.5 |
    out$mean_nonrobust_ci_width_ratio >= 1.5
  out$screening_candidate <- out$stable &
    out$meaningful_precision_change

  out <- out[order(out$contamination_size), , drop = FALSE]
  rownames(out) <- NULL
  out
}

make_bad_leverage_selection_aid <- function(comparison) {
  nonrobust_methods <- c("ri", "cr2", "cats")

  leverage_results <- comparison[
    comparison$calibration_stage == "bad_leverage_severity" &
      comparison$model %in% nonrobust_methods,
    ,
    drop = FALSE
  ]

  split_results <- split(
    leverage_results,
    leverage_results$candidate_id
  )

  rows <- lapply(split_results, function(x) {
    n_contaminated <- max(
      1L,
      as.integer(round(
        x$cluster_size[1L] * x$contamination_prop[1L]
      ))
    )

    n_clean <- x$cluster_size[1L] - n_contaminated
    leverage_size <- x$leverage_size[1L]
    contamination_size <- x$contamination_size[1L]

    theoretical_induced_slope <- -(
      n_contaminated * leverage_size * contamination_size
    ) / (
      n_clean +
        n_contaminated * leverage_size^2
    )

    data.frame(
      candidate_id = x$candidate_id[1L],
      leverage_size = leverage_size,
      contamination_size = contamination_size,
      theoretical_induced_slope = theoretical_induced_slope,
      mean_nonrobust_bias = mean_or_na(x$bias),
      mean_nonrobust_absolute_bias = mean_or_na(abs(x$bias)),
      maximum_nonrobust_absolute_bias = max_or_na(abs(x$bias)),
      mean_nonrobust_rejection_rate = mean_or_na(
        x$rejection_rate
      ),
      maximum_nonrobust_rejection_rate = max_or_na(
        x$rejection_rate
      ),
      minimum_nonrobust_coverage = min_or_na(x$coverage),
      mean_nonrobust_rmse_ratio = mean_or_na(x$rmse_ratio),
      maximum_nonrobust_failure_rate = max_or_na(
        x$failure_rate
      ),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  out$stable <- out$maximum_nonrobust_failure_rate <= 1
  out$moderate_theoretical_shift <-
    abs(out$theoretical_induced_slope) >= 0.10 &
    abs(out$theoretical_induced_slope) <= 0.30
  out$screening_candidate <- out$stable &
    out$moderate_theoretical_shift

  out <- out[
    order(out$leverage_size, out$contamination_size),
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
      "The pbkrtest package is required because the calibration includes",
      "the random-intercept method with Kenward-Roger inference."
    ),
    call. = FALSE
  )
}

pkgload::load_all(project_root, quiet = TRUE)

# -------------------------------------------------------------------------
# Parameter-calibration configuration
# -------------------------------------------------------------------------

parameter_reps <- 250L
effect_seed <- 20260801L
contamination_seed <- 20260802L
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
  "parameter-calibration"
)

checkpoint_dir <- file.path(output_dir, "conditions")

dir.create(
  checkpoint_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# A common random-number seed is used within each calibration block.
# This makes comparisons across candidate values more precise because the
# underlying clean data are the same for corresponding replications.

contamination_reference <- data.frame(
  calibration_stage = "contamination_reference",
  candidate_id = "R01",
  n_clusters = 20L,
  beta = 0,
  contamination = "none",
  contamination_size = 0,
  leverage_size = 0,
  condition_seed = contamination_seed,
  stringsAsFactors = FALSE
)

effect_design <- data.frame(
  calibration_stage = "effect_size",
  candidate_id = sprintf("E%02d", 1:4),
  n_clusters = 20L,
  beta = c(0.06, 0.08, 0.10, 0.12),
  contamination = "none",
  contamination_size = 0,
  leverage_size = 0,
  condition_seed = effect_seed,
  stringsAsFactors = FALSE
)

vertical_design <- data.frame(
  calibration_stage = "vertical_severity",
  candidate_id = sprintf("V%02d", 1:4),
  n_clusters = 20L,
  beta = 0,
  contamination = "vertical",
  contamination_size = c(4, 6, 8, 10),
  leverage_size = 0,
  condition_seed = contamination_seed,
  stringsAsFactors = FALSE
)

bad_leverage_grid <- expand.grid(
  leverage_size = c(4, 6),
  contamination_size = c(1, 2, 3),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

bad_leverage_grid <- bad_leverage_grid[
  order(
    bad_leverage_grid$leverage_size,
    bad_leverage_grid$contamination_size
  ),
  ,
  drop = FALSE
]

bad_leverage_design <- data.frame(
  calibration_stage = "bad_leverage_severity",
  candidate_id = sprintf(
    "B%02d",
    seq_len(nrow(bad_leverage_grid))
  ),
  n_clusters = 20L,
  beta = 0,
  contamination = "bad_leverage",
  contamination_size = bad_leverage_grid$contamination_size,
  leverage_size = bad_leverage_grid$leverage_size,
  condition_seed = contamination_seed,
  stringsAsFactors = FALSE
)

parameter_design <- rbind(
  contamination_reference,
  effect_design,
  vertical_design,
  bad_leverage_design
)

rownames(parameter_design) <- NULL

parameter_design$condition_id <- sprintf(
  "PC%03d",
  seq_len(nrow(parameter_design))
)
parameter_design$cluster_size <- 40L
parameter_design$intercept <- 0
parameter_design$random_intercept_sd <- 1
parameter_design$residual_sd <- 1
parameter_design$x_sd <- 1
parameter_design$contamination_prop <- 0.05
parameter_design$reps <- parameter_reps
parameter_design$alpha <- 0.05
parameter_design$method_set <- paste(methods, collapse = ",")

parameter_design <- parameter_design[
  ,
  c(
    "condition_id",
    "calibration_stage",
    "candidate_id",
    "n_clusters",
    "cluster_size",
    "beta",
    "intercept",
    "random_intercept_sd",
    "residual_sd",
    "x_sd",
    "contamination",
    "contamination_prop",
    "contamination_size",
    "leverage_size",
    "reps",
    "alpha",
    "method_set",
    "condition_seed"
  )
]

design_path <- file.path(
  output_dir,
  "parameter_calibration_design.rds"
)

if (file.exists(design_path)) {
  existing_design <- readRDS(design_path)

  if (!identical(existing_design, parameter_design)) {
    if (!overwrite_completed) {
      stop(
        paste(
          "The saved parameter-calibration design differs from the",
          "current design. Set overwrite_completed <- TRUE to replace",
          "prior parameter-calibration results."
        ),
        call. = FALSE
      )
    }

    old_checkpoints <- list.files(
      checkpoint_dir,
      pattern = "^condition_PC[0-9]{3}[.]rds$",
      full.names = TRUE
    )

    if (length(old_checkpoints) > 0L) {
      file.remove(old_checkpoints)
    }
  }
}

save_rds_atomic(parameter_design, design_path)

package_description <- read.dcf(
  file.path(project_root, "DESCRIPTION")
)

metadata <- list(
  study = "mmiCATs Study 1 parameter calibration",
  created_at = Sys.time(),
  project_root = project_root,
  package_version = unname(package_description[1L, "Version"]),
  r_version = R.version.string,
  parameter_reps = parameter_reps,
  effect_seed = effect_seed,
  contamination_seed = contamination_seed,
  methods = methods,
  effect_selection_rule = paste(
    "Select the candidate with random-intercept power closest to",
    "70 percent under clean data."
  ),
  vertical_screening_rule = paste(
    "Flag the smallest stable magnitude for which the mean RMSE ratio",
    "or mean confidence-interval width ratio among ri, cr2, and cats",
    "is at least 1.5 relative to the clean reference."
  ),
  bad_leverage_screening_rule = paste(
    "Flag stable candidates with an approximate induced slope between",
    "0.10 and 0.30 in absolute value. Final selection should also",
    "consider observed bias, rejection, and coverage."
  ),
  common_random_numbers = TRUE,
  overwrite_completed = overwrite_completed,
  session_info = utils::sessionInfo()
)

save_rds_atomic(
  metadata,
  file.path(output_dir, "parameter_calibration_metadata.rds")
)

writeLines(
  capture.output(utils::sessionInfo()),
  con = file.path(output_dir, "session_info.txt"),
  useBytes = TRUE
)

# -------------------------------------------------------------------------
# Run each parameter-calibration condition
# -------------------------------------------------------------------------

for (condition_index in seq_len(nrow(parameter_design))) {
  condition <- parameter_design[
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
          "Skipping completed condition %s of %s.",
          condition$condition_id,
          nrow(parameter_design)
        )
      )
      next
    }
  }

  message(
    sprintf(
      paste0(
        "Running condition %s of %s: stage = %s, candidate = %s, ",
        "beta = %s, contamination = %s, contamination size = %s, ",
        "leverage size = %s"
      ),
      condition$condition_id,
      nrow(parameter_design),
      condition$calibration_stage,
      condition$candidate_id,
      format(condition$beta, trim = TRUE),
      condition$contamination,
      format(condition$contamination_size, trim = TRUE),
      format(condition$leverage_size, trim = TRUE)
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
      contamination_size = if (
        condition$contamination == "none"
      ) {
        1
      } else {
        condition$contamination_size
      },
      leverage_size = if (
        condition$contamination == "bad_leverage"
      ) {
        condition$leverage_size
      } else {
        1
      },
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
  pattern = "^condition_PC[0-9]{3}[.]rds$",
  full.names = TRUE
)

checkpoints <- lapply(checkpoint_paths, readRDS)

parameter_status <- do.call(
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

rownames(parameter_status) <- NULL

complete_checkpoints <- checkpoints[
  vapply(
    checkpoints,
    function(checkpoint) identical(checkpoint$status, "complete"),
    logical(1)
  )
]

if (length(complete_checkpoints) == 0L) {
  stop(
    "No parameter-calibration conditions completed successfully.",
    call. = FALSE
  )
}

parameter_summary <- do.call(
  rbind,
  lapply(
    complete_checkpoints,
    function(checkpoint) checkpoint$result$summary
  )
)

parameter_replicates <- do.call(
  rbind,
  lapply(
    complete_checkpoints,
    function(checkpoint) checkpoint$result$replicates
  )
)

rownames(parameter_summary) <- NULL
rownames(parameter_replicates) <- NULL

parameter_diagnostics <- condition_diagnostics(
  parameter_replicates
)

# -------------------------------------------------------------------------
# Create selection aids
# -------------------------------------------------------------------------

effect_size_selection <- parameter_summary[
  parameter_summary$calibration_stage == "effect_size" &
    parameter_summary$model == "ri",
  c(
    "condition_id",
    "candidate_id",
    "beta",
    "rejection_rate",
    "rejection_rate_se",
    "coverage",
    "rmse",
    "avg_ci_width",
    "failure_rate",
    "singular_rate"
  ),
  drop = FALSE
]

effect_size_selection$distance_from_70 <- abs(
  effect_size_selection$rejection_rate - 70
)

effect_size_selection <- effect_size_selection[
  order(
    effect_size_selection$distance_from_70,
    effect_size_selection$beta
  ),
  ,
  drop = FALSE
]

effect_size_selection$recommended <- FALSE

if (nrow(effect_size_selection) > 0L) {
  effect_size_selection$recommended[1L] <- TRUE
}

rownames(effect_size_selection) <- NULL

contamination_comparison <- make_contamination_comparison(
  parameter_summary
)

vertical_selection_aid <- make_vertical_selection_aid(
  contamination_comparison
)

bad_leverage_selection_aid <- make_bad_leverage_selection_aid(
  contamination_comparison
)

parameter_calibration <- list(
  design = parameter_design,
  status = parameter_status,
  summary = parameter_summary,
  replicates = parameter_replicates,
  diagnostics = parameter_diagnostics,
  effect_size_selection = effect_size_selection,
  contamination_comparison = contamination_comparison,
  vertical_selection_aid = vertical_selection_aid,
  bad_leverage_selection_aid = bad_leverage_selection_aid,
  metadata = metadata
)

# -------------------------------------------------------------------------
# Save combined outputs
# -------------------------------------------------------------------------

save_rds_atomic(
  parameter_status,
  file.path(output_dir, "parameter_calibration_status.rds")
)
save_rds_atomic(
  parameter_summary,
  file.path(output_dir, "parameter_calibration_summary.rds")
)
save_rds_atomic(
  parameter_replicates,
  file.path(output_dir, "parameter_calibration_replicates.rds")
)
save_rds_atomic(
  parameter_diagnostics,
  file.path(output_dir, "parameter_calibration_diagnostics.rds")
)
save_rds_atomic(
  effect_size_selection,
  file.path(output_dir, "effect_size_selection.rds")
)
save_rds_atomic(
  contamination_comparison,
  file.path(output_dir, "contamination_comparison.rds")
)
save_rds_atomic(
  vertical_selection_aid,
  file.path(output_dir, "vertical_selection_aid.rds")
)
save_rds_atomic(
  bad_leverage_selection_aid,
  file.path(output_dir, "bad_leverage_selection_aid.rds")
)
save_rds_atomic(
  parameter_calibration,
  file.path(output_dir, "study1_parameter_calibration.rds")
)

utils::write.csv(
  parameter_design,
  file.path(output_dir, "parameter_calibration_design.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  parameter_status,
  file.path(output_dir, "parameter_calibration_status.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  parameter_summary,
  file.path(output_dir, "parameter_calibration_summary.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  parameter_diagnostics,
  file.path(output_dir, "parameter_calibration_diagnostics.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  effect_size_selection,
  file.path(output_dir, "effect_size_selection.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  contamination_comparison,
  file.path(output_dir, "contamination_comparison.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  vertical_selection_aid,
  file.path(output_dir, "vertical_selection_aid.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  bad_leverage_selection_aid,
  file.path(output_dir, "bad_leverage_selection_aid.csv"),
  row.names = FALSE,
  na = ""
)

# -------------------------------------------------------------------------
# Console summary
# -------------------------------------------------------------------------

message("")
message("Study 1 parameter calibration complete.")
message(
  sprintf(
    "Completed conditions: %s of %s.",
    sum(parameter_status$status == "complete"),
    nrow(parameter_design)
  )
)
message(
  sprintf(
    "Total elapsed time across saved conditions: %.2f minutes.",
    sum(
      parameter_status$elapsed_sec[
        parameter_status$status == "complete"
      ],
      na.rm = TRUE
    ) / 60
  )
)
message(paste("Results saved to:", output_dir))

message("")
message("Effect-size selection:")
print(effect_size_selection, row.names = FALSE)

message("")
message("Vertical-contamination selection aid:")
print(vertical_selection_aid, row.names = FALSE)

message("")
message("Bad-leverage selection aid:")
print(bad_leverage_selection_aid, row.names = FALSE)

diagnostic_problems <- parameter_diagnostics[
  parameter_diagnostics$warning_rate > 0 |
    parameter_diagnostics$error_rate > 0 |
    parameter_diagnostics$failure_rate > 0 |
    (
      !is.na(parameter_diagnostics$singular_rate) &
        parameter_diagnostics$singular_rate > 0
    ),
  ,
  drop = FALSE
]

message("")

if (nrow(diagnostic_problems) == 0L) {
  message(
    "No warnings, errors, failures, or singular fits were detected."
  )
} else {
  message("Diagnostic issues detected:")
  print(
    diagnostic_problems[
      ,
      c(
        "condition_id",
        "model",
        "warning_rate",
        "error_rate",
        "failure_rate",
        "singular_rate",
        "mean_retained_clusters",
        "mean_runtime_sec"
      )
    ],
    row.names = FALSE
  )
}
