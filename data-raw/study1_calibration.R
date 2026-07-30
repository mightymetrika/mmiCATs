# Study 1 calibration pilot
#
# This script runs a small plumbing and runtime pilot for pwr_func_study1().
# It saves one checkpoint per simulation condition, combines completed
# conditions, and creates preliminary diagnostic summaries.
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
# Pilot configuration
# -------------------------------------------------------------------------

pilot_reps <- 5L
base_seed <- 20260728L
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
  "calibration"
)

checkpoint_dir <- file.path(output_dir, "conditions")

dir.create(
  checkpoint_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

calibration_design <- expand.grid(
  n_clusters = c(10L, 20L, 40L),
  beta = c(0, 0.20),
  contamination = c("none", "vertical", "bad_leverage"),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

contamination_order <- match(
  calibration_design$contamination,
  c("none", "vertical", "bad_leverage")
)

calibration_design <- calibration_design[
  order(
    calibration_design$n_clusters,
    calibration_design$beta,
    contamination_order
  ),
  ,
  drop = FALSE
]

rownames(calibration_design) <- NULL

calibration_design$condition_id <- sprintf(
  "%03d",
  seq_len(nrow(calibration_design))
)
calibration_design$cluster_size <- 40L
calibration_design$intercept <- 0
calibration_design$random_intercept_sd <- 1
calibration_design$residual_sd <- 1
calibration_design$x_sd <- 1
calibration_design$contamination_prop <- 0.05
calibration_design$contamination_size <- 10
calibration_design$leverage_size <- 10
calibration_design$reps <- pilot_reps
calibration_design$alpha <- 0.05
calibration_design$method_set <- paste(methods, collapse = ",")
calibration_design$condition_seed <- as.integer(
  base_seed + seq_len(nrow(calibration_design))
)

calibration_design <- calibration_design[
  ,
  c(
    "condition_id",
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

design_path <- file.path(output_dir, "calibration_design.rds")

if (file.exists(design_path)) {
  existing_design <- readRDS(design_path)

  if (!identical(existing_design, calibration_design)) {
    if (!overwrite_completed) {
      stop(
        paste(
          "The saved calibration design differs from the current design.",
          "Set overwrite_completed <- TRUE to replace prior pilot results."
        ),
        call. = FALSE
      )
    }

    old_checkpoints <- list.files(
      checkpoint_dir,
      pattern = "^condition_[0-9]{3}[.]rds$",
      full.names = TRUE
    )

    if (length(old_checkpoints) > 0L) {
      file.remove(old_checkpoints)
    }
  }
}

save_rds_atomic(calibration_design, design_path)

package_description <- read.dcf(
  file.path(project_root, "DESCRIPTION")
)

metadata <- list(
  study = "mmiCATs Study 1 calibration pilot",
  created_at = Sys.time(),
  project_root = project_root,
  package_version = unname(package_description[1L, "Version"]),
  r_version = R.version.string,
  pilot_reps = pilot_reps,
  base_seed = base_seed,
  methods = methods,
  overwrite_completed = overwrite_completed,
  session_info = utils::sessionInfo()
)

save_rds_atomic(
  metadata,
  file.path(output_dir, "calibration_metadata.rds")
)

writeLines(
  capture.output(utils::sessionInfo()),
  con = file.path(output_dir, "session_info.txt"),
  useBytes = TRUE
)

# -------------------------------------------------------------------------
# Run each pilot condition
# -------------------------------------------------------------------------

for (condition_index in seq_len(nrow(calibration_design))) {
  condition <- calibration_design[
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
          nrow(calibration_design)
        )
      )
      next
    }
  }

  message(
    sprintf(
      paste0(
        "Running condition %s of %s: ",
        "G = %s, beta = %s, contamination = %s"
      ),
      condition$condition_id,
      nrow(calibration_design),
      condition$n_clusters,
      format(condition$beta, trim = TRUE),
      condition$contamination
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
# Combine checkpoints and create preliminary diagnostics
# -------------------------------------------------------------------------

checkpoint_paths <- list.files(
  checkpoint_dir,
  pattern = "^condition_[0-9]{3}[.]rds$",
  full.names = TRUE
)

checkpoints <- lapply(checkpoint_paths, readRDS)

calibration_status <- do.call(
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

rownames(calibration_status) <- NULL

complete_checkpoints <- checkpoints[
  vapply(
    checkpoints,
    function(checkpoint) identical(checkpoint$status, "complete"),
    logical(1)
  )
]

if (length(complete_checkpoints) == 0L) {
  stop(
    "No calibration conditions completed successfully.",
    call. = FALSE
  )
}

calibration_summary <- do.call(
  rbind,
  lapply(
    complete_checkpoints,
    function(checkpoint) checkpoint$result$summary
  )
)

calibration_replicates <- do.call(
  rbind,
  lapply(
    complete_checkpoints,
    function(checkpoint) checkpoint$result$replicates
  )
)

rownames(calibration_summary) <- NULL
rownames(calibration_replicates) <- NULL

calibration_diagnostics <- condition_diagnostics(
  calibration_replicates
)

calibration_pilot <- list(
  design = calibration_design,
  status = calibration_status,
  summary = calibration_summary,
  replicates = calibration_replicates,
  diagnostics = calibration_diagnostics,
  metadata = metadata
)

save_rds_atomic(
  calibration_status,
  file.path(output_dir, "calibration_status.rds")
)
save_rds_atomic(
  calibration_summary,
  file.path(output_dir, "calibration_summary.rds")
)
save_rds_atomic(
  calibration_replicates,
  file.path(output_dir, "calibration_replicates.rds")
)
save_rds_atomic(
  calibration_diagnostics,
  file.path(output_dir, "calibration_diagnostics.rds")
)
save_rds_atomic(
  calibration_pilot,
  file.path(output_dir, "study1_calibration_pilot.rds")
)

utils::write.csv(
  calibration_design,
  file.path(output_dir, "calibration_design.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  calibration_status,
  file.path(output_dir, "calibration_status.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  calibration_summary,
  file.path(output_dir, "calibration_summary.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  calibration_diagnostics,
  file.path(output_dir, "calibration_diagnostics.csv"),
  row.names = FALSE,
  na = ""
)

# -------------------------------------------------------------------------
# Console summary
# -------------------------------------------------------------------------

message("")
message("Study 1 calibration pilot complete.")
message(
  sprintf(
    "Completed conditions: %s of %s.",
    sum(calibration_status$status == "complete"),
    nrow(calibration_design)
  )
)
message(
  sprintf(
    "Total elapsed time across saved conditions: %.2f minutes.",
    sum(
      calibration_status$elapsed_sec[
        calibration_status$status == "complete"
      ],
      na.rm = TRUE
    ) / 60
  )
)
message(paste("Results saved to:", output_dir))

print(
  calibration_status[
    ,
    c(
      "condition_id",
      "n_clusters",
      "beta",
      "contamination",
      "status",
      "elapsed_sec"
    )
  ],
  row.names = FALSE
)

print(
  calibration_diagnostics[
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
