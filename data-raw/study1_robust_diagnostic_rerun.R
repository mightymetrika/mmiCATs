# Study 1 robust CATs diagnostic rerun
#
# This script reruns the two robust CATs methods for three selected conditions:
#   1. clean null data;
#   2. vertical contamination of 6 residual standard deviations; and
#   3. bad leverage with leverage size 4 and outcome contamination size 1.
#
# The original parameter-calibration seed is reused so the updated robust CATs
# implementation can be compared directly with the saved calibration results.
#
# Run this script from the mmiCATs project. The project root is located
# automatically.

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

collapse_unique <- function(x) {
  x <- unique(x[!is.na(x) & nzchar(x)])

  if (length(x) == 0L) {
    return(NA_character_)
  }

  paste(x, collapse = " | ")
}

summarize_diagnostics <- function(replicates) {
  condition_ids <- unique(replicates$condition_id)

  rows <- lapply(condition_ids, function(condition_id) {
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

      data.frame(
        condition_id = condition_id,
        condition_label = method_results$condition_label[1L],
        method = method,
        reps = nrow(method_results),
        fit_success_rate = 100 * mean(method_results$fit_success),
        warning_rep_rate = 100 * mean(
          method_results$cluster_warning_count > 0
        ),
        error_rep_rate = 100 * mean(
          method_results$cluster_error_count > 0
        ),
        dropped_rep_rate = 100 * mean(
          method_results$dropped_cluster_count > 0
        ),
        mean_cluster_warning_count = mean(
          method_results$cluster_warning_count
        ),
        mean_cluster_error_count = mean(
          method_results$cluster_error_count
        ),
        mean_dropped_cluster_count = mean(
          method_results$dropped_cluster_count
        ),
        max_cluster_warning_count = max(
          method_results$cluster_warning_count
        ),
        max_cluster_error_count = max(
          method_results$cluster_error_count
        ),
        max_dropped_cluster_count = max(
          method_results$dropped_cluster_count
        ),
        template_warning_rate = 100 * mean(
          !is.na(method_results$template_warning) &
            nzchar(method_results$template_warning)
        ),
        cluster_warning_ids = collapse_unique(
          method_results$cluster_warning_ids
        ),
        cluster_error_ids = collapse_unique(
          method_results$cluster_error_ids
        ),
        dropped_cluster_ids = collapse_unique(
          method_results$dropped_cluster_ids
        ),
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, method_rows)
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

numeric_comparison <- function(new_results, old_results) {
  key_columns <- c(
    "condition_id",
    "replicate",
    "method"
  )

  available_key_columns <- key_columns[
    key_columns %in% names(new_results) &
      key_columns %in% names(old_results)
  ]

  if (length(available_key_columns) < 3L) {
    stop(
      paste0(
        "Could not identify the condition, replicate, and method keys. ",
        "New columns: ",
        paste(names(new_results), collapse = ", "),
        ". Old columns: ",
        paste(names(old_results), collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  candidate_columns <- c(
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

  comparison_columns <- candidate_columns[
    candidate_columns %in% names(new_results) &
      candidate_columns %in% names(old_results)
  ]

  old_subset <- old_results[
    ,
    c(available_key_columns, comparison_columns),
    drop = FALSE
  ]
  new_subset <- new_results[
    ,
    c(available_key_columns, comparison_columns),
    drop = FALSE
  ]

  names(old_subset)[
    names(old_subset) %in% comparison_columns
  ] <- paste0(
    names(old_subset)[
      names(old_subset) %in% comparison_columns
    ],
    "_old"
  )

  names(new_subset)[
    names(new_subset) %in% comparison_columns
  ] <- paste0(
    names(new_subset)[
      names(new_subset) %in% comparison_columns
    ],
    "_new"
  )

  merged <- merge(
    old_subset,
    new_subset,
    by = available_key_columns,
    all = TRUE,
    sort = FALSE
  )

  comparison_rows <- lapply(comparison_columns, function(variable) {
    old_name <- paste0(variable, "_old")
    new_name <- paste0(variable, "_new")

    old_value <- merged[[old_name]]
    new_value <- merged[[new_name]]

    if (is.numeric(old_value) || is.integer(old_value)) {
      difference <- new_value - old_value
      absolute_difference <- abs(difference)

      data.frame(
        variable = variable,
        compared_rows = sum(
          !is.na(old_value) & !is.na(new_value)
        ),
        missing_old = sum(is.na(old_value)),
        missing_new = sum(is.na(new_value)),
        exact_match_rate = 100 * mean(
          is.na(old_value) & is.na(new_value) |
            (!is.na(old_value) & !is.na(new_value) &
               old_value == new_value)
        ),
        maximum_absolute_difference = if (
          all(is.na(absolute_difference))
        ) {
          NA_real_
        } else {
          max(absolute_difference, na.rm = TRUE)
        },
        mean_absolute_difference = if (
          all(is.na(absolute_difference))
        ) {
          NA_real_
        } else {
          mean(absolute_difference, na.rm = TRUE)
        },
        stringsAsFactors = FALSE
      )
    } else {
      equal_value <- is.na(old_value) & is.na(new_value) |
        (!is.na(old_value) & !is.na(new_value) &
           old_value == new_value)

      data.frame(
        variable = variable,
        compared_rows = sum(
          !is.na(old_value) & !is.na(new_value)
        ),
        missing_old = sum(is.na(old_value)),
        missing_new = sum(is.na(new_value)),
        exact_match_rate = 100 * mean(equal_value),
        maximum_absolute_difference = NA_real_,
        mean_absolute_difference = NA_real_,
        stringsAsFactors = FALSE
      )
    }
  })

  summary <- do.call(rbind, comparison_rows)
  rownames(summary) <- NULL

  list(
    merged = merged,
    summary = summary
  )
}

project_root <- find_project_root()

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop(
    "The pkgload package is required to run this data-raw script.",
    call. = FALSE
  )
}

pkgload::load_all(project_root, quiet = TRUE)

# -------------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------------

reps <- 250L
parameter_calibration_seed <- 20260802L
overwrite_completed <- FALSE

methods <- c(
  "cats_robust",
  "cats_robustbase"
)

output_dir <- file.path(
  project_root,
  "data-raw",
  "study1-results",
  "robust-diagnostic-rerun"
)

checkpoint_dir <- file.path(output_dir, "conditions")

dir.create(
  checkpoint_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

diagnostic_design <- data.frame(
  condition_id = c("DR001", "DR002", "DR003"),
  original_condition_id = c("PC001", "PC007", "PC010"),
  condition_label = c(
    "clean_null",
    "vertical_6",
    "bad_leverage_4_1"
  ),
  n_clusters = 20L,
  cluster_size = 40L,
  beta = 0,
  intercept = 0,
  random_intercept_sd = 1,
  residual_sd = 1,
  x_sd = 1,
  contamination = c(
    "none",
    "vertical",
    "bad_leverage"
  ),
  contamination_prop = 0.05,
  contamination_size = c(1, 6, 1),
  leverage_size = c(1, 1, 4),
  reps = reps,
  alpha = 0.05,
  condition_seed = parameter_calibration_seed,
  stringsAsFactors = FALSE
)

design_path <- file.path(
  output_dir,
  "robust_diagnostic_design.rds"
)

if (file.exists(design_path)) {
  existing_design <- readRDS(design_path)

  if (!identical(existing_design, diagnostic_design)) {
    if (!overwrite_completed) {
      stop(
        paste(
          "The saved diagnostic design differs from the current design.",
          "Set overwrite_completed <- TRUE to replace prior results."
        ),
        call. = FALSE
      )
    }

    old_checkpoints <- list.files(
      checkpoint_dir,
      pattern = "^condition_DR[0-9]{3}[.]rds$",
      full.names = TRUE
    )

    if (length(old_checkpoints) > 0L) {
      file.remove(old_checkpoints)
    }
  }
}

save_rds_atomic(diagnostic_design, design_path)

# -------------------------------------------------------------------------
# Run selected conditions
# -------------------------------------------------------------------------

for (condition_index in seq_len(nrow(diagnostic_design))) {
  condition <- diagnostic_design[
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
      "Running %s: %s.",
      condition$condition_id,
      condition$condition_label
    )
  )

  started_at <- Sys.time()

  result <- tryCatch(
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

  if (inherits(result, "error")) {
    checkpoint <- list(
      status = "error",
      condition = condition,
      result = NULL,
      error = conditionMessage(result),
      started_at = started_at,
      completed_at = completed_at,
      elapsed_sec = elapsed_sec
    )

    save_rds_atomic(checkpoint, checkpoint_path)
    next
  }

  result$summary <- add_condition_columns(
    result$summary,
    condition
  )
  result$replicates <- add_condition_columns(
    result$replicates,
    condition
  )

  checkpoint <- list(
    status = "complete",
    condition = condition,
    result = result,
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

# -------------------------------------------------------------------------
# Combine rerun results
# -------------------------------------------------------------------------

checkpoint_paths <- list.files(
  checkpoint_dir,
  pattern = "^condition_DR[0-9]{3}[.]rds$",
  full.names = TRUE
)

checkpoints <- lapply(checkpoint_paths, readRDS)

complete_checkpoints <- checkpoints[
  vapply(
    checkpoints,
    function(checkpoint) identical(checkpoint$status, "complete"),
    logical(1)
  )
]

if (length(complete_checkpoints) == 0L) {
  stop(
    "No diagnostic rerun conditions completed successfully.",
    call. = FALSE
  )
}

diagnostic_summary <- do.call(
  rbind,
  lapply(
    complete_checkpoints,
    function(checkpoint) checkpoint$result$summary
  )
)

diagnostic_replicates <- do.call(
  rbind,
  lapply(
    complete_checkpoints,
    function(checkpoint) checkpoint$result$replicates
  )
)

rownames(diagnostic_summary) <- NULL
rownames(diagnostic_replicates) <- NULL

diagnostic_counts <- summarize_diagnostics(
  diagnostic_replicates
)

# -------------------------------------------------------------------------
# Compare with the original parameter calibration when available
# -------------------------------------------------------------------------

old_replicate_path <- file.path(
  project_root,
  "data-raw",
  "study1-results",
  "parameter-calibration",
  "parameter_calibration_replicates.rds"
)

comparison <- NULL

if (file.exists(old_replicate_path)) {
  old_replicates <- readRDS(old_replicate_path)

  old_replicates <- old_replicates[
    old_replicates$condition_id %in%
      diagnostic_design$original_condition_id &
      old_replicates$method %in% methods,
    ,
    drop = FALSE
  ]

  new_replicates <- diagnostic_replicates

  if (!"original_condition_id" %in% names(new_replicates)) {
    stop(
      paste(
        "The diagnostic rerun results do not contain",
        "'original_condition_id'."
      ),
      call. = FALSE
    )
  }

  new_replicates$diagnostic_condition_id <-
    new_replicates$condition_id
  new_replicates$condition_id <-
    new_replicates$original_condition_id

  comparison <- numeric_comparison(
    new_results = new_replicates,
    old_results = old_replicates
  )

  save_rds_atomic(
    comparison$merged,
    file.path(output_dir, "old_new_replicate_comparison.rds")
  )

  utils::write.csv(
    comparison$summary,
    file.path(output_dir, "old_new_comparison_summary.csv"),
    row.names = FALSE,
    na = ""
  )
}

# -------------------------------------------------------------------------
# Save outputs
# -------------------------------------------------------------------------

result_object <- list(
  design = diagnostic_design,
  summary = diagnostic_summary,
  replicates = diagnostic_replicates,
  diagnostics = diagnostic_counts,
  old_new_comparison = comparison,
  session_info = utils::sessionInfo()
)

save_rds_atomic(
  diagnostic_summary,
  file.path(output_dir, "robust_diagnostic_summary.rds")
)
save_rds_atomic(
  diagnostic_replicates,
  file.path(output_dir, "robust_diagnostic_replicates.rds")
)
save_rds_atomic(
  diagnostic_counts,
  file.path(output_dir, "robust_diagnostic_counts.rds")
)
save_rds_atomic(
  result_object,
  file.path(output_dir, "study1_robust_diagnostic_rerun.rds")
)

utils::write.csv(
  diagnostic_design,
  file.path(output_dir, "robust_diagnostic_design.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  diagnostic_summary,
  file.path(output_dir, "robust_diagnostic_summary.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  diagnostic_counts,
  file.path(output_dir, "robust_diagnostic_counts.csv"),
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
message("Study 1 robust CATs diagnostic rerun complete.")
message(paste("Results saved to:", output_dir))

print(
  diagnostic_counts,
  row.names = FALSE
)

if (!is.null(comparison)) {
  message("")
  message("Comparison with original parameter calibration:")
  print(
    comparison$summary,
    row.names = FALSE
  )
}
