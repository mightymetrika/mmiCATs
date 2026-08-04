# Study 2 random-slope convergence diagnostic
#
# Purpose:
# Estimate the usable-fit, genuine convergence-failure, and singularity rates
# of the correctly specified random-slope/Kenward-Roger comparator before the
# final Study 2 simulation.
#
# Design:
#   - n_clusters: 10, 20, 40
#   - random_slope_sd: 0.05, 0.10
#   - contamination: none, vertical
#   - beta: 0
#   - cluster_size: 40
#   - 250 replications per condition
#   - method: rs only
#
# Beta is fixed at zero because changing beta does not change the covariance
# structure. The prespecified criterion is at least 95 percent usable fits in
# every condition. Singular fits remain usable when fixed-effect inference is
# finite and there is no genuine optimizer or gradient convergence failure.

find_project_root <- function(path = getwd()) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  repeat {
    if (file.exists(file.path(path, "DESCRIPTION"))) {
      return(path)
    }

    parent <- dirname(path)

    if (identical(parent, path)) {
      stop("Could not locate the mmiCATs project root.", call. = FALSE)
    }

    path <- parent
  }
}

save_rds_atomic <- function(object, path, compress = "gzip") {
  temp_path <- tempfile(
    pattern = "study2_rs_",
    tmpdir = dirname(path),
    fileext = ".rds"
  )

  saveRDS(object, temp_path, version = 3, compress = compress)

  if (file.exists(path) && !file.remove(path)) {
    stop(paste("Could not replace existing file:", path), call. = FALSE)
  }

  if (!file.rename(temp_path, path)) {
    stop(paste("Could not save file:", path), call. = FALSE)
  }

  invisible(path)
}

write_csv_atomic <- function(object, path) {
  temp_path <- tempfile(
    pattern = "study2_rs_",
    tmpdir = dirname(path),
    fileext = ".csv"
  )

  utils::write.csv(object, temp_path, row.names = FALSE, na = "")

  if (file.exists(path) && !file.remove(path)) {
    stop(paste("Could not replace existing file:", path), call. = FALSE)
  }

  if (!file.rename(temp_path, path)) {
    stop(paste("Could not save file:", path), call. = FALSE)
  }

  invisible(path)
}

add_condition_columns <- function(x, condition) {
  condition_rows <- condition[rep(1L, nrow(x)), , drop = FALSE]
  out <- cbind(condition_rows, x)
  rownames(out) <- NULL
  out
}

has_text <- function(x) {
  !is.na(x) & nzchar(x)
}

mean_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA_real_)
  mean(x)
}

min_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA_real_)
  min(x)
}

max_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA_real_)
  max(x)
}

quantile_or_na <- function(x, probability) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA_real_)

  unname(stats::quantile(
    x,
    probs = probability,
    names = FALSE,
    type = 7
  ))
}

column_or_default <- function(data, name, default) {
  if (name %in% names(data)) return(data[[name]])
  rep(default, nrow(data))
}

binomial_interval <- function(successes, trials, conf_level = 0.95) {
  if (trials <= 0L) {
    return(c(lower = NA_real_, upper = NA_real_))
  }

  interval <- stats::binom.test(
    x = successes,
    n = trials,
    conf.level = conf_level
  )$conf.int

  c(
    lower = 100 * unname(interval[1L]),
    upper = 100 * unname(interval[2L])
  )
}

make_design <- function(reps, cluster_seeds) {
  rows <- list()
  index <- 0L

  for (n_clusters in c(10L, 20L, 40L)) {
    for (random_slope_sd in c(0.05, 0.10)) {
      for (contamination in c("none", "vertical")) {
        index <- index + 1L

        rows[[index]] <- data.frame(
          condition_id = sprintf("S2RSD%03d", index),
          n_clusters = n_clusters,
          cluster_size = 40L,
          beta = 0,
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
          reps = reps,
          alpha = 0.05,
          method_set = "rs",
          condition_seed = unname(
            cluster_seeds[as.character(n_clusters)]
          ),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  design <- do.call(rbind, rows)
  rownames(design) <- NULL
  design
}

checkpoint_path_for <- function(checkpoint_dir, condition_id) {
  file.path(
    checkpoint_dir,
    paste0("condition_", condition_id, ".rds")
  )
}

read_checkpoint_safely <- function(path) {
  tryCatch(
    readRDS(path),
    error = function(e) {
      list(
        status = "unreadable",
        condition = NULL,
        result = NULL,
        error = conditionMessage(e),
        started_at = as.POSIXct(NA),
        completed_at = as.POSIXct(NA),
        elapsed_sec = NA_real_
      )
    }
  )
}

collect_checkpoints <- function(checkpoint_dir) {
  paths <- sort(list.files(
    checkpoint_dir,
    pattern = "^condition_S2RSD[0-9]{3}[.]rds$",
    full.names = TRUE
  ))

  if (length(paths) == 0L) return(list())
  lapply(paths, read_checkpoint_safely)
}

checkpoint_status_row <- function(checkpoint) {
  if (is.null(checkpoint$condition)) {
    return(data.frame(
      condition_id = NA_character_,
      status = checkpoint$status,
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
  if (length(checkpoints) == 0L) return(data.frame())

  out <- do.call(
    rbind,
    lapply(checkpoints, checkpoint_status_row)
  )
  rownames(out) <- NULL
  out
}

save_status <- function(checkpoint_dir, output_dir) {
  status <- collect_status(collect_checkpoints(checkpoint_dir))

  if (nrow(status) > 0L) {
    save_rds_atomic(
      status,
      file.path(output_dir, "study2_rs_diagnostic_status.rds")
    )
    write_csv_atomic(
      status,
      file.path(output_dir, "study2_rs_diagnostic_status.csv")
    )
  }

  invisible(status)
}

summarize_diagnostics <- function(replicates) {
  condition_ids <- unique(replicates$condition_id)

  rows <- lapply(condition_ids, function(condition_id) {
    x <- replicates[
      replicates$condition_id == condition_id,
      ,
      drop = FALSE
    ]

    optimizer_warning <- column_or_default(
      x,
      "optimizer_warning",
      NA_character_
    )
    optimizer_code <- column_or_default(
      x,
      "optimizer_code",
      NA_real_
    )
    warning <- column_or_default(x, "warning", NA_character_)
    error <- column_or_default(x, "error", NA_character_)
    converged <- column_or_default(x, "converged", NA)
    singular <- column_or_default(x, "singular", NA)
    fitted_rs_sd <- column_or_default(
      x,
      "estimated_random_slope_sd",
      NA_real_
    )
    fitted_ri_sd <- column_or_default(
      x,
      "estimated_random_intercept_sd",
      NA_real_
    )

    attempted <- nrow(x)
    usable <- sum(x$fit_success)
    failures <- attempted - usable
    singular_trials <- sum(!is.na(singular))
    singular_count <- sum(singular, na.rm = TRUE)

    usable_ci <- binomial_interval(usable, attempted)
    failure_ci <- binomial_interval(failures, attempted)
    singular_ci <- binomial_interval(
      singular_count,
      singular_trials
    )

    data.frame(
      condition_id = condition_id,
      n_clusters = x$n_clusters[1L],
      random_slope_sd = x$random_slope_sd[1L],
      contamination = x$contamination[1L],
      contamination_label = x$contamination_label[1L],
      reps = attempted,
      usable_reps = usable,
      failure_reps = failures,
      usable_fit_rate = 100 * usable / attempted,
      usable_fit_rate_ci_low = usable_ci["lower"],
      usable_fit_rate_ci_high = usable_ci["upper"],
      failure_rate = 100 * failures / attempted,
      failure_rate_ci_low = failure_ci["lower"],
      failure_rate_ci_high = failure_ci["upper"],
      optimizer_warning_reps = sum(
        has_text(optimizer_warning)
      ),
      optimizer_warning_rep_rate = 100 * mean(
        has_text(optimizer_warning)
      ),
      nonzero_optimizer_code_reps = sum(
        !is.na(optimizer_code) & optimizer_code != 0
      ),
      convergence_failure_reps = sum(
        !is.na(converged) & !converged
      ),
      error_reps = sum(has_text(error)),
      any_warning_reps = sum(has_text(warning)),
      singular_reps = singular_count,
      singular_rate = if (
        singular_trials == 0L
      ) {
        NA_real_
      } else {
        100 * singular_count / singular_trials
      },
      singular_rate_ci_low = singular_ci["lower"],
      singular_rate_ci_high = singular_ci["upper"],
      mean_estimated_random_intercept_sd = mean_or_na(
        fitted_ri_sd
      ),
      mean_estimated_random_slope_sd = mean_or_na(
        fitted_rs_sd
      ),
      minimum_estimated_random_slope_sd = min_or_na(
        fitted_rs_sd
      ),
      p05_estimated_random_slope_sd = quantile_or_na(
        fitted_rs_sd,
        0.05
      ),
      median_estimated_random_slope_sd = quantile_or_na(
        fitted_rs_sd,
        0.50
      ),
      p95_estimated_random_slope_sd = quantile_or_na(
        fitted_rs_sd,
        0.95
      ),
      maximum_estimated_random_slope_sd = max_or_na(
        fitted_rs_sd
      ),
      mean_runtime_sec = mean_or_na(x$runtime_sec),
      maximum_runtime_sec = max_or_na(x$runtime_sec),
      meets_95_percent_usability =
        100 * usable / attempted >= 95,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

make_crn_audit <- function(checkpoints, replicates, tolerance = 1e-12) {
  complete <- checkpoints[
    vapply(
      checkpoints,
      function(x) identical(x$status, "complete"),
      logical(1)
    )
  ]

  seed_rows <- do.call(
    rbind,
    lapply(complete, function(checkpoint) {
      data.frame(
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

  groups <- unique(seed_rows[c("n_clusters", "replicate")])
  groups <- groups[
    order(groups$n_clusters, groups$replicate),
    ,
    drop = FALSE
  ]

  rows <- lapply(seq_len(nrow(groups)), function(i) {
    n_clusters <- groups$n_clusters[i]
    replicate_id <- groups$replicate[i]

    seed_group <- seed_rows[
      seed_rows$n_clusters == n_clusters &
        seed_rows$replicate == replicate_id,
      ,
      drop = FALSE
    ]

    x <- replicates[
      replicates$n_clusters == n_clusters &
        replicates$replicate == replicate_id,
      c(
        "random_slope_sd",
        "contamination",
        "realized_mean_slope",
        "realized_random_slope_sd"
      ),
      drop = FALSE
    ]

    contamination_diff <- numeric(0)

    for (slope_sd in c(0.05, 0.10)) {
      y <- x[x$random_slope_sd == slope_sd, , drop = FALSE]

      if (nrow(y) == 2L) {
        contamination_diff <- c(
          contamination_diff,
          diff(range(y$realized_mean_slope)),
          diff(range(y$realized_random_slope_sd))
        )
      } else {
        contamination_diff <- c(
          contamination_diff,
          Inf,
          Inf
        )
      }
    }

    low <- x[
      x$random_slope_sd == 0.05 &
        x$contamination == "none",
      ,
      drop = FALSE
    ]
    high <- x[
      x$random_slope_sd == 0.10 &
        x$contamination == "none",
      ,
      drop = FALSE
    ]

    mean_scale_diff <- if (
      nrow(low) == 1L && nrow(high) == 1L
    ) {
      abs(
        high$realized_mean_slope -
          2 * low$realized_mean_slope
      )
    } else {
      Inf
    }

    sd_scale_diff <- if (
      nrow(low) == 1L && nrow(high) == 1L
    ) {
      abs(
        high$realized_random_slope_sd -
          2 * low$realized_random_slope_sd
      )
    } else {
      Inf
    }

    max_contam_diff <- max(contamination_diff)
    unique_seed_count <- length(unique(seed_group$replicate_seed))

    data.frame(
      n_clusters = n_clusters,
      replicate = replicate_id,
      condition_count = nrow(x),
      unique_seed_count = unique_seed_count,
      max_contamination_difference = max_contam_diff,
      slope_mean_rescaling_difference = mean_scale_diff,
      slope_sd_rescaling_difference = sd_scale_diff,
      passed = nrow(x) == 4L &&
        unique_seed_count == 1L &&
        max_contam_diff <= tolerance &&
        mean_scale_diff <= tolerance &&
        sd_scale_diff <= tolerance,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

make_message_frequency <- function(replicates) {
  rows <- list()
  index <- 0L

  for (message_type in c(
    "warning",
    "optimizer_warning",
    "error"
  )) {
    values <- column_or_default(
      replicates,
      message_type,
      NA_character_
    )
    keep <- has_text(values)

    if (!any(keep)) next

    x <- data.frame(
      condition_id = replicates$condition_id[keep],
      message_type = message_type,
      message = values[keep],
      stringsAsFactors = FALSE
    )

    counts <- stats::aggregate(
      rep(1L, nrow(x)),
      by = x,
      FUN = sum
    )
    names(counts)[names(counts) == "x"] <- "frequency"

    index <- index + 1L
    rows[[index]] <- counts
  }

  if (length(rows) == 0L) {
    return(data.frame(
      condition_id = character(0),
      message_type = character(0),
      message = character(0),
      frequency = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, rows)
  out <- out[
    order(
      out$condition_id,
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
    "The pkgload package is required to run this script.",
    call. = FALSE
  )
}

if (!requireNamespace("pbkrtest", quietly = TRUE)) {
  stop(
    "The pbkrtest package is required for Kenward-Roger inference.",
    call. = FALSE
  )
}

pkgload::load_all(project_root, quiet = TRUE)

# Configuration
diagnostic_reps <- 250L
minimum_usable_rate <- 95
overwrite_completed <- FALSE
condition_ids_to_run <- NULL

cluster_seeds <- c(
  "10" = 20260830L,
  "20" = 20260831L,
  "40" = 20260901L
)

output_dir <- file.path(
  project_root,
  "data-raw",
  "study2-results",
  "random-slope-convergence-diagnostic"
)

checkpoint_dir <- file.path(output_dir, "conditions")

dir.create(
  checkpoint_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

design <- make_design(
  reps = diagnostic_reps,
  cluster_seeds = cluster_seeds
)

if (!is.null(condition_ids_to_run)) {
  unknown <- setdiff(condition_ids_to_run, design$condition_id)

  if (length(unknown) > 0L) {
    stop(
      paste(
        "Unknown condition IDs:",
        paste(unknown, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

design_path <- file.path(
  output_dir,
  "study2_rs_diagnostic_design.rds"
)

if (file.exists(design_path)) {
  existing_design <- readRDS(design_path)

  if (!identical(existing_design, design)) {
    if (!overwrite_completed) {
      stop(
        paste(
          "The saved diagnostic design differs from the current",
          "design. Set overwrite_completed <- TRUE to replace it."
        ),
        call. = FALSE
      )
    }

    old_checkpoints <- list.files(
      checkpoint_dir,
      pattern = "^condition_S2RSD[0-9]{3}[.]rds$",
      full.names = TRUE
    )

    if (length(old_checkpoints) > 0L) {
      file.remove(old_checkpoints)
    }
  }
}

save_rds_atomic(design, design_path)
write_csv_atomic(
  design,
  file.path(output_dir, "study2_rs_diagnostic_design.csv")
)

description <- read.dcf(file.path(project_root, "DESCRIPTION"))

metadata <- list(
  study = "mmiCATs Study 2 random-slope convergence diagnostic",
  created_at = Sys.time(),
  package_version = unname(description[1L, "Version"]),
  r_version = R.version.string,
  diagnostic_reps = diagnostic_reps,
  minimum_usable_rate = minimum_usable_rate,
  conditions = nrow(design),
  method = "rs",
  cluster_seeds = cluster_seeds,
  common_random_numbers = TRUE,
  overwrite_completed = overwrite_completed,
  condition_ids_to_run = condition_ids_to_run,
  session_info = utils::sessionInfo()
)

save_rds_atomic(
  metadata,
  file.path(output_dir, "study2_rs_diagnostic_metadata.rds")
)

writeLines(
  capture.output(utils::sessionInfo()),
  con = file.path(output_dir, "session_info.txt"),
  useBytes = TRUE
)

# Run conditions
started_all <- Sys.time()

for (i in seq_len(nrow(design))) {
  condition <- design[i, , drop = FALSE]

  if (!is.null(condition_ids_to_run) &&
      !(condition$condition_id %in% condition_ids_to_run)) {
    next
  }

  checkpoint_path <- checkpoint_path_for(
    checkpoint_dir,
    condition$condition_id
  )

  if (file.exists(checkpoint_path) && !overwrite_completed) {
    existing <- read_checkpoint_safely(checkpoint_path)

    if (identical(existing$status, "complete")) {
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
        "Running %s of %s: G = %s, random-slope SD = %s, ",
        "condition = %s, reps = %s."
      ),
      condition$condition_id,
      nrow(design),
      condition$n_clusters,
      format(condition$random_slope_sd, trim = TRUE),
      condition$contamination_label,
      condition$reps
    )
  )

  started <- Sys.time()

  result <- tryCatch(
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
      methods = "rs",
      seed = condition$condition_seed,
      keep_replicates = TRUE
    ),
    error = function(e) e
  )

  completed <- Sys.time()
  elapsed_sec <- as.numeric(
    difftime(completed, started, units = "secs")
  )

  if (inherits(result, "error")) {
    checkpoint <- list(
      status = "error",
      condition = condition,
      result = NULL,
      error = conditionMessage(result),
      started_at = started,
      completed_at = completed,
      elapsed_sec = elapsed_sec
    )

    save_rds_atomic(checkpoint, checkpoint_path)
    save_status(checkpoint_dir, output_dir)

    message(
      sprintf(
        "Condition %s failed: %s",
        condition$condition_id,
        conditionMessage(result)
      )
    )

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
    started_at = started,
    completed_at = completed,
    elapsed_sec = elapsed_sec
  )

  save_rds_atomic(checkpoint, checkpoint_path)
  current_status <- save_status(checkpoint_dir, output_dir)

  message(
    sprintf(
      paste0(
        "Completed %s in %.2f minutes. Overall progress: ",
        "%s of %s conditions; %.2f elapsed hours."
      ),
      condition$condition_id,
      elapsed_sec / 60,
      sum(current_status$status == "complete", na.rm = TRUE),
      nrow(design),
      as.numeric(
        difftime(Sys.time(), started_all, units = "hours")
      )
    )
  )
}

# Combine checkpoints
checkpoints <- collect_checkpoints(checkpoint_dir)
status <- collect_status(checkpoints)

if (nrow(status) == 0L) {
  stop("No diagnostic checkpoints were found.", call. = FALSE)
}

complete <- checkpoints[
  vapply(
    checkpoints,
    function(x) identical(x$status, "complete"),
    logical(1)
  )
]

if (length(complete) == 0L) {
  stop(
    "No diagnostic conditions completed successfully.",
    call. = FALSE
  )
}

summary_results <- do.call(
  rbind,
  lapply(complete, function(x) x$result$summary)
)

replicate_results <- do.call(
  rbind,
  lapply(complete, function(x) x$result$replicates)
)

rownames(summary_results) <- NULL
rownames(replicate_results) <- NULL

diagnostics <- summarize_diagnostics(replicate_results)
crn_audit <- make_crn_audit(checkpoints, replicate_results)
message_frequency <- make_message_frequency(replicate_results)

flagged <- replicate_results[
  !replicate_results$fit_success |
    replicate_results$singular |
    has_text(replicate_results$optimizer_warning) |
    (
      !is.na(replicate_results$optimizer_code) &
        replicate_results$optimizer_code != 0
    ) |
    has_text(replicate_results$error),
  ,
  drop = FALSE
]

flagged_csv <- flagged
if ("cluster_diagnostics" %in% names(flagged_csv)) {
  flagged_csv$cluster_diagnostics <- NULL
}

expected_summary_rows <- nrow(design)
expected_replicate_rows <- nrow(design) * diagnostic_reps

validation <- data.frame(
  check = c(
    "all_conditions_completed",
    "combined_dimensions_correct",
    "all_crn_checks_passed",
    "all_conditions_meet_95_percent_usability"
  ),
  passed = c(
    sum(status$status == "complete") == nrow(design),
    nrow(summary_results) == expected_summary_rows &&
      nrow(replicate_results) == expected_replicate_rows,
    nrow(crn_audit) ==
      length(cluster_seeds) * diagnostic_reps &&
      all(crn_audit$passed),
    nrow(diagnostics) == nrow(design) &&
      all(
        diagnostics$usable_fit_rate >= minimum_usable_rate
      )
  ),
  details = c(
    sprintf(
      "%s of %s conditions complete",
      sum(status$status == "complete"),
      nrow(design)
    ),
    sprintf(
      "%s summary rows and %s replicate rows; expected %s and %s",
      nrow(summary_results),
      nrow(replicate_results),
      expected_summary_rows,
      expected_replicate_rows
    ),
    sprintf(
      "%s of %s CRN checks passed",
      sum(crn_audit$passed),
      nrow(crn_audit)
    ),
    sprintf(
      "%s of %s conditions meet the %.1f percent criterion",
      sum(
        diagnostics$usable_fit_rate >= minimum_usable_rate
      ),
      nrow(diagnostics),
      minimum_usable_rate
    )
  ),
  stringsAsFactors = FALSE
)

combined <- list(
  design = design,
  status = status,
  summary = summary_results,
  replicates = replicate_results,
  diagnostics = diagnostics,
  flagged_replicates = flagged,
  common_random_number_audit = crn_audit,
  message_frequency = message_frequency,
  validation = validation,
  metadata = metadata
)

# Save outputs
save_rds_atomic(
  status,
  file.path(output_dir, "study2_rs_diagnostic_status.rds")
)
save_rds_atomic(
  summary_results,
  file.path(output_dir, "study2_rs_diagnostic_summary.rds")
)
save_rds_atomic(
  replicate_results,
  file.path(output_dir, "study2_rs_diagnostic_replicates.rds")
)
save_rds_atomic(
  diagnostics,
  file.path(output_dir, "study2_rs_diagnostic_diagnostics.rds")
)
save_rds_atomic(
  flagged,
  file.path(
    output_dir,
    "study2_rs_diagnostic_flagged_replicates.rds"
  )
)
save_rds_atomic(
  crn_audit,
  file.path(output_dir, "study2_rs_diagnostic_crn_audit.rds")
)
save_rds_atomic(
  message_frequency,
  file.path(
    output_dir,
    "study2_rs_diagnostic_message_frequency.rds"
  )
)
save_rds_atomic(
  validation,
  file.path(output_dir, "study2_rs_diagnostic_validation.rds")
)
save_rds_atomic(
  combined,
  file.path(
    output_dir,
    "study2_random_slope_convergence_diagnostic.rds"
  )
)

write_csv_atomic(
  status,
  file.path(output_dir, "study2_rs_diagnostic_status.csv")
)
write_csv_atomic(
  summary_results,
  file.path(output_dir, "study2_rs_diagnostic_summary.csv")
)
write_csv_atomic(
  diagnostics,
  file.path(output_dir, "study2_rs_diagnostic_diagnostics.csv")
)
write_csv_atomic(
  flagged_csv,
  file.path(
    output_dir,
    "study2_rs_diagnostic_flagged_replicates.csv"
  )
)
write_csv_atomic(
  crn_audit,
  file.path(output_dir, "study2_rs_diagnostic_crn_audit.csv")
)
write_csv_atomic(
  message_frequency,
  file.path(
    output_dir,
    "study2_rs_diagnostic_message_frequency.csv"
  )
)
write_csv_atomic(
  validation,
  file.path(output_dir, "study2_rs_diagnostic_validation.csv")
)

# Console summary
message("")
message("Study 2 random-slope convergence diagnostic complete.")
message(
  sprintf(
    "Completed conditions: %s of %s.",
    sum(status$status == "complete"),
    nrow(design)
  )
)
message(
  sprintf(
    "Total elapsed time: %.2f hours.",
    sum(
      status$elapsed_sec[status$status == "complete"],
      na.rm = TRUE
    ) / 3600
  )
)
message(paste("Results saved to:", output_dir))

message("")
message("Diagnostic validation checks:")
print(validation, row.names = FALSE)

message("")
message("Random-slope usability and singularity results:")
print(
  diagnostics[
    ,
    c(
      "condition_id",
      "n_clusters",
      "random_slope_sd",
      "contamination_label",
      "usable_reps",
      "usable_fit_rate",
      "failure_reps",
      "optimizer_warning_reps",
      "nonzero_optimizer_code_reps",
      "singular_reps",
      "singular_rate",
      "mean_estimated_random_slope_sd",
      "meets_95_percent_usability"
    )
  ],
  row.names = FALSE
)

message("")

if (all(validation$passed)) {
  message(
    paste(
      "All random-slope convergence diagnostic criteria passed.",
      "The random-slope/KR comparator is ready for the final",
      "Study 2 simulation."
    )
  )
} else {
  message(
    paste(
      "One or more diagnostic criteria failed.",
      "Review the diagnostics, flagged replicates, and message",
      "frequencies before launching the final simulation."
    )
  )
}
