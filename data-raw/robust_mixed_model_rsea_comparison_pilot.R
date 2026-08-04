# Part 2 of the robust mixed-model runtime pilot
#
# This script regenerates the same 60 Study 2 random-slope pilot datasets used
# for the default RSEn fits. It adds:
#
#   1. robustlmm random slope with setting = "RSEa";
#   2. conventional lmer random slope with Kenward-Roger inference.
#
# The existing RSEn results are not rerun. The new fits are joined to them by
# condition ID, replicate, and replicate seed.
#
# This is a settings, boundary-behavior, and runtime diagnostic. Five
# replications per structural cell are not sufficient to assess bias, Type I
# error, coverage, or power.

project_root <- rmm_find_project_root()
rmm_require_packages()

pkgload::load_all(
  project_root,
  quiet = TRUE
)

overwrite_completed <- FALSE
condition_ids_to_run <- NULL

first_pilot_dir <- file.path(
  project_root,
  "data-raw",
  "robust-mixed-model-results",
  "runtime-pilot"
)

existing_path <- file.path(
  first_pilot_dir,
  "robust_mixed_model_pilot_replicates.rds"
)

if (!file.exists(existing_path)) {
  stop(
    paste(
      "The completed first runtime pilot was not found at:",
      existing_path
    ),
    call. = FALSE
  )
}

existing <- readRDS(existing_path)

rsen <- existing[
  existing$study == "Study 2" &
    existing$model == "study2_robust_rs",
  ,
  drop = FALSE
]

rsen <- rsen[
  order(
    rsen$condition_id,
    rsen$replicate
  ),
  ,
  drop = FALSE
]

expected_columns <- c(
  "condition_id",
  "replicate",
  "replicate_seed",
  "n_clusters",
  "cluster_size",
  "random_slope_sd",
  "contamination",
  "contamination_label",
  "beta"
)

missing_columns <- setdiff(
  expected_columns,
  names(rsen)
)

if (length(missing_columns) > 0L) {
  stop(
    paste(
      "The first-pilot results are missing required columns:",
      paste(missing_columns, collapse = ", ")
    ),
    call. = FALSE
  )
}

if (nrow(rsen) != 60L) {
  stop(
    paste(
      "Expected 60 Study 2 RSEn random-slope rows;",
      "found",
      nrow(rsen),
      "."
    ),
    call. = FALSE
  )
}

key <- paste(
  rsen$condition_id,
  rsen$replicate,
  rsen$replicate_seed,
  sep = "::"
)

if (anyDuplicated(key)) {
  stop(
    "The first-pilot RSEn replicate keys are not unique.",
    call. = FALSE
  )
}

design_columns <- c(
  "condition_id",
  "n_clusters",
  "cluster_size",
  "random_slope_sd",
  "contamination",
  "contamination_label",
  "beta"
)

design <- unique(
  rsen[, design_columns, drop = FALSE]
)
design <- design[
  order(design$condition_id),
  ,
  drop = FALSE
]
rownames(design) <- NULL

if (nrow(design) != 12L) {
  stop(
    paste(
      "Expected 12 structural random-slope conditions;",
      "found",
      nrow(design),
      "."
    ),
    call. = FALSE
  )
}

output_dir <- file.path(
  project_root,
  "data-raw",
  "robust-mixed-model-results",
  "random-slope-settings-pilot"
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

rmm_write_csv_atomic(
  design,
  file.path(
    output_dir,
    "random_slope_settings_pilot_design.csv"
  )
)

rmm_save_rds_atomic(
  design,
  file.path(
    output_dir,
    "random_slope_settings_pilot_design.rds"
  )
)

source_files <- c(
  robust_mixed_model_pilot_helpers = file.path(
    project_root,
    "data-raw",
    "robust_mixed_model_pilot_helpers.R"
  ),
  robust_mixed_model_rsea_pilot_helpers = file.path(
    project_root,
    "data-raw",
    "robust_mixed_model_rsea_pilot_helpers.R"
  ),
  robust_mixed_model_rsea_comparison_pilot = file.path(
    project_root,
    "data-raw",
    "robust_mixed_model_rsea_comparison_pilot.R"
  )
)

source_checksums <- rmm_source_checksums(
  project_root = project_root,
  files = source_files
)

rmm_write_csv_atomic(
  source_checksums,
  file.path(
    output_dir,
    "random_slope_settings_source_checksums.csv"
  )
)

metadata <- list(
  purpose = paste(
    "Compare RSEa with the existing RSEn robust",
    "random-slope fits and with conventional",
    "random-slope lmer fits on the same 60 datasets."
  ),
  robustlmm_version = as.character(
    utils::packageVersion("robustlmm")
  ),
  robust_method = "DAStau",
  robust_settings = c("RSEn", "RSEa"),
  robust_inference = paste(
    "summary(fit, df = 'satterthwaite')",
    "with the default covariance."
  ),
  conventional_inference = "Kenward-Roger",
  source_checksums = source_checksums,
  session_info = utils::sessionInfo()
)

rmm_save_rds_atomic(
  metadata,
  file.path(
    output_dir,
    "random_slope_settings_pilot_metadata.rds"
  )
)

if (!is.null(condition_ids_to_run)) {
  unknown <- setdiff(
    condition_ids_to_run,
    design$condition_id
  )

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

pilot_started <- Sys.time()

for (condition_index in seq_len(nrow(design))) {
  condition <- design[
    condition_index,
    ,
    drop = FALSE
  ]

  if (!is.null(condition_ids_to_run) &&
      !(condition$condition_id %in%
          condition_ids_to_run)) {
    next
  }

  checkpoint_path <- file.path(
    checkpoint_dir,
    paste0(
      "condition_",
      condition$condition_id,
      ".rds"
    )
  )

  if (file.exists(checkpoint_path) &&
      !overwrite_completed) {
    checkpoint <- tryCatch(
      readRDS(checkpoint_path),
      error = function(e) NULL
    )

    if (!is.null(checkpoint) &&
        identical(checkpoint$status, "complete")) {
      message(
        sprintf(
          "Skipping completed condition %s.",
          condition$condition_id
        )
      )
      next
    }
  }

  condition_rows <- rsen[
    rsen$condition_id ==
      condition$condition_id,
    ,
    drop = FALSE
  ]
  condition_rows <- condition_rows[
    order(condition_rows$replicate),
    ,
    drop = FALSE
  ]

  if (nrow(condition_rows) != 5L) {
    stop(
      paste(
        "Expected five first-pilot rows for",
        condition$condition_id,
        "but found",
        nrow(condition_rows),
        "."
      ),
      call. = FALSE
    )
  }

  message(
    sprintf(
      paste0(
        "Running %s of %s: G = %s, slope SD = %s, ",
        "%s, 5 paired datasets."
      ),
      condition$condition_id,
      nrow(design),
      condition$n_clusters,
      format(
        condition$random_slope_sd,
        trim = TRUE
      ),
      condition$contamination_label
    )
  )

  started_at <- Sys.time()

  condition_result <- tryCatch(
    {
      rows <- list()
      row_index <- 0L

      for (
        replicate_index in
        seq_len(nrow(condition_rows))
      ) {
        original <- condition_rows[
          replicate_index,
          ,
          drop = FALSE
        ]

        set.seed(original$replicate_seed)

        dat <- rmm_simulate_study2(
          n_clusters = original$n_clusters,
          random_slope_sd =
            original$random_slope_sd,
          contamination =
            original$contamination
        )

        rsea_result <-
          rmm_fit_and_extract_rlmer_setting(
            dat = dat,
            beta = original$beta,
            setting = "RSEa",
            return_fit = FALSE
          )

        row_index <- row_index + 1L
        rows[[row_index]] <-
          rmm_comparison_result_to_row(
            result = rsea_result,
            condition_id =
              original$condition_id,
            replicate =
              original$replicate,
            replicate_seed =
              original$replicate_seed,
            n_clusters =
              original$n_clusters,
            random_slope_sd =
              original$random_slope_sd,
            contamination =
              original$contamination,
            contamination_label =
              original$contamination_label,
            beta = original$beta
          )

        conventional_result <-
          rmm_fit_and_extract_lmer_rs(
            dat = dat,
            beta = original$beta,
            return_fit = FALSE
          )

        row_index <- row_index + 1L
        rows[[row_index]] <-
          rmm_comparison_result_to_row(
            result = conventional_result,
            condition_id =
              original$condition_id,
            replicate =
              original$replicate,
            replicate_seed =
              original$replicate_seed,
            n_clusters =
              original$n_clusters,
            random_slope_sd =
              original$random_slope_sd,
            contamination =
              original$contamination,
            contamination_label =
              original$contamination_label,
            beta = original$beta
          )

        rm(
          dat,
          rsea_result,
          conventional_result
        )
        gc(verbose = FALSE)
      }

      rmm_bind_rows_fill(rows)
    },
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

  checkpoint <- if (
    inherits(condition_result, "error")
  ) {
    list(
      status = "error",
      condition = condition,
      results = NULL,
      error = conditionMessage(
        condition_result
      ),
      started_at = started_at,
      completed_at = completed_at,
      elapsed_sec = elapsed_sec
    )
  } else {
    list(
      status = "complete",
      condition = condition,
      results = condition_result,
      error = NA_character_,
      started_at = started_at,
      completed_at = completed_at,
      elapsed_sec = elapsed_sec
    )
  }

  rmm_save_rds_atomic(
    checkpoint,
    checkpoint_path
  )

  completed_paths <- list.files(
    checkpoint_dir,
    pattern = "^condition_RMM[0-9]{3}[.]rds$",
    full.names = TRUE
  )

  completed_count <- sum(vapply(
    completed_paths,
    function(path) {
      object <- tryCatch(
        readRDS(path),
        error = function(e) NULL
      )
      !is.null(object) &&
        identical(object$status, "complete")
    },
    logical(1)
  ))

  message(
    sprintf(
      paste0(
        "Completed %s in %.2f minutes. Progress: ",
        "%s of %s conditions; %.2f elapsed hours."
      ),
      condition$condition_id,
      elapsed_sec / 60,
      completed_count,
      nrow(design),
      as.numeric(
        difftime(
          Sys.time(),
          pilot_started,
          units = "hours"
        )
      )
    )
  )
}

checkpoint_paths <- sort(list.files(
  checkpoint_dir,
  pattern = "^condition_RMM[0-9]{3}[.]rds$",
  full.names = TRUE
))

checkpoints <- lapply(
  checkpoint_paths,
  function(path) {
    tryCatch(
      readRDS(path),
      error = function(e) {
        list(
          status = "unreadable",
          condition = NULL,
          results = NULL,
          error = conditionMessage(e),
          elapsed_sec = NA_real_
        )
      }
    )
  }
)

completed <- checkpoints[
  vapply(
    checkpoints,
    function(x) {
      identical(x$status, "complete")
    },
    logical(1)
  )
]

if (length(completed) == 0L) {
  stop(
    "No random-slope settings conditions completed.",
    call. = FALSE
  )
}

new_replicates <- rmm_bind_rows_fill(
  lapply(
    completed,
    function(x) x$results
  )
)

status_rows <- lapply(
  seq_len(nrow(design)),
  function(index) {
    condition <- design[index, , drop = FALSE]
    matching <- checkpoints[
      vapply(
        checkpoints,
        function(x) {
          !is.null(x$condition) &&
            identical(
              as.character(
                x$condition$condition_id
              ),
              as.character(
                condition$condition_id
              )
            )
        },
        logical(1)
      )
    ]

    if (length(matching) == 0L) {
      return(data.frame(
        condition,
        status = "not_started",
        error = NA_character_,
        elapsed_sec = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    checkpoint <- matching[[1L]]

    data.frame(
      condition,
      status = checkpoint$status,
      error = checkpoint$error,
      elapsed_sec = checkpoint$elapsed_sec,
      stringsAsFactors = FALSE
    )
  }
)

status <- do.call(rbind, status_rows)
rownames(status) <- NULL

rsen_selected <- data.frame(
  condition_id = rsen$condition_id,
  replicate = rsen$replicate,
  replicate_seed = rsen$replicate_seed,
  n_clusters = rsen$n_clusters,
  cluster_size = rsen$cluster_size,
  random_slope_sd = rsen$random_slope_sd,
  contamination = rsen$contamination,
  contamination_label =
    rsen$contamination_label,
  beta = rsen$beta,
  method = "rlmer_RSEn",
  method_label =
    "Robust random slope (RSEn)",
  setting = "RSEn",
  estimate = rsen$estimate,
  std_error = rsen$std_error,
  df = rsen$df,
  statistic = rsen$statistic,
  p_value = rsen$p_value,
  conf_low = rsen$conf_low,
  conf_high = rsen$conf_high,
  reject = rsen$reject,
  cover = rsen$cover,
  fit_available = rsen$fit_available,
  inference_complete =
    rsen$inference_complete,
  convergence_code =
    rsen$convergence_code,
  convergence_code_zero =
    rsen$convergence_code_zero,
  boundary_fit = rsen$boundary_fit,
  estimated_random_intercept_sd =
    rsen$estimated_random_intercept_sd,
  estimated_random_slope_sd =
    rsen$estimated_random_slope_sd,
  estimated_residual_sd =
    rsen$estimated_residual_sd,
  residual_weight_minimum =
    rsen$residual_weight_minimum,
  residual_weight_mean =
    rsen$residual_weight_mean,
  residual_weight_prop_below_0_5 =
    rsen$residual_weight_prop_below_0_5,
  residual_weight_prop_below_0_8 =
    rsen$residual_weight_prop_below_0_8,
  residual_weight_count =
    rsen$residual_weight_count,
  random_effect_weight_minimum =
    rsen$random_effect_weight_minimum,
  random_effect_weight_mean =
    rsen$random_effect_weight_mean,
  random_effect_weight_prop_below_0_5 =
    rsen$random_effect_weight_prop_below_0_5,
  random_effect_weight_prop_below_0_8 =
    rsen$random_effect_weight_prop_below_0_8,
  random_effect_weight_count =
    rsen$random_effect_weight_count,
  fit_warning = rsen$fit_warning,
  fit_message = rsen$fit_message,
  fit_error = rsen$fit_error,
  inference_warning =
    rsen$inference_warning,
  inference_message =
    rsen$inference_message,
  inference_error =
    rsen$inference_error,
  process_warning =
    rsen$process_warning,
  process_message =
    rsen$process_message,
  process_error = rsen$process_error,
  variance_component_error =
    rsen$variance_component_error,
  summary_column_names =
    rsen$summary_column_names,
  fit_elapsed_sec = rsen$fit_elapsed_sec,
  inference_elapsed_sec =
    rsen$inference_elapsed_sec,
  process_elapsed_sec =
    rsen$process_elapsed_sec,
  total_elapsed_sec =
    rsen$total_elapsed_sec,
  usable = rsen$usable,
  stringsAsFactors = FALSE
)

all_replicates <- rmm_bind_rows_fill(
  list(
    rsen_selected,
    new_replicates
  )
)

all_replicates <- all_replicates[
  order(
    all_replicates$condition_id,
    all_replicates$replicate,
    all_replicates$method
  ),
  ,
  drop = FALSE
]

diagnostic_groups <- split(
  all_replicates,
  interaction(
    all_replicates$condition_id,
    all_replicates$method,
    drop = TRUE
  )
)

diagnostics <- do.call(
  rbind,
  lapply(
    diagnostic_groups,
    function(x) {
      convergence_codes <- x$convergence_code[
        is.finite(x$convergence_code)
      ]
      boundary_values <- x$boundary_fit[
        !is.na(x$boundary_fit)
      ]

      data.frame(
        condition_id = x$condition_id[1L],
        n_clusters = x$n_clusters[1L],
        random_slope_sd =
          x$random_slope_sd[1L],
        contamination =
          x$contamination[1L],
        contamination_label =
          x$contamination_label[1L],
        method = x$method[1L],
        method_label = x$method_label[1L],
        setting = x$setting[1L],
        reps = nrow(x),
        fit_available_rate = 100 * mean(
          x$fit_available
        ),
        inference_complete_rate = 100 * mean(
          x$inference_complete
        ),
        usable_rate = 100 * mean(
          x$usable
        ),
        convergence_failure_rate = if (
          length(convergence_codes) == 0L
        ) {
          NA_real_
        } else {
          100 * mean(convergence_codes != 0)
        },
        boundary_fit_rate = if (
          length(boundary_values) == 0L
        ) {
          NA_real_
        } else {
          100 * mean(boundary_values)
        },
        mean_estimated_random_slope_sd =
          rmm_mean_or_na(
            x$estimated_random_slope_sd
          ),
        median_estimated_random_slope_sd =
          stats::median(
            x$estimated_random_slope_sd,
            na.rm = TRUE
          ),
        mean_estimate = rmm_mean_or_na(
          x$estimate
        ),
        mean_std_error = rmm_mean_or_na(
          x$std_error
        ),
        mean_df = rmm_mean_or_na(
          x$df
        ),
        mean_fit_sec = rmm_mean_or_na(
          x$fit_elapsed_sec
        ),
        mean_inference_sec = rmm_mean_or_na(
          x$inference_elapsed_sec
        ),
        mean_total_sec = rmm_mean_or_na(
          x$total_elapsed_sec
        ),
        maximum_total_sec = rmm_max_or_na(
          x$total_elapsed_sec
        ),
        fit_warning_rate = 100 * mean(
          rmm_has_text(x$fit_warning)
        ),
        fit_message_rate = 100 * mean(
          rmm_has_text(x$fit_message)
        ),
        inference_warning_rate = 100 * mean(
          rmm_has_text(x$inference_warning)
        ),
        inference_message_rate = 100 * mean(
          rmm_has_text(x$inference_message)
        ),
        fit_error_rate = 100 * mean(
          rmm_has_text(x$fit_error)
        ),
        inference_error_rate = 100 * mean(
          rmm_has_text(x$inference_error)
        ),
        process_warning_rate = 100 * mean(
          rmm_has_text(x$process_warning)
        ),
        stringsAsFactors = FALSE
      )
    }
  )
)
rownames(diagnostics) <- NULL

key_columns <- c(
  "condition_id",
  "replicate",
  "replicate_seed"
)

wide_source <- all_replicates[
  ,
  c(
    key_columns,
    "n_clusters",
    "random_slope_sd",
    "contamination",
    "contamination_label",
    "method",
    "estimate",
    "std_error",
    "df",
    "boundary_fit",
    "estimated_random_slope_sd",
    "total_elapsed_sec",
    "usable"
  )
]

make_method_frame <- function(method_name,
                              prefix) {
  x <- wide_source[
    wide_source$method == method_name,
    ,
    drop = FALSE
  ]

  value_columns <- setdiff(
    names(x),
    c(
      key_columns,
      "n_clusters",
      "random_slope_sd",
      "contamination",
      "contamination_label",
      "method"
    )
  )

  names(x)[match(
    value_columns,
    names(x)
  )] <- paste0(prefix, value_columns)

  x$method <- NULL
  x
}

wide <- Reduce(
  function(x, y) {
    merge(
      x,
      y,
      by = c(
        key_columns,
        "n_clusters",
        "random_slope_sd",
        "contamination",
        "contamination_label"
      ),
      all = TRUE,
      sort = FALSE
    )
  },
  list(
    make_method_frame(
      "rlmer_RSEn",
      "rsen_"
    ),
    make_method_frame(
      "rlmer_RSEa",
      "rsea_"
    ),
    make_method_frame(
      "lmer_kr",
      "lmer_"
    )
  )
)

wide <- wide[
  order(
    wide$condition_id,
    wide$replicate
  ),
  ,
  drop = FALSE
]

wide$rsea_minus_rsen_slope_sd <-
  wide$rsea_estimated_random_slope_sd -
  wide$rsen_estimated_random_slope_sd

wide$rsea_minus_rsen_estimate <-
  wide$rsea_estimate -
  wide$rsen_estimate

wide$rsea_to_rsen_se_ratio <-
  wide$rsea_std_error /
  wide$rsen_std_error

wide$lmer_minus_rsea_slope_sd <-
  wide$lmer_estimated_random_slope_sd -
  wide$rsea_estimated_random_slope_sd

wide$rsea_to_rsen_runtime_ratio <-
  wide$rsea_total_elapsed_sec /
  wide$rsen_total_elapsed_sec

wide$boundary_transition <- paste0(
  "RSEn_",
  ifelse(
    wide$rsen_boundary_fit,
    "boundary",
    "positive"
  ),
  "__RSEa_",
  ifelse(
    wide$rsea_boundary_fit,
    "boundary",
    "positive"
  )
)

transition_groups <- split(
  wide,
  interaction(
    wide$n_clusters,
    wide$random_slope_sd,
    wide$contamination,
    drop = TRUE
  )
)

boundary_transitions <- do.call(
  rbind,
  lapply(
    transition_groups,
    function(x) {
      counts <- table(
        factor(
          x$boundary_transition,
          levels = c(
            "RSEn_boundary__RSEa_boundary",
            "RSEn_boundary__RSEa_positive",
            "RSEn_positive__RSEa_boundary",
            "RSEn_positive__RSEa_positive"
          )
        )
      )

      data.frame(
        n_clusters = x$n_clusters[1L],
        random_slope_sd =
          x$random_slope_sd[1L],
        contamination =
          x$contamination[1L],
        contamination_label =
          x$contamination_label[1L],
        rsen_boundary_rsea_boundary =
          as.integer(counts[1L]),
        rsen_boundary_rsea_positive =
          as.integer(counts[2L]),
        rsen_positive_rsea_boundary =
          as.integer(counts[3L]),
        rsen_positive_rsea_positive =
          as.integer(counts[4L]),
        stringsAsFactors = FALSE
      )
    }
  )
)
rownames(boundary_transitions) <- NULL

overall_transition_counts <- table(
  factor(
    wide$boundary_transition,
    levels = c(
      "RSEn_boundary__RSEa_boundary",
      "RSEn_boundary__RSEa_positive",
      "RSEn_positive__RSEa_boundary",
      "RSEn_positive__RSEa_positive"
    )
  )
)

overall_boundary_transitions <- data.frame(
  transition = names(overall_transition_counts),
  count = as.integer(
    overall_transition_counts
  ),
  percent = 100 * as.integer(
    overall_transition_counts
  ) / nrow(wide),
  stringsAsFactors = FALSE
)

paired_groups <- split(
  wide,
  interaction(
    wide$n_clusters,
    wide$random_slope_sd,
    wide$contamination,
    drop = TRUE
  )
)

paired_summary <- do.call(
  rbind,
  lapply(
    paired_groups,
    function(x) {
      data.frame(
        n_clusters = x$n_clusters[1L],
        random_slope_sd =
          x$random_slope_sd[1L],
        contamination =
          x$contamination[1L],
        contamination_label =
          x$contamination_label[1L],
        reps = nrow(x),
        rsen_boundary_rate = 100 * mean(
          x$rsen_boundary_fit
        ),
        rsea_boundary_rate = 100 * mean(
          x$rsea_boundary_fit
        ),
        lmer_boundary_rate = 100 * mean(
          x$lmer_boundary_fit
        ),
        rsen_mean_slope_sd = rmm_mean_or_na(
          x$rsen_estimated_random_slope_sd
        ),
        rsea_mean_slope_sd = rmm_mean_or_na(
          x$rsea_estimated_random_slope_sd
        ),
        lmer_mean_slope_sd = rmm_mean_or_na(
          x$lmer_estimated_random_slope_sd
        ),
        mean_rsea_minus_rsen_slope_sd =
          rmm_mean_or_na(
            x$rsea_minus_rsen_slope_sd
          ),
        mean_abs_rsea_minus_rsen_estimate =
          rmm_mean_or_na(
            abs(
              x$rsea_minus_rsen_estimate
            )
          ),
        mean_rsea_to_rsen_se_ratio =
          rmm_mean_or_na(
            x$rsea_to_rsen_se_ratio
          ),
        mean_rsea_to_rsen_runtime_ratio =
          rmm_mean_or_na(
            x$rsea_to_rsen_runtime_ratio
          ),
        stringsAsFactors = FALSE
      )
    }
  )
)
rownames(paired_summary) <- NULL

runtime_source <- diagnostics[
  diagnostics$method %in% c(
    "rlmer_RSEn",
    "rlmer_RSEa",
    "lmer_kr"
  ),
  ,
  drop = FALSE
]

runtime_source$projected_full_fits <- 4000L
runtime_source$projected_full_seconds <-
  runtime_source$mean_total_sec *
  runtime_source$projected_full_fits

runtime_projection <- stats::aggregate(
  runtime_source$projected_full_seconds,
  by = list(
    method = runtime_source$method,
    method_label = runtime_source$method_label
  ),
  FUN = sum
)

names(runtime_projection)[3L] <-
  "projected_full_seconds"

runtime_projection$projected_full_hours <-
  runtime_projection$projected_full_seconds /
  3600
runtime_projection$projected_full_days <-
  runtime_projection$projected_full_hours /
  24

runtime_projection <- runtime_projection[
  order(runtime_projection$method),
  ,
  drop = FALSE
]

message_columns <- c(
  "fit_warning",
  "fit_message",
  "fit_error",
  "inference_warning",
  "inference_message",
  "inference_error",
  "process_warning",
  "process_message",
  "process_error",
  "variance_component_error"
)

message_rows <- list()
message_index <- 0L

for (column in message_columns) {
  values <- all_replicates[[column]]
  keep <- rmm_has_text(values)

  if (!any(keep)) {
    next
  }

  counts <- sort(
    table(values[keep]),
    decreasing = TRUE
  )

  message_index <- message_index + 1L
  message_rows[[message_index]] <- data.frame(
    source = column,
    message = names(counts),
    count = as.integer(counts),
    stringsAsFactors = FALSE
  )
}

message_frequency <- if (
  length(message_rows) == 0L
) {
  data.frame(
    source = character(),
    message = character(),
    count = integer(),
    stringsAsFactors = FALSE
  )
} else {
  do.call(rbind, message_rows)
}

validation <- data.frame(
  check = c(
    "all_12_conditions_completed",
    "new_fit_dimensions_correct",
    "all_60_rsea_rows_present",
    "all_60_conventional_rows_present",
    "all_60_rsen_rows_joined",
    "all_replicate_keys_match",
    "all_new_models_returned_inference",
    "all_rsea_convergence_codes_available"
  ),
  passed = c(
    sum(status$status == "complete") == 12L,
    nrow(new_replicates) == 120L,
    sum(
      new_replicates$method ==
        "rlmer_RSEa"
    ) == 60L,
    sum(
      new_replicates$method ==
        "lmer_kr"
    ) == 60L,
    sum(
      all_replicates$method ==
        "rlmer_RSEn"
    ) == 60L,
    nrow(wide) == 60L &&
      !anyDuplicated(
        paste(
          wide$condition_id,
          wide$replicate,
          wide$replicate_seed,
          sep = "::"
        )
      ),
    all(new_replicates$inference_complete),
    all(is.finite(
      new_replicates$convergence_code[
        new_replicates$method ==
          "rlmer_RSEa"
      ]
    ))
  ),
  details = c(
    sprintf(
      "%s of 12 conditions complete",
      sum(status$status == "complete")
    ),
    sprintf(
      "%s new rows; expected 120",
      nrow(new_replicates)
    ),
    sprintf(
      "%s RSEa rows; expected 60",
      sum(
        new_replicates$method ==
          "rlmer_RSEa"
      )
    ),
    sprintf(
      "%s conventional rows; expected 60",
      sum(
        new_replicates$method ==
          "lmer_kr"
      )
    ),
    sprintf(
      "%s RSEn rows; expected 60",
      sum(
        all_replicates$method ==
          "rlmer_RSEn"
      )
    ),
    sprintf(
      "%s paired replicate rows; expected 60",
      nrow(wide)
    ),
    sprintf(
      "%s of %s new fits returned inference",
      sum(new_replicates$inference_complete),
      nrow(new_replicates)
    ),
    sprintf(
      "%s of 60 RSEa fits returned convergence codes",
      sum(is.finite(
        new_replicates$convergence_code[
          new_replicates$method ==
            "rlmer_RSEa"
        ]
      ))
    )
  ),
  stringsAsFactors = FALSE
)

combined <- list(
  design = design,
  status = status,
  validation = validation,
  existing_rsen = rsen_selected,
  new_replicates = new_replicates,
  all_replicates = all_replicates,
  paired_replicates = wide,
  diagnostics = diagnostics,
  paired_summary = paired_summary,
  boundary_transitions =
    boundary_transitions,
  overall_boundary_transitions =
    overall_boundary_transitions,
  runtime_projection = runtime_projection,
  message_frequency = message_frequency,
  metadata = metadata
)

rmm_save_rds_atomic(
  new_replicates,
  file.path(
    output_dir,
    "random_slope_settings_new_replicates.rds"
  )
)
rmm_save_rds_atomic(
  all_replicates,
  file.path(
    output_dir,
    "random_slope_settings_all_replicates.rds"
  )
)
rmm_save_rds_atomic(
  wide,
  file.path(
    output_dir,
    "random_slope_settings_paired_replicates.rds"
  )
)
rmm_save_rds_atomic(
  combined,
  file.path(
    output_dir,
    "random_slope_settings_pilot_results.rds"
  )
)

rmm_write_csv_atomic(
  status,
  file.path(
    output_dir,
    "random_slope_settings_status.csv"
  )
)
rmm_write_csv_atomic(
  validation,
  file.path(
    output_dir,
    "random_slope_settings_validation.csv"
  )
)
rmm_write_csv_atomic(
  new_replicates,
  file.path(
    output_dir,
    "random_slope_settings_new_replicates.csv"
  )
)
rmm_write_csv_atomic(
  wide,
  file.path(
    output_dir,
    "random_slope_settings_paired_replicates.csv"
  )
)
rmm_write_csv_atomic(
  diagnostics,
  file.path(
    output_dir,
    "random_slope_settings_diagnostics.csv"
  )
)
rmm_write_csv_atomic(
  paired_summary,
  file.path(
    output_dir,
    "random_slope_settings_paired_summary.csv"
  )
)
rmm_write_csv_atomic(
  boundary_transitions,
  file.path(
    output_dir,
    "random_slope_settings_boundary_transitions.csv"
  )
)
rmm_write_csv_atomic(
  overall_boundary_transitions,
  file.path(
    output_dir,
    "random_slope_settings_overall_boundary_transitions.csv"
  )
)
rmm_write_csv_atomic(
  runtime_projection,
  file.path(
    output_dir,
    "random_slope_settings_runtime_projection.csv"
  )
)
rmm_write_csv_atomic(
  message_frequency,
  file.path(
    output_dir,
    "random_slope_settings_message_frequency.csv"
  )
)

writeLines(
  capture.output(utils::sessionInfo()),
  con = file.path(
    output_dir,
    "session_info.txt"
  ),
  useBytes = TRUE
)

message("")
message("Random-slope settings comparison pilot complete.")
message(
  sprintf(
    "Completed conditions: %s of 12.",
    sum(status$status == "complete")
  )
)
message(
  sprintf(
    "New fits completed: %s.",
    nrow(new_replicates)
  )
)
message(paste("Results saved to:", output_dir))

message("")
message("Pilot validation:")
print(validation, row.names = FALSE)

message("")
message("Boundary comparison by condition:")
print(
  paired_summary[
    ,
    c(
      "n_clusters",
      "random_slope_sd",
      "contamination_label",
      "rsen_boundary_rate",
      "rsea_boundary_rate",
      "lmer_boundary_rate",
      "rsen_mean_slope_sd",
      "rsea_mean_slope_sd",
      "lmer_mean_slope_sd"
    )
  ],
  row.names = FALSE
)

message("")
message("Overall RSEn-to-RSEa boundary transitions:")
print(
  overall_boundary_transitions,
  row.names = FALSE
)

message("")
message("Projected full Study 2 runtime by method:")
print(
  runtime_projection[
    ,
    c(
      "method_label",
      "projected_full_hours",
      "projected_full_days"
    )
  ],
  row.names = FALSE
)

message("")
message(
  paste(
    "This five-replication paired pilot evaluates",
    "boundary behavior, software behavior, and runtime only.",
    "Do not interpret bias, Type I error, coverage, or power."
  )
)
