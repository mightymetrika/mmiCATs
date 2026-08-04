# Robust mixed-model runtime and computational pilot
#
# This pilot evaluates only the proposed robust mixed-model additions:
#
# Study 1
#   - robust random-intercept model
#
# Study 2
#   - robust random-intercept model, misspecified when slopes vary
#   - robust independent random-intercept and random-slope model
#
# It does not refit CATs, robust CATs, CR2, or the conventional mixed models.
# Beta is fixed at zero because changing the population mean slope does not
# change the model dimensions or covariance structure. Five replications are
# used per structural condition. The pilot is for runtime, extraction,
# convergence, singularity, and warning diagnostics only, not performance
# inference.
#
# Completed condition checkpoints are skipped on rerun.

project_root <- rmm_find_project_root()
rmm_require_packages()

pkgload::load_all(
  project_root,
  quiet = TRUE
)

pilot_reps <- 5L
overwrite_completed <- FALSE
condition_ids_to_run <- NULL

output_dir <- file.path(
  project_root,
  "data-raw",
  "robust-mixed-model-results",
  "runtime-pilot"
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

study1_rows <- list()
index <- 0L

for (n_clusters in c(10L, 20L, 40L)) {
  for (
    contamination in
    c("none", "vertical", "bad_leverage")
  ) {
    index <- index + 1L

    study1_rows[[index]] <- data.frame(
      study = "Study 1",
      n_clusters = n_clusters,
      cluster_size = 40L,
      beta = 0,
      random_slope_sd = 0,
      contamination = contamination,
      contamination_label = switch(
        contamination,
        none = "Clean",
        vertical = "Vertical outliers",
        bad_leverage = "Bad leverage"
      ),
      model = "study1_robust_ri",
      model_label = rmm_model_label(
        "study1_robust_ri"
      ),
      reps = pilot_reps,
      condition_seed = c(
        "10" = 20260920L,
        "20" = 20260921L,
        "40" = 20260922L
      )[as.character(n_clusters)],
      stringsAsFactors = FALSE
    )
  }
}

study2_rows <- list()
index <- 0L

for (n_clusters in c(10L, 20L, 40L)) {
  for (random_slope_sd in c(0.05, 0.10)) {
    for (contamination in c("none", "vertical")) {
      for (
        model in
        c(
          "study2_robust_ri",
          "study2_robust_rs"
        )
      ) {
        index <- index + 1L

        study2_rows[[index]] <- data.frame(
          study = "Study 2",
          n_clusters = n_clusters,
          cluster_size = 40L,
          beta = 0,
          random_slope_sd = random_slope_sd,
          contamination = contamination,
          contamination_label = if (
            contamination == "none"
          ) {
            "Clean"
          } else {
            "Vertical outliers"
          },
          model = model,
          model_label = rmm_model_label(model),
          reps = pilot_reps,
          condition_seed = c(
            "10" = 20260923L,
            "20" = 20260924L,
            "40" = 20260925L
          )[as.character(n_clusters)],
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

pilot_design <- rbind(
  do.call(rbind, study1_rows),
  do.call(rbind, study2_rows)
)
rownames(pilot_design) <- NULL

pilot_design$condition_id <- sprintf(
  "RMM%03d",
  seq_len(nrow(pilot_design))
)

pilot_design <- pilot_design[
  ,
  c(
    "condition_id",
    "study",
    "n_clusters",
    "cluster_size",
    "beta",
    "random_slope_sd",
    "contamination",
    "contamination_label",
    "model",
    "model_label",
    "reps",
    "condition_seed"
  )
]

design_path <- file.path(
  output_dir,
  "robust_mixed_model_pilot_design.rds"
)

if (file.exists(design_path)) {
  existing_design <- readRDS(design_path)

  if (!identical(existing_design, pilot_design)) {
    if (!overwrite_completed) {
      stop(
        paste(
          "The saved pilot design differs from the current design.",
          "Set overwrite_completed <- TRUE only if the pilot is",
          "intentionally being replaced."
        ),
        call. = FALSE
      )
    }

    old_checkpoints <- list.files(
      checkpoint_dir,
      pattern = "^condition_RMM[0-9]{3}[.]rds$",
      full.names = TRUE
    )

    if (length(old_checkpoints) > 0L) {
      file.remove(old_checkpoints)
    }
  }
}

rmm_save_rds_atomic(
  pilot_design,
  design_path
)

rmm_write_csv_atomic(
  pilot_design,
  file.path(
    output_dir,
    "robust_mixed_model_pilot_design.csv"
  )
)

if (!is.null(condition_ids_to_run)) {
  unknown <- setdiff(
    condition_ids_to_run,
    pilot_design$condition_id
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

source_files <- c(
  robust_mixed_model_pilot_helpers = file.path(
    project_root,
    "data-raw",
    "robust_mixed_model_pilot_helpers.R"
  ),
  robust_mixed_model_runtime_pilot = file.path(
    project_root,
    "data-raw",
    "robust_mixed_model_runtime_pilot.R"
  )
)

source_checksums <- rmm_source_checksums(
  project_root = project_root,
  files = source_files
)

metadata <- list(
  purpose = paste(
    "Estimate robustlmm runtime and computational usability",
    "before deciding whether to add robust mixed models to",
    "Studies 1 and 2."
  ),
  pilot_reps = pilot_reps,
  beta = 0,
  robustlmm_version = as.character(
    utils::packageVersion("robustlmm")
  ),
  robustlmm_method = "DAStau",
  robustlmm_setting = paste(
    "Package default. The setting argument is intentionally omitted."
  ),
  inference = paste(
    "Robust Satterthwaite inference from",
    "summary(fit, df = 'satterthwaite') using the default covariance."
  ),
  study1_full_addon_fits = 18L * 2000L,
  study2_full_addon_fits = 24L * 2000L * 2L,
  total_full_addon_fits =
    18L * 2000L + 24L * 2000L * 2L,
  source_checksums = source_checksums,
  session_info = utils::sessionInfo()
)

rmm_save_rds_atomic(
  metadata,
  file.path(
    output_dir,
    "robust_mixed_model_pilot_metadata.rds"
  )
)

rmm_write_csv_atomic(
  source_checksums,
  file.path(
    output_dir,
    "robust_mixed_model_source_checksums.csv"
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

pilot_started <- Sys.time()

for (condition_index in seq_len(nrow(pilot_design))) {
  condition <- pilot_design[
    condition_index,
    ,
    drop = FALSE
  ]

  if (!is.null(condition_ids_to_run) &&
      !(condition$condition_id %in%
          condition_ids_to_run)) {
    next
  }

  checkpoint_path <- rmm_checkpoint_path(
    checkpoint_dir,
    condition$condition_id
  )

  if (file.exists(checkpoint_path) &&
      !overwrite_completed) {
    checkpoint <- rmm_read_checkpoint(
      checkpoint_path
    )

    if (identical(checkpoint$status, "complete")) {
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
        "Running %s of %s: %s, G = %s, slope SD = %s, ",
        "%s, model = %s, reps = %s."
      ),
      condition$condition_id,
      nrow(pilot_design),
      condition$study,
      condition$n_clusters,
      format(
        condition$random_slope_sd,
        trim = TRUE
      ),
      condition$contamination_label,
      condition$model_label,
      condition$reps
    )
  )

  started_at <- Sys.time()

  set.seed(condition$condition_seed)
  replicate_seeds <- sample.int(
    .Machine$integer.max,
    size = condition$reps,
    replace = FALSE
  )

  condition_result <- tryCatch(
    {
      replicate_rows <- lapply(
        seq_len(condition$reps),
        function(replicate_id) {
          replicate_seed <- replicate_seeds[
            replicate_id
          ]
          set.seed(replicate_seed)

          dat <- if (condition$study == "Study 1") {
            rmm_simulate_study1(
              n_clusters = condition$n_clusters,
              contamination =
                condition$contamination
            )
          } else {
            rmm_simulate_study2(
              n_clusters = condition$n_clusters,
              random_slope_sd =
                condition$random_slope_sd,
              contamination =
                condition$contamination
            )
          }

          result <- rmm_fit_and_extract(
            dat = dat,
            model = condition$model,
            beta = condition$beta,
            alpha = 0.05,
            return_fit = FALSE
          )

          row <- rmm_result_to_row(
            result = result,
            replicate = replicate_id,
            replicate_seed = replicate_seed
          )

          condition_columns <- condition[
            rep(1L, nrow(row)),
            ,
            drop = FALSE
          ]

          out <- cbind(
            condition_columns,
            row
          )
          rownames(out) <- NULL

          rm(dat, result)
          gc(verbose = FALSE)

          out
        }
      )

      do.call(rbind, replicate_rows)
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

  if (inherits(condition_result, "error")) {
    checkpoint <- list(
      status = "error",
      condition = condition,
      results = NULL,
      replicate_seeds = replicate_seeds,
      error = conditionMessage(condition_result),
      started_at = started_at,
      completed_at = completed_at,
      elapsed_sec = elapsed_sec
    )
  } else {
    checkpoint <- list(
      status = "complete",
      condition = condition,
      results = condition_result,
      replicate_seeds = replicate_seeds,
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

  current_checkpoints <- rmm_collect_checkpoints(
    checkpoint_dir
  )
  current_status <- rmm_collect_status(
    current_checkpoints,
    pilot_design
  )

  rmm_write_csv_atomic(
    current_status,
    file.path(
      output_dir,
      "robust_mixed_model_pilot_status.csv"
    )
  )

  completed_count <- sum(
    current_status$status == "complete"
  )

  message(
    sprintf(
      paste0(
        "Completed %s in %.2f minutes. Progress: ",
        "%s of %s conditions; %.2f elapsed hours."
      ),
      condition$condition_id,
      elapsed_sec / 60,
      completed_count,
      nrow(pilot_design),
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

checkpoints <- rmm_collect_checkpoints(
  checkpoint_dir
)
status <- rmm_collect_status(
  checkpoints,
  pilot_design
)

complete_checkpoints <- checkpoints[
  vapply(
    checkpoints,
    function(checkpoint) {
      identical(
        checkpoint$status,
        "complete"
      )
    },
    logical(1)
  )
]

if (length(complete_checkpoints) == 0L) {
  stop(
    "No robust mixed-model pilot conditions completed.",
    call. = FALSE
  )
}

replicates <- rmm_bind_rows_fill(
  lapply(
    complete_checkpoints,
    function(checkpoint) {
      checkpoint$results
    }
  )
)

condition_ids <- unique(replicates$condition_id)

diagnostic_rows <- lapply(
  condition_ids,
  function(condition_id) {
    x <- replicates[
      replicates$condition_id == condition_id,
      ,
      drop = FALSE
    ]

    convergence_codes <- x$convergence_code[
      is.finite(x$convergence_code)
    ]
    boundary_values <- x$boundary_fit[
      !is.na(x$boundary_fit)
    ]

    data.frame(
      condition_id = condition_id,
      study = x$study[1L],
      n_clusters = x$n_clusters[1L],
      random_slope_sd =
        x$random_slope_sd[1L],
      contamination = x$contamination[1L],
      contamination_label =
        x$contamination_label[1L],
      model = x$model[1L],
      model_label = x$model_label[1L],
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
      convergence_code_available_rate =
        100 * mean(
          is.finite(x$convergence_code)
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
      mean_estimated_random_intercept_sd =
        rmm_mean_or_na(
          x$estimated_random_intercept_sd
        ),
      mean_estimated_random_slope_sd =
        rmm_mean_or_na(
          x$estimated_random_slope_sd
        ),
      mean_residual_weight =
        rmm_mean_or_na(
          x$residual_weight_mean
        ),
      minimum_residual_weight =
        rmm_min_or_na(
          x$residual_weight_minimum
        ),
      mean_prop_residual_weights_below_0_8 =
        rmm_mean_or_na(
          x$residual_weight_prop_below_0_8
        ),
      mean_random_effect_weight =
        rmm_mean_or_na(
          x$random_effect_weight_mean
        ),
      minimum_random_effect_weight =
        rmm_min_or_na(
          x$random_effect_weight_minimum
        ),
      stringsAsFactors = FALSE
    )
  }
)

diagnostics <- do.call(
  rbind,
  diagnostic_rows
)
rownames(diagnostics) <- NULL

# Runtime projections preserve the structural-condition mix. Each pilot
# structural condition corresponds to two final beta conditions, each with
# 2,000 replications.
runtime_rows <- diagnostics

runtime_rows$projected_full_fits <-
  2L * 2000L

runtime_rows$projected_full_seconds <-
  runtime_rows$mean_total_sec *
  runtime_rows$projected_full_fits

runtime_by_study_model <- stats::aggregate(
  runtime_rows$projected_full_seconds,
  by = list(
    study = runtime_rows$study,
    model = runtime_rows$model,
    model_label = runtime_rows$model_label
  ),
  FUN = sum
)

names(runtime_by_study_model)[4L] <-
  "projected_full_seconds"

runtime_by_study_model$projected_full_hours <-
  runtime_by_study_model$projected_full_seconds /
  3600

runtime_by_study_model$projected_full_days <-
  runtime_by_study_model$projected_full_hours /
  24

runtime_by_cluster <- stats::aggregate(
  runtime_rows$projected_full_seconds,
  by = list(
    study = runtime_rows$study,
    n_clusters = runtime_rows$n_clusters,
    model = runtime_rows$model
  ),
  FUN = sum
)

names(runtime_by_cluster)[4L] <-
  "projected_full_seconds"

runtime_by_cluster$projected_full_hours <-
  runtime_by_cluster$projected_full_seconds /
  3600

runtime_total <- data.frame(
  study1_projected_hours = sum(
    runtime_by_study_model$projected_full_hours[
      runtime_by_study_model$study == "Study 1"
    ],
    na.rm = TRUE
  ),
  study2_projected_hours = sum(
    runtime_by_study_model$projected_full_hours[
      runtime_by_study_model$study == "Study 2"
    ],
    na.rm = TRUE
  ),
  total_projected_hours = sum(
    runtime_by_study_model$projected_full_hours,
    na.rm = TRUE
  ),
  total_projected_days = sum(
    runtime_by_study_model$projected_full_hours,
    na.rm = TRUE
  ) / 24,
  stringsAsFactors = FALSE
)

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
  values <- replicates[[column]]
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

expected_conditions <- nrow(pilot_design)
expected_replicates <- sum(pilot_design$reps)

validation <- data.frame(
  check = c(
    "all_conditions_completed",
    "replicate_dimensions_correct",
    "all_models_returned_inference",
    "all_convergence_codes_available"
  ),
  passed = c(
    sum(status$status == "complete") ==
      expected_conditions,
    nrow(replicates) == expected_replicates,
    all(replicates$inference_complete),
    all(is.finite(replicates$convergence_code))
  ),
  details = c(
    sprintf(
      "%s of %s conditions complete",
      sum(status$status == "complete"),
      expected_conditions
    ),
    sprintf(
      "%s replicate fits; expected %s",
      nrow(replicates),
      expected_replicates
    ),
    sprintf(
      "%s of %s fits returned complete inference",
      sum(replicates$inference_complete),
      nrow(replicates)
    ),
    sprintf(
      "%s of %s fits returned convergence codes",
      sum(is.finite(replicates$convergence_code)),
      nrow(replicates)
    )
  ),
  stringsAsFactors = FALSE
)

combined <- list(
  design = pilot_design,
  status = status,
  validation = validation,
  replicates = replicates,
  diagnostics = diagnostics,
  runtime_by_study_model =
    runtime_by_study_model,
  runtime_by_cluster = runtime_by_cluster,
  runtime_total = runtime_total,
  message_frequency = message_frequency,
  metadata = metadata
)

rmm_save_rds_atomic(
  status,
  file.path(
    output_dir,
    "robust_mixed_model_pilot_status.rds"
  )
)
rmm_save_rds_atomic(
  replicates,
  file.path(
    output_dir,
    "robust_mixed_model_pilot_replicates.rds"
  )
)
rmm_save_rds_atomic(
  diagnostics,
  file.path(
    output_dir,
    "robust_mixed_model_pilot_diagnostics.rds"
  )
)
rmm_save_rds_atomic(
  combined,
  file.path(
    output_dir,
    "robust_mixed_model_runtime_pilot.rds"
  )
)

rmm_write_csv_atomic(
  status,
  file.path(
    output_dir,
    "robust_mixed_model_pilot_status.csv"
  )
)
rmm_write_csv_atomic(
  validation,
  file.path(
    output_dir,
    "robust_mixed_model_pilot_validation.csv"
  )
)
rmm_write_csv_atomic(
  replicates,
  file.path(
    output_dir,
    "robust_mixed_model_pilot_replicates.csv"
  )
)
rmm_write_csv_atomic(
  diagnostics,
  file.path(
    output_dir,
    "robust_mixed_model_pilot_diagnostics.csv"
  )
)
rmm_write_csv_atomic(
  runtime_by_study_model,
  file.path(
    output_dir,
    "robust_mixed_model_runtime_by_study_model.csv"
  )
)
rmm_write_csv_atomic(
  runtime_by_cluster,
  file.path(
    output_dir,
    "robust_mixed_model_runtime_by_cluster.csv"
  )
)
rmm_write_csv_atomic(
  runtime_total,
  file.path(
    output_dir,
    "robust_mixed_model_runtime_total.csv"
  )
)
rmm_write_csv_atomic(
  message_frequency,
  file.path(
    output_dir,
    "robust_mixed_model_message_frequency.csv"
  )
)

message("")
message("Robust mixed-model runtime pilot complete.")
message(
  sprintf(
    "Completed conditions: %s of %s.",
    sum(status$status == "complete"),
    nrow(pilot_design)
  )
)
message(
  sprintf(
    "Completed robust fits: %s.",
    nrow(replicates)
  )
)
message(paste("Results saved to:", output_dir))

message("")
message("Pilot validation:")
print(validation, row.names = FALSE)

message("")
message("Runtime projection by study and model:")
print(
  runtime_by_study_model[
    ,
    c(
      "study",
      "model_label",
      "projected_full_hours",
      "projected_full_days"
    )
  ],
  row.names = FALSE
)

message("")
message("Total projected add-on runtime:")
print(runtime_total, row.names = FALSE)

diagnostic_events <- diagnostics[
  diagnostics$usable_rate < 100 |
    diagnostics$convergence_failure_rate > 0 |
    diagnostics$boundary_fit_rate > 0 |
    diagnostics$fit_warning_rate > 0 |
    diagnostics$inference_warning_rate > 0 |
    diagnostics$fit_error_rate > 0 |
    diagnostics$inference_error_rate > 0,
  ,
  drop = FALSE
]

message("")

if (nrow(diagnostic_events) == 0L) {
  message(
    "No computational diagnostic events were detected."
  )
} else {
  message("Computational diagnostic events:")
  print(
    diagnostic_events[
      ,
      c(
        "condition_id",
        "study",
        "n_clusters",
        "random_slope_sd",
        "contamination_label",
        "model_label",
        "usable_rate",
        "convergence_failure_rate",
        "boundary_fit_rate",
        "fit_warning_rate",
        "inference_warning_rate",
        "mean_total_sec"
      )
    ],
    row.names = FALSE
  )
}

message("")
message(
  paste(
    "This five-replication pilot is for runtime and software",
    "behavior only. Do not interpret bias, Type I error, power,",
    "or coverage from these results."
  )
)
