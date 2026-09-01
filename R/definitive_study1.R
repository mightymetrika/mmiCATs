# Package-owned definitive Study 1 configuration and runner.

study1d_methods <- function() {
  c(
    "ri",
    "cr2",
    "cats",
    "cats_trunc",
    "cats_robust",
    "cats_robustbase",
    "robust_ri"
  )
}

study1d_frozen_config <- function() {
  list(
    final_reps = 2000L,
    alpha = 0.05,
    final_seed_base = 20260815L,
    shard_size = 10L,
    minimum_free_gb = 2.0,
    retain_completed_shards = FALSE
  )
}

study1d_frozen_design <- function() {
  config <- study1d_frozen_config()
  methods <- study1d_methods()

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
  final_design$reps <- config$final_reps
  final_design$alpha <- config$alpha
  final_design$shard_size <- config$shard_size
  final_design$minimum_free_gb <- config$minimum_free_gb
  final_design$retain_completed_shards <- config$retain_completed_shards
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
    config$final_seed_base +
      cluster_seed_index -
      1L
  )
  final_design$common_random_number_group <- paste0(
    "G",
    final_design$n_clusters
  )

  final_design[
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
}

study1d_make_source_checksums <- function(project_root) {
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
      project_root,
      "R",
      "robust_mixed_models.R"
    ),
    definitive_sharding_helpers = file.path(
      project_root,
      "R",
      "definitive_sharding_helpers.R"
    ),
    definitive_study1_helpers = file.path(
      project_root,
      "R",
      "definitive_study1_helpers.R"
    ),
    definitive_study1 = file.path(
      project_root,
      "R",
      "definitive_study1.R"
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
    md5 = unname(
      tools::md5sum(paths)
    ),
    stringsAsFactors = FALSE
  )
}

#' Run the Frozen Definitive Study 1 Simulation
#'
#' Runs the manuscript-version Study 1 simulation using the frozen design,
#' deterministic replication seeds, and checkpoint/resume infrastructure.
#' The scientific design is not configurable through this function.
#'
#' @param project_root Source checkout root containing DESCRIPTION. If `NULL`,
#'   the root is located from the working directory.
#' @param output_dir Destination for definitive Study 1 artifacts. If `NULL`,
#'   uses `data-raw/study1-results/definitive-study` under `project_root`.
#' @param condition_ids_to_run Optional character vector of frozen condition IDs
#'   to execute. `NULL` means all 18 conditions. This controls scheduling only;
#'   it does not change any condition.
#' @param overwrite_completed Logical. If `FALSE`, completed compatible
#'   condition checkpoints are reused.
#'
#' @return Invisibly, the combined Study 1 result object.
#' @export
run_study1_definitive <- function(
    project_root = NULL,
    output_dir = NULL,
    condition_ids_to_run = NULL,
    overwrite_completed = FALSE) {
  if (is.null(project_root)) {
    project_root <- study1d_find_project_root()
  }

  project_root <- normalizePath(
    project_root,
    winslash = "/",
    mustWork = TRUE
  )

  if (!requireNamespace("pbkrtest", quietly = TRUE)) {
    stop(
      paste(
        "The pbkrtest package is required because definitive Study 1",
        "includes the random-intercept method with Kenward-Roger inference."
      ),
      call. = FALSE
    )
  }

  if (!requireNamespace("robustlmm", quietly = TRUE)) {
    stop(
      paste(
        "The robustlmm package is required for the definitive",
        "robust mixed-model comparator."
      ),
      call. = FALSE
    )
  }

  config <- study1d_frozen_config()
  final_reps <- config$final_reps
  alpha <- config$alpha
  final_seed_base <- config$final_seed_base
  shard_size <- config$shard_size
  minimum_free_gb <- config$minimum_free_gb
  retain_completed_shards <- config$retain_completed_shards
  methods <- study1d_methods()

  if (is.null(output_dir)) {
    output_dir <- file.path(
      project_root,
      "data-raw",
      "study1-results",
      "definitive-study"
    )
  }

  checkpoint_dir <- file.path(
    output_dir,
    "conditions"
  )

  dir.create(
    checkpoint_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  shard_dir <- file.path(
    output_dir,
    "shards"
  )
  shard_status_dir <- file.path(
    output_dir,
    "shard-status"
  )

  dir.create(
    shard_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    shard_status_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  final_design <- study1d_frozen_design()

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

  definitive_save_rds_atomic(
    final_design,
    design_path
  )
  definitive_write_csv_atomic(
    final_design,
    file.path(output_dir, "study1_final_design.csv")
  )

  source_checksums <- study1d_make_source_checksums(
    project_root
  )

  package_description <- base::read.dcf(
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

  definitive_save_rds_atomic(
    metadata,
    file.path(output_dir, "study1_final_metadata.rds")
  )

  definitive_write_csv_atomic(
    source_checksums,
    file.path(output_dir, "study1_source_checksums.csv")
  )

  writeLines(
    utils::capture.output(utils::sessionInfo()),
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

      definitive_write_csv_atomic(
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

    definitive_write_csv_atomic(
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

    prepared <- study1d_prepare_replicates_for_storage(
      replicates = collected$replicates,
      settings = full_settings,
      condition = condition
    )

    condition_summary <- study1_summarize_results(
      replicate_results = collected$replicates,
      methods = methods,
      reps = condition$reps
    )
    rownames(condition_summary) <- NULL
    condition_summary <- study1d_add_condition_columns(condition_summary, condition)
    condition_summary <- study1d_add_method_labels(condition_summary)

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

    definitive_save_rds_atomic(condition_checkpoint, checkpoint_path)

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

    status_snapshot <- study1d_make_status_snapshot(
      checkpoint_dir = checkpoint_dir,
      design = final_design
    )
    definitive_write_csv_atomic(
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

  final_status <- study1d_make_status_snapshot(
    checkpoint_dir = checkpoint_dir,
    design = final_design
  )

  definitive_write_csv_atomic(
    final_status,
    file.path(
      output_dir,
      "study1_condition_status.csv"
    )
  )
  definitive_save_rds_atomic(
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

  flagged_cluster_diagnostics <- study1d_rbind_fill(
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

  final_diagnostics <- study1d_summarize_diagnostics(
    final_replicates
  )

  primary_performance <- study1d_make_primary_performance_table(
    final_summary
  )

  negative_control_comparison <-
    study1d_make_negative_control_comparison(
      final_replicates
    )

  robust_vs_cats <- study1d_make_robust_vs_cats(
    final_summary
  )

  message_frequency <- study1d_make_message_frequency(
    replicates = final_replicates,
    flagged_diagnostics =
      flagged_cluster_diagnostics
  )

  mcse_summary <- study1d_make_mcse_summary(
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

  definitive_save_rds_atomic(
    final_summary,
    file.path(
      output_dir,
      "study1_final_summary.rds"
    )
  )
  definitive_save_rds_atomic(
    primary_performance,
    file.path(
      output_dir,
      "study1_primary_performance.rds"
    )
  )
  definitive_save_rds_atomic(
    final_replicates,
    file.path(
      output_dir,
      "study1_final_replicates.rds"
    )
  )
  definitive_save_rds_atomic(
    final_diagnostics,
    file.path(
      output_dir,
      "study1_final_diagnostics.rds"
    )
  )
  definitive_save_rds_atomic(
    flagged_cluster_diagnostics,
    file.path(
      output_dir,
      "study1_flagged_cluster_diagnostics.rds"
    )
  )
  definitive_save_rds_atomic(
    negative_control_comparison,
    file.path(
      output_dir,
      "study1_cats_trunc_negative_control.rds"
    )
  )
  definitive_save_rds_atomic(
    robust_vs_cats,
    file.path(
      output_dir,
      "study1_robust_vs_cats.rds"
    )
  )
  definitive_save_rds_atomic(
    mcse_summary,
    file.path(
      output_dir,
      "study1_mcse_summary.rds"
    )
  )
  definitive_save_rds_atomic(
    final_results,
    file.path(
      output_dir,
      "study1_final_results.rds"
    )
  )

  definitive_write_csv_atomic(
    final_summary,
    file.path(
      output_dir,
      "study1_final_summary.csv"
    )
  )
  definitive_write_csv_atomic(
    primary_performance,
    file.path(
      output_dir,
      "study1_primary_performance.csv"
    )
  )
  definitive_write_csv_atomic(
    final_diagnostics,
    file.path(
      output_dir,
      "study1_final_diagnostics.csv"
    )
  )
  definitive_write_csv_atomic(
    flagged_cluster_diagnostics,
    file.path(
      output_dir,
      "study1_flagged_cluster_diagnostics.csv"
    )
  )
  definitive_write_csv_atomic(
    message_frequency,
    file.path(
      output_dir,
      "study1_message_frequency.csv"
    )
  )
  definitive_write_csv_atomic(
    negative_control_comparison,
    file.path(
      output_dir,
      "study1_cats_trunc_negative_control.csv"
    )
  )
  definitive_write_csv_atomic(
    robust_vs_cats,
    file.path(
      output_dir,
      "study1_robust_vs_cats.csv"
    )
  )
  definitive_write_csv_atomic(
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

  invisible(final_results)
}
