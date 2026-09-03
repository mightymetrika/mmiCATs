# Package-owned definitive Study 2 configuration and runner.

study2d_methods <- function() {
  c(
    "rs",
    "ri",
    "cr2",
    "cats",
    "cats_trunc",
    "cats_robust",
    "cats_robustbase",
    "robust_ri",
    "robust_rs"
  )
}

study2d_frozen_config <- function() {
  list(
    final_reps = 2000L,
    alpha = 0.05,
    minimum_usable_reps = 1900L,
    final_seed_base = 20260905L,
    shard_size = 10L,
    minimum_free_gb = 2.0,
    retain_completed_shards = FALSE
  )
}

study2d_frozen_design <- function() {
  config <- study2d_frozen_config()
  methods <- study2d_methods()

  contamination_specifications <- data.frame(
    contamination = c(
      "none",
      "vertical"
    ),
    contamination_label = c(
      "Clean",
      "Vertical outliers"
    ),
    contamination_size = c(
      0,
      6
    ),
    stringsAsFactors = FALSE
  )

  design_rows <- list()
  design_index <- 0L

  for (n_clusters in c(10L, 20L, 40L)) {
    for (beta in c(0, 0.10)) {
      for (random_slope_sd in c(0.05, 0.10)) {
        for (
          contamination_index in
          seq_len(nrow(contamination_specifications))
        ) {
          design_index <- design_index + 1L

          design_rows[[design_index]] <- data.frame(
            n_clusters = n_clusters,
            beta = beta,
            random_slope_sd = random_slope_sd,
            contamination_specifications[
              contamination_index,
              ,
              drop = FALSE
            ],
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  final_design <- do.call(
    rbind,
    design_rows
  )
  rownames(final_design) <- NULL

  final_design$condition_id <- sprintf(
    "S2C%03d",
    seq_len(nrow(final_design))
  )
  final_design$cluster_size <- 40L
  final_design$effect_label <- ifelse(
    final_design$beta == 0,
    "Null",
    "Alternative"
  )
  final_design$intercept <- 0
  final_design$random_intercept_sd <- 1
  final_design$random_slope_variance <-
    final_design$random_slope_sd^2
  final_design$residual_sd <- 1
  final_design$x_sd <- 1
  final_design$contamination_prop <- 0.05
  final_design$reps <- config$final_reps
  final_design$alpha <- config$alpha
  final_design$shard_size <- config$shard_size
  final_design$minimum_free_gb <- config$minimum_free_gb
  final_design$retain_completed_shards <- config$retain_completed_shards
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
      "random_slope_sd",
      "random_slope_variance",
      "residual_sd",
      "x_sd",
      "contamination",
      "contamination_label",
      "contamination_prop",
      "contamination_size",
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

study2d_make_source_checksums <- function(project_root) {
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
    pwr_func_study2 = file.path(
      project_root,
      "R",
      "pwr_func_study2.R"
    ),
    pwr_func_study2_helpers = file.path(
      project_root,
      "R",
      "pwr_func_study2_helpers.R"
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
    definitive_study2_helpers = file.path(
      project_root,
      "R",
      "definitive_study2_helpers.R"
    ),
    definitive_study2 = file.path(
      project_root,
      "R",
      "definitive_study2.R"
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

#' Run the Frozen Definitive Study 2 Simulation
#'
#' Runs the manuscript-version Study 2 simulation using the frozen design,
#' deterministic replication seeds, and checkpoint/resume infrastructure.
#' The scientific design is not configurable through this function.
#'
#' @param project_root Source checkout root containing DESCRIPTION. If `NULL`,
#'   the root is located from the working directory.
#' @param output_dir Destination for definitive Study 2 artifacts. If `NULL`,
#'   uses `data-raw/study2-results/definitive-study` under `project_root`.
#' @param condition_ids_to_run Optional character vector of frozen condition IDs
#'   to execute. `NULL` means all 24 conditions. This controls scheduling only;
#'   it does not change any condition.
#' @param overwrite_completed Logical. If `FALSE`, completed compatible
#'   condition checkpoints are reused.
#'
#' @return Invisibly, the combined Study 2 result object.
#' @export
run_study2_definitive <- function(
    project_root = NULL,
    output_dir = NULL,
    condition_ids_to_run = NULL,
    overwrite_completed = FALSE) {
  if (is.null(project_root)) {
    project_root <- study2d_find_project_root()
  }

  project_root <- normalizePath(
    project_root,
    winslash = "/",
    mustWork = TRUE
  )

  # The definitive manuscript-version simulation is prospectively gated.
  # This verifies the immutable Study 1/2 source freeze, exact frozen source/Git
  # state, package/RNG environment, and completed external pre-results
  # registration before any output directory or shard can be created.
  study12f_verify_gate(
    project_root = project_root,
    verify_current_source = TRUE,
    verify_current_git = TRUE,
    verify_package_versions = TRUE,
    verify_rng = TRUE
  )

  if (!requireNamespace("pbkrtest", quietly = TRUE)) {
    stop(
      paste(
        "The pbkrtest package is required because definitive Study 2",
        "includes mixed models with Kenward-Roger inference."
      ),
      call. = FALSE
    )
  }

  if (!requireNamespace("robustlmm", quietly = TRUE)) {
    stop(
      paste(
        "The robustlmm package is required for the definitive",
        "robust mixed-model comparators."
      ),
      call. = FALSE
    )
  }

  config <- study2d_frozen_config()
  final_reps <- config$final_reps
  alpha <- config$alpha
  minimum_usable_reps <- config$minimum_usable_reps
  final_seed_base <- config$final_seed_base
  shard_size <- config$shard_size
  minimum_free_gb <- config$minimum_free_gb
  retain_completed_shards <- config$retain_completed_shards
  methods <- study2d_methods()

  if (is.null(output_dir)) {
    output_dir <- file.path(
      project_root,
      "data-raw",
      "study2-results",
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

  final_design <- study2d_frozen_design()

  design_path <- file.path(
    output_dir,
    "study2_final_design.rds"
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
        pattern = "^condition_S2C[0-9]{3}[.]rds$",
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
    file.path(output_dir, "study2_final_design.csv")
  )

  source_checksums <- study2d_make_source_checksums(
    project_root
  )

  package_description <- base::read.dcf(
    file.path(project_root, "DESCRIPTION")
  )

  metadata <- list(
    study = "mmiCATs Study 2 definitive manuscript-version simulation",
    created_at = Sys.time(),
    project_root = project_root,
    package_version = unname(
      package_description[1L, "Version"]
    ),
    r_version = R.version.string,
    final_reps = final_reps,
    alpha = alpha,
    minimum_usable_reps = minimum_usable_reps,
    final_seed_base = final_seed_base,
    shard_size = shard_size,
    minimum_free_gb = minimum_free_gb,
    retain_completed_shards = retain_completed_shards,
    methods = methods,
    primary_estimand = paste(
      "The superpopulation mean cluster slope beta. The realized",
      "sample mean cluster slope is retained only as a diagnostic."
    ),
    common_random_numbers = paste(
      "All eight conditions with the same number of clusters use",
      "the same condition seed. Seeds differ across cluster counts."
    ),
    calibration_independence = paste(
      "The definitive-study seeds differ from numerical validation,",
      "plumbing-pilot, and random-slope convergence-diagnostic seeds."
    ),
    frozen_parameters = list(
      n_clusters = c(10L, 20L, 40L),
      cluster_size = 40L,
      beta = c(0, 0.10),
      random_intercept_sd = 1,
      random_slope_sd = c(0.05, 0.10),
      random_slope_variance = c(0.0025, 0.0100),
      intercept_slope_covariance = 0,
      residual_sd = 1,
      x_sd = 1,
      contamination_prop = 0.05,
      vertical_contamination_size = 6
    ),
    correctly_specified_model = paste(
      "out ~ x + (1 + x || cluster), fitted by REML with",
      "Kenward-Roger inference and default lmerTest/lme4 optimizer settings."
    ),
    singularity_rule = paste(
      "A singular fit remains usable when fixed-effect inference is",
      "complete and finite and there is no genuine convergence failure."
    ),
    failed_fit_rule = paste(
      "Failed fits are not replaced. Method-specific usable denominators",
      "and failure rates are reported."
    ),
    computational_adequacy_rule = paste(
      "At least 1,900 usable fits among 2,000 attempts are required",
      "for every method-condition combination."
    ),
    truncated_cats_role = paste(
      "Truncated CATs is retained as a negative control because",
      "contamination is distributed similarly within every cluster."
    ),
    no_contamination_placeholder = paste(
      "pwr_func_study2 requires a positive contamination_size.",
      "The script passes 1 under contamination = 'none'; the value is unused."
    ),
    source_checksums = source_checksums,
    overwrite_completed = overwrite_completed,
    condition_ids_to_run = condition_ids_to_run,
    session_info = utils::sessionInfo(),
    system_info = Sys.info()
  )

  definitive_save_rds_atomic(
    metadata,
    file.path(output_dir, "study2_final_metadata.rds")
  )

  definitive_write_csv_atomic(
    source_checksums,
    file.path(output_dir, "study2_source_checksums.csv")
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
        "Running %s of %s: G = %s, beta = %s, random-slope SD = %s, ",
        "condition = %s; %s shards of up to %s reps."
      ),
      condition$condition_id,
      nrow(final_design),
      condition$n_clusters,
      format(condition$beta, trim = TRUE),
      format(condition$random_slope_sd, trim = TRUE),
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
          study = "study2",
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

    prepared <- study2d_prepare_replicates_for_storage(
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
    condition_summary <- study2d_add_condition_columns(condition_summary, condition)
    condition_summary <- study2d_add_method_labels(condition_summary)

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

    status_snapshot <- study2d_make_status_snapshot(
      checkpoint_dir = checkpoint_dir,
      design = final_design
    )
    definitive_write_csv_atomic(
      status_snapshot,
      file.path(output_dir, "study2_condition_status.csv")
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

  final_status <- study2d_make_status_snapshot(
    checkpoint_dir = checkpoint_dir,
    design = final_design
  )

  definitive_write_csv_atomic(
    final_status,
    file.path(
      output_dir,
      "study2_condition_status.csv"
    )
  )
  definitive_save_rds_atomic(
    final_status,
    file.path(
      output_dir,
      "study2_condition_status.rds"
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

  flagged_cluster_diagnostics <- study2d_rbind_fill(
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
      final_summary$random_slope_sd,
      match(
        final_summary$contamination,
        c("none", "vertical")
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
      final_replicates$random_slope_sd,
      match(
        final_replicates$contamination,
        c("none", "vertical")
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

  final_diagnostics <- study2d_summarize_diagnostics(
    final_replicates
  )

  primary_performance <- study2d_make_primary_performance_table(
    final_summary
  )

  negative_control_comparison <-
    study2d_make_negative_control_comparison(
      final_replicates
    )

  robust_vs_cats <- study2d_make_method_vs_reference(
    summary_results = final_summary,
    comparison_methods = c(
      "cats_robust",
      "cats_robustbase"
    ),
    reference_method = "cats",
    comparison_name = "Robust CATs versus ordinary CATs"
  )

  random_slope_vs_ri <- study2d_make_method_vs_reference(
    summary_results = final_summary,
    comparison_methods = "rs",
    reference_method = "ri",
    comparison_name = paste(
      "Correct random-slope model versus",
      "misspecified random-intercept model"
    )
  )

  rs_sensitivity <- study2d_make_rs_singularity_sensitivity(
    final_replicates
  )

  dgp_diagnostics <- study2d_make_dgp_diagnostics(
    final_replicates
  )

  crn_audit <- study2d_make_crn_audit(
    final_replicates
  )

  message_frequency <- study2d_make_message_frequency(
    replicates = final_replicates,
    flagged_diagnostics =
      flagged_cluster_diagnostics
  )

  mcse_summary <- study2d_make_mcse_summary(
    final_summary
  )

  flagged_mixed_model_replicates <- final_replicates[
    final_replicates$method %in% c("rs", "ri") &
      (
        !final_replicates$fit_success |
          (
            !is.na(final_replicates$singular) &
              final_replicates$singular
          ) |
          study2d_has_text(final_replicates$optimizer_warning) |
          (
            !is.na(final_replicates$optimizer_code) &
              final_replicates$optimizer_code != 0
          ) |
          study2d_has_text(final_replicates$error)
      ),
    ,
    drop = FALSE
  ]

  # -------------------------------------------------------------------------
  # Final validation
  # -------------------------------------------------------------------------

  expected_summary_rows <-
    nrow(final_design) * length(methods)
  expected_replicate_rows <-
    nrow(final_design) * final_reps * length(methods)

  observed_counts <- stats::aggregate(
    rep(1L, nrow(final_replicates)),
    by = list(
      condition_id = final_replicates$condition_id,
      method = final_replicates$method
    ),
    FUN = sum
  )
  names(observed_counts)[3L] <- "observed_reps"

  usable_counts <- stats::aggregate(
    as.integer(final_replicates$fit_success),
    by = list(
      condition_id = final_replicates$condition_id,
      method = final_replicates$method
    ),
    FUN = sum
  )
  names(usable_counts)[3L] <- "usable_reps"

  condition_method_counts <- merge(
    observed_counts,
    usable_counts,
    by = c("condition_id", "method"),
    all = TRUE,
    sort = FALSE
  )

  condition_method_counts <- condition_method_counts[
    order(
      match(
        condition_method_counts$condition_id,
        final_design$condition_id
      ),
      match(
        condition_method_counts$method,
        methods
      )
    ),
    ,
    drop = FALSE
  ]
  rownames(condition_method_counts) <- NULL

  condition_method_counts$meets_usable_criterion <-
    condition_method_counts$usable_reps >=
      minimum_usable_reps

  completed_conditions <- sum(
    final_status$status == "complete"
  )

  all_conditions_completed <-
    completed_conditions == nrow(final_design)

  combined_dimensions_correct <-
    nrow(final_summary) == expected_summary_rows &&
      nrow(final_replicates) == expected_replicate_rows

  condition_method_dimensions_correct <-
    nrow(condition_method_counts) ==
      expected_summary_rows &&
      all(
        condition_method_counts$observed_reps ==
          final_reps
      )

  all_crn_checks_passed <-
    nrow(crn_audit) ==
      length(unique(final_design$n_clusters)) *
        final_reps &&
      all(crn_audit$passed)

  all_method_conditions_meet_usability <-
    nrow(condition_method_counts) ==
      expected_summary_rows &&
      all(
        condition_method_counts$meets_usable_criterion
      )

  final_validation <- data.frame(
    check = c(
      "all_conditions_completed",
      "combined_dimensions_correct",
      "condition_method_dimensions_correct",
      "all_crn_checks_passed",
      "all_method_conditions_meet_1900_usable_fits"
    ),
    passed = c(
      all_conditions_completed,
      combined_dimensions_correct,
      condition_method_dimensions_correct,
      all_crn_checks_passed,
      all_method_conditions_meet_usability
    ),
    details = c(
      sprintf(
        "%s of %s conditions complete",
        completed_conditions,
        nrow(final_design)
      ),
      sprintf(
        paste0(
          "%s summary rows and %s replicate rows; ",
          "expected %s and %s"
        ),
        nrow(final_summary),
        nrow(final_replicates),
        expected_summary_rows,
        expected_replicate_rows
      ),
      sprintf(
        paste0(
          "%s condition-method rows; all completed rows ",
          "should contain %s attempts"
        ),
        nrow(condition_method_counts),
        final_reps
      ),
      sprintf(
        "%s of %s cluster-by-replication CRN checks passed",
        sum(crn_audit$passed),
        nrow(crn_audit)
      ),
      sprintf(
        "%s of %s method-condition combinations have at least %s usable fits",
        sum(
          condition_method_counts$meets_usable_criterion
        ),
        nrow(condition_method_counts),
        minimum_usable_reps
      )
    ),
    stringsAsFactors = FALSE
  )

  final_results <- list(
    design = final_design,
    status = final_status,
    validation = final_validation,
    condition_method_counts =
      condition_method_counts,
    summary = final_summary,
    primary_performance = primary_performance,
    replicates = final_replicates,
    diagnostics = final_diagnostics,
    dgp_diagnostics = dgp_diagnostics,
    common_random_number_audit = crn_audit,
    flagged_mixed_model_replicates =
      flagged_mixed_model_replicates,
    flagged_cluster_diagnostics =
      flagged_cluster_diagnostics,
    message_frequency = message_frequency,
    cats_trunc_negative_control =
      negative_control_comparison,
    robust_vs_cats = robust_vs_cats,
    random_slope_vs_random_intercept =
      random_slope_vs_ri,
    random_slope_singularity_sensitivity =
      rs_sensitivity,
    mcse_summary = mcse_summary,
    metadata = metadata
  )

  # -------------------------------------------------------------------------
  # Save combined outputs
  # -------------------------------------------------------------------------

  definitive_save_rds_atomic(
    final_validation,
    file.path(
      output_dir,
      "study2_final_validation.rds"
    )
  )
  definitive_save_rds_atomic(
    condition_method_counts,
    file.path(
      output_dir,
      "study2_condition_method_counts.rds"
    )
  )
  definitive_save_rds_atomic(
    final_summary,
    file.path(
      output_dir,
      "study2_final_summary.rds"
    )
  )
  definitive_save_rds_atomic(
    primary_performance,
    file.path(
      output_dir,
      "study2_primary_performance.rds"
    )
  )
  definitive_save_rds_atomic(
    final_replicates,
    file.path(
      output_dir,
      "study2_final_replicates.rds"
    )
  )
  definitive_save_rds_atomic(
    final_diagnostics,
    file.path(
      output_dir,
      "study2_final_diagnostics.rds"
    )
  )
  definitive_save_rds_atomic(
    dgp_diagnostics,
    file.path(
      output_dir,
      "study2_dgp_diagnostics.rds"
    )
  )
  definitive_save_rds_atomic(
    crn_audit,
    file.path(
      output_dir,
      "study2_crn_audit.rds"
    )
  )
  definitive_save_rds_atomic(
    flagged_mixed_model_replicates,
    file.path(
      output_dir,
      "study2_flagged_mixed_model_replicates.rds"
    )
  )
  definitive_save_rds_atomic(
    flagged_cluster_diagnostics,
    file.path(
      output_dir,
      "study2_flagged_cluster_diagnostics.rds"
    )
  )
  definitive_save_rds_atomic(
    negative_control_comparison,
    file.path(
      output_dir,
      "study2_cats_trunc_negative_control.rds"
    )
  )
  definitive_save_rds_atomic(
    robust_vs_cats,
    file.path(
      output_dir,
      "study2_robust_vs_cats.rds"
    )
  )
  definitive_save_rds_atomic(
    random_slope_vs_ri,
    file.path(
      output_dir,
      "study2_random_slope_vs_random_intercept.rds"
    )
  )
  definitive_save_rds_atomic(
    rs_sensitivity,
    file.path(
      output_dir,
      "study2_random_slope_singularity_sensitivity.rds"
    )
  )
  definitive_save_rds_atomic(
    message_frequency,
    file.path(
      output_dir,
      "study2_message_frequency.rds"
    )
  )
  definitive_save_rds_atomic(
    mcse_summary,
    file.path(
      output_dir,
      "study2_mcse_summary.rds"
    )
  )
  definitive_save_rds_atomic(
    final_results,
    file.path(
      output_dir,
      "study2_final_results.rds"
    )
  )

  definitive_write_csv_atomic(
    final_validation,
    file.path(
      output_dir,
      "study2_final_validation.csv"
    )
  )
  definitive_write_csv_atomic(
    condition_method_counts,
    file.path(
      output_dir,
      "study2_condition_method_counts.csv"
    )
  )
  definitive_write_csv_atomic(
    final_summary,
    file.path(
      output_dir,
      "study2_final_summary.csv"
    )
  )
  definitive_write_csv_atomic(
    primary_performance,
    file.path(
      output_dir,
      "study2_primary_performance.csv"
    )
  )
  definitive_write_csv_atomic(
    final_diagnostics,
    file.path(
      output_dir,
      "study2_final_diagnostics.csv"
    )
  )
  definitive_write_csv_atomic(
    dgp_diagnostics,
    file.path(
      output_dir,
      "study2_dgp_diagnostics.csv"
    )
  )
  definitive_write_csv_atomic(
    crn_audit,
    file.path(
      output_dir,
      "study2_crn_audit.csv"
    )
  )
  definitive_write_csv_atomic(
    flagged_mixed_model_replicates,
    file.path(
      output_dir,
      "study2_flagged_mixed_model_replicates.csv"
    )
  )
  # definitive_write_csv_atomic(
  #   flagged_cluster_diagnostics,
  #   file.path(
  #     output_dir,
  #     "study2_flagged_cluster_diagnostics.csv"
  #   )
  # )
  definitive_write_csv_atomic(
    message_frequency,
    file.path(
      output_dir,
      "study2_message_frequency.csv"
    )
  )
  definitive_write_csv_atomic(
    negative_control_comparison,
    file.path(
      output_dir,
      "study2_cats_trunc_negative_control.csv"
    )
  )
  definitive_write_csv_atomic(
    robust_vs_cats,
    file.path(
      output_dir,
      "study2_robust_vs_cats.csv"
    )
  )
  definitive_write_csv_atomic(
    random_slope_vs_ri,
    file.path(
      output_dir,
      "study2_random_slope_vs_random_intercept.csv"
    )
  )
  definitive_write_csv_atomic(
    rs_sensitivity$long,
    file.path(
      output_dir,
      "study2_random_slope_singularity_sensitivity_long.csv"
    )
  )
  definitive_write_csv_atomic(
    rs_sensitivity$comparison,
    file.path(
      output_dir,
      "study2_random_slope_singularity_sensitivity_comparison.csv"
    )
  )
  definitive_write_csv_atomic(
    mcse_summary,
    file.path(
      output_dir,
      "study2_mcse_summary.csv"
    )
  )

  # -------------------------------------------------------------------------
  # Console summary
  # -------------------------------------------------------------------------

  total_conditions <- nrow(final_design)
  total_elapsed_hours <- sum(
    final_status$elapsed_sec[
      final_status$status == "complete"
    ],
    na.rm = TRUE
  ) / 3600

  message("")
  message("Study 2 final simulation processing complete.")
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
  message("Final validation checks:")
  print(
    final_validation,
    row.names = FALSE
  )

  message("")
  message("Monte Carlo standard-error summary:")
  print(
    mcse_summary,
    row.names = FALSE
  )

  diagnostic_problems <- final_diagnostics[
    final_diagnostics$failure_rate > 0 |
      final_diagnostics$optimizer_warning_rep_rate > 0 |
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
        "No fit failures, genuine optimizer warnings, errors,",
        "singular fits, or dropped robust-CATs clusters were detected."
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
          "successful_reps",
          "failure_rate",
          "optimizer_warning_rep_rate",
          "error_rep_rate",
          "convergence_failure_rate",
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
        "random_slope_sd",
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

    if (all(final_validation$passed)) {
      message(
        "All 24 frozen Study 2 conditions and validation checks passed."
      )
    } else {
      message(
        paste(
          "All Study 2 conditions completed, but one or more",
          "post-run validation criteria require review."
        )
      )
    }
  }

  invisible(final_results)
}
