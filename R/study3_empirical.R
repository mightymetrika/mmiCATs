#' Prepare the Frozen Study 3 Empirical Illustration
#'
#' Recreates the prespecified Phase 6B empirical inputs from
#' `lme4::sleepstudy`, or verifies and returns an existing completed freeze.
#' The function never fits comparative Study 3 methods.
#'
#' @param project_root Project root containing DESCRIPTION and the approved
#'   Study 3 plan. If `NULL`, the root is located from the working directory.
#' @param plan_path Path to the approved Study 3 empirical-analysis plan.
#'   If `NULL`, uses the project `data-raw` plan.
#' @param freeze_dir Destination for the immutable pre-results freeze.
#'   If `NULL`, uses `data-raw/study3-results/pre-results-freeze`.
#'
#' @return Invisibly, the Study 3 freeze record.
#' @export
prepare_study3_empirical <- function(
    project_root = NULL,
    plan_path = NULL,
    freeze_dir = NULL) {
  if (is.null(project_root)) {
    project_root <- study3_find_project_root()
  }

  project_root <- normalizePath(
    project_root,
    winslash = "/",
    mustWork = TRUE
  )

  if (is.null(plan_path)) {
    plan_path <- file.path(
      project_root,
      "data-raw",
      "study3_empirical_analysis_plan_approved_20260825.txt"
    )
  }

  if (!file.exists(plan_path)) {
    stop(
      paste(
        "Approved Study 3 plan not found:",
        plan_path
      ),
      call. = FALSE
    )
  }

  if (is.null(freeze_dir)) {
    freeze_dir <- file.path(
      project_root,
      "data-raw",
      "study3-results",
      "pre-results-freeze"
    )
  }

  completion_marker <- file.path(
    freeze_dir,
    "FREEZE_COMPLETE.txt"
  )

  # -------------------------------------------------------------------------
  # Immutable rerun behavior
  # -------------------------------------------------------------------------

  if (file.exists(completion_marker)) {
    message(
      paste(
        "A completed Study 3 pre-results freeze already exists.",
        "No artifacts will be overwritten."
      )
    )

    checksum_path <- file.path(
      freeze_dir,
      "study3_frozen_artifact_checksums.csv"
    )

    if (!file.exists(checksum_path)) {
      stop(
        paste(
          "Freeze completion marker exists but checksum record is missing:",
          checksum_path
        ),
        call. = FALSE
      )
    }

    recorded <- utils::read.csv(
      checksum_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    recorded_paths <- file.path(
      project_root,
      recorded$relative_path
    )

    missing_files <- recorded$relative_path[
      !file.exists(
        recorded_paths
      )
    ]

    if (length(missing_files) > 0L) {
      stop(
        paste(
          "Frozen Study 3 artifacts are missing:",
          paste(
            missing_files,
            collapse = "; "
          )
        ),
        call. = FALSE
      )
    }

    current_md5 <- unname(
      tools::md5sum(
        recorded_paths
      )
    )

    if (!identical(
      current_md5,
      recorded$md5
    )) {
      mismatch <- recorded$file[
        current_md5 !=
          recorded$md5
      ]

      stop(
        paste(
          "Frozen Study 3 artifact checksum mismatch:",
          paste(
            mismatch,
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }

    completion_lines <- readLines(
      completion_marker,
      warn = FALSE
    )

    freeze_record_line <- grep(
      "^Freeze record MD5:",
      completion_lines,
      value = TRUE
    )

    if (length(freeze_record_line) != 1L) {
      stop(
        "Freeze completion marker does not contain exactly one freeze-record checksum.",
        call. = FALSE
      )
    }

    recorded_freeze_record_md5 <- trimws(
      sub(
        "^Freeze record MD5:",
        "",
        freeze_record_line
      )
    )

    freeze_record_path <- file.path(
      freeze_dir,
      "study3_freeze_record.rds"
    )

    current_freeze_record_md5 <- unname(
      tools::md5sum(
        freeze_record_path
      )
    )

    if (!identical(
      current_freeze_record_md5,
      recorded_freeze_record_md5
    )) {
      stop(
        "Frozen Study 3 freeze-record checksum mismatch.",
        call. = FALSE
      )
    }

    message(
      paste(
        "Existing Study 3 freeze verified against its recorded checksums.",
        "Nothing was regenerated."
      )
    )

    return(
      invisible(
        readRDS(
          freeze_record_path
        )
      )
    )
  } else {
    if (dir.exists(freeze_dir)) {
      existing <- list.files(
        freeze_dir,
        all.files = TRUE,
        no.. = TRUE
      )

      if (length(existing) > 0L) {
        stop(
          paste(
            "Study 3 freeze directory exists but is incomplete/nonempty.",
            "Inspect it before proceeding:",
            freeze_dir
          ),
          call. = FALSE
        )
      }
    }

    dir.create(
      freeze_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )

    # -----------------------------------------------------------------------
    # Canonical data and structural gate
    # -----------------------------------------------------------------------

    canonical <- lme4::sleepstudy[
      ,
      c(
        "Reaction",
        "Days",
        "Subject"
      )
    ]

    canonical <- canonical[
      order(
        as.character(
          canonical$Subject
        ),
        canonical$Days
      ),
      ,
      drop = FALSE
    ]

    canonical$Subject <- factor(
      as.character(
        canonical$Subject
      ),
      levels = sort(
        unique(
          as.character(
            canonical$Subject
          )
        )
      )
    )

    rownames(canonical) <- NULL

    structure_checks <-
      study3_structural_checks(
        canonical
      )

    if (!all(
      structure_checks$passed
    )) {
      failed <- structure_checks$check[
        !structure_checks$passed
      ]

      stop(
        paste(
          "Canonical sleepstudy structure failed:",
          paste(
            failed,
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }

    # Pre-model diagnostics are permitted by the approved plan.
    pre_model <- cluster_data_explore(
      Reaction ~ Days,
      ~ Subject,
      canonical
    )

    # -----------------------------------------------------------------------
    # Fixed contamination map
    # -----------------------------------------------------------------------

    selection_seed <- 20261105L

    rng_kind_before <- RNGkind()

    set.seed(selection_seed)

    rng_state_after_seed <- .Random.seed

    indices_by_subject <- split(
      seq_len(
        nrow(canonical)
      ),
      canonical$Subject
    )

    selected_index <- unlist(
      lapply(
        indices_by_subject,
        function(index) {
          sample(
            index,
            size = 1L,
            replace = FALSE
          )
        }
      ),
      use.names = FALSE
    )

    contamination_sign <- sample(
      c(-1L, 1L),
      size = length(
        selected_index
      ),
      replace = TRUE
    )

    rng_state_after_selection <-
      .Random.seed

    contamination_map <- data.frame(
      canonical_row = as.integer(
        selected_index
      ),
      Subject = as.character(
        canonical$Subject[
          selected_index
        ]
      ),
      Days = canonical$Days[
        selected_index
      ],
      sign = as.integer(
        contamination_sign
      ),
      stringsAsFactors = FALSE
    )

    # Preserve subject order explicitly.
    contamination_map <- contamination_map[
      match(
        levels(
          canonical$Subject
        ),
        contamination_map$Subject
      ),
      ,
      drop = FALSE
    ]
    rownames(contamination_map) <- NULL

    # -----------------------------------------------------------------------
    # Prespecified reference residual scale
    # -----------------------------------------------------------------------

    reference_model <- stats::lm(
      Reaction ~
        0 +
        Subject +
        Subject:Days,
      data = canonical
    )

    reference_residual_sd <-
      stats::sigma(
        reference_model
      )

    if (!is.finite(
      reference_residual_sd
    ) ||
        reference_residual_sd <= 0) {
      stop(
        "Reference residual SD is not positive and finite.",
        call. = FALSE
      )
    }

    displacement_magnitude <-
      6 * reference_residual_sd

    # -----------------------------------------------------------------------
    # Create perturbed data
    # -----------------------------------------------------------------------

    perturbed <- canonical

    perturbed$Reaction_observed <-
      canonical$Reaction

    perturbed$contaminated <-
      FALSE

    perturbed$contamination_sign <-
      0L

    perturbed$signed_displacement <-
      0

    perturbed$absolute_displacement <-
      0

    perturbed$contaminated[
      selected_index
    ] <- TRUE

    perturbed$contamination_sign[
      selected_index
    ] <- contamination_sign

    perturbed$signed_displacement[
      selected_index
    ] <-
      contamination_sign *
      displacement_magnitude

    perturbed$absolute_displacement[
      selected_index
    ] <-
      displacement_magnitude

    perturbed$Reaction[
      selected_index
    ] <-
      canonical$Reaction[
        selected_index
      ] +
      contamination_sign *
        displacement_magnitude

    realized_contamination_prop <-
      mean(
        perturbed$contaminated
      )

    # -----------------------------------------------------------------------
    # Save immutable input artifacts
    # -----------------------------------------------------------------------

    canonical_csv <- file.path(
      freeze_dir,
      "sleepstudy_canonical.csv"
    )

    canonical_rds <- file.path(
      freeze_dir,
      "sleepstudy_canonical.rds"
    )

    map_csv <- file.path(
      freeze_dir,
      "study3_contamination_map.csv"
    )

    map_rds <- file.path(
      freeze_dir,
      "study3_contamination_map.rds"
    )

    perturbed_csv <- file.path(
      freeze_dir,
      "sleepstudy_perturbed.csv"
    )

    perturbed_rds <- file.path(
      freeze_dir,
      "sleepstudy_perturbed.rds"
    )

    structure_csv <- file.path(
      freeze_dir,
      "study3_structure_checks.csv"
    )

    pre_overall_csv <- file.path(
      freeze_dir,
      "study3_pre_model_overall.csv"
    )

    pre_clusters_csv <- file.path(
      freeze_dir,
      "study3_pre_model_cluster_summary.csv"
    )

    study3_write_csv_atomic(
      canonical,
      canonical_csv
    )

    study3_save_rds_atomic(
      canonical,
      canonical_rds
    )

    study3_write_csv_atomic(
      contamination_map,
      map_csv
    )

    study3_save_rds_atomic(
      contamination_map,
      map_rds
    )

    study3_write_csv_atomic(
      perturbed,
      perturbed_csv
    )

    study3_save_rds_atomic(
      perturbed,
      perturbed_rds
    )

    study3_write_csv_atomic(
      structure_checks,
      structure_csv
    )

    study3_write_csv_atomic(
      pre_model$overall,
      pre_overall_csv
    )

    study3_write_csv_atomic(
      pre_model$cluster_summary,
      pre_clusters_csv
    )

    # -----------------------------------------------------------------------
    # Freeze metadata
    # -----------------------------------------------------------------------

    source_files <- c(
      approved_plan = plan_path,
      study3_empirical = file.path(
        project_root,
        "R",
        "study3_empirical.R"
      ),
      study3_empirical_helpers = file.path(
        project_root,
        "R",
        "study3_empirical_helpers.R"
      ),
      cluster_diagnostics = file.path(
        project_root,
        "R",
        "cluster_diagnostics.R"
      ),
      study1_helpers = file.path(
        project_root,
        "R",
        "pwr_func_study1_helpers.R"
      ),
      study2_helpers = file.path(
        project_root,
        "R",
        "pwr_func_study2_helpers.R"
      ),
      robust_mixed_models = file.path(
        project_root,
        "R",
        "robust_mixed_models.R"
      ),
      description = file.path(
        project_root,
        "DESCRIPTION"
      )
    )

    if (!all(
      file.exists(
        source_files
      )
    )) {
      stop(
        paste(
          "Required source file missing:",
          paste(
            names(
              source_files
            )[
              !file.exists(
                source_files
              )
            ],
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }

    normalized_source_files <- normalizePath(
      source_files,
      winslash = "/",
      mustWork = TRUE
    )

    project_prefix <- paste0(
      normalizePath(
        project_root,
        winslash = "/",
        mustWork = TRUE
      ),
      "/"
    )

    source_checksums <- data.frame(
      source = names(
        source_files
      ),
      relative_path = substring(
        normalized_source_files,
        nchar(project_prefix) + 1L
      ),
      md5 = unname(
        tools::md5sum(
          normalized_source_files
        )
      ),
      stringsAsFactors = FALSE
    )

    package_names <- c(
      "mmiCATs",
      "lme4",
      "lmerTest",
      "pbkrtest",
      "clubSandwich",
      "clusterSEs",
      "robust",
      "robustbase",
      "robustlmm",
      "ggplot2",
      "testthat"
    )

    package_versions <- data.frame(
      package = package_names,
      version = vapply(
        package_names,
        function(package_name) {
          if (!requireNamespace(
            package_name,
            quietly = TRUE
          )) {
            return(
              NA_character_
            )
          }

          as.character(
            utils::packageVersion(
              package_name
            )
          )
        },
        FUN.VALUE = character(1)
      ),
      stringsAsFactors = FALSE
    )

    freeze_metadata <- data.frame(
      field = c(
        "approved_plan",
        "dataset",
        "outcome",
        "predictor",
        "cluster",
        "random_slope_model",
        "random_intercept_model",
        "alpha",
        "nominal_contamination_prop",
        "realized_contamination_prop",
        "contaminated_observations",
        "selection_seed",
        "reference_scale_model",
        "reference_residual_sd",
        "contamination_multiplier",
        "absolute_displacement",
        "comparative_models_fit_during_freeze"
      ),
      value = c(
        basename(plan_path),
        "lme4::sleepstudy",
        "Reaction",
        "Days",
        "Subject",
        "Reaction ~ Days + (1 + Days || Subject)",
        "Reaction ~ Days + (1 | Subject)",
        "0.05",
        "0.05",
        format(
          realized_contamination_prop,
          digits = 17
        ),
        as.character(
          length(
            selected_index
          )
        ),
        as.character(
          selection_seed
        ),
        "Reaction ~ 0 + Subject + Subject:Days",
        format(
          reference_residual_sd,
          digits = 17
        ),
        "6",
        format(
          displacement_magnitude,
          digits = 17
        ),
        "FALSE"
      ),
      stringsAsFactors = FALSE
    )

    rng_record <- list(
      rng_kind_before =
        rng_kind_before,
      selection_seed =
        selection_seed,
      rng_state_after_seed =
        rng_state_after_seed,
      rng_state_after_selection =
        rng_state_after_selection
    )

    metadata_csv <- file.path(
      freeze_dir,
      "study3_freeze_metadata.csv"
    )

    source_csv <- file.path(
      freeze_dir,
      "study3_source_checksums.csv"
    )

    packages_csv <- file.path(
      freeze_dir,
      "study3_package_versions.csv"
    )

    rng_rds <- file.path(
      freeze_dir,
      "study3_rng_record.rds"
    )

    session_txt <- file.path(
      freeze_dir,
      "session_info.txt"
    )

    study3_write_csv_atomic(
      freeze_metadata,
      metadata_csv
    )

    study3_write_csv_atomic(
      source_checksums,
      source_csv
    )

    study3_write_csv_atomic(
      package_versions,
      packages_csv
    )

    study3_save_rds_atomic(
      rng_record,
      rng_rds
    )

    if (file.exists(session_txt)) {
      stop(
        paste(
          "Refusing to overwrite frozen Study 3 artifact:",
          session_txt
        ),
        call. = FALSE
      )
    }

    writeLines(
      utils::capture.output(
        utils::sessionInfo()
      ),
      con = session_txt,
      useBytes = TRUE
    )

    # -----------------------------------------------------------------------
    # Record checksums of every immutable data/metadata artifact created above.
    # The checksum table and completion marker are intentionally excluded from
    # their own checksum set.
    # -----------------------------------------------------------------------

    frozen_artifacts <- c(
      canonical_csv,
      canonical_rds,
      map_csv,
      map_rds,
      perturbed_csv,
      perturbed_rds,
      structure_csv,
      pre_overall_csv,
      pre_clusters_csv,
      metadata_csv,
      source_csv,
      packages_csv,
      rng_rds,
      session_txt
    )

    artifact_checksums <-
      study3_file_md5(
        frozen_artifacts,
        project_root = project_root
      )

    checksum_path <- file.path(
      freeze_dir,
      "study3_frozen_artifact_checksums.csv"
    )

    study3_write_csv_atomic(
      artifact_checksums,
      checksum_path
    )

    freeze_record <- list(
      freeze_metadata =
        freeze_metadata,
      structure_checks =
        structure_checks,
      contamination_map =
        contamination_map,
      source_checksums =
        source_checksums,
      package_versions =
        package_versions,
      artifact_checksums =
        artifact_checksums,
      rng_record =
        rng_record,
      session_info =
        utils::sessionInfo()
    )

    freeze_record_path <- file.path(
      freeze_dir,
      "study3_freeze_record.rds"
    )

    study3_save_rds_atomic(
      freeze_record,
      freeze_record_path
    )

    # Add the freeze record itself to a separate top-level checksum line.
    freeze_record_md5 <- unname(
      tools::md5sum(
        freeze_record_path
      )
    )

    completion_lines <- c(
      "mmiCATs Study 3 Phase 6B pre-results freeze COMPLETE",
      "",
      paste(
        "Created:",
        format(
          Sys.time(),
          tz = "America/Los_Angeles",
          usetz = TRUE
        )
      ),
      paste(
        "Approved plan MD5:",
        unname(
          tools::md5sum(
            plan_path
          )
        )
      ),
      paste(
        "Canonical data MD5:",
        unname(
          tools::md5sum(
            canonical_csv
          )
        )
      ),
      paste(
        "Contamination map MD5:",
        unname(
          tools::md5sum(
            map_csv
          )
        )
      ),
      paste(
        "Perturbed data MD5:",
        unname(
          tools::md5sum(
            perturbed_csv
          )
        )
      ),
      paste(
        "Freeze record MD5:",
        freeze_record_md5
      ),
      paste(
        "Selection seed:",
        selection_seed
      ),
      paste(
        "Contaminated observations:",
        length(
          selected_index
        ),
        "of",
        nrow(canonical)
      ),
      paste(
        "Reference residual SD:",
        format(
          reference_residual_sd,
          digits = 17
        )
      ),
      paste(
        "Absolute displacement:",
        format(
          displacement_magnitude,
          digits = 17
        )
      ),
      "Comparative Study 3 models fit during Phase 6B: FALSE"
    )

    if (file.exists(completion_marker)) {
      stop(
        paste(
          "Refusing to overwrite:",
          completion_marker
        ),
        call. = FALSE
      )
    }

    writeLines(
      completion_lines,
      completion_marker,
      useBytes = TRUE
    )

    message("")
    message(
      "Study 3 Phase 6B pre-results freeze created successfully."
    )
    message(
      paste(
        "Freeze directory:",
        freeze_dir
      )
    )
    message(
      paste(
        "Selected:",
        length(selected_index),
        "of",
        nrow(canonical),
        "observations."
      )
    )
    message(
      paste(
        "Reference residual SD:",
        format(
          reference_residual_sd,
          digits = 10
        )
      )
    )
    message(
      paste(
        "Absolute vertical displacement:",
        format(
          displacement_magnitude,
          digits = 10
        )
      )
    )
    message(
      "No comparative Study 3 methods were fit."
    )
    invisible(
      freeze_record
    )

  }
}


#' Run the Study 3 Empirical Illustration
#'
#' Runs the prespecified nine-method observed/perturbed Study 3 comparison
#' using an existing immutable empirical freeze. The contamination map is
#' never regenerated by this function.
#'
#' @param freeze_dir Completed Study 3 pre-results freeze directory.
#'   If `NULL`, uses the project default.
#' @param output_dir Study 3 result directory. Use a new directory for a
#'   post-results amended rerun.
#' @param project_root Project root. If `NULL`, located automatically.
#' @param verify_original_sources Whether current project source files must
#'   exactly match the source checksums recorded in the Phase 6B freeze.
#'   This is `FALSE` by default because documented post-results amendments
#'   necessarily change current source code while leaving frozen artifacts
#'   immutable.
#'
#' @return Invisibly, the saved Study 3 result record when available.
#' @export
run_study3_empirical <- function(
    freeze_dir = NULL,
    output_dir = NULL,
    project_root = NULL,
    verify_original_sources = FALSE) {
  if (is.null(project_root)) {
    project_root <- study3c_find_project_root()
  }

  project_root <- normalizePath(
    project_root,
    winslash = "/",
    mustWork = TRUE
  )

  if (is.null(freeze_dir)) {
    freeze_dir <- file.path(
      project_root,
      "data-raw",
      "study3-results",
      "pre-results-freeze"
    )
  }

  freeze <- study3c_verify_freeze(
    project_root = project_root,
    freeze_dir = freeze_dir,
    verify_original_sources =
      verify_original_sources
  )

  freeze_dir <- freeze$freeze_dir

  if (is.null(output_dir)) {
    output_dir <- file.path(
      project_root,
      "data-raw",
      "study3-results",
      "definitive-study3"
    )
  }

  definitive_dir <- output_dir
  checkpoint_dir <- file.path(definitive_dir, "checkpoints")
  plot_dir <- file.path(definitive_dir, "plots")
  completion_marker <- file.path(definitive_dir, "STUDY3_COMPLETE.txt")
  checksum_path <- file.path(definitive_dir, "study3_output_checksums.csv")

  if (file.exists(completion_marker)) {
    if (!file.exists(checksum_path)) {
      stop(
        "Study 3 completion marker exists but output checksums are missing.",
        call. = FALSE
      )
    }

    x <- utils::read.csv(
      checksum_path, stringsAsFactors = FALSE, check.names = FALSE
    )
    paths <- file.path(project_root, x$relative_path)

    if (!all(file.exists(paths))) {
      stop(
        "Completed Study 3 output is missing: ",
        paste(x$relative_path[!file.exists(paths)], collapse = ", "),
        call. = FALSE
      )
    }

    current <- unname(tools::md5sum(paths))
    if (!identical(current, x$md5)) {
      stop(
        "Completed Study 3 output checksum mismatch: ",
        paste(x$relative_path[current != x$md5], collapse = ", "),
        call. = FALSE
      )
    }

    message(
      "Completed Study 3 definitive analysis verified. No results were regenerated."
    )

  } else {

    dir.create(definitive_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

    methods <- study3c_methods()
    analysis_seed <- study3c_analysis_seed()

    canonical_path <- file.path(freeze_dir, "sleepstudy_canonical.rds")
    perturbed_path <- file.path(freeze_dir, "sleepstudy_perturbed.rds")
    map_path <- file.path(freeze_dir, "study3_contamination_map.rds")

    canonical <- readRDS(canonical_path)
    perturbed <- readRDS(perturbed_path)
    contamination_map <- readRDS(map_path)

    canonical_md5 <- unname(tools::md5sum(canonical_path))
    perturbed_md5 <- unname(tools::md5sum(perturbed_path))

    run_full_checkpoint <- function(
        data,
        dataset,
        input_md5,
        path) {

      if (file.exists(path)) {
        x <- readRDS(path)
        ok <- is.list(x) &&
          identical(x$status, "complete") &&
          identical(x$dataset, dataset) &&
          identical(x$input_md5, input_md5) &&
          identical(x$methods, methods) &&
          identical(as.integer(x$seed), analysis_seed)

        if (!ok) {
          stop(
            "Existing full-analysis checkpoint does not match frozen request: ",
            path,
            call. = FALSE
          )
        }

        message("Reusing ", dataset, " full-analysis checkpoint.")
        return(x$result)
      }

      message("Study 3: fitting ", dataset, " nine-method comparison...")
      started <- Sys.time()

      result <- study3c_fit_full(
        data = data,
        methods = methods,
        seed = analysis_seed
      )

      completed <- Sys.time()

      checkpoint <- list(
        status = "complete",
        dataset = dataset,
        input_md5 = input_md5,
        methods = methods,
        seed = analysis_seed,
        result = result,
        started_at = started,
        completed_at = completed,
        elapsed_sec = as.numeric(difftime(completed, started, units = "secs"))
      )

      definitive_save_rds_atomic(checkpoint, path)
      result
    }

    observed <- run_full_checkpoint(
      canonical,
      "observed",
      canonical_md5,
      file.path(checkpoint_dir, "observed_full.rds")
    )

    perturbed_result <- run_full_checkpoint(
      perturbed,
      "perturbed",
      perturbed_md5,
      file.path(checkpoint_dir, "perturbed_full.rds")
    )

    observed_dat <- observed$analysis_data
    perturbed_dat <- perturbed_result$analysis_data

    observed_loo_dir <- file.path(checkpoint_dir, "loo-observed")
    perturbed_loo_dir <- file.path(checkpoint_dir, "loo-perturbed")
    dir.create(observed_loo_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(perturbed_loo_dir, recursive = TRUE, showWarnings = FALSE)

    message("Study 3: observed leave-one-Subject-out checkpoints...")
    for (i in seq_along(levels(observed_dat$cluster))) {
      z <- study3c_run_loo_checkpoint(
        dat = observed_dat,
        methods = methods,
        seed = analysis_seed,
        full_comparison = observed$comparison,
        cluster_index = i,
        dataset = "observed",
        input_md5 = canonical_md5,
        checkpoint_dir = observed_loo_dir
      )
      message(
        sprintf(
          "  Observed Subject %s: %s",
          levels(observed_dat$cluster)[i],
          z$action
        )
      )
    }

    message("Study 3: perturbed leave-one-Subject-out checkpoints...")
    for (i in seq_along(levels(perturbed_dat$cluster))) {
      z <- study3c_run_loo_checkpoint(
        dat = perturbed_dat,
        methods = methods,
        seed = analysis_seed,
        full_comparison = perturbed_result$comparison,
        cluster_index = i,
        dataset = "perturbed",
        input_md5 = perturbed_md5,
        checkpoint_dir = perturbed_loo_dir
      )
      message(
        sprintf(
          "  Perturbed Subject %s: %s",
          levels(perturbed_dat$cluster)[i],
          z$action
        )
      )
    }

    observed_loo <- study3c_collect_loo(
      observed_dat, methods, analysis_seed, "observed",
      canonical_md5, observed_loo_dir
    )

    perturbed_loo <- study3c_collect_loo(
      perturbed_dat, methods, analysis_seed, "perturbed",
      perturbed_md5, perturbed_loo_dir
    )

    definitive_write_csv_atomic(
      observed_loo$status,
      file.path(definitive_dir, "study3_observed_loo_status.csv")
    )
    definitive_write_csv_atomic(
      perturbed_loo$status,
      file.path(definitive_dir, "study3_perturbed_loo_status.csv")
    )

    if (!observed_loo$complete || !perturbed_loo$complete) {
      stop(
        paste(
          "Study 3 LOO checkpoints are incomplete.",
          "Rerun to retry matching error checkpoints."
        ),
        call. = FALSE
      )
    }

    observed_loo_results <- observed_loo$results
    observed_loo_results$dataset <- "Observed"

    perturbed_loo_results <- perturbed_loo$results
    perturbed_loo_results$dataset <- "Perturbed"

    loo_results <- rbind(observed_loo_results, perturbed_loo_results)
    rownames(loo_results) <- NULL

    comparison <- study3c_make_comparison(
      observed$comparison,
      perturbed_result$comparison
    )

    contamination_columns <- data.frame(
      row_id = seq_len(nrow(perturbed)),
      Subject = as.character(perturbed$Subject),
      Days = perturbed$Days,
      Reaction_observed = perturbed$Reaction_observed,
      Reaction_perturbed = perturbed$Reaction,
      contaminated = perturbed$contaminated,
      contamination_sign = perturbed$contamination_sign,
      signed_displacement = perturbed$signed_displacement,
      absolute_displacement = perturbed$absolute_displacement,
      stringsAsFactors = FALSE
    )

    contaminated_diagnostics <- merge(
      contamination_columns,
      perturbed_result$observation_diagnostics,
      by = "row_id",
      all.x = TRUE,
      sort = FALSE
    )
    contaminated_diagnostics <- contaminated_diagnostics[
      contaminated_diagnostics$contaminated %in% TRUE,
      ,
      drop = FALSE
    ]
    contaminated_diagnostics <- contaminated_diagnostics[
      order(contaminated_diagnostics$Subject),
      ,
      drop = FALSE
    ]
    rownames(contaminated_diagnostics) <- NULL

    tables <- list(
      study3_observed_method_comparison = observed$comparison,
      study3_perturbed_method_comparison = perturbed_result$comparison,
      study3_observed_vs_perturbed = comparison,
      study3_observed_cluster_fits = observed$cluster_fits,
      study3_perturbed_cluster_fits = perturbed_result$cluster_fits,
      study3_observed_cluster_slope_differences =
        observed$cluster_slope_differences,
      study3_perturbed_cluster_slope_differences =
        perturbed_result$cluster_slope_differences,
      study3_observed_observation_diagnostics =
        observed$observation_diagnostics,
      study3_perturbed_observation_diagnostics =
        perturbed_result$observation_diagnostics,
      study3_contaminated_observation_diagnostics =
        contaminated_diagnostics,
      study3_leave_one_subject_out = loo_results,
      study3_observed_loo_status = observed_loo$status,
      study3_perturbed_loo_status = perturbed_loo$status
    )

    table_paths <- vapply(
      names(tables),
      function(nm) file.path(definitive_dir, paste0(nm, ".csv")),
      FUN.VALUE = character(1)
    )

    for (nm in names(tables)) {
      definitive_write_csv_atomic(tables[[nm]], table_paths[[nm]])
    }

    # Preserve the full diagnostic objects (including their ggplot objects).
    result_rds_path <- file.path(
      definitive_dir, "study3_definitive_results.rds"
    )

    results_record <- list(
      methods = methods,
      analysis_seed = analysis_seed,
      observed = observed,
      perturbed = perturbed_result,
      observed_vs_perturbed = comparison,
      contamination_map = contamination_map,
      contaminated_observation_diagnostics = contaminated_diagnostics,
      leave_one_subject_out = loo_results,
      observed_loo_status = observed_loo$status,
      perturbed_loo_status = perturbed_loo$status,
      phase6b_freeze_record = readRDS(
        file.path(freeze_dir, "study3_freeze_record.rds")
      ),
      session_info = utils::sessionInfo()
    )

    definitive_save_rds_atomic(results_record, result_rds_path)

    # Save the six standard diagnostic plots for each dataset.
    plot_sizes <- list(
      method_comparison = c(8, 5.5),
      cluster_slopes = c(8, 7),
      slope_differences = c(8, 7),
      residual_leverage = c(7, 5.5),
      robust_weights = c(8, 5.5),
      cluster_fits = c(10, 11)
    )

    plot_paths <- character(0)

    save_set <- function(diagnostic, prefix) {
      paths <- character(0)
      for (nm in intersect(names(plot_sizes), names(diagnostic$plots))) {
        dims <- plot_sizes[[nm]]
        path <- file.path(plot_dir, paste0(prefix, "_", nm, ".pdf"))
        study3c_save_plot(
          diagnostic$plots[[nm]], path,
          width = dims[1L], height = dims[2L]
        )
        paths <- c(paths, path)
      }
      paths
    }

    plot_paths <- c(
      plot_paths,
      save_set(observed, "study3_observed"),
      save_set(perturbed_result, "study3_perturbed")
    )

    cross_path <- file.path(
      plot_dir, "study3_observed_vs_perturbed.pdf"
    )
    study3c_save_plot(
      study3c_cross_dataset_plot(comparison),
      cross_path,
      width = 8.5,
      height = 6
    )
    plot_paths <- c(plot_paths, cross_path)

    loo_plot_path <- file.path(
      plot_dir, "study3_leave_one_subject_out.pdf"
    )
    study3c_save_plot(
      study3c_loo_plot(loo_results),
      loo_plot_path,
      width = 18,
      height = 9
    )
    plot_paths <- c(plot_paths, loo_plot_path)

    metadata <- data.frame(
      field = c(
        "analysis_seed",
        "method_count",
        "methods",
        "alpha",
        "observed_input_md5",
        "perturbed_input_md5",
        "observed_subjects",
        "perturbed_subjects",
        "observed_loo_rows",
        "perturbed_loo_rows",
        "contaminated_observations",
        "comparative_analysis_complete"
      ),
      value = c(
        as.character(analysis_seed),
        as.character(length(methods)),
        paste(methods, collapse = ","),
        "0.05",
        canonical_md5,
        perturbed_md5,
        as.character(nlevels(observed_dat$cluster)),
        as.character(nlevels(perturbed_dat$cluster)),
        as.character(nrow(observed_loo_results)),
        as.character(nrow(perturbed_loo_results)),
        as.character(sum(perturbed$contaminated)),
        "TRUE"
      ),
      stringsAsFactors = FALSE
    )

    metadata_path <- file.path(
      definitive_dir, "study3_execution_metadata.csv"
    )
    definitive_write_csv_atomic(metadata, metadata_path)

    package_versions_path <- file.path(
      definitive_dir, "study3_package_versions.csv"
    )
    frozen_versions <- utils::read.csv(
      file.path(freeze_dir, "study3_package_versions.csv"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    definitive_write_csv_atomic(frozen_versions, package_versions_path)

    session_path <- file.path(definitive_dir, "session_info.txt")
    writeLines(
      utils::capture.output(utils::sessionInfo()),
      session_path,
      useBytes = TRUE
    )

    output_paths <- c(
      unname(table_paths),
      result_rds_path,
      plot_paths,
      metadata_path,
      package_versions_path,
      session_path
    )

    output_checksums <- study3c_output_checksums(
      output_paths,
      project_root
    )
    definitive_write_csv_atomic(output_checksums, checksum_path)

    writeLines(
      c(
        "mmiCATs Study 3 definitive empirical analysis COMPLETE",
        "",
        paste(
          "Completed:",
          format(Sys.time(), tz = "America/Los_Angeles", usetz = TRUE)
        ),
        paste("Methods:", paste(methods, collapse = ", ")),
        paste("Analysis seed:", analysis_seed),
        paste("Observed Subjects:", nlevels(observed_dat$cluster)),
        paste("Perturbed Subjects:", nlevels(perturbed_dat$cluster)),
        paste("Observed LOO rows:", nrow(observed_loo_results)),
        paste("Perturbed LOO rows:", nrow(perturbed_loo_results)),
        paste("Contaminated observations:", sum(perturbed$contaminated)),
        paste("Output checksum rows:", nrow(output_checksums))
      ),
      completion_marker,
      useBytes = TRUE
    )

    message("")
    message("Study 3 definitive comparative analysis complete.")
    message("Results saved to: ", definitive_dir)
    message("")
    message("Observed vs perturbed method comparison:")

    print(
      comparison[
        ,
        c(
          "method", "method_label",
          "observed_estimate", "observed_conf_low", "observed_conf_high",
          "perturbed_estimate", "perturbed_conf_low", "perturbed_conf_high",
          "estimate_change", "absolute_estimate_change", "ci_width_change"
        ),
        drop = FALSE
      ],
      row.names = FALSE
    )
  }
  result_path <- file.path(
    definitive_dir,
    "study3_definitive_results.rds"
  )

  if (file.exists(result_path)) {
    return(
      invisible(
        readRDS(result_path)
      )
    )
  }

  invisible(NULL)

}
