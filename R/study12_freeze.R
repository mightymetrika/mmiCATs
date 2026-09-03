# Prospective Study 1/2 source freeze and pre-results registration gate.

#' Prepare the Prospective Study 1/2 Source Freeze
#'
#' Creates an immutable, non-model-fitting freeze of the exact manuscript-version
#' source state and the frozen Study 1/2 simulation specifications. The function
#' refuses to create a new freeze unless the Git worktree is clean, the current
#' commit matches its configured upstream, the definitive Study 1/2 output
#' directories are absent/empty, and every prospective scientific-design check
#' passes.
#'
#' This function does not run Study 1 or Study 2 and does not perform the external
#' registration. After this source freeze is archived prospectively, call
#' `record_study12_registration()` with the permanent registration location.
#'
#' @param project_root Source checkout root containing DESCRIPTION. If `NULL`,
#'   located from the working directory.
#' @param protocol_path Approved prospective Study 1/2 protocol. If `NULL`, uses
#'   `data-raw/study12_definitive_protocol_approved_20260902.txt`.
#' @param freeze_dir Destination for the immutable source-freeze bundle. If
#'   `NULL`, uses `data-raw/study12-results/pre-results-freeze`.
#'
#' @return Invisibly, the saved Study 1/2 freeze record.
#' @export
prepare_study12_freeze <- function(
    project_root = NULL,
    protocol_path = NULL,
    freeze_dir = NULL) {
  if (is.null(project_root)) {
    project_root <- study12f_find_project_root()
  }

  project_root <- study12f_normalize_root(
    project_root
  )

  if (is.null(protocol_path)) {
    protocol_path <-
      study12f_default_protocol_path(
        project_root
      )
  }

  if (!file.exists(protocol_path)) {
    stop(
      paste(
        "Approved prospective Study 1/2 protocol not found:",
        protocol_path
      ),
      call. = FALSE
    )
  }

  protocol_path <- normalizePath(
    protocol_path,
    winslash = "/",
    mustWork = TRUE
  )

  if (is.null(freeze_dir)) {
    freeze_dir <- study12f_default_freeze_dir(
      project_root
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
        "A completed prospective Study 1/2 source freeze already exists.",
        "No frozen artifacts will be overwritten."
      )
    )

    freeze_record <- study12f_verify_freeze(
      project_root = project_root,
      freeze_dir = freeze_dir,
      verify_current_source = TRUE,
      verify_current_git = TRUE,
      verify_package_versions = TRUE,
      verify_rng = TRUE
    )

    if (file.exists(
      file.path(
        freeze_dir,
        "REGISTRATION_COMPLETE.txt"
      )
    )) {
      study12f_verify_registration(
        freeze_dir
      )

      message(
        "The external pre-results registration record is also complete and verified."
      )
    } else {
      message(
        paste(
          "The source freeze is complete, but the external registration",
          "has not yet been recorded."
        )
      )
    }

    return(
      invisible(
        freeze_record
      )
    )
  }

  if (dir.exists(freeze_dir)) {
    existing <- list.files(
      freeze_dir,
      all.files = TRUE,
      no.. = TRUE
    )

    if (length(existing) > 0L) {
      stop(
        paste(
          "Study 1/2 freeze directory exists but is incomplete/nonempty.",
          "Inspect it before proceeding:",
          freeze_dir
        ),
        call. = FALSE
      )
    }
  }

  # -------------------------------------------------------------------------
  # Prospective preflight: no definitive results + clean/pushed exact source
  # -------------------------------------------------------------------------

  output_absent <-
    study12f_definitive_outputs_absent(
      project_root
    )

  if (!all(output_absent)) {
    bad <- names(output_absent)[
      !output_absent
    ]

    stop(
      paste(
        "Definitive Study 1/2 output already exists for:",
        paste(
          bad,
          collapse = ", "
        ),
        "The prospective gate cannot be created after definitive results exist."
      ),
      call. = FALSE
    )
  }

  git_record <- study12f_git_record(
    project_root = project_root,
    require_clean = TRUE,
    require_pushed = TRUE
  )

  # -------------------------------------------------------------------------
  # Reconstruct the already-frozen scientific specification without fitting
  # -------------------------------------------------------------------------

  study1_design <- study1d_frozen_design()
  study2_design <- study2d_frozen_design()
  method_schedule <- study12f_method_schedule()
  seed_blocks <- study12f_seed_blocks()
  replicate_seeds <-
    study12f_replicate_seed_table()
  shard_plan <- study12f_shard_plan()

  scientific_checks <-
    study12f_scientific_checks(
      study1_design = study1_design,
      study2_design = study2_design,
      method_schedule =
        method_schedule,
      seed_blocks = seed_blocks,
      replicate_seeds =
        replicate_seeds,
      shard_plan = shard_plan
    )

  if (!all(
    scientific_checks$passed
  )) {
    failed <- scientific_checks$check[
      !scientific_checks$passed
    ]

    stop(
      paste(
        "Prospective Study 1/2 scientific-design checks failed:",
        paste(
          failed,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  source_checksums <-
    study12f_source_checksums(
      project_root = project_root,
      protocol_path = protocol_path
    )

  package_versions <-
    study12f_package_versions(
      project_root
    )

  rng_record <- list(
    rng_kind = RNGkind(),
    machine_integer_max =
      .Machine$integer.max,
    replicate_seed_rule = paste(
      "set.seed(condition_seed);",
      "sample.int(.Machine$integer.max, 2000, replace = FALSE)"
    ),
    method_seed_rule = paste(
      "Method-specific seeds are deterministically derived from each",
      "replication seed by the validated Study 1/2 package machinery."
    )
  )

  planned_output_dirs <-
    study12f_definitive_output_dirs(
      project_root
    )

  protocol_md5 <- unname(
    tools::md5sum(
      protocol_path
    )
  )

  freeze_metadata <- data.frame(
    field = c(
      "freeze_scope",
      "package",
      "package_version",
      "git_commit",
      "git_branch",
      "git_upstream",
      "git_pushed_to_upstream",
      "git_worktree_clean_before_freeze",
      "protocol_file",
      "protocol_md5",
      "study1_conditions",
      "study1_methods",
      "study1_reps_per_condition",
      "study1_seed_base",
      "study2_conditions",
      "study2_methods",
      "study2_reps_per_condition",
      "study2_seed_base",
      "shard_size",
      "shards_per_condition",
      "minimum_free_gb",
      "study2_minimum_usable_reps",
      "study1_output_dir",
      "study2_output_dir",
      "definitive_study1_results_present_before_freeze",
      "definitive_study2_results_present_before_freeze",
      "definitive_simulations_run_during_freeze",
      "registration_status"
    ),
    value = c(
      "Prospective definitive Study 1/2 manuscript-version source freeze",
      "mmiCATs",
      package_versions$version[
        package_versions$package ==
          "mmiCATs"
      ],
      git_record$commit,
      git_record$branch,
      git_record$upstream,
      as.character(
        git_record$pushed_to_upstream
      ),
      as.character(
        git_record$worktree_clean_before_freeze
      ),
      basename(protocol_path),
      protocol_md5,
      "18",
      "7",
      "2000",
      "20260815",
      "24",
      "9",
      "2000",
      "20260905",
      "10",
      "200",
      "2",
      "1900",
      study12f_relative_path(
        planned_output_dirs["study1"],
        project_root
      ),
      study12f_relative_path(
        planned_output_dirs["study2"],
        project_root
      ),
      "FALSE",
      "FALSE",
      "FALSE",
      "SOURCE_FREEZE_COMPLETE_REGISTRATION_PENDING"
    ),
    stringsAsFactors = FALSE
  )

  # -------------------------------------------------------------------------
  # Create immutable freeze bundle
  # -------------------------------------------------------------------------

  dir.create(
    freeze_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  study1_csv <- file.path(
    freeze_dir,
    "study1_frozen_design.csv"
  )
  study1_rds <- file.path(
    freeze_dir,
    "study1_frozen_design.rds"
  )
  study2_csv <- file.path(
    freeze_dir,
    "study2_frozen_design.csv"
  )
  study2_rds <- file.path(
    freeze_dir,
    "study2_frozen_design.rds"
  )
  methods_csv <- file.path(
    freeze_dir,
    "study12_method_schedule.csv"
  )
  seed_blocks_csv <- file.path(
    freeze_dir,
    "study12_seed_blocks.csv"
  )
  seeds_csv <- file.path(
    freeze_dir,
    "study12_replicate_seeds.csv"
  )
  seeds_rds <- file.path(
    freeze_dir,
    "study12_replicate_seeds.rds"
  )
  shard_csv <- file.path(
    freeze_dir,
    "study12_shard_plan.csv"
  )
  shard_rds <- file.path(
    freeze_dir,
    "study12_shard_plan.rds"
  )
  git_csv <- file.path(
    freeze_dir,
    "study12_git_record.csv"
  )
  source_csv <- file.path(
    freeze_dir,
    "study12_source_checksums.csv"
  )
  packages_csv <- file.path(
    freeze_dir,
    "study12_package_versions.csv"
  )
  rng_rds <- file.path(
    freeze_dir,
    "study12_rng_record.rds"
  )
  checks_csv <- file.path(
    freeze_dir,
    "study12_scientific_checks.csv"
  )
  metadata_csv <- file.path(
    freeze_dir,
    "study12_freeze_metadata.csv"
  )
  protocol_copy <- file.path(
    freeze_dir,
    "study12_definitive_protocol.txt"
  )
  registration_manifest <- file.path(
    freeze_dir,
    "study12_registration_manifest.txt"
  )
  session_txt <- file.path(
    freeze_dir,
    "session_info.txt"
  )

  study12f_write_csv_atomic(
    study1_design,
    study1_csv
  )
  study12f_save_rds_atomic(
    study1_design,
    study1_rds
  )
  study12f_write_csv_atomic(
    study2_design,
    study2_csv
  )
  study12f_save_rds_atomic(
    study2_design,
    study2_rds
  )
  study12f_write_csv_atomic(
    method_schedule,
    methods_csv
  )
  study12f_write_csv_atomic(
    seed_blocks,
    seed_blocks_csv
  )
  study12f_write_csv_atomic(
    replicate_seeds,
    seeds_csv
  )
  study12f_save_rds_atomic(
    replicate_seeds,
    seeds_rds
  )
  study12f_write_csv_atomic(
    shard_plan,
    shard_csv
  )
  study12f_save_rds_atomic(
    shard_plan,
    shard_rds
  )
  study12f_write_csv_atomic(
    git_record,
    git_csv
  )
  study12f_write_csv_atomic(
    source_checksums,
    source_csv
  )
  study12f_write_csv_atomic(
    package_versions,
    packages_csv
  )
  study12f_save_rds_atomic(
    rng_record,
    rng_rds
  )
  study12f_write_csv_atomic(
    scientific_checks,
    checks_csv
  )
  study12f_write_csv_atomic(
    freeze_metadata,
    metadata_csv
  )
  study12f_copy_atomic(
    protocol_path,
    protocol_copy
  )

  registration_manifest_lines <- c(
    "mmiCATs definitive Study 1/2 prospective registration manifest",
    "",
    paste(
      "Created:",
      format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S %Z"
      )
    ),
    paste(
      "Git commit:",
      git_record$commit
    ),
    paste(
      "Git upstream:",
      git_record$upstream
    ),
    paste(
      "Git upstream commit:",
      git_record$upstream_commit
    ),
    paste(
      "Git commit verified pushed:",
      git_record$pushed_to_upstream
    ),
    paste(
      "Worktree clean before freeze:",
      git_record$worktree_clean_before_freeze
    ),
    paste(
      "Protocol file:",
      basename(protocol_copy)
    ),
    paste(
      "Protocol MD5:",
      protocol_md5
    ),
    paste(
      "Source checksum record:",
      basename(source_csv)
    ),
    paste(
      "Package version record:",
      basename(packages_csv)
    ),
    paste(
      "Study 1 conditions/methods/reps:",
      "18 / 7 / 2000"
    ),
    paste(
      "Study 2 conditions/methods/reps:",
      "24 / 9 / 2000"
    ),
    paste(
      "Study 1 condition seed blocks:",
      "20260815, 20260816, 20260817"
    ),
    paste(
      "Study 2 condition seed blocks:",
      "20260905, 20260906, 20260907"
    ),
    paste(
      "Replication seed rule:",
      rng_record$replicate_seed_rule
    ),
    paste(
      "RNG kind:",
      paste(
        rng_record$rng_kind,
        collapse = " / "
      )
    ),
    "Shard size: 10",
    "Shards per condition: 200",
    "Disk guard: 2 GB minimum free space before a new shard",
    "Definitive Study 1/2 simulations run during this freeze: FALSE",
    "",
    paste(
      "This manifest is part of the prospective registration bundle.",
      "The protocol and freeze artifacts disclose that earlier calibration,",
      "developmental simulations, numerical audits, and pilots were observed",
      "before this definitive manuscript-version registration."
    )
  )

  study12f_write_lines_atomic(
    registration_manifest_lines,
    registration_manifest
  )

  study12f_write_lines_atomic(
    utils::capture.output(
      utils::sessionInfo()
    ),
    session_txt
  )

  frozen_artifacts <- c(
    study1_csv,
    study1_rds,
    study2_csv,
    study2_rds,
    methods_csv,
    seed_blocks_csv,
    seeds_csv,
    seeds_rds,
    shard_csv,
    shard_rds,
    git_csv,
    source_csv,
    packages_csv,
    rng_rds,
    checks_csv,
    metadata_csv,
    registration_manifest,
    protocol_copy,
    session_txt
  )

  artifact_checksums <-
    study12f_file_md5(
      frozen_artifacts,
      project_root = project_root
    )

  checksum_path <- file.path(
    freeze_dir,
    "study12_frozen_artifact_checksums.csv"
  )

  study12f_write_csv_atomic(
    artifact_checksums,
    checksum_path
  )

  freeze_record <- list(
    created_at = Sys.time(),
    freeze_metadata =
      freeze_metadata,
    study1_design =
      study1_design,
    study2_design =
      study2_design,
    method_schedule =
      method_schedule,
    seed_blocks =
      seed_blocks,
    replicate_seeds =
      replicate_seeds,
    shard_plan =
      shard_plan,
    git_record =
      git_record,
    source_checksums =
      source_checksums,
    package_versions =
      package_versions,
    rng_record =
      rng_record,
    scientific_checks =
      scientific_checks,
    artifact_checksums =
      artifact_checksums,
    session_info =
      utils::sessionInfo(),
    definitive_simulations_run =
      FALSE,
    registration_complete =
      FALSE
  )

  freeze_record_path <- file.path(
    freeze_dir,
    "study12_freeze_record.rds"
  )

  study12f_save_rds_atomic(
    freeze_record,
    freeze_record_path
  )

  freeze_record_md5 <- unname(
    tools::md5sum(
      freeze_record_path
    )
  )

  completion_lines <- c(
    "mmiCATs prospective Study 1/2 SOURCE FREEZE COMPLETE",
    "External pre-results registration: PENDING",
    "",
    paste(
      "Created:",
      format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S %Z"
      )
    ),
    paste(
      "Git commit:",
      git_record$commit
    ),
    paste(
      "Git upstream commit:",
      git_record$upstream_commit
    ),
    paste(
      "Protocol MD5:",
      protocol_md5
    ),
    paste(
      "Freeze record MD5:",
      freeze_record_md5
    ),
    paste(
      "Scientific checks:",
      sum(
        scientific_checks$passed
      ),
      "of",
      nrow(
        scientific_checks
      ),
      "passed"
    ),
    "Study 1 definitive results present before freeze: FALSE",
    "Study 2 definitive results present before freeze: FALSE",
    "Definitive Study 1/2 simulations run during freeze: FALSE",
    "",
    paste(
      "DO NOT run Study 1 or Study 2 yet.",
      "Archive/register this prospective bundle first, then record the",
      "registration with record_study12_registration()."
    )
  )

  study12f_write_lines_atomic(
    completion_lines,
    completion_marker
  )

  message("")
  message(
    "Prospective Study 1/2 source freeze created successfully."
  )
  message(
    paste(
      "Freeze directory:",
      freeze_dir
    )
  )
  message(
    paste(
      "Git commit:",
      git_record$commit
    )
  )
  message(
    paste(
      "Scientific checks:",
      sum(
        scientific_checks$passed
      ),
      "of",
      nrow(
        scientific_checks
      ),
      "passed."
    )
  )
  message(
    paste(
      "The definitive simulations remain BLOCKED until the external",
      "pre-results registration is recorded."
    )
  )

  invisible(
    freeze_record
  )
}


#' Record the Completed External Study 1/2 Registration
#'
#' Records the permanent location of the externally completed prospective
#' registration after `prepare_study12_freeze()` has created and verified the
#' source-freeze bundle. This function does not contact an external service and
#' must be called only after the user has completed the registration/archive.
#'
#' @param registration_location Permanent URL, DOI, or other stable identifier
#'   for the prospective registration containing the frozen protocol/bundle.
#' @param project_root Source checkout root. If `NULL`, located automatically.
#' @param freeze_dir Completed Study 1/2 source-freeze directory. If `NULL`, uses
#'   the project default.
#'
#' @return Invisibly, the saved registration record.
#' @export
record_study12_registration <- function(
    registration_location,
    project_root = NULL,
    freeze_dir = NULL) {
  registration_location <-
    study12f_validate_registration_location(
      registration_location
    )

  if (is.null(project_root)) {
    project_root <- study12f_find_project_root()
  }

  project_root <- study12f_normalize_root(
    project_root
  )

  if (is.null(freeze_dir)) {
    freeze_dir <- study12f_default_freeze_dir(
      project_root
    )
  }

  study12f_verify_freeze(
    project_root = project_root,
    freeze_dir = freeze_dir,
    verify_current_source = TRUE,
    verify_current_git = TRUE,
    verify_package_versions = TRUE,
    verify_rng = TRUE
  )

  registration_csv <- file.path(
    freeze_dir,
    "study12_registration_record.csv"
  )
  registration_marker <- file.path(
    freeze_dir,
    "REGISTRATION_COMPLETE.txt"
  )

  if (file.exists(registration_marker) ||
      file.exists(registration_csv)) {
    existing <- study12f_verify_registration(
      freeze_dir
    )

    if (!identical(
      existing$registration_location[1L],
      registration_location
    )) {
      stop(
        paste(
          "A different immutable Study 1/2 registration record already exists.",
          "Do not overwrite it."
        ),
        call. = FALSE
      )
    }

    message(
      "The existing Study 1/2 registration record verified successfully."
    )

    return(
      invisible(
        existing
      )
    )
  }

  freeze_record_path <- file.path(
    freeze_dir,
    "study12_freeze_record.rds"
  )

  freeze_record_md5 <- unname(
    tools::md5sum(
      freeze_record_path
    )
  )

  git_record <- utils::read.csv(
    file.path(
      freeze_dir,
      "study12_git_record.csv"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  registration_record <- data.frame(
    recorded_at = format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S %Z"
    ),
    registration_location =
      registration_location,
    frozen_git_commit =
      git_record$commit[1L],
    freeze_record_md5 =
      freeze_record_md5,
    registration_bundle =
      "study12 prospective source-freeze bundle",
    stringsAsFactors = FALSE
  )

  study12f_write_csv_atomic(
    registration_record,
    registration_csv
  )

  registration_record_md5 <-
    unname(
      tools::md5sum(
        registration_csv
      )
    )

  completion_lines <- c(
    "mmiCATs prospective Study 1/2 PRE-RESULTS REGISTRATION RECORDED",
    "",
    paste(
      "Recorded:",
      registration_record$recorded_at[1L]
    ),
    paste(
      "Registration location:",
      registration_location
    ),
    paste(
      "Frozen Git commit:",
      git_record$commit[1L]
    ),
    paste(
      "Freeze record MD5:",
      freeze_record_md5
    ),
    paste(
      "Registration record MD5:",
      registration_record_md5
    ),
    "",
    "The prospective Study 1/2 freeze/registration gate is complete.",
    "Definitive Study 1 may now be launched from the exact frozen source state.",
    "Study 2 remains sequenced after Study 1 is secured and reviewed."
  )

  study12f_write_lines_atomic(
    completion_lines,
    registration_marker
  )

  # Final full-gate verification.
  study12f_verify_gate(
    project_root = project_root,
    freeze_dir = freeze_dir,
    verify_current_source = TRUE,
    verify_current_git = TRUE,
    verify_package_versions = TRUE,
    verify_rng = TRUE
  )

  message("")
  message(
    "Prospective Study 1/2 registration record created and verified."
  )
  message(
    paste(
      "Registration location:",
      registration_location
    )
  )
  message(
    "The freeze/registration gate is now complete."
  )

  invisible(
    registration_record
  )
}
