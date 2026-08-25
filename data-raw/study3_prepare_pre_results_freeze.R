# Study 3 empirical illustration: Phase 6B pre-results freeze
#
# PURPOSE
# -------
# Create and archive the approved Study 3 inputs BEFORE any comparative
# Study 3 method results are generated.
#
# This script:
#   * validates the canonical lme4::sleepstudy structure;
#   * sorts and archives the canonical data;
#   * creates the fixed contamination map from seed 20261105L;
#   * computes the prespecified within-Subject OLS reference residual SD;
#   * creates and archives the perturbed data;
#   * saves checksums, package versions, RNG information, and sessionInfo();
#   * does NOT call cluster_model_diagnostics() or fit any of the nine
#     comparative Study 3 methods.
#
# Rerunning against an already-completed freeze verifies the immutable
# artifacts rather than replacing them.

library(devtools)

load_all()

study3_find_project_root <- function(path = getwd()) {
  path <- normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )

  repeat {
    if (file.exists(
      file.path(
        path,
        "DESCRIPTION"
      )
    )) {
      return(path)
    }

    parent <- dirname(path)

    if (identical(
      parent,
      path
    )) {
      stop(
        "Could not locate the mmiCATs project root.",
        call. = FALSE
      )
    }

    path <- parent
  }
}


study3_write_csv_atomic <- function(data,
                                    path) {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  temp_path <- tempfile(
    pattern = paste0(
      basename(path),
      "_"
    ),
    tmpdir = dirname(path),
    fileext = ".tmp"
  )

  on.exit(
    if (file.exists(temp_path)) {
      unlink(
        temp_path,
        force = TRUE
      )
    },
    add = TRUE
  )

  utils::write.csv(
    data,
    temp_path,
    row.names = FALSE,
    na = ""
  )

  if (file.exists(path)) {
    stop(
      paste(
        "Refusing to overwrite frozen Study 3 artifact:",
        path
      ),
      call. = FALSE
    )
  }

  if (!file.rename(
    temp_path,
    path
  )) {
    stop(
      paste(
        "Could not atomically save:",
        path
      ),
      call. = FALSE
    )
  }

  invisible(path)
}


study3_save_rds_atomic <- function(object,
                                   path) {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  temp_path <- tempfile(
    pattern = paste0(
      basename(path),
      "_"
    ),
    tmpdir = dirname(path),
    fileext = ".tmp"
  )

  on.exit(
    if (file.exists(temp_path)) {
      unlink(
        temp_path,
        force = TRUE
      )
    },
    add = TRUE
  )

  saveRDS(
    object,
    temp_path,
    version = 3,
    compress = "gzip"
  )

  # Verify readability before final rename.
  readRDS(temp_path)

  if (file.exists(path)) {
    stop(
      paste(
        "Refusing to overwrite frozen Study 3 artifact:",
        path
      ),
      call. = FALSE
    )
  }

  if (!file.rename(
    temp_path,
    path
  )) {
    stop(
      paste(
        "Could not atomically save:",
        path
      ),
      call. = FALSE
    )
  }

  invisible(path)
}


study3_file_md5 <- function(paths,
                            project_root) {
  normalized_paths <- normalizePath(
    paths,
    winslash = "/",
    mustWork = TRUE
  )

  normalized_root <- normalizePath(
    project_root,
    winslash = "/",
    mustWork = TRUE
  )

  prefix <- paste0(
    normalized_root,
    "/"
  )

  if (!all(
    startsWith(
      normalized_paths,
      prefix
    )
  )) {
    stop(
      "All frozen artifacts must be inside the project root.",
      call. = FALSE
    )
  }

  data.frame(
    file = basename(
      normalized_paths
    ),
    relative_path = substring(
      normalized_paths,
      nchar(prefix) + 1L
    ),
    md5 = unname(
      tools::md5sum(
        normalized_paths
      )
    ),
    stringsAsFactors = FALSE
  )
}


study3_structural_checks <- function(dat) {
  subject_character <- as.character(
    dat$Subject
  )

  subject_counts <- table(
    subject_character
  )

  days_by_subject <- split(
    dat$Days,
    subject_character
  )

  data.frame(
    check = c(
      "rows_equal_180",
      "subjects_equal_18",
      "no_missing_analysis_values",
      "each_subject_has_10_rows",
      "each_subject_has_days_0_to_9",
      "reaction_is_numeric",
      "days_is_numeric"
    ),
    passed = c(
      nrow(dat) == 180L,
      length(
        unique(
          subject_character
        )
      ) == 18L,
      !anyNA(
        dat[
          ,
          c(
            "Reaction",
            "Days",
            "Subject"
          )
        ]
      ),
      length(subject_counts) == 18L &&
        all(
          subject_counts == 10L
        ),
      length(days_by_subject) == 18L &&
        all(
          vapply(
            days_by_subject,
            function(x) {
              identical(
                sort(
                  as.numeric(x)
                ),
                as.numeric(0:9)
              )
            },
            logical(1)
          )
        ),
      is.numeric(
        dat$Reaction
      ),
      is.numeric(
        dat$Days
      )
    ),
    stringsAsFactors = FALSE
  )
}


project_root <- study3_find_project_root()

plan_path <- file.path(
  project_root,
  "data-raw",
  "study3_empirical_analysis_plan_approved_20260825.txt"
)

if (!file.exists(plan_path)) {
  stop(
    paste(
      "Approved Study 3 plan not found:",
      plan_path
    ),
    call. = FALSE
  )
}

freeze_dir <- file.path(
  project_root,
  "data-raw",
  "study3-results",
  "pre-results-freeze"
)

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

  invisible(
    readRDS(
      freeze_record_path
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
    preparation_script = file.path(
      project_root,
      "data-raw",
      "study3_prepare_pre_results_freeze.R"
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
    capture.output(
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
}
