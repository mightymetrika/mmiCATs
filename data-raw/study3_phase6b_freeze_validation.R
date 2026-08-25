# Study 3 Phase 6B independent freeze validation
#
# Reads the completed pre-results freeze, reconstructs the fixed contamination
# selection independently from the approved seed/rule, and verifies data,
# perturbation, checksums, package-method schedule, and the absence of
# comparative Study 3 result files.
#
# This validator does NOT fit comparative Study 3 models.

library(devtools)

load_all()

project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

freeze_dir <- file.path(
  project_root,
  "data-raw",
  "study3-results",
  "pre-results-freeze"
)

required_files <- c(
  "FREEZE_COMPLETE.txt",
  "sleepstudy_canonical.csv",
  "sleepstudy_canonical.rds",
  "study3_contamination_map.csv",
  "study3_contamination_map.rds",
  "sleepstudy_perturbed.csv",
  "sleepstudy_perturbed.rds",
  "study3_structure_checks.csv",
  "study3_freeze_metadata.csv",
  "study3_source_checksums.csv",
  "study3_package_versions.csv",
  "study3_rng_record.rds",
  "study3_frozen_artifact_checksums.csv",
  "study3_freeze_record.rds",
  "session_info.txt"
)

required_paths <- file.path(
  freeze_dir,
  required_files
)

if (!all(
  file.exists(
    required_paths
  )
)) {
  stop(
    paste(
      "Missing Phase 6B freeze artifact(s):",
      paste(
        required_files[
          !file.exists(
            required_paths
          )
        ],
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

checks <- list()

add_check <- function(check,
                      passed,
                      details = NA_character_) {
  checks[[length(checks) + 1L]] <<-
    data.frame(
      check = check,
      passed = as.logical(
        passed
      ),
      details = details,
      stringsAsFactors = FALSE
    )
}

canonical <- readRDS(
  file.path(
    freeze_dir,
    "sleepstudy_canonical.rds"
  )
)

perturbed <- readRDS(
  file.path(
    freeze_dir,
    "sleepstudy_perturbed.rds"
  )
)

map <- readRDS(
  file.path(
    freeze_dir,
    "study3_contamination_map.rds"
  )
)

metadata <- utils::read.csv(
  file.path(
    freeze_dir,
    "study3_freeze_metadata.csv"
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

structure_checks <- utils::read.csv(
  file.path(
    freeze_dir,
    "study3_structure_checks.csv"
  ),
  stringsAsFactors = FALSE
)

add_check(
  "all_saved_structure_checks_passed",
  all(
    structure_checks$passed
  )
)

add_check(
  "canonical_has_180_rows",
  nrow(canonical) == 180L
)

add_check(
  "canonical_has_18_subjects",
  length(
    unique(
      as.character(
        canonical$Subject
      )
    )
  ) == 18L
)

canonical_reference <-
  lme4::sleepstudy[
    ,
    c(
      "Reaction",
      "Days",
      "Subject"
    )
  ]

canonical_reference <-
  canonical_reference[
    order(
      as.character(
        canonical_reference$Subject
      ),
      canonical_reference$Days
    ),
    ,
    drop = FALSE
  ]

canonical_reference$Subject <- factor(
  as.character(
    canonical_reference$Subject
  ),
  levels = sort(
    unique(
      as.character(
        canonical_reference$Subject
      )
    )
  )
)

rownames(canonical_reference) <- NULL

add_check(
  "canonical_rds_matches_lme4_sleepstudy",
  identical(
    canonical,
    canonical_reference
  )
)

# Reconstruct the contamination map independently.
selection_seed <- 20261105L

set.seed(selection_seed)

indices_by_subject <- split(
  seq_len(
    nrow(canonical_reference)
  ),
  canonical_reference$Subject
)

expected_index <- unlist(
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

expected_sign <- sample(
  c(-1L, 1L),
  size = length(
    expected_index
  ),
  replace = TRUE
)

expected_map <- data.frame(
  canonical_row =
    as.integer(
      expected_index
    ),
  Subject =
    as.character(
      canonical_reference$Subject[
        expected_index
      ]
    ),
  Days =
    canonical_reference$Days[
      expected_index
    ],
  sign =
    as.integer(
      expected_sign
    ),
  stringsAsFactors = FALSE
)

expected_map <- expected_map[
  match(
    levels(
      canonical_reference$Subject
    ),
    expected_map$Subject
  ),
  ,
  drop = FALSE
]
rownames(expected_map) <- NULL

add_check(
  "contamination_map_matches_independent_reconstruction",
  identical(
    map,
    expected_map
  )
)

add_check(
  "one_contaminated_observation_per_subject",
  nrow(map) == 18L &&
    all(
      table(
        map$Subject
      ) == 1L
    )
)

add_check(
  "realized_contamination_fraction_is_10_percent",
  isTRUE(
    all.equal(
      mean(
        perturbed$contaminated
      ),
      0.10,
      tolerance = 1e-15
    )
  )
)

reference_fit <- stats::lm(
  Reaction ~
    0 +
    Subject +
    Subject:Days,
  data = canonical_reference
)

expected_sd <- stats::sigma(
  reference_fit
)

expected_displacement <-
  6 * expected_sd

metadata_lookup <- setNames(
  metadata$value,
  metadata$field
)

add_check(
  "reference_residual_sd_matches_independent_recalculation",
  isTRUE(
    all.equal(
      as.numeric(
        metadata_lookup[
          "reference_residual_sd"
        ]
      ),
      expected_sd,
      tolerance = 1e-12
    )
  ),
  paste(
    "Observed:",
    metadata_lookup[
      "reference_residual_sd"
    ]
  )
)

add_check(
  "absolute_displacement_matches_6_reference_sd",
  isTRUE(
    all.equal(
      as.numeric(
        metadata_lookup[
          "absolute_displacement"
        ]
      ),
      expected_displacement,
      tolerance = 1e-12
    )
  )
)

expected_perturbed <-
  canonical_reference

expected_perturbed$Reaction_observed <-
  canonical_reference$Reaction

expected_perturbed$contaminated <-
  FALSE

expected_perturbed$contamination_sign <-
  0L

expected_perturbed$signed_displacement <-
  0

expected_perturbed$absolute_displacement <-
  0

expected_perturbed$contaminated[
  expected_index
] <- TRUE

expected_perturbed$contamination_sign[
  expected_index
] <- expected_sign

expected_perturbed$signed_displacement[
  expected_index
] <-
  expected_sign *
  expected_displacement

expected_perturbed$absolute_displacement[
  expected_index
] <-
  expected_displacement

expected_perturbed$Reaction[
  expected_index
] <-
  canonical_reference$Reaction[
    expected_index
  ] +
  expected_sign *
    expected_displacement

add_check(
  "perturbed_rds_matches_independent_reconstruction",
  isTRUE(
    all.equal(
      perturbed,
      expected_perturbed,
      tolerance = 1e-12,
      check.attributes = TRUE
    )
  )
)

add_check(
  "days_and_subject_are_unchanged_by_perturbation",
  identical(
    perturbed$Days,
    canonical$Days
  ) &&
    identical(
      perturbed$Subject,
      canonical$Subject
    )
)

add_check(
  "only_selected_reaction_values_changed",
  identical(
    which(
      perturbed$Reaction !=
        canonical$Reaction
    ),
    sort(
      expected_index
    )
  )
)

# Validate immutable artifact checksums.
recorded_checksums <- utils::read.csv(
  file.path(
    freeze_dir,
    "study3_frozen_artifact_checksums.csv"
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

recorded_artifact_paths <- file.path(
  project_root,
  recorded_checksums$relative_path
)

current_md5 <- unname(
  tools::md5sum(
    recorded_artifact_paths
  )
)

add_check(
  "all_frozen_artifact_checksums_match",
  identical(
    current_md5,
    recorded_checksums$md5
  )
)

completion_lines <- readLines(
  file.path(
    freeze_dir,
    "FREEZE_COMPLETE.txt"
  ),
  warn = FALSE
)

freeze_record_line <- grep(
  "^Freeze record MD5:",
  completion_lines,
  value = TRUE
)

recorded_freeze_record_md5 <- if (
  length(freeze_record_line) == 1L
) {
  trimws(
    sub(
      "^Freeze record MD5:",
      "",
      freeze_record_line
    )
  )
} else {
  NA_character_
}

current_freeze_record_md5 <- unname(
  tools::md5sum(
    file.path(
      freeze_dir,
      "study3_freeze_record.rds"
    )
  )
)

add_check(
  "freeze_record_checksum_matches_completion_marker",
  !is.na(
    recorded_freeze_record_md5
  ) &&
    identical(
      current_freeze_record_md5,
      recorded_freeze_record_md5
    )
)

# Validate source checksums.
source_checksums <- utils::read.csv(
  file.path(
    freeze_dir,
    "study3_source_checksums.csv"
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

recorded_source_paths <- file.path(
  project_root,
  source_checksums$relative_path
)

source_current_md5 <- unname(
  tools::md5sum(
    recorded_source_paths
  )
)

add_check(
  "all_recorded_source_checksums_still_match",
  identical(
    source_current_md5,
    source_checksums$md5
  )
)

# Method schedule must still be the exact 9-method schedule.
expected_methods <- c(
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

add_check(
  "canonical_nine_method_schedule_is_unchanged",
  identical(
    mmiCATs:::study2_method_names(),
    expected_methods
  )
)

# Approved plan must retain the frozen model forms and seed.
plan_path <- file.path(
  project_root,
  "data-raw",
  "study3_empirical_analysis_plan_approved_20260825.txt"
)

plan_text <- paste(
  readLines(
    plan_path,
    warn = FALSE
  ),
  collapse = "\n"
)

add_check(
  "approved_plan_contains_independent_random_slope_model",
  grepl(
    "Reaction ~ Days + (1 + Days || Subject)",
    plan_text,
    fixed = TRUE
  )
)

add_check(
  "approved_plan_contains_random_intercept_model",
  grepl(
    "Reaction ~ Days + (1 | Subject)",
    plan_text,
    fixed = TRUE
  )
)

add_check(
  "approved_plan_contains_selection_seed",
  grepl(
    "20261105L",
    plan_text,
    fixed = TRUE
  )
)

add_check(
  "freeze_metadata_records_no_comparative_models",
  identical(
    metadata_lookup[[
      "comparative_models_fit_during_freeze"
    ]],
    "FALSE"
  )
)

# Study 3 comparative-results directory must not yet exist with result tables.
comparative_dir <- file.path(
  project_root,
  "data-raw",
  "study3-results",
  "definitive-study3"
)

comparative_files <- if (
  dir.exists(
    comparative_dir
  )
) {
  list.files(
    comparative_dir,
    recursive = TRUE,
    full.names = TRUE
  )
} else {
  character(0)
}

add_check(
  "no_comparative_study3_results_exist_yet",
  length(
    comparative_files
  ) == 0L,
  if (
    length(
      comparative_files
    ) == 0L
  ) {
    NA_character_
  } else {
    paste(
      basename(
        comparative_files
      ),
      collapse = ", "
    )
  }
)

checks_df <- do.call(
  rbind,
  checks
)

rownames(checks_df) <- NULL

validation_dir <- file.path(
  project_root,
  "data-raw",
  "study3-results",
  "phase6b-freeze-validation"
)

dir.create(
  validation_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

utils::write.csv(
  checks_df,
  file.path(
    validation_dir,
    "phase6b_checks.csv"
  ),
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  expected_map,
  file.path(
    validation_dir,
    "phase6b_independently_reconstructed_map.csv"
  ),
  row.names = FALSE,
  na = ""
)

writeLines(
  capture.output(
    utils::sessionInfo()
  ),
  file.path(
    validation_dir,
    "session_info.txt"
  ),
  useBytes = TRUE
)

summary_lines <- c(
  "mmiCATs Study 3 Phase 6B",
  "Pre-results freeze validation",
  "",
  paste(
    "Checks passed:",
    sum(
      checks_df$passed
    ),
    "of",
    nrow(checks_df)
  ),
  paste(
    "Frozen subjects:",
    length(
      unique(
        as.character(
          canonical$Subject
        )
      )
    )
  ),
  paste(
    "Frozen observations:",
    nrow(canonical)
  ),
  paste(
    "Contaminated observations:",
    nrow(map)
  ),
  paste(
    "Nominal within-subject contamination rule:",
    "0.05"
  ),
  paste(
    "Realized overall contamination:",
    format(
      mean(
        perturbed$contaminated
      ),
      digits = 6
    )
  ),
  paste(
    "Reference residual SD:",
    format(
      expected_sd,
      digits = 10
    )
  ),
  paste(
    "Absolute displacement:",
    format(
      expected_displacement,
      digits = 10
    )
  ),
  "Comparative Study 3 results inspected: FALSE"
)

writeLines(
  summary_lines,
  file.path(
    validation_dir,
    "phase6b_summary.txt"
  ),
  useBytes = TRUE
)

message("")
message(
  "Phase 6B checks:"
)

print(
  checks_df,
  row.names = FALSE
)

if (!all(
  checks_df$passed
)) {
  stop(
    paste(
      sum(
        !checks_df$passed
      ),
      "Phase 6B Study 3 freeze validation check(s) failed.",
      "Do not run comparative Study 3 models."
    ),
    call. = FALSE
  )
}

message("")
message(
  paste(
    "All Phase 6B Study 3 pre-results freeze checks passed.",
    "The frozen empirical inputs are ready for Phase 6C comparative analysis."
  )
)
