# Phase 6D-B1: package infrastructure migration validation
#
# This validator compares the new package-owned helpers against the still-
# preserved legacy data-raw helper files. It does not generate Study 3 results
# and does not run definitive Study 1/2 simulations.

library(devtools)
library(testthat)

load_all()

project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

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


# Compare executable function bodies without source-reference metadata.
#
# body() objects sourced from different files can carry different srcref
# attributes even when the executable R expressions are identical. deparse()
# removes those irrelevant file/line attributes while preserving the code.
phase6d_body_text <- function(fn) {
  paste(
    deparse(
      body(fn),
      width.cutoff = 500L
    ),
    collapse = "\n"
  )
}

# -------------------------------------------------------------------------
# Compare deterministic sharding helper bodies to the validated legacy file.
# -------------------------------------------------------------------------

legacy_sharding <- new.env(
  parent = globalenv()
)

sys.source(
  file.path(
    project_root,
    "data-raw",
    "definitive_sharding_helpers.R"
  ),
  envir = legacy_sharding
)

sharding_functions <- c(
  "definitive_save_rds_atomic",
  "definitive_write_csv_atomic",
  "definitive_make_replicate_seeds",
  "definitive_make_shard_plan",
  "definitive_shard_checkpoint_path",
  "definitive_read_checkpoint",
  "definitive_checkpoint_spec_matches",
  "definitive_validate_complete_checkpoint",
  "definitive_get_free_gb",
  "definitive_disk_guard",
  "definitive_offset_shard_replicates",
  "definitive_run_study1_shard",
  "definitive_run_study2_shard",
  "definitive_run_shard_checkpoint",
  "definitive_collect_condition_shards"
)

for (fn in sharding_functions) {
  legacy_fn <- get(
    fn,
    envir = legacy_sharding,
    inherits = FALSE
  )

  package_fn <- getFromNamespace(
    fn,
    "mmiCATs"
  )

  add_check(
    paste0(
      "sharding_body_matches_legacy__",
      fn
    ),
    identical(
      phase6d_body_text(package_fn),
      phase6d_body_text(legacy_fn)
    )
  )
}

# -------------------------------------------------------------------------
# Compare unchanged Study 3 Phase 6C helper bodies where no amendment-aware
# change was intentionally required.
# -------------------------------------------------------------------------

legacy_study3 <- new.env(
  parent = globalenv()
)

sys.source(
  file.path(
    project_root,
    "data-raw",
    "study3_analysis_helpers.R"
  ),
  envir = legacy_study3
)

unchanged_study3_helpers <- c(
  "study3c_methods",
  "study3c_analysis_seed",
  "study3c_find_project_root",
  "study3c_verify_checksum_record",
  "study3c_prepare_analysis_data",
  "study3c_fit_full",
  "study3c_fit_loo_subject",
  "study3c_loo_path",
  "study3c_checkpoint_matches",
  "study3c_run_loo_checkpoint",
  "study3c_collect_loo",
  "study3c_make_comparison",
  "study3c_cross_dataset_plot",
  "study3c_loo_plot",
  "study3c_save_plot",
  "study3c_output_checksums"
)

for (fn in unchanged_study3_helpers) {
  legacy_fn <- get(
    fn,
    envir = legacy_study3,
    inherits = FALSE
  )

  package_fn <- getFromNamespace(
    fn,
    "mmiCATs"
  )

  add_check(
    paste0(
      "study3_body_matches_legacy__",
      fn
    ),
    identical(
      phase6d_body_text(package_fn),
      phase6d_body_text(legacy_fn)
    )
  )
}

# study3c_verify_freeze intentionally changed only to:
# - accept an explicit freeze_dir; and
# - make current-source checksum equality optional after a documented amendment.
verify_text <- paste(
  deparse(
    body(
      getFromNamespace(
        "study3c_verify_freeze",
        "mmiCATs"
      )
    )
  ),
  collapse = "\n"
)

add_check(
  "freeze_verifier_still_checks_frozen_artifact_checksums",
  grepl(
    "study3_frozen_artifact_checksums.csv",
    verify_text,
    fixed = TRUE
  )
)

add_check(
  "freeze_verifier_original_source_check_is_explicitly_optional",
  grepl(
    "verify_original_sources",
    verify_text,
    fixed = TRUE
  ) &&
    grepl(
      "study3_source_checksums.csv",
      verify_text,
      fixed = TRUE
    )
)

# Public API / no-data-raw-source requirement.
add_check(
  "prepare_study3_empirical_is_exported",
  "prepare_study3_empirical" %in%
    getNamespaceExports(
      "mmiCATs"
    )
)

add_check(
  "run_study3_empirical_is_exported",
  "run_study3_empirical" %in%
    getNamespaceExports(
      "mmiCATs"
    )
)

runner_text <- paste(
  deparse(
    body(
      run_study3_empirical
    )
  ),
  collapse = "\n"
)

add_check(
  "package_study3_runner_has_no_source_call",
  !grepl(
    "source(",
    runner_text,
    fixed = TRUE
  )
)

add_check(
  "package_study3_runner_reads_frozen_observed_data",
  grepl(
    "sleepstudy_canonical.rds",
    runner_text,
    fixed = TRUE
  )
)

add_check(
  "package_study3_runner_reads_frozen_perturbed_data",
  grepl(
    "sleepstudy_perturbed.rds",
    runner_text,
    fixed = TRUE
  )
)

add_check(
  "package_study3_runner_cannot_resample_contamination",
  !grepl(
    "20261105L",
    runner_text,
    fixed = TRUE
  ) &&
    !grepl(
      "sample(",
      runner_text,
      fixed = TRUE
    )
)

# Run the package tests that directly cover the migrated infrastructure.
message(
  "Phase 6D-B1: running migrated sharding helper tests..."
)

test_file(
  file.path(
    project_root,
    "tests",
    "testthat",
    "test-definitive-sharding-runner.R"
  ),
  stop_on_failure = TRUE
)

message(
  "Phase 6D-B1: running package Study 3 architecture tests..."
)

test_file(
  file.path(
    project_root,
    "tests",
    "testthat",
    "test-study3-empirical.R"
  ),
  stop_on_failure = TRUE
)

checks_df <- do.call(
  rbind,
  checks
)

rownames(checks_df) <- NULL

output_dir <- file.path(
  project_root,
  "data-raw",
  "definitive-runner-results",
  "phase6d-b1-package-infrastructure"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

utils::write.csv(
  checks_df,
  file.path(
    output_dir,
    "phase6d_b1_checks.csv"
  ),
  row.names = FALSE,
  na = ""
)

writeLines(
  capture.output(
    utils::sessionInfo()
  ),
  file.path(
    output_dir,
    "session_info.txt"
  ),
  useBytes = TRUE
)

writeLines(
  c(
    "mmiCATs Phase 6D-B1",
    "Package infrastructure migration validation",
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
      "Exact sharding-helper bodies checked:",
      length(
        sharding_functions
      )
    ),
    paste(
      "Exact unchanged Study 3 helper bodies checked:",
      length(
        unchanged_study3_helpers
      )
    ),
    "Study 3 comparative results generated: FALSE",
    "Definitive Study 1/2 simulations launched: FALSE"
  ),
  file.path(
    output_dir,
    "phase6d_b1_summary.txt"
  ),
  useBytes = TRUE
)

message("")
message(
  "Phase 6D-B1 package-infrastructure checks:"
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
      "Phase 6D-B1 package-infrastructure check(s) failed."
    ),
    call. = FALSE
  )
}

message("")
message(
  paste(
    "All Phase 6D-B1 package-infrastructure checks passed.",
    "The legacy data-raw helper files can remain as the comparison",
    "baseline while Phase 6D-B2 migrates the definitive Study 1/2",
    "orchestration."
  )
)
