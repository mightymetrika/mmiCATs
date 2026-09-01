# Phase 6D-B2A: Study 1/2 package-runner migration validation
#
# This validator does NOT launch the 2,000-rep definitive simulations.
# The legacy data-raw Study 1/2 runners remain untouched for comparison.

library(devtools)
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
      passed = isTRUE(passed),
      details = as.character(details),
      stringsAsFactors = FALSE
    )
}

canonical_body <- function(fn,
                           prefix = NULL) {
  x <- paste(
    deparse(
      body(fn),
      width.cutoff = 500L
    ),
    collapse = "\n"
  )

  if (!is.null(prefix)) {
    x <- gsub(
      prefix,
      "",
      x,
      fixed = TRUE
    )
  }

  # Package code explicitly qualifies setNames for CRAN hygiene. The legacy
  # data-raw script did not need that qualification.
  x <- gsub(
    "stats::setNames",
    "setNames",
    x,
    fixed = TRUE
  )

  # Reparse after canonicalizing names. This removes irrelevant differences
  # in deparse line wrapping caused by the longer migrated helper prefixes,
  # while preserving the actual R expression, constants, strings, and calls.
  parse(
    text = x,
    keep.source = FALSE
  )[[1L]]
}

load_legacy_helpers <- function(path) {
  lines <- readLines(
    path,
    warn = FALSE
  )

  stop_index <- grep(
    "^project_root <- find_project_root[(][)]",
    lines
  )

  if (length(stop_index) != 1L) {
    stop(
      paste(
        "Could not isolate legacy helper definitions in",
        path
      ),
      call. = FALSE
    )
  }

  text <- paste(
    lines[
      seq_len(stop_index - 1L)
    ],
    collapse = "\n"
  )

  env <- new.env(
    parent = globalenv()
  )

  eval(
    parse(
      text = text
    ),
    envir = env
  )

  env
}

message(
  "Phase 6D-B2A: running package definitive-runner tests..."
)

testthat::test_file(
  file.path(
    project_root,
    "tests",
    "testthat",
    "test-definitive-study-runners.R"
  ),
  reporter = "progress",
  stop_on_failure = TRUE,
  stop_on_warning = FALSE
)

message(
  "Phase 6D-B2A: rerunning deterministic sharding tests..."
)

testthat::test_file(
  file.path(
    project_root,
    "tests",
    "testthat",
    "test-definitive-sharding-runner.R"
  ),
  reporter = "progress",
  stop_on_failure = TRUE,
  stop_on_warning = FALSE
)

s1_legacy_path <- file.path(
  project_root,
  "data-raw",
  "study1_final_simulation.R"
)

s2_legacy_path <- file.path(
  project_root,
  "data-raw",
  "study2_final_simulation.R"
)

add_check(
  "legacy_study1_runner_still_present_for_b2a_comparison",
  file.exists(s1_legacy_path)
)

add_check(
  "legacy_study2_runner_still_present_for_b2a_comparison",
  file.exists(s2_legacy_path)
)

legacy_s1 <- load_legacy_helpers(
  s1_legacy_path
)

legacy_s2 <- load_legacy_helpers(
  s2_legacy_path
)

s1_helpers <- c(
  "find_project_root",
  "mean_or_na",
  "max_or_na",
  "min_or_na",
  "sum_or_zero",
  "has_text",
  "column_or_default",
  "select_existing",
  "add_condition_columns",
  "rbind_fill",
  "method_labels",
  "add_method_labels",
  "extract_flagged_cluster_diagnostics",
  "prepare_replicates_for_storage",
  "make_status_snapshot",
  "summarize_diagnostics",
  "count_text_values",
  "make_message_frequency",
  "make_negative_control_comparison",
  "make_robust_vs_cats",
  "make_primary_performance_table",
  "make_mcse_summary"
)

s2_helpers <- c(
  "find_project_root",
  "mean_or_na",
  "min_or_na",
  "max_or_na",
  "quantile_or_na",
  "has_text",
  "column_or_default",
  "select_existing",
  "add_condition_columns",
  "rbind_fill",
  "method_labels",
  "add_method_labels",
  "extract_flagged_cluster_diagnostics",
  "prepare_replicates_for_storage",
  "make_status_snapshot",
  "summarize_diagnostics",
  "count_text_values",
  "make_message_frequency",
  "make_negative_control_comparison",
  "make_method_vs_reference",
  "summarize_rs_subset",
  "make_rs_singularity_sensitivity",
  "make_dgp_diagnostics",
  "make_crn_audit",
  "make_primary_performance_table",
  "make_mcse_summary"
)

for (name in s1_helpers) {
  package_name <- paste0(
    "study1d_",
    name
  )

  package_fn <- get(
    package_name,
    envir = asNamespace(
      "mmiCATs"
    ),
    inherits = FALSE
  )

  legacy_fn <- get(
    name,
    envir = legacy_s1,
    inherits = FALSE
  )

  add_check(
    paste0(
      "study1_helper_matches_legacy__",
      name
    ),
    identical(
      canonical_body(
        package_fn,
        prefix = "study1d_"
      ),
      canonical_body(
        legacy_fn
      )
    )
  )
}

for (name in s2_helpers) {
  package_name <- paste0(
    "study2d_",
    name
  )

  package_fn <- get(
    package_name,
    envir = asNamespace(
      "mmiCATs"
    ),
    inherits = FALSE
  )

  legacy_fn <- get(
    name,
    envir = legacy_s2,
    inherits = FALSE
  )

  add_check(
    paste0(
      "study2_helper_matches_legacy__",
      name
    ),
    identical(
      canonical_body(
        package_fn,
        prefix = "study2d_"
      ),
      canonical_body(
        legacy_fn
      )
    )
  )
}

s1_design <- mmiCATs:::study1d_frozen_design()
s2_design <- mmiCATs:::study2d_frozen_design()

add_check(
  "study1_frozen_condition_count_18",
  nrow(s1_design) == 18L
)

add_check(
  "study2_frozen_condition_count_24",
  nrow(s2_design) == 24L
)

add_check(
  "study1_frozen_reps_2000",
  all(
    s1_design$reps == 2000L
  )
)

add_check(
  "study2_frozen_reps_2000",
  all(
    s2_design$reps == 2000L
  )
)

add_check(
  "study1_frozen_shard_size_10",
  all(
    s1_design$shard_size == 10L
  )
)

add_check(
  "study2_frozen_shard_size_10",
  all(
    s2_design$shard_size == 10L
  )
)

add_check(
  "study1_frozen_seed_blocks_exact",
  identical(
    as.integer(
      tapply(
        s1_design$condition_seed,
        s1_design$n_clusters,
        unique
      )
    ),
    c(
      20260815L,
      20260816L,
      20260817L
    )
  )
)

add_check(
  "study2_frozen_seed_blocks_exact",
  identical(
    as.integer(
      tapply(
        s2_design$condition_seed,
        s2_design$n_clusters,
        unique
      )
    ),
    c(
      20260905L,
      20260906L,
      20260907L
    )
  )
)

exports <- getNamespaceExports(
  "mmiCATs"
)

add_check(
  "run_study1_definitive_exported",
  "run_study1_definitive" %in%
    exports
)

add_check(
  "run_study2_definitive_exported",
  "run_study2_definitive" %in%
    exports
)

s1_runner_text <- paste(
  deparse(
    body(
      mmiCATs::run_study1_definitive
    ),
    width.cutoff = 500L
  ),
  collapse = "\n"
)

s2_runner_text <- paste(
  deparse(
    body(
      mmiCATs::run_study2_definitive
    ),
    width.cutoff = 500L
  ),
  collapse = "\n"
)

add_check(
  "study1_package_runner_has_no_source_call",
  !grepl(
    "source(",
    s1_runner_text,
    fixed = TRUE
  )
)

add_check(
  "study2_package_runner_has_no_source_call",
  !grepl(
    "source(",
    s2_runner_text,
    fixed = TRUE
  )
)

add_check(
  "study1_package_runner_uses_package_sharding",
  grepl(
    "definitive_run_shard_checkpoint",
    s1_runner_text,
    fixed = TRUE
  )
)

add_check(
  "study2_package_runner_uses_package_sharding",
  grepl(
    "definitive_run_shard_checkpoint",
    s2_runner_text,
    fixed = TRUE
  )
)

checks_df <- do.call(
  rbind,
  checks
)

rownames(checks_df) <- NULL

message("")
message(
  "Phase 6D-B2A Study 1/2 package-runner checks:"
)

print(
  checks_df,
  row.names = FALSE
)

if (!all(checks_df$passed)) {
  stop(
    paste(
      sum(!checks_df$passed),
      "Phase 6D-B2A check(s) failed."
    ),
    call. = FALSE
  )
}

message("")
message(
  paste(
    "All Phase 6D-B2A checks passed.",
    "The long legacy Study 1/2 data-raw runners may then be replaced",
    "with thin package-function wrappers in Phase 6D-B2B."
  )
)
