# Study 3 Phase 6C runner validation
#
# Does NOT fit comparative methods to sleepstudy. Synthetic data are used to
# validate the resumable one-Subject-at-a-time LOO implementation.

library(devtools)
library(testthat)

load_all()

source("data-raw/definitive_sharding_helpers.R")
source("data-raw/study3_analysis_helpers.R")

project_root <- study3c_find_project_root()
study3c_verify_freeze(project_root)

checks <- list()

add_check <- function(check, passed, details = NA_character_) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check,
    passed = as.logical(passed),
    details = details,
    stringsAsFactors = FALSE
  )
}

add_check("phase6b_freeze_preflight_passes", TRUE)

add_check(
  "analysis_seed_equals_frozen_diagnostic_default",
  identical(study3c_analysis_seed(), 20261101L)
)

add_check(
  "phase6c_uses_exact_nine_method_schedule",
  identical(study3c_methods(), mmiCATs:::study2_method_names())
)

runner_path <- file.path(project_root, "data-raw", "study3_definitive_analysis.R")
runner_text <- paste(readLines(runner_path, warn = FALSE), collapse = "\n")

add_check(
  "runner_reads_phase6b_canonical_rds",
  grepl("sleepstudy_canonical.rds", runner_text, fixed = TRUE)
)

add_check(
  "runner_reads_phase6b_perturbed_rds",
  grepl("sleepstudy_perturbed.rds", runner_text, fixed = TRUE)
)

add_check(
  "runner_does_not_recreate_contamination",
  !grepl("20261105L", runner_text, fixed = TRUE) &&
    !grepl("sample(", runner_text, fixed = TRUE)
)

add_check(
  "runner_uses_separate_definitive_study3_directory",
  grepl("definitive-study3", runner_text, fixed = TRUE)
)

message("Phase 6C validation: checking synthetic LOO equivalence...")

set.seed(20261110L)

cluster <- factor(rep(seq_len(6L), each = 12L))
x <- stats::rnorm(length(cluster))
u <- stats::rnorm(nlevels(cluster), sd = 0.4)

synthetic <- data.frame(
  Reaction = 0.20 * x + u[as.integer(cluster)] + stats::rnorm(length(cluster)),
  Days = x,
  Subject = cluster
)

methods <- c("cr2", "cats")
seed <- study3c_analysis_seed()

direct <- mmiCATs::cluster_model_diagnostics(
  Reaction ~ Days,
  ~ Subject,
  synthetic,
  methods = methods,
  alpha = 0.05,
  seed = seed,
  leave_one_cluster_out = TRUE
)

full <- study3c_fit_full(
  synthetic,
  methods = methods,
  seed = seed
)

dat <- study3c_prepare_analysis_data(synthetic)

custom <- do.call(
  rbind,
  lapply(seq_along(levels(dat$cluster)), function(i) {
    study3c_fit_loo_subject(
      dat = dat,
      methods = methods,
      seed = seed,
      full_comparison = full$comparison,
      cluster_index = i
    )
  })
)
rownames(custom) <- NULL

direct_loo <- direct$influence
rownames(direct_loo) <- NULL

add_check(
  "custom_loo_matches_package_loo",
  isTRUE(all.equal(
    custom,
    direct_loo,
    tolerance = 1e-12,
    check.attributes = TRUE
  ))
)

validation_dir <- file.path(
  project_root, "data-raw", "study3-results", "phase6c-runner-validation"
)

checkpoint_dir <- file.path(validation_dir, "synthetic-checkpoints")

if (dir.exists(checkpoint_dir)) {
  unlink(checkpoint_dir, recursive = TRUE, force = TRUE)
}
dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

input_path <- file.path(checkpoint_dir, "synthetic_input.rds")
saveRDS(synthetic, input_path)
input_md5 <- unname(tools::md5sum(input_path))

first <- study3c_run_loo_checkpoint(
  dat = dat,
  methods = methods,
  seed = seed,
  full_comparison = full$comparison,
  cluster_index = 1L,
  dataset = "synthetic",
  input_md5 = input_md5,
  checkpoint_dir = checkpoint_dir
)

first_md5 <- unname(tools::md5sum(first$path))
first_mtime <- file.info(first$path)$mtime

Sys.sleep(1)

second <- study3c_run_loo_checkpoint(
  dat = dat,
  methods = methods,
  seed = seed,
  full_comparison = full$comparison,
  cluster_index = 1L,
  dataset = "synthetic",
  input_md5 = input_md5,
  checkpoint_dir = checkpoint_dir
)

second_md5 <- unname(tools::md5sum(first$path))
second_mtime <- file.info(first$path)$mtime

add_check(
  "completed_loo_checkpoint_is_skipped",
  identical(second$action, "skipped")
)

add_check(
  "completed_loo_checkpoint_is_not_rewritten",
  identical(first_md5, second_md5) &&
    identical(first_mtime, second_mtime)
)

for (i in seq_along(levels(dat$cluster))) {
  study3c_run_loo_checkpoint(
    dat = dat,
    methods = methods,
    seed = seed,
    full_comparison = full$comparison,
    cluster_index = i,
    dataset = "synthetic",
    input_md5 = input_md5,
    checkpoint_dir = checkpoint_dir
  )
}

collected <- study3c_collect_loo(
  dat = dat,
  methods = methods,
  seed = seed,
  dataset = "synthetic",
  input_md5 = input_md5,
  checkpoint_dir = checkpoint_dir
)

add_check(
  "all_synthetic_loo_checkpoints_collect",
  collected$complete &&
    nrow(collected$results) == length(methods) * nlevels(dat$cluster)
)

add_check(
  "collected_loo_matches_package_loo",
  isTRUE(all.equal(
    collected$results,
    direct_loo,
    tolerance = 1e-12,
    check.attributes = TRUE
  ))
)

definitive_dir <- file.path(
  project_root, "data-raw", "study3-results", "definitive-study3"
)

definitive_files <- if (dir.exists(definitive_dir)) {
  list.files(definitive_dir, recursive = TRUE, full.names = TRUE)
} else {
  character(0)
}

add_check(
  "no_definitive_study3_outputs_exist_before_execution",
  length(definitive_files) == 0L,
  if (length(definitive_files) > 0L) {
    paste(basename(definitive_files), collapse = ", ")
  } else {
    NA_character_
  }
)

checks_df <- do.call(rbind, checks)
rownames(checks_df) <- NULL

dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

utils::write.csv(
  checks_df,
  file.path(validation_dir, "phase6c_checks.csv"),
  row.names = FALSE,
  na = ""
)

writeLines(
  capture.output(utils::sessionInfo()),
  file.path(validation_dir, "session_info.txt"),
  useBytes = TRUE
)

writeLines(
  c(
    "mmiCATs Study 3 Phase 6C",
    "Comparative-analysis runner validation",
    "",
    paste("Checks passed:", sum(checks_df$passed), "of", nrow(checks_df)),
    paste("Synthetic clusters:", nlevels(dat$cluster)),
    paste("Synthetic LOO methods:", length(methods)),
    paste("Synthetic LOO rows:", nrow(collected$results)),
    "sleepstudy comparative results generated during validation: FALSE"
  ),
  file.path(validation_dir, "phase6c_summary.txt"),
  useBytes = TRUE
)

message("")
message("Phase 6C runner-validation checks:")
print(checks_df, row.names = FALSE)

if (!all(checks_df$passed)) {
  stop(
    paste(
      sum(!checks_df$passed),
      "Phase 6C runner-validation check(s) failed.",
      "Do not run the definitive Study 3 comparative analysis."
    ),
    call. = FALSE
  )
}

message("")
message(
  paste(
    "All Phase 6C Study 3 runner-validation checks passed.",
    "The frozen Study 3 inputs are ready for definitive comparative execution."
  )
)
