# Definitive simulation runner engineering: Phase 5B
#
# Validates deterministic shard execution, checkpoint integrity, resume/skip
# behavior, global replicate numbering, and recombination against monolithic
# pwr_func_study1()/pwr_func_study2() calls.
#
# This is an engineering validation only. It does not modify the definitive
# Study 1 or Study 2 final-run scripts and it does not generate manuscript
# evidence.

library(devtools)

load_all()

source(
  "data-raw/definitive_sharding_helpers.R"
)

project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

output_dir <- file.path(
  project_root,
  "data-raw",
  "definitive-runner-results",
  "phase5b-sharding-validation"
)

if (dir.exists(output_dir)) {
  unlink(
    output_dir,
    recursive = TRUE,
    force = TRUE
  )
}

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

study1_dir <- file.path(
  output_dir,
  "study1-shards"
)

study2_dir <- file.path(
  output_dir,
  "study2-shards"
)

dir.create(
  study1_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  study2_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

drop_runtime <- function(data) {
  # Runtime is intentionally excluded from equivalence checks. Replicate-level
  # runtime_sec and summary-level mean_runtime_sec are wall-clock measurements,
  # so they can differ when the same deterministic replications are executed
  # monolithically versus as separate shard calls.
  data[
    ,
    setdiff(
      names(data),
      c(
        "runtime_sec",
        "mean_runtime_sec"
      )
    ),
    drop = FALSE
  ]
}

sort_replicates <- function(data) {
  data <- data[
    order(
      data$replicate,
      data$method
    ),
    ,
    drop = FALSE
  ]
  rownames(data) <- NULL
  data
}

sort_summary <- function(data) {
  key <- if ("model" %in% names(data)) {
    "model"
  } else {
    "method"
  }

  data <- data[
    order(data[[key]]),
    ,
    drop = FALSE
  ]
  rownames(data) <- NULL
  data
}

equal_no_runtime <- function(x,
                             y,
                             tolerance = 1e-10) {
  isTRUE(
    all.equal(
      drop_runtime(x),
      drop_runtime(y),
      tolerance = tolerance,
      check.attributes = TRUE
    )
  )
}

make_s1_condition <- function() {
  data.frame(
    condition_id = "P5B_S1",
    n_clusters = 8L,
    cluster_size = 30L,
    beta = 0.10,
    intercept = 0,
    random_intercept_sd = 1,
    residual_sd = 1,
    x_sd = 1,
    contamination = "vertical",
    contamination_prop = 0.05,
    contamination_size = 6,
    leverage_size = 1,
    alpha = 0.05,
    stringsAsFactors = FALSE
  )
}

make_s2_condition <- function() {
  data.frame(
    condition_id = "P5B_S2",
    n_clusters = 8L,
    cluster_size = 30L,
    beta = 0.10,
    intercept = 0,
    random_intercept_sd = 1,
    random_slope_sd = 0.10,
    residual_sd = 1,
    x_sd = 1,
    contamination = "vertical",
    contamination_prop = 0.05,
    contamination_size = 6,
    alpha = 0.05,
    stringsAsFactors = FALSE
  )
}

study1_methods <- mmiCATs:::study1_method_names()
study2_methods <- mmiCATs:::study2_method_names()

total_reps <- 3L
shard_size <- 1L

study1_condition <- make_s1_condition()
study2_condition <- make_s2_condition()

study1_condition_seed <- 20261040L
study2_condition_seed <- 20261041L

study1_seeds <- definitive_make_replicate_seeds(
  study1_condition_seed,
  total_reps
)

study2_seeds <- definitive_make_replicate_seeds(
  study2_condition_seed,
  total_reps
)

shard_plan <- definitive_make_shard_plan(
  total_reps = total_reps,
  shard_size = shard_size
)

message(
  "Phase 5B: monolithic Study 1 reference..."
)

study1_monolithic <- suppressWarnings(
  mmiCATs::pwr_func_study1(
    n_clusters = study1_condition$n_clusters,
    cluster_size = study1_condition$cluster_size,
    beta = study1_condition$beta,
    intercept = study1_condition$intercept,
    random_intercept_sd =
      study1_condition$random_intercept_sd,
    residual_sd =
      study1_condition$residual_sd,
    x_sd = study1_condition$x_sd,
    contamination =
      study1_condition$contamination,
    contamination_prop =
      study1_condition$contamination_prop,
    contamination_size =
      study1_condition$contamination_size,
    leverage_size = 1,
    reps = total_reps,
    alpha = study1_condition$alpha,
    methods = study1_methods,
    seed = NULL,
    replicate_seeds = study1_seeds,
    keep_replicates = TRUE
  )
)

message(
  "Phase 5B: running first Study 1 shard, then simulating resume..."
)

first_s1 <- definitive_run_shard_checkpoint(
  study = "study1",
  condition = study1_condition,
  shard_row = shard_plan[1L, , drop = FALSE],
  replicate_seed_vector = study1_seeds,
  methods = study1_methods,
  shard_dir = study1_dir,
  minimum_free_gb = 0
)

first_s1_md5 <- unname(
  tools::md5sum(first_s1$path)
)

first_s1_mtime <- file.info(
  first_s1$path
)$mtime

Sys.sleep(1)

for (i in seq_len(nrow(shard_plan))) {
  definitive_run_shard_checkpoint(
    study = "study1",
    condition = study1_condition,
    shard_row = shard_plan[i, , drop = FALSE],
    replicate_seed_vector = study1_seeds,
    methods = study1_methods,
    shard_dir = study1_dir,
    minimum_free_gb = 0
  )
}

first_s1_md5_after <- unname(
  tools::md5sum(first_s1$path)
)

first_s1_mtime_after <- file.info(
  first_s1$path
)$mtime

study1_collected <-
  definitive_collect_condition_shards(
    condition = study1_condition,
    shard_plan = shard_plan,
    replicate_seed_vector = study1_seeds,
    methods = study1_methods,
    shard_dir = study1_dir
  )

study1_combined <- sort_replicates(
  study1_collected$replicates
)

study1_reference <- sort_replicates(
  study1_monolithic$replicates
)

study1_replicate_match <-
  equal_no_runtime(
    study1_combined,
    study1_reference
  )

study1_combined_summary <-
  mmiCATs:::study1_summarize_results(
    replicate_results =
      study1_collected$replicates,
    methods = study1_methods,
    reps = total_reps
  )

study1_summary_match <-
  equal_no_runtime(
    sort_summary(
      study1_combined_summary
    ),
    sort_summary(
      study1_monolithic$summary
    )
  )

study1_resume_preserved <-
  identical(
    first_s1_md5,
    first_s1_md5_after
  ) &&
  identical(
    first_s1_mtime,
    first_s1_mtime_after
  )

study1_global_ids <-
  identical(
    sort(
      unique(
        study1_collected$
          replicates$replicate
      )
    ),
    seq_len(total_reps)
  )


message(
  "Phase 5B: monolithic Study 2 reference..."
)

study2_monolithic <- suppressWarnings(
  suppressMessages(
    mmiCATs::pwr_func_study2(
      n_clusters = study2_condition$n_clusters,
      cluster_size = study2_condition$cluster_size,
      beta = study2_condition$beta,
      intercept = study2_condition$intercept,
      random_intercept_sd =
        study2_condition$random_intercept_sd,
      random_slope_sd =
        study2_condition$random_slope_sd,
      residual_sd =
        study2_condition$residual_sd,
      x_sd = study2_condition$x_sd,
      contamination =
        study2_condition$contamination,
      contamination_prop =
        study2_condition$contamination_prop,
      contamination_size =
        study2_condition$contamination_size,
      reps = total_reps,
      alpha = study2_condition$alpha,
      methods = study2_methods,
      seed = NULL,
      replicate_seeds = study2_seeds,
      keep_replicates = TRUE
    )
  )
)

message(
  "Phase 5B: running first Study 2 shard, then simulating resume..."
)

first_s2 <- definitive_run_shard_checkpoint(
  study = "study2",
  condition = study2_condition,
  shard_row = shard_plan[1L, , drop = FALSE],
  replicate_seed_vector = study2_seeds,
  methods = study2_methods,
  shard_dir = study2_dir,
  minimum_free_gb = 0
)

first_s2_md5 <- unname(
  tools::md5sum(first_s2$path)
)

first_s2_mtime <- file.info(
  first_s2$path
)$mtime

Sys.sleep(1)

for (i in seq_len(nrow(shard_plan))) {
  definitive_run_shard_checkpoint(
    study = "study2",
    condition = study2_condition,
    shard_row = shard_plan[i, , drop = FALSE],
    replicate_seed_vector = study2_seeds,
    methods = study2_methods,
    shard_dir = study2_dir,
    minimum_free_gb = 0
  )
}

first_s2_md5_after <- unname(
  tools::md5sum(first_s2$path)
)

first_s2_mtime_after <- file.info(
  first_s2$path
)$mtime

study2_collected <-
  definitive_collect_condition_shards(
    condition = study2_condition,
    shard_plan = shard_plan,
    replicate_seed_vector = study2_seeds,
    methods = study2_methods,
    shard_dir = study2_dir
  )

study2_combined <- sort_replicates(
  study2_collected$replicates
)

study2_reference <- sort_replicates(
  study2_monolithic$replicates
)

study2_replicate_match <-
  equal_no_runtime(
    study2_combined,
    study2_reference
  )

study2_combined_summary <-
  mmiCATs:::study1_summarize_results(
    replicate_results =
      study2_collected$replicates,
    methods = study2_methods,
    reps = total_reps
  )

study2_summary_match <-
  equal_no_runtime(
    sort_summary(
      study2_combined_summary
    ),
    sort_summary(
      study2_monolithic$summary
    )
  )

study2_resume_preserved <-
  identical(
    first_s2_md5,
    first_s2_md5_after
  ) &&
  identical(
    first_s2_mtime,
    first_s2_mtime_after
  )

study2_global_ids <-
  identical(
    sort(
      unique(
        study2_collected$
          replicates$replicate
      )
    ),
    seq_len(total_reps)
  )


message(
  "Phase 5B: testing disk guard and invalid-checkpoint refusal..."
)

disk_guard_pass <- FALSE

tryCatch(
  {
    definitive_disk_guard(
      output_dir,
      minimum_free_gb = 2,
      free_gb = 1.5
    )
  },
  error = function(e) {
    disk_guard_pass <<-
      grepl(
        "No new shard was started",
        conditionMessage(e),
        fixed = TRUE
      )
  }
)

invalid_checkpoint_pass <- FALSE

invalid_path <- definitive_shard_checkpoint_path(
  shard_dir = file.path(
    output_dir,
    "invalid-checkpoint-test"
  ),
  condition_id = "P5B_INVALID",
  shard_id = "R0001-R0001"
)

dir.create(
  dirname(invalid_path),
  recursive = TRUE,
  showWarnings = FALSE
)

bad_checkpoint <- list(
  status = "complete",
  study = "study1",
  condition_id = "P5B_INVALID",
  shard_id = "R0001-R0001",
  replicate_start = 1L,
  replicate_end = 1L,
  replicate_seeds = 999L,
  methods = study1_methods,
  replicates = data.frame(
    replicate = rep(
      1L,
      length(study1_methods)
    ),
    method = study1_methods,
    stringsAsFactors = FALSE
  )
)

saveRDS(
  bad_checkpoint,
  invalid_path
)

invalid_condition <- study1_condition
invalid_condition$condition_id <-
  "P5B_INVALID"

tryCatch(
  {
    definitive_run_shard_checkpoint(
      study = "study1",
      condition = invalid_condition,
      shard_row =
        shard_plan[1L, , drop = FALSE],
      replicate_seed_vector =
        study1_seeds,
      methods = study1_methods,
      shard_dir = dirname(invalid_path),
      minimum_free_gb = 0
    )
  },
  error = function(e) {
    invalid_checkpoint_pass <<-
      grepl(
        "does not match",
        conditionMessage(e),
        fixed = TRUE
      )
  }
)


checks <- data.frame(
  check = c(
    "study1_all_shards_complete",
    "study1_global_replicate_ids",
    "study1_replicate_equivalence",
    "study1_summary_equivalence",
    "study1_completed_shard_preserved_on_resume",
    "study2_all_shards_complete",
    "study2_global_replicate_ids",
    "study2_replicate_equivalence",
    "study2_summary_equivalence",
    "study2_completed_shard_preserved_on_resume",
    "disk_guard_blocks_new_work_below_threshold",
    "mismatched_existing_checkpoint_is_refused"
  ),
  passed = c(
    study1_collected$complete,
    study1_global_ids,
    study1_replicate_match,
    study1_summary_match,
    study1_resume_preserved,
    study2_collected$complete,
    study2_global_ids,
    study2_replicate_match,
    study2_summary_match,
    study2_resume_preserved,
    disk_guard_pass,
    invalid_checkpoint_pass
  ),
  stringsAsFactors = FALSE
)

comparison <- data.frame(
  study = c("study1", "study2"),
  total_reps = total_reps,
  shard_size = shard_size,
  shards = nrow(shard_plan),
  methods = c(
    length(study1_methods),
    length(study2_methods)
  ),
  replicate_results_match = c(
    study1_replicate_match,
    study2_replicate_match
  ),
  summary_results_match = c(
    study1_summary_match,
    study2_summary_match
  ),
  resume_preserved_first_shard = c(
    study1_resume_preserved,
    study2_resume_preserved
  ),
  stringsAsFactors = FALSE
)

source_files <- c(
  sharding_helpers = file.path(
    project_root,
    "data-raw",
    "definitive_sharding_helpers.R"
  ),
  phase5b_validator = file.path(
    project_root,
    "data-raw",
    "definitive_sharding_phase5b_validation.R"
  ),
  pwr_func_study1 = file.path(
    project_root,
    "R",
    "pwr_func_study1.R"
  ),
  pwr_func_study2 = file.path(
    project_root,
    "R",
    "pwr_func_study2.R"
  )
)

source_checksums <- data.frame(
  source = names(source_files),
  path = normalizePath(
    source_files,
    winslash = "/",
    mustWork = TRUE
  ),
  md5 = unname(
    tools::md5sum(
      source_files
    )
  ),
  stringsAsFactors = FALSE
)

package_names <- c(
  "mmiCATs",
  "robustlmm",
  "lme4",
  "lmerTest",
  "pbkrtest",
  "clubSandwich",
  "robust",
  "robustbase",
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
        return(NA_character_)
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

definitive_write_csv_atomic(
  checks,
  file.path(
    output_dir,
    "phase5b_checks.csv"
  )
)

definitive_write_csv_atomic(
  comparison,
  file.path(
    output_dir,
    "phase5b_comparison.csv"
  )
)

definitive_write_csv_atomic(
  shard_plan,
  file.path(
    output_dir,
    "phase5b_shard_plan.csv"
  )
)

definitive_write_csv_atomic(
  study1_collected$status,
  file.path(
    output_dir,
    "phase5b_study1_shard_status.csv"
  )
)

definitive_write_csv_atomic(
  study2_collected$status,
  file.path(
    output_dir,
    "phase5b_study2_shard_status.csv"
  )
)

definitive_write_csv_atomic(
  source_checksums,
  file.path(
    output_dir,
    "phase5b_source_checksums.csv"
  )
)

definitive_write_csv_atomic(
  package_versions,
  file.path(
    output_dir,
    "phase5b_package_versions.csv"
  )
)

definitive_save_rds_atomic(
  list(
    checks = checks,
    comparison = comparison,
    shard_plan = shard_plan,
    study1_status =
      study1_collected$status,
    study2_status =
      study2_collected$status,
    source_checksums =
      source_checksums,
    package_versions =
      package_versions,
    session_info =
      utils::sessionInfo()
  ),
  file.path(
    output_dir,
    "phase5b_results.rds"
  )
)

writeLines(
  capture.output(
    utils::sessionInfo()
  ),
  con = file.path(
    output_dir,
    "session_info.txt"
  ),
  useBytes = TRUE
)

summary_lines <- c(
  "Definitive simulation runner engineering: Phase 5B",
  "Deterministic shard/checkpoint/resume validation",
  "",
  paste(
    "Checks passed:",
    sum(checks$passed),
    "of",
    nrow(checks)
  ),
  paste(
    "Study 1 full-method replicate equivalence:",
    study1_replicate_match
  ),
  paste(
    "Study 1 full-method non-runtime summary equivalence:",
    study1_summary_match
  ),
  paste(
    "Study 1 first completed shard preserved on resume:",
    study1_resume_preserved
  ),
  paste(
    "Study 2 full-method replicate equivalence:",
    study2_replicate_match
  ),
  paste(
    "Study 2 full-method non-runtime summary equivalence:",
    study2_summary_match
  ),
  paste(
    "Study 2 first completed shard preserved on resume:",
    study2_resume_preserved
  ),
  paste(
    "Disk guard passed:",
    disk_guard_pass
  ),
  paste(
    "Mismatched checkpoint refusal passed:",
    invalid_checkpoint_pass
  )
)

writeLines(
  summary_lines,
  con = file.path(
    output_dir,
    "phase5b_summary.txt"
  ),
  useBytes = TRUE
)

message("")
message("Phase 5B checks:")
print(
  checks,
  row.names = FALSE
)

message("")
print(
  comparison,
  row.names = FALSE
)

message("")
message(
  paste(
    "Results saved to:",
    output_dir
  )
)

if (!all(checks$passed)) {
  stop(
    paste(
      sum(!checks$passed),
      "Phase 5B sharding validation check(s) failed.",
      "Do not convert the definitive runners yet."
    ),
    call. = FALSE
  )
}

message("")
message(
  paste(
    "All Phase 5B deterministic sharding checks passed.",
    "The checkpoint/resume layer is ready for definitive-runner integration."
  )
)
