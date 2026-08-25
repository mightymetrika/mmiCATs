# Definitive simulation runner engineering: Phase 5A
# Deterministic replication-seed plumbing validation
#
# Purpose:
#   Add a narrowly scoped explicit-replication-seed path to pwr_func_study1()
#   and pwr_func_study2() so definitive simulations can be split into small
#   restartable shards without changing the data, method-specific seeds, or
#   inferential results that a monolithic run would produce.
#
# This is a software/plumbing validation, not a statistical performance study.
# It does not modify the frozen Study 1 or Study 2 DGPs, method definitions,
# condition seeds, common-random-number groups, or inferential rules.

library(devtools)

load_all()

phase5a_find_project_root <- function(path = getwd()) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  repeat {
    if (file.exists(file.path(path, "DESCRIPTION"))) {
      return(path)
    }

    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Could not locate the mmiCATs project root.", call. = FALSE)
    }
    path <- parent
  }
}

phase5a_save_rds_atomic <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    pattern = "phase5a_",
    tmpdir = dirname(path),
    fileext = ".rds"
  )
  saveRDS(object, temporary, version = 3, compress = "gzip")

  if (file.exists(path) && !file.remove(path)) {
    stop(paste("Could not replace existing file:", path), call. = FALSE)
  }
  if (!file.rename(temporary, path)) {
    stop(paste("Could not save file:", path), call. = FALSE)
  }
  invisible(path)
}

phase5a_write_csv_atomic <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    pattern = "phase5a_",
    tmpdir = dirname(path),
    fileext = ".csv"
  )
  utils::write.csv(object, temporary, row.names = FALSE, na = "")

  if (file.exists(path) && !file.remove(path)) {
    stop(paste("Could not replace existing file:", path), call. = FALSE)
  }
  if (!file.rename(temporary, path)) {
    stop(paste("Could not save file:", path), call. = FALSE)
  }
  invisible(path)
}

phase5a_drop_runtime <- function(data) {
  data[
    ,
    setdiff(names(data), "runtime_sec"),
    drop = FALSE
  ]
}

phase5a_sort_replicates <- function(data) {
  data <- data[
    order(data$replicate, data$method),
    ,
    drop = FALSE
  ]
  rownames(data) <- NULL
  data
}

phase5a_equal <- function(x, y, tolerance = 1e-12) {
  isTRUE(all.equal(
    x,
    y,
    tolerance = tolerance,
    check.attributes = TRUE
  ))
}

phase5a_generated_seeds <- function(seed, reps) {
  set.seed(seed)
  sample.int(
    .Machine$integer.max,
    size = reps,
    replace = FALSE
  )
}

phase5a_bind_shards <- function(shards, starts) {
  rows <- lapply(
    seq_along(shards),
    function(index) {
      data <- shards[[index]]$replicates
      data$replicate <- data$replicate + starts[index] - 1L
      data
    }
  )

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

phase5a_compare_one <- function(study = c("study1", "study2")) {
  study <- match.arg(study)
  reps <- 2L
  shard_starts <- c(1L, 2L)
  shard_ends <- c(1L, 2L)

  if (study == "study1") {
    condition_seed <- 20261030L
    methods <- study1_method_names()
    args <- list(
      n_clusters = 6L,
      cluster_size = 20L,
      beta = 0.10,
      intercept = 0,
      random_intercept_sd = 1,
      residual_sd = 1,
      x_sd = 1,
      contamination = "bad_leverage",
      contamination_prop = 0.05,
      contamination_size = 0.375,
      leverage_size = 4,
      reps = reps,
      alpha = 0.05,
      methods = methods,
      seed = condition_seed,
      keep_replicates = TRUE
    )
    simulation_function <- pwr_func_study1
  } else {
    condition_seed <- 20261031L
    methods <- study2_method_names()
    args <- list(
      n_clusters = 6L,
      cluster_size = 20L,
      beta = 0.10,
      intercept = 0,
      random_intercept_sd = 1,
      random_slope_sd = 0.10,
      residual_sd = 1,
      x_sd = 1,
      contamination = "vertical",
      contamination_prop = 0.05,
      contamination_size = 6,
      reps = reps,
      alpha = 0.05,
      methods = methods,
      seed = condition_seed,
      keep_replicates = TRUE
    )
    simulation_function <- pwr_func_study2
  }

  monolithic <- suppressWarnings(
    suppressMessages(
      do.call(simulation_function, args)
    )
  )

  expected_seeds <- phase5a_generated_seeds(
    seed = condition_seed,
    reps = reps
  )

  shards <- lapply(
    seq_along(shard_starts),
    function(index) {
      shard_args <- args
      shard_args$reps <- shard_ends[index] - shard_starts[index] + 1L
      shard_args$seed <- NULL
      shard_args$replicate_seeds <- expected_seeds[
        shard_starts[index]:shard_ends[index]
      ]

      suppressWarnings(
        suppressMessages(
          do.call(simulation_function, shard_args)
        )
      )
    }
  )

  sharded_replicates <- phase5a_bind_shards(
    shards = shards,
    starts = shard_starts
  )

  monolithic_compare <- phase5a_drop_runtime(
    phase5a_sort_replicates(monolithic$replicates)
  )
  sharded_compare <- phase5a_drop_runtime(
    phase5a_sort_replicates(sharded_replicates)
  )

  sharded_summary <- study1_summarize_results(
    replicate_results = sharded_replicates,
    methods = methods,
    reps = reps
  )

  summary_columns <- setdiff(
    names(monolithic$summary),
    "mean_runtime_sec"
  )

  monolithic_summary <- monolithic$summary[
    ,
    summary_columns,
    drop = FALSE
  ]
  sharded_summary_compare <- sharded_summary[
    ,
    summary_columns,
    drop = FALSE
  ]

  data.frame(
    study = study,
    condition_seed = condition_seed,
    reps = reps,
    methods = length(methods),
    generated_seed_vector_matches = identical(
      as.integer(monolithic$settings$replicate_seeds),
      as.integer(expected_seeds)
    ),
    shard_seed_vectors_match = identical(
      as.integer(unlist(lapply(
        shards,
        function(x) x$settings$replicate_seeds
      ))),
      as.integer(expected_seeds)
    ),
    replicate_results_match = phase5a_equal(
      monolithic_compare,
      sharded_compare
    ),
    summary_results_match = phase5a_equal(
      monolithic_summary,
      sharded_summary_compare
    ),
    stringsAsFactors = FALSE
  )
}

project_root <- phase5a_find_project_root()
output_dir <- file.path(
  project_root,
  "data-raw",
  "definitive-runner-results",
  "phase5a-seed-plumbing"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

message("Phase 5A: validating Study 1 full-method sharding equivalence...")
study1_result <- phase5a_compare_one("study1")

message("Phase 5A: validating Study 2 full-method sharding equivalence...")
study2_result <- phase5a_compare_one("study2")

comparison <- rbind(study1_result, study2_result)
rownames(comparison) <- NULL

checks <- data.frame(
  check = c(
    "study1_generated_seed_vector_matches",
    "study1_shard_seed_vectors_match",
    "study1_replicate_results_match",
    "study1_summary_results_match",
    "study2_generated_seed_vector_matches",
    "study2_shard_seed_vectors_match",
    "study2_replicate_results_match",
    "study2_summary_results_match"
  ),
  passed = c(
    comparison$generated_seed_vector_matches[comparison$study == "study1"],
    comparison$shard_seed_vectors_match[comparison$study == "study1"],
    comparison$replicate_results_match[comparison$study == "study1"],
    comparison$summary_results_match[comparison$study == "study1"],
    comparison$generated_seed_vector_matches[comparison$study == "study2"],
    comparison$shard_seed_vectors_match[comparison$study == "study2"],
    comparison$replicate_results_match[comparison$study == "study2"],
    comparison$summary_results_match[comparison$study == "study2"]
  ),
  stringsAsFactors = FALSE
)

source_files <- c(
  pwr_func_study1 = file.path(project_root, "R", "pwr_func_study1.R"),
  pwr_func_study1_helpers = file.path(
    project_root,
    "R",
    "pwr_func_study1_helpers.R"
  ),
  pwr_func_study2 = file.path(project_root, "R", "pwr_func_study2.R"),
  phase5a_tests = file.path(
    project_root,
    "tests",
    "testthat",
    "test-definitive-sharding-seeds.R"
  ),
  phase5a_validator = file.path(
    project_root,
    "data-raw",
    "definitive_sharding_phase5a_seed_validation.R"
  )
)

source_checksums <- data.frame(
  source = names(source_files),
  path = normalizePath(
    source_files,
    winslash = "/",
    mustWork = TRUE
  ),
  md5 = unname(tools::md5sum(source_files)),
  stringsAsFactors = FALSE
)

package_names <- c(
  "mmiCATs",
  "robustlmm",
  "lme4",
  "lmerTest",
  "pbkrtest",
  "clubSandwich",
  "clusterSEs",
  "robust",
  "robustbase",
  "testthat"
)

package_versions <- data.frame(
  package = package_names,
  version = vapply(
    package_names,
    function(package_name) {
      if (requireNamespace(package_name, quietly = TRUE)) {
        as.character(utils::packageVersion(package_name))
      } else {
        NA_character_
      }
    },
    character(1)
  ),
  stringsAsFactors = FALSE
)

phase5a_write_csv_atomic(
  comparison,
  file.path(output_dir, "phase5a_comparison.csv")
)
phase5a_write_csv_atomic(
  checks,
  file.path(output_dir, "phase5a_checks.csv")
)
phase5a_write_csv_atomic(
  source_checksums,
  file.path(output_dir, "phase5a_source_checksums.csv")
)
phase5a_write_csv_atomic(
  package_versions,
  file.path(output_dir, "phase5a_package_versions.csv")
)

phase5a_save_rds_atomic(
  list(
    comparison = comparison,
    checks = checks,
    source_checksums = source_checksums,
    package_versions = package_versions,
    session_info = utils::sessionInfo()
  ),
  file.path(output_dir, "phase5a_results.rds")
)

writeLines(
  capture.output(utils::sessionInfo()),
  file.path(output_dir, "session_info.txt"),
  useBytes = TRUE
)

summary_lines <- c(
  "Definitive simulation runner engineering: Phase 5A",
  "Deterministic replication-seed plumbing validation",
  "",
  paste("Checks passed:", sum(checks$passed), "of", nrow(checks)),
  paste(
    "Study 1 full-method replicate equivalence:",
    comparison$replicate_results_match[comparison$study == "study1"]
  ),
  paste(
    "Study 1 full-method summary equivalence:",
    comparison$summary_results_match[comparison$study == "study1"]
  ),
  paste(
    "Study 2 full-method replicate equivalence:",
    comparison$replicate_results_match[comparison$study == "study2"]
  ),
  paste(
    "Study 2 full-method summary equivalence:",
    comparison$summary_results_match[comparison$study == "study2"]
  )
)
writeLines(
  summary_lines,
  file.path(output_dir, "phase5a_summary.txt"),
  useBytes = TRUE
)

message("")
message("Phase 5A checks:")
print(checks, row.names = FALSE)
message("")
print(comparison, row.names = FALSE)
message("")
message(paste("Results saved to:", output_dir))

if (!all(checks$passed)) {
  stop(
    paste(
      sum(!checks$passed),
      "Phase 5A validation check(s) failed.",
      "Do not build the definitive sharded runners yet."
    ),
    call. = FALSE
  )
}

message("")
message(
  paste(
    "All Phase 5A deterministic seed-plumbing checks passed.",
    "The explicit replication-seed path is ready for sharded-runner engineering."
  )
)
