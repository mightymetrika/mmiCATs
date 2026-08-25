# Definitive simulation runner engineering: Phase 5C
#
# Validates the production runner integration without launching the 2,000-rep
# definitive simulations.

library(devtools)
load_all()
source("data-raw/definitive_sharding_helpers.R")

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
output_dir <- file.path(
  project_root,
  "data-raw",
  "definitive-runner-results",
  "phase5c-definitive-runner-integration"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

read_compact <- function(path) {
  paste0(
    gsub(
      "[[:space:]]+",
      "",
      paste(readLines(path, warn = FALSE), collapse = "\n")
    ),
    collapse = ""
  )
}

add_check <- local({
  rows <- list()
  i <- 0L
  function(name = NULL, passed = NULL, details = NA_character_, get = FALSE) {
    if (get) return(do.call(rbind, rows))
    i <<- i + 1L
    rows[[i]] <<- data.frame(
      check = name,
      passed = isTRUE(passed),
      details = as.character(details),
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }
})

s1_path <- file.path(project_root, "data-raw", "study1_final_simulation.R")
s2_path <- file.path(project_root, "data-raw", "study2_final_simulation.R")
helper_path <- file.path(project_root, "data-raw", "definitive_sharding_helpers.R")

s1 <- read_compact(s1_path)
s2 <- read_compact(s2_path)

s1_methods <- 'methods<-c("ri","cr2","cats","cats_trunc","cats_robust","cats_robustbase","robust_ri")'
s2_methods <- 'methods<-c("rs","ri","cr2","cats","cats_trunc","cats_robust","cats_robustbase","robust_ri","robust_rs")'

add_check("study1_canonical_7_method_schedule", grepl(s1_methods, s1, fixed = TRUE))
add_check("study2_canonical_9_method_schedule", grepl(s2_methods, s2, fixed = TRUE))
add_check("study1_2000_reps_frozen", grepl("final_reps<-2000L", s1, fixed = TRUE))
add_check("study2_2000_reps_frozen", grepl("final_reps<-2000L", s2, fixed = TRUE))
add_check("study1_seed_base_unchanged", grepl("final_seed_base<-20260815L", s1, fixed = TRUE))
add_check("study2_seed_base_unchanged", grepl("final_seed_base<-20260905L", s2, fixed = TRUE))
add_check("study1_shard_size_10", grepl("shard_size<-10L", s1, fixed = TRUE))
add_check("study2_shard_size_10", grepl("shard_size<-10L", s2, fixed = TRUE))
add_check("study1_disk_guard_2gb", grepl("minimum_free_gb<-2.0", s1, fixed = TRUE))
add_check("study2_disk_guard_2gb", grepl("minimum_free_gb<-2.0", s2, fixed = TRUE))
add_check("study1_uses_definitive_output_directory", grepl('"definitive-study"', s1, fixed = TRUE))
add_check("study2_uses_definitive_output_directory", grepl('"definitive-study"', s2, fixed = TRUE))
add_check("study1_sources_validated_sharding_helper", grepl('"definitive_sharding_helpers.R"', s1, fixed = TRUE))
add_check("study2_sources_validated_sharding_helper", grepl('"definitive_sharding_helpers.R"', s2, fixed = TRUE))
add_check("study1_calls_shard_engine", grepl("definitive_run_shard_checkpoint(", s1, fixed = TRUE))
add_check("study2_calls_shard_engine", grepl("definitive_run_shard_checkpoint(", s2, fixed = TRUE))
add_check("study1_recombines_before_summary", grepl("study1_summarize_results(", s1, fixed = TRUE))
add_check("study2_recombines_before_summary", grepl("study1_summarize_results(", s2, fixed = TRUE))
add_check("study1_temp_shard_cleanup_frozen", grepl("retain_completed_shards<-FALSE", s1, fixed = TRUE))
add_check("study2_temp_shard_cleanup_frozen", grepl("retain_completed_shards<-FALSE", s2, fixed = TRUE))

plan <- definitive_make_shard_plan(2000L, 10L)
coverage <- unlist(
  Map(seq.int, plan$replicate_start, plan$replicate_end),
  use.names = FALSE
)
add_check(
  "production_shard_plan_has_200_shards",
  nrow(plan) == 200L,
  paste("Observed", nrow(plan))
)
add_check(
  "production_shard_plan_covers_1_to_2000_exactly_once",
  identical(as.integer(coverage), 1:2000)
)

s1_seed_10 <- definitive_make_replicate_seeds(20260815L, 2000L)
s1_seed_20 <- definitive_make_replicate_seeds(20260816L, 2000L)
s1_seed_40 <- definitive_make_replicate_seeds(20260817L, 2000L)
s2_seed_10 <- definitive_make_replicate_seeds(20260905L, 2000L)
s2_seed_20 <- definitive_make_replicate_seeds(20260906L, 2000L)
s2_seed_40 <- definitive_make_replicate_seeds(20260907L, 2000L)

add_check(
  "study1_cluster_count_seed_vectors_are_distinct",
  !identical(s1_seed_10, s1_seed_20) &&
    !identical(s1_seed_10, s1_seed_40) &&
    !identical(s1_seed_20, s1_seed_40)
)
add_check(
  "study2_cluster_count_seed_vectors_are_distinct",
  !identical(s2_seed_10, s2_seed_20) &&
    !identical(s2_seed_10, s2_seed_40) &&
    !identical(s2_seed_20, s2_seed_40)
)

# Verify the actual machine can resolve free space and currently clears the
# frozen safety threshold.
free_gb <- definitive_get_free_gb(output_dir)
add_check(
  "production_machine_free_space_is_measurable",
  is.finite(free_gb),
  paste("Free GB:", format(free_gb, digits = 6))
)
add_check(
  "production_machine_currently_clears_2gb_guard",
  is.finite(free_gb) && free_gb >= 2.0,
  paste("Free GB:", format(free_gb, digits = 6))
)

# End-to-end retry check for a matching caught-error checkpoint. This uses the
# real full Study 1 7-method schedule for one small replication.
retry_dir <- file.path(output_dir, "matching-error-retry")
if (dir.exists(retry_dir)) unlink(retry_dir, recursive = TRUE, force = TRUE)
dir.create(retry_dir, recursive = TRUE, showWarnings = FALSE)

retry_condition <- data.frame(
  condition_id = "P5C_RETRY",
  n_clusters = 6L,
  cluster_size = 20L,
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
retry_plan <- definitive_make_shard_plan(1L, 1L)
retry_seeds <- definitive_make_replicate_seeds(20261050L, 1L)
retry_methods <- mmiCATs:::study1_method_names()
retry_path <- definitive_shard_checkpoint_path(
  retry_dir,
  retry_condition$condition_id,
  retry_plan$shard_id[1L]
)

synthetic_error <- list(
  status = "error",
  study = "study1",
  condition_id = retry_condition$condition_id,
  shard_id = retry_plan$shard_id[1L],
  replicate_start = 1L,
  replicate_end = 1L,
  replicate_seeds = as.integer(retry_seeds),
  methods = retry_methods,
  replicates = NULL,
  error = "Synthetic caught error used to validate automatic retry.",
  started_at = Sys.time(),
  completed_at = Sys.time(),
  elapsed_sec = 0
)
definitive_save_rds_atomic(synthetic_error, retry_path)

retry_result <- tryCatch(
  definitive_run_shard_checkpoint(
    study = "study1",
    condition = retry_condition,
    shard_row = retry_plan[1L, , drop = FALSE],
    replicate_seed_vector = retry_seeds,
    methods = retry_methods,
    shard_dir = retry_dir,
    minimum_free_gb = 0,
    overwrite_completed = FALSE
  ),
  error = function(e) e
)

retry_pass <- !inherits(retry_result, "error") &&
  identical(retry_result$action, "completed") &&
  definitive_validate_complete_checkpoint(
    retry_result$checkpoint,
    retry_condition$condition_id,
    retry_plan[1L, , drop = FALSE],
    retry_seeds,
    retry_methods
  )
add_check("matching_error_checkpoint_retries_successfully", retry_pass)

checks <- add_check(get = TRUE)
rownames(checks) <- NULL

source_files <- c(
  study1_final_simulation = s1_path,
  study2_final_simulation = s2_path,
  definitive_sharding_helpers = helper_path,
  phase5c_validator = file.path(project_root, "data-raw", "definitive_sharding_phase5c_validation.R")
)
source_checksums <- data.frame(
  source = names(source_files),
  path = normalizePath(source_files, winslash = "/", mustWork = TRUE),
  md5 = unname(tools::md5sum(source_files)),
  stringsAsFactors = FALSE
)

write.csv(checks, file.path(output_dir, "phase5c_checks.csv"), row.names = FALSE, na = "")
write.csv(plan, file.path(output_dir, "phase5c_production_shard_plan.csv"), row.names = FALSE, na = "")
write.csv(source_checksums, file.path(output_dir, "phase5c_source_checksums.csv"), row.names = FALSE, na = "")
writeLines(capture.output(sessionInfo()), file.path(output_dir, "session_info.txt"))

summary_lines <- c(
  "Definitive simulation runner engineering: Phase 5C",
  "Production definitive-runner integration validation",
  "",
  paste("Checks passed:", sum(checks$passed), "of", nrow(checks)),
  paste("Production shard count per condition:", nrow(plan)),
  paste("Production shard size:", 10L),
  paste("Frozen disk-space guard (GB):", 2.0),
  paste("Observed free disk space (GB):", format(free_gb, digits = 6)),
  paste("Matching caught-error checkpoint retry:", retry_pass)
)
writeLines(summary_lines, file.path(output_dir, "phase5c_summary.txt"))

message("")
message("Phase 5C checks:")
print(checks, row.names = FALSE)
message("")
message(paste("Results saved to:", output_dir))

if (!all(checks$passed)) {
  stop(
    paste(
      sum(!checks$passed),
      "Phase 5C definitive-runner integration check(s) failed.",
      "Do not freeze or launch the definitive simulations yet."
    ),
    call. = FALSE
  )
}

message("")
message(
  paste(
    "All Phase 5C definitive-runner integration checks passed.",
    "The runner code is ready for the final pre-results/code-freeze gate."
  )
)
