# Robust CATs Phase 3E: A-13 post-fix validation
#
# Validates the Study 1 random-intercept convergence-classification correction.
# This script does not modify production code.

library(devtools)

load_all()

project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

output_dir <- file.path(
  project_root,
  "data-raw",
  "robust-cats-audit-results",
  "phase3e-a13-post-fix"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

checks <- list()

add_check <- function(check,
                      passed,
                      details = NA_character_) {
  checks[[length(checks) + 1L]] <<-
    data.frame(
      check = check,
      passed = as.logical(passed),
      details = details,
      stringsAsFactors = FALSE
    )
}

message("1. Running focused A-13 regression tests...")

testthat::test_file(
  file.path(
    project_root,
    "tests",
    "testthat",
    "test-study1-ri-convergence.R"
  ),
  reporter = "progress",
  stop_on_failure = TRUE,
  stop_on_warning = FALSE
)

message("2. Reproducing the boundary-singular Study 1 case...")

n_clusters <- 10L
cluster_size <- 20L
x_pattern <- seq(
  -2,
  2,
  length.out = cluster_size
)
residual_pattern <- 0.25 * sin(
  seq_len(cluster_size)
)

singular_dat <- data.frame(
  cluster = factor(
    rep(
      seq_len(n_clusters),
      each = cluster_size
    )
  ),
  x = rep(
    x_pattern,
    times = n_clusters
  )
)

singular_dat$out <- 0.10 +
  0.25 * singular_dat$x +
  rep(
    residual_pattern,
    times = n_clusters
  )

direct_fit <- suppressWarnings(
  lmerTest::lmer(
    out ~ x + (1 | cluster),
    data = singular_dat,
    REML = TRUE
  )
)

direct_summary <- summary(
  direct_fit,
  ddf = "Kenward-Roger"
)
direct_row <- stats::coef(
  direct_summary
)["x", , drop = FALSE]

expected <- c(
  estimate = unname(
    direct_row[1L, "Estimate"]
  ),
  std_error = unname(
    direct_row[1L, "Std. Error"]
  ),
  df = unname(
    direct_row[1L, "df"]
  ),
  p_value = unname(
    direct_row[1L, "Pr(>|t|)"]
  )
)

critical <- stats::qt(
  0.975,
  df = expected["df"]
)

expected <- c(
  expected,
  conf_low =
    expected["estimate"] -
    critical * expected["std_error"],
  conf_high =
    expected["estimate"] +
    critical * expected["std_error"]
)

observed <- suppressWarnings(
  study1_fit_ri(
    dat = singular_dat,
    alpha = 0.05
  )
)

observed_values <- c(
  estimate = observed$estimate,
  std_error = observed$std_error,
  df = observed$df,
  p_value = observed$p_value,
  conf_low = observed$conf_low,
  conf_high = observed$conf_high
)

max_difference <- max(
  abs(
    observed_values -
      expected
  )
)

replicate_result <- suppressWarnings(
  study1_fit_method(
    dat = singular_dat,
    method = "ri",
    beta = 0.25,
    alpha = 0.05,
    replicate_id = 1L,
    method_seed = 20268101L
  )
)

singular_results <- data.frame(
  direct_singular =
    lme4::isSingular(
      direct_fit,
      tol = 1e-4
    ),
  helper_singular =
    observed$singular,
  helper_converged =
    observed$converged,
  replicate_fit_success =
    replicate_result$fit_success,
  replicate_converged =
    replicate_result$converged,
  replicate_singular =
    replicate_result$singular,
  maximum_numeric_difference =
    max_difference,
  stringsAsFactors = FALSE
)

add_check(
  "finite_boundary_singularity_is_nonfatal",
  isTRUE(singular_results$direct_singular) &&
    isTRUE(singular_results$helper_singular) &&
    isTRUE(singular_results$helper_converged) &&
    isTRUE(singular_results$replicate_fit_success) &&
    isTRUE(singular_results$replicate_converged) &&
    isTRUE(singular_results$replicate_singular),
  paste(
    "helper converged:",
    observed$converged,
    "; replicate success:",
    replicate_result$fit_success
  )
)

add_check(
  "boundary_case_numeric_inference_is_preserved",
  is.finite(max_difference) &&
    max_difference <= 1e-10,
  paste(
    "max absolute difference:",
    format(
      max_difference,
      scientific = TRUE
    )
  )
)

message("3. Verifying normal-case RI inference against direct KR calculations...")

set.seed(20268102L)

normal_dat <- study1_simulate_data(
  n_clusters = 10L,
  cluster_size = 40L,
  beta = 0.10,
  intercept = 0,
  random_intercept_sd = 1,
  residual_sd = 1,
  x_sd = 1,
  contamination = "none",
  contamination_prop = 0.05,
  contamination_size = 6,
  leverage_size = 4
)

normal_direct_fit <- suppressWarnings(
  lmerTest::lmer(
    out ~ x + (1 | cluster),
    data = normal_dat,
    REML = TRUE
  )
)

normal_direct_summary <- summary(
  normal_direct_fit,
  ddf = "Kenward-Roger"
)
normal_direct_row <- stats::coef(
  normal_direct_summary
)["x", , drop = FALSE]

normal_expected <- c(
  estimate = unname(
    normal_direct_row[1L, "Estimate"]
  ),
  std_error = unname(
    normal_direct_row[1L, "Std. Error"]
  ),
  df = unname(
    normal_direct_row[1L, "df"]
  ),
  p_value = unname(
    normal_direct_row[1L, "Pr(>|t|)"]
  )
)

normal_critical <- stats::qt(
  0.975,
  df = normal_expected["df"]
)

normal_expected <- c(
  normal_expected,
  conf_low =
    normal_expected["estimate"] -
    normal_critical *
      normal_expected["std_error"],
  conf_high =
    normal_expected["estimate"] +
    normal_critical *
      normal_expected["std_error"]
)

normal_observed <- suppressWarnings(
  study1_fit_ri(
    dat = normal_dat,
    alpha = 0.05
  )
)

normal_observed_values <- c(
  estimate =
    normal_observed$estimate,
  std_error =
    normal_observed$std_error,
  df = normal_observed$df,
  p_value =
    normal_observed$p_value,
  conf_low =
    normal_observed$conf_low,
  conf_high =
    normal_observed$conf_high
)

normal_max_difference <- max(
  abs(
    normal_observed_values -
      normal_expected
  )
)

normal_convergence_expected <-
  study2_classify_convergence(
    messages =
      normal_direct_fit@
        optinfo$conv$lme4$messages,
    optimizer_code =
      normal_direct_fit@
        optinfo$conv$opt
  )$converged

add_check(
  "normal_case_numeric_inference_matches_direct_kr",
  is.finite(normal_max_difference) &&
    normal_max_difference <= 1e-10,
  paste(
    "max absolute difference:",
    format(
      normal_max_difference,
      scientific = TRUE
    )
  )
)

add_check(
  "normal_case_convergence_matches_shared_classifier",
  identical(
    normal_observed$converged,
    normal_convergence_expected
  ),
  paste(
    "observed:",
    normal_observed$converged,
    "; expected:",
    normal_convergence_expected
  )
)

message("4. Verifying Study 1 / Study 2 RI shared dispatch remains aligned...")

shared_seed <- 20268103L

study1_shared <- suppressWarnings(
  study1_fit_method(
    dat = normal_dat,
    method = "ri",
    beta = 0.10,
    alpha = 0.05,
    replicate_id = 1L,
    method_seed = shared_seed
  )
)

study2_shared <- suppressWarnings(
  study2_fit_method(
    dat = normal_dat,
    method = "ri",
    beta = 0.10,
    alpha = 0.05,
    replicate_id = 1L,
    method_seed = shared_seed,
    realized_mean_slope = 0.10,
    realized_random_slope_sd = 0
  )
)

shared_columns <- intersect(
  names(study1_shared),
  names(study2_shared)
)

shared_columns <- setdiff(
  shared_columns,
  c(
    "runtime_sec",
    "realized_mean_slope",
    "realized_random_slope_sd",
    "estimated_random_intercept_sd",
    "estimated_random_slope_sd",
    "optimizer_warning",
    "optimizer_code"
  )
)

study1_compare <- study1_shared[
  ,
  shared_columns,
  drop = FALSE
]
study2_compare <- study2_shared[
  ,
  shared_columns,
  drop = FALSE
]

rownames(study1_compare) <- NULL
rownames(study2_compare) <- NULL

shared_match <- isTRUE(
  all.equal(
    study1_compare,
    study2_compare,
    tolerance = 1e-10,
    check.attributes = TRUE
  )
)

add_check(
  "study1_study2_ri_dispatch_remains_aligned",
  shared_match
)

message("5. Verifying method order remains invariant after the correction...")

order_one <- suppressWarnings(
  pwr_func_study1(
    n_clusters = 8L,
    cluster_size = 40L,
    beta = 0.10,
    contamination = "vertical",
    contamination_size = 6,
    reps = 2L,
    methods = c(
      "ri",
      "cr2",
      "cats"
    ),
    seed = 20268104L,
    keep_replicates = TRUE
  )
)

order_two <- suppressWarnings(
  pwr_func_study1(
    n_clusters = 8L,
    cluster_size = 40L,
    beta = 0.10,
    contamination = "vertical",
    contamination_size = 6,
    reps = 2L,
    methods = c(
      "cats",
      "ri",
      "cr2"
    ),
    seed = 20268104L,
    keep_replicates = TRUE
  )
)

comparison_columns <- setdiff(
  names(order_one$replicates),
  "runtime_sec"
)

first <- order_one$replicates[
  order(
    order_one$replicates$replicate,
    order_one$replicates$method
  ),
  comparison_columns,
  drop = FALSE
]

second <- order_two$replicates[
  order(
    order_two$replicates$replicate,
    order_two$replicates$method
  ),
  comparison_columns,
  drop = FALSE
]

rownames(first) <- NULL
rownames(second) <- NULL

order_match <- isTRUE(
  all.equal(
    first,
    second,
    tolerance = 1e-12,
    check.attributes = TRUE
  )
)

add_check(
  "study1_method_order_remains_invariant",
  order_match
)

message("6. Saving Phase 3E validation evidence...")

checks_df <- do.call(
  rbind,
  checks
)
rownames(checks_df) <- NULL

source_files <- c(
  study1_helpers =
    file.path(
      project_root,
      "R",
      "pwr_func_study1_helpers.R"
    ),
  study2_helpers =
    file.path(
      project_root,
      "R",
      "pwr_func_study2_helpers.R"
    ),
  phase3e_validator =
    file.path(
      project_root,
      "data-raw",
      "robust_cats_phase3e_a13_post_fix_validation.R"
    ),
  phase3e_test =
    file.path(
      project_root,
      "tests",
      "testthat",
      "test-study1-ri-convergence.R"
    )
)

source_checksums <- data.frame(
  source = names(source_files),
  path = unname(source_files),
  exists = file.exists(source_files),
  md5 = unname(
    tools::md5sum(
      source_files
    )
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  checks_df,
  file.path(
    output_dir,
    "phase3e_checks.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  singular_results,
  file.path(
    output_dir,
    "phase3e_singularity_results.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  source_checksums,
  file.path(
    output_dir,
    "phase3e_source_checksums.csv"
  ),
  row.names = FALSE
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

saveRDS(
  list(
    checks = checks_df,
    singularity_results =
      singular_results,
    source_checksums =
      source_checksums
  ),
  file.path(
    output_dir,
    "phase3e_results.rds"
  )
)

message("")
message("Phase 3E checks:")
print(
  checks_df,
  row.names = FALSE
)

message("")
message("Phase 3E boundary-singular result:")
print(
  singular_results,
  row.names = FALSE
)

failed <- checks_df[
  !(checks_df$passed %in% TRUE),
  ,
  drop = FALSE
]

if (nrow(failed) > 0L) {
  stop(
    paste(
      nrow(failed),
      "Phase 3E validation check(s) failed.",
      "Review the saved evidence before further changes."
    ),
    call. = FALSE
  )
}

message("")
message(
  "All Phase 3E A-13 validations passed."
)
