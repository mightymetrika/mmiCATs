# Study 2 random-slope numerical validation
#
# This script validates:
#   1. the algebra of the random-slope data-generating process;
#   2. empirical moments of the simulated predictor, random effects, and
#      residuals;
#   3. exact common-random-number rescaling across random-slope SD values;
#   4. the Study 1 vertical-contamination convention; and
#   5. numerical agreement between study2_fit_rs() and a direct
#      lmerTest Kenward-Roger calculation.
#
# Run this script after adding pwr_func_study2() and its helpers, running
# devtools::document(), and loading the package with devtools::load_all().

find_project_root <- function(path = getwd()) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  repeat {
    if (file.exists(file.path(path, "DESCRIPTION"))) {
      return(path)
    }

    parent <- dirname(path)

    if (identical(parent, path)) {
      stop(
        "Could not locate the mmiCATs project root.",
        call. = FALSE
      )
    }

    path <- parent
  }
}

validation_check <- function(name,
                             observed,
                             target,
                             tolerance,
                             comparison = c("absolute", "exact")) {
  comparison <- match.arg(comparison)

  difference <- if (comparison == "absolute") {
    abs(observed - target)
  } else {
    if (identical(observed, target)) 0 else Inf
  }

  data.frame(
    check = name,
    observed = if (length(observed) == 1L) {
      as.character(observed)
    } else {
      paste(observed, collapse = ",")
    },
    target = if (length(target) == 1L) {
      as.character(target)
    } else {
      paste(target, collapse = ",")
    },
    tolerance = tolerance,
    difference = difference,
    passed = is.finite(difference) && difference <= tolerance,
    stringsAsFactors = FALSE
  )
}

project_root <- find_project_root()

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop(
    "The pkgload package is required to run this validation script.",
    call. = FALSE
  )
}

if (!requireNamespace("pbkrtest", quietly = TRUE)) {
  stop(
    "The pbkrtest package is required for Kenward-Roger validation.",
    call. = FALSE
  )
}

pkgload::load_all(project_root, quiet = TRUE)

output_dir <- file.path(
  project_root,
  "data-raw",
  "study2-results",
  "numerical-validation"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

checks <- list()

# -------------------------------------------------------------------------
# 1. Empirical moment validation
# -------------------------------------------------------------------------

set.seed(20260820)

moment_dat <- study2_simulate_data(
  n_clusters = 5000,
  cluster_size = 10,
  beta = 0.10,
  intercept = 0,
  random_intercept_sd = 1,
  random_slope_sd = 0.10,
  residual_sd = 1,
  x_sd = 1,
  contamination = "none",
  contamination_prop = 0.05,
  contamination_size = 6
)

cluster_rows <- !duplicated(moment_dat$cluster)

moment_results <- data.frame(
  quantity = c(
    "x_mean",
    "x_sd",
    "random_intercept_mean",
    "random_intercept_sd",
    "random_slope_mean",
    "random_slope_sd",
    "intercept_slope_correlation",
    "residual_mean",
    "residual_sd",
    "true_cluster_slope_mean"
  ),
  observed = c(
    mean(moment_dat$x),
    stats::sd(moment_dat$x),
    mean(moment_dat$random_intercept[cluster_rows]),
    stats::sd(moment_dat$random_intercept[cluster_rows]),
    mean(moment_dat$random_slope[cluster_rows]),
    stats::sd(moment_dat$random_slope[cluster_rows]),
    stats::cor(
      moment_dat$random_intercept[cluster_rows],
      moment_dat$random_slope[cluster_rows]
    ),
    mean(moment_dat$residual),
    stats::sd(moment_dat$residual),
    mean(moment_dat$true_cluster_slope[cluster_rows])
  ),
  target = c(0, 1, 0, 1, 0, 0.10, 0, 0, 1, 0.10),
  tolerance = c(
    0.02,
    0.02,
    0.04,
    0.04,
    0.005,
    0.005,
    0.04,
    0.02,
    0.02,
    0.005
  ),
  stringsAsFactors = FALSE
)
moment_results$difference <- abs(
  moment_results$observed - moment_results$target
)
moment_results$passed <-
  moment_results$difference <= moment_results$tolerance

checks[[length(checks) + 1L]] <- data.frame(
  check = paste0("moment_", moment_results$quantity),
  observed = as.character(moment_results$observed),
  target = as.character(moment_results$target),
  tolerance = moment_results$tolerance,
  difference = moment_results$difference,
  passed = moment_results$passed,
  stringsAsFactors = FALSE
)

# -------------------------------------------------------------------------
# 2. Algebra and common-random-number validation
# -------------------------------------------------------------------------

common_settings <- list(
  n_clusters = 20,
  cluster_size = 40,
  beta = 0.10,
  intercept = 0.25,
  random_intercept_sd = 1,
  residual_sd = 1,
  x_sd = 1,
  contamination = "vertical",
  contamination_prop = 0.05,
  contamination_size = 6
)

set.seed(20260821)
low_slope <- do.call(
  study2_simulate_data,
  c(common_settings, list(random_slope_sd = 0.05))
)

set.seed(20260821)
high_slope <- do.call(
  study2_simulate_data,
  c(common_settings, list(random_slope_sd = 0.10))
)

reconstructed <- common_settings$intercept +
  low_slope$random_intercept +
  low_slope$true_cluster_slope * low_slope$x +
  low_slope$residual

checks[[length(checks) + 1L]] <- validation_check(
  "clean_outcome_algebra",
  max(abs(low_slope$out_clean - reconstructed)),
  0,
  1e-12
)
checks[[length(checks) + 1L]] <- validation_check(
  "common_x_draws",
  max(abs(low_slope$x - high_slope$x)),
  0,
  0
)
checks[[length(checks) + 1L]] <- validation_check(
  "common_random_intercept_draws",
  max(abs(
    low_slope$random_intercept -
      high_slope$random_intercept
  )),
  0,
  0
)
checks[[length(checks) + 1L]] <- validation_check(
  "common_residual_draws",
  max(abs(low_slope$residual - high_slope$residual)),
  0,
  0
)
checks[[length(checks) + 1L]] <- validation_check(
  "random_slope_rescaling",
  max(abs(
    high_slope$random_slope -
      2 * low_slope$random_slope
  )),
  0,
  1e-12
)
checks[[length(checks) + 1L]] <- validation_check(
  "common_contamination_locations",
  identical(
    low_slope$contaminated,
    high_slope$contaminated
  ),
  TRUE,
  0,
  comparison = "exact"
)

contaminated_by_cluster <- tapply(
  low_slope$contaminated,
  low_slope$cluster,
  sum
)
vertical_displacement <- abs(
  low_slope$out[low_slope$contaminated] -
    low_slope$out_clean[low_slope$contaminated]
)

checks[[length(checks) + 1L]] <- validation_check(
  "two_contaminated_per_cluster",
  max(abs(as.integer(contaminated_by_cluster) - 2L)),
  0,
  0
)
checks[[length(checks) + 1L]] <- validation_check(
  "vertical_displacement",
  max(abs(vertical_displacement - 6)),
  0,
  1e-12
)

# -------------------------------------------------------------------------
# 3. Direct Kenward-Roger numerical validation
# -------------------------------------------------------------------------

set.seed(20260822)

kr_dat <- study2_simulate_data(
  n_clusters = 30,
  cluster_size = 20,
  beta = 0.25,
  intercept = 0.40,
  random_intercept_sd = 0.80,
  random_slope_sd = 0.20,
  residual_sd = 0.70,
  x_sd = 1,
  contamination = "none",
  contamination_prop = 0.05,
  contamination_size = 6
)

direct_fit <- lmerTest::lmer(
  out ~ x + (1 + x || cluster),
  data = kr_dat,
  REML = TRUE
)
direct_summary <- summary(
  direct_fit,
  ddf = "Kenward-Roger"
)
direct_row <- stats::coef(
  direct_summary
)["x", , drop = FALSE]

direct_estimate <- unname(direct_row[1L, "Estimate"])
direct_se <- unname(direct_row[1L, "Std. Error"])
direct_df <- unname(direct_row[1L, "df"])
direct_p <- unname(direct_row[1L, "Pr(>|t|)"])
direct_critical <- stats::qt(
  1 - 0.05 / 2,
  df = direct_df
)
direct_low <- direct_estimate - direct_critical * direct_se
direct_high <- direct_estimate + direct_critical * direct_se
direct_sds <- study2_extract_random_effect_sds(direct_fit)

helper_result <- study2_fit_rs(
  dat = kr_dat,
  alpha = 0.05
)

kr_comparison <- data.frame(
  quantity = c(
    "estimate",
    "std_error",
    "df",
    "p_value",
    "conf_low",
    "conf_high",
    "random_intercept_sd",
    "random_slope_sd"
  ),
  helper = c(
    helper_result$estimate,
    helper_result$std_error,
    helper_result$df,
    helper_result$p_value,
    helper_result$conf_low,
    helper_result$conf_high,
    helper_result$estimated_random_intercept_sd,
    helper_result$estimated_random_slope_sd
  ),
  direct = c(
    direct_estimate,
    direct_se,
    direct_df,
    direct_p,
    direct_low,
    direct_high,
    unname(direct_sds["random_intercept_sd"]),
    unname(direct_sds["random_slope_sd"])
  ),
  stringsAsFactors = FALSE
)
kr_comparison$absolute_difference <- abs(
  kr_comparison$helper - kr_comparison$direct
)
kr_comparison$passed <-
  kr_comparison$absolute_difference <= 1e-8

checks[[length(checks) + 1L]] <- data.frame(
  check = paste0("kr_", kr_comparison$quantity),
  observed = as.character(kr_comparison$helper),
  target = as.character(kr_comparison$direct),
  tolerance = 1e-8,
  difference = kr_comparison$absolute_difference,
  passed = kr_comparison$passed,
  stringsAsFactors = FALSE
)

direct_convergence <- study2_classify_convergence(
  messages = direct_fit@optinfo$conv$lme4$messages,
  optimizer_code = direct_fit@optinfo$conv$opt
)

checks[[length(checks) + 1L]] <- validation_check(
  "kr_convergence_classification",
  helper_result$converged,
  direct_convergence$converged,
  0,
  comparison = "exact"
)
checks[[length(checks) + 1L]] <- validation_check(
  "kr_singularity_classification",
  helper_result$singular,
  lme4::isSingular(direct_fit, tol = 1e-4),
  0,
  comparison = "exact"
)

# -------------------------------------------------------------------------
# 4. Synthetic convergence-classification validation
# -------------------------------------------------------------------------

singular_message <- paste(
  "boundary (singular) fit:",
  "see help('isSingular')"
)
gradient_message <- paste(
  "Model failed to converge with max|grad| = 0.01",
  "(tol = 0.002, component 1)"
)

singular_only <- study2_classify_convergence(
  messages = singular_message,
  optimizer_code = 0L
)
genuine_failure <- study2_classify_convergence(
  messages = c(singular_message, gradient_message),
  optimizer_code = 0L
)
nonzero_code <- study2_classify_convergence(
  messages = singular_message,
  optimizer_code = 1L
)

checks[[length(checks) + 1L]] <- validation_check(
  "singular_message_remains_converged",
  singular_only$converged,
  TRUE,
  0,
  comparison = "exact"
)
checks[[length(checks) + 1L]] <- validation_check(
  "gradient_message_is_nonconverged",
  genuine_failure$converged,
  FALSE,
  0,
  comparison = "exact"
)
checks[[length(checks) + 1L]] <- validation_check(
  "nonzero_optimizer_code_is_nonconverged",
  nonzero_code$converged,
  FALSE,
  0,
  comparison = "exact"
)

usable_singular_result <- list(
  estimate = 0.10,
  std_error = 0.04,
  df = 8,
  p_value = 0.04,
  conf_low = 0.01,
  conf_high = 0.19,
  retained_clusters = 10,
  converged = TRUE,
  singular = TRUE
)

checks[[length(checks) + 1L]] <- validation_check(
  "singular_finite_result_is_usable",
  study2_result_is_usable(usable_singular_result),
  TRUE,
  0,
  comparison = "exact"
)

validation_results <- do.call(rbind, checks)
rownames(validation_results) <- NULL

utils::write.csv(
  moment_results,
  file.path(output_dir, "study2_moment_validation.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  kr_comparison,
  file.path(output_dir, "study2_kr_direct_comparison.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  validation_results,
  file.path(output_dir, "study2_numerical_validation_checks.csv"),
  row.names = FALSE,
  na = ""
)

saveRDS(
  list(
    moment_validation = moment_results,
    kr_comparison = kr_comparison,
    checks = validation_results,
    session_info = utils::sessionInfo()
  ),
  file.path(output_dir, "study2_numerical_validation.rds"),
  version = 3
)

writeLines(
  capture.output(utils::sessionInfo()),
  con = file.path(output_dir, "session_info.txt"),
  useBytes = TRUE
)

print(validation_results, row.names = FALSE)

if (!all(validation_results$passed)) {
  stop(
    "One or more Study 2 numerical validation checks failed.",
    call. = FALSE
  )
}

message("")
message("All Study 2 numerical validation checks passed.")
message(paste("Results saved to:", output_dir))
