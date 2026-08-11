# Robust CATs audit: Phase 3D end-to-end production-path verification
#
# Audit-only. Does not modify production code.

# Load the shared audit helpers before calling any rca_* helper.
audit_helper_path <- file.path(
  "data-raw",
  "robust_cats_audit_helpers.R"
)

if (!file.exists(audit_helper_path)) {
  stop(
    paste(
      "Could not find",
      audit_helper_path,
      "from the current working directory.",
      "Run this audit from the mmiCATs project root."
    ),
    call. = FALSE
  )
}

source(audit_helper_path)

project_root <- rca_find_project_root()
rca_require_packages()
pkgload::load_all(project_root, quiet = TRUE, export_all = TRUE)

output_dir <- file.path(
  project_root, "data-raw", "robust-cats-audit-results",
  "phase3d-end-to-end"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

p3d_equal <- function(x, y, tolerance = 1e-12) {
  isTRUE(all.equal(x, y, tolerance = tolerance, check.attributes = TRUE))
}

p3d_details <- function(x, y, tolerance = 1e-12) {
  out <- all.equal(x, y, tolerance = tolerance, check.attributes = TRUE)
  if (isTRUE(out)) "equal" else paste(as.character(out), collapse = " | ")
}

p3d_sort_rep <- function(x) {
  x <- x[order(x$replicate, x$method), , drop = FALSE]
  rownames(x) <- NULL
  x
}

p3d_sort_sum <- function(x) {
  x <- x[order(x$model), , drop = FALSE]
  rownames(x) <- NULL
  x
}

p3d_no_runtime <- function(x) {
  x[, setdiff(names(x), "runtime_sec"), drop = FALSE]
}

p3d_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) NA_real_ else mean(x)
}

p3d_rmse <- function(est, truth) {
  ok <- is.finite(est) & is.finite(truth)
  if (!any(ok)) NA_real_ else sqrt(mean((est[ok] - truth[ok])^2))
}

p3d_mcse <- function(p, n) {
  if (!is.finite(p) || n <= 0L) NA_real_ else sqrt(p * (1 - p) / n)
}

p3d_manual_summary <- function(rep_results, methods, reps) {
  out <- lapply(methods, function(method) {
    z <- rep_results[rep_results$method == method, , drop = FALSE]
    good <- z$fit_success %in% TRUE
    zg <- z[good, , drop = FALSE]
    n_success <- nrow(zg)
    rejection <- p3d_mean(zg$reject)
    coverage <- p3d_mean(zg$cover)
    singular <- zg$singular[!is.na(zg$singular)]
    data.frame(
      model = method,
      mean_coef = p3d_mean(zg$estimate),
      bias = p3d_mean(zg$estimate - zg$true_beta),
      rejection_rate = 100 * rejection,
      rejection_rate_se = 100 * p3d_mcse(rejection, n_success),
      rmse = p3d_rmse(zg$estimate, zg$true_beta),
      coverage = 100 * coverage,
      coverage_se = 100 * p3d_mcse(coverage, n_success),
      avg_ci_width = p3d_mean(zg$conf_high - zg$conf_low),
      success = n_success,
      failure_rate = 100 * (reps - n_success) / reps,
      singular_rate = if (length(singular) == 0L) NA_real_ else 100 * mean(singular),
      mean_retained_clusters = p3d_mean(zg$retained_clusters),
      mean_runtime_sec = p3d_mean(z$runtime_sec),
      stringsAsFactors = FALSE
    )
  })
  ans <- do.call(rbind, out)
  rownames(ans) <- NULL
  ans
}

p3d_with_binding <- function(name, replacement, code) {
  ns <- asNamespace("mmiCATs")
  original <- get(name, envir = ns, inherits = FALSE)
  was_locked <- bindingIsLocked(name, ns)
  set_binding <- function(value) {
    if (bindingIsLocked(name, ns)) unlockBinding(name, ns)
    assign(name, value, envir = ns)
    if (was_locked) lockBinding(name, ns)
  }
  set_binding(replacement)
  on.exit(set_binding(original), add = TRUE)
  force(code)
}

check_rows <- list()
add_check <- function(category, check, passed, required = TRUE,
                      issue_id = NA_character_, details = NA_character_) {
  check_rows[[length(check_rows) + 1L]] <<- data.frame(
    category = category, check = check, passed = as.logical(passed),
    required = as.logical(required), issue_id = issue_id,
    details = details, stringsAsFactors = FALSE
  )
}

study1_methods <- study1_method_names()
study2_methods <- study2_method_names()

s1_args <- list(
  n_clusters = 10L, cluster_size = 40L, beta = 0.10, intercept = 0,
  random_intercept_sd = 1, residual_sd = 1, x_sd = 1,
  contamination = "vertical", contamination_prop = 0.05,
  contamination_size = 6, leverage_size = 4, reps = 2L,
  alpha = 0.05, seed = 20267001L, keep_replicates = TRUE
)

s2_args <- list(
  n_clusters = 10L, cluster_size = 40L, beta = 0.10, intercept = 0,
  random_intercept_sd = 1, random_slope_sd = 0.05,
  residual_sd = 1, x_sd = 1, contamination = "vertical",
  contamination_prop = 0.05, contamination_size = 6, reps = 2L,
  alpha = 0.05, seed = 20267002L, keep_replicates = TRUE
)

run_s1 <- function(methods) suppressWarnings(
  do.call(pwr_func_study1, c(s1_args, list(methods = methods)))
)
run_s2 <- function(methods) suppressWarnings(
  do.call(pwr_func_study2, c(s2_args, list(methods = methods)))
)

message("1. Canonical runs, reproducibility, and method invariance...")

s1 <- run_s1(study1_methods)
s2 <- run_s2(study2_methods)
s1_repeat <- run_s1(study1_methods)
s2_repeat <- run_s2(study2_methods)
s1_reverse <- run_s1(rev(study1_methods))
s2_reverse <- run_s2(rev(study2_methods))
s1_subset_methods <- c("cats_robustbase", "cr2", "ri")
s2_subset_methods <- c("cats_robustbase", "cr2", "rs")
s1_subset <- run_s1(s1_subset_methods)
s2_subset <- run_s2(s2_subset_methods)

compare_rep <- function(a, b, methods = NULL) {
  if (!is.null(methods)) {
    a <- a[a$method %in% methods, , drop = FALSE]
    b <- b[b$method %in% methods, , drop = FALSE]
  }
  a <- p3d_no_runtime(p3d_sort_rep(a))
  b <- p3d_no_runtime(p3d_sort_rep(b))
  list(passed = p3d_equal(a, b), details = p3d_details(a, b))
}

x <- compare_rep(s1$replicates, s1_repeat$replicates)
add_check("Reproducibility", "study1_replicates_reproduce", x$passed, details = x$details)
x <- compare_rep(s2$replicates, s2_repeat$replicates)
add_check("Reproducibility", "study2_replicates_reproduce", x$passed, details = x$details)
x <- compare_rep(s1$replicates, s1_reverse$replicates)
add_check("Method invariance", "study1_method_order_invariance", x$passed, details = x$details)
x <- compare_rep(s2$replicates, s2_reverse$replicates)
add_check("Method invariance", "study2_method_order_invariance", x$passed, details = x$details)
x <- compare_rep(s1$replicates, s1_subset$replicates, s1_subset_methods)
add_check("Method invariance", "study1_method_subset_invariance", x$passed, details = x$details)
x <- compare_rep(s2$replicates, s2_subset$replicates, s2_subset_methods)
add_check("Method invariance", "study2_method_subset_invariance", x$passed, details = x$details)

add_check(
  "Seed plumbing", "replicate_seeds_independent_of_method_request",
  identical(s1$settings$replicate_seeds, s1_repeat$settings$replicate_seeds) &&
    identical(s1$settings$replicate_seeds, s1_reverse$settings$replicate_seeds) &&
    identical(s1$settings$replicate_seeds, s1_subset$settings$replicate_seeds) &&
    identical(s2$settings$replicate_seeds, s2_repeat$settings$replicate_seeds) &&
    identical(s2$settings$replicate_seeds, s2_reverse$settings$replicate_seeds) &&
    identical(s2$settings$replicate_seeds, s2_subset$settings$replicate_seeds)
)


message("2. Instrumenting data and method-specific seed plumbing...")

instrument_one <- function(study = c("study1", "study2"), seed) {
  study <- match.arg(study)
  env <- new.env(parent = emptyenv())
  env$first_dat <- NULL
  env$rows <- list()

  if (study == "study1") {
    original <- get("study1_fit_method", envir = asNamespace("mmiCATs"))
    set.seed(seed)
    env$replicate_seed <- sample.int(.Machine$integer.max, 1L, replace = FALSE)

    wrapper <- function(dat, method, beta, alpha, replicate_id, method_seed) {
      if (is.null(env$first_dat)) env$first_dat <- dat
      expected <- study1_method_seed(
        env$replicate_seed, match(method, study1_method_names())
      )
      env$rows[[length(env$rows) + 1L]] <- data.frame(
        method = method,
        same_data = identical(dat, env$first_dat),
        method_seed = method_seed,
        expected_seed = expected,
        seed_matches = identical(as.integer(method_seed), as.integer(expected)),
        stringsAsFactors = FALSE
      )
      original(dat, method, beta, alpha, replicate_id, method_seed)
    }

    p3d_with_binding("study1_fit_method", wrapper, {
      suppressWarnings(pwr_func_study1(
        n_clusters = 8L, cluster_size = 40L, beta = 0.10,
        contamination = "vertical", contamination_size = 6,
        reps = 1L, methods = c("cats_robustbase", "cr2", "cats"),
        seed = seed, keep_replicates = TRUE
      ))
    })
  } else {
    original <- get("study2_fit_method", envir = asNamespace("mmiCATs"))
    set.seed(seed)
    env$replicate_seed <- sample.int(.Machine$integer.max, 1L, replace = FALSE)

    wrapper <- function(dat, method, beta, alpha, replicate_id, method_seed,
                        realized_mean_slope, realized_random_slope_sd) {
      if (is.null(env$first_dat)) env$first_dat <- dat
      expected <- study2_method_seed(
        env$replicate_seed, match(method, study2_method_names())
      )
      env$rows[[length(env$rows) + 1L]] <- data.frame(
        method = method,
        same_data = identical(dat, env$first_dat),
        method_seed = method_seed,
        expected_seed = expected,
        seed_matches = identical(as.integer(method_seed), as.integer(expected)),
        stringsAsFactors = FALSE
      )
      original(
        dat, method, beta, alpha, replicate_id, method_seed,
        realized_mean_slope, realized_random_slope_sd
      )
    }

    p3d_with_binding("study2_fit_method", wrapper, {
      suppressWarnings(pwr_func_study2(
        n_clusters = 8L, cluster_size = 40L, beta = 0.10,
        random_slope_sd = 0.05, contamination = "vertical",
        contamination_size = 6, reps = 1L,
        methods = c("cats_robustbase", "cr2", "rs"),
        seed = seed, keep_replicates = TRUE
      ))
    })
  }

  out <- do.call(rbind, env$rows)
  rownames(out) <- NULL
  out
}

s1_instrument <- instrument_one("study1", 20267101L)
s2_instrument <- instrument_one("study2", 20267102L)

add_check(
  "Data plumbing", "study1_same_data_passed_to_all_methods",
  all(s1_instrument$same_data),
  details = paste(s1_instrument$method, s1_instrument$same_data,
                  sep = "=", collapse = "; ")
)
add_check(
  "Data plumbing", "study2_same_data_passed_to_all_methods",
  all(s2_instrument$same_data),
  details = paste(s2_instrument$method, s2_instrument$same_data,
                  sep = "=", collapse = "; ")
)
add_check(
  "Seed plumbing", "canonical_method_seed_assignment",
  all(s1_instrument$seed_matches) && all(s2_instrument$seed_matches),
  details = paste(
    "Study1", paste(s1_instrument$seed_matches, collapse = ","),
    "Study2", paste(s2_instrument$seed_matches, collapse = ",")
  )
)

message("3. Reconstructing top-level replicate records directly...")

s1_recon <- list()
k <- 0L
for (replicate_id in seq_len(s1_args$reps)) {
  rep_seed <- s1$settings$replicate_seeds[replicate_id]
  set.seed(rep_seed)
  dat <- study1_simulate_data(
    n_clusters = s1_args$n_clusters,
    cluster_size = s1_args$cluster_size,
    beta = s1_args$beta,
    intercept = s1_args$intercept,
    random_intercept_sd = s1_args$random_intercept_sd,
    residual_sd = s1_args$residual_sd,
    x_sd = s1_args$x_sd,
    contamination = s1_args$contamination,
    contamination_prop = s1_args$contamination_prop,
    contamination_size = s1_args$contamination_size,
    leverage_size = s1_args$leverage_size
  )
  for (method in study1_methods) {
    method_seed <- study1_method_seed(
      rep_seed, match(method, study1_method_names())
    )
    direct <- suppressWarnings(study1_fit_method(
      dat, method, s1_args$beta, s1_args$alpha,
      replicate_id, method_seed
    ))
    stored <- s1$replicates[
      s1$replicates$replicate == replicate_id &
        s1$replicates$method == method, , drop = FALSE
    ]
    direct <- p3d_no_runtime(direct)
    stored <- p3d_no_runtime(stored)
    rownames(direct) <- rownames(stored) <- NULL
    k <- k + 1L
    s1_recon[[k]] <- data.frame(
      replicate = replicate_id, method = method,
      matched = p3d_equal(stored, direct),
      details = p3d_details(stored, direct),
      stringsAsFactors = FALSE
    )
  }
}
s1_recon <- do.call(rbind, s1_recon)

s2_recon <- list()
k <- 0L
for (replicate_id in seq_len(s2_args$reps)) {
  rep_seed <- s2$settings$replicate_seeds[replicate_id]
  set.seed(rep_seed)
  dat <- study2_simulate_data(
    n_clusters = s2_args$n_clusters,
    cluster_size = s2_args$cluster_size,
    beta = s2_args$beta,
    intercept = s2_args$intercept,
    random_intercept_sd = s2_args$random_intercept_sd,
    random_slope_sd = s2_args$random_slope_sd,
    residual_sd = s2_args$residual_sd,
    x_sd = s2_args$x_sd,
    contamination = s2_args$contamination,
    contamination_prop = s2_args$contamination_prop,
    contamination_size = s2_args$contamination_size
  )
  realized_mean <- mean(dat$true_cluster_slope[!duplicated(dat$cluster)])
  realized_sd <- stats::sd(dat$random_slope[!duplicated(dat$cluster)])

  for (method in study2_methods) {
    method_seed <- study2_method_seed(
      rep_seed, match(method, study2_method_names())
    )
    direct <- suppressWarnings(study2_fit_method(
      dat, method, s2_args$beta, s2_args$alpha,
      replicate_id, method_seed, realized_mean, realized_sd
    ))
    stored <- s2$replicates[
      s2$replicates$replicate == replicate_id &
        s2$replicates$method == method, , drop = FALSE
    ]
    direct <- p3d_no_runtime(direct)
    stored <- p3d_no_runtime(stored)
    rownames(direct) <- rownames(stored) <- NULL
    k <- k + 1L
    s2_recon[[k]] <- data.frame(
      replicate = replicate_id, method = method,
      matched = p3d_equal(stored, direct),
      details = p3d_details(stored, direct),
      stringsAsFactors = FALSE
    )
  }
}
s2_recon <- do.call(rbind, s2_recon)

add_check(
  "Direct reconstruction", "study1_top_level_records_reconstruct",
  all(s1_recon$matched),
  details = paste(sum(s1_recon$matched), "of", nrow(s1_recon), "matched")
)
add_check(
  "Direct reconstruction", "study2_top_level_records_reconstruct",
  all(s2_recon$matched),
  details = paste(sum(s2_recon$matched), "of", nrow(s2_recon), "matched")
)


message("4. Independently reconstructing summaries and testing denominators...")

s1_manual <- p3d_manual_summary(s1$replicates, study1_methods, s1_args$reps)
s2_manual <- p3d_manual_summary(s2$replicates, study2_methods, s2_args$reps)

s1_sum_ref <- p3d_sort_sum(s1$summary)
s1_sum_obs <- p3d_sort_sum(s1_manual)
s2_sum_ref <- p3d_sort_sum(s2$summary)
s2_sum_obs <- p3d_sort_sum(s2_manual)

add_check(
  "Summary reconstruction", "study1_summary_reconstructs",
  p3d_equal(s1_sum_ref, s1_sum_obs),
  details = p3d_details(s1_sum_ref, s1_sum_obs)
)
add_check(
  "Summary reconstruction", "study2_summary_reconstructs",
  p3d_equal(s2_sum_ref, s2_sum_obs),
  details = p3d_details(s2_sum_ref, s2_sum_obs)
)

synthetic <- data.frame(
  replicate = 1:3, method = rep("synthetic", 3), true_beta = rep(0, 3),
  estimate = c(0.10, 0.20, 999), std_error = c(0.05, 0.05, NA),
  df = c(8, 8, NA), p_value = c(0.01, 0.50, NA),
  conf_low = c(0.01, 0.05, NA), conf_high = c(0.19, 0.35, NA),
  reject = c(TRUE, FALSE, NA), cover = c(TRUE, TRUE, NA),
  fit_success = c(TRUE, TRUE, FALSE), converged = c(TRUE, TRUE, FALSE),
  singular = c(FALSE, TRUE, NA), retained_clusters = c(10, 10, NA),
  warning = c(NA, NA, NA), error = c(NA, NA, "Synthetic failure"),
  runtime_sec = c(1, 2, 3), stringsAsFactors = FALSE
)
synthetic_sum <- study1_summarize_results(synthetic, "synthetic", 3L)

denominator_ok <- isTRUE(all.equal(synthetic_sum$rejection_rate, 50)) &&
  isTRUE(all.equal(synthetic_sum$coverage, 100)) &&
  identical(synthetic_sum$success, 2L) &&
  isTRUE(all.equal(synthetic_sum$failure_rate, 100 / 3)) &&
  isTRUE(all.equal(synthetic_sum$mean_coef, 0.15))

mutated_rate <- 100 * sum(synthetic$reject %in% TRUE) / 3
mutation_detected <- abs(synthetic_sum$rejection_rate - mutated_rate) > 1

add_check(
  "Summary mutation", "successful_replication_denominator_is_used",
  denominator_ok,
  details = paste(
    "rejection", synthetic_sum$rejection_rate,
    "coverage", synthetic_sum$coverage,
    "success", synthetic_sum$success,
    "failure", synthetic_sum$failure_rate
  )
)
add_check(
  "Summary mutation", "wrong_total_replication_denominator_is_detected",
  mutation_detected,
  details = paste("correct", synthetic_sum$rejection_rate,
                  "mutated", mutated_rate)
)

message("5. Testing failure propagation through method dispatch...")

synthetic_cats_failure <- function(dat, alpha, truncate) {
  stop("Synthetic dispatch failure.", call. = FALSE)
}

set.seed(20267201L)
failure_dat1 <- study1_simulate_data(
  6L, 20L, 0.10, 0, 1, 1, 1, "none", 0.05, 6, 4
)
failure1 <- p3d_with_binding("study1_fit_cats", synthetic_cats_failure, {
  study1_fit_method(
    failure_dat1, "cats", 0.10, 0.05, 1L, 20267211L
  )
})

set.seed(20267202L)
failure_dat2 <- study2_simulate_data(
  6L, 20L, 0.10, 0, 1, 0.05, 1, 1, "none", 0.05, 6
)
failure_mean2 <- mean(
  failure_dat2$true_cluster_slope[!duplicated(failure_dat2$cluster)]
)
failure_sd2 <- stats::sd(
  failure_dat2$random_slope[!duplicated(failure_dat2$cluster)]
)
failure2 <- p3d_with_binding("study1_fit_cats", synthetic_cats_failure, {
  study2_fit_method(
    failure_dat2, "cats", 0.10, 0.05, 1L, 20267212L,
    failure_mean2, failure_sd2
  )
})

failure_table <- data.frame(
  study = c("Study 1", "Study 2"),
  fit_success = c(failure1$fit_success, failure2$fit_success),
  estimate_missing = c(is.na(failure1$estimate), is.na(failure2$estimate)),
  error_recorded = c(
    grepl("Synthetic dispatch failure", failure1$error, fixed = TRUE),
    grepl("Synthetic dispatch failure", failure2$error, fixed = TRUE)
  ),
  stringsAsFactors = FALSE
)
add_check(
  "Failure propagation", "method_failure_becomes_failed_replicate_record",
  all(!failure_table$fit_success) &&
    all(failure_table$estimate_missing) &&
    all(failure_table$error_recorded),
  details = paste(failure_table$study, failure_table$error_recorded,
                  sep = "=", collapse = "; ")
)

message("6. Comparing Study 1 and Study 2 shared estimators on identical data...")

set.seed(20267301L)
shared_dat <- study2_simulate_data(
  10L, 40L, 0.10, 0, 1, 0.05, 1, 1, "vertical", 0.05, 6
)
shared_mean <- mean(
  shared_dat$true_cluster_slope[!duplicated(shared_dat$cluster)]
)
shared_sd <- stats::sd(
  shared_dat$random_slope[!duplicated(shared_dat$cluster)]
)
shared_methods <- intersect(study1_method_names(), study2_method_names())
shared_rows <- lapply(seq_along(shared_methods), function(i) {
  method <- shared_methods[i]
  common_seed <- as.integer(20267310L + i)
  a <- suppressWarnings(study1_fit_method(
    shared_dat, method, 0.10, 0.05, 1L, common_seed
  ))
  b <- suppressWarnings(study2_fit_method(
    shared_dat, method, 0.10, 0.05, 1L, common_seed, shared_mean, shared_sd
  ))
  excluded <- c(
    "runtime_sec", "realized_mean_slope", "realized_random_slope_sd",
    "estimated_random_intercept_sd", "estimated_random_slope_sd",
    "optimizer_warning", "optimizer_code"
  )
  common <- setdiff(intersect(names(a), names(b)), excluded)
  aa <- a[, common, drop = FALSE]
  bb <- b[, common, drop = FALSE]
  rownames(aa) <- rownames(bb) <- NULL
  data.frame(
    method = method,
    matched = p3d_equal(aa, bb, 1e-10),
    details = p3d_details(aa, bb, 1e-10),
    stringsAsFactors = FALSE
  )
})
shared_results <- do.call(rbind, shared_rows)

add_check(
  "Shared estimator dispatch", "study1_study2_shared_methods_match",
  all(shared_results$matched),
  details = paste(shared_results$method, shared_results$matched,
                  sep = "=", collapse = "; ")
)

message("7. Checking seed-mutation sensitivity...")

mut_methods <- rev(study1_method_names())
seed_mutation <- do.call(rbind, lapply(seq_along(mut_methods), function(pos) {
  method <- mut_methods[pos]
  canonical <- study1_method_seed(
    20267401L, match(method, study1_method_names())
  )
  positional <- study1_method_seed(20267401L, pos)
  data.frame(
    method = method, requested_position = pos,
    canonical_seed = canonical, positional_seed = positional,
    mutation_detected = canonical != positional,
    stringsAsFactors = FALSE
  )
}))
add_check(
  "Seed mutation", "positional_seed_mutation_is_detectable",
  any(seed_mutation$mutation_detected),
  details = paste(sum(seed_mutation$mutation_detected), "of",
                  nrow(seed_mutation), "seeds differ")
)

message("8. Auditing mixed-model singularity/convergence classification...")

# Construct identical clusters, which should drive the RI variance to the boundary.
ncl <- 10L
m <- 20L
xpat <- seq(-2, 2, length.out = m)
epat <- 0.25 * sin(seq_len(m))
singular_dat <- data.frame(
  cluster = factor(rep(seq_len(ncl), each = m)),
  x = rep(xpat, times = ncl)
)
singular_dat$out <- 0.10 + 0.25 * singular_dat$x + rep(epat, times = ncl)

singular_capture <- study1_capture_fit(function() {
  study1_fit_ri(singular_dat, 0.05)
})
singular_result <- singular_capture$value
case_source <- "identical-cluster construction"

finite_singular <- !is.null(singular_result) &&
  isTRUE(singular_result$singular) &&
  all(is.finite(c(
    singular_result$estimate, singular_result$std_error, singular_result$df,
    singular_result$p_value, singular_result$conf_low,
    singular_result$conf_high, singular_result$retained_clusters
  )))

# Fallback search in case the installed lme4 version does not flag the construction.
if (!finite_singular) {
  singular_result <- NULL
  for (j in seq_len(100L)) {
    set.seed(20267500L + j)
    cl <- factor(rep(seq_len(10L), each = 30L))
    xx <- stats::rnorm(length(cl))
    yy <- 0.25 * xx + stats::rnorm(length(cl))
    candidate <- data.frame(cluster = cl, x = xx, out = yy)
    cap <- study1_capture_fit(function() study1_fit_ri(candidate, 0.05))
    rr <- cap$value
    ok <- !is.null(rr) && isTRUE(rr$singular) &&
      all(is.finite(c(rr$estimate, rr$std_error, rr$df, rr$p_value,
                      rr$conf_low, rr$conf_high, rr$retained_clusters)))
    if (ok) {
      singular_result <- rr
      singular_capture <- cap
      finite_singular <- TRUE
      case_source <- paste("fallback seed", 20267500L + j)
      break
    }
  }
}

ri_source <- paste(deparse(body(study1_fit_ri)), collapse = "\n")
uses_separate_classifier <- grepl(
  "study2_classify_convergence", ri_source, fixed = TRUE
)
uses_any_message_rule <- grepl(
  "is.null(convergence_messages)", ri_source, fixed = TRUE
)
s2_singular_semantics <- study2_classify_convergence(
  paste("boundary (singular) fit:", "see help('isSingular')"), 0L
)

a13_reproduced <- (
  finite_singular && !isTRUE(singular_result$converged)
) || (!uses_separate_classifier && uses_any_message_rule)

singularity_results <- data.frame(
  finite_singular_case_found = finite_singular,
  case_source = case_source,
  study1_converged = if (finite_singular) singular_result$converged else NA,
  study1_singular = if (finite_singular) singular_result$singular else NA,
  captured_warning = singular_capture$warning,
  uses_separate_classifier = uses_separate_classifier,
  uses_any_message_rule = uses_any_message_rule,
  study2_singularity_only_converged = s2_singular_semantics$converged,
  a13_reproduced = a13_reproduced,
  stringsAsFactors = FALSE
)

add_check(
  "Mixed-model convergence", "study2_singularity_only_is_nonfatal",
  isTRUE(s2_singular_semantics$converged),
  details = paste("converged", s2_singular_semantics$converged)
)
add_check(
  "Mixed-model convergence", "study1_finite_singular_ri_is_nonfatal",
  !a13_reproduced, required = TRUE, issue_id = "A-13",
  details = paste(
    "finite singular", finite_singular,
    "Study1 converged", if (finite_singular) singular_result$converged else NA,
    "separate classifier", uses_separate_classifier,
    "any-message rule", uses_any_message_rule
  )
)


message("9. Running short frozen-DGP pilots...")

s1_design <- expand.grid(
  beta = c(0, 0.10),
  contamination = c("none", "vertical", "bad_leverage"),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

s1_pilot <- do.call(rbind, lapply(seq_len(nrow(s1_design)), function(i) {
  d <- s1_design[i, , drop = FALSE]
  csize <- if (d$contamination == "bad_leverage") 0.375 else 6
  result <- suppressWarnings(pwr_func_study1(
    n_clusters = 10L, cluster_size = 40L, beta = d$beta,
    intercept = 0, random_intercept_sd = 1, residual_sd = 1, x_sd = 1,
    contamination = d$contamination, contamination_prop = 0.05,
    contamination_size = csize, leverage_size = 4,
    reps = 1L, alpha = 0.05, methods = study1_method_names(),
    seed = as.integer(20267600L + i), keep_replicates = TRUE
  ))
  good <- result$replicates$fit_success %in% TRUE
  complete <- if (!any(good)) TRUE else all(apply(
    result$replicates[good, c(
      "estimate", "std_error", "df", "p_value",
      "conf_low", "conf_high", "retained_clusters"
    ), drop = FALSE],
    1L, function(row) all(is.finite(as.numeric(row)))
  ))
  data.frame(
    study = "Study 1", condition_id = i, beta = d$beta,
    contamination = d$contamination, random_slope_sd = NA_real_,
    expected_rows = length(study1_method_names()),
    observed_rows = nrow(result$replicates),
    successful_fits = sum(good), failed_fits = sum(!good),
    successful_rows_complete = complete,
    stringsAsFactors = FALSE
  )
}))

s2_design <- expand.grid(
  beta = c(0, 0.10),
  random_slope_sd = c(0.05, 0.10),
  contamination = c("none", "vertical"),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

s2_pilot <- do.call(rbind, lapply(seq_len(nrow(s2_design)), function(i) {
  d <- s2_design[i, , drop = FALSE]
  result <- suppressWarnings(pwr_func_study2(
    n_clusters = 10L, cluster_size = 40L, beta = d$beta,
    intercept = 0, random_intercept_sd = 1,
    random_slope_sd = d$random_slope_sd, residual_sd = 1, x_sd = 1,
    contamination = d$contamination, contamination_prop = 0.05,
    contamination_size = 6, reps = 1L, alpha = 0.05,
    methods = study2_method_names(),
    seed = as.integer(20267700L + i), keep_replicates = TRUE
  ))
  good <- result$replicates$fit_success %in% TRUE
  complete <- if (!any(good)) TRUE else all(apply(
    result$replicates[good, c(
      "estimate", "std_error", "df", "p_value",
      "conf_low", "conf_high", "retained_clusters"
    ), drop = FALSE],
    1L, function(row) all(is.finite(as.numeric(row)))
  ))
  data.frame(
    study = "Study 2", condition_id = i, beta = d$beta,
    contamination = d$contamination, random_slope_sd = d$random_slope_sd,
    expected_rows = length(study2_method_names()),
    observed_rows = nrow(result$replicates),
    successful_fits = sum(good), failed_fits = sum(!good),
    successful_rows_complete = complete,
    stringsAsFactors = FALSE
  )
}))

pilot_results <- rbind(s1_pilot, s2_pilot)

add_check(
  "Frozen-DGP pilot", "pilot_method_row_counts_complete",
  all(pilot_results$observed_rows == pilot_results$expected_rows),
  details = paste(
    sum(pilot_results$observed_rows == pilot_results$expected_rows),
    "of", nrow(pilot_results), "conditions"
  )
)
add_check(
  "Frozen-DGP pilot", "successful_pilot_rows_are_finite",
  all(pilot_results$successful_rows_complete),
  details = paste(
    "successful", sum(pilot_results$successful_fits),
    "failed", sum(pilot_results$failed_fits)
  )
)

message("10. Saving Phase 3D evidence...")

checks <- do.call(rbind, check_rows)
rownames(checks) <- NULL

issue_register <- data.frame(
  issue_id = "A-13",
  issue = paste(
    "Study 1 random-intercept convergence classification treats a",
    "boundary-singularity message as a convergence failure rather than",
    "a separate nonfatal singularity diagnostic."
  ),
  reproduced = a13_reproduced,
  potentially_result_altering = TRUE,
  disposition = if (a13_reproduced) {
    paste(
      "Reproduced or source-confirmed. Apply a minimal Study 1 RI",
      "convergence-classification correction and preservation tests",
      "before the definitive simulation freeze."
    )
  } else {
    "Not reproduced and no incompatible source rule detected."
  },
  stringsAsFactors = FALSE
)

source_files <- c(
  phase3d_audit = file.path(
    project_root, "data-raw", "robust_cats_phase3d_end_to_end_audit.R"
  ),
  audit_helpers = file.path(
    project_root, "data-raw", "robust_cats_audit_helpers.R"
  ),
  study1 = file.path(project_root, "R", "pwr_func_study1.R"),
  study1_helpers = file.path(project_root, "R", "pwr_func_study1_helpers.R"),
  study2 = file.path(project_root, "R", "pwr_func_study2.R"),
  study2_helpers = file.path(project_root, "R", "pwr_func_study2_helpers.R"),
  robust_helpers = file.path(project_root, "R", "helpers_cimrob.R")
)
source_checksums <- rca_source_checksums(source_files)

package_names <- c(
  "mmiCATs", "clusterSEs", "robust", "robustbase",
  "lme4", "lmerTest", "pbkrtest", "clubSandwich", "testthat"
)
package_versions <- data.frame(
  package = package_names,
  version = vapply(package_names, function(x) {
    if (requireNamespace(x, quietly = TRUE)) {
      as.character(utils::packageVersion(x))
    } else {
      NA_character_
    }
  }, character(1)),
  stringsAsFactors = FALSE
)

write.csv(checks, file.path(output_dir, "phase3d_checks.csv"), row.names = FALSE)
write.csv(issue_register, file.path(output_dir, "phase3d_issue_register.csv"), row.names = FALSE)
write.csv(s1_instrument, file.path(output_dir, "phase3d_study1_data_seed_instrumentation.csv"), row.names = FALSE)
write.csv(s2_instrument, file.path(output_dir, "phase3d_study2_data_seed_instrumentation.csv"), row.names = FALSE)
write.csv(s1_recon, file.path(output_dir, "phase3d_study1_reconstruction.csv"), row.names = FALSE)
write.csv(s2_recon, file.path(output_dir, "phase3d_study2_reconstruction.csv"), row.names = FALSE)
write.csv(s1_manual, file.path(output_dir, "phase3d_study1_manual_summary.csv"), row.names = FALSE)
write.csv(s2_manual, file.path(output_dir, "phase3d_study2_manual_summary.csv"), row.names = FALSE)
write.csv(failure_table, file.path(output_dir, "phase3d_failure_propagation.csv"), row.names = FALSE)
write.csv(shared_results, file.path(output_dir, "phase3d_shared_method_consistency.csv"), row.names = FALSE)
write.csv(seed_mutation, file.path(output_dir, "phase3d_seed_mutation.csv"), row.names = FALSE)
write.csv(singularity_results, file.path(output_dir, "phase3d_singularity_classification.csv"), row.names = FALSE)
write.csv(pilot_results, file.path(output_dir, "phase3d_frozen_dgp_pilot.csv"), row.names = FALSE)
write.csv(source_checksums, file.path(output_dir, "phase3d_source_checksums.csv"), row.names = FALSE)
write.csv(package_versions, file.path(output_dir, "phase3d_package_versions.csv"), row.names = FALSE)

results <- list(
  checks = checks,
  issue_register = issue_register,
  study1_instrument = s1_instrument,
  study2_instrument = s2_instrument,
  study1_reconstruction = s1_recon,
  study2_reconstruction = s2_recon,
  study1_manual_summary = s1_manual,
  study2_manual_summary = s2_manual,
  failure_propagation = failure_table,
  shared_method_consistency = shared_results,
  seed_mutation = seed_mutation,
  singularity_classification = singularity_results,
  frozen_dgp_pilot = pilot_results,
  source_checksums = source_checksums,
  package_versions = package_versions
)
saveRDS(results, file.path(output_dir, "phase3d_results.rds"))

writeLines(
  capture.output(utils::sessionInfo()),
  file.path(output_dir, "session_info.txt"),
  useBytes = TRUE
)

required_failures <- checks[
  checks$required %in% TRUE & !(checks$passed %in% TRUE), ,
  drop = FALSE
]

summary_lines <- c(
  "Robust CATs Phase 3D end-to-end audit",
  "",
  paste(
    "Required checks passed:",
    sum(checks$required %in% TRUE & checks$passed %in% TRUE),
    "of", sum(checks$required %in% TRUE)
  ),
  paste("Unresolved required checks:", nrow(required_failures)),
  paste("A-13 reproduced:", a13_reproduced),
  paste("Study 1 direct records:", sum(s1_recon$matched), "of", nrow(s1_recon)),
  paste("Study 2 direct records:", sum(s2_recon$matched), "of", nrow(s2_recon)),
  paste(
    "Pilot successful fits:", sum(pilot_results$successful_fits),
    "of", sum(pilot_results$observed_rows)
  )
)
writeLines(summary_lines, file.path(output_dir, "phase3d_summary.txt"), useBytes = TRUE)

message("")
message("Phase 3D checks:")
print(checks, row.names = FALSE)
message("")
message("Phase 3D singularity classification:")
print(singularity_results, row.names = FALSE)
message("")
message("Phase 3D issue register:")
print(issue_register, row.names = FALSE)
message("")
message("Phase 3D frozen-DGP pilot:")
print(pilot_results, row.names = FALSE)
message("")
message(paste(
  "Required checks passed:",
  sum(checks$required %in% TRUE & checks$passed %in% TRUE),
  "of", sum(checks$required %in% TRUE)
))
message(paste("Results saved to:", output_dir))

if (nrow(required_failures) > 0L) {
  stop(
    paste(
      nrow(required_failures),
      "required Phase 3D check(s) remain unresolved.",
      "Evidence has been saved; review before changing production code."
    ),
    call. = FALSE
  )
}

message("")
message("All Phase 3D end-to-end verification gates passed.")
