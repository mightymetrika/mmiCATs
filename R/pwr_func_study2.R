#' Simulation Study of Robust Cluster-Adjusted t Statistics with Random Slopes
#'
#' Runs one condition from Study 2, which extends Study 1 by generating genuine
#' cluster-to-cluster slope heterogeneity. The comparison includes a correctly
#' specified independent random-intercept and random-slope model, the Study 1
#' random-intercept benchmark, robust random-intercept and independent
#' random-slope mixed models, ordinary least squares with CR2 inference,
#' ordinary cluster-adjusted t statistics (CATs), truncated CATs, and robust
#' CATs based on either `robust::lmRob()` or `robustbase::lmrob()`.
#'
#' Data are generated from a random-intercept and random-slope model with zero
#' intercept-slope covariance. Optional symmetric vertical contamination is
#' applied within every cluster after the clean data are generated. All
#' requested methods are fit to the same data in each replication. The `"rs"`
#' and `"ri"` methods use Kenward-Roger inference and require the suggested
#' package `pbkrtest`. The `"robust_ri"` and `"robust_rs"` methods use robust
#' Satterthwaite inference and require the suggested package `robustlmm`.
#'
#' @param n_clusters Integer number of clusters.
#' @param cluster_size Integer number of observations in each cluster.
#' @param beta Numeric value of the population mean slope for `x`.
#' @param intercept Numeric fixed intercept.
#' @param random_intercept_sd Positive standard deviation of the random
#'   intercept.
#' @param random_slope_sd Positive standard deviation of the random slope.
#' @param residual_sd Positive standard deviation of the residual error.
#' @param x_sd Positive standard deviation of the normally distributed
#'   predictor.
#' @param contamination Character string specifying the contamination condition:
#'   `"none"` or `"vertical"`.
#' @param contamination_prop Numeric proportion of observations contaminated
#'   within each cluster. The number contaminated is rounded to the nearest
#'   whole observation, with at least one observation selected when the
#'   proportion is greater than zero.
#' @param contamination_size Positive size of the outcome contamination in
#'   residual standard deviation units.
#' @param reps Integer number of simulation replications.
#' @param alpha Numeric significance level.
#' @param methods Character vector of methods to fit. Available values are
#'   `"rs"`, `"ri"`, `"cr2"`, `"cats"`, `"cats_trunc"`, `"cats_robust"`,
#'   `"cats_robustbase"`, `"robust_ri"`, and `"robust_rs"`.
#' @param seed Optional non-negative integer random-number seed.
#' @param keep_replicates Logical; if `TRUE`, retain the replicate-level results.
#' @param replicate_seeds Optional integer vector of length `reps` giving the
#'   exact random-number seed for each replication. This is intended for
#'   deterministic sharded execution. When supplied, `seed` must be `NULL`.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{summary}{A data frame containing method-level simulation results.}
#'   \item{replicates}{A data frame containing replicate-level results when
#'     `keep_replicates = TRUE`; otherwise `NULL`.}
#'   \item{settings}{A list containing the simulation settings and replication
#'     seeds.}
#' }
#'
#' Rejection rates, coverage rates, failure rates, and singularity rates are
#' reported as percentages. Their Monte Carlo standard errors are reported in
#' percentage points. The primary estimand is the superpopulation mean slope
#' `beta`. The realized mean of the sampled cluster slopes is retained at the
#' replication level as a diagnostic but is not used as the simulation truth.
#'
#' @references
#' Esarey, J., and Menger, A. (2019). Practical and effective approaches to
#' dealing with clustered data. *Political Science Research and Methods*, 7(3),
#' 541-559. \doi{10.1017/psrm.2017.42}
#'
#' Pustejovsky, J. E., and Tipton, E. (2018). Small-sample methods for
#' cluster-robust variance estimation and hypothesis testing in fixed effects
#' models. *Journal of Business and Economic Statistics*, 36(4), 672-683.
#' \doi{10.1080/07350015.2016.1247004}
#'
#' @examples
#' pwr_func_study2(
#'   n_clusters = 6,
#'   cluster_size = 20,
#'   random_slope_sd = 0.10,
#'   reps = 1,
#'   methods = c("rs", "ri"),
#'   seed = 123
#' )
#'
#' @export
pwr_func_study2 <- function(
    n_clusters = 20,
    cluster_size = 40,
    beta = 0,
    intercept = 0,
    random_intercept_sd = 1,
    random_slope_sd = 0.05,
    residual_sd = 1,
    x_sd = 1,
    contamination = c("none", "vertical"),
    contamination_prop = 0.05,
    contamination_size = 6,
    reps = 1000,
    alpha = 0.05,
    methods = c(
      "rs",
      "ri",
      "cr2",
      "cats",
      "cats_trunc",
      "cats_robust",
      "cats_robustbase",
      "robust_ri",
      "robust_rs"
    ),
    seed = NULL,
    keep_replicates = FALSE,
    replicate_seeds = NULL) {
  contamination <- match.arg(contamination)

  study2_validate_inputs(
    n_clusters = n_clusters,
    cluster_size = cluster_size,
    beta = beta,
    intercept = intercept,
    random_intercept_sd = random_intercept_sd,
    random_slope_sd = random_slope_sd,
    residual_sd = residual_sd,
    x_sd = x_sd,
    contamination = contamination,
    contamination_prop = contamination_prop,
    contamination_size = contamination_size,
    reps = reps,
    alpha = alpha,
    methods = methods,
    seed = seed,
    keep_replicates = keep_replicates
  )

  replicate_seeds <- study_validate_replicate_seeds(
    replicate_seeds = replicate_seeds,
    reps = reps
  )

  if (!is.null(seed) && !is.null(replicate_seeds)) {
    stop(
      "seed must be NULL when replicate_seeds is supplied.",
      call. = FALSE
    )
  }

  if (is.null(replicate_seeds)) {
    if (!is.null(seed)) {
      set.seed(seed)
    }

    replicate_seeds <- sample.int(
      .Machine$integer.max,
      size = reps,
      replace = FALSE
    )
  }

  replicate_results <- lapply(seq_len(reps), function(replicate_id) {
    replicate_seed <- replicate_seeds[replicate_id]
    set.seed(replicate_seed)

    dat <- study2_simulate_data(
      n_clusters = n_clusters,
      cluster_size = cluster_size,
      beta = beta,
      intercept = intercept,
      random_intercept_sd = random_intercept_sd,
      random_slope_sd = random_slope_sd,
      residual_sd = residual_sd,
      x_sd = x_sd,
      contamination = contamination,
      contamination_prop = contamination_prop,
      contamination_size = contamination_size
    )

    realized_mean_slope <- mean(
      dat$true_cluster_slope[
        !duplicated(dat$cluster)
      ]
    )
    realized_random_slope_sd <- stats::sd(
      dat$random_slope[
        !duplicated(dat$cluster)
      ]
    )

    method_results <- lapply(seq_along(methods), function(method_index) {
      study2_fit_method(
        dat = dat,
        method = methods[method_index],
        beta = beta,
        alpha = alpha,
        replicate_id = replicate_id,
        method_seed = study2_method_seed(
          replicate_seed = replicate_seed,
          method_index = match(
            methods[method_index],
            study2_method_names()
          )
        ),
        realized_mean_slope = realized_mean_slope,
        realized_random_slope_sd = realized_random_slope_sd
      )
    })

    do.call(rbind, method_results)
  })

  replicate_results <- do.call(rbind, replicate_results)
  rownames(replicate_results) <- NULL

  summary_results <- study1_summarize_results(
    replicate_results = replicate_results,
    methods = methods,
    reps = reps
  )
  rownames(summary_results) <- NULL

  settings <- list(
    n_clusters = n_clusters,
    cluster_size = cluster_size,
    beta = beta,
    intercept = intercept,
    random_intercept_sd = random_intercept_sd,
    random_slope_sd = random_slope_sd,
    random_slope_variance = random_slope_sd^2,
    residual_sd = residual_sd,
    x_sd = x_sd,
    contamination = contamination,
    contamination_prop = contamination_prop,
    contamination_size = contamination_size,
    reps = reps,
    alpha = alpha,
    methods = methods,
    seed = seed,
    replicate_seeds = replicate_seeds
  )

  list(
    summary = summary_results,
    replicates = if (keep_replicates) replicate_results else NULL,
    settings = settings
  )
}


