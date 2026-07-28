#' Simulation Study of Robust Cluster-Adjusted t Statistics
#'
#' Runs one condition from a focused simulation study comparing a correctly
#' specified random-intercept model, ordinary least squares with CR2 inference,
#' ordinary cluster-adjusted t statistics (CATs), truncated CATs, and robust
#' CATs based on either `robust::lmRob()` or `robustbase::lmrob()`.
#'
#' Data are generated from a constant-slope random-intercept model. Optional
#' contamination is applied within every cluster after the clean data are
#' generated. All requested methods are fit to the same data in each replication.
#' The `"ri"` method uses Kenward-Roger inference and requires the suggested
#' package `pbkrtest`.
#'
#' @param n_clusters Integer number of clusters.
#' @param cluster_size Integer number of observations in each cluster.
#' @param beta Numeric value of the true slope for `x`.
#' @param intercept Numeric fixed intercept.
#' @param random_intercept_sd Positive standard deviation of the random
#'   intercept.
#' @param residual_sd Positive standard deviation of the residual error.
#' @param x_sd Positive standard deviation of the normally distributed
#'   predictor.
#' @param contamination Character string specifying the contamination condition:
#'   `"none"`, `"vertical"`, or `"bad_leverage"`.
#' @param contamination_prop Numeric proportion of observations contaminated
#'   within each cluster. The number contaminated is rounded to the nearest
#'   whole observation, with at least one observation selected when the
#'   proportion is greater than zero.
#' @param contamination_size Positive size of the outcome contamination in
#'   residual standard deviation units.
#' @param leverage_size Positive absolute size of contaminated predictor values
#'   in predictor standard deviation units.
#' @param reps Integer number of simulation replications.
#' @param alpha Numeric significance level.
#' @param methods Character vector of methods to fit. Available values are
#'   `"ri"`, `"cr2"`, `"cats"`, `"cats_trunc"`, `"cats_robust"`, and
#'   `"cats_robustbase"`.
#' @param seed Optional non-negative integer random-number seed.
#' @param keep_replicates Logical; if `TRUE`, retain the replicate-level results.
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
#' percentage points.
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
#' pwr_func_study1(
#'   n_clusters = 5,
#'   cluster_size = 20,
#'   reps = 1,
#'   methods = c("cr2", "cats"),
#'   seed = 123
#' )
#'
#' @export
pwr_func_study1 <- function(
    n_clusters = 20,
    cluster_size = 40,
    beta = 0,
    intercept = 0,
    random_intercept_sd = 1,
    residual_sd = 1,
    x_sd = 1,
    contamination = c("none", "vertical", "bad_leverage"),
    contamination_prop = 0.05,
    contamination_size = 10,
    leverage_size = 10,
    reps = 1000,
    alpha = 0.05,
    methods = c(
      "ri",
      "cr2",
      "cats",
      "cats_trunc",
      "cats_robust",
      "cats_robustbase"
    ),
    seed = NULL,
    keep_replicates = FALSE) {
  contamination <- match.arg(contamination)

  study1_validate_inputs(
    n_clusters = n_clusters,
    cluster_size = cluster_size,
    beta = beta,
    intercept = intercept,
    random_intercept_sd = random_intercept_sd,
    residual_sd = residual_sd,
    x_sd = x_sd,
    contamination = contamination,
    contamination_prop = contamination_prop,
    contamination_size = contamination_size,
    leverage_size = leverage_size,
    reps = reps,
    alpha = alpha,
    methods = methods,
    seed = seed,
    keep_replicates = keep_replicates
  )

  if (!is.null(seed)) {
    set.seed(seed)
  }

  replicate_seeds <- sample.int(
    .Machine$integer.max,
    size = reps,
    replace = FALSE
  )

  replicate_results <- lapply(seq_len(reps), function(replicate_id) {
    replicate_seed <- replicate_seeds[replicate_id]
    set.seed(replicate_seed)

    dat <- study1_simulate_data(
      n_clusters = n_clusters,
      cluster_size = cluster_size,
      beta = beta,
      intercept = intercept,
      random_intercept_sd = random_intercept_sd,
      residual_sd = residual_sd,
      x_sd = x_sd,
      contamination = contamination,
      contamination_prop = contamination_prop,
      contamination_size = contamination_size,
      leverage_size = leverage_size
    )

    method_results <- lapply(seq_along(methods), function(method_index) {
      study1_fit_method(
        dat = dat,
        method = methods[method_index],
        beta = beta,
        alpha = alpha,
        replicate_id = replicate_id,
        method_seed = study1_method_seed(
          replicate_seed = replicate_seed,
          method_index = match(
            methods[method_index],
            study1_method_names()
          )
        )
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
    residual_sd = residual_sd,
    x_sd = x_sd,
    contamination = contamination,
    contamination_prop = contamination_prop,
    contamination_size = contamination_size,
    leverage_size = leverage_size,
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
