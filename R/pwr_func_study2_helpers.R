#' Study 2 Method Names
#'
#' Returns the method names supported by `pwr_func_study2()`.
#'
#' @return A character vector of method names.
#'
#' @keywords internal
study2_method_names <- function() {
  c(
    "rs",
    "ri",
    "cr2",
    "cats",
    "cats_trunc",
    "cats_robust",
    "cats_robustbase",
    "robust_ri",
    "robust_rs"
  )
}

#' Validate Study 2 Simulation Inputs
#'
#' @param n_clusters Number of clusters.
#' @param cluster_size Number of observations per cluster.
#' @param beta Population mean slope.
#' @param intercept Fixed intercept.
#' @param random_intercept_sd Standard deviation of the random intercept.
#' @param random_slope_sd Standard deviation of the random slope.
#' @param residual_sd Standard deviation of the residual error.
#' @param x_sd Standard deviation of the predictor.
#' @param contamination Contamination condition.
#' @param contamination_prop Proportion contaminated within each cluster.
#' @param contamination_size Size of the vertical outcome contamination.
#' @param reps Number of simulation replications.
#' @param alpha Significance level.
#' @param methods Methods to fit.
#' @param seed Optional random-number seed.
#' @param keep_replicates Whether to retain replicate-level results.
#'
#' @return `NULL`, invisibly. An error is raised for invalid inputs.
#'
#' @keywords internal
study2_validate_inputs <- function(n_clusters,
                                   cluster_size,
                                   beta,
                                   intercept,
                                   random_intercept_sd,
                                   random_slope_sd,
                                   residual_sd,
                                   x_sd,
                                   contamination,
                                   contamination_prop,
                                   contamination_size,
                                   reps,
                                   alpha,
                                   methods,
                                   seed,
                                   keep_replicates) {
  study1_check_integer(n_clusters, "n_clusters", lower = 3L)
  study1_check_integer(cluster_size, "cluster_size", lower = 5L)
  study1_check_integer(reps, "reps", lower = 1L)

  study1_check_numeric(beta, "beta")
  study1_check_numeric(intercept, "intercept")
  study1_check_numeric(
    random_intercept_sd,
    "random_intercept_sd",
    lower = 0,
    lower_open = TRUE
  )
  study1_check_numeric(
    random_slope_sd,
    "random_slope_sd",
    lower = 0,
    lower_open = TRUE
  )
  study1_check_numeric(
    residual_sd,
    "residual_sd",
    lower = 0,
    lower_open = TRUE
  )
  study1_check_numeric(
    x_sd,
    "x_sd",
    lower = 0,
    lower_open = TRUE
  )
  study1_check_numeric(
    contamination_prop,
    "contamination_prop",
    lower = 0,
    upper = 1
  )
  study1_check_numeric(
    contamination_size,
    "contamination_size",
    lower = 0,
    lower_open = TRUE
  )
  study1_check_numeric(
    alpha,
    "alpha",
    lower = 0,
    upper = 1,
    lower_open = TRUE,
    upper_open = TRUE
  )

  valid_contamination <- c("none", "vertical")
  if (length(contamination) != 1L ||
      !is.character(contamination) ||
      !(contamination %in% valid_contamination)) {
    stop(
      "contamination must be 'none' or 'vertical'.",
      call. = FALSE
    )
  }

  valid_methods <- study2_method_names()
  if (!is.character(methods) ||
      length(methods) == 0L ||
      anyNA(methods) ||
      any(!methods %in% valid_methods)) {
    stop(
      paste0(
        "methods must contain one or more of: ",
        paste(valid_methods, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (anyDuplicated(methods)) {
    stop("methods must not contain duplicate values.", call. = FALSE)
  }

  if (any(c("rs", "ri") %in% methods) &&
      !requireNamespace("pbkrtest", quietly = TRUE)) {
    stop(
      paste0(
        "Package 'pbkrtest' is required when methods includes 'rs' or ",
        "'ri'. Install it before running a mixed-model benchmark."
      ),
      call. = FALSE
    )
  }

  if (any(c("robust_ri", "robust_rs") %in% methods)) {
    study_require_robustlmm()
  }

  if (!is.null(seed)) {
    study1_check_integer(
      seed,
      "seed",
      lower = 0L,
      upper = .Machine$integer.max
    )
  }

  if (!is.logical(keep_replicates) ||
      length(keep_replicates) != 1L ||
      is.na(keep_replicates)) {
    stop("keep_replicates must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(NULL)
}

#' Simulate One Study 2 Data Set
#'
#' Generates data from an independent random-intercept and random-slope model
#' and then applies the Study 1 vertical-contamination mechanism within every
#' cluster.
#'
#' @param n_clusters Number of clusters.
#' @param cluster_size Number of observations per cluster.
#' @param beta Population mean slope.
#' @param intercept Fixed intercept.
#' @param random_intercept_sd Standard deviation of the random intercept.
#' @param random_slope_sd Standard deviation of the random slope.
#' @param residual_sd Standard deviation of the residual error.
#' @param x_sd Standard deviation of the predictor.
#' @param contamination Contamination condition.
#' @param contamination_prop Proportion contaminated within each cluster.
#' @param contamination_size Size of the outcome contamination in residual
#'   standard deviation units.
#'
#' @return A data frame containing the simulated and latent data.
#'
#' @keywords internal
study2_simulate_data <- function(n_clusters,
                                 cluster_size,
                                 beta,
                                 intercept,
                                 random_intercept_sd,
                                 random_slope_sd,
                                 residual_sd,
                                 x_sd,
                                 contamination,
                                 contamination_prop,
                                 contamination_size) {
  cluster <- factor(
    rep(seq_len(n_clusters), each = cluster_size),
    levels = seq_len(n_clusters)
  )
  n_obs <- length(cluster)

  x <- stats::rnorm(n_obs, mean = 0, sd = x_sd)

  random_intercept_cluster <- stats::rnorm(
    n_clusters,
    mean = 0,
    sd = random_intercept_sd
  )
  random_slope_standardized <- stats::rnorm(
    n_clusters,
    mean = 0,
    sd = 1
  )
  random_slope_cluster <-
    random_slope_standardized * random_slope_sd

  random_intercept <- rep(
    random_intercept_cluster,
    each = cluster_size
  )
  random_slope <- rep(
    random_slope_cluster,
    each = cluster_size
  )
  residual <- stats::rnorm(
    n_obs,
    mean = 0,
    sd = residual_sd
  )

  true_cluster_slope <- beta + random_slope
  out <- intercept +
    random_intercept +
    true_cluster_slope * x +
    residual

  dat <- data.frame(
    cluster = cluster,
    x = x,
    out = out,
    x_clean = x,
    out_clean = out,
    contaminated = FALSE,
    random_intercept = random_intercept,
    random_slope = random_slope,
    random_slope_standardized = rep(
      random_slope_standardized,
      each = cluster_size
    ),
    true_cluster_slope = true_cluster_slope,
    residual = residual,
    stringsAsFactors = FALSE
  )

  study1_apply_contamination(
    dat = dat,
    contamination = contamination,
    contamination_prop = contamination_prop,
    contamination_size = contamination_size,
    leverage_size = 1,
    residual_sd = residual_sd,
    x_sd = x_sd
  )
}

#' Derive a Study 2 Method-Specific Seed
#'
#' @param replicate_seed Seed for the replication.
#' @param method_index Position of the method in the canonical Study 2 method
#'   vector.
#'
#' @return An integer seed.
#'
#' @keywords internal
study2_method_seed <- function(replicate_seed, method_index) {
  study1_method_seed(
    replicate_seed = replicate_seed,
    method_index = method_index
  )
}

#' Extract Random-Effect Standard Deviations
#'
#' @param fit Fitted mixed model.
#'
#' @return A named numeric vector containing the fitted random-intercept and
#'   random-slope standard deviations when available.
#'
#' @keywords internal
study2_extract_random_effect_sds <- function(fit) {
  variance_components <- as.data.frame(lme4::VarCorr(fit))
  random_rows <- variance_components$grp != "Residual" &
    is.na(variance_components$var2)

  intercept_row <- random_rows &
    variance_components$var1 == "(Intercept)"
  slope_row <- random_rows &
    variance_components$var1 == "x"

  c(
    random_intercept_sd = if (any(intercept_row)) {
      unname(variance_components$sdcor[which(intercept_row)[1L]])
    } else {
      NA_real_
    },
    random_slope_sd = if (any(slope_row)) {
      unname(variance_components$sdcor[which(slope_row)[1L]])
    } else {
      NA_real_
    }
  )
}


#' Normalize Mixed-Model Convergence Messages
#'
#' @param messages Convergence messages from an `lme4` fit.
#'
#' @return A character vector with empty and missing messages removed.
#'
#' @keywords internal
study2_normalize_convergence_messages <- function(messages) {
  if (is.null(messages)) {
    return(character(0))
  }

  messages <- unlist(
    messages,
    recursive = TRUE,
    use.names = FALSE
  )
  messages <- as.character(messages)
  messages <- trimws(messages)
  messages[!is.na(messages) & nzchar(messages)]
}

#' Identify the Standard lme4 Singularity Message
#'
#' @param messages Character vector of convergence messages.
#'
#' @return A logical vector indicating messages that only report a boundary
#'   singular fit.
#'
#' @keywords internal
study2_is_singularity_message <- function(messages) {
  messages <- study2_normalize_convergence_messages(messages)

  if (length(messages) == 0L) {
    return(logical(0))
  }

  startsWith(
    messages,
    "boundary (singular) fit"
  )
}

#' Classify Mixed-Model Convergence Information
#'
#' Separates the standard `lme4` boundary-singularity message from genuine
#' optimizer or gradient convergence messages. A boundary singularity is
#' retained as a separate diagnostic and does not by itself make a finite fit
#' unsuccessful.
#'
#' @param messages Convergence messages from `fit@optinfo$conv$lme4$messages`.
#' @param optimizer_code Optimizer return code from `fit@optinfo$conv$opt`.
#'
#' @return A list containing all messages, singularity messages, optimizer
#'   messages, optimizer code, and the resulting convergence classification.
#'
#' @keywords internal
study2_classify_convergence <- function(messages,
                                        optimizer_code = 0L) {
  all_messages <- study2_normalize_convergence_messages(messages)
  singularity_message <- study2_is_singularity_message(
    all_messages
  )

  singularity_messages <- all_messages[
    singularity_message
  ]
  optimizer_messages <- all_messages[
    !singularity_message
  ]

  optimizer_code <- as.numeric(optimizer_code)
  optimizer_code_ok <- length(optimizer_code) == 0L ||
    all(is.na(optimizer_code) | optimizer_code == 0)

  list(
    all_messages = all_messages,
    singularity_messages = singularity_messages,
    optimizer_messages = optimizer_messages,
    optimizer_code = optimizer_code,
    converged = optimizer_code_ok &&
      length(optimizer_messages) == 0L
  )
}

#' Determine Whether a Study 2 Method Result Is Usable
#'
#' A singular mixed-model fit remains usable when the requested inference is
#' complete and finite and the fit has no genuine convergence failure.
#'
#' @param result Standardized method-result list.
#'
#' @return A single logical value.
#'
#' @keywords internal
study2_result_is_usable <- function(result) {
  complete_result <- all(is.finite(c(
    result$estimate,
    result$std_error,
    result$df,
    result$p_value,
    result$conf_low,
    result$conf_high,
    result$retained_clusters
  )))

  converged <- result$converged
  convergence_ok <- is.na(converged) || isTRUE(converged)

  complete_result && convergence_ok
}

#' Fit the Correctly Specified Random-Slope Benchmark
#'
#' Fits the independent random-intercept and random-slope model used to generate
#' Study 2 data. The default `lmerTest::lmer()` optimizer settings are retained
#' to match the Study 1 random-intercept benchmark.
#'
#' @param dat Simulated data.
#' @param alpha Significance level.
#'
#' @return A standardized result list.
#'
#' @keywords internal
study2_fit_rs <- function(dat, alpha) {
  if (!requireNamespace("pbkrtest", quietly = TRUE)) {
    stop(
      "Package 'pbkrtest' is required for Kenward-Roger inference.",
      call. = FALSE
    )
  }

  fit <- lmerTest::lmer(
    out ~ x + (1 + x || cluster),
    data = dat,
    REML = TRUE
  )

  fit_summary <- summary(fit, ddf = "Kenward-Roger")
  coefficient_table <- stats::coef(fit_summary)
  coefficient_row <- coefficient_table["x", , drop = FALSE]

  estimate <- unname(coefficient_row[1L, "Estimate"])
  std_error <- unname(coefficient_row[1L, "Std. Error"])
  df <- unname(coefficient_row[1L, "df"])
  p_value <- unname(coefficient_row[1L, "Pr(>|t|)"])
  critical_value <- stats::qt(1 - alpha / 2, df = df)

  convergence <- study2_classify_convergence(
    messages = fit@optinfo$conv$lme4$messages,
    optimizer_code = fit@optinfo$conv$opt
  )
  fitted_sds <- study2_extract_random_effect_sds(fit)

  list(
    estimate = estimate,
    std_error = std_error,
    df = df,
    p_value = p_value,
    conf_low = estimate - critical_value * std_error,
    conf_high = estimate + critical_value * std_error,
    converged = convergence$converged,
    singular = lme4::isSingular(fit, tol = 1e-4),
    retained_clusters = nlevels(dat$cluster),
    warning = study1_collapse_messages(
      convergence$all_messages
    ),
    optimizer_warning = study1_collapse_messages(
      convergence$optimizer_messages
    ),
    optimizer_code = if (
      length(convergence$optimizer_code) == 0L
    ) {
      NA_real_
    } else {
      convergence$optimizer_code[1L]
    },
    estimated_random_intercept_sd = unname(
      fitted_sds["random_intercept_sd"]
    ),
    estimated_random_slope_sd = unname(
      fitted_sds["random_slope_sd"]
    )
  )
}

#' Fit One Study 2 Method
#'
#' @param dat Simulated data.
#' @param method Method name.
#' @param beta Population mean slope.
#' @param alpha Significance level.
#' @param replicate_id Replication number.
#' @param method_seed Method-specific seed.
#' @param realized_mean_slope Mean true slope among sampled clusters.
#' @param realized_random_slope_sd SD of sampled random slopes.
#'
#' @return A one-row data frame of replicate-level results.
#'
#' @keywords internal
study2_fit_method <- function(dat,
                              method,
                              beta,
                              alpha,
                              replicate_id,
                              method_seed,
                              realized_mean_slope,
                              realized_random_slope_sd) {
  set.seed(method_seed)

  captured <- study1_capture_fit(function() {
    switch(
      method,
      "rs" = study2_fit_rs(dat = dat, alpha = alpha),
      "ri" = study1_fit_ri(dat = dat, alpha = alpha),
      "cr2" = study1_fit_cr2(dat = dat, alpha = alpha),
      "cats" = study1_fit_cats(
        dat = dat,
        alpha = alpha,
        truncate = FALSE
      ),
      "cats_trunc" = study1_fit_cats(
        dat = dat,
        alpha = alpha,
        truncate = TRUE
      ),
      "cats_robust" = study1_fit_robust_cats(
        dat = dat,
        alpha = alpha,
        engine = "robust"
      ),
      "cats_robustbase" = study1_fit_robust_cats(
        dat = dat,
        alpha = alpha,
        engine = "robustbase"
      ),
      "robust_ri" = study_fit_robust_mixed(
        dat = dat,
        alpha = alpha,
        model = "ri"
      ),
      "robust_rs" = study_fit_robust_mixed(
        dat = dat,
        alpha = alpha,
        model = "rs"
      ),
      stop("Unknown Study 2 method.", call. = FALSE)
    )
  })

  if (is.null(captured$value)) {
    return(study2_empty_result(
      replicate_id = replicate_id,
      method = method,
      beta = beta,
      warning = captured$warning,
      error = captured$error,
      runtime = captured$runtime,
      realized_mean_slope = realized_mean_slope,
      realized_random_slope_sd = realized_random_slope_sd
    ))
  }

  result <- captured$value
  fit_success <- study2_result_is_usable(result)
  converged <- result$converged

  data.frame(
    replicate = replicate_id,
    method = method,
    true_beta = beta,
    realized_mean_slope = realized_mean_slope,
    realized_random_slope_sd = realized_random_slope_sd,
    estimate = result$estimate,
    std_error = result$std_error,
    df = result$df,
    p_value = result$p_value,
    conf_low = result$conf_low,
    conf_high = result$conf_high,
    reject = if (fit_success) result$p_value < alpha else NA,
    cover = if (fit_success) {
      result$conf_low <= beta && result$conf_high >= beta
    } else {
      NA
    },
    fit_success = fit_success,
    converged = converged,
    singular = result$singular,
    retained_clusters = result$retained_clusters,
    estimated_random_intercept_sd = study1_result_component(
      result,
      "estimated_random_intercept_sd",
      NA_real_
    ),
    estimated_random_slope_sd = study1_result_component(
      result,
      "estimated_random_slope_sd",
      NA_real_
    ),
    warning = study1_collapse_messages(c(
      captured$warning,
      result$warning
    )),
    optimizer_warning = study1_result_component(
      result,
      "optimizer_warning",
      NA_character_
    ),
    optimizer_code = study1_result_component(
      result,
      "optimizer_code",
      NA_real_
    ),
    error = captured$error,
    template_warning = study1_result_component(
      result,
      "template_warning",
      NA_character_
    ),
    template_error = study1_result_component(
      result,
      "template_error",
      NA_character_
    ),
    cluster_warning_count = study1_result_component(
      result,
      "cluster_warning_count",
      NA_integer_
    ),
    cluster_error_count = study1_result_component(
      result,
      "cluster_error_count",
      NA_integer_
    ),
    dropped_cluster_count = study1_result_component(
      result,
      "dropped_cluster_count",
      NA_integer_
    ),
    cluster_warning_ids = study1_result_component(
      result,
      "cluster_warning_ids",
      NA_character_
    ),
    cluster_error_ids = study1_result_component(
      result,
      "cluster_error_ids",
      NA_character_
    ),
    dropped_cluster_ids = study1_result_component(
      result,
      "dropped_cluster_ids",
      NA_character_
    ),
    runtime_sec = captured$runtime,
    cluster_diagnostics = I(list(study1_result_component(
      result,
      "cluster_diagnostics",
      data.frame(
        cluster = character(0),
        intercept = numeric(0),
        x = numeric(0),
        retained = logical(0),
        warning = character(0),
        error = character(0),
        stringsAsFactors = FALSE
      )
    ))),
    stringsAsFactors = FALSE
  )
}

#' Create an Empty Study 2 Result
#'
#' @param replicate_id Replication number.
#' @param method Method name.
#' @param beta Population mean slope.
#' @param warning Warning text.
#' @param error Error text.
#' @param runtime Runtime in seconds.
#' @param realized_mean_slope Mean true slope among sampled clusters.
#' @param realized_random_slope_sd SD of sampled random slopes.
#'
#' @return A one-row data frame.
#'
#' @keywords internal
study2_empty_result <- function(replicate_id,
                                method,
                                beta,
                                warning,
                                error,
                                runtime,
                                realized_mean_slope,
                                realized_random_slope_sd) {
  data.frame(
    replicate = replicate_id,
    method = method,
    true_beta = beta,
    realized_mean_slope = realized_mean_slope,
    realized_random_slope_sd = realized_random_slope_sd,
    estimate = NA_real_,
    std_error = NA_real_,
    df = NA_real_,
    p_value = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    reject = NA,
    cover = NA,
    fit_success = FALSE,
    converged = NA,
    singular = NA,
    retained_clusters = NA_integer_,
    estimated_random_intercept_sd = NA_real_,
    estimated_random_slope_sd = NA_real_,
    warning = warning,
    optimizer_warning = NA_character_,
    optimizer_code = NA_real_,
    error = error,
    template_warning = NA_character_,
    template_error = NA_character_,
    cluster_warning_count = NA_integer_,
    cluster_error_count = NA_integer_,
    dropped_cluster_count = NA_integer_,
    cluster_warning_ids = NA_character_,
    cluster_error_ids = NA_character_,
    dropped_cluster_ids = NA_character_,
    runtime_sec = runtime,
    cluster_diagnostics = I(list(data.frame(
      cluster = character(0),
      intercept = numeric(0),
      x = numeric(0),
      retained = logical(0),
      warning = character(0),
      error = character(0),
      stringsAsFactors = FALSE
    ))),
    stringsAsFactors = FALSE
  )
}
