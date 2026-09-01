#' Study 1 Method Names
#'
#' Returns the method names supported by `pwr_func_study1()`.
#'
#' @return A character vector of method names.
#'
#' @keywords internal
study1_method_names <- function() {
  c(
    "ri",
    "cr2",
    "cats",
    "cats_trunc",
    "cats_robust",
    "cats_robustbase",
    "robust_ri"
  )
}

#' Validate Study 1 Simulation Inputs
#'
#' @param n_clusters Number of clusters.
#' @param cluster_size Number of observations per cluster.
#' @param beta True slope.
#' @param intercept Fixed intercept.
#' @param random_intercept_sd Standard deviation of the random intercept.
#' @param residual_sd Standard deviation of the residual error.
#' @param x_sd Standard deviation of the predictor.
#' @param contamination Contamination condition.
#' @param contamination_prop Proportion contaminated within each cluster.
#' @param contamination_size Size of the outcome contamination.
#' @param leverage_size Size of the contaminated predictor values.
#' @param reps Number of simulation replications.
#' @param alpha Significance level.
#' @param methods Methods to fit.
#' @param seed Optional random-number seed.
#' @param keep_replicates Whether to retain replicate-level results.
#'
#' @return `NULL`, invisibly. An error is raised for invalid inputs.
#'
#' @keywords internal
study1_validate_inputs <- function(n_clusters,
                                   cluster_size,
                                   beta,
                                   intercept,
                                   random_intercept_sd,
                                   residual_sd,
                                   x_sd,
                                   contamination,
                                   contamination_prop,
                                   contamination_size,
                                   leverage_size,
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
  study1_check_numeric(random_intercept_sd, "random_intercept_sd",
                       lower = 0, lower_open = TRUE)
  study1_check_numeric(residual_sd, "residual_sd",
                       lower = 0, lower_open = TRUE)
  study1_check_numeric(x_sd, "x_sd", lower = 0, lower_open = TRUE)
  study1_check_numeric(contamination_prop, "contamination_prop",
                       lower = 0, upper = 1)
  study1_check_numeric(contamination_size, "contamination_size",
                       lower = 0, lower_open = TRUE)
  study1_check_numeric(leverage_size, "leverage_size",
                       lower = 0, lower_open = TRUE)
  study1_check_numeric(alpha, "alpha", lower = 0, upper = 1,
                       lower_open = TRUE, upper_open = TRUE)

  valid_contamination <- c("none", "vertical", "bad_leverage")
  if (length(contamination) != 1L ||
      !is.character(contamination) ||
      !(contamination %in% valid_contamination)) {
    stop(
      "contamination must be 'none', 'vertical', or 'bad_leverage'.",
      call. = FALSE
    )
  }

  valid_methods <- study1_method_names()
  if (!is.character(methods) || length(methods) == 0L ||
      anyNA(methods) || any(!methods %in% valid_methods)) {
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

  if ("ri" %in% methods &&
      !requireNamespace("pbkrtest", quietly = TRUE)) {
    stop(
      paste0(
        "Package 'pbkrtest' is required when methods includes 'ri'. ",
        "Install it before running the random-intercept benchmark."
      ),
      call. = FALSE
    )
  }

  if ("robust_ri" %in% methods) {
    study_require_robustlmm()
  }

  if (!is.null(seed)) {
    study1_check_integer(seed, "seed", lower = 0L,
                         upper = .Machine$integer.max)
  }

  if (!is.logical(keep_replicates) || length(keep_replicates) != 1L ||
      is.na(keep_replicates)) {
    stop("keep_replicates must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(NULL)
}

#' Check a Numeric Scalar
#'
#' @param x Object to check.
#' @param name Argument name.
#' @param lower Optional lower bound.
#' @param upper Optional upper bound.
#' @param lower_open Whether the lower bound is open.
#' @param upper_open Whether the upper bound is open.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
study1_check_numeric <- function(x,
                                 name,
                                 lower = -Inf,
                                 upper = Inf,
                                 lower_open = FALSE,
                                 upper_open = FALSE) {
  valid <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)

  if (valid) {
    valid_lower <- if (lower_open) x > lower else x >= lower
    valid_upper <- if (upper_open) x < upper else x <= upper
    valid <- valid_lower && valid_upper
  }

  if (!valid) {
    stop(paste0(name, " is not valid."), call. = FALSE)
  }

  invisible(NULL)
}

#' Check an Integer Scalar
#'
#' @inheritParams study1_check_numeric
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
study1_check_integer <- function(x,
                                 name,
                                 lower = -Inf,
                                 upper = Inf) {
  study1_check_numeric(x, name, lower = lower, upper = upper)

  if (x != floor(x)) {
    stop(paste0(name, " must be an integer."), call. = FALSE)
  }

  invisible(NULL)
}

#' Validate Explicit Replication Seeds
#'
#' @param replicate_seeds Optional vector of explicit replication seeds.
#' @param reps Number of requested replications.
#'
#' @return `NULL` when `replicate_seeds` is `NULL`; otherwise an integer vector
#'   containing the validated seeds.
#'
#' @keywords internal
study_validate_replicate_seeds <- function(replicate_seeds, reps) {
  if (is.null(replicate_seeds)) {
    return(NULL)
  }

  valid <- is.numeric(replicate_seeds) &&
    length(replicate_seeds) == reps &&
    !anyNA(replicate_seeds) &&
    all(is.finite(replicate_seeds)) &&
    all(replicate_seeds == floor(replicate_seeds)) &&
    all(replicate_seeds >= 1) &&
    all(replicate_seeds <= .Machine$integer.max)

  if (!valid) {
    stop(
      paste(
        "replicate_seeds must be NULL or a numeric integer-valued vector",
        "of length reps with values from 1 through .Machine$integer.max."
      ),
      call. = FALSE
    )
  }

  if (anyDuplicated(replicate_seeds)) {
    stop(
      "replicate_seeds must contain unique values.",
      call. = FALSE
    )
  }

  as.integer(replicate_seeds)
}

#' Simulate One Study 1 Data Set
#'
#' Generates data from a constant-slope random-intercept model and then applies
#' the requested observation-level contamination within every cluster.
#'
#' @param n_clusters Number of clusters.
#' @param cluster_size Number of observations per cluster.
#' @param beta True slope.
#' @param intercept Fixed intercept.
#' @param random_intercept_sd Standard deviation of the random intercept.
#' @param residual_sd Standard deviation of the residual error.
#' @param x_sd Standard deviation of the predictor.
#' @param contamination Contamination condition.
#' @param contamination_prop Proportion contaminated within each cluster.
#' @param contamination_size Size of the outcome contamination in residual
#'   standard deviation units.
#' @param leverage_size Absolute size of contaminated predictor values in
#'   predictor standard deviation units.
#'
#' @return A data frame containing the simulated data.
#'
#' @keywords internal
study1_simulate_data <- function(n_clusters,
                                 cluster_size,
                                 beta,
                                 intercept,
                                 random_intercept_sd,
                                 residual_sd,
                                 x_sd,
                                 contamination,
                                 contamination_prop,
                                 contamination_size,
                                 leverage_size) {
  cluster <- factor(
    rep(seq_len(n_clusters), each = cluster_size),
    levels = seq_len(n_clusters)
  )
  n_obs <- length(cluster)

  x <- stats::rnorm(n_obs, mean = 0, sd = x_sd)
  random_intercept <- stats::rnorm(
    n_clusters,
    mean = 0,
    sd = random_intercept_sd
  )
  random_intercept <- rep(random_intercept, each = cluster_size)
  residual <- stats::rnorm(n_obs, mean = 0, sd = residual_sd)

  out <- intercept + beta * x + random_intercept + residual

  dat <- data.frame(
    cluster = cluster,
    x = x,
    out = out,
    x_clean = x,
    out_clean = out,
    contaminated = FALSE
  )

  study1_apply_contamination(
    dat = dat,
    contamination = contamination,
    contamination_prop = contamination_prop,
    contamination_size = contamination_size,
    leverage_size = leverage_size,
    residual_sd = residual_sd,
    x_sd = x_sd
  )
}

#' Apply Study 1 Contamination
#'
#' @param dat Clean simulated data.
#' @param contamination Contamination condition.
#' @param contamination_prop Proportion contaminated within each cluster.
#' @param contamination_size Size of the outcome contamination.
#' @param leverage_size Size of contaminated predictor values.
#' @param residual_sd Standard deviation of the residual error.
#' @param x_sd Standard deviation of the predictor.
#'
#' @return The data with the requested contamination applied.
#'
#' @keywords internal
study1_apply_contamination <- function(dat,
                                       contamination,
                                       contamination_prop,
                                       contamination_size,
                                       leverage_size,
                                       residual_sd,
                                       x_sd) {
  if (contamination == "none" || contamination_prop == 0) {
    return(dat)
  }

  cluster_indices <- split(seq_len(nrow(dat)), dat$cluster)
  cluster_size <- length(cluster_indices[[1L]])
  n_contaminated <- max(
    1L,
    as.integer(floor(cluster_size * contamination_prop + 0.5))
  )
  n_contaminated <- min(n_contaminated, cluster_size)

  contaminated_index <- unlist(
    lapply(
      cluster_indices,
      function(index) sample(index, size = n_contaminated, replace = FALSE)
    ),
    use.names = FALSE
  )

  contamination_sign <- sample(
    c(-1, 1),
    size = length(contaminated_index),
    replace = TRUE
  )

  dat$contaminated[contaminated_index] <- TRUE

  if (contamination == "vertical") {
    dat$out[contaminated_index] <-
      dat$out[contaminated_index] +
      contamination_sign * contamination_size * residual_sd
  }

  if (contamination == "bad_leverage") {
    dat$x[contaminated_index] <-
      contamination_sign * leverage_size * x_sd
    dat$out[contaminated_index] <-
      dat$out_clean[contaminated_index] -
      contamination_sign * contamination_size * residual_sd
  }

  dat
}

#' Capture a Study 1 Model Fit
#'
#' @param fit_function A zero-argument function that fits and extracts one method.
#'
#' @return A list containing the value, warning text, error text, and runtime.
#'
#' @keywords internal
study1_capture_fit <- function(fit_function) {
  warning_messages <- character(0)
  error_message <- NA_character_
  start_time <- proc.time()[["elapsed"]]

  value <- withCallingHandlers(
    tryCatch(
      fit_function(),
      error = function(e) {
        error_message <<- conditionMessage(e)
        NULL
      }
    ),
    warning = function(w) {
      warning_messages <<- c(warning_messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  runtime <- proc.time()[["elapsed"]] - start_time

  list(
    value = value,
    warning = study1_collapse_messages(warning_messages),
    error = error_message,
    runtime = unname(runtime)
  )
}

#' Collapse Warning or Error Messages
#'
#' @param messages Character vector of messages.
#'
#' @return A single character value or `NA_character_`.
#'
#' @keywords internal
study1_collapse_messages <- function(messages) {
  messages <- unique(messages[!is.na(messages) & nzchar(messages)])

  if (length(messages) == 0L) {
    return(NA_character_)
  }

  paste(messages, collapse = " | ")
}

#' Derive a Method-Specific Seed
#'
#' @param replicate_seed Seed for the replication.
#' @param method_index Position of the method in the requested method vector.
#'
#' @return An integer seed.
#'
#' @keywords internal
study1_method_seed <- function(replicate_seed, method_index) {
  max_seed <- .Machine$integer.max - 1
  seed <- (as.double(replicate_seed) + as.double(method_index) * 104729) %%
    max_seed
  as.integer(seed + 1)
}

#' Return a Result Component or a Default
#'
#' @param result Standardized method result.
#' @param name Component name.
#' @param default Default value.
#'
#' @return The requested component or the supplied default.
#'
#' @keywords internal
study1_result_component <- function(result, name, default) {
  value <- result[[name]]

  if (is.null(value)) {
    return(default)
  }

  value
}

#' Fit One Study 1 Method
#'
#' @param dat Simulated data.
#' @param method Method name.
#' @param beta True slope.
#' @param alpha Significance level.
#' @param replicate_id Replication number.
#' @param method_seed Method-specific seed.
#'
#' @return A one-row data frame of replicate-level results.
#'
#' @keywords internal
study1_fit_method <- function(dat,
                              method,
                              beta,
                              alpha,
                              replicate_id,
                              method_seed) {
  set.seed(method_seed)

  captured <- study1_capture_fit(function() {
    switch(
      method,
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
      stop("Unknown Study 1 method.", call. = FALSE)
    )
  })

  if (is.null(captured$value)) {
    return(study1_empty_result(
      replicate_id = replicate_id,
      method = method,
      beta = beta,
      warning = captured$warning,
      error = captured$error,
      runtime = captured$runtime
    ))
  }

  result <- captured$value
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
  fit_success <- complete_result && convergence_ok

  data.frame(
    replicate = replicate_id,
    method = method,
    true_beta = beta,
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

#' Create an Empty Study 1 Result
#'
#' @param replicate_id Replication number.
#' @param method Method name.
#' @param beta True slope.
#' @param warning Warning text.
#' @param error Error text.
#' @param runtime Runtime in seconds.
#'
#' @return A one-row data frame.
#'
#' @keywords internal
study1_empty_result <- function(replicate_id,
                                method,
                                beta,
                                warning,
                                error,
                                runtime) {
  data.frame(
    replicate = replicate_id,
    method = method,
    true_beta = beta,
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

#' Fit the Random-Intercept Benchmark
#'
#' @param dat Simulated data.
#' @param alpha Significance level.
#'
#' @return A standardized result list.
#'
#' @keywords internal
study1_fit_ri <- function(dat, alpha) {
  if (!requireNamespace("pbkrtest", quietly = TRUE)) {
    stop(
      "Package 'pbkrtest' is required for Kenward-Roger inference.",
      call. = FALSE
    )
  }

  fit <- lmerTest::lmer(
    out ~ x + (1 | cluster),
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
    warning = study1_collapse_messages(convergence$all_messages)
  )
}

#' Fit the CR2 Benchmark
#'
#' @param dat Simulated data.
#' @param alpha Significance level.
#'
#' @return A standardized result list.
#'
#' @keywords internal
study1_fit_cr2 <- function(dat, alpha) {
  fit <- stats::lm(out ~ x, data = dat)

  coefficient_test <- clubSandwich::coef_test(
    fit,
    vcov = "CR2",
    cluster = dat$cluster,
    test = "Satterthwaite",
    coefs = "x"
  )
  confidence_interval <- clubSandwich::conf_int(
    fit,
    vcov = "CR2",
    cluster = dat$cluster,
    level = 1 - alpha,
    test = "Satterthwaite",
    coefs = "x"
  )

  list(
    estimate = unname(coefficient_test$beta[1L]),
    std_error = unname(coefficient_test$SE[1L]),
    df = unname(coefficient_test$df_Satt[1L]),
    p_value = unname(coefficient_test$p_Satt[1L]),
    conf_low = unname(confidence_interval$CI_L[1L]),
    conf_high = unname(confidence_interval$CI_U[1L]),
    converged = NA,
    singular = NA,
    retained_clusters = nlevels(dat$cluster),
    warning = NA_character_
  )
}

#' Fit an Ordinary CATs Method
#'
#' @param dat Simulated data.
#' @param alpha Significance level.
#' @param truncate Whether to drop outlying cluster-specific estimates.
#'
#' @return A standardized result list.
#'
#' @keywords internal
study1_fit_cats <- function(dat, alpha, truncate) {
  fit <- stats::glm(
    out ~ x,
    data = dat,
    family = stats::gaussian()
  )

  cats_fit <- clusterSEs::cluster.im.glm(
    mod = fit,
    dat = dat,
    cluster = ~ cluster,
    ci.level = 1 - alpha,
    report = FALSE,
    drop = TRUE,
    truncate = truncate,
    return.vcv = TRUE
  )

  study1_extract_cats(
    cats_fit = cats_fit,
    alpha = alpha,
    n_clusters = nlevels(dat$cluster)
  )
}

#' Fit One Robust Linear Model for Study 1
#'
#' @param formula Model formula.
#' @param data Data used to fit the model.
#' @param engine Robust regression engine.
#'
#' @return A fitted robust linear model.
#'
#' @keywords internal
study1_fit_robust_model <- function(formula, data, engine) {
  switch(
    engine,
    "robust" = robust::lmRob(formula = formula, data = data),
    "robustbase" = robustbase::lmrob(formula = formula, data = data),
    stop("Unknown robust engine.", call. = FALSE)
  )
}

#' Fit One Cluster-Specific Robust Model
#'
#' Fits a robust linear model in one cluster while retaining warnings and
#' errors. A cluster is retained when both requested coefficients are present
#' and finite. Warnings do not cause an otherwise usable estimate to be
#' dropped.
#'
#' @param cluster_id Cluster identifier.
#' @param dat Complete simulated data.
#' @param formula Model formula.
#' @param engine Robust regression engine.
#' @param fit_function Function used to fit the robust model.
#'
#' @return A one-row data frame containing the cluster-specific coefficients
#'   and diagnostics.
#'
#' @keywords internal
study1_fit_robust_cluster <- function(
    cluster_id,
    dat,
    formula,
    engine,
    fit_function = study1_fit_robust_model) {
  cluster_label <- as.character(cluster_id)
  cluster_dat <- dat[
    as.character(dat$cluster) == cluster_label,
    ,
    drop = FALSE
  ]

  captured <- study1_capture_fit(function() {
    fit <- fit_function(
      formula = formula,
      data = cluster_dat,
      engine = engine
    )
    coefficients <- stats::coef(fit)
    required_coefficients <- c("(Intercept)", "x")

    if (!all(required_coefficients %in% names(coefficients))) {
      stop(
        "Cluster-specific fit did not return all required coefficients.",
        call. = FALSE
      )
    }

    coefficients <- coefficients[required_coefficients]

    if (any(!is.finite(coefficients))) {
      stop(
        "Cluster-specific fit returned non-finite coefficients.",
        call. = FALSE
      )
    }

    coefficients
  })

  retained <- !is.null(captured$value)

  data.frame(
    cluster = cluster_label,
    intercept = if (retained) {
      unname(captured$value["(Intercept)"])
    } else {
      NA_real_
    },
    x = if (retained) {
      unname(captured$value["x"])
    } else {
      NA_real_
    },
    retained = retained,
    warning = captured$warning,
    error = captured$error,
    stringsAsFactors = FALSE
  )
}

#' Fit a Robust CATs Method
#'
#' Fits robust regression separately in every cluster and applies the CATs
#' calculation to the retained cluster-specific coefficients. Cluster-level
#' warnings and errors are recorded without changing the public
#' `cluster_im_lmRob()` function.
#'
#' @param dat Simulated data.
#' @param alpha Significance level.
#' @param engine Robust regression engine.
#' @param fit_function Function used for the full-data template and
#'   cluster-specific robust fits.
#'
#' @return A standardized result list with cluster-specific diagnostics.
#'
#' @keywords internal
study1_fit_robust_cats <- function(
    dat,
    alpha,
    engine,
    fit_function = study1_fit_robust_model) {
  formula <- out ~ x

  # Preserve the existing robust CATs random-number sequence by fitting the
  # full-data model before the cluster-specific models. The template estimates
  # are not used for CATs inference, so a template failure is recorded but does
  # not block otherwise viable cluster-specific fits.
  template_fit <- study1_capture_fit(function() {
    fit_function(
      formula = formula,
      data = dat,
      engine = engine
    )
  })

  cluster_ids <- unique(as.character(dat$cluster))
  cluster_diagnostics <- do.call(
    rbind,
    lapply(cluster_ids, function(cluster_id) {
      study1_fit_robust_cluster(
        cluster_id = cluster_id,
        dat = dat,
        formula = formula,
        engine = engine,
        fit_function = fit_function
      )
    })
  )
  rownames(cluster_diagnostics) <- NULL

  retained <- cluster_diagnostics$retained %in% TRUE
  retained_clusters <- sum(retained)

  if (retained_clusters < 2L) {
    stop(
      "Fewer than two cluster-specific robust estimates were retained.",
      call. = FALSE
    )
  }

  beta_cluster <- as.matrix(
    cluster_diagnostics[
      retained,
      c("intercept", "x"),
      drop = FALSE
    ]
  )
  colnames(beta_cluster) <- c("(Intercept)", "x")

  beta_average <- colMeans(beta_cluster)
  coefficient_vcv <- stats::cov(beta_cluster)
  coefficient_variance <- unname(coefficient_vcv["x", "x"])
  std_error <- sqrt(coefficient_variance / retained_clusters)
  estimate <- unname(beta_average["x"])
  df <- retained_clusters - 1L
  t_statistic <- estimate / std_error
  p_value <- 2 * stats::pt(
    abs(t_statistic),
    df = df,
    lower.tail = FALSE
  )
  critical_value <- stats::qt(1 - alpha / 2, df = df)

  warning_index <- !is.na(cluster_diagnostics$warning) &
    nzchar(cluster_diagnostics$warning)
  error_index <- !is.na(cluster_diagnostics$error) &
    nzchar(cluster_diagnostics$error)
  dropped_index <- !retained

  cluster_warning <- if (any(warning_index)) {
    study1_collapse_messages(
      paste0(
        "Cluster ",
        cluster_diagnostics$cluster[warning_index],
        ": ",
        cluster_diagnostics$warning[warning_index]
      )
    )
  } else {
    NA_character_
  }

  list(
    estimate = estimate,
    std_error = std_error,
    df = df,
    p_value = p_value,
    conf_low = estimate - critical_value * std_error,
    conf_high = estimate + critical_value * std_error,
    converged = NA,
    singular = NA,
    retained_clusters = retained_clusters,
    warning = cluster_warning,
    template_warning = template_fit$warning,
    template_error = template_fit$error,
    cluster_warning_count = sum(warning_index),
    cluster_error_count = sum(error_index),
    dropped_cluster_count = sum(dropped_index),
    cluster_warning_ids = study1_collapse_messages(
      cluster_diagnostics$cluster[warning_index]
    ),
    cluster_error_ids = study1_collapse_messages(
      cluster_diagnostics$cluster[error_index]
    ),
    dropped_cluster_ids = study1_collapse_messages(
      cluster_diagnostics$cluster[dropped_index]
    ),
    cluster_diagnostics = cluster_diagnostics
  )
}

#' Extract a CATs Result
#'
#' @param cats_fit CATs output.
#' @param alpha Significance level.
#' @param n_clusters Maximum number of clusters.
#'
#' @return A standardized result list.
#'
#' @keywords internal
study1_extract_cats <- function(cats_fit, alpha, n_clusters) {
  estimate <- unname(cats_fit$beta.bar["x"])
  p_value <- unname(cats_fit$p.values["x", 1L])
  conf_low <- unname(cats_fit$ci["x", 1L])
  conf_high <- unname(cats_fit$ci["x", 2L])
  coefficient_variance <- unname(cats_fit$vcv.hat["x", "x"])

  retained_clusters <- study1_infer_retained_clusters(
    coefficient_variance = coefficient_variance,
    conf_low = conf_low,
    conf_high = conf_high,
    alpha = alpha,
    n_clusters = n_clusters
  )

  list(
    estimate = estimate,
    std_error = sqrt(coefficient_variance / retained_clusters),
    df = retained_clusters - 1,
    p_value = p_value,
    conf_low = conf_low,
    conf_high = conf_high,
    converged = NA,
    singular = NA,
    retained_clusters = retained_clusters,
    warning = NA_character_
  )
}

#' Infer the Number of Retained CATs Clusters
#'
#' The CATs functions return the cross-cluster coefficient variance and
#' confidence interval but not the number of retained clusters. This helper
#' recovers the retained count by matching the reported confidence interval to
#' the CATs confidence interval formula.
#'
#' @param coefficient_variance Cross-cluster coefficient variance.
#' @param conf_low Lower confidence limit.
#' @param conf_high Upper confidence limit.
#' @param alpha Significance level.
#' @param n_clusters Maximum number of clusters.
#'
#' @return The inferred number of retained clusters.
#'
#' @keywords internal
study1_infer_retained_clusters <- function(coefficient_variance,
                                           conf_low,
                                           conf_high,
                                           alpha,
                                           n_clusters) {
  if (!is.finite(coefficient_variance) || coefficient_variance < 0 ||
      !is.finite(conf_low) || !is.finite(conf_high)) {
    return(NA_integer_)
  }

  if (coefficient_variance == 0) {
    stop(
      paste(
        "The retained cluster count cannot be inferred when the",
        "cross-cluster coefficient variance is zero."
      ),
      call. = FALSE
    )
  }

  candidate_clusters <- seq.int(2L, n_clusters)
  target_half_width <- (conf_high - conf_low) / 2
  candidate_half_width <- stats::qt(
    1 - alpha / 2,
    df = candidate_clusters - 1
  ) * sqrt(coefficient_variance / candidate_clusters)

  as.integer(candidate_clusters[
    which.min(abs(candidate_half_width - target_half_width))
  ])
}

#' Summarize Study 1 Replicate Results
#'
#' @param replicate_results Replicate-level results.
#' @param methods Ordered method names.
#' @param reps Number of requested replications.
#'
#' @return A data frame of method-level simulation summaries.
#'
#' @keywords internal
study1_summarize_results <- function(replicate_results, methods, reps) {
  summaries <- lapply(methods, function(method) {
    method_results <- replicate_results[
      replicate_results$method == method,
      ,
      drop = FALSE
    ]
    successful <- method_results$fit_success %in% TRUE
    successful_results <- method_results[successful, , drop = FALSE]
    n_success <- nrow(successful_results)

    rejection <- successful_results$reject
    coverage <- successful_results$cover
    rejection_prop <- study1_mean_or_na(rejection)
    coverage_prop <- study1_mean_or_na(coverage)

    singular_values <- successful_results$singular
    singular_values <- singular_values[!is.na(singular_values)]

    data.frame(
      model = method,
      mean_coef = study1_mean_or_na(successful_results$estimate),
      bias = study1_mean_or_na(
        successful_results$estimate - successful_results$true_beta
      ),
      rejection_rate = 100 * rejection_prop,
      rejection_rate_se = 100 * study1_binomial_mcse(
        rejection_prop,
        n_success
      ),
      rmse = study1_rmse_or_na(
        successful_results$estimate,
        successful_results$true_beta
      ),
      coverage = 100 * coverage_prop,
      coverage_se = 100 * study1_binomial_mcse(
        coverage_prop,
        n_success
      ),
      avg_ci_width = study1_mean_or_na(
        successful_results$conf_high - successful_results$conf_low
      ),
      success = n_success,
      failure_rate = 100 * (reps - n_success) / reps,
      singular_rate = if (length(singular_values) == 0L) {
        NA_real_
      } else {
        100 * mean(singular_values)
      },
      mean_retained_clusters = study1_mean_or_na(
        successful_results$retained_clusters
      ),
      mean_runtime_sec = study1_mean_or_na(method_results$runtime_sec),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, summaries)
}

#' Calculate a Mean or Return Missing
#'
#' @param x Numeric or logical vector.
#'
#' @return A scalar numeric value.
#'
#' @keywords internal
study1_mean_or_na <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  mean(x)
}

#' Calculate RMSE or Return Missing
#'
#' @param estimate Estimated coefficients.
#' @param truth True coefficients.
#'
#' @return A scalar numeric value.
#'
#' @keywords internal
study1_rmse_or_na <- function(estimate, truth) {
  valid <- is.finite(estimate) & is.finite(truth)

  if (!any(valid)) {
    return(NA_real_)
  }

  sqrt(mean((estimate[valid] - truth[valid])^2))
}

#' Calculate a Binomial Monte Carlo Standard Error
#'
#' @param proportion Estimated probability.
#' @param n Number of successful replications.
#'
#' @return A scalar numeric value.
#'
#' @keywords internal
study1_binomial_mcse <- function(proportion, n) {
  if (!is.finite(proportion) || n <= 0L) {
    return(NA_real_)
  }

  sqrt(proportion * (1 - proportion) / n)
}