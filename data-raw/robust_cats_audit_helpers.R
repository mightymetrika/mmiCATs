# Independent CATs audit helpers
#
# These functions are test-only. They intentionally do not call the mmiCATs
# CATs aggregation helpers or clusterSEs aggregation helpers when constructing
# the independent oracle.
#
# Initial scope:
#   outcome: out
#   focal predictor: x
#   cluster variable: cluster
#   model: out ~ x

rca_find_project_root <- function(path = getwd()) {
  path <- normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )

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

rca_require_packages <- function() {
  required <- c(
    "pkgload",
    "clusterSEs",
    "robust",
    "robustbase"
  )

  missing <- required[
    !vapply(
      required,
      requireNamespace,
      quietly = TRUE,
      FUN.VALUE = logical(1)
    )
  ]

  if (length(missing) > 0L) {
    stop(
      paste0(
        "Install the required package(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

rca_collapse_text <- function(x) {
  x <- trimws(as.character(x))
  x <- unique(x[!is.na(x) & nzchar(x)])

  if (length(x) == 0L) {
    return(NA_character_)
  }

  paste(x, collapse = " | ")
}

rca_has_text <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

rca_capture <- function(expr) {
  warnings <- character()
  messages <- character()
  started <- proc.time()[["elapsed"]]

  value <- withCallingHandlers(
    tryCatch(
      expr,
      error = function(e) e
    ),
    warning = function(w) {
      warnings <<- c(
        warnings,
        conditionMessage(w)
      )
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      messages <<- c(
        messages,
        conditionMessage(m)
      )
      invokeRestart("muffleMessage")
    }
  )

  elapsed <- proc.time()[["elapsed"]] - started

  if (inherits(value, "error")) {
    return(list(
      value = NULL,
      warning = rca_collapse_text(warnings),
      message = rca_collapse_text(messages),
      error = conditionMessage(value),
      elapsed_sec = unname(elapsed)
    ))
  }

  list(
    value = value,
    warning = rca_collapse_text(warnings),
    message = rca_collapse_text(messages),
    error = NA_character_,
    elapsed_sec = unname(elapsed)
  )
}

rca_save_rds_atomic <- function(object,
                                path,
                                compress = "gzip") {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  temporary <- tempfile(
    pattern = "rca_",
    tmpdir = dirname(path),
    fileext = ".rds"
  )

  saveRDS(
    object,
    temporary,
    version = 3,
    compress = compress
  )

  if (file.exists(path) && !file.remove(path)) {
    stop(
      paste("Could not replace:", path),
      call. = FALSE
    )
  }

  if (!file.rename(temporary, path)) {
    stop(
      paste("Could not save:", path),
      call. = FALSE
    )
  }

  invisible(path)
}

rca_write_csv_atomic <- function(object,
                                 path) {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  temporary <- tempfile(
    pattern = "rca_",
    tmpdir = dirname(path),
    fileext = ".csv"
  )

  utils::write.csv(
    object,
    temporary,
    row.names = FALSE,
    na = ""
  )

  if (file.exists(path) && !file.remove(path)) {
    stop(
      paste("Could not replace:", path),
      call. = FALSE
    )
  }

  if (!file.rename(temporary, path)) {
    stop(
      paste("Could not save:", path),
      call. = FALSE
    )
  }

  invisible(path)
}

rca_source_checksums <- function(files) {
  exists <- file.exists(files)

  data.frame(
    source = names(files),
    path = normalizePath(
      files,
      winslash = "/",
      mustWork = FALSE
    ),
    exists = exists,
    md5 = ifelse(
      exists,
      unname(tools::md5sum(files)),
      NA_character_
    ),
    stringsAsFactors = FALSE
  )
}

rca_fit_model <- function(formula,
                          data,
                          engine) {
  switch(
    engine,
    "glm" = stats::glm(
      formula = formula,
      data = data,
      family = stats::gaussian()
    ),
    "robust" = robust::lmRob(
      formula = formula,
      data = data
    ),
    "robustbase" = robustbase::lmrob(
      formula = formula,
      data = data
    ),
    stop(
      "engine must be 'glm', 'robust', or 'robustbase'.",
      call. = FALSE
    )
  )
}

rca_extract_required_coefficients <- function(fit,
                                              required) {
  coefficients <- stats::coef(fit)

  if (!all(required %in% names(coefficients))) {
    stop(
      paste0(
        "The fit did not return all required coefficients: ",
        paste(required, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  coefficients <- coefficients[required]

  if (any(!is.finite(coefficients))) {
    stop(
      "The fit returned nonfinite required coefficients.",
      call. = FALSE
    )
  }

  coefficients
}

rca_fit_cluster_models <- function(
    dat,
    formula = out ~ x,
    cluster_var = "cluster",
    engine = c("glm", "robust", "robustbase"),
    required = c("(Intercept)", "x"),
    consume_template = engine != "glm") {
  engine <- match.arg(engine)

  if (!is.data.frame(dat)) {
    stop("dat must be a data frame.", call. = FALSE)
  }

  required_columns <- unique(c(
    all.vars(formula),
    cluster_var
  ))

  missing_columns <- setdiff(
    required_columns,
    names(dat)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "dat is missing: ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  template <- list(
    warning = NA_character_,
    message = NA_character_,
    error = NA_character_
  )

  if (isTRUE(consume_template)) {
    captured_template <- rca_capture(
      rca_fit_model(
        formula = formula,
        data = dat,
        engine = engine
      )
    )

    template <- captured_template[c(
      "warning",
      "message",
      "error"
    )]

    if (rca_has_text(captured_template$error)) {
      stop(
        paste(
          "The full-data template fit failed:",
          captured_template$error
        ),
        call. = FALSE
      )
    }
  }

  cluster_order <- unique(
    as.character(dat[[cluster_var]])
  )

  rows <- lapply(
    cluster_order,
    function(cluster_id) {
      cluster_dat <- dat[
        as.character(dat[[cluster_var]]) ==
          cluster_id,
        ,
        drop = FALSE
      ]

      captured <- rca_capture({
        fit <- rca_fit_model(
          formula = formula,
          data = cluster_dat,
          engine = engine
        )

        rca_extract_required_coefficients(
          fit,
          required = required
        )
      })

      retained <- !is.null(captured$value)

      data.frame(
        cluster = cluster_id,
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
        retained_before_truncation = retained,
        warning = captured$warning,
        message = captured$message,
        error = captured$error,
        elapsed_sec = captured$elapsed_sec,
        stringsAsFactors = FALSE
      )
    }
  )

  diagnostics <- do.call(rbind, rows)
  rownames(diagnostics) <- NULL

  list(
    diagnostics = diagnostics,
    cluster_order = cluster_order,
    template_warning = template$warning,
    template_message = template$message,
    template_error = template$error
  )
}

rca_truncation_flags <- function(
    coefficient_matrix,
    rule = c(
      "none",
      "clusterSEs",
      "documented"
    ),
    multiplier = 6) {
  rule <- match.arg(rule)

  coefficient_matrix <- as.matrix(
    coefficient_matrix
  )
  storage.mode(coefficient_matrix) <- "double"

  if (nrow(coefficient_matrix) < 2L) {
    stop(
      "At least two coefficient rows are required.",
      call. = FALSE
    )
  }

  if (any(!is.finite(coefficient_matrix))) {
    stop(
      "The coefficient matrix must be finite.",
      call. = FALSE
    )
  }

  if (identical(rule, "none")) {
    return(rep(FALSE, nrow(coefficient_matrix)))
  }

  coefficient_mean <- colMeans(
    coefficient_matrix
  )
  coefficient_iqr <- apply(
    coefficient_matrix,
    2L,
    stats::IQR
  )

  if (identical(rule, "clusterSEs")) {
    threshold <- abs(coefficient_mean) +
      multiplier * coefficient_iqr
    coefficient_flags <- sweep(
      abs(coefficient_matrix),
      2L,
      threshold,
      FUN = ">"
    )
  } else {
    deviation <- abs(
      sweep(
        coefficient_matrix,
        2L,
        coefficient_mean,
        FUN = "-"
      )
    )
    threshold <- multiplier * coefficient_iqr
    coefficient_flags <- sweep(
      deviation,
      2L,
      threshold,
      FUN = ">"
    )
  }

  rowSums(coefficient_flags) > 0L
}

rca_aggregate_coefficients <- function(
    diagnostics,
    alpha = 0.05,
    focal = "x",
    truncation_rule = c(
      "none",
      "clusterSEs",
      "documented"
    ),
    multiplier = 6) {
  truncation_rule <- match.arg(truncation_rule)

  required_columns <- c(
    "cluster",
    "intercept",
    "x",
    "retained_before_truncation"
  )

  if (!all(required_columns %in% names(diagnostics))) {
    stop(
      "diagnostics does not have the required columns.",
      call. = FALSE
    )
  }

  initially_retained <-
    diagnostics$retained_before_truncation %in% TRUE

  initial_matrix <- as.matrix(
    diagnostics[
      initially_retained,
      c("intercept", "x"),
      drop = FALSE
    ]
  )
  colnames(initial_matrix) <- c(
    "(Intercept)",
    "x"
  )

  if (nrow(initial_matrix) < 2L) {
    stop(
      "Fewer than two cluster estimates were initially retained.",
      call. = FALSE
    )
  }

  truncation_flags <- rca_truncation_flags(
    coefficient_matrix = initial_matrix,
    rule = truncation_rule,
    multiplier = multiplier
  )

  retained_indices <- which(initially_retained)[
    !truncation_flags
  ]

  if (length(retained_indices) < 2L) {
    stop(
      "Fewer than two cluster estimates remained after truncation.",
      call. = FALSE
    )
  }

  retained_matrix <- as.matrix(
    diagnostics[
      retained_indices,
      c("intercept", "x"),
      drop = FALSE
    ]
  )
  colnames(retained_matrix) <- c(
    "(Intercept)",
    "x"
  )

  retained_clusters <- nrow(retained_matrix)
  beta_bar <- colMeans(retained_matrix)
  vcv_hat <- stats::cov(retained_matrix)
  focal_variance <- unname(
    vcv_hat[focal, focal]
  )
  std_error <- sqrt(
    focal_variance / retained_clusters
  )
  estimate <- unname(beta_bar[focal])
  df <- retained_clusters - 1L
  t_statistic <- estimate / std_error
  p_value <- 2 * pmin(
    stats::pt(
      t_statistic,
      df = df,
      lower.tail = TRUE
    ),
    stats::pt(
      t_statistic,
      df = df,
      lower.tail = FALSE
    )
  )
  critical_value <- stats::qt(
    1 - alpha / 2,
    df = df
  )

  retained_after_truncation <- rep(
    FALSE,
    nrow(diagnostics)
  )
  retained_after_truncation[
    retained_indices
  ] <- TRUE

  list(
    estimate = estimate,
    std_error = std_error,
    df = df,
    t_statistic = t_statistic,
    p_value = p_value,
    conf_low = estimate -
      critical_value * std_error,
    conf_high = estimate +
      critical_value * std_error,
    retained_clusters = retained_clusters,
    beta_bar = beta_bar,
    vcv_hat = vcv_hat,
    retained_after_truncation =
      retained_after_truncation,
    dropped_by_truncation =
      initially_retained &
      !retained_after_truncation,
    retained_cluster_ids =
      diagnostics$cluster[
        retained_after_truncation
      ],
    dropped_cluster_ids =
      diagnostics$cluster[
        !retained_after_truncation
      ],
    truncation_rule = truncation_rule
  )
}

rca_oracle <- function(
    dat,
    engine = c("glm", "robust", "robustbase"),
    alpha = 0.05,
    truncation_rule = c(
      "none",
      "clusterSEs",
      "documented"
    ),
    consume_template = engine != "glm") {
  engine <- match.arg(engine)
  truncation_rule <- match.arg(
    truncation_rule
  )

  fits <- rca_fit_cluster_models(
    dat = dat,
    engine = engine,
    consume_template = consume_template
  )

  aggregate <- rca_aggregate_coefficients(
    diagnostics = fits$diagnostics,
    alpha = alpha,
    focal = "x",
    truncation_rule = truncation_rule
  )

  list(
    engine = engine,
    diagnostics = fits$diagnostics,
    cluster_order = fits$cluster_order,
    template_warning =
      fits$template_warning,
    template_message =
      fits$template_message,
    template_error =
      fits$template_error,
    aggregate = aggregate
  )
}

rca_make_validation_data <- function(
    seed = 20261001L,
    n_clusters = 6L,
    cluster_size = 18L) {
  set.seed(seed)

  cluster <- factor(
    rep(
      seq_len(n_clusters),
      each = cluster_size
    ),
    levels = seq_len(n_clusters)
  )
  x <- stats::rnorm(
    n_clusters * cluster_size
  )
  random_intercept <- rep(
    stats::rnorm(
      n_clusters,
      mean = 0,
      sd = 0.75
    ),
    each = cluster_size
  )
  residual <- stats::rnorm(
    n_clusters * cluster_size,
    mean = 0,
    sd = 0.65
  )

  data.frame(
    cluster = cluster,
    x = x,
    out = 0.35 +
      0.25 * x +
      random_intercept +
      residual
  )
}

rca_make_truncation_data <- function() {
  slopes <- c(
    -2.0,
    1.9,
    2.0,
    2.1,
    2.2
  )
  intercepts <- c(
    -0.20,
    -0.10,
    0.00,
    0.10,
    0.20
  )
  x_pattern <- rep(
    c(-2, -1, 0, 1, 2),
    times = 2L
  )

  rows <- lapply(
    seq_along(slopes),
    function(index) {
      data.frame(
        cluster = factor(
          rep(index, length(x_pattern)),
          levels = seq_along(slopes)
        ),
        x = x_pattern,
        out = intercepts[index] +
          slopes[index] * x_pattern
      )
    }
  )

  dat <- do.call(rbind, rows)
  dat$cluster <- factor(
    as.character(dat$cluster),
    levels = as.character(
      seq_along(slopes)
    )
  )
  rownames(dat) <- NULL
  dat
}

rca_package_cats <- function(
    dat,
    alpha = 0.05,
    truncate = FALSE,
    retained_clusters) {
  fit <- stats::glm(
    out ~ x,
    data = dat,
    family = stats::gaussian()
  )

  result <- clusterSEs::cluster.im.glm(
    mod = fit,
    dat = dat,
    cluster = ~ cluster,
    ci.level = 1 - alpha,
    report = FALSE,
    drop = TRUE,
    truncate = truncate,
    return.vcv = TRUE
  )

  variance <- unname(
    result$vcv.hat["x", "x"]
  )

  list(
    estimate = unname(
      result$beta.bar["x"]
    ),
    std_error = sqrt(
      variance / retained_clusters
    ),
    df = retained_clusters - 1L,
    p_value = unname(
      result$p.values["x", 1L]
    ),
    conf_low = unname(
      result$ci["x", 1L]
    ),
    conf_high = unname(
      result$ci["x", 2L]
    ),
    vcv_hat = result$vcv.hat,
    beta_bar = result$beta.bar,
    raw = result
  )
}

rca_public_robust_cats <- function(
    dat,
    engine = c("robust", "robustbase"),
    seed,
    alpha = 0.05,
    retained_clusters) {
  engine <- match.arg(engine)
  set.seed(seed)

  full_fit <- rca_fit_model(
    formula = out ~ x,
    data = dat,
    engine = engine
  )

  result <- cluster_im_lmRob(
    robmod = full_fit,
    formula = out ~ x,
    dat = dat,
    cluster = ~ cluster,
    ci.level = 1 - alpha,
    drop = TRUE,
    return.vcv = TRUE,
    engine = engine
  )

  variance <- unname(
    result$vcv.hat["x", "x"]
  )

  list(
    estimate = unname(
      result$beta.bar["x"]
    ),
    std_error = sqrt(
      variance / retained_clusters
    ),
    df = retained_clusters - 1L,
    p_value = unname(
      result$p.values["x", 1L]
    ),
    conf_low = unname(
      result$ci["x", 1L]
    ),
    conf_high = unname(
      result$ci["x", 2L]
    ),
    vcv_hat = result$vcv.hat,
    beta_bar = result$beta.bar,
    raw = result
  )
}

rca_simulation_robust_cats <- function(
    dat,
    engine = c("robust", "robustbase"),
    seed,
    alpha = 0.05) {
  engine <- match.arg(engine)
  set.seed(seed)

  study1_fit_robust_cats(
    dat = dat,
    alpha = alpha,
    engine = engine
  )
}

rca_result_values <- function(result) {
  c(
    estimate = result$estimate,
    std_error = result$std_error,
    df = result$df,
    p_value = result$p_value,
    conf_low = result$conf_low,
    conf_high = result$conf_high
  )
}

rca_compare_results <- function(
    reference,
    observed,
    comparison,
    tolerance = 1e-8) {
  reference_values <- rca_result_values(
    reference
  )
  observed_values <- rca_result_values(
    observed
  )

  quantities <- union(
    names(reference_values),
    names(observed_values)
  )

  rows <- lapply(
    quantities,
    function(quantity) {
      reference_value <- unname(
        reference_values[quantity]
      )
      observed_value <- unname(
        observed_values[quantity]
      )
      difference <- abs(
        reference_value -
          observed_value
      )

      data.frame(
        comparison = comparison,
        quantity = quantity,
        reference_value = reference_value,
        observed_value = observed_value,
        absolute_difference = difference,
        tolerance = tolerance,
        passed = isTRUE(
          is.finite(difference) &&
            difference <= tolerance
        ),
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(rbind, rows)
}

rca_compare_cluster_coefficients <- function(
    oracle_diagnostics,
    simulation_diagnostics,
    engine,
    tolerance = 1e-8) {
  oracle <- oracle_diagnostics[
    ,
    c(
      "cluster",
      "intercept",
      "x",
      "retained_before_truncation"
    ),
    drop = FALSE
  ]
  names(oracle)[
    names(oracle) ==
      "retained_before_truncation"
  ] <- "retained"

  simulation <- simulation_diagnostics[
    ,
    c(
      "cluster",
      "intercept",
      "x",
      "retained"
    ),
    drop = FALSE
  ]

  merged <- merge(
    oracle,
    simulation,
    by = "cluster",
    suffixes = c(
      "_oracle",
      "_simulation"
    ),
    all = TRUE,
    sort = TRUE
  )

  merged$engine <- engine
  merged$intercept_difference <- abs(
    merged$intercept_oracle -
      merged$intercept_simulation
  )
  merged$x_difference <- abs(
    merged$x_oracle -
      merged$x_simulation
  )
  merged$passed <- with(
    merged,
    retained_oracle ==
      retained_simulation &
      (
        !retained_oracle |
          (
            is.finite(
              intercept_difference
            ) &
              intercept_difference <=
              tolerance &
              is.finite(x_difference) &
              x_difference <= tolerance
          )
      )
  )

  merged
}

rca_mutated_aggregate <- function(
    coefficient_matrix,
    alpha = 0.05,
    mutation = c(
      "population_variance",
      "omit_sqrt_g",
      "wrong_df"
    )) {
  mutation <- match.arg(mutation)
  coefficient_matrix <- as.matrix(
    coefficient_matrix
  )
  g <- nrow(coefficient_matrix)
  focal <- coefficient_matrix[, "x"]
  estimate <- mean(focal)
  sample_variance <- stats::var(focal)

  if (identical(
    mutation,
    "population_variance"
  )) {
    variance_used <- sum(
      (focal - estimate)^2
    ) / g
    std_error <- sqrt(
      variance_used / g
    )
    df <- g - 1L
  } else if (identical(
    mutation,
    "omit_sqrt_g"
  )) {
    std_error <- sqrt(
      sample_variance
    )
    df <- g - 1L
  } else {
    std_error <- sqrt(
      sample_variance / g
    )
    df <- g
  }

  t_statistic <- estimate / std_error
  p_value <- 2 * stats::pt(
    abs(t_statistic),
    df = df,
    lower.tail = FALSE
  )
  critical_value <- stats::qt(
    1 - alpha / 2,
    df = df
  )

  list(
    estimate = estimate,
    std_error = std_error,
    df = df,
    p_value = p_value,
    conf_low = estimate -
      critical_value * std_error,
    conf_high = estimate +
      critical_value * std_error
  )
}

rca_bind_rows <- function(rows) {
  if (length(rows) == 0L) {
    return(data.frame())
  }

  all_names <- unique(unlist(
    lapply(rows, names),
    use.names = FALSE
  ))

  normalized <- lapply(
    rows,
    function(row) {
      missing <- setdiff(
        all_names,
        names(row)
      )

      for (name in missing) {
        row[[name]] <- NA
      }

      row[all_names]
    }
  )

  do.call(rbind, normalized)
}
