# Robust mixed-model pilot helpers
#
# These helpers are intentionally stored under data-raw rather than R/.
# The pilot is an experimental evaluation of robustlmm and does not alter the
# currently frozen mmiCATs simulation functions.

rmm_find_project_root <- function(path = getwd()) {
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

rmm_save_rds_atomic <- function(object,
                                path,
                                compress = "gzip") {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  temp_path <- tempfile(
    pattern = "rmm_",
    tmpdir = dirname(path),
    fileext = ".rds"
  )

  saveRDS(
    object,
    temp_path,
    version = 3,
    compress = compress
  )

  if (file.exists(path) && !file.remove(path)) {
    stop(
      paste("Could not replace existing file:", path),
      call. = FALSE
    )
  }

  if (!file.rename(temp_path, path)) {
    stop(
      paste("Could not save file:", path),
      call. = FALSE
    )
  }

  invisible(path)
}

rmm_write_csv_atomic <- function(object,
                                 path) {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  temp_path <- tempfile(
    pattern = "rmm_",
    tmpdir = dirname(path),
    fileext = ".csv"
  )

  utils::write.csv(
    object,
    temp_path,
    row.names = FALSE,
    na = ""
  )

  if (file.exists(path) && !file.remove(path)) {
    stop(
      paste("Could not replace existing file:", path),
      call. = FALSE
    )
  }

  if (!file.rename(temp_path, path)) {
    stop(
      paste("Could not save file:", path),
      call. = FALSE
    )
  }

  invisible(path)
}

rmm_collapse_text <- function(x) {
  x <- trimws(as.character(x))
  x <- unique(x[!is.na(x) & nzchar(x)])

  if (length(x) == 0L) {
    return(NA_character_)
  }

  paste(x, collapse = " | ")
}

rmm_has_text <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

rmm_mean_or_na <- function(x) {
  x <- x[is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  mean(x)
}

rmm_min_or_na <- function(x) {
  x <- x[is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  min(x)
}

rmm_max_or_na <- function(x) {
  x <- x[is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  max(x)
}

rmm_capture <- function(expr) {
  warnings <- character()
  messages <- character()
  started <- proc.time()[["elapsed"]]

  value <- withCallingHandlers(
    tryCatch(
      expr,
      error = function(e) e
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  elapsed <- proc.time()[["elapsed"]] - started

  if (inherits(value, "error")) {
    return(list(
      value = NULL,
      error = conditionMessage(value),
      warning = rmm_collapse_text(warnings),
      message = rmm_collapse_text(messages),
      elapsed_sec = unname(elapsed)
    ))
  }

  list(
    value = value,
    error = NA_character_,
    warning = rmm_collapse_text(warnings),
    message = rmm_collapse_text(messages),
    elapsed_sec = unname(elapsed)
  )
}

rmm_require_packages <- function() {
  required <- c(
    "pkgload",
    "lme4",
    "robustlmm"
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
        "Install the required package(s) before running the pilot: ",
        paste(missing, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  installed_version <- utils::packageVersion("robustlmm")
  minimum_version <- base::package_version("3.5.0-2")

  if (installed_version < minimum_version) {
    stop(
      paste0(
        "The pilot requires robustlmm >= 3.5.0-2 because it uses ",
        "the package's robust Satterthwaite inference. Installed: ",
        as.character(installed_version),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

rmm_is_boundary_fit <- function(fit,
                                tol = 1e-4) {
  theta <- tryCatch(
    as.numeric(
      robustlmm::getME(fit, "theta")
    ),
    error = function(e) numeric()
  )
  lower <- tryCatch(
    as.numeric(
      robustlmm::getME(fit, "lower")
    ),
    error = function(e) numeric()
  )

  if (length(theta) == 0L ||
      length(lower) != length(theta)) {
    return(NA)
  }

  diagonal <- lower == 0

  if (!any(diagonal)) {
    return(FALSE)
  }

  any(theta[diagonal] < tol)
}

rmm_model_formula <- function(model) {
  switch(
    model,
    study1_robust_ri =
      stats::as.formula("out ~ x + (1 | cluster)"),
    study2_robust_ri =
      stats::as.formula("out ~ x + (1 | cluster)"),
    study2_robust_rs =
      stats::as.formula("out ~ x + (1 + x || cluster)"),
    stop("Unknown robust mixed-model name.", call. = FALSE)
  )
}

rmm_model_label <- function(model) {
  switch(
    model,
    study1_robust_ri =
      "Study 1 robust random intercept",
    study2_robust_ri =
      "Study 2 robust random intercept",
    study2_robust_rs =
      "Study 2 robust random slope",
    model
  )
}

rmm_find_column <- function(column_names,
                            patterns) {
  normalized <- gsub(
    "[[:space:]]+",
    " ",
    trimws(column_names)
  )

  for (pattern in patterns) {
    match_index <- grep(
      pattern,
      normalized,
      ignore.case = TRUE
    )

    if (length(match_index) > 0L) {
      return(match_index[1L])
    }
  }

  NA_integer_
}

rmm_extract_coefficient_table <- function(summary_object,
                                          coefficient = "x") {
  coefficient_table <- tryCatch(
    stats::coef(summary_object),
    error = function(e) e
  )

  if (inherits(coefficient_table, "error")) {
    return(list(
      row = NULL,
      column_names = character(),
      error = conditionMessage(coefficient_table)
    ))
  }

  if (!is.matrix(coefficient_table) &&
      !is.data.frame(coefficient_table)) {
    return(list(
      row = NULL,
      column_names = character(),
      error = paste(
        "coef(summary(fit)) did not return a matrix",
        "or data frame."
      )
    ))
  }

  if (!(coefficient %in% rownames(coefficient_table))) {
    return(list(
      row = NULL,
      column_names = colnames(coefficient_table),
      error = paste(
        "Coefficient",
        coefficient,
        "was not found in the summary table."
      )
    ))
  }

  row <- coefficient_table[
    coefficient,
    ,
    drop = FALSE
  ]
  column_names <- colnames(row)

  estimate_col <- rmm_find_column(
    column_names,
    c("^Estimate$")
  )
  se_col <- rmm_find_column(
    column_names,
    c("^Std\\.? Error$", "standard error")
  )
  df_col <- rmm_find_column(
    column_names,
    c("^df$", "denominator.*df")
  )
  statistic_col <- rmm_find_column(
    column_names,
    c("^t value$", "^t-value$", "statistic")
  )
  p_col <- rmm_find_column(
    column_names,
    c("^Pr\\(", "p.value", "p value")
  )

  get_value <- function(index) {
    if (is.na(index)) {
      return(NA_real_)
    }

    as.numeric(row[1L, index])
  }

  list(
    estimate = get_value(estimate_col),
    std_error = get_value(se_col),
    df = get_value(df_col),
    statistic = get_value(statistic_col),
    p_value = get_value(p_col),
    row = row,
    column_names = column_names,
    error = NA_character_
  )
}

rmm_extract_process_fit <- function(fit) {
  processed <- rmm_capture(
    robustlmm::processFit(
      fit,
      all = FALSE,
      coefs = FALSE,
      stdErrors = FALSE,
      tValues = FALSE,
      sigma = FALSE,
      thetas = FALSE,
      b = FALSE,
      meanB = FALSE,
      meanAbsB = FALSE,
      residuals = FALSE,
      converged = TRUE,
      numWarnings = TRUE,
      procTime = FALSE
    )
  )

  convergence_code <- NA_real_
  process_warning_count <- NA_real_

  if (is.null(processed$error) ||
      is.na(processed$error)) {
    convergence_code <- tryCatch(
      as.numeric(processed$value$converged[1L]),
      error = function(e) NA_real_
    )
    process_warning_count <- tryCatch(
      as.numeric(processed$value$numberOfWarnings[1L]),
      error = function(e) NA_real_
    )
  }

  list(
    convergence_code = convergence_code,
    process_warning_count = process_warning_count,
    process_error = processed$error,
    process_warning = processed$warning,
    process_message = processed$message,
    process_elapsed_sec = processed$elapsed_sec,
    raw = processed$value
  )
}

rmm_extract_random_effect_sds <- function(fit) {
  variance_table <- tryCatch(
    as.data.frame(lme4::VarCorr(fit)),
    error = function(e) e
  )

  if (inherits(variance_table, "error")) {
    return(list(
      random_intercept_sd = NA_real_,
      random_slope_sd = NA_real_,
      residual_sd = NA_real_,
      table = NULL,
      error = conditionMessage(variance_table)
    ))
  }

  diagonal <- variance_table[
    is.na(variance_table$var2),
    ,
    drop = FALSE
  ]

  intercept_rows <- diagonal[
    diagonal$var1 == "(Intercept)" &
      diagonal$grp != "Residual",
    ,
    drop = FALSE
  ]
  slope_rows <- diagonal[
    diagonal$var1 == "x" &
      diagonal$grp != "Residual",
    ,
    drop = FALSE
  ]
  residual_rows <- diagonal[
    diagonal$grp == "Residual",
    ,
    drop = FALSE
  ]

  list(
    random_intercept_sd = if (
      nrow(intercept_rows) > 0L
    ) {
      as.numeric(intercept_rows$sdcor[1L])
    } else {
      NA_real_
    },
    random_slope_sd = if (
      nrow(slope_rows) > 0L
    ) {
      as.numeric(slope_rows$sdcor[1L])
    } else {
      NA_real_
    },
    residual_sd = if (
      nrow(residual_rows) > 0L
    ) {
      as.numeric(residual_rows$sdcor[1L])
    } else {
      tryCatch(
        as.numeric(stats::sigma(fit)),
        error = function(e) NA_real_
      )
    },
    table = variance_table,
    error = NA_character_
  )
}

rmm_extract_weights <- function(fit) {
  residual_weights <- tryCatch(
    robustlmm::getME(fit, "w_e"),
    error = function(e) numeric()
  )
  random_effect_weights <- tryCatch(
    robustlmm::getME(fit, "w_b_vector"),
    error = function(e) numeric()
  )

  summarize_weights <- function(weights,
                                prefix) {
    weights <- as.numeric(weights)
    weights <- weights[is.finite(weights)]

    if (length(weights) == 0L) {
      return(setNames(
        rep(NA_real_, 5L),
        paste0(
          prefix,
          c(
            "_minimum",
            "_mean",
            "_prop_below_0_5",
            "_prop_below_0_8",
            "_count"
          )
        )
      ))
    }

    setNames(
      c(
        min(weights),
        mean(weights),
        mean(weights < 0.5),
        mean(weights < 0.8),
        length(weights)
      ),
      paste0(
        prefix,
        c(
          "_minimum",
          "_mean",
          "_prop_below_0_5",
          "_prop_below_0_8",
          "_count"
        )
      )
    )
  }

  c(
    summarize_weights(
      residual_weights,
      "residual_weight"
    ),
    summarize_weights(
      random_effect_weights,
      "random_effect_weight"
    )
  )
}

rmm_fit_and_extract <- function(dat,
                                model,
                                beta,
                                alpha = 0.05,
                                return_fit = FALSE) {
  formula <- rmm_model_formula(model)

  fit_capture <- rmm_capture(
    robustlmm::rlmer(
      formula = formula,
      data = dat,
      method = "DAStau"
    )
  )

  empty <- list(
    model = model,
    model_label = rmm_model_label(model),
    estimate = NA_real_,
    std_error = NA_real_,
    df = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    reject = NA,
    cover = NA,
    fit_available = FALSE,
    inference_complete = FALSE,
    convergence_code = NA_real_,
    convergence_code_zero = NA,
    boundary_fit = NA,
    estimated_random_intercept_sd = NA_real_,
    estimated_random_slope_sd = NA_real_,
    estimated_residual_sd = NA_real_,
    residual_weight_minimum = NA_real_,
    residual_weight_mean = NA_real_,
    residual_weight_prop_below_0_5 = NA_real_,
    residual_weight_prop_below_0_8 = NA_real_,
    residual_weight_count = NA_real_,
    random_effect_weight_minimum = NA_real_,
    random_effect_weight_mean = NA_real_,
    random_effect_weight_prop_below_0_5 = NA_real_,
    random_effect_weight_prop_below_0_8 = NA_real_,
    random_effect_weight_count = NA_real_,
    fit_warning = fit_capture$warning,
    fit_message = fit_capture$message,
    fit_error = fit_capture$error,
    inference_warning = NA_character_,
    inference_message = NA_character_,
    inference_error = NA_character_,
    process_warning = NA_character_,
    process_message = NA_character_,
    process_error = NA_character_,
    variance_component_error = NA_character_,
    summary_column_names = NA_character_,
    fit_elapsed_sec = fit_capture$elapsed_sec,
    inference_elapsed_sec = NA_real_,
    process_elapsed_sec = NA_real_,
    total_elapsed_sec = NA_real_,
    fit = NULL
  )

  if (!is.na(fit_capture$error)) {
    empty$total_elapsed_sec <- fit_capture$elapsed_sec
    return(empty)
  }

  fit <- fit_capture$value
  empty$fit_available <- TRUE

  inference_capture <- rmm_capture(
    summary(
      fit,
      df = "satterthwaite"
    )
  )

  empty$inference_warning <- inference_capture$warning
  empty$inference_message <- inference_capture$message
  empty$inference_error <- inference_capture$error
  empty$inference_elapsed_sec <-
    inference_capture$elapsed_sec

  process <- rmm_extract_process_fit(fit)
  empty$convergence_code <- process$convergence_code
  empty$convergence_code_zero <- if (
    is.finite(process$convergence_code)
  ) {
    process$convergence_code == 0
  } else {
    NA
  }
  empty$process_warning <- process$process_warning
  empty$process_message <- process$process_message
  empty$process_error <- process$process_error
  empty$process_elapsed_sec <-
    process$process_elapsed_sec

  empty$boundary_fit <- rmm_is_boundary_fit(
    fit,
    tol = 1e-4
  )

  variance_components <- rmm_extract_random_effect_sds(
    fit
  )
  empty$estimated_random_intercept_sd <-
    variance_components$random_intercept_sd
  empty$estimated_random_slope_sd <-
    variance_components$random_slope_sd
  empty$estimated_residual_sd <-
    variance_components$residual_sd
  empty$variance_component_error <-
    variance_components$error

  weight_summary <- rmm_extract_weights(fit)
  for (name in names(weight_summary)) {
    empty[[name]] <- unname(weight_summary[name])
  }

  if (!is.na(inference_capture$error)) {
    empty$total_elapsed_sec <- sum(
      c(
        empty$fit_elapsed_sec,
        empty$inference_elapsed_sec,
        empty$process_elapsed_sec
      ),
      na.rm = TRUE
    )

    if (return_fit) {
      empty$fit <- fit
    }

    return(empty)
  }

  extracted <- rmm_extract_coefficient_table(
    summary_object = inference_capture$value,
    coefficient = "x"
  )

  empty$summary_column_names <- paste(
    extracted$column_names,
    collapse = " | "
  )

  if (!is.na(extracted$error)) {
    empty$inference_error <- rmm_collapse_text(
      c(
        empty$inference_error,
        extracted$error
      )
    )
  } else {
    empty$estimate <- extracted$estimate
    empty$std_error <- extracted$std_error
    empty$df <- extracted$df
    empty$statistic <- extracted$statistic
    empty$p_value <- extracted$p_value

    if (!is.finite(empty$statistic) &&
        is.finite(empty$estimate) &&
        is.finite(empty$std_error) &&
        empty$std_error > 0) {
      empty$statistic <-
        empty$estimate / empty$std_error
    }

    if (!is.finite(empty$p_value) &&
        is.finite(empty$statistic) &&
        is.finite(empty$df) &&
        empty$df > 0) {
      empty$p_value <- 2 * stats::pt(
        -abs(empty$statistic),
        df = empty$df
      )
    }

    if (is.finite(empty$estimate) &&
        is.finite(empty$std_error) &&
        is.finite(empty$df) &&
        empty$df > 0) {
      critical_value <- stats::qt(
        1 - alpha / 2,
        df = empty$df
      )
      empty$conf_low <-
        empty$estimate -
        critical_value * empty$std_error
      empty$conf_high <-
        empty$estimate +
        critical_value * empty$std_error
    }

    empty$inference_complete <- all(is.finite(c(
      empty$estimate,
      empty$std_error,
      empty$df,
      empty$statistic,
      empty$p_value,
      empty$conf_low,
      empty$conf_high
    ))) &&
      empty$std_error > 0 &&
      empty$df > 0

    if (empty$inference_complete) {
      empty$reject <- empty$p_value < alpha
      empty$cover <- beta >= empty$conf_low &&
        beta <= empty$conf_high
    }
  }

  empty$total_elapsed_sec <- sum(
    c(
      empty$fit_elapsed_sec,
      empty$inference_elapsed_sec,
      empty$process_elapsed_sec
    ),
    na.rm = TRUE
  )

  if (return_fit) {
    empty$fit <- fit
  }

  empty
}

rmm_result_to_row <- function(result,
                              replicate,
                              replicate_seed) {
  fields <- setdiff(
    names(result),
    "fit"
  )

  row <- as.data.frame(
    result[fields],
    stringsAsFactors = FALSE,
    optional = TRUE
  )

  row$replicate <- replicate
  row$replicate_seed <- replicate_seed

  row$usable <- row$fit_available &&
    row$inference_complete &&
    (
      is.na(row$convergence_code_zero) ||
        row$convergence_code_zero
    ) &&
    !rmm_has_text(row$fit_error) &&
    !rmm_has_text(row$inference_error)

  row
}

rmm_simulate_study1 <- function(n_clusters,
                                contamination) {
  simulate <- getFromNamespace(
    "study1_simulate_data",
    "mmiCATs"
  )

  contamination_size <- switch(
    contamination,
    none = 1,
    vertical = 6,
    bad_leverage = 0.375
  )
  leverage_size <- switch(
    contamination,
    none = 1,
    vertical = 1,
    bad_leverage = 4
  )

  simulate(
    n_clusters = n_clusters,
    cluster_size = 40,
    beta = 0,
    intercept = 0,
    random_intercept_sd = 1,
    residual_sd = 1,
    x_sd = 1,
    contamination = contamination,
    contamination_prop = 0.05,
    contamination_size = contamination_size,
    leverage_size = leverage_size
  )
}

rmm_simulate_study2 <- function(n_clusters,
                                random_slope_sd,
                                contamination) {
  simulate <- getFromNamespace(
    "study2_simulate_data",
    "mmiCATs"
  )

  simulate(
    n_clusters = n_clusters,
    cluster_size = 40,
    beta = 0,
    intercept = 0,
    random_intercept_sd = 1,
    random_slope_sd = random_slope_sd,
    residual_sd = 1,
    x_sd = 1,
    contamination = contamination,
    contamination_prop = 0.05,
    contamination_size = if (
      contamination == "none"
    ) {
      1
    } else {
      6
    }
  )
}

rmm_checkpoint_path <- function(checkpoint_dir,
                                condition_id) {
  file.path(
    checkpoint_dir,
    paste0(
      "condition_",
      condition_id,
      ".rds"
    )
  )
}

rmm_read_checkpoint <- function(path) {
  tryCatch(
    readRDS(path),
    error = function(e) {
      list(
        status = "unreadable",
        condition = NULL,
        results = NULL,
        error = conditionMessage(e),
        started_at = as.POSIXct(NA),
        completed_at = as.POSIXct(NA),
        elapsed_sec = NA_real_
      )
    }
  )
}

rmm_collect_checkpoints <- function(checkpoint_dir) {
  paths <- sort(list.files(
    checkpoint_dir,
    pattern = "^condition_RMM[0-9]{3}[.]rds$",
    full.names = TRUE
  ))

  if (length(paths) == 0L) {
    return(list())
  }

  lapply(paths, rmm_read_checkpoint)
}

rmm_collect_status <- function(checkpoints,
                               design) {
  checkpoint_by_id <- list()

  for (checkpoint in checkpoints) {
    if (!is.null(checkpoint$condition)) {
      checkpoint_by_id[[checkpoint$condition$condition_id]] <- checkpoint
    }
  }

  rows <- lapply(
    seq_len(nrow(design)),
    function(index) {
      condition <- design[index, , drop = FALSE]
      checkpoint <- checkpoint_by_id[[condition$condition_id]]

      if (is.null(checkpoint)) {
        return(data.frame(
          condition,
          status = "not_started",
          condition_error = NA_character_,
          started_at = NA_character_,
          completed_at = NA_character_,
          elapsed_sec = NA_real_,
          stringsAsFactors = FALSE
        ))
      }

      data.frame(
        condition,
        status = checkpoint$status,
        condition_error = checkpoint$error,
        started_at = as.character(
          checkpoint$started_at
        ),
        completed_at = as.character(
          checkpoint$completed_at
        ),
        elapsed_sec = checkpoint$elapsed_sec,
        stringsAsFactors = FALSE
      )
    }
  )

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

rmm_bind_rows_fill <- function(data_list) {
  data_list <- data_list[
    vapply(
      data_list,
      function(x) {
        is.data.frame(x) && nrow(x) > 0L
      },
      logical(1)
    )
  ]

  if (length(data_list) == 0L) {
    return(data.frame())
  }

  all_names <- unique(unlist(
    lapply(data_list, names),
    use.names = FALSE
  ))

  aligned <- lapply(data_list, function(data) {
    missing <- setdiff(all_names, names(data))

    for (name in missing) {
      data[[name]] <- NA
    }

    data[, all_names, drop = FALSE]
  })

  out <- do.call(rbind, aligned)
  rownames(out) <- NULL
  out
}

rmm_source_checksums <- function(project_root,
                                 files) {
  paths <- c(
    pwr_func_study1 = file.path(
      project_root,
      "R",
      "pwr_func_study1.R"
    ),
    pwr_func_study1_helpers = file.path(
      project_root,
      "R",
      "pwr_func_study1_helpers.R"
    ),
    pwr_func_study2 = file.path(
      project_root,
      "R",
      "pwr_func_study2.R"
    ),
    pwr_func_study2_helpers = file.path(
      project_root,
      "R",
      "pwr_func_study2_helpers.R"
    ),
    files
  )

  exists <- file.exists(paths)
  paths <- paths[exists]

  data.frame(
    source = names(paths),
    path = normalizePath(
      paths,
      winslash = "/",
      mustWork = TRUE
    ),
    md5 = unname(tools::md5sum(paths)),
    stringsAsFactors = FALSE
  )
}
