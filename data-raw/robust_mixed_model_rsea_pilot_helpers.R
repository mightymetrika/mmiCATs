# RSEa random-slope comparison pilot helpers
#
# These helpers supplement robust_mixed_model_pilot_helpers.R. They do not
# modify the package functions under R/.

rmm_fit_and_extract_rlmer_setting <- function(
    dat,
    beta,
    setting = c("RSEa", "RSEn"),
    alpha = 0.05,
    return_fit = FALSE) {
  setting <- match.arg(setting)

  fit_capture <- rmm_capture(
    robustlmm::rlmer(
      formula = out ~ x + (1 + x || cluster),
      data = dat,
      method = "DAStau",
      setting = setting
    )
  )

  empty <- list(
    method = paste0("rlmer_", setting),
    method_label = paste0(
      "Robust random slope (",
      setting,
      ")"
    ),
    setting = setting,
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

rmm_is_singularity_message <- function(message) {
  if (is.na(message) || !nzchar(trimws(message))) {
    return(FALSE)
  }

  grepl(
    "boundary[[:space:]]*[(]singular[)][[:space:]]*fit",
    message,
    ignore.case = TRUE
  )
}

rmm_lmer_convergence_details <- function(fit) {
  messages <- tryCatch(
    fit@optinfo$conv$lme4$messages,
    error = function(e) NULL
  )
  messages <- as.character(messages)
  messages <- messages[
    !is.na(messages) & nzchar(trimws(messages))
  ]

  optimizer_code <- tryCatch(
    as.numeric(fit@optinfo$conv$opt),
    error = function(e) NA_real_
  )

  substantive_messages <- messages[
    !vapply(
      messages,
      rmm_is_singularity_message,
      logical(1)
    )
  ]

  list(
    optimizer_code = optimizer_code,
    optimizer_code_zero = if (
      is.finite(optimizer_code)
    ) {
      optimizer_code == 0
    } else {
      NA
    },
    convergence_messages = rmm_collapse_text(
      substantive_messages
    ),
    singularity_messages = rmm_collapse_text(
      messages[
        vapply(
          messages,
          rmm_is_singularity_message,
          logical(1)
        )
      ]
    )
  )
}

rmm_extract_lmer_random_effect_sds <- function(fit) {
  variance_table <- tryCatch(
    as.data.frame(lme4::VarCorr(fit)),
    error = function(e) e
  )

  if (inherits(variance_table, "error")) {
    return(list(
      random_intercept_sd = NA_real_,
      random_slope_sd = NA_real_,
      residual_sd = NA_real_,
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
    error = NA_character_
  )
}

rmm_fit_and_extract_lmer_rs <- function(
    dat,
    beta,
    alpha = 0.05,
    return_fit = FALSE) {
  fit_capture <- rmm_capture(
    lmerTest::lmer(
      out ~ x + (1 + x || cluster),
      data = dat,
      REML = TRUE
    )
  )

  empty <- list(
    method = "lmer_kr",
    method_label = "Conventional random slope (KR)",
    setting = NA_character_,
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
    process_elapsed_sec = 0,
    total_elapsed_sec = NA_real_,
    fit = NULL
  )

  if (!is.na(fit_capture$error)) {
    empty$total_elapsed_sec <- fit_capture$elapsed_sec
    return(empty)
  }

  fit <- fit_capture$value
  empty$fit_available <- TRUE

  convergence <- rmm_lmer_convergence_details(fit)
  empty$convergence_code <- convergence$optimizer_code
  empty$convergence_code_zero <-
    convergence$optimizer_code_zero
  empty$process_warning <-
    convergence$convergence_messages
  empty$process_message <-
    convergence$singularity_messages

  empty$boundary_fit <- tryCatch(
    lme4::isSingular(fit, tol = 1e-4),
    error = function(e) NA
  )

  variance_components <-
    rmm_extract_lmer_random_effect_sds(fit)
  empty$estimated_random_intercept_sd <-
    variance_components$random_intercept_sd
  empty$estimated_random_slope_sd <-
    variance_components$random_slope_sd
  empty$estimated_residual_sd <-
    variance_components$residual_sd
  empty$variance_component_error <-
    variance_components$error

  inference_capture <- rmm_capture(
    summary(
      fit,
      ddf = "Kenward-Roger"
    )
  )

  empty$inference_warning <- inference_capture$warning
  empty$inference_message <- inference_capture$message
  empty$inference_error <- inference_capture$error
  empty$inference_elapsed_sec <-
    inference_capture$elapsed_sec

  if (!is.na(inference_capture$error)) {
    empty$total_elapsed_sec <- sum(
      c(
        empty$fit_elapsed_sec,
        empty$inference_elapsed_sec
      ),
      na.rm = TRUE
    )

    if (return_fit) {
      empty$fit <- fit
    }

    return(empty)
  }

  coefficient_table <- stats::coef(
    inference_capture$value
  )
  empty$summary_column_names <- paste(
    colnames(coefficient_table),
    collapse = " | "
  )

  if (!("x" %in% rownames(coefficient_table))) {
    empty$inference_error <-
      "Coefficient x was not found."
  } else {
    row <- coefficient_table[
      "x",
      ,
      drop = FALSE
    ]

    empty$estimate <- as.numeric(
      row[1L, "Estimate"]
    )
    empty$std_error <- as.numeric(
      row[1L, "Std. Error"]
    )
    empty$df <- as.numeric(
      row[1L, "df"]
    )
    empty$statistic <- as.numeric(
      row[1L, "t value"]
    )

    p_column <- grep(
      "^Pr[(]",
      colnames(row),
      value = TRUE
    )

    if (length(p_column) > 0L) {
      empty$p_value <- as.numeric(
        row[1L, p_column[1L]]
      )
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
      empty$inference_elapsed_sec
    ),
    na.rm = TRUE
  )

  if (return_fit) {
    empty$fit <- fit
  }

  empty
}

rmm_comparison_result_to_row <- function(
    result,
    condition_id,
    replicate,
    replicate_seed,
    n_clusters,
    random_slope_sd,
    contamination,
    contamination_label,
    beta = 0) {
  fields <- setdiff(
    names(result),
    "fit"
  )

  row <- as.data.frame(
    result[fields],
    stringsAsFactors = FALSE,
    optional = TRUE
  )

  row$condition_id <- condition_id
  row$replicate <- replicate
  row$replicate_seed <- replicate_seed
  row$n_clusters <- n_clusters
  row$cluster_size <- 40L
  row$random_slope_sd <- random_slope_sd
  row$contamination <- contamination
  row$contamination_label <-
    contamination_label
  row$beta <- beta

  row$usable <- row$fit_available &&
    row$inference_complete &&
    (
      is.na(row$convergence_code_zero) ||
        row$convergence_code_zero
    ) &&
    !rmm_has_text(row$fit_error) &&
    !rmm_has_text(row$inference_error) &&
    !rmm_has_text(row$process_warning)

  row
}
