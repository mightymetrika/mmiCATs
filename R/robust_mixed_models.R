# Robust mixed-model helpers for the simulation studies


#' Fit a Robust Mixed-Effects Comparator
#'
#' Fits the robust random-intercept or independent random-slope comparator used
#' by the simulation studies. The robust fit uses
#' `robustlmm::rlmer(method = "DAStau", setting = "RSEn")`. Fixed-effect
#' inference uses robust Satterthwaite degrees of freedom and the package's
#' default covariance. Boundary fits are recorded separately and are not, by
#' themselves, treated as convergence failures.
#'
#' @param dat Simulated data containing `out`, `x`, and `cluster`.
#' @param alpha Significance level used for the confidence interval.
#' @param model Robust mixed-model structure: `"ri"` for a random intercept or
#'   `"rs"` for independent random-intercept and random-slope terms.
#'
#' @return A standardized result list compatible with the Study 1 and Study 2
#'   simulation result machinery.
#'
#' @keywords internal
study_fit_robust_mixed <- function(dat,
                                   alpha,
                                   model = c("ri", "rs")) {
  model <- match.arg(model)

  study_require_robustlmm()

  formula <- if (identical(model, "ri")) {
    stats::as.formula(
      "out ~ x + (1 | cluster)"
    )
  } else {
    stats::as.formula(
      "out ~ x + (1 + x || cluster)"
    )
  }

  fit_conditions <- study_capture_robust_mixed_conditions(
    robustlmm::rlmer(
      formula = formula,
      data = dat,
      method = "DAStau",
      setting = "RSEn"
    )
  )
  fit <- fit_conditions$value

  inference_conditions <- study_capture_robust_mixed_conditions(
    summary(
      fit,
      df = "satterthwaite"
    )
  )
  fit_summary <- inference_conditions$value

  coefficient_table <- stats::coef(
    fit_summary
  )

  if (!("x" %in% rownames(coefficient_table))) {
    stop(
      "Coefficient 'x' was not returned by the robust mixed model.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "Estimate",
    "Std. Error",
    "df"
  )
  missing_columns <- setdiff(
    required_columns,
    colnames(coefficient_table)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "The robust Satterthwaite summary is missing: ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  coefficient_row <- coefficient_table[
    "x",
    ,
    drop = FALSE
  ]

  estimate <- as.numeric(
    coefficient_row[1L, "Estimate"]
  )
  std_error <- as.numeric(
    coefficient_row[1L, "Std. Error"]
  )
  df <- as.numeric(
    coefficient_row[1L, "df"]
  )

  statistic_column <- grep(
    "^t value$|^t-value$|statistic",
    colnames(coefficient_row),
    ignore.case = TRUE,
    value = TRUE
  )
  p_value_column <- grep(
    "^Pr[(]|p[.]value|p value",
    colnames(coefficient_row),
    ignore.case = TRUE,
    value = TRUE
  )

  statistic <- if (length(statistic_column) > 0L) {
    as.numeric(
      coefficient_row[
        1L,
        statistic_column[1L]
      ]
    )
  } else {
    estimate / std_error
  }

  p_value <- if (length(p_value_column) > 0L) {
    as.numeric(
      coefficient_row[
        1L,
        p_value_column[1L]
      ]
    )
  } else {
    2 * stats::pt(
      -abs(statistic),
      df = df
    )
  }

  required_inference <- c(
    estimate,
    std_error,
    df,
    statistic,
    p_value
  )

  if (
    any(!is.finite(required_inference)) ||
    !is.finite(std_error) ||
    std_error <= 0 ||
    !is.finite(df) ||
    df <= 0
  ) {
    stop(
      "The robust mixed model did not return finite fixed-effect inference.",
      call. = FALSE
    )
  }

  critical_value <- stats::qt(
    1 - alpha / 2,
    df = df
  )

  process_conditions <- study_capture_robust_mixed_conditions(
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
  process_fit <- process_conditions$value

  convergence_code <- tryCatch(
    as.numeric(
      process_fit$converged[1L]
    ),
    error = function(e) NA_real_
  )

  if (!is.finite(convergence_code)) {
    stop(
      "robustlmm::processFit() did not return a finite convergence code.",
      call. = FALSE
    )
  }

  converged <- convergence_code == 0
  boundary_fit <- study_robust_mixed_boundary(
    fit,
    tol = 1e-4
  )
  fitted_sds <- study_robust_mixed_random_effect_sds(
    fit
  )

  warning_text <- study1_collapse_messages(
    c(
      fit_conditions$warning,
      fit_conditions$message,
      inference_conditions$warning,
      inference_conditions$message,
      process_conditions$warning,
      process_conditions$message
    )
  )

  optimizer_warning <- if (converged) {
    NA_character_
  } else {
    paste(
      "robustlmm processFit convergence code:",
      convergence_code
    )
  }

  list(
    estimate = estimate,
    std_error = std_error,
    df = df,
    p_value = p_value,
    conf_low = estimate -
      critical_value * std_error,
    conf_high = estimate +
      critical_value * std_error,
    converged = converged,
    singular = boundary_fit,
    retained_clusters = nlevels(
      factor(dat$cluster)
    ),
    warning = warning_text,
    optimizer_warning = optimizer_warning,
    optimizer_code = convergence_code,
    estimated_random_intercept_sd = unname(
      fitted_sds[
        "random_intercept_sd"
      ]
    ),
    estimated_random_slope_sd = unname(
      fitted_sds[
        "random_slope_sd"
      ]
    )
  )
}


#' Require the Robust Mixed-Model Dependency
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
study_require_robustlmm <- function() {
  if (!requireNamespace(
    "robustlmm",
    quietly = TRUE
  )) {
    stop(
      paste(
        "Package 'robustlmm' is required for the robust",
        "mixed-model simulation comparators."
      ),
      call. = FALSE
    )
  }

  installed_version <- utils::packageVersion(
    "robustlmm"
  )
  minimum_version <- base::package_version(
    "3.5.0-2"
  )

  if (installed_version < minimum_version) {
    stop(
      paste0(
        "The robust mixed-model comparators require robustlmm >= 3.5.0-2. ",
        "Installed: ",
        as.character(installed_version),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(NULL)
}


#' Capture Robust Mixed-Model Conditions
#'
#' Captures warnings and messages while preserving an error as an error. This
#' keeps routine boundary messages out of long simulation runs while retaining
#' them in the returned diagnostics.
#'
#' @param expr Expression that fits or summarizes a robust mixed model.
#'
#' @return A list containing the value, warning text, and message text.
#'
#' @keywords internal
study_capture_robust_mixed_conditions <- function(expr) {
  warnings <- character()
  messages <- character()

  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(
        warnings,
        conditionMessage(w)
      )
      invokeRestart(
        "muffleWarning"
      )
    },
    message = function(m) {
      messages <<- c(
        messages,
        conditionMessage(m)
      )
      invokeRestart(
        "muffleMessage"
      )
    }
  )

  list(
    value = value,
    warning = study1_collapse_messages(
      warnings
    ),
    message = study1_collapse_messages(
      messages
    )
  )
}


#' Identify a Robust Mixed-Model Boundary Fit
#'
#' Uses the same theta/lower criterion used in the locked pilot
#' specification. Boundary status is diagnostic rather than an automatic
#' failure criterion.
#'
#' @param fit A fitted `robustlmm` model.
#' @param tol Boundary tolerance.
#'
#' @return A logical scalar.
#'
#' @keywords internal
study_robust_mixed_boundary <- function(fit,
                                        tol = 1e-4) {
  theta <- as.numeric(
    robustlmm::getME(
      fit,
      "theta"
    )
  )
  lower <- as.numeric(
    robustlmm::getME(
      fit,
      "lower"
    )
  )

  if (
    length(theta) == 0L ||
    length(lower) != length(theta)
  ) {
    stop(
      "Could not determine robust mixed-model boundary status.",
      call. = FALSE
    )
  }

  diagonal <- lower == 0

  if (!any(diagonal)) {
    return(FALSE)
  }

  any(
    theta[diagonal] < tol
  )
}


#' Extract Robust Mixed-Model Random-Effect Standard Deviations
#'
#' @param fit A fitted `robustlmm` model.
#'
#' @return A named numeric vector containing random-intercept and random-slope
#'   standard deviations.
#'
#' @keywords internal
study_robust_mixed_random_effect_sds <- function(fit) {
  variance_components <- as.data.frame(
    lme4::VarCorr(fit)
  )

  random_rows <-
    variance_components$grp !=
      "Residual" &
    is.na(
      variance_components$var2
    )

  intercept_row <-
    random_rows &
    variance_components$var1 ==
      "(Intercept)"

  slope_row <-
    random_rows &
    variance_components$var1 ==
      "x"

  c(
    random_intercept_sd = if (
      any(intercept_row)
    ) {
      as.numeric(
        variance_components$sdcor[
          which(intercept_row)[1L]
        ]
      )
    } else {
      NA_real_
    },
    random_slope_sd = if (
      any(slope_row)
    ) {
      as.numeric(
        variance_components$sdcor[
          which(slope_row)[1L]
        ]
      )
    } else {
      NA_real_
    }
  )
}
