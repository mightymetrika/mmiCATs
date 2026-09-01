# Clustered-data diagnostic tools


# Declare data-frame columns used inside ggplot2 aesthetics so R CMD check
# does not interpret them as undefined global variables. These names are
# created locally by the diagnostic functions; this declaration changes no
# runtime behavior.
utils::globalVariables(
  c(
    "cluster_plot",
    "n_total",
    "x_sd",
    "x",
    "out",
    "missing_percent",
    "variable",
    "estimate",
    "method_label",
    "conf_low",
    "conf_high",
    "slope",
    "engine",
    "slope_difference",
    "comparison",
    "leverage",
    "standardized_residual",
    "weight",
    "intercept",
    "omitted_cluster",
    "estimate_change"
  )
)


#' Explore Clustered Data Before Model Fitting
#'
#' Produces structured pre-model summaries and plots for a clustered linear
#' analysis with one continuous focal predictor. The function is descriptive:
#' it does not select or recommend an estimator.
#'
#' @param formula A two-sided formula of the form `outcome ~ predictor`. The
#'   initial implementation supports one untransformed numeric predictor.
#' @param cluster A one-sided formula such as `~ school`, or a character string
#'   naming the clustering variable.
#' @param data A data frame.
#'
#' @return A list containing `overall`, `cluster_summary`, `missingness`,
#'   `cluster_missingness`, `analysis_data`, and `plots`. `plots` is a named
#'   list of `ggplot2` objects.
#'
#' @examples
#' dat <- data.frame(
#'   y = rnorm(60),
#'   x = rnorm(60),
#'   id = rep(1:6, each = 10)
#' )
#' out <- cluster_data_explore(y ~ x, ~ id, dat)
#' out$overall
#' out$cluster_summary
#'
#' @export
cluster_data_explore <- function(formula,
                                 cluster,
                                 data) {
  spec <- cluster_diag_validate_specification(
    formula = formula,
    cluster = cluster,
    data = data
  )

  prepared <- cluster_diag_prepare_data(
    data = data,
    response = spec$response,
    predictor = spec$predictor,
    cluster = spec$cluster
  )

  all_data <- prepared$all_data
  analysis_data <- prepared$analysis_data

  cluster_levels <- levels(
    analysis_data$cluster
  )

  cluster_summary <- do.call(
    rbind,
    lapply(
      cluster_levels,
      function(cluster_id) {
        rows <- analysis_data$cluster == cluster_id
        d <- analysis_data[
          rows,
          ,
          drop = FALSE
        ]

        n_total <- nrow(d)
        complete <- stats::complete.cases(
          d[
            ,
            c("out", "x"),
            drop = FALSE
          ]
        )
        dc <- d[
          complete,
          ,
          drop = FALSE
        ]

        n_complete <- nrow(dc)
        unique_x <- length(
          unique(dc$x)
        )

        design_rank <- if (n_complete >= 2L) {
          qr(
            cbind(
              1,
              dc$x
            )
          )$rank
        } else {
          NA_integer_
        }

        data.frame(
          cluster = cluster_id,
          n_total = n_total,
          n_complete = n_complete,
          n_missing_analysis =
            n_total - n_complete,
          unique_x = unique_x,
          design_rank = design_rank,
          estimable_slope =
            is.finite(design_rank) &&
            design_rank >= 2L,
          x_mean =
            cluster_diag_mean(dc$x),
          x_sd =
            cluster_diag_sd(dc$x),
          x_min =
            cluster_diag_min(dc$x),
          x_max =
            cluster_diag_max(dc$x),
          x_range =
            cluster_diag_range(dc$x),
          out_mean =
            cluster_diag_mean(dc$out),
          out_sd =
            cluster_diag_sd(dc$out),
          out_median =
            cluster_diag_median(dc$out),
          out_iqr =
            cluster_diag_iqr(dc$out),
          out_min =
            cluster_diag_min(dc$out),
          out_max =
            cluster_diag_max(dc$out),
          out_range =
            cluster_diag_range(dc$out),
          stringsAsFactors = FALSE
        )
      }
    )
  )
  rownames(cluster_summary) <- NULL

  missingness <- data.frame(
    variable = c(
      spec$response,
      spec$predictor,
      spec$cluster
    ),
    missing_n = c(
      sum(is.na(all_data$out)),
      sum(is.na(all_data$x)),
      sum(is.na(all_data$cluster_raw))
    ),
    total_n = nrow(all_data),
    stringsAsFactors = FALSE
  )
  missingness$missing_percent <-
    100 * missingness$missing_n /
    missingness$total_n

  cluster_missingness <- do.call(
    rbind,
    lapply(
      cluster_levels,
      function(cluster_id) {
        d <- analysis_data[
          analysis_data$cluster == cluster_id,
          ,
          drop = FALSE
        ]

        data.frame(
          cluster = cluster_id,
          variable = c(
            spec$response,
            spec$predictor
          ),
          missing_n = c(
            sum(is.na(d$out)),
            sum(is.na(d$x))
          ),
          total_n = nrow(d),
          stringsAsFactors = FALSE
        )
      }
    )
  )
  rownames(cluster_missingness) <- NULL
  cluster_missingness$missing_percent <-
    100 * cluster_missingness$missing_n /
    cluster_missingness$total_n

  cluster_sizes <- cluster_summary$n_total
  complete_sizes <-
    cluster_summary$n_complete

  overall <- data.frame(
    response = spec$response,
    predictor = spec$predictor,
    cluster_variable = spec$cluster,
    observations = nrow(all_data),
    observations_with_cluster =
      nrow(analysis_data),
    complete_analysis_observations =
      sum(
        stats::complete.cases(
          analysis_data[
            ,
            c("out", "x"),
            drop = FALSE
          ]
        )
      ),
    missing_cluster_observations =
      sum(is.na(all_data$cluster_raw)),
    clusters = nrow(cluster_summary),
    min_cluster_size =
      cluster_diag_min(cluster_sizes),
    median_cluster_size =
      cluster_diag_median(cluster_sizes),
    max_cluster_size =
      cluster_diag_max(cluster_sizes),
    min_complete_cluster_size =
      cluster_diag_min(complete_sizes),
    median_complete_cluster_size =
      cluster_diag_median(complete_sizes),
    max_complete_cluster_size =
      cluster_diag_max(complete_sizes),
    clusters_with_estimable_slope =
      sum(
        cluster_summary$estimable_slope,
        na.rm = TRUE
      ),
    clusters_without_estimable_slope =
      sum(
        !cluster_summary$estimable_slope |
          is.na(
            cluster_summary$estimable_slope
          )
      ),
    stringsAsFactors = FALSE
  )

  plot_data <- analysis_data[
    stats::complete.cases(
      analysis_data[
        ,
        c("out", "x"),
        drop = FALSE
      ]
    ),
    ,
    drop = FALSE
  ]

  cluster_order <- cluster_summary$cluster[
    order(
      cluster_summary$n_total
    )
  ]

  cluster_summary$cluster_plot <-
    factor(
      cluster_summary$cluster,
      levels = cluster_order
    )

  plot_data$cluster_plot <-
    factor(
      as.character(plot_data$cluster),
      levels = cluster_order
    )

  cluster_missingness$cluster_plot <-
    factor(
      cluster_missingness$cluster,
      levels = cluster_order
    )

  plots <- list(
    cluster_size =
      ggplot2::ggplot(
        cluster_summary,
        ggplot2::aes(
          x = cluster_plot,
          y = n_total
        )
      ) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = spec$cluster,
        y = "Observations",
        title = "Cluster sizes"
      ) +
      ggplot2::theme_minimal(),

    predictor_variation =
      ggplot2::ggplot(
        cluster_summary,
        ggplot2::aes(
          x = cluster_plot,
          y = x_sd
        )
      ) +
      ggplot2::geom_point() +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = spec$cluster,
        y = paste0(
          "Within-cluster SD of ",
          spec$predictor
        ),
        title = "Within-cluster predictor variation"
      ) +
      ggplot2::theme_minimal(),

    raw_scatter =
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
          x = x,
          y = out
        )
      ) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(
        method = "lm",
        se = FALSE
      ) +
      ggplot2::facet_wrap(
        ~ cluster_plot
      ) +
      ggplot2::labs(
        x = spec$predictor,
        y = spec$response,
        title = "Within-cluster raw relationships"
      ) +
      ggplot2::theme_minimal(),

    outcome_by_cluster =
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
          x = cluster_plot,
          y = out
        )
      ) +
      ggplot2::geom_boxplot() +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = spec$cluster,
        y = spec$response,
        title = "Outcome distributions by cluster"
      ) +
      ggplot2::theme_minimal(),

    predictor_by_cluster =
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
          x = cluster_plot,
          y = x
        )
      ) +
      ggplot2::geom_boxplot() +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = spec$cluster,
        y = spec$predictor,
        title = "Predictor distributions by cluster"
      ) +
      ggplot2::theme_minimal(),

    missingness_by_cluster =
      ggplot2::ggplot(
        cluster_missingness,
        ggplot2::aes(
          x = cluster_plot,
          y = missing_percent,
          group = variable,
          shape = variable
        )
      ) +
      ggplot2::geom_point(
        position = ggplot2::position_dodge(
          width = 0.4
        )
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = spec$cluster,
        y = "Missing (%)",
        shape = "Variable",
        title = "Analysis-variable missingness by cluster"
      ) +
      ggplot2::theme_minimal()
  )

  list(
    overall = overall,
    cluster_summary =
      cluster_summary[
        ,
        setdiff(
          names(cluster_summary),
          "cluster_plot"
        ),
        drop = FALSE
      ],
    missingness = missingness,
    cluster_missingness =
      cluster_missingness[
        ,
        setdiff(
          names(cluster_missingness),
          "cluster_plot"
        ),
        drop = FALSE
      ],
    analysis_data = analysis_data,
    plots = plots
  )
}


#' Compare Clustered Linear-Model Diagnostics Across Methods
#'
#' Fits selected clustered-inference methods to the same complete-case data and
#' returns method-level inference, cluster-specific slopes, observation-level
#' diagnostics, optional leave-one-cluster-out influence results, and plots.
#' The function is descriptive and does not recommend or select an estimator.
#'
#' The initial implementation supports one untransformed numeric predictor.
#' Internally, the requested variables are mapped to the `out`, `x`, and
#' `cluster` names used by the verified Study 1/Study 2 fitting helpers so that
#' the empirical diagnostics use the same fitting and extraction rules as the
#' simulation studies.
#'
#' @param formula A two-sided formula of the form `outcome ~ predictor`.
#' @param cluster A one-sided formula such as `~ school`, or a character string
#'   naming the clustering variable.
#' @param data A data frame.
#' @param methods Character vector containing any of `"rs"`, `"ri"`, `"cr2"`,
#'   `"cats"`, `"cats_trunc"`, `"cats_robust"`, `"cats_robustbase"`,
#'   `"robust_ri"`, or `"robust_rs"`.
#' @param alpha Significance level used for confidence intervals.
#' @param seed Non-negative integer used to make method fitting reproducible.
#' @param leave_one_cluster_out Logical; if `TRUE`, refit every requested method
#'   after omitting each cluster and return estimate changes.
#'
#' @return A list containing `comparison`, `cluster_fits`,
#'   `cluster_slope_differences`, `observation_diagnostics`, `influence`,
#'   `analysis_data`, and `plots`. The function does not retain fitted model
#'   objects.
#'
#' @examples
#' dat <- data.frame(
#'   y = rnorm(100),
#'   x = rnorm(100),
#'   id = rep(1:10, each = 10)
#' )
#' out <- cluster_model_diagnostics(
#'   y ~ x,
#'   ~ id,
#'   dat,
#'   methods = c("cr2", "cats"),
#'   seed = 123
#' )
#' out$comparison
#'
#' @export
cluster_model_diagnostics <- function(
    formula,
    cluster,
    data,
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
    alpha = 0.05,
    seed = 20261101L,
    leave_one_cluster_out = FALSE) {
  spec <- cluster_diag_validate_specification(
    formula = formula,
    cluster = cluster,
    data = data
  )

  if (!is.numeric(alpha) ||
      length(alpha) != 1L ||
      is.na(alpha) ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha >= 1) {
    stop(
      "alpha must be one finite number strictly between 0 and 1.",
      call. = FALSE
    )
  }

  if (!is.numeric(seed) ||
      length(seed) != 1L ||
      is.na(seed) ||
      !is.finite(seed) ||
      seed < 0 ||
      seed > .Machine$integer.max ||
      seed != floor(seed)) {
    stop(
      "seed must be one non-negative integer.",
      call. = FALSE
    )
  }

  if (!is.logical(
    leave_one_cluster_out
  ) ||
      length(
        leave_one_cluster_out
      ) != 1L ||
      is.na(
        leave_one_cluster_out
      )) {
    stop(
      "leave_one_cluster_out must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  valid_methods <- study2_method_names()

  if (!is.character(methods) ||
      length(methods) == 0L ||
      anyNA(methods) ||
      anyDuplicated(methods) ||
      any(!methods %in% valid_methods)) {
    stop(
      paste0(
        "methods must contain unique values from: ",
        paste(
          valid_methods,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  prepared <- cluster_diag_prepare_data(
    data = data,
    response = spec$response,
    predictor = spec$predictor,
    cluster = spec$cluster
  )

  dat <- prepared$analysis_data[
    stats::complete.cases(
      prepared$analysis_data[
        ,
        c("out", "x"),
        drop = FALSE
      ]
    ),
    ,
    drop = FALSE
  ]

  dat$cluster <- droplevels(
    factor(dat$cluster)
  )

  if (nrow(dat) == 0L) {
    stop(
      "No complete observations are available for model fitting.",
      call. = FALSE
    )
  }

  if (nlevels(dat$cluster) < 3L) {
    stop(
      "At least three clusters are required for the model diagnostics.",
      call. = FALSE
    )
  }

  fit_rows <- lapply(
    methods,
    function(method) {
      method_index <- match(
        method,
        valid_methods
      )

      study2_fit_method(
        dat = dat[
          ,
          c(
            "out",
            "x",
            "cluster"
          ),
          drop = FALSE
        ],
        method = method,
        beta = 0,
        alpha = alpha,
        replicate_id = 1L,
        method_seed =
          study2_method_seed(
            replicate_seed =
              as.integer(seed),
            method_index =
              method_index
          ),
        realized_mean_slope =
          NA_real_,
        realized_random_slope_sd =
          NA_real_
      )
    }
  )

  fit_rows <- do.call(
    rbind,
    fit_rows
  )
  rownames(fit_rows) <- NULL

  comparison_columns <- c(
    "method",
    "estimate",
    "std_error",
    "df",
    "p_value",
    "conf_low",
    "conf_high",
    "fit_success",
    "converged",
    "singular",
    "retained_clusters",
    "estimated_random_intercept_sd",
    "estimated_random_slope_sd",
    "warning",
    "optimizer_warning",
    "optimizer_code",
    "error",
    "template_warning",
    "template_error",
    "cluster_warning_count",
    "cluster_error_count",
    "dropped_cluster_count",
    "cluster_warning_ids",
    "cluster_error_ids",
    "dropped_cluster_ids",
    "runtime_sec"
  )

  comparison <- fit_rows[
    ,
    comparison_columns,
    drop = FALSE
  ]

  comparison$method_order <- match(
    comparison$method,
    valid_methods
  )
  comparison$method_label <-
    cluster_diag_method_labels(
      comparison$method
    )

  comparison <- comparison[
    order(
      comparison$method_order
    ),
    ,
    drop = FALSE
  ]
  rownames(comparison) <- NULL

  cluster_fits <-
    cluster_diag_cluster_fits(
      dat
    )

  cluster_slope_differences <-
    cluster_diag_slope_differences(
      cluster_fits
    )

  observation_diagnostics <-
    cluster_diag_observation_diagnostics(
      dat
    )

  influence <- if (
    isTRUE(
      leave_one_cluster_out
    )
  ) {
    cluster_diag_leave_one_cluster_out(
      dat = dat,
      methods = methods,
      alpha = alpha,
      seed = as.integer(seed),
      full_comparison = comparison
    )
  } else {
    data.frame(
      method = character(0),
      method_label = character(0),
      omitted_cluster =
        character(0),
      full_estimate =
        numeric(0),
      leave_one_out_estimate =
        numeric(0),
      estimate_change =
        numeric(0),
      fit_success =
        logical(0),
      warning =
        character(0),
      error =
        character(0),
      stringsAsFactors = FALSE
    )
  }

  plot_dat <- dat
  plot_dat$cluster <- factor(
    as.character(plot_dat$cluster),
    levels = levels(dat$cluster)
  )

  comparison_plot_data <-
    comparison[
      comparison$fit_success %in%
        TRUE &
      is.finite(
        comparison$estimate
      ) &
      is.finite(
        comparison$conf_low
      ) &
      is.finite(
        comparison$conf_high
      ),
      ,
      drop = FALSE
    ]

  comparison_plot_data$method_label <-
    factor(
      comparison_plot_data$method_label,
      levels = rev(
        cluster_diag_method_labels(
          valid_methods[
            valid_methods %in%
              comparison_plot_data$method
          ]
        )
      )
    )

  # Keep the returned cluster_fits table in its natural character form.
  # Factor ordering is needed only for plotting and should not alter the
  # user-facing diagnostic table.
  cluster_fits_plot <- cluster_fits

  cluster_fits_plot$engine <-
    factor(
      cluster_fits_plot$engine,
      levels = c(
        "ols",
        "robust",
        "robustbase"
      )
    )

  cluster_fits_plot$cluster <-
    factor(
      cluster_fits_plot$cluster,
      levels = levels(dat$cluster)
    )

  slope_plot_data <- cluster_fits_plot[
    cluster_fits_plot$fit_success %in%
      TRUE &
    is.finite(
      cluster_fits_plot$slope
    ),
    ,
    drop = FALSE
  ]

  slope_difference_plot_data <-
    cluster_slope_differences[
      is.finite(
        cluster_slope_differences$
          slope_difference
      ),
      ,
      drop = FALSE
    ]

  weight_plot_data <- rbind(
    data.frame(
      observation_diagnostics[
        ,
        c(
          "row_id",
          "cluster",
          "x"
        ),
        drop = FALSE
      ],
      engine = "robust",
      weight =
        observation_diagnostics$
          robust_weight,
      stringsAsFactors = FALSE
    ),
    data.frame(
      observation_diagnostics[
        ,
        c(
          "row_id",
          "cluster",
          "x"
        ),
        drop = FALSE
      ],
      engine = "robustbase",
      weight =
        observation_diagnostics$
          robustbase_weight,
      stringsAsFactors = FALSE
    )
  )

  plots <- list(
    method_comparison =
      ggplot2::ggplot(
        comparison_plot_data,
        ggplot2::aes(
          x = estimate,
          y = method_label
        )
      ) +
      ggplot2::geom_vline(
        xintercept = 0,
        linetype = 2
      ) +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          xmin = conf_low,
          xmax = conf_high
        ),
        width = 0,
        orientation = "y"
      ) +
      ggplot2::geom_point() +
      ggplot2::labs(
        x = paste0(
          "Coefficient for ",
          spec$predictor
        ),
        y = NULL,
        title = "Method-level estimates and confidence intervals"
      ) +
      ggplot2::theme_minimal(),

    cluster_slopes =
      ggplot2::ggplot(
        slope_plot_data,
        ggplot2::aes(
          x = cluster,
          y = slope,
          shape = engine,
          group = engine
        )
      ) +
      ggplot2::geom_point(
        position =
          ggplot2::position_dodge(
            width = 0.5
          )
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = spec$cluster,
        y = paste0(
          "Within-cluster slope for ",
          spec$predictor
        ),
        shape = "Fit",
        title = "Cluster-specific slopes"
      ) +
      ggplot2::theme_minimal(),

    slope_differences =
      ggplot2::ggplot(
        slope_difference_plot_data,
        ggplot2::aes(
          x = cluster,
          y = slope_difference,
          shape = comparison
        )
      ) +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype = 2
      ) +
      ggplot2::geom_point(
        position =
          ggplot2::position_dodge(
            width = 0.5
          )
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = spec$cluster,
        y = "Robust slope minus OLS slope",
        shape = "Comparison",
        title = "Within-cluster slope differences"
      ) +
      ggplot2::theme_minimal(),

    residual_leverage =
      ggplot2::ggplot(
        observation_diagnostics,
        ggplot2::aes(
          x = leverage,
          y = standardized_residual
        )
      ) +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype = 2
      ) +
      ggplot2::geom_point() +
      ggplot2::labs(
        x = "OLS leverage",
        y = "OLS standardized residual",
        title = "Residual and leverage diagnostics"
      ) +
      ggplot2::theme_minimal(),

    robust_weights =
      ggplot2::ggplot(
        weight_plot_data,
        ggplot2::aes(
          x = x,
          y = weight
        )
      ) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(
        ~ engine
      ) +
      ggplot2::labs(
        x = spec$predictor,
        y = "Robust weight",
        title = "Observation-level robust weights"
      ) +
      ggplot2::theme_minimal(),

    cluster_fits =
      ggplot2::ggplot(
        plot_dat,
        ggplot2::aes(
          x = x,
          y = out
        )
      ) +
      ggplot2::geom_point() +
      ggplot2::geom_abline(
        data = slope_plot_data,
        ggplot2::aes(
          intercept = intercept,
          slope = slope,
          linetype = engine
        ),
        inherit.aes = FALSE
      ) +
      ggplot2::facet_wrap(
        ~ cluster
      ) +
      ggplot2::labs(
        x = spec$predictor,
        y = spec$response,
        linetype = "Fit",
        title = "Selected within-cluster linear fits"
      ) +
      ggplot2::theme_minimal()
  )

  if (nrow(influence) > 0L) {
    plots$leave_one_cluster_out <-
      ggplot2::ggplot(
        influence[
          is.finite(
            influence$estimate_change
          ),
          ,
          drop = FALSE
        ],
        ggplot2::aes(
          x = omitted_cluster,
          y = estimate_change
        )
      ) +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype = 2
      ) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(
        ~ method_label,
        scales = "free_y"
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = paste(
          "Omitted",
          spec$cluster
        ),
        y = "Leave-one-cluster-out estimate minus full estimate",
        title = "Leave-one-cluster-out influence"
      ) +
      ggplot2::theme_minimal()
  }

  list(
    comparison = comparison,
    cluster_fits = cluster_fits,
    cluster_slope_differences =
      cluster_slope_differences,
    observation_diagnostics =
      observation_diagnostics,
    influence = influence,
    analysis_data = dat,
    plots = plots
  )
}


#' Validate a Cluster Diagnostic Specification
#'
#' @keywords internal
cluster_diag_validate_specification <- function(
    formula,
    cluster,
    data) {
  if (!is.data.frame(data)) {
    stop(
      "data must be a data frame.",
      call. = FALSE
    )
  }

  if (!inherits(formula, "formula") ||
      length(formula) != 3L) {
    stop(
      "formula must be a two-sided formula such as outcome ~ predictor.",
      call. = FALSE
    )
  }

  response_expr <- formula[[2L]]
  predictor_expr <- formula[[3L]]

  if (!is.name(response_expr) ||
      !is.name(predictor_expr)) {
    stop(
      paste(
        "The initial diagnostic implementation requires",
        "one untransformed response and one untransformed predictor."
      ),
      call. = FALSE
    )
  }

  response <- as.character(
    response_expr
  )
  predictor <- as.character(
    predictor_expr
  )

  cluster_name <- if (
    inherits(
      cluster,
      "formula"
    )
  ) {
    if (length(cluster) != 2L ||
        !is.name(cluster[[2L]])) {
      stop(
        "cluster must be a one-sided formula such as ~ school.",
        call. = FALSE
      )
    }

    as.character(
      cluster[[2L]]
    )
  } else if (
    is.character(cluster) &&
    length(cluster) == 1L &&
    !is.na(cluster) &&
    nzchar(cluster)
  ) {
    cluster
  } else {
    stop(
      paste(
        "cluster must be a one-sided formula",
        "or one variable name."
      ),
      call. = FALSE
    )
  }

  required <- c(
    response,
    predictor,
    cluster_name
  )

  missing_variables <- setdiff(
    required,
    names(data)
  )

  if (length(missing_variables) > 0L) {
    stop(
      paste(
        "Variables not found in data:",
        paste(
          missing_variables,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  if (!is.numeric(
    data[[response]]
  )) {
    stop(
      "The response must be numeric.",
      call. = FALSE
    )
  }

  if (!is.numeric(
    data[[predictor]]
  )) {
    stop(
      "The focal predictor must be numeric.",
      call. = FALSE
    )
  }

  list(
    response = response,
    predictor = predictor,
    cluster = cluster_name
  )
}


#' Prepare Canonical Diagnostic Data
#'
#' @keywords internal
cluster_diag_prepare_data <- function(
    data,
    response,
    predictor,
    cluster) {
  cluster_raw <- data[[cluster]]

  all_data <- data.frame(
    row_id = seq_len(nrow(data)),
    out = data[[response]],
    x = data[[predictor]],
    cluster_raw = cluster_raw,
    stringsAsFactors = FALSE
  )

  analysis_data <- all_data[
    !is.na(all_data$cluster_raw),
    ,
    drop = FALSE
  ]

  analysis_data$cluster <- factor(
    analysis_data$cluster_raw
  )

  analysis_data$cluster_raw <- NULL
  rownames(analysis_data) <- NULL

  list(
    all_data = all_data,
    analysis_data = analysis_data
  )
}


#' Cluster-Specific Linear Fits
#'
#' @keywords internal
cluster_diag_cluster_fits <- function(dat) {
  engines <- c(
    "ols",
    "robust",
    "robustbase"
  )

  rows <- list()
  index <- 0L

  for (cluster_id in levels(dat$cluster)) {
    d <- dat[
      dat$cluster == cluster_id,
      ,
      drop = FALSE
    ]

    for (engine in engines) {
      index <- index + 1L

      captured <- study1_capture_fit(
        function() {
          switch(
            engine,
            ols = stats::lm(
              out ~ x,
              data = d
            ),
            robust =
              robust::lmRob(
                out ~ x,
                data = d
              ),
            robustbase =
              robustbase::lmrob(
                out ~ x,
                data = d
              )
          )
        }
      )

      fit <- captured$value

      coefs <- if (
        is.null(fit)
      ) {
        c(
          intercept = NA_real_,
          slope = NA_real_
        )
      } else {
        b <- tryCatch(
          stats::coef(fit),
          error = function(e) {
            numeric(0)
          }
        )

        c(
          intercept = if (
            "(Intercept)" %in% names(b)
          ) {
            as.numeric(
              b["(Intercept)"]
            )
          } else {
            NA_real_
          },
          slope = if (
            "x" %in% names(b)
          ) {
            as.numeric(
              b["x"]
            )
          } else {
            NA_real_
          }
        )
      }

      rows[[index]] <- data.frame(
        cluster = cluster_id,
        engine = engine,
        intercept =
          unname(
            coefs["intercept"]
          ),
        slope =
          unname(
            coefs["slope"]
          ),
        fit_success =
          is.finite(
            coefs["intercept"]
          ) &&
          is.finite(
            coefs["slope"]
          ),
        warning =
          captured$warning,
        error =
          captured$error,
        stringsAsFactors = FALSE
      )
    }
  }

  out <- do.call(
    rbind,
    rows
  )
  rownames(out) <- NULL
  out
}


#' Compute Robust-vs-OLS Cluster Slope Differences
#'
#' @keywords internal
cluster_diag_slope_differences <- function(
    cluster_fits) {
  ols <- cluster_fits[
    cluster_fits$engine == "ols",
    c(
      "cluster",
      "slope"
    ),
    drop = FALSE
  ]
  names(ols)[2L] <- "ols_slope"

  robust <- cluster_fits[
    cluster_fits$engine == "robust",
    c(
      "cluster",
      "slope"
    ),
    drop = FALSE
  ]
  names(robust)[2L] <- "robust_slope"

  robustbase <- cluster_fits[
    cluster_fits$engine == "robustbase",
    c(
      "cluster",
      "slope"
    ),
    drop = FALSE
  ]
  names(robustbase)[2L] <-
    "robustbase_slope"

  joined <- merge(
    ols,
    robust,
    by = "cluster",
    all = TRUE,
    sort = FALSE
  )

  joined <- merge(
    joined,
    robustbase,
    by = "cluster",
    all = TRUE,
    sort = FALSE
  )

  rbind(
    data.frame(
      cluster = joined$cluster,
      comparison =
        "robust - OLS",
      slope_difference =
        joined$robust_slope -
        joined$ols_slope,
      stringsAsFactors = FALSE
    ),
    data.frame(
      cluster = joined$cluster,
      comparison =
        "robustbase - OLS",
      slope_difference =
        joined$robustbase_slope -
        joined$ols_slope,
      stringsAsFactors = FALSE
    )
  )
}


#' Observation-Level OLS and Robust Diagnostics
#'
#' @keywords internal
cluster_diag_observation_diagnostics <- function(
    dat) {
  ols <- stats::lm(
    out ~ x,
    data = dat
  )

  robust_fit <- study1_capture_fit(
    function() {
      robust::lmRob(
        out ~ x,
        data = dat
      )
    }
  )$value

  robustbase_fit <- study1_capture_fit(
    function() {
      robustbase::lmrob(
        out ~ x,
        data = dat
      )
    }
  )$value

  standardized_residual <- tryCatch(
    stats::rstandard(ols),
    error = function(e) {
      rep(
        NA_real_,
        nrow(dat)
      )
    }
  )

  data.frame(
    row_id = dat$row_id,
    cluster =
      as.character(
        dat$cluster
      ),
    out = dat$out,
    x = dat$x,
    ols_residual =
      stats::residuals(ols),
    standardized_residual =
      standardized_residual,
    leverage =
      stats::hatvalues(ols),
    cooks_distance =
      stats::cooks.distance(ols),
    robust_weight =
      cluster_diag_extract_weights(
        robust_fit,
        nrow(dat)
      ),
    robustbase_weight =
      cluster_diag_extract_weights(
        robustbase_fit,
        nrow(dat)
      ),
    stringsAsFactors = FALSE
  )
}


#' Extract Robust Regression Weights
#'
#' @keywords internal
cluster_diag_extract_weights <- function(
    fit,
    n) {
  if (is.null(fit)) {
    return(
      rep(
        NA_real_,
        n
      )
    )
  }

  candidates <- list(
    # robust::lmRob() stores final MM robustness weights here.
    fit[["M.weights"]],
    tryCatch(
      stats::weights(fit),
      error = function(e) NULL
    ),
    fit[["rweights"]],
    fit[["robust.weights"]],
    fit[["weights"]],
    fit[["w"]]
  )

  for (value in candidates) {
    if (is.numeric(value) &&
        length(value) == n) {
      return(
        as.numeric(value)
      )
    }
  }

  rep(
    NA_real_,
    n
  )
}


#' Leave-One-Cluster-Out Estimate Changes
#'
#' @keywords internal
cluster_diag_leave_one_cluster_out <- function(
    dat,
    methods,
    alpha,
    seed,
    full_comparison) {
  valid_methods <- study2_method_names()
  clusters <- levels(dat$cluster)

  rows <- list()
  index <- 0L

  for (
    cluster_index in
    seq_along(clusters)
  ) {
    omitted <- clusters[
      cluster_index
    ]

    d <- dat[
      dat$cluster != omitted,
      ,
      drop = FALSE
    ]
    d$cluster <- droplevels(
      factor(d$cluster)
    )

    for (method in methods) {
      index <- index + 1L

      method_index <- match(
        method,
        valid_methods
      )

      cluster_seed <-
        cluster_diag_loo_seed(
          seed = seed,
          cluster_index =
            cluster_index
        )

      result <- study2_fit_method(
        dat = d[
          ,
          c(
            "out",
            "x",
            "cluster"
          ),
          drop = FALSE
        ],
        method = method,
        beta = 0,
        alpha = alpha,
        replicate_id = 1L,
        method_seed =
          study2_method_seed(
            replicate_seed =
              cluster_seed,
            method_index =
              method_index
          ),
        realized_mean_slope =
          NA_real_,
        realized_random_slope_sd =
          NA_real_
      )

      full_estimate <-
        full_comparison$estimate[
          full_comparison$method ==
            method
        ][1L]

      rows[[index]] <- data.frame(
        method = method,
        method_label =
          cluster_diag_method_labels(
            method
          ),
        omitted_cluster = omitted,
        full_estimate =
          full_estimate,
        leave_one_out_estimate =
          result$estimate[1L],
        estimate_change =
          result$estimate[1L] -
          full_estimate,
        fit_success =
          result$fit_success[1L],
        warning =
          result$warning[1L],
        error =
          result$error[1L],
        stringsAsFactors = FALSE
      )
    }
  }

  out <- do.call(
    rbind,
    rows
  )
  rownames(out) <- NULL
  out
}


#' Derive a Deterministic Leave-One-Cluster-Out Seed
#'
#' @keywords internal
cluster_diag_loo_seed <- function(
    seed,
    cluster_index) {
  max_seed <-
    .Machine$integer.max - 1

  value <- (
    as.double(seed) +
      as.double(cluster_index) *
        7919
  ) %% max_seed

  as.integer(
    value + 1
  )
}


#' Diagnostic Method Labels
#'
#' @keywords internal
cluster_diag_method_labels <- function(
    methods) {
  labels <- c(
    rs = "Random slope / KR",
    ri = "Random intercept / KR",
    cr2 = "OLS / CR2",
    cats = "CATs",
    cats_trunc = "Truncated CATs",
    cats_robust = "Robust CATs: lmRob",
    cats_robustbase =
      "Robust CATs: lmrob",
    robust_ri =
      "Robust random intercept",
    robust_rs =
      "Robust random slope"
  )

  unname(
    labels[
      methods
    ]
  )
}


#' Safe Summary Helpers
#'
#' @keywords internal
cluster_diag_mean <- function(x) {
  if (length(x) == 0L ||
      all(is.na(x))) {
    return(NA_real_)
  }

  mean(
    x,
    na.rm = TRUE
  )
}


#' @keywords internal
cluster_diag_sd <- function(x) {
  x <- x[
    is.finite(x)
  ]

  if (length(x) < 2L) {
    return(NA_real_)
  }

  stats::sd(x)
}


#' @keywords internal
cluster_diag_min <- function(x) {
  x <- x[
    is.finite(x)
  ]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  min(x)
}


#' @keywords internal
cluster_diag_max <- function(x) {
  x <- x[
    is.finite(x)
  ]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  max(x)
}


#' @keywords internal
cluster_diag_range <- function(x) {
  x <- x[
    is.finite(x)
  ]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  diff(range(x))
}


#' @keywords internal
cluster_diag_median <- function(x) {
  x <- x[
    is.finite(x)
  ]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  stats::median(x)
}


#' @keywords internal
cluster_diag_iqr <- function(x) {
  x <- x[
    is.finite(x)
  ]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  stats::IQR(x)
}