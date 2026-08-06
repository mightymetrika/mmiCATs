# Robust CATs audit: Phase 2B row-order stress diagnostic
#
# Purpose:
#   Determine whether the very small row-order differences observed in Phase 2A
#   are isolated numerical variation or a potentially consequential instability.
#
# Requirements:
#   library(devtools)
#   load_all()
#   source("data-raw/robust_cats_audit_helpers.R")
#
# This script does not modify production functions.

project_root <- rca_find_project_root()
rca_require_packages()

pkgload::load_all(
  project_root,
  quiet = TRUE
)

output_dir <- file.path(
  project_root,
  "data-raw",
  "robust-cats-audit-results",
  "phase2b-row-order-stress"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# A small but broader stress diagnostic:
#   3 contamination conditions
#   5 fixed datasets per condition
#   2 robust engines
#   4 ordering variants
#
# The provisional numerical-equivalence threshold is deliberately much wider
# than machine precision but still far below a substantively meaningful change.
n_datasets_per_condition <- 5L
absolute_tolerance <- 1e-5
relative_tolerance <- 1e-5
alpha <- 0.05

condition_table <- data.frame(
  contamination = c(
    "none",
    "vertical",
    "bad_leverage"
  ),
  contamination_size = c(
    0.375,
    6,
    0.375
  ),
  leverage_size = c(
    4,
    4,
    4
  ),
  stringsAsFactors = FALSE
)

engines <- c(
  "robust",
  "robustbase"
)

variant_names <- c(
  "exact_repeat",
  "within_cluster_row_order",
  "cluster_block_order",
  "global_row_order"
)

make_within_cluster_permutation <- function(
    dat,
    seed) {
  set.seed(seed)

  cluster_levels <- unique(
    as.character(dat$cluster)
  )

  pieces <- lapply(
    cluster_levels,
    function(cluster_id) {
      cluster_dat <- dat[
        as.character(dat$cluster) ==
          cluster_id,
        ,
        drop = FALSE
      ]

      cluster_dat[
        sample(seq_len(nrow(cluster_dat))),
        ,
        drop = FALSE
      ]
    }
  )

  result <- do.call(rbind, pieces)
  result$cluster <- factor(
    as.character(result$cluster),
    levels = levels(dat$cluster)
  )
  rownames(result) <- NULL
  result
}

make_cluster_block_permutation <- function(
    dat,
    seed) {
  set.seed(seed)

  cluster_levels <- unique(
    as.character(dat$cluster)
  )
  block_order <- sample(cluster_levels)

  pieces <- lapply(
    block_order,
    function(cluster_id) {
      dat[
        as.character(dat$cluster) ==
          cluster_id,
        ,
        drop = FALSE
      ]
    }
  )

  result <- do.call(rbind, pieces)
  result$cluster <- factor(
    as.character(result$cluster),
    levels = levels(dat$cluster)
  )
  rownames(result) <- NULL
  result
}

make_global_permutation <- function(
    dat,
    seed) {
  set.seed(seed)

  result <- dat[
    sample(seq_len(nrow(dat))),
    ,
    drop = FALSE
  ]
  result$cluster <- factor(
    as.character(result$cluster),
    levels = levels(dat$cluster)
  )
  rownames(result) <- NULL
  result
}

fit_oracle_captured <- function(
    dat,
    engine,
    seed) {
  set.seed(seed)

  rca_capture(
    rca_oracle(
      dat = dat,
      engine = engine,
      alpha = alpha,
      truncation_rule = "none",
      consume_template = TRUE
    )
  )
}

extract_aggregate_values <- function(
    captured) {
  quantities <- c(
    "estimate",
    "std_error",
    "df",
    "p_value",
    "conf_low",
    "conf_high",
    "retained_clusters"
  )

  if (!is.null(captured$value)) {
    aggregate <- captured$value$aggregate

    values <- vapply(
      quantities,
      function(quantity) {
        as.numeric(
          aggregate[[quantity]]
        )
      },
      numeric(1)
    )
  } else {
    values <- setNames(
      rep(NA_real_, length(quantities)),
      quantities
    )
  }

  values
}

compare_fits <- function(
    baseline,
    observed,
    contamination,
    dataset_id,
    engine,
    variant) {
  baseline_values <-
    extract_aggregate_values(baseline)
  observed_values <-
    extract_aggregate_values(observed)

  successful <- !is.null(baseline$value) &&
    !is.null(observed$value)

  quantity_names <- c(
    "estimate",
    "std_error",
    "p_value",
    "conf_low",
    "conf_high"
  )

  quantity_rows <- lapply(
    quantity_names,
    function(quantity) {
      reference_value <-
        baseline_values[quantity]
      observed_value <-
        observed_values[quantity]
      absolute_difference <- abs(
        reference_value - observed_value
      )
      allowed_difference <-
        absolute_tolerance +
        relative_tolerance *
        abs(reference_value)

      data.frame(
        contamination = contamination,
        dataset_id = dataset_id,
        engine = engine,
        variant = variant,
        quantity = quantity,
        reference_value =
          unname(reference_value),
        observed_value =
          unname(observed_value),
        absolute_difference =
          unname(absolute_difference),
        allowed_difference =
          unname(allowed_difference),
        numerically_equivalent =
          isTRUE(
            successful &&
              is.finite(
                absolute_difference
              ) &&
              absolute_difference <=
                allowed_difference
          ),
        stringsAsFactors = FALSE
      )
    }
  )

  quantity_comparison <- do.call(
    rbind,
    quantity_rows
  )

  baseline_reject <- if (
    successful
  ) {
    baseline_values["p_value"] < alpha
  } else {
    NA
  }
  observed_reject <- if (
    successful
  ) {
    observed_values["p_value"] < alpha
  } else {
    NA
  }

  baseline_ci_excludes_zero <- if (
    successful
  ) {
    baseline_values["conf_low"] > 0 ||
      baseline_values["conf_high"] < 0
  } else {
    NA
  }
  observed_ci_excludes_zero <- if (
    successful
  ) {
    observed_values["conf_low"] > 0 ||
      observed_values["conf_high"] < 0
  } else {
    NA
  }

  diagnostic_summary <- data.frame(
    contamination = contamination,
    dataset_id = dataset_id,
    engine = engine,
    variant = variant,
    baseline_success =
      !is.null(baseline$value),
    observed_success =
      !is.null(observed$value),
    baseline_error = baseline$error,
    observed_error = observed$error,
    baseline_warning = baseline$warning,
    observed_warning = observed$warning,
    baseline_message = baseline$message,
    observed_message = observed$message,
    df_difference = if (successful) {
      abs(
        baseline_values["df"] -
          observed_values["df"]
      )
    } else {
      NA_real_
    },
    retained_cluster_difference =
      if (successful) {
        abs(
          baseline_values[
            "retained_clusters"
          ] -
            observed_values[
              "retained_clusters"
            ]
        )
      } else {
        NA_real_
      },
    rejection_changed = if (
      successful
    ) {
      baseline_reject != observed_reject
    } else {
      NA
    },
    ci_decision_changed = if (
      successful
    ) {
      baseline_ci_excludes_zero !=
        observed_ci_excludes_zero
    } else {
      NA
    },
    all_quantities_equivalent =
      successful &&
      all(
        quantity_comparison$
          numerically_equivalent
      ),
    max_absolute_difference =
      if (successful) {
        max(
          quantity_comparison$
            absolute_difference
        )
      } else {
        NA_real_
      },
    stringsAsFactors = FALSE
  )

  cluster_comparison <- data.frame()

  if (successful) {
    baseline_clusters <-
      baseline$value$diagnostics[
        ,
        c(
          "cluster",
          "intercept",
          "x",
          "retained_before_truncation"
        ),
        drop = FALSE
      ]
    observed_clusters <-
      observed$value$diagnostics[
        ,
        c(
          "cluster",
          "intercept",
          "x",
          "retained_before_truncation"
        ),
        drop = FALSE
      ]

    cluster_comparison <- merge(
      baseline_clusters,
      observed_clusters,
      by = "cluster",
      suffixes = c(
        "_baseline",
        "_observed"
      ),
      all = TRUE,
      sort = TRUE
    )

    cluster_comparison$contamination <-
      contamination
    cluster_comparison$dataset_id <-
      dataset_id
    cluster_comparison$engine <- engine
    cluster_comparison$variant <- variant
    cluster_comparison$
      intercept_absolute_difference <-
      abs(
        cluster_comparison$
          intercept_baseline -
          cluster_comparison$
            intercept_observed
      )
    cluster_comparison$
      x_absolute_difference <-
      abs(
        cluster_comparison$x_baseline -
          cluster_comparison$x_observed
      )
    cluster_comparison$
      retention_changed <-
      cluster_comparison$
        retained_before_truncation_baseline !=
      cluster_comparison$
        retained_before_truncation_observed
  }

  list(
    quantity_comparison =
      quantity_comparison,
    diagnostic_summary =
      diagnostic_summary,
    cluster_comparison =
      cluster_comparison
  )
}

quantity_rows <- list()
summary_rows <- list()
cluster_rows <- list()
row_index <- 0L

started_at <- Sys.time()

for (
  condition_index in
  seq_len(nrow(condition_table))
) {
  condition <- condition_table[
    condition_index,
    ,
    drop = FALSE
  ]

  for (
    dataset_id in
    seq_len(n_datasets_per_condition)
  ) {
    data_seed <-
      20261100L +
      condition_index * 100L +
      dataset_id

    set.seed(data_seed)

    dat <- study1_simulate_data(
      n_clusters = 10,
      cluster_size = 40,
      beta = 0,
      intercept = 0,
      random_intercept_sd = 1,
      residual_sd = 1,
      x_sd = 1,
      contamination =
        condition$contamination,
      contamination_prop = 0.05,
      contamination_size =
        condition$contamination_size,
      leverage_size =
        condition$leverage_size
    )

    permutation_seed <-
      20262100L +
      condition_index * 100L +
      dataset_id

    variants <- list(
      exact_repeat = dat,
      within_cluster_row_order =
        make_within_cluster_permutation(
          dat,
          permutation_seed
        ),
      cluster_block_order =
        make_cluster_block_permutation(
          dat,
          permutation_seed + 10000L
        ),
      global_row_order =
        make_global_permutation(
          dat,
          permutation_seed + 20000L
        )
    )

    for (engine_index in seq_along(engines)) {
      engine <- engines[engine_index]

      fit_seed <-
        20263100L +
        condition_index * 1000L +
        dataset_id * 10L +
        engine_index

      baseline <- fit_oracle_captured(
        dat = dat,
        engine = engine,
        seed = fit_seed
      )

      for (variant in variant_names) {
        observed <- fit_oracle_captured(
          dat = variants[[variant]],
          engine = engine,
          seed = fit_seed
        )

        comparison <- compare_fits(
          baseline = baseline,
          observed = observed,
          contamination =
            condition$contamination,
          dataset_id = dataset_id,
          engine = engine,
          variant = variant
        )

        row_index <- row_index + 1L
        quantity_rows[[row_index]] <-
          comparison$quantity_comparison
        summary_rows[[row_index]] <-
          comparison$diagnostic_summary
        cluster_rows[[row_index]] <-
          comparison$cluster_comparison
      }
    }

    message(
      sprintf(
        paste0(
          "Completed %s dataset %s of %s. ",
          "Elapsed: %.2f minutes."
        ),
        condition$contamination,
        dataset_id,
        n_datasets_per_condition,
        as.numeric(
          difftime(
            Sys.time(),
            started_at,
            units = "mins"
          )
        )
      )
    )
  }
}

quantity_results <- do.call(
  rbind,
  quantity_rows
)
diagnostic_results <- do.call(
  rbind,
  summary_rows
)

nonempty_cluster_rows <- cluster_rows[
  vapply(
    cluster_rows,
    nrow,
    integer(1)
  ) > 0L
]

cluster_results <- if (
  length(nonempty_cluster_rows) == 0L
) {
  data.frame()
} else {
  do.call(
    rbind,
    nonempty_cluster_rows
  )
}

summary_groups <- split(
  diagnostic_results,
  interaction(
    diagnostic_results$contamination,
    diagnostic_results$engine,
    diagnostic_results$variant,
    drop = TRUE
  )
)

stress_summary <- do.call(
  rbind,
  lapply(
    summary_groups,
    function(x) {
      quantity_subset <- quantity_results[
        quantity_results$contamination ==
          x$contamination[1L] &
          quantity_results$engine ==
            x$engine[1L] &
          quantity_results$variant ==
            x$variant[1L],
        ,
        drop = FALSE
      ]

      cluster_subset <- if (
        nrow(cluster_results) == 0L
      ) {
        data.frame()
      } else {
        cluster_results[
          cluster_results$contamination ==
            x$contamination[1L] &
            cluster_results$engine ==
              x$engine[1L] &
            cluster_results$variant ==
              x$variant[1L],
          ,
          drop = FALSE
        ]
      }

      data.frame(
        contamination =
          x$contamination[1L],
        engine = x$engine[1L],
        variant = x$variant[1L],
        datasets = nrow(x),
        successful_pairs = sum(
          x$baseline_success &
            x$observed_success
        ),
        all_quantities_equivalent_rate =
          100 * mean(
            x$all_quantities_equivalent
          ),
        maximum_output_difference =
          max(
            x$max_absolute_difference,
            na.rm = TRUE
          ),
        maximum_cluster_intercept_difference =
          if (nrow(cluster_subset) == 0L) {
            NA_real_
          } else {
            max(
              cluster_subset$
                intercept_absolute_difference,
              na.rm = TRUE
            )
          },
        maximum_cluster_slope_difference =
          if (nrow(cluster_subset) == 0L) {
            NA_real_
          } else {
            max(
              cluster_subset$
                x_absolute_difference,
              na.rm = TRUE
            )
          },
        retained_cluster_changes =
          sum(
            x$retained_cluster_difference != 0,
            na.rm = TRUE
          ),
        rejection_changes = sum(
          x$rejection_changed,
          na.rm = TRUE
        ),
        ci_decision_changes = sum(
          x$ci_decision_changed,
          na.rm = TRUE
        ),
        fit_failures = sum(
          !x$baseline_success |
            !x$observed_success
        ),
        stringsAsFactors = FALSE
      )
    }
  )
)

rownames(stress_summary) <- NULL

exact_repeat <- diagnostic_results[
  diagnostic_results$variant ==
    "exact_repeat",
  ,
  drop = FALSE
]

audit_checks <- data.frame(
  check = c(
    "all_fits_returned_results",
    "exact_repeat_is_exact",
    "no_retained_cluster_changes",
    "no_rejection_changes",
    "no_ci_decision_changes",
    "all_pairs_within_provisional_tolerance"
  ),
  passed = c(
    all(
      diagnostic_results$baseline_success &
        diagnostic_results$observed_success
    ),
    all(
      exact_repeat$max_absolute_difference ==
        0
    ),
    !any(
      diagnostic_results$
        retained_cluster_difference != 0,
      na.rm = TRUE
    ),
    !any(
      diagnostic_results$rejection_changed,
      na.rm = TRUE
    ),
    !any(
      diagnostic_results$ci_decision_changed,
      na.rm = TRUE
    ),
    all(
      diagnostic_results$
        all_quantities_equivalent
    )
  ),
  details = c(
    sprintf(
      "%s of %s fit pairs successful",
      sum(
        diagnostic_results$baseline_success &
          diagnostic_results$observed_success
      ),
      nrow(diagnostic_results)
    ),
    sprintf(
      "Maximum exact-repeat difference: %.12g",
      max(
        exact_repeat$max_absolute_difference,
        na.rm = TRUE
      )
    ),
    sprintf(
      "%s retained-cluster changes",
      sum(
        diagnostic_results$
          retained_cluster_difference != 0,
        na.rm = TRUE
      )
    ),
    sprintf(
      "%s rejection changes",
      sum(
        diagnostic_results$rejection_changed,
        na.rm = TRUE
      )
    ),
    sprintf(
      "%s confidence-interval decision changes",
      sum(
        diagnostic_results$ci_decision_changed,
        na.rm = TRUE
      )
    ),
    sprintf(
      "%s of %s pairs within abs = %g and rel = %g",
      sum(
        diagnostic_results$
          all_quantities_equivalent
      ),
      nrow(diagnostic_results),
      absolute_tolerance,
      relative_tolerance
    )
  ),
  stringsAsFactors = FALSE
)

source_files <- c(
  robust_cats_audit_helpers = file.path(
    project_root,
    "data-raw",
    "robust_cats_audit_helpers.R"
  ),
  robust_cats_row_order_stress =
    file.path(
      project_root,
      "data-raw",
      "robust_cats_row_order_stress.R"
    ),
  pwr_func_study1_helpers = file.path(
    project_root,
    "R",
    "pwr_func_study1_helpers.R"
  )
)

source_checksums <- rca_source_checksums(
  source_files
)

results <- list(
  settings = list(
    n_datasets_per_condition =
      n_datasets_per_condition,
    absolute_tolerance =
      absolute_tolerance,
    relative_tolerance =
      relative_tolerance,
    alpha = alpha,
    condition_table =
      condition_table,
    engines = engines,
    variants = variant_names
  ),
  checks = audit_checks,
  summary = stress_summary,
  diagnostic_results =
    diagnostic_results,
  quantity_results =
    quantity_results,
  cluster_results =
    cluster_results,
  source_checksums =
    source_checksums,
  session_info =
    utils::sessionInfo()
)

rca_write_csv_atomic(
  audit_checks,
  file.path(
    output_dir,
    "robust_cats_row_order_checks.csv"
  )
)

rca_write_csv_atomic(
  stress_summary,
  file.path(
    output_dir,
    "robust_cats_row_order_summary.csv"
  )
)

rca_write_csv_atomic(
  diagnostic_results,
  file.path(
    output_dir,
    "robust_cats_row_order_fit_pairs.csv"
  )
)

rca_write_csv_atomic(
  quantity_results,
  file.path(
    output_dir,
    "robust_cats_row_order_quantities.csv"
  )
)

rca_write_csv_atomic(
  cluster_results,
  file.path(
    output_dir,
    "robust_cats_row_order_cluster_coefficients.csv"
  )
)

rca_write_csv_atomic(
  source_checksums,
  file.path(
    output_dir,
    "robust_cats_row_order_source_checksums.csv"
  )
)

rca_save_rds_atomic(
  results,
  file.path(
    output_dir,
    "robust_cats_row_order_stress_results.rds"
  )
)

writeLines(
  capture.output(
    utils::sessionInfo()
  ),
  con = file.path(
    output_dir,
    "session_info.txt"
  ),
  useBytes = TRUE
)

message("")
message("Robust CATs Phase 2B row-order stress checks:")
print(audit_checks, row.names = FALSE)

message("")
message("Stress summary:")
print(stress_summary, row.names = FALSE)

message("")
message(paste(
  "Results saved to:",
  output_dir
))

message("")
message(
  paste(
    "These checks characterize numerical row-order",
    "sensitivity. They do not compare statistical",
    "performance across estimators."
  )
)
