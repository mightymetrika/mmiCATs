# Robust mixed-model Phase 4D end-to-end integration pilot
#
# Purpose:
#   Exercise the fully integrated Study 1 and Study 2 method schedules across
#   the frozen DGP structures before modifying the definitive simulation
#   runners. This is a software/plumbing/runtime pilot, not a statistical
#   performance study.
#
# Phase 4D changes no production code and does not modify the definitive
# Study 1 or Study 2 runner scripts.
#
# Pilot coverage:
#   Study 1:
#     - G = 10: beta 0/0.10 x clean/vertical/bad leverage, all seven methods
#     - G = 20/40: one clean beta = 0.10 smoke condition, robust_ri only
#   Study 2:
#     - G = 10: beta 0/0.10 x slope SD 0.05/0.10 x clean/vertical,
#       all nine methods
#     - G = 20/40: one clean beta = 0.10, slope SD = 0.10 smoke condition,
#       robust_ri and robust_rs only
#
# Additional integration checks:
#   - exact Phase 4C production-source baseline;
#   - append-only canonical method schedules;
#   - all-method order invariance on one representative condition per study;
#   - robust-method subset invariance on the same conditions;
#   - complete expected method-row counts;
#   - finite inference on every successful fit;
#   - robust mixed models observed successfully in every frozen structural
#     family and every cluster-count tier;
#   - boundary fits remain diagnostic rather than automatically failed.
#
# A one-replication pilot is not sufficient to assess Type I error, power,
# bias, coverage, or estimator ranking.

library(devtools)

load_all()

phase4d_find_project_root <- function(path = getwd()) {
  path <- normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )

  repeat {
    if (file.exists(
      file.path(path, "DESCRIPTION")
    )) {
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

phase4d_bind_rows_fill <- function(data_list) {
  data_list <- data_list[
    vapply(
      data_list,
      function(x) {
        is.data.frame(x) &&
          nrow(x) > 0L
      },
      logical(1)
    )
  ]

  if (length(data_list) == 0L) {
    return(data.frame())
  }

  all_names <- unique(
    unlist(
      lapply(
        data_list,
        names
      ),
      use.names = FALSE
    )
  )

  aligned <- lapply(
    data_list,
    function(data) {
      missing_names <- setdiff(
        all_names,
        names(data)
      )

      for (name in missing_names) {
        data[[name]] <- NA
      }

      data[
        ,
        all_names,
        drop = FALSE
      ]
    }
  )

  out <- do.call(
    rbind,
    aligned
  )
  rownames(out) <- NULL
  out
}

phase4d_drop_runtime <- function(data) {
  data[
    ,
    setdiff(
      names(data),
      "runtime_sec"
    ),
    drop = FALSE
  ]
}

phase4d_sort_method_rows <- function(data) {
  data <- data[
    order(
      data$method
    ),
    ,
    drop = FALSE
  ]
  rownames(data) <- NULL
  data
}

phase4d_equal <- function(x,
                          y,
                          tolerance = 1e-10) {
  isTRUE(
    all.equal(
      x,
      y,
      tolerance = tolerance,
      check.attributes = TRUE
    )
  )
}

phase4d_add_condition_columns <- function(data,
                                          condition) {
  data.frame(
    condition_id =
      condition$condition_id,
    n_clusters =
      condition$n_clusters,
    beta =
      condition$beta,
    contamination =
      condition$contamination,
    random_slope_sd =
      condition$random_slope_sd,
    method_scope =
      condition$method_scope,
    data,
    stringsAsFactors = FALSE
  )
}

project_root <- phase4d_find_project_root()

output_dir <- file.path(
  project_root,
  "data-raw",
  "robust-mixed-model-results",
  "phase4d-end-to-end-pilot"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

checks <- list()

add_check <- function(category,
                      check,
                      passed,
                      required = TRUE,
                      details = NA_character_) {
  checks[[length(checks) + 1L]] <<-
    data.frame(
      category = category,
      check = check,
      passed = as.logical(passed),
      required = as.logical(required),
      details = details,
      stringsAsFactors = FALSE
    )
}

message("1. Verifying the Phase 4C production baseline...")

expected_md5 <- c(
  robust_mixed_models =
    "a3f55f48736df665fa8ce45706dd9c49",
  pwr_func_study1 =
    "483c16ba66b163f122c1783f6120dd9e",
  pwr_func_study1_helpers =
    "4061bd490e77184ba9f79fcd5ab95384",
  pwr_func_study2 =
    "2ca209d83733efc9169363b48686e405",
  pwr_func_study2_helpers =
    "dd5f484c1855bf55dc8af72f770fb8d3"
)

production_paths <- c(
  robust_mixed_models = file.path(
    project_root,
    "R",
    "robust_mixed_models.R"
  ),
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
  )
)

observed_md5 <- unname(
  tools::md5sum(
    production_paths
  )
)
names(observed_md5) <- names(
  production_paths
)

source_baseline <- data.frame(
  source = names(production_paths),
  path = normalizePath(
    production_paths,
    winslash = "/",
    mustWork = TRUE
  ),
  expected_md5 =
    unname(
      expected_md5[
        names(production_paths)
      ]
    ),
  observed_md5 =
    unname(observed_md5),
  matched =
    unname(observed_md5) ==
    unname(
      expected_md5[
        names(production_paths)
      ]
    ),
  stringsAsFactors = FALSE
)

add_check(
  "Source baseline",
  "phase4c_production_sources_match_expected_md5",
  all(source_baseline$matched),
  details = paste(
    source_baseline$source,
    source_baseline$matched,
    sep = "=",
    collapse = "; "
  )
)

study1_methods <- study1_method_names()
study2_methods <- study2_method_names()

expected_study1_methods <- c(
  "ri",
  "cr2",
  "cats",
  "cats_trunc",
  "cats_robust",
  "cats_robustbase",
  "robust_ri"
)

expected_study2_methods <- c(
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

add_check(
  "Method schedule",
  "study1_canonical_schedule_is_frozen",
  identical(
    study1_methods,
    expected_study1_methods
  ),
  details = paste(
    study1_methods,
    collapse = ","
  )
)

add_check(
  "Method schedule",
  "study2_canonical_schedule_is_frozen",
  identical(
    study2_methods,
    expected_study2_methods
  ),
  details = paste(
    study2_methods,
    collapse = ","
  )
)

message("2. Building the Phase 4D pilot design...")

study1_g10 <- expand.grid(
  beta = c(0, 0.10),
  contamination = c(
    "none",
    "vertical",
    "bad_leverage"
  ),
  stringsAsFactors = FALSE
)

study1_design <- data.frame(
  condition_id = sprintf(
    "P4D_S1_%02d",
    seq_len(
      nrow(study1_g10) + 2L
    )
  ),
  n_clusters = c(
    rep(
      10L,
      nrow(study1_g10)
    ),
    20L,
    40L
  ),
  beta = c(
    study1_g10$beta,
    0.10,
    0.10
  ),
  contamination = c(
    study1_g10$contamination,
    "none",
    "none"
  ),
  random_slope_sd = NA_real_,
  method_scope = c(
    rep(
      "all",
      nrow(study1_g10)
    ),
    "robust_only",
    "robust_only"
  ),
  seed = c(
    rep(
      20269601L,
      nrow(study1_g10)
    ),
    20269602L,
    20269603L
  ),
  stringsAsFactors = FALSE
)

study2_g10 <- expand.grid(
  beta = c(0, 0.10),
  random_slope_sd = c(
    0.05,
    0.10
  ),
  contamination = c(
    "none",
    "vertical"
  ),
  stringsAsFactors = FALSE
)

study2_design <- data.frame(
  condition_id = sprintf(
    "P4D_S2_%02d",
    seq_len(
      nrow(study2_g10) + 2L
    )
  ),
  n_clusters = c(
    rep(
      10L,
      nrow(study2_g10)
    ),
    20L,
    40L
  ),
  beta = c(
    study2_g10$beta,
    0.10,
    0.10
  ),
  contamination = c(
    study2_g10$contamination,
    "none",
    "none"
  ),
  random_slope_sd = c(
    study2_g10$random_slope_sd,
    0.10,
    0.10
  ),
  method_scope = c(
    rep(
      "all",
      nrow(study2_g10)
    ),
    "robust_only",
    "robust_only"
  ),
  seed = c(
    rep(
      20269611L,
      nrow(study2_g10)
    ),
    20269612L,
    20269613L
  ),
  stringsAsFactors = FALSE
)

message("3. Running Study 1 pilot conditions...")

study1_results <- vector(
  "list",
  nrow(study1_design)
)
study1_summaries <- vector(
  "list",
  nrow(study1_design)
)

for (index in seq_len(
  nrow(study1_design)
)) {
  condition <- study1_design[
    index,
    ,
    drop = FALSE
  ]

  methods <- if (
    identical(
      condition$method_scope,
      "all"
    )
  ) {
    study1_methods
  } else {
    "robust_ri"
  }

  contamination_size <- switch(
    condition$contamination,
    none = 1,
    vertical = 6,
    bad_leverage = 0.375
  )

  leverage_size <- if (
    identical(
      condition$contamination,
      "bad_leverage"
    )
  ) {
    4
  } else {
    4
  }

  result <- suppressWarnings(
    pwr_func_study1(
      n_clusters =
        condition$n_clusters,
      cluster_size = 40L,
      beta = condition$beta,
      intercept = 0,
      random_intercept_sd = 1,
      residual_sd = 1,
      x_sd = 1,
      contamination =
        condition$contamination,
      contamination_prop = 0.05,
      contamination_size =
        contamination_size,
      leverage_size = leverage_size,
      reps = 1L,
      alpha = 0.05,
      methods = methods,
      seed = condition$seed,
      keep_replicates = TRUE
    )
  )

  study1_results[[index]] <-
    phase4d_add_condition_columns(
      result$replicates,
      condition
    )

  study1_summaries[[index]] <-
    phase4d_add_condition_columns(
      result$summary,
      condition
    )

  message(
    sprintf(
      "  Study 1 %s complete (%s method rows).",
      condition$condition_id,
      nrow(result$replicates)
    )
  )
}

message("4. Running Study 2 pilot conditions...")

study2_results <- vector(
  "list",
  nrow(study2_design)
)
study2_summaries <- vector(
  "list",
  nrow(study2_design)
)

for (index in seq_len(
  nrow(study2_design)
)) {
  condition <- study2_design[
    index,
    ,
    drop = FALSE
  ]

  methods <- if (
    identical(
      condition$method_scope,
      "all"
    )
  ) {
    study2_methods
  } else {
    c(
      "robust_ri",
      "robust_rs"
    )
  }

  contamination_size <- if (
    identical(
      condition$contamination,
      "none"
    )
  ) {
    1
  } else {
    6
  }

  result <- suppressWarnings(
    pwr_func_study2(
      n_clusters =
        condition$n_clusters,
      cluster_size = 40L,
      beta = condition$beta,
      intercept = 0,
      random_intercept_sd = 1,
      random_slope_sd =
        condition$random_slope_sd,
      residual_sd = 1,
      x_sd = 1,
      contamination =
        condition$contamination,
      contamination_prop = 0.05,
      contamination_size =
        contamination_size,
      reps = 1L,
      alpha = 0.05,
      methods = methods,
      seed = condition$seed,
      keep_replicates = TRUE
    )
  )

  study2_results[[index]] <-
    phase4d_add_condition_columns(
      result$replicates,
      condition
    )

  study2_summaries[[index]] <-
    phase4d_add_condition_columns(
      result$summary,
      condition
    )

  message(
    sprintf(
      "  Study 2 %s complete (%s method rows).",
      condition$condition_id,
      nrow(result$replicates)
    )
  )
}

study1_replicates <-
  phase4d_bind_rows_fill(
    study1_results
  )
study2_replicates <-
  phase4d_bind_rows_fill(
    study2_results
  )

study1_summary <-
  phase4d_bind_rows_fill(
    study1_summaries
  )
study2_summary <-
  phase4d_bind_rows_fill(
    study2_summaries
  )

all_replicates <-
  phase4d_bind_rows_fill(
    list(
      data.frame(
        study = "Study 1",
        study1_replicates,
        stringsAsFactors = FALSE
      ),
      data.frame(
        study = "Study 2",
        study2_replicates,
        stringsAsFactors = FALSE
      )
    )
  )

all_summaries <-
  phase4d_bind_rows_fill(
    list(
      data.frame(
        study = "Study 1",
        study1_summary,
        stringsAsFactors = FALSE
      ),
      data.frame(
        study = "Study 2",
        study2_summary,
        stringsAsFactors = FALSE
      )
    )
  )

message("5. Checking row counts, finite inference, and robust-method coverage...")

expected_study1_rows <-
  sum(
    study1_design$method_scope ==
      "all"
  ) *
    length(study1_methods) +
  sum(
    study1_design$method_scope ==
      "robust_only"
  )

expected_study2_rows <-
  sum(
    study2_design$method_scope ==
      "all"
  ) *
    length(study2_methods) +
  2L *
    sum(
      study2_design$method_scope ==
        "robust_only"
    )

add_check(
  "Pilot plumbing",
  "study1_expected_method_rows_returned",
  nrow(study1_replicates) ==
    expected_study1_rows,
  details = paste(
    nrow(study1_replicates),
    "observed;",
    expected_study1_rows,
    "expected"
  )
)

add_check(
  "Pilot plumbing",
  "study2_expected_method_rows_returned",
  nrow(study2_replicates) ==
    expected_study2_rows,
  details = paste(
    nrow(study2_replicates),
    "observed;",
    expected_study2_rows,
    "expected"
  )
)

successful <- all_replicates[
  all_replicates$fit_success %in%
    TRUE,
  ,
  drop = FALSE
]

successful_finite <- if (
  nrow(successful) == 0L
) {
  FALSE
} else {
  all(
    is.finite(
      successful$estimate
    )
  ) &&
    all(
      is.finite(
        successful$std_error
      )
    ) &&
    all(
      is.finite(
        successful$df
      )
    ) &&
    all(
      is.finite(
        successful$p_value
      )
    ) &&
    all(
      is.finite(
        successful$conf_low
      )
    ) &&
    all(
      is.finite(
        successful$conf_high
      )
    )
}

add_check(
  "Pilot inference",
  "every_successful_fit_has_finite_inference",
  successful_finite,
  details = paste(
    nrow(successful),
    "successful rows of",
    nrow(all_replicates),
    "total"
  )
)

robust_rows <- all_replicates[
  all_replicates$method %in%
    c(
      "robust_ri",
      "robust_rs"
    ),
  ,
  drop = FALSE
]

robust_success_by_study_method <-
  aggregate(
    fit_success ~ study + method,
    data = robust_rows,
    FUN = function(x) {
      sum(x %in% TRUE)
    }
  )

robust_attempts_by_study_method <-
  aggregate(
    fit_success ~ study + method,
    data = robust_rows,
    FUN = length
  )
names(
  robust_attempts_by_study_method
)[
  names(
    robust_attempts_by_study_method
  ) == "fit_success"
] <- "attempts"

robust_success <-
  merge(
    robust_attempts_by_study_method,
    robust_success_by_study_method,
    by = c(
      "study",
      "method"
    ),
    all = TRUE,
    sort = FALSE
  )
names(
  robust_success
)[
  names(robust_success) ==
    "fit_success"
] <- "successful"

robust_success$
  success_rate <-
  robust_success$successful /
  robust_success$attempts

required_robust_methods <- data.frame(
  study = c(
    "Study 1",
    "Study 2",
    "Study 2"
  ),
  method = c(
    "robust_ri",
    "robust_ri",
    "robust_rs"
  ),
  stringsAsFactors = FALSE
)

coverage_check <- merge(
  required_robust_methods,
  robust_success,
  by = c(
    "study",
    "method"
  ),
  all.x = TRUE,
  sort = FALSE
)

add_check(
  "Robust mixed coverage",
  "every_new_method_has_at_least_one_success",
  all(
    is.finite(
      coverage_check$successful
    )
  ) &&
    all(
      coverage_check$successful >
        0
    ),
  details = paste(
    coverage_check$study,
    coverage_check$method,
    coverage_check$successful,
    "/",
    coverage_check$attempts,
    sep = ":",
    collapse = "; "
  )
)

cluster_coverage <- aggregate(
  fit_success ~
    study +
    method +
    n_clusters,
  data = robust_rows,
  FUN = function(x) {
    any(x %in% TRUE)
  }
)

add_check(
  "Robust mixed coverage",
  "robust_methods_succeed_across_all_cluster_tiers",
  all(
    cluster_coverage$fit_success
  ),
  details = paste(
    cluster_coverage$study,
    cluster_coverage$method,
    cluster_coverage$n_clusters,
    cluster_coverage$fit_success,
    sep = ":",
    collapse = "; "
  )
)

message("6. Testing full-schedule order and robust-subset invariance...")

# Study 1 representative condition:
# G = 10, beta = 0.10, bad leverage.
study1_reference_condition <-
  study1_design[
    study1_design$n_clusters == 10L &
      study1_design$beta == 0.10 &
      study1_design$contamination ==
        "bad_leverage",
    ,
    drop = FALSE
  ]

study1_reference_forward <-
  study1_replicates[
    study1_replicates$condition_id ==
      study1_reference_condition$
        condition_id,
    ,
    drop = FALSE
  ]

study1_reference_reverse <-
  suppressWarnings(
    pwr_func_study1(
      n_clusters = 10L,
      cluster_size = 40L,
      beta = 0.10,
      intercept = 0,
      random_intercept_sd = 1,
      residual_sd = 1,
      x_sd = 1,
      contamination =
        "bad_leverage",
      contamination_prop = 0.05,
      contamination_size = 0.375,
      leverage_size = 4,
      reps = 1L,
      alpha = 0.05,
      methods =
        rev(study1_methods),
      seed =
        study1_reference_condition$seed,
      keep_replicates = TRUE
    )
  )$replicates

study1_compare_columns <- intersect(
  names(study1_reference_forward),
  names(study1_reference_reverse)
)
study1_compare_columns <- setdiff(
  study1_compare_columns,
  c(
    "condition_id",
    "n_clusters",
    "beta",
    "contamination",
    "random_slope_sd",
    "method_scope",
    "runtime_sec"
  )
)

study1_order_match <-
  phase4d_equal(
    phase4d_sort_method_rows(
      study1_reference_forward[
        ,
        study1_compare_columns,
        drop = FALSE
      ]
    ),
    phase4d_sort_method_rows(
      study1_reference_reverse[
        ,
        study1_compare_columns,
        drop = FALSE
      ]
    ),
    tolerance = 1e-10
  )

study1_robust_subset <-
  suppressWarnings(
    pwr_func_study1(
      n_clusters = 10L,
      cluster_size = 40L,
      beta = 0.10,
      intercept = 0,
      random_intercept_sd = 1,
      residual_sd = 1,
      x_sd = 1,
      contamination =
        "bad_leverage",
      contamination_prop = 0.05,
      contamination_size = 0.375,
      leverage_size = 4,
      reps = 1L,
      alpha = 0.05,
      methods = "robust_ri",
      seed =
        study1_reference_condition$seed,
      keep_replicates = TRUE
    )
  )$replicates

study1_robust_full <-
  study1_reference_forward[
    study1_reference_forward$method ==
      "robust_ri",
    study1_compare_columns,
    drop = FALSE
  ]

study1_robust_subset <-
  study1_robust_subset[
    ,
    study1_compare_columns,
    drop = FALSE
  ]

rownames(study1_robust_full) <- NULL
rownames(study1_robust_subset) <- NULL

study1_subset_match <-
  phase4d_equal(
    study1_robust_full,
    study1_robust_subset,
    tolerance = 1e-10
  )

# Study 2 representative condition:
# G = 10, beta = 0.10, slope SD = 0.05, vertical contamination.
study2_reference_condition <-
  study2_design[
    study2_design$n_clusters == 10L &
      study2_design$beta == 0.10 &
      study2_design$random_slope_sd ==
        0.05 &
      study2_design$contamination ==
        "vertical",
    ,
    drop = FALSE
  ]

study2_reference_forward <-
  study2_replicates[
    study2_replicates$condition_id ==
      study2_reference_condition$
        condition_id,
    ,
    drop = FALSE
  ]

study2_reference_reverse <-
  suppressWarnings(
    pwr_func_study2(
      n_clusters = 10L,
      cluster_size = 40L,
      beta = 0.10,
      intercept = 0,
      random_intercept_sd = 1,
      random_slope_sd = 0.05,
      residual_sd = 1,
      x_sd = 1,
      contamination = "vertical",
      contamination_prop = 0.05,
      contamination_size = 6,
      reps = 1L,
      alpha = 0.05,
      methods =
        rev(study2_methods),
      seed =
        study2_reference_condition$seed,
      keep_replicates = TRUE
    )
  )$replicates

study2_compare_columns <- intersect(
  names(study2_reference_forward),
  names(study2_reference_reverse)
)
study2_compare_columns <- setdiff(
  study2_compare_columns,
  c(
    "condition_id",
    "n_clusters",
    "beta",
    "contamination",
    "random_slope_sd",
    "method_scope",
    "runtime_sec"
  )
)

study2_order_match <-
  phase4d_equal(
    phase4d_sort_method_rows(
      study2_reference_forward[
        ,
        study2_compare_columns,
        drop = FALSE
      ]
    ),
    phase4d_sort_method_rows(
      study2_reference_reverse[
        ,
        study2_compare_columns,
        drop = FALSE
      ]
    ),
    tolerance = 1e-10
  )

study2_robust_subset <-
  suppressWarnings(
    pwr_func_study2(
      n_clusters = 10L,
      cluster_size = 40L,
      beta = 0.10,
      intercept = 0,
      random_intercept_sd = 1,
      random_slope_sd = 0.05,
      residual_sd = 1,
      x_sd = 1,
      contamination = "vertical",
      contamination_prop = 0.05,
      contamination_size = 6,
      reps = 1L,
      alpha = 0.05,
      methods = c(
        "robust_ri",
        "robust_rs"
      ),
      seed =
        study2_reference_condition$seed,
      keep_replicates = TRUE
    )
  )$replicates

study2_robust_full <-
  study2_reference_forward[
    study2_reference_forward$method %in%
      c(
        "robust_ri",
        "robust_rs"
      ),
    study2_compare_columns,
    drop = FALSE
  ]

study2_robust_subset <-
  study2_robust_subset[
    ,
    study2_compare_columns,
    drop = FALSE
  ]

study2_robust_full <-
  phase4d_sort_method_rows(
    study2_robust_full
  )
study2_robust_subset <-
  phase4d_sort_method_rows(
    study2_robust_subset
  )

study2_subset_match <-
  phase4d_equal(
    study2_robust_full,
    study2_robust_subset,
    tolerance = 1e-10
  )

add_check(
  "Method invariance",
  "study1_full_schedule_order_invariance",
  study1_order_match,
  details = paste(
    "matched:",
    study1_order_match
  )
)

add_check(
  "Method invariance",
  "study2_full_schedule_order_invariance",
  study2_order_match,
  details = paste(
    "matched:",
    study2_order_match
  )
)

add_check(
  "Method invariance",
  "study1_robust_subset_matches_full_schedule",
  study1_subset_match,
  details = paste(
    "matched:",
    study1_subset_match
  )
)

add_check(
  "Method invariance",
  "study2_robust_subset_matches_full_schedule",
  study2_subset_match,
  details = paste(
    "matched:",
    study2_subset_match
  )
)

message("7. Characterizing failures, convergence, boundaries, and runtime...")

method_diagnostics <- aggregate(
  cbind(
    attempted =
      rep(
        1L,
        nrow(all_replicates)
      ),
    successful =
      as.integer(
        all_replicates$fit_success %in%
          TRUE
      ),
    boundary =
      as.integer(
        all_replicates$singular %in%
          TRUE
      ),
    nonconverged =
      as.integer(
        all_replicates$converged %in%
          FALSE
      ),
    runtime_sec =
      all_replicates$runtime_sec
  ) ~
    study +
    method,
  data = all_replicates,
  FUN = sum,
  na.action = stats::na.pass
)

method_diagnostics$
  failure_count <-
  method_diagnostics$attempted -
  method_diagnostics$successful

method_diagnostics$
  success_rate <-
  method_diagnostics$successful /
  method_diagnostics$attempted

method_diagnostics$
  boundary_rate <-
  method_diagnostics$boundary /
  method_diagnostics$attempted

method_diagnostics$
  nonconvergence_rate <-
  method_diagnostics$nonconverged /
  method_diagnostics$attempted

# The pilot may legitimately contain failed or boundary fits. These are
# diagnostic outputs rather than required zero-event gates.
add_check(
  "Pilot diagnostics",
  "pilot_completed_with_diagnostics_recorded",
  nrow(method_diagnostics) ==
    length(study1_methods) +
      length(study2_methods),
  required = TRUE,
  details = paste(
    nrow(method_diagnostics),
    "study-method diagnostic rows"
  )
)

message("8. Confirming definitive runners remain untouched for the next phase...")

study1_runner_path <- file.path(
  project_root,
  "data-raw",
  "study1_final_simulation.R"
)
study2_runner_path <- file.path(
  project_root,
  "data-raw",
  "study2_final_simulation.R"
)

runner_scan <- data.frame(
  runner = c(
    "study1_final_simulation.R",
    "study2_final_simulation.R"
  ),
  exists = c(
    file.exists(study1_runner_path),
    file.exists(study2_runner_path)
  ),
  has_robust_ri = c(
    if (file.exists(study1_runner_path)) {
      grepl(
        "robust_ri",
        paste(
          readLines(
            study1_runner_path,
            warn = FALSE
          ),
          collapse = "\n"
        ),
        fixed = TRUE
      )
    } else {
      NA
    },
    if (file.exists(study2_runner_path)) {
      grepl(
        "robust_ri",
        paste(
          readLines(
            study2_runner_path,
            warn = FALSE
          ),
          collapse = "\n"
        ),
        fixed = TRUE
      )
    } else {
      NA
    }
  ),
  has_robust_rs = c(
    NA,
    if (file.exists(study2_runner_path)) {
      grepl(
        "robust_rs",
        paste(
          readLines(
            study2_runner_path,
            warn = FALSE
          ),
          collapse = "\n"
        ),
        fixed = TRUE
      )
    } else {
      NA
    }
  ),
  stringsAsFactors = FALSE
)

add_check(
  "Runner staging",
  "definitive_runners_are_still_pre_integration",
  all(runner_scan$exists) &&
    identical(
      runner_scan$has_robust_ri[1L],
      FALSE
    ) &&
    identical(
      runner_scan$has_robust_ri[2L],
      FALSE
    ) &&
    identical(
      runner_scan$has_robust_rs[2L],
      FALSE
    ),
  details = paste(
    "Study1 robust_ri:",
    runner_scan$has_robust_ri[1L],
    "; Study2 robust_ri:",
    runner_scan$has_robust_ri[2L],
    "; Study2 robust_rs:",
    runner_scan$has_robust_rs[2L]
  )
)

message("9. Saving Phase 4D evidence...")

checks_df <- do.call(
  rbind,
  checks
)
rownames(checks_df) <- NULL

source_files <- c(
  production_paths,
  phase4d_pilot = file.path(
    project_root,
    "data-raw",
    "robust_mixed_model_phase4d_end_to_end_pilot.R"
  ),
  study1_final_runner =
    study1_runner_path,
  study2_final_runner =
    study2_runner_path
)

source_checksums <- data.frame(
  source = names(source_files),
  path = normalizePath(
    source_files,
    winslash = "/",
    mustWork = TRUE
  ),
  md5 = unname(
    tools::md5sum(
      source_files
    )
  ),
  stringsAsFactors = FALSE
)

package_names <- c(
  "mmiCATs",
  "robustlmm",
  "lme4",
  "lmerTest",
  "pbkrtest",
  "clubSandwich",
  "clusterSEs",
  "robust",
  "robustbase",
  "testthat"
)

package_versions <- data.frame(
  package = package_names,
  version = vapply(
    package_names,
    function(package_name) {
      if (
        requireNamespace(
          package_name,
          quietly = TRUE
        )
      ) {
        as.character(
          utils::packageVersion(
            package_name
          )
        )
      } else {
        NA_character_
      }
    },
    character(1)
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  checks_df,
  file.path(
    output_dir,
    "phase4d_checks.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  study1_design,
  file.path(
    output_dir,
    "phase4d_study1_design.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  study2_design,
  file.path(
    output_dir,
    "phase4d_study2_design.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  all_replicates,
  file.path(
    output_dir,
    "phase4d_replicates.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  all_summaries,
  file.path(
    output_dir,
    "phase4d_summaries.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  robust_success,
  file.path(
    output_dir,
    "phase4d_robust_method_success.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  cluster_coverage,
  file.path(
    output_dir,
    "phase4d_robust_cluster_coverage.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  method_diagnostics,
  file.path(
    output_dir,
    "phase4d_method_diagnostics.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  runner_scan,
  file.path(
    output_dir,
    "phase4d_runner_scan.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  source_baseline,
  file.path(
    output_dir,
    "phase4d_source_baseline.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  source_checksums,
  file.path(
    output_dir,
    "phase4d_source_checksums.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  package_versions,
  file.path(
    output_dir,
    "phase4d_package_versions.csv"
  ),
  row.names = FALSE
)

saveRDS(
  list(
    checks = checks_df,
    study1_design =
      study1_design,
    study2_design =
      study2_design,
    replicates =
      all_replicates,
    summaries =
      all_summaries,
    robust_method_success =
      robust_success,
    robust_cluster_coverage =
      cluster_coverage,
    method_diagnostics =
      method_diagnostics,
    runner_scan =
      runner_scan,
    source_baseline =
      source_baseline,
    source_checksums =
      source_checksums,
    package_versions =
      package_versions,
    session_info =
      utils::sessionInfo()
  ),
  file.path(
    output_dir,
    "phase4d_results.rds"
  ),
  version = 3
)

writeLines(
  capture.output(
    utils::sessionInfo()
  ),
  file.path(
    output_dir,
    "session_info.txt"
  ),
  useBytes = TRUE
)

required_failures <- checks_df[
  checks_df$required %in% TRUE &
    !(checks_df$passed %in% TRUE),
  ,
  drop = FALSE
]

summary_lines <- c(
  "Robust mixed-model Phase 4D end-to-end integration pilot",
  "",
  paste(
    "Required checks passed:",
    sum(
      checks_df$required %in% TRUE &
        checks_df$passed %in% TRUE
    ),
    "of",
    sum(
      checks_df$required %in% TRUE
    )
  ),
  paste(
    "Unresolved required checks:",
    nrow(required_failures)
  ),
  paste(
    "Study 1 pilot rows:",
    nrow(study1_replicates)
  ),
  paste(
    "Study 2 pilot rows:",
    nrow(study2_replicates)
  ),
  paste(
    "Successful rows:",
    nrow(successful),
    "of",
    nrow(all_replicates)
  ),
  paste(
    "Study 1 full-schedule order invariance:",
    study1_order_match
  ),
  paste(
    "Study 2 full-schedule order invariance:",
    study2_order_match
  ),
  paste(
    "Study 1 robust subset invariance:",
    study1_subset_match
  ),
  paste(
    "Study 2 robust subset invariance:",
    study2_subset_match
  )
)

writeLines(
  summary_lines,
  file.path(
    output_dir,
    "phase4d_summary.txt"
  ),
  useBytes = TRUE
)

message("")
message("Phase 4D checks:")
print(
  checks_df,
  row.names = FALSE
)

message("")
message("Robust mixed-model success by study/method:")
print(
  robust_success,
  row.names = FALSE
)

message("")
message("Robust mixed-model cluster-tier coverage:")
print(
  cluster_coverage,
  row.names = FALSE
)

message("")
message("Method diagnostics:")
print(
  method_diagnostics,
  row.names = FALSE
)

message("")
message(
  paste(
    "Required checks passed:",
    sum(
      checks_df$required %in% TRUE &
        checks_df$passed %in% TRUE
    ),
    "of",
    sum(
      checks_df$required %in% TRUE
    )
  )
)
message(
  paste(
    "Results saved to:",
    output_dir
  )
)

if (nrow(required_failures) > 0L) {
  stop(
    paste(
      nrow(required_failures),
      "required Phase 4D check(s) failed.",
      "Review the saved evidence before modifying the definitive runners."
    ),
    call. = FALSE
  )
}

message("")
message(
  "All Phase 4D end-to-end robust mixed-model gates passed."
)
