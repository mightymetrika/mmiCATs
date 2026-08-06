# Independent CATs numerical validation
#
# Run after:
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
  "phase2a-numerical-validation"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

alpha <- 0.05
comparison_rows <- list()
comparison_index <- 0L
check_rows <- list()
check_index <- 0L
cluster_rows <- list()
cluster_index <- 0L

add_check <- function(category,
                      check,
                      passed,
                      required,
                      details) {
  check_index <<- check_index + 1L
  check_rows[[check_index]] <<-
    data.frame(
      category = category,
      check = check,
      passed = isTRUE(passed),
      required = isTRUE(required),
      details = as.character(details),
      stringsAsFactors = FALSE
    )
}

add_comparison <- function(comparison) {
  comparison_index <<-
    comparison_index + 1L
  comparison_rows[[comparison_index]] <<-
    comparison
}

add_cluster_comparison <- function(comparison) {
  cluster_index <<-
    cluster_index + 1L
  cluster_rows[[cluster_index]] <<-
    comparison
}

# -------------------------------------------------------------------------
# 1. Hand-calculated scalar specification
# -------------------------------------------------------------------------

hand_diagnostics <- data.frame(
  cluster = as.character(1:4),
  intercept = c(
    1.00,
    1.10,
    0.90,
    1.20
  ),
  x = c(
    0.10,
    0.20,
    0.30,
    0.40
  ),
  retained_before_truncation =
    rep(TRUE, 4L),
  stringsAsFactors = FALSE
)

hand_result <- rca_aggregate_coefficients(
  diagnostics = hand_diagnostics,
  alpha = alpha,
  truncation_rule = "none"
)

hand_expected <- list(
  estimate = 0.25,
  std_error = 0.06454972243679027,
  df = 3,
  p_value = 0.03046629166217096,
  conf_low = 0.04457397432394794,
  conf_high = 0.45542602567605206
)

hand_comparison <- rca_compare_results(
  reference = hand_expected,
  observed = hand_result,
  comparison =
    "Hand-calculated scalar specification",
  tolerance = 1e-12
)

add_comparison(hand_comparison)
add_check(
  category = "Mathematical specification",
  check = "hand_calculation_matches",
  passed = all(hand_comparison$passed),
  required = TRUE,
  details = paste(
    sum(hand_comparison$passed),
    "of",
    nrow(hand_comparison),
    "quantities matched."
  )
)

# -------------------------------------------------------------------------
# 2. Ordinary CATs versus independent oracle
# -------------------------------------------------------------------------

validation_data <- rca_make_validation_data()

ordinary_oracle <- rca_oracle(
  dat = validation_data,
  engine = "glm",
  alpha = alpha,
  truncation_rule = "none",
  consume_template = FALSE
)

ordinary_package <- rca_package_cats(
  dat = validation_data,
  alpha = alpha,
  truncate = FALSE,
  retained_clusters =
    ordinary_oracle$aggregate$
      retained_clusters
)

ordinary_comparison <- rca_compare_results(
  reference = ordinary_oracle$aggregate,
  observed = ordinary_package,
  comparison =
    "Independent ordinary CATs versus clusterSEs",
  tolerance = 1e-10
)

add_comparison(ordinary_comparison)
add_check(
  category = "Ordinary CATs",
  check =
    "ordinary_oracle_matches_clusterSEs",
  passed = all(
    ordinary_comparison$passed
  ),
  required = TRUE,
  details = paste(
    sum(ordinary_comparison$passed),
    "of",
    nrow(ordinary_comparison),
    "quantities matched."
  )
)

# -------------------------------------------------------------------------
# 3. Package and documented truncation rules
# -------------------------------------------------------------------------

truncation_data <- rca_make_truncation_data()

package_rule_oracle <- rca_oracle(
  dat = truncation_data,
  engine = "glm",
  alpha = alpha,
  truncation_rule = "clusterSEs",
  consume_template = FALSE
)

documented_rule_oracle <- rca_oracle(
  dat = truncation_data,
  engine = "glm",
  alpha = alpha,
  truncation_rule = "documented",
  consume_template = FALSE
)

package_truncated <- rca_package_cats(
  dat = truncation_data,
  alpha = alpha,
  truncate = TRUE,
  retained_clusters =
    package_rule_oracle$aggregate$
      retained_clusters
)

truncation_package_comparison <-
  rca_compare_results(
    reference =
      package_rule_oracle$aggregate,
    observed = package_truncated,
    comparison = paste(
      "Independent package-rule truncation",
      "versus clusterSEs"
    ),
    tolerance = 1e-10
  )

add_comparison(
  truncation_package_comparison
)

add_check(
  category = "Truncated CATs",
  check =
    "package_rule_oracle_matches_clusterSEs",
  passed = all(
    truncation_package_comparison$passed
  ),
  required = TRUE,
  details = paste(
    sum(
      truncation_package_comparison$passed
    ),
    "of",
    nrow(truncation_package_comparison),
    "quantities matched."
  )
)

package_dropped <- paste(
  package_rule_oracle$aggregate$
    dropped_cluster_ids,
  collapse = ","
)
documented_dropped <- paste(
  documented_rule_oracle$aggregate$
    dropped_cluster_ids,
  collapse = ","
)

add_check(
  category = "Truncated CATs",
  check =
    "constructed_example_distinguishes_rules",
  passed = (
    package_rule_oracle$aggregate$
      retained_clusters == 5L &&
      documented_rule_oracle$aggregate$
        retained_clusters == 4L &&
      identical(
        documented_rule_oracle$aggregate$
          dropped_cluster_ids,
        "1"
      )
  ),
  required = TRUE,
  details = paste0(
    "Package-rule dropped: ",
    ifelse(
      nzchar(package_dropped),
      package_dropped,
      "none"
    ),
    "; documented-rule dropped: ",
    ifelse(
      nzchar(documented_dropped),
      documented_dropped,
      "none"
    ),
    "."
  )
)

truncation_summary <- data.frame(
  cluster = package_rule_oracle$
    diagnostics$cluster,
  intercept = package_rule_oracle$
    diagnostics$intercept,
  x = package_rule_oracle$
    diagnostics$x,
  retained_package_rule =
    package_rule_oracle$aggregate$
      retained_after_truncation,
  retained_documented_rule =
    documented_rule_oracle$aggregate$
      retained_after_truncation,
  stringsAsFactors = FALSE
)

# -------------------------------------------------------------------------
# 4. Robust CATs: oracle, public function, simulation helper
# -------------------------------------------------------------------------

robust_seeds <- c(
  robust = 20261002L,
  robustbase = 20261003L
)

for (engine in names(robust_seeds)) {
  seed <- unname(
    robust_seeds[engine]
  )

  set.seed(seed)
  robust_oracle <- rca_oracle(
    dat = validation_data,
    engine = engine,
    alpha = alpha,
    truncation_rule = "none",
    consume_template = TRUE
  )

  robust_public <-
    rca_public_robust_cats(
      dat = validation_data,
      engine = engine,
      seed = seed,
      alpha = alpha,
      retained_clusters =
        robust_oracle$aggregate$
          retained_clusters
    )

  robust_simulation <-
    rca_simulation_robust_cats(
      dat = validation_data,
      engine = engine,
      seed = seed,
      alpha = alpha
    )

  public_comparison <-
    rca_compare_results(
      reference =
        robust_oracle$aggregate,
      observed = robust_public,
      comparison = paste(
        engine,
        "oracle versus public function"
      ),
      tolerance = 1e-8
    )

  simulation_comparison <-
    rca_compare_results(
      reference =
        robust_oracle$aggregate,
      observed = robust_simulation,
      comparison = paste(
        engine,
        "oracle versus simulation helper"
      ),
      tolerance = 1e-8
    )

  cluster_comparison <-
    rca_compare_cluster_coefficients(
      oracle_diagnostics =
        robust_oracle$diagnostics,
      simulation_diagnostics =
        robust_simulation$
          cluster_diagnostics,
      engine = engine,
      tolerance = 1e-8
    )

  add_comparison(public_comparison)
  add_comparison(
    simulation_comparison
  )
  add_cluster_comparison(
    cluster_comparison
  )

  add_check(
    category = "Robust CATs",
    check = paste0(
      engine,
      "_oracle_matches_public"
    ),
    passed = all(
      public_comparison$passed
    ),
    required = TRUE,
    details = paste(
      sum(public_comparison$passed),
      "of",
      nrow(public_comparison),
      "quantities matched."
    )
  )

  add_check(
    category = "Robust CATs",
    check = paste0(
      engine,
      "_oracle_matches_simulation"
    ),
    passed = all(
      simulation_comparison$passed
    ),
    required = TRUE,
    details = paste(
      sum(
        simulation_comparison$passed
      ),
      "of",
      nrow(simulation_comparison),
      "quantities matched."
    )
  )

  add_check(
    category = "Robust CATs",
    check = paste0(
      engine,
      "_cluster_coefficients_match"
    ),
    passed = all(
      cluster_comparison$passed
    ),
    required = TRUE,
    details = paste(
      sum(cluster_comparison$passed),
      "of",
      nrow(cluster_comparison),
      "cluster fits matched."
    )
  )
}

# -------------------------------------------------------------------------
# 5. Study 1 and Study 2 dispatch equivalence
# -------------------------------------------------------------------------

dispatch_rows <- list()
dispatch_index <- 0L

for (method in c(
  "cats_robust",
  "cats_robustbase"
)) {
  method_seed <- if (
    identical(method, "cats_robust")
  ) {
    20261004L
  } else {
    20261005L
  }

  study1_result <- study1_fit_method(
    dat = validation_data,
    method = method,
    beta = 0.25,
    alpha = alpha,
    replicate_id = 1L,
    method_seed = method_seed
  )

  study2_result <- study2_fit_method(
    dat = validation_data,
    method = method,
    beta = 0.25,
    alpha = alpha,
    replicate_id = 1L,
    method_seed = method_seed,
    realized_mean_slope = 0.25,
    realized_random_slope_sd = 0
  )

  quantities <- c(
    "estimate",
    "std_error",
    "df",
    "p_value",
    "conf_low",
    "conf_high",
    "retained_clusters"
  )

  differences <- vapply(
    quantities,
    function(quantity) {
      abs(
        study1_result[[quantity]] -
          study2_result[[quantity]]
      )
    },
    numeric(1)
  )

  dispatch_index <- dispatch_index + 1L
  dispatch_rows[[dispatch_index]] <-
    data.frame(
      method = method,
      quantity = quantities,
      study1_value = vapply(
        quantities,
        function(quantity) {
          study1_result[[quantity]]
        },
        numeric(1)
      ),
      study2_value = vapply(
        quantities,
        function(quantity) {
          study2_result[[quantity]]
        },
        numeric(1)
      ),
      absolute_difference =
        differences,
      passed = differences <= 1e-8,
      stringsAsFactors = FALSE
    )

  add_check(
    category = "Study dispatch",
    check = paste0(
      method,
      "_study1_study2_equivalence"
    ),
    passed = all(
      differences <= 1e-8
    ),
    required = TRUE,
    details = paste(
      sum(differences <= 1e-8),
      "of",
      length(differences),
      "quantities matched."
    )
  )
}

dispatch_comparison <- do.call(
  rbind,
  dispatch_rows
)

# -------------------------------------------------------------------------
# 6. Method-order invariance for both robust engines
# -------------------------------------------------------------------------

order_one <- suppressWarnings(
  pwr_func_study1(
    n_clusters = 5,
    cluster_size = 15,
    beta = 0.10,
    contamination = "none",
    reps = 1,
    methods = c(
      "cats_robust",
      "cats_robustbase"
    ),
    seed = 20261006L,
    keep_replicates = TRUE
  )
)

order_two <- suppressWarnings(
  pwr_func_study1(
    n_clusters = 5,
    cluster_size = 15,
    beta = 0.10,
    contamination = "none",
    reps = 1,
    methods = c(
      "cats_robustbase",
      "cats_robust"
    ),
    seed = 20261006L,
    keep_replicates = TRUE
  )
)

order_quantities <- c(
  "estimate",
  "std_error",
  "df",
  "p_value",
  "conf_low",
  "conf_high",
  "retained_clusters"
)

order_comparison_rows <- list()
order_comparison_index <- 0L

for (method in c(
  "cats_robust",
  "cats_robustbase"
)) {
  first <- order_one$replicates[
    order_one$replicates$method ==
      method,
    ,
    drop = FALSE
  ]
  second <- order_two$replicates[
    order_two$replicates$method ==
      method,
    ,
    drop = FALSE
  ]

  differences <- vapply(
    order_quantities,
    function(quantity) {
      abs(
        first[[quantity]] -
          second[[quantity]]
      )
    },
    numeric(1)
  )

  order_comparison_index <-
    order_comparison_index + 1L
  order_comparison_rows[[
    order_comparison_index
  ]] <- data.frame(
    method = method,
    quantity = order_quantities,
    first_order_value = vapply(
      order_quantities,
      function(quantity) {
        first[[quantity]]
      },
      numeric(1)
    ),
    reverse_order_value = vapply(
      order_quantities,
      function(quantity) {
        second[[quantity]]
      },
      numeric(1)
    ),
    absolute_difference =
      differences,
    passed = differences <= 1e-8,
    stringsAsFactors = FALSE
  )

  add_check(
    category = "Random-number behavior",
    check = paste0(
      method,
      "_method_order_invariance"
    ),
    passed = all(
      differences <= 1e-8
    ),
    required = TRUE,
    details = paste(
      sum(differences <= 1e-8),
      "of",
      length(differences),
      "quantities matched."
    )
  )
}

order_comparison <- do.call(
  rbind,
  order_comparison_rows
)

# -------------------------------------------------------------------------
# 7. Row and cluster-label invariance
# -------------------------------------------------------------------------

invariance_rows <- list()
invariance_index <- 0L

set.seed(20261007L)
global_shuffle <- validation_data[
  sample(seq_len(nrow(validation_data))),
  ,
  drop = FALSE
]
rownames(global_shuffle) <- NULL

within_cluster_shuffle <- do.call(
  rbind,
  lapply(
    split(
      validation_data,
      validation_data$cluster
    ),
    function(cluster_dat) {
      cluster_dat[
        sample(seq_len(nrow(cluster_dat))),
        ,
        drop = FALSE
      ]
    }
  )
)
within_cluster_shuffle$cluster <- factor(
  as.character(
    within_cluster_shuffle$cluster
  ),
  levels = levels(
    validation_data$cluster
  )
)
rownames(within_cluster_shuffle) <- NULL

label_map <- setNames(
  paste0(
    "cluster_",
    rev(levels(validation_data$cluster))
  ),
  levels(validation_data$cluster)
)
relabeled <- validation_data
relabeled$cluster <- factor(
  unname(
    label_map[
      as.character(
        relabeled$cluster
      )
    ]
  ),
  levels = unique(
    unname(label_map)
  )
)

for (engine in names(robust_seeds)) {
  seed <- unname(
    robust_seeds[engine]
  )

  set.seed(seed)
  baseline <- rca_oracle(
    dat = validation_data,
    engine = engine,
    alpha = alpha,
    truncation_rule = "none",
    consume_template = TRUE
  )

  variants <- list(
    within_cluster_row_order =
      within_cluster_shuffle,
    global_row_order =
      global_shuffle,
    cluster_label_permutation =
      relabeled
  )

  for (variant_name in names(variants)) {
    set.seed(seed)
    variant <- rca_oracle(
      dat = variants[[variant_name]],
      engine = engine,
      alpha = alpha,
      truncation_rule = "none",
      consume_template = TRUE
    )

    comparison <- rca_compare_results(
      reference = baseline$aggregate,
      observed = variant$aggregate,
      comparison = paste(
        engine,
        variant_name
      ),
      tolerance = 1e-8
    )

    invariance_index <-
      invariance_index + 1L
    comparison$engine <- engine
    comparison$variant <- variant_name
    invariance_rows[[
      invariance_index
    ]] <- comparison

    add_check(
      category = "Random-number behavior",
      check = paste0(
        engine,
        "_",
        variant_name
      ),
      passed = all(
        comparison$passed
      ),
      required = TRUE,
      details = paste(
        sum(comparison$passed),
        "of",
        nrow(comparison),
        "quantities matched."
      )
    )
  }
}

invariance_comparison <- do.call(
  rbind,
  invariance_rows
)

# -------------------------------------------------------------------------
# 8. Mutation detection
# -------------------------------------------------------------------------

hand_matrix <- as.matrix(
  hand_diagnostics[
    ,
    c("intercept", "x"),
    drop = FALSE
  ]
)
colnames(hand_matrix) <- c(
  "(Intercept)",
  "x"
)

mutation_rows <- list()
mutation_index <- 0L

for (mutation in c(
  "population_variance",
  "omit_sqrt_g",
  "wrong_df"
)) {
  mutated <- rca_mutated_aggregate(
    coefficient_matrix = hand_matrix,
    alpha = alpha,
    mutation = mutation
  )

  comparison <- rca_compare_results(
    reference = hand_result,
    observed = mutated,
    comparison = paste(
      "Mutation:",
      mutation
    ),
    tolerance = 1e-12
  )

  mutation_index <- mutation_index + 1L
  mutation_rows[[mutation_index]] <-
    comparison

  detected <- any(
    !comparison$passed
  )

  add_check(
    category = "Mutation testing",
    check = paste0(
      mutation,
      "_detected"
    ),
    passed = detected,
    required = TRUE,
    details = paste(
      sum(!comparison$passed),
      "of",
      nrow(comparison),
      "quantities differed from the oracle."
    )
  )
}

mutation_comparison <- do.call(
  rbind,
  mutation_rows
)

# -------------------------------------------------------------------------
# 9. Minimum retained-cluster behavior
# -------------------------------------------------------------------------

two_cluster_diagnostics <- data.frame(
  cluster = c("A", "B"),
  intercept = c(0.0, 0.2),
  x = c(0.1, 0.3),
  retained_before_truncation =
    c(TRUE, TRUE),
  stringsAsFactors = FALSE
)

two_cluster_result <- rca_capture(
  rca_aggregate_coefficients(
    diagnostics =
      two_cluster_diagnostics,
    alpha = alpha,
    truncation_rule = "none"
  )
)

one_cluster_diagnostics <-
  two_cluster_diagnostics
one_cluster_diagnostics$
  retained_before_truncation <-
  c(TRUE, FALSE)

one_cluster_result <- rca_capture(
  rca_aggregate_coefficients(
    diagnostics =
      one_cluster_diagnostics,
    alpha = alpha,
    truncation_rule = "none"
  )
)

add_check(
  category = "Retained clusters",
  check =
    "two_retained_clusters_supported",
  passed = (
    is.na(two_cluster_result$error) &&
      two_cluster_result$value$df == 1
  ),
  required = TRUE,
  details = paste(
    "Error:",
    two_cluster_result$error,
    "; df:",
    if (
      is.null(two_cluster_result$value)
    ) {
      NA
    } else {
      two_cluster_result$value$df
    }
  )
)

add_check(
  category = "Retained clusters",
  check =
    "one_retained_cluster_rejected",
  passed = rca_has_text(
    one_cluster_result$error
  ),
  required = TRUE,
  details = paste(
    "Error:",
    one_cluster_result$error
  )
)

# -------------------------------------------------------------------------
# 10. Save all evidence
# -------------------------------------------------------------------------

comparisons <- rca_bind_rows(
  comparison_rows
)
checks <- rca_bind_rows(
  check_rows
)
cluster_comparisons <- rca_bind_rows(
  cluster_rows
)

source_files <- c(
  robust_cats_audit_helpers = file.path(
    project_root,
    "data-raw",
    "robust_cats_audit_helpers.R"
  ),
  robust_cats_audit_numerical_validation =
    file.path(
      project_root,
      "data-raw",
      "robust_cats_audit_numerical_validation.R"
    ),
  pwr_func_study1_helpers = file.path(
    project_root,
    "R",
    "pwr_func_study1_helpers.R"
  ),
  pwr_func_study2_helpers = file.path(
    project_root,
    "R",
    "pwr_func_study2_helpers.R"
  ),
  cluster_im_lmRob = file.path(
    project_root,
    "R",
    "cluster_im_lmRob.R"
  ),
  helpers_cimrob = file.path(
    project_root,
    "R",
    "helpers_cimrob.R"
  )
)

source_checksums <- rca_source_checksums(
  source_files
)

metadata <- list(
  purpose = paste(
    "Independent numerical verification of ordinary,",
    "truncated, and robust CATs before production changes."
  ),
  alpha = alpha,
  package_versions = c(
    mmiCATs = as.character(
      utils::packageVersion("mmiCATs")
    ),
    clusterSEs = as.character(
      utils::packageVersion("clusterSEs")
    ),
    robust = as.character(
      utils::packageVersion("robust")
    ),
    robustbase = as.character(
      utils::packageVersion("robustbase")
    )
  ),
  source_checksums = source_checksums,
  session_info = utils::sessionInfo()
)

results <- list(
  checks = checks,
  comparisons = comparisons,
  cluster_comparisons =
    cluster_comparisons,
  truncation_summary =
    truncation_summary,
  dispatch_comparison =
    dispatch_comparison,
  order_comparison =
    order_comparison,
  invariance_comparison =
    invariance_comparison,
  mutation_comparison =
    mutation_comparison,
  ordinary_oracle =
    ordinary_oracle,
  package_rule_oracle =
    package_rule_oracle,
  documented_rule_oracle =
    documented_rule_oracle,
  metadata = metadata
)

rca_write_csv_atomic(
  checks,
  file.path(
    output_dir,
    "robust_cats_audit_validation_checks.csv"
  )
)

rca_write_csv_atomic(
  comparisons,
  file.path(
    output_dir,
    "robust_cats_audit_numerical_comparisons.csv"
  )
)

rca_write_csv_atomic(
  cluster_comparisons,
  file.path(
    output_dir,
    "robust_cats_audit_cluster_coefficients.csv"
  )
)

rca_write_csv_atomic(
  truncation_summary,
  file.path(
    output_dir,
    "robust_cats_audit_truncation_rules.csv"
  )
)

rca_write_csv_atomic(
  dispatch_comparison,
  file.path(
    output_dir,
    "robust_cats_audit_study_dispatch.csv"
  )
)

rca_write_csv_atomic(
  order_comparison,
  file.path(
    output_dir,
    "robust_cats_audit_method_order.csv"
  )
)

rca_write_csv_atomic(
  invariance_comparison,
  file.path(
    output_dir,
    "robust_cats_audit_invariance.csv"
  )
)

rca_write_csv_atomic(
  mutation_comparison,
  file.path(
    output_dir,
    "robust_cats_audit_mutation_detection.csv"
  )
)

rca_write_csv_atomic(
  source_checksums,
  file.path(
    output_dir,
    "robust_cats_audit_source_checksums.csv"
  )
)

rca_save_rds_atomic(
  results,
  file.path(
    output_dir,
    "robust_cats_audit_phase2a_results.rds"
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

required_failures <- checks[
  checks$required %in% TRUE &
    !(checks$passed %in% TRUE),
  ,
  drop = FALSE
]

message("")
message("Robust CATs Phase 2A validation:")
print(checks, row.names = FALSE)
message("")
message(paste(
  "Required checks passed:",
  sum(
    checks$required %in% TRUE &
      checks$passed %in% TRUE
  ),
  "of",
  sum(checks$required %in% TRUE)
))
message(paste(
  "Results saved to:",
  output_dir
))

if (nrow(required_failures) > 0L) {
  stop(
    paste(
      nrow(required_failures),
      "required audit check(s) failed.",
      "Do not modify or freeze production CATs code yet."
    ),
    call. = FALSE
  )
}

message("")
message(
  "All required Phase 2A checks passed."
)
