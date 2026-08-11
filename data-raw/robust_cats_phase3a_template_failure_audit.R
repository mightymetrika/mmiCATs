# Robust CATs audit: Phase 3A adversarial full-data template failure
#
# Purpose:
#   Investigate A-05 without changing production code.
#
#   study1_fit_robust_cats() currently fits an unused full-data robust model
#   before the cluster-specific models. The fit is retained to preserve the
#   historical robust-CATs random-number sequence, but a failure of this
#   unused template currently aborts the method.
#
#   This audit:
#     1. structurally tests whether a template-only failure can block otherwise
#        viable cluster-specific CATs inference;
#     2. records whether the real robust engines consume RNG in the template fit;
#     3. spot-scans frozen Study 1 DGP conditions for naturally occurring
#        template-failure / cluster-viability combinations.
#
# This script does not modify production source files. One package-namespace
# binding is temporarily replaced for the structural test and restored
# immediately, even if the test errors.

project_root <- rca_find_project_root()
rca_require_packages()

pkgload::load_all(
  project_root,
  quiet = TRUE,
  export_all = TRUE
)

output_dir <- file.path(
  project_root,
  "data-raw",
  "robust-cats-audit-results",
  "phase3a-template-failure"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

phase3_with_namespace_binding <- function(name,
                                          replacement,
                                          code) {
  namespace <- asNamespace("mmiCATs")

  if (!exists(name, envir = namespace, inherits = FALSE)) {
    stop(
      paste("Could not find package binding:", name),
      call. = FALSE
    )
  }

  original <- get(
    name,
    envir = namespace,
    inherits = FALSE
  )
  was_locked <- bindingIsLocked(
    name,
    namespace
  )

  replace_binding <- function(value) {
    if (bindingIsLocked(name, namespace)) {
      unlockBinding(name, namespace)
    }

    assign(
      name,
      value,
      envir = namespace
    )

    if (was_locked) {
      lockBinding(name, namespace)
    }
  }

  replace_binding(replacement)

  on.exit(
    replace_binding(original),
    add = TRUE
  )

  force(code)
}

make_phase3a_data <- function(seed = 20263001L,
                              n_clusters = 6L,
                              cluster_size = 30L) {
  set.seed(seed)

  study1_simulate_data(
    n_clusters = n_clusters,
    cluster_size = cluster_size,
    beta = 0.10,
    intercept = 0,
    random_intercept_sd = 1,
    residual_sd = 1,
    x_sd = 1,
    contamination = "none",
    contamination_prop = 0.05,
    contamination_size = 6,
    leverage_size = 4
  )
}

# -------------------------------------------------------------------------
# 1. Structural A-05 reproduction
# -------------------------------------------------------------------------

structural_dat <- make_phase3a_data()

synthetic_fit <- function(formula,
                          data,
                          engine) {
  n_data_clusters <- length(
    unique(
      as.character(data$cluster)
    )
  )

  if (n_data_clusters > 1L) {
    stop(
      "Synthetic full-data template failure.",
      call. = FALSE
    )
  }

  stats::lm(
    formula = formula,
    data = data
  )
}

structural_rows <- lapply(
  c("robust", "robustbase"),
  function(engine) {
    structural_result <- phase3_with_namespace_binding(
      name = "study1_fit_robust_model",
      replacement = synthetic_fit,
      code = {
        cluster_ids <- unique(
          as.character(
            structural_dat$cluster
          )
        )

        cluster_results <- do.call(
          rbind,
          lapply(
            cluster_ids,
            function(cluster_id) {
              study1_fit_robust_cluster(
                cluster_id = cluster_id,
                dat = structural_dat,
                formula = out ~ x,
                engine = engine
              )
            }
          )
        )

        production_result <- rca_capture(
          study1_fit_robust_cats(
            dat = structural_dat,
            alpha = 0.05,
            engine = engine
          )
        )

        list(
          cluster_results = cluster_results,
          production_result = production_result
        )
      }
    )

    cluster_results <- structural_result$cluster_results
    production_result <- structural_result$production_result

    retained_clusters <- sum(
      cluster_results$retained %in% TRUE
    )

    data.frame(
      engine = engine,
      total_clusters = nrow(cluster_results),
      retained_cluster_fits = retained_clusters,
      cluster_path_viable = retained_clusters >= 2L,
      production_returned_value =
        !is.null(production_result$value),
      production_error =
        production_result$error,
      template_failure_blocked_viable_inference =
        retained_clusters >= 2L &&
        is.null(production_result$value) &&
        rca_has_text(production_result$error) &&
        grepl(
          "Synthetic full-data template failure",
          production_result$error,
          fixed = TRUE
        ),
      stringsAsFactors = FALSE
    )
  }
)

structural_results <- do.call(
  rbind,
  structural_rows
)
rownames(structural_results) <- NULL

# -------------------------------------------------------------------------
# 2. Does the real template fit consume RNG?
# -------------------------------------------------------------------------

rng_dat <- make_phase3a_data(
  seed = 20263002L
)

rng_rows <- lapply(
  c("robust", "robustbase"),
  function(engine) {
    set.seed(20263003L)

    seed_before <- .Random.seed

    captured <- study1_capture_fit(function() {
      study1_fit_robust_model(
        formula = out ~ x,
        data = rng_dat,
        engine = engine
      )
    })

    seed_after <- .Random.seed

    data.frame(
      engine = engine,
      template_fit_success =
        !is.null(captured$value),
      template_warning = captured$warning,
      template_error = captured$error,
      rng_state_changed =
        !identical(seed_before, seed_after),
      stringsAsFactors = FALSE
    )
  }
)

rng_results <- do.call(
  rbind,
  rng_rows
)
rownames(rng_results) <- NULL

# -------------------------------------------------------------------------
# 3. Frozen-DGP spot scan using the real robust engines
# -------------------------------------------------------------------------

contamination_specs <- data.frame(
  contamination = c(
    "none",
    "vertical",
    "bad_leverage"
  ),
  contamination_size = c(
    6,
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

scan_design <- expand.grid(
  engine = c(
    "robust",
    "robustbase"
  ),
  beta = c(
    0,
    0.10
  ),
  contamination_index =
    seq_len(
      nrow(contamination_specs)
    ),
  dataset_id = seq_len(3L),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

scan_rows <- lapply(
  seq_len(nrow(scan_design)),
  function(index) {
    design_row <- scan_design[index, , drop = FALSE]
    specification <- contamination_specs[
      design_row$contamination_index,
      ,
      drop = FALSE
    ]

    data_seed <- as.integer(
      20263100L + index
    )
    method_seed <- as.integer(
      20264100L + index
    )

    set.seed(data_seed)

    dat <- study1_simulate_data(
      n_clusters = 10L,
      cluster_size = 40L,
      beta = design_row$beta,
      intercept = 0,
      random_intercept_sd = 1,
      residual_sd = 1,
      x_sd = 1,
      contamination =
        specification$contamination,
      contamination_prop = 0.05,
      contamination_size =
        specification$contamination_size,
      leverage_size =
        specification$leverage_size
    )

    set.seed(method_seed)

    template_fit <- study1_capture_fit(function() {
      study1_fit_robust_model(
        formula = out ~ x,
        data = dat,
        engine = design_row$engine
      )
    })

    cluster_results <- do.call(
      rbind,
      lapply(
        unique(as.character(dat$cluster)),
        function(cluster_id) {
          study1_fit_robust_cluster(
            cluster_id = cluster_id,
            dat = dat,
            formula = out ~ x,
            engine = design_row$engine
          )
        }
      )
    )

    retained_clusters <- sum(
      cluster_results$retained %in% TRUE
    )

    data.frame(
      engine = design_row$engine,
      beta = design_row$beta,
      contamination =
        specification$contamination,
      contamination_size =
        specification$contamination_size,
      leverage_size =
        specification$leverage_size,
      dataset_id = design_row$dataset_id,
      data_seed = data_seed,
      method_seed = method_seed,
      template_success =
        !is.null(template_fit$value),
      template_warning =
        template_fit$warning,
      template_error =
        template_fit$error,
      retained_cluster_fits =
        retained_clusters,
      cluster_path_viable =
        retained_clusters >= 2L,
      natural_a05_case =
        is.null(template_fit$value) &&
        retained_clusters >= 2L,
      stringsAsFactors = FALSE
    )
  }
)

scan_results <- do.call(
  rbind,
  scan_rows
)
rownames(scan_results) <- NULL

# -------------------------------------------------------------------------
# 4. Checks and disposition
# -------------------------------------------------------------------------

checks <- data.frame(
  check = c(
    "synthetic_cluster_paths_are_viable",
    "synthetic_template_failure_blocks_viable_inference",
    "real_template_fits_complete_in_spot_scan"
  ),
  passed = c(
    all(
      structural_results$
        cluster_path_viable
    ),
    all(
      structural_results$
        template_failure_blocked_viable_inference
    ),
    all(
      scan_results$template_success
    )
  ),
  required_for_interpretation = c(
    TRUE,
    TRUE,
    FALSE
  ),
  details = c(
    paste(
      structural_results$engine,
      structural_results$retained_cluster_fits,
      sep = "=",
      collapse = "; "
    ),
    paste(
      structural_results$engine,
      structural_results$
        template_failure_blocked_viable_inference,
      sep = "=",
      collapse = "; "
    ),
    paste(
      sum(scan_results$template_success),
      "of",
      nrow(scan_results),
      "real-engine template fits succeeded;",
      sum(scan_results$natural_a05_case),
      "natural A-05 cases observed."
    )
  ),
  stringsAsFactors = FALSE
)

a05_reproduced <- all(
  structural_results$
    template_failure_blocked_viable_inference
)

issue_summary <- data.frame(
  issue_id = "A-05",
  issue = paste(
    "An unused full-data robust template fit can make",
    "study1_fit_robust_cats() fail before otherwise viable",
    "cluster-specific inference is attempted."
  ),
  structurally_reproduced = a05_reproduced,
  natural_cases_in_frozen_dgp_spot_scan =
    sum(scan_results$natural_a05_case),
  disposition = if (a05_reproduced) {
    paste(
      "Reproduced structurally.",
      "Do not change production code until the minimum",
      "RNG-preserving correction is specified and tested."
    )
  } else {
    paste(
      "Not reproduced structurally.",
      "Review the audit harness before changing production code."
    )
  },
  stringsAsFactors = FALSE
)

source_files <- c(
  phase3a_audit =
    file.path(
      project_root,
      "data-raw",
      "robust_cats_phase3a_template_failure_audit.R"
    ),
  audit_helpers =
    file.path(
      project_root,
      "data-raw",
      "robust_cats_audit_helpers.R"
    ),
  study1_helpers =
    file.path(
      project_root,
      "R",
      "pwr_func_study1_helpers.R"
    )
)

source_checksums <- rca_source_checksums(
  source_files
)

results <- list(
  checks = checks,
  structural_results =
    structural_results,
  rng_results = rng_results,
  scan_results = scan_results,
  issue_summary = issue_summary,
  source_checksums = source_checksums
)

rca_write_csv_atomic(
  checks,
  file.path(
    output_dir,
    "phase3a_checks.csv"
  )
)

rca_write_csv_atomic(
  structural_results,
  file.path(
    output_dir,
    "phase3a_structural_results.csv"
  )
)

rca_write_csv_atomic(
  rng_results,
  file.path(
    output_dir,
    "phase3a_rng_results.csv"
  )
)

rca_write_csv_atomic(
  scan_results,
  file.path(
    output_dir,
    "phase3a_frozen_dgp_scan.csv"
  )
)

rca_write_csv_atomic(
  issue_summary,
  file.path(
    output_dir,
    "phase3a_issue_summary.csv"
  )
)

rca_write_csv_atomic(
  source_checksums,
  file.path(
    output_dir,
    "phase3a_source_checksums.csv"
  )
)

rca_save_rds_atomic(
  results,
  file.path(
    output_dir,
    "phase3a_results.rds"
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
message("Phase 3A checks:")
print(
  checks,
  row.names = FALSE
)

message("")
message("Phase 3A structural A-05 results:")
print(
  structural_results,
  row.names = FALSE
)

message("")
message("Phase 3A real-engine RNG behavior:")
print(
  rng_results,
  row.names = FALSE
)

message("")
message("Phase 3A issue summary:")
print(
  issue_summary,
  row.names = FALSE
)

message("")
message(paste(
  "Natural A-05 cases in frozen-DGP spot scan:",
  sum(scan_results$natural_a05_case),
  "of",
  nrow(scan_results)
))

message(paste(
  "Results saved to:",
  output_dir
))

required_failures <- checks[
  checks$required_for_interpretation %in% TRUE &
    !(checks$passed %in% TRUE),
  ,
  drop = FALSE
]

if (nrow(required_failures) > 0L) {
  stop(
    paste(
      nrow(required_failures),
      "required Phase 3A audit precondition(s) failed.",
      "Review the harness before changing production code."
    ),
    call. = FALSE
  )
}

message("")
message(
  "Phase 3A adversarial audit completed."
)
