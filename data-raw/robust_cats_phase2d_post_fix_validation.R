# Robust CATs audit: Phase 2D post-fix validation
#
# Run from the mmiCATs project root after replacing the Phase 2D files and
# running devtools::document().

library(devtools)

load_all()

source(
  "data-raw/robust_cats_audit_helpers.R"
)

message("")
message("1. Running focused Phase 2D regression tests...")
test(
  filter = "robust-cats-failure-retention",
  stop_on_failure = TRUE
)

message("")
message("2. Verifying ordinary CATs remains unchanged...")
dat <- rca_make_validation_data(
  seed = 20261220L,
  n_clusters = 6L,
  cluster_size = 20L
)

ordinary_oracle <- rca_oracle(
  dat = dat,
  engine = "glm",
  alpha = 0.05,
  truncation_rule = "none",
  consume_template = FALSE
)

ordinary_package <- rca_package_cats(
  dat = dat,
  alpha = 0.05,
  truncate = FALSE,
  retained_clusters =
    ordinary_oracle$aggregate$retained_clusters
)

ordinary_comparison <- rca_compare_results(
  reference = ordinary_oracle$aggregate,
  observed = ordinary_package,
  comparison = "Phase 2D ordinary CATs preservation",
  tolerance = 1e-10
)

print(ordinary_comparison, row.names = FALSE)

if (!all(ordinary_comparison$passed)) {
  stop(
    "Ordinary CATs no longer matches the independent oracle.",
    call. = FALSE
  )
}

message("")
message("3. Verifying robust CATs normal-case results...")

for (engine in c("robust", "robustbase")) {
  seed <- if (engine == "robust") {
    20261221L
  } else {
    20261222L
  }

  set.seed(seed)
  oracle <- rca_oracle(
    dat = dat,
    engine = engine,
    alpha = 0.05,
    truncation_rule = "none",
    consume_template = TRUE
  )

  simulation <- rca_simulation_robust_cats(
    dat = dat,
    engine = engine,
    seed = seed,
    alpha = 0.05
  )

  comparison <- rca_compare_results(
    reference = oracle$aggregate,
    observed = simulation,
    comparison = paste(
      "Phase 2D robust CATs preservation:",
      engine
    ),
    tolerance = 1e-8
  )

  print(comparison, row.names = FALSE)

  if (!all(comparison$passed)) {
    stop(
      paste(
        "Robust CATs no longer matches the independent oracle:",
        engine
      ),
      call. = FALSE
    )
  }
}

message("")
message("4. Verifying public robust CATs drops a constant-x cluster...")

set.seed(20261223L)

rank_deficient_data <- local({
  cluster_size <- 16L
  cluster <- factor(
    rep(1:4, each = cluster_size),
    levels = 1:4
  )
  x <- unlist(
    lapply(
      1:4,
      function(cluster_id) {
        if (cluster_id == 1L) {
          rep(0, cluster_size)
        } else {
          seq(
            -1.5,
            1.5,
            length.out = cluster_size
          )
        }
      }
    ),
    use.names = FALSE
  )
  cluster_intercept <- rep(
    c(-0.30, -0.10, 0.10, 0.30),
    each = cluster_size
  )

  data.frame(
    cluster = cluster,
    x = x,
    out = 0.25 +
      cluster_intercept +
      0.40 * x +
      stats::rnorm(
        4L * cluster_size,
        sd = 0.15
      )
  )
})

for (engine in c("robust", "robustbase")) {
  seed <- if (engine == "robust") {
    20261224L
  } else {
    20261225L
  }

  set.seed(seed)
  oracle <- rca_oracle(
    dat = rank_deficient_data,
    engine = engine,
    alpha = 0.05,
    truncation_rule = "none",
    consume_template = TRUE
  )

  public <- rca_public_robust_cats(
    dat = rank_deficient_data,
    engine = engine,
    seed = seed,
    alpha = 0.05,
    retained_clusters =
      oracle$aggregate$retained_clusters
  )

  comparison <- rca_compare_results(
    reference = oracle$aggregate,
    observed = public,
    comparison = paste(
      "Phase 2D constant-x public path:",
      engine
    ),
    tolerance = 1e-8
  )

  print(comparison, row.names = FALSE)

  if (oracle$aggregate$retained_clusters != 3L ||
      !all(comparison$passed)) {
    stop(
      paste(
        "The public robust CATs path did not correctly drop",
        "the constant-x cluster:",
        engine
      ),
      call. = FALSE
    )
  }
}

message("")
message("5. Rerunning the Phase 2C adversarial audit...")

phase2c_error <- tryCatch(
  {
    source(
      "data-raw/robust_cats_failure_retention_audit.R"
    )
    NULL
  },
  error = function(e) e
)

phase2c_output_dir <- file.path(
  rca_find_project_root(),
  "data-raw",
  "robust-cats-audit-results",
  "phase2c-failure-retention"
)

phase2c_checks <- utils::read.csv(
  file.path(
    phase2c_output_dir,
    "robust_cats_failure_retention_checks.csv"
  ),
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

failed_readiness <- phase2c_checks[
  phase2c_checks$readiness_required %in% TRUE &
    !(phase2c_checks$passed %in% TRUE),
  ,
  drop = FALSE
]

expected_upstream_check <-
  "clusterSEs_drop_true_drops_omitted_coefficient_cluster"

unexpected_failures <- failed_readiness[
  failed_readiness$check !=
    expected_upstream_check,
  ,
  drop = FALSE
]

if (nrow(unexpected_failures) > 0L) {
  print(unexpected_failures, row.names = FALSE)
  stop(
    paste(
      nrow(unexpected_failures),
      "unexpected Phase 2C readiness check(s) failed."
    ),
    call. = FALSE
  )
}

if (nrow(failed_readiness) == 1L &&
    identical(
      failed_readiness$check,
      expected_upstream_check
    )) {
  message(
    paste(
      "Phase 2C reproduced only the expected upstream",
      "clusterSEs omitted-coefficient limitation."
    )
  )
} else if (nrow(failed_readiness) == 0L) {
  message(
    paste(
      "Phase 2C reported no readiness failures;",
      "the installed clusterSEs behavior may have changed."
    )
  )
}

if (!is.null(phase2c_error)) {
  message(
    paste(
      "The Phase 2C script stopped after saving results, as expected:",
      conditionMessage(phase2c_error)
    )
  )
}

message("")
message("All Phase 2D targeted validations passed.")
