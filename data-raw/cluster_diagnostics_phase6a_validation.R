# Phase 6A: package diagnostic-function validation
#
# Validates the planned pre-model and post-fit clustered-data diagnostic tools
# before Study 3 is specified and run. This script does not generate Study 3
# results and does not alter the definitive simulation design.

library(devtools)

load_all()

project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

output_dir <- file.path(
  project_root,
  "data-raw",
  "definitive-runner-results",
  "phase6a-cluster-diagnostics"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

checks <- list()

add_check <- function(check,
                      passed,
                      details = NA_character_) {
  checks[[length(checks) + 1L]] <<-
    data.frame(
      check = check,
      passed = as.logical(passed),
      details = details,
      stringsAsFactors = FALSE
    )
}

message(
  "Phase 6A: running focused diagnostic-function tests..."
)

testthat::test_file(
  file.path(
    project_root,
    "tests",
    "testthat",
    "test-cluster-diagnostics.R"
  ),
  reporter = "progress",
  stop_on_failure = TRUE,
  stop_on_warning = FALSE
)

message(
  "Phase 6A: exercising pre-fit diagnostics on sleepstudy without fitting models..."
)

sleepstudy <- lme4::sleepstudy

pre <- cluster_data_explore(
  Reaction ~ Days,
  ~ Subject,
  sleepstudy
)

add_check(
  "sleepstudy_pre_model_cluster_count",
  identical(
    as.integer(
      pre$overall$clusters
    ),
    18L
  ),
  paste(
    "Observed",
    pre$overall$clusters
  )
)

add_check(
  "sleepstudy_pre_model_observation_count",
  identical(
    as.integer(
      pre$overall$observations
    ),
    180L
  ),
  paste(
    "Observed",
    pre$overall$observations
  )
)

add_check(
  "sleepstudy_all_clusters_have_estimable_days_slope",
  all(
    pre$cluster_summary$
      estimable_slope
  )
)

add_check(
  "pre_model_outputs_are_structured_not_recommendations",
  !("recommendation" %in% names(pre))
)

add_check(
  "pre_model_plots_are_ggplot_objects",
  all(
    vapply(
      pre$plots,
      inherits,
      logical(1),
      what = "ggplot"
    )
  ),
  paste(
    "Plot count:",
    length(pre$plots)
  )
)

message(
  "Phase 6A: exercising post-fit diagnostics on a small deterministic dataset..."
)

set.seed(20261102L)

cluster <- factor(
  rep(
    seq_len(8L),
    each = 20L
  )
)

x <- stats::rnorm(
  length(cluster)
)

u <- stats::rnorm(
  nlevels(cluster),
  sd = 0.5
)

validation_data <- data.frame(
  out = 0.15 * x +
    u[as.integer(cluster)] +
    stats::rnorm(
      length(cluster)
    ),
  x = x,
  cluster = cluster
)

post <- cluster_model_diagnostics(
  out ~ x,
  ~ cluster,
  validation_data,
  methods = c(
    "ri",
    "cr2",
    "cats",
    "cats_robust",
    "cats_robustbase"
  ),
  alpha = 0.05,
  seed = 20261103L,
  leave_one_cluster_out = FALSE
)

add_check(
  "post_fit_requested_method_rows_returned",
  identical(
    post$comparison$method,
    c(
      "ri",
      "cr2",
      "cats",
      "cats_robust",
      "cats_robustbase"
    )
  )
)

add_check(
  "post_fit_method_rows_have_complete_identity",
  all(
    nzchar(
      post$comparison$
        method_label
    )
  )
)

add_check(
  "post_fit_cluster_fits_cover_all_clusters",
  identical(
    sort(
      unique(
        as.character(
          post$cluster_fits$cluster
        )
      )
    ),
    sort(
      levels(cluster)
    )
  )
)

add_check(
  "post_fit_observation_diagnostics_cover_all_rows",
  identical(
    nrow(
      post$observation_diagnostics
    ),
    nrow(validation_data)
  )
)

add_check(
  "post_fit_outputs_are_structured_not_recommendations",
  !("recommendation" %in% names(post))
)

add_check(
  "post_fit_core_plots_are_ggplot_objects",
  all(
    vapply(
      post$plots,
      inherits,
      logical(1),
      what = "ggplot"
    )
  ),
  paste(
    "Plot count:",
    length(post$plots)
  )
)

message(
  "Phase 6A: verifying optional leave-one-cluster-out path..."
)

loo <- cluster_model_diagnostics(
  out ~ x,
  ~ cluster,
  validation_data,
  methods = c(
    "cr2",
    "cats"
  ),
  alpha = 0.05,
  seed = 20261104L,
  leave_one_cluster_out = TRUE
)

add_check(
  "loo_has_method_by_cluster_rows",
  identical(
    nrow(loo$influence),
    2L * nlevels(cluster)
  ),
  paste(
    "Observed rows:",
    nrow(loo$influence)
  )
)

add_check(
  "loo_plot_created",
  "leave_one_cluster_out" %in%
    names(loo$plots) &&
    inherits(
      loo$plots$
        leave_one_cluster_out,
      "ggplot"
    )
)

checks_df <- do.call(
  rbind,
  checks
)
rownames(checks_df) <- NULL

source_files <- c(
  diagnostic_functions = file.path(
    project_root,
    "R",
    "cluster_diagnostics.R"
  ),
  description = file.path(
    project_root,
    "DESCRIPTION"
  ),
  tests = file.path(
    project_root,
    "tests",
    "testthat",
    "test-cluster-diagnostics.R"
  ),
  validator = file.path(
    project_root,
    "data-raw",
    "cluster_diagnostics_phase6a_validation.R"
  )
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
  "ggplot2",
  "clusterSEs",
  "clubSandwich",
  "robust",
  "robustbase",
  "lme4",
  "lmerTest",
  "pbkrtest",
  "robustlmm",
  "testthat"
)

package_versions <- data.frame(
  package = package_names,
  version = vapply(
    package_names,
    function(package_name) {
      if (!requireNamespace(
        package_name,
        quietly = TRUE
      )) {
        return(
          NA_character_
        )
      }

      as.character(
        utils::packageVersion(
          package_name
        )
      )
    },
    FUN.VALUE = character(1)
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  checks_df,
  file.path(
    output_dir,
    "phase6a_checks.csv"
  ),
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  pre$overall,
  file.path(
    output_dir,
    "sleepstudy_pre_model_overall.csv"
  ),
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  pre$cluster_summary,
  file.path(
    output_dir,
    "sleepstudy_pre_model_cluster_summary.csv"
  ),
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  post$comparison,
  file.path(
    output_dir,
    "phase6a_post_fit_comparison.csv"
  ),
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  source_checksums,
  file.path(
    output_dir,
    "phase6a_source_checksums.csv"
  ),
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  package_versions,
  file.path(
    output_dir,
    "phase6a_package_versions.csv"
  ),
  row.names = FALSE,
  na = ""
)

saveRDS(
  list(
    checks = checks_df,
    sleepstudy_pre_model = list(
      overall = pre$overall,
      cluster_summary =
        pre$cluster_summary,
      missingness =
        pre$missingness
    ),
    post_fit_comparison =
      post$comparison,
    source_checksums =
      source_checksums,
    package_versions =
      package_versions,
    session_info =
      utils::sessionInfo()
  ),
  file.path(
    output_dir,
    "phase6a_results.rds"
  ),
  version = 3
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

summary_lines <- c(
  "mmiCATs Phase 6A",
  "Pre-model and post-fit clustered-data diagnostic functions",
  "",
  paste(
    "Checks passed:",
    sum(checks_df$passed),
    "of",
    nrow(checks_df)
  ),
  paste(
    "sleepstudy clusters:",
    pre$overall$clusters
  ),
  paste(
    "sleepstudy observations:",
    pre$overall$observations
  ),
  paste(
    "Pre-model plot count:",
    length(pre$plots)
  ),
  paste(
    "Post-fit plot count:",
    length(post$plots)
  ),
  paste(
    "Leave-one-cluster-out rows:",
    nrow(loo$influence)
  )
)

writeLines(
  summary_lines,
  con = file.path(
    output_dir,
    "phase6a_summary.txt"
  ),
  useBytes = TRUE
)

message("")
message("Phase 6A checks:")
print(
  checks_df,
  row.names = FALSE
)

failed <- checks_df[
  !(checks_df$passed %in% TRUE),
  ,
  drop = FALSE
]

if (nrow(failed) > 0L) {
  stop(
    paste(
      nrow(failed),
      "Phase 6A diagnostic-function validation check(s) failed.",
      "Do not proceed to Study 3 yet."
    ),
    call. = FALSE
  )
}

message("")
message(
  paste(
    "All Phase 6A clustered-data diagnostic checks passed.",
    "The package diagnostic layer is ready for Study 3 planning."
  )
)
