# Study 3 definitive empirical comparison
#
# Consumes ONLY the frozen Phase 6B canonical and perturbed inputs.
# Completed checkpoints are reused; the contamination map is never regenerated.

library(devtools)

load_all()

source("data-raw/definitive_sharding_helpers.R")
source("data-raw/study3_analysis_helpers.R")

project_root <- study3c_find_project_root()
freeze <- study3c_verify_freeze(project_root)
freeze_dir <- freeze$freeze_dir

definitive_dir <- file.path(
  project_root, "data-raw", "study3-results", "definitive-study3"
)
checkpoint_dir <- file.path(definitive_dir, "checkpoints")
plot_dir <- file.path(definitive_dir, "plots")
completion_marker <- file.path(definitive_dir, "STUDY3_COMPLETE.txt")
checksum_path <- file.path(definitive_dir, "study3_output_checksums.csv")

if (file.exists(completion_marker)) {
  if (!file.exists(checksum_path)) {
    stop(
      "Study 3 completion marker exists but output checksums are missing.",
      call. = FALSE
    )
  }

  x <- utils::read.csv(
    checksum_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  paths <- file.path(project_root, x$relative_path)

  if (!all(file.exists(paths))) {
    stop(
      "Completed Study 3 output is missing: ",
      paste(x$relative_path[!file.exists(paths)], collapse = ", "),
      call. = FALSE
    )
  }

  current <- unname(tools::md5sum(paths))
  if (!identical(current, x$md5)) {
    stop(
      "Completed Study 3 output checksum mismatch: ",
      paste(x$relative_path[current != x$md5], collapse = ", "),
      call. = FALSE
    )
  }

  message(
    "Completed Study 3 definitive analysis verified. No results were regenerated."
  )

} else {

  dir.create(definitive_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

  methods <- study3c_methods()
  analysis_seed <- study3c_analysis_seed()

  canonical_path <- file.path(freeze_dir, "sleepstudy_canonical.rds")
  perturbed_path <- file.path(freeze_dir, "sleepstudy_perturbed.rds")
  map_path <- file.path(freeze_dir, "study3_contamination_map.rds")

  canonical <- readRDS(canonical_path)
  perturbed <- readRDS(perturbed_path)
  contamination_map <- readRDS(map_path)

  canonical_md5 <- unname(tools::md5sum(canonical_path))
  perturbed_md5 <- unname(tools::md5sum(perturbed_path))

  run_full_checkpoint <- function(
      data,
      dataset,
      input_md5,
      path) {

    if (file.exists(path)) {
      x <- readRDS(path)
      ok <- is.list(x) &&
        identical(x$status, "complete") &&
        identical(x$dataset, dataset) &&
        identical(x$input_md5, input_md5) &&
        identical(x$methods, methods) &&
        identical(as.integer(x$seed), analysis_seed)

      if (!ok) {
        stop(
          "Existing full-analysis checkpoint does not match frozen request: ",
          path,
          call. = FALSE
        )
      }

      message("Reusing ", dataset, " full-analysis checkpoint.")
      return(x$result)
    }

    message("Study 3: fitting ", dataset, " nine-method comparison...")
    started <- Sys.time()

    result <- study3c_fit_full(
      data = data,
      methods = methods,
      seed = analysis_seed
    )

    completed <- Sys.time()

    checkpoint <- list(
      status = "complete",
      dataset = dataset,
      input_md5 = input_md5,
      methods = methods,
      seed = analysis_seed,
      result = result,
      started_at = started,
      completed_at = completed,
      elapsed_sec = as.numeric(difftime(completed, started, units = "secs"))
    )

    definitive_save_rds_atomic(checkpoint, path)
    result
  }

  observed <- run_full_checkpoint(
    canonical,
    "observed",
    canonical_md5,
    file.path(checkpoint_dir, "observed_full.rds")
  )

  perturbed_result <- run_full_checkpoint(
    perturbed,
    "perturbed",
    perturbed_md5,
    file.path(checkpoint_dir, "perturbed_full.rds")
  )

  observed_dat <- observed$analysis_data
  perturbed_dat <- perturbed_result$analysis_data

  observed_loo_dir <- file.path(checkpoint_dir, "loo-observed")
  perturbed_loo_dir <- file.path(checkpoint_dir, "loo-perturbed")
  dir.create(observed_loo_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(perturbed_loo_dir, recursive = TRUE, showWarnings = FALSE)

  message("Study 3: observed leave-one-Subject-out checkpoints...")
  for (i in seq_along(levels(observed_dat$cluster))) {
    z <- study3c_run_loo_checkpoint(
      dat = observed_dat,
      methods = methods,
      seed = analysis_seed,
      full_comparison = observed$comparison,
      cluster_index = i,
      dataset = "observed",
      input_md5 = canonical_md5,
      checkpoint_dir = observed_loo_dir
    )
    message(
      sprintf(
        "  Observed Subject %s: %s",
        levels(observed_dat$cluster)[i],
        z$action
      )
    )
  }

  message("Study 3: perturbed leave-one-Subject-out checkpoints...")
  for (i in seq_along(levels(perturbed_dat$cluster))) {
    z <- study3c_run_loo_checkpoint(
      dat = perturbed_dat,
      methods = methods,
      seed = analysis_seed,
      full_comparison = perturbed_result$comparison,
      cluster_index = i,
      dataset = "perturbed",
      input_md5 = perturbed_md5,
      checkpoint_dir = perturbed_loo_dir
    )
    message(
      sprintf(
        "  Perturbed Subject %s: %s",
        levels(perturbed_dat$cluster)[i],
        z$action
      )
    )
  }

  observed_loo <- study3c_collect_loo(
    observed_dat, methods, analysis_seed, "observed",
    canonical_md5, observed_loo_dir
  )

  perturbed_loo <- study3c_collect_loo(
    perturbed_dat, methods, analysis_seed, "perturbed",
    perturbed_md5, perturbed_loo_dir
  )

  definitive_write_csv_atomic(
    observed_loo$status,
    file.path(definitive_dir, "study3_observed_loo_status.csv")
  )
  definitive_write_csv_atomic(
    perturbed_loo$status,
    file.path(definitive_dir, "study3_perturbed_loo_status.csv")
  )

  if (!observed_loo$complete || !perturbed_loo$complete) {
    stop(
      paste(
        "Study 3 LOO checkpoints are incomplete.",
        "Rerun to retry matching error checkpoints."
      ),
      call. = FALSE
    )
  }

  observed_loo_results <- observed_loo$results
  observed_loo_results$dataset <- "Observed"

  perturbed_loo_results <- perturbed_loo$results
  perturbed_loo_results$dataset <- "Perturbed"

  loo_results <- rbind(observed_loo_results, perturbed_loo_results)
  rownames(loo_results) <- NULL

  comparison <- study3c_make_comparison(
    observed$comparison,
    perturbed_result$comparison
  )

  contamination_columns <- data.frame(
    row_id = seq_len(nrow(perturbed)),
    Subject = as.character(perturbed$Subject),
    Days = perturbed$Days,
    Reaction_observed = perturbed$Reaction_observed,
    Reaction_perturbed = perturbed$Reaction,
    contaminated = perturbed$contaminated,
    contamination_sign = perturbed$contamination_sign,
    signed_displacement = perturbed$signed_displacement,
    absolute_displacement = perturbed$absolute_displacement,
    stringsAsFactors = FALSE
  )

  contaminated_diagnostics <- merge(
    contamination_columns,
    perturbed_result$observation_diagnostics,
    by = "row_id",
    all.x = TRUE,
    sort = FALSE
  )
  contaminated_diagnostics <- contaminated_diagnostics[
    contaminated_diagnostics$contaminated %in% TRUE,
    ,
    drop = FALSE
  ]
  contaminated_diagnostics <- contaminated_diagnostics[
    order(contaminated_diagnostics$Subject),
    ,
    drop = FALSE
  ]
  rownames(contaminated_diagnostics) <- NULL

  tables <- list(
    study3_observed_method_comparison = observed$comparison,
    study3_perturbed_method_comparison = perturbed_result$comparison,
    study3_observed_vs_perturbed = comparison,
    study3_observed_cluster_fits = observed$cluster_fits,
    study3_perturbed_cluster_fits = perturbed_result$cluster_fits,
    study3_observed_cluster_slope_differences =
      observed$cluster_slope_differences,
    study3_perturbed_cluster_slope_differences =
      perturbed_result$cluster_slope_differences,
    study3_observed_observation_diagnostics =
      observed$observation_diagnostics,
    study3_perturbed_observation_diagnostics =
      perturbed_result$observation_diagnostics,
    study3_contaminated_observation_diagnostics =
      contaminated_diagnostics,
    study3_leave_one_subject_out = loo_results,
    study3_observed_loo_status = observed_loo$status,
    study3_perturbed_loo_status = perturbed_loo$status
  )

  table_paths <- vapply(
    names(tables),
    function(nm) file.path(definitive_dir, paste0(nm, ".csv")),
    FUN.VALUE = character(1)
  )

  for (nm in names(tables)) {
    definitive_write_csv_atomic(tables[[nm]], table_paths[[nm]])
  }

  # Preserve the full diagnostic objects (including their ggplot objects).
  result_rds_path <- file.path(
    definitive_dir, "study3_definitive_results.rds"
  )

  results_record <- list(
    methods = methods,
    analysis_seed = analysis_seed,
    observed = observed,
    perturbed = perturbed_result,
    observed_vs_perturbed = comparison,
    contamination_map = contamination_map,
    contaminated_observation_diagnostics = contaminated_diagnostics,
    leave_one_subject_out = loo_results,
    observed_loo_status = observed_loo$status,
    perturbed_loo_status = perturbed_loo$status,
    phase6b_freeze_record = readRDS(
      file.path(freeze_dir, "study3_freeze_record.rds")
    ),
    session_info = utils::sessionInfo()
  )

  definitive_save_rds_atomic(results_record, result_rds_path)

  # Save the six standard diagnostic plots for each dataset.
  plot_sizes <- list(
    method_comparison = c(8, 5.5),
    cluster_slopes = c(8, 7),
    slope_differences = c(8, 7),
    residual_leverage = c(7, 5.5),
    robust_weights = c(8, 5.5),
    cluster_fits = c(10, 11)
  )

  plot_paths <- character(0)

  save_set <- function(diagnostic, prefix) {
    paths <- character(0)
    for (nm in intersect(names(plot_sizes), names(diagnostic$plots))) {
      dims <- plot_sizes[[nm]]
      path <- file.path(plot_dir, paste0(prefix, "_", nm, ".pdf"))
      study3c_save_plot(
        diagnostic$plots[[nm]], path,
        width = dims[1L], height = dims[2L]
      )
      paths <- c(paths, path)
    }
    paths
  }

  plot_paths <- c(
    plot_paths,
    save_set(observed, "study3_observed"),
    save_set(perturbed_result, "study3_perturbed")
  )

  cross_path <- file.path(
    plot_dir, "study3_observed_vs_perturbed.pdf"
  )
  study3c_save_plot(
    study3c_cross_dataset_plot(comparison),
    cross_path,
    width = 8.5,
    height = 6
  )
  plot_paths <- c(plot_paths, cross_path)

  loo_plot_path <- file.path(
    plot_dir, "study3_leave_one_subject_out.pdf"
  )
  study3c_save_plot(
    study3c_loo_plot(loo_results),
    loo_plot_path,
    width = 18,
    height = 9
  )
  plot_paths <- c(plot_paths, loo_plot_path)

  metadata <- data.frame(
    field = c(
      "analysis_seed",
      "method_count",
      "methods",
      "alpha",
      "observed_input_md5",
      "perturbed_input_md5",
      "observed_subjects",
      "perturbed_subjects",
      "observed_loo_rows",
      "perturbed_loo_rows",
      "contaminated_observations",
      "comparative_analysis_complete"
    ),
    value = c(
      as.character(analysis_seed),
      as.character(length(methods)),
      paste(methods, collapse = ","),
      "0.05",
      canonical_md5,
      perturbed_md5,
      as.character(nlevels(observed_dat$cluster)),
      as.character(nlevels(perturbed_dat$cluster)),
      as.character(nrow(observed_loo_results)),
      as.character(nrow(perturbed_loo_results)),
      as.character(sum(perturbed$contaminated)),
      "TRUE"
    ),
    stringsAsFactors = FALSE
  )

  metadata_path <- file.path(
    definitive_dir, "study3_execution_metadata.csv"
  )
  definitive_write_csv_atomic(metadata, metadata_path)

  package_versions_path <- file.path(
    definitive_dir, "study3_package_versions.csv"
  )
  frozen_versions <- utils::read.csv(
    file.path(freeze_dir, "study3_package_versions.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  definitive_write_csv_atomic(frozen_versions, package_versions_path)

  session_path <- file.path(definitive_dir, "session_info.txt")
  writeLines(
    capture.output(utils::sessionInfo()),
    session_path,
    useBytes = TRUE
  )

  output_paths <- c(
    unname(table_paths),
    result_rds_path,
    plot_paths,
    metadata_path,
    package_versions_path,
    session_path
  )

  output_checksums <- study3c_output_checksums(
    output_paths,
    project_root
  )
  definitive_write_csv_atomic(output_checksums, checksum_path)

  writeLines(
    c(
      "mmiCATs Study 3 definitive empirical analysis COMPLETE",
      "",
      paste(
        "Completed:",
        format(Sys.time(), tz = "America/Los_Angeles", usetz = TRUE)
      ),
      paste("Methods:", paste(methods, collapse = ", ")),
      paste("Analysis seed:", analysis_seed),
      paste("Observed Subjects:", nlevels(observed_dat$cluster)),
      paste("Perturbed Subjects:", nlevels(perturbed_dat$cluster)),
      paste("Observed LOO rows:", nrow(observed_loo_results)),
      paste("Perturbed LOO rows:", nrow(perturbed_loo_results)),
      paste("Contaminated observations:", sum(perturbed$contaminated)),
      paste("Output checksum rows:", nrow(output_checksums))
    ),
    completion_marker,
    useBytes = TRUE
  )

  message("")
  message("Study 3 definitive comparative analysis complete.")
  message("Results saved to: ", definitive_dir)
  message("")
  message("Observed vs perturbed method comparison:")

  print(
    comparison[
      ,
      c(
        "method", "method_label",
        "observed_estimate", "observed_conf_low", "observed_conf_high",
        "perturbed_estimate", "perturbed_conf_low", "perturbed_conf_high",
        "estimate_change", "absolute_estimate_change", "ci_width_change"
      ),
      drop = FALSE
    ],
    row.names = FALSE
  )
}
