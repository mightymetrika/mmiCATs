# mmiCATs computer-audit R environment and benchmark
#
# Read-only with respect to package source. Outputs go under
# data-raw/computer-audit-results/<label>/.
#
# Use:
#   source("data-raw/computer_audit_r_benchmark.R")
#   run_mmicats_computer_audit("current-laptop")

run_mmicats_computer_audit <- function(label,
                                       run_benchmark = TRUE) {
  if (
    length(label) != 1L ||
    is.na(label) ||
    !nzchar(trimws(label))
  ) {
    stop(
      "label must be one nonempty character string.",
      call. = FALSE
    )
  }

  safe_label <- gsub(
    "[^A-Za-z0-9._-]",
    "_",
    trimws(label)
  )

  find_project_root <- function(path = getwd()) {
    path <- normalizePath(
      path,
      winslash = "/",
      mustWork = TRUE
    )

    repeat {
      if (file.exists(file.path(path, "DESCRIPTION"))) {
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

  project_root <- find_project_root()
  output_dir <- file.path(
    project_root,
    "data-raw",
    "computer-audit-results",
    safe_label
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop(
      "Package 'pkgload' is required for the R benchmark.",
      call. = FALSE
    )
  }

  pkgload::load_all(
    project_root,
    quiet = TRUE,
    export_all = TRUE
  )

  required_packages <- c(
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
    package = required_packages,
    version = vapply(
      required_packages,
      function(package_name) {
        if (requireNamespace(package_name, quietly = TRUE)) {
          as.character(utils::packageVersion(package_name))
        } else {
          NA_character_
        }
      },
      character(1)
    ),
    stringsAsFactors = FALSE
  )

  utils::write.csv(
    package_versions,
    file.path(output_dir, "r_package_versions.csv"),
    row.names = FALSE
  )

  cpu_logical <- parallel::detectCores(logical = TRUE)
  cpu_physical <- tryCatch(
    parallel::detectCores(logical = FALSE),
    error = function(e) NA_integer_
  )
  blas_path <- tryCatch(
    as.character(base::La_library()),
    error = function(e) NA_character_
  )

  environment <- data.frame(
    item = c(
      "audit_label",
      "r_version",
      "r_platform",
      "r_arch",
      "r_os",
      "logical_cores_detected",
      "physical_cores_detected",
      "blas_lapack_library",
      "working_directory",
      "project_root"
    ),
    value = c(
      safe_label,
      R.version.string,
      R.version$platform,
      R.version$arch,
      R.version$os,
      as.character(cpu_logical),
      as.character(cpu_physical),
      blas_path,
      normalizePath(getwd(), winslash = "/", mustWork = TRUE),
      project_root
    ),
    stringsAsFactors = FALSE
  )

  utils::write.csv(
    environment,
    file.path(output_dir, "r_environment.csv"),
    row.names = FALSE
  )

  source_files <- c(
    DESCRIPTION = file.path(project_root, "DESCRIPTION"),
    robust_mixed_models = file.path(project_root, "R", "robust_mixed_models.R"),
    pwr_func_study1 = file.path(project_root, "R", "pwr_func_study1.R"),
    pwr_func_study1_helpers = file.path(project_root, "R", "pwr_func_study1_helpers.R"),
    pwr_func_study2 = file.path(project_root, "R", "pwr_func_study2.R"),
    pwr_func_study2_helpers = file.path(project_root, "R", "pwr_func_study2_helpers.R")
  )

  source_checksums <- data.frame(
    source = names(source_files),
    path = normalizePath(source_files, winslash = "/", mustWork = TRUE),
    md5 = unname(tools::md5sum(source_files)),
    stringsAsFactors = FALSE
  )

  utils::write.csv(
    source_checksums,
    file.path(output_dir, "r_source_checksums.csv"),
    row.names = FALSE
  )

  expected_production_md5 <- c(
    robust_mixed_models = "a3f55f48736df665fa8ce45706dd9c49",
    pwr_func_study1 = "483c16ba66b163f122c1783f6120dd9e",
    pwr_func_study1_helpers = "4061bd490e77184ba9f79fcd5ab95384",
    pwr_func_study2 = "2ca209d83733efc9169363b48686e405",
    pwr_func_study2_helpers = "dd5f484c1855bf55dc8af72f770fb8d3"
  )

  baseline <- merge(
    data.frame(
      source = names(expected_production_md5),
      expected_md5 = unname(expected_production_md5),
      stringsAsFactors = FALSE
    ),
    source_checksums[
      source_checksums$source %in% names(expected_production_md5),
      c("source", "md5"),
      drop = FALSE
    ],
    by = "source",
    all.x = TRUE,
    sort = FALSE
  )
  baseline$matched <- baseline$expected_md5 == baseline$md5

  utils::write.csv(
    baseline,
    file.path(output_dir, "r_production_baseline.csv"),
    row.names = FALSE
  )

  writeLines(
    capture.output(print(extSoftVersion())),
    con = file.path(output_dir, "r_external_software.txt"),
    useBytes = TRUE
  )
  writeLines(
    capture.output(utils::sessionInfo()),
    con = file.path(output_dir, "r_session_info.txt"),
    useBytes = TRUE
  )

  if (!run_benchmark) {
    benchmark_results <- data.frame()
  } else {
    if (!all(baseline$matched %in% TRUE)) {
      stop(
        paste(
          "Production-source checksums do not match the Phase 4D baseline.",
          "Do not benchmark until the exact frozen source is installed."
        ),
        call. = FALSE
      )
    }

    if (
      is.na(
        package_versions$version[
          package_versions$package == "robustlmm"
        ]
      )
    ) {
      stop("Package 'robustlmm' is not installed.", call. = FALSE)
    }

    message("")
    message("Generating the fixed Study 2 G=40 benchmark data...")

    set.seed(20269701L)
    dat <- study2_simulate_data(
      n_clusters = 40L,
      cluster_size = 40L,
      beta = 0.10,
      intercept = 0,
      random_intercept_sd = 1,
      random_slope_sd = 0.10,
      residual_sd = 1,
      x_sd = 1,
      contamination = "none",
      contamination_prop = 0.05,
      contamination_size = 1
    )

    dataset_fingerprint <- data.frame(
      item = c(
        "rows",
        "clusters",
        "mean_out",
        "sd_out",
        "mean_x",
        "sd_x",
        "mean_true_cluster_slope",
        "sd_random_slope"
      ),
      value = c(
        nrow(dat),
        nlevels(factor(dat$cluster)),
        mean(dat$out),
        stats::sd(dat$out),
        mean(dat$x),
        stats::sd(dat$x),
        mean(dat$true_cluster_slope[!duplicated(dat$cluster)]),
        stats::sd(dat$random_slope[!duplicated(dat$cluster)])
      ),
      stringsAsFactors = FALSE
    )
    utils::write.csv(
      dataset_fingerprint,
      file.path(output_dir, "r_benchmark_dataset_fingerprint.csv"),
      row.names = FALSE
    )

    run_one <- function(model, seed) {
      gc(reset = TRUE, full = TRUE)
      set.seed(seed)
      captured <- NULL

      timing <- system.time({
        captured <- tryCatch(
          study_fit_robust_mixed(
            dat = dat,
            alpha = 0.05,
            model = model
          ),
          error = function(e) {
            structure(
              list(error_message = conditionMessage(e)),
              class = "mmiCATs_benchmark_error"
            )
          }
        )
      })

      if (inherits(captured, "mmiCATs_benchmark_error")) {
        return(data.frame(
          model = model,
          success = FALSE,
          elapsed_sec = unname(timing["elapsed"]),
          user_sec = unname(timing["user.self"]),
          system_sec = unname(timing["sys.self"]),
          estimate = NA_real_,
          std_error = NA_real_,
          df = NA_real_,
          p_value = NA_real_,
          converged = NA,
          boundary = NA,
          optimizer_code = NA_real_,
          estimated_random_intercept_sd = NA_real_,
          estimated_random_slope_sd = NA_real_,
          warning = NA_character_,
          error = captured$error_message,
          stringsAsFactors = FALSE
        ))
      }

      data.frame(
        model = model,
        success = TRUE,
        elapsed_sec = unname(timing["elapsed"]),
        user_sec = unname(timing["user.self"]),
        system_sec = unname(timing["sys.self"]),
        estimate = captured$estimate,
        std_error = captured$std_error,
        df = captured$df,
        p_value = captured$p_value,
        converged = captured$converged,
        boundary = captured$singular,
        optimizer_code = captured$optimizer_code,
        estimated_random_intercept_sd = captured$estimated_random_intercept_sd,
        estimated_random_slope_sd = captured$estimated_random_slope_sd,
        warning = captured$warning,
        error = NA_character_,
        stringsAsFactors = FALSE
      )
    }

    message("Benchmark 1 of 2: robust random-intercept model at G=40...")
    ri <- run_one(model = "ri", seed = 20269702L)

    message("Benchmark 2 of 2: robust random-slope model at G=40...")
    rs <- run_one(model = "rs", seed = 20269703L)

    benchmark_results <- rbind(ri, rs)
    rownames(benchmark_results) <- NULL
    utils::write.csv(
      benchmark_results,
      file.path(output_dir, "r_benchmark_results.csv"),
      row.names = FALSE
    )
  }

  summary_lines <- c(
    "mmiCATs R computer audit",
    "",
    paste("Label:", safe_label),
    paste("R:", R.version.string),
    paste("Logical cores detected:", cpu_logical),
    paste("Physical cores detected:", cpu_physical),
    paste(
      "Phase 4D production baseline matched:",
      all(baseline$matched %in% TRUE)
    ),
    paste("Benchmark requested:", run_benchmark)
  )

  if (run_benchmark && nrow(benchmark_results) > 0L) {
    summary_lines <- c(
      summary_lines,
      paste(
        "Robust RI G=40 elapsed seconds:",
        benchmark_results$elapsed_sec[benchmark_results$model == "ri"]
      ),
      paste(
        "Robust RS G=40 elapsed seconds:",
        benchmark_results$elapsed_sec[benchmark_results$model == "rs"]
      ),
      paste(
        "Benchmark successes:",
        sum(benchmark_results$success %in% TRUE),
        "of",
        nrow(benchmark_results)
      )
    )
  }

  writeLines(
    summary_lines,
    con = file.path(output_dir, "r_audit_summary.txt"),
    useBytes = TRUE
  )

  saveRDS(
    list(
      environment = environment,
      package_versions = package_versions,
      source_checksums = source_checksums,
      production_baseline = baseline,
      benchmark_results = benchmark_results,
      session_info = utils::sessionInfo()
    ),
    file.path(output_dir, "r_audit_results.rds"),
    version = 3
  )

  message("")
  message(paste("R audit complete. Results saved to:", output_dir))

  invisible(list(
    output_dir = output_dir,
    environment = environment,
    package_versions = package_versions,
    source_checksums = source_checksums,
    production_baseline = baseline,
    benchmark_results = benchmark_results
  ))
}
