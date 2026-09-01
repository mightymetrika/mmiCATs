# Internal Study 3 empirical preparation/execution helpers.
#
# The Phase 6B freeze artifacts remain immutable. During Phase 6D and later,
# current source-code checksums are allowed to differ from the source versions
# recorded inside the pre-results freeze because those changes are explicitly
# documented post-results. The frozen artifact checksum record itself is still
# verified, and callers may set verify_original_sources = TRUE when reproducing
# the exact pre-amendment source tree.
#
# `dataset` is a locally created plotting column used inside ggplot2::aes().
# Declare it for R CMD check without changing runtime behavior.
utils::globalVariables("dataset")

study3_find_project_root <- function(path = getwd()) {
  path <- normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )

  repeat {
    if (file.exists(
      file.path(
        path,
        "DESCRIPTION"
      )
    )) {
      return(path)
    }

    parent <- dirname(path)

    if (identical(
      parent,
      path
    )) {
      stop(
        "Could not locate the mmiCATs project root.",
        call. = FALSE
      )
    }

    path <- parent
  }
}


study3_write_csv_atomic <- function(data,
                                    path) {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  temp_path <- tempfile(
    pattern = paste0(
      basename(path),
      "_"
    ),
    tmpdir = dirname(path),
    fileext = ".tmp"
  )

  on.exit(
    if (file.exists(temp_path)) {
      unlink(
        temp_path,
        force = TRUE
      )
    },
    add = TRUE
  )

  utils::write.csv(
    data,
    temp_path,
    row.names = FALSE,
    na = ""
  )

  if (file.exists(path)) {
    stop(
      paste(
        "Refusing to overwrite frozen Study 3 artifact:",
        path
      ),
      call. = FALSE
    )
  }

  if (!file.rename(
    temp_path,
    path
  )) {
    stop(
      paste(
        "Could not atomically save:",
        path
      ),
      call. = FALSE
    )
  }

  invisible(path)
}


study3_save_rds_atomic <- function(object,
                                   path) {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  temp_path <- tempfile(
    pattern = paste0(
      basename(path),
      "_"
    ),
    tmpdir = dirname(path),
    fileext = ".tmp"
  )

  on.exit(
    if (file.exists(temp_path)) {
      unlink(
        temp_path,
        force = TRUE
      )
    },
    add = TRUE
  )

  saveRDS(
    object,
    temp_path,
    version = 3,
    compress = "gzip"
  )

  # Verify readability before final rename.
  readRDS(temp_path)

  if (file.exists(path)) {
    stop(
      paste(
        "Refusing to overwrite frozen Study 3 artifact:",
        path
      ),
      call. = FALSE
    )
  }

  if (!file.rename(
    temp_path,
    path
  )) {
    stop(
      paste(
        "Could not atomically save:",
        path
      ),
      call. = FALSE
    )
  }

  invisible(path)
}


study3_file_md5 <- function(paths,
                            project_root) {
  normalized_paths <- normalizePath(
    paths,
    winslash = "/",
    mustWork = TRUE
  )

  normalized_root <- normalizePath(
    project_root,
    winslash = "/",
    mustWork = TRUE
  )

  prefix <- paste0(
    normalized_root,
    "/"
  )

  if (!all(
    startsWith(
      normalized_paths,
      prefix
    )
  )) {
    stop(
      "All frozen artifacts must be inside the project root.",
      call. = FALSE
    )
  }

  data.frame(
    file = basename(
      normalized_paths
    ),
    relative_path = substring(
      normalized_paths,
      nchar(prefix) + 1L
    ),
    md5 = unname(
      tools::md5sum(
        normalized_paths
      )
    ),
    stringsAsFactors = FALSE
  )
}


study3_structural_checks <- function(dat) {
  subject_character <- as.character(
    dat$Subject
  )

  subject_counts <- table(
    subject_character
  )

  days_by_subject <- split(
    dat$Days,
    subject_character
  )

  data.frame(
    check = c(
      "rows_equal_180",
      "subjects_equal_18",
      "no_missing_analysis_values",
      "each_subject_has_10_rows",
      "each_subject_has_days_0_to_9",
      "reaction_is_numeric",
      "days_is_numeric"
    ),
    passed = c(
      nrow(dat) == 180L,
      length(
        unique(
          subject_character
        )
      ) == 18L,
      !anyNA(
        dat[
          ,
          c(
            "Reaction",
            "Days",
            "Subject"
          )
        ]
      ),
      length(subject_counts) == 18L &&
        all(
          subject_counts == 10L
        ),
      length(days_by_subject) == 18L &&
        all(
          vapply(
            days_by_subject,
            function(x) {
              identical(
                sort(
                  as.numeric(x)
                ),
                as.numeric(0:9)
              )
            },
            logical(1)
          )
        ),
      is.numeric(
        dat$Reaction
      ),
      is.numeric(
        dat$Days
      )
    ),
    stringsAsFactors = FALSE
  )
}




# Study 3 Phase 6C helpers (data-raw research infrastructure)

study3c_methods <- function() {
  c(
    "rs", "ri", "cr2", "cats", "cats_trunc",
    "cats_robust", "cats_robustbase", "robust_ri", "robust_rs"
  )
}

study3c_analysis_seed <- function() 20261101L

study3c_find_project_root <- function(path = getwd()) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "DESCRIPTION"))) return(path)
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Could not locate the mmiCATs project root.", call. = FALSE)
    }
    path <- parent
  }
}

study3c_verify_checksum_record <- function(path, project_root) {
  x <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(c("relative_path", "md5") %in% names(x))) {
    stop("Checksum record lacks relative_path/md5: ", path, call. = FALSE)
  }
  paths <- file.path(project_root, x$relative_path)
  if (!all(file.exists(paths))) {
    stop(
      "Recorded file missing: ",
      paste(x$relative_path[!file.exists(paths)], collapse = ", "),
      call. = FALSE
    )
  }
  current <- unname(tools::md5sum(paths))
  if (!identical(current, x$md5)) {
    stop(
      "Checksum mismatch: ",
      paste(x$relative_path[current != x$md5], collapse = ", "),
      call. = FALSE
    )
  }
  invisible(x)
}

study3c_verify_freeze <- function(
    project_root,
    freeze_dir = NULL,
    verify_original_sources = FALSE) {
  if (is.null(freeze_dir)) {
    freeze_dir <- file.path(
      project_root,
      "data-raw",
      "study3-results",
      "pre-results-freeze"
    )
  }

  freeze_dir <- normalizePath(
    freeze_dir,
    winslash = "/",
    mustWork = TRUE
  )
  required <- c(
    "FREEZE_COMPLETE.txt", "sleepstudy_canonical.rds",
    "sleepstudy_perturbed.rds", "study3_contamination_map.rds",
    "study3_freeze_metadata.csv", "study3_source_checksums.csv",
    "study3_package_versions.csv", "study3_frozen_artifact_checksums.csv",
    "study3_freeze_record.rds", "session_info.txt"
  )
  paths <- file.path(freeze_dir, required)
  if (!all(file.exists(paths))) {
    stop(
      "Missing Phase 6B artifact: ",
      paste(required[!file.exists(paths)], collapse = ", "),
      call. = FALSE
    )
  }

  study3c_verify_checksum_record(
    file.path(freeze_dir, "study3_frozen_artifact_checksums.csv"),
    project_root
  )
  if (isTRUE(verify_original_sources)) {
    study3c_verify_checksum_record(
      file.path(
        freeze_dir,
        "study3_source_checksums.csv"
      ),
      project_root
    )
  }

  completion <- readLines(
    file.path(freeze_dir, "FREEZE_COMPLETE.txt"), warn = FALSE
  )
  line <- grep("^Freeze record MD5:", completion, value = TRUE)
  if (length(line) != 1L) {
    stop("Freeze marker lacks exactly one freeze-record MD5.", call. = FALSE)
  }
  expected <- trimws(sub("^Freeze record MD5:", "", line))
  observed <- unname(tools::md5sum(
    file.path(freeze_dir, "study3_freeze_record.rds")
  ))
  if (!identical(expected, observed)) {
    stop("Phase 6B freeze-record checksum mismatch.", call. = FALSE)
  }

  frozen_packages <- utils::read.csv(
    file.path(freeze_dir, "study3_package_versions.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  current_versions <- vapply(
    frozen_packages$package,
    function(p) {
      if (!requireNamespace(p, quietly = TRUE)) return(NA_character_)
      as.character(utils::packageVersion(p))
    },
    FUN.VALUE = character(1)
  )
  if (!identical(
    unname(current_versions),
    unname(frozen_packages$version)
  )) {
    bad <- frozen_packages$package[
      is.na(current_versions) |
        unname(current_versions) !=
          unname(frozen_packages$version)
    ]
    stop(
      "Package versions changed since Phase 6B: ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }

  frozen_session <- readLines(
    file.path(freeze_dir, "session_info.txt"), warn = FALSE
  )
  if (length(frozen_session) < 1L ||
      !identical(frozen_session[1L], R.version.string)) {
    stop("R version changed since Phase 6B.", call. = FALSE)
  }

  if (!identical(study2_method_names(), study3c_methods())) {
    stop("Frozen nine-method schedule has changed.", call. = FALSE)
  }

  default_seed <- as.integer(eval(
    formals(cluster_model_diagnostics)$seed
  ))
  if (!identical(default_seed, study3c_analysis_seed())) {
    stop("cluster_model_diagnostics() default seed changed.", call. = FALSE)
  }

  metadata <- utils::read.csv(
    file.path(freeze_dir, "study3_freeze_metadata.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  lookup <- stats::setNames(metadata$value, metadata$field)
  ok <- identical(
    lookup[["random_slope_model"]],
    "Reaction ~ Days + (1 + Days || Subject)"
  ) &&
    identical(
      lookup[["random_intercept_model"]],
      "Reaction ~ Days + (1 | Subject)"
    ) &&
    identical(
      lookup[["comparative_models_fit_during_freeze"]],
      "FALSE"
    )
  if (!ok) stop("Phase 6B metadata does not match the approved plan.", call. = FALSE)

  invisible(list(freeze_dir = freeze_dir, metadata = metadata))
}

study3c_prepare_analysis_data <- function(data) {
  x <- cluster_diag_prepare_data(
    data = data, response = "Reaction", predictor = "Days", cluster = "Subject"
  )$analysis_data
  x <- x[stats::complete.cases(x[, c("out", "x"), drop = FALSE]), , drop = FALSE]
  x$cluster <- droplevels(factor(x$cluster))
  x
}

study3c_fit_full <- function(
    data,
    methods = study3c_methods(),
    seed = study3c_analysis_seed()) {
  cluster_model_diagnostics(
    Reaction ~ Days, ~ Subject, data,
    methods = methods, alpha = 0.05, seed = seed,
    leave_one_cluster_out = FALSE
  )
}

study3c_fit_loo_subject <- function(
    dat, methods, seed, full_comparison, cluster_index) {
  clusters <- levels(dat$cluster)
  if (cluster_index < 1L || cluster_index > length(clusters)) {
    stop("cluster_index outside range.", call. = FALSE)
  }
  omitted <- clusters[cluster_index]
  d <- dat[dat$cluster != omitted, , drop = FALSE]
  d$cluster <- droplevels(factor(d$cluster))
  valid_methods <- study2_method_names()

  rows <- lapply(methods, function(method) {
    method_index <- match(method, valid_methods)
    cluster_seed <- cluster_diag_loo_seed(
      seed = seed, cluster_index = cluster_index
    )
    z <- study2_fit_method(
      dat = d[, c("out", "x", "cluster"), drop = FALSE],
      method = method, beta = 0, alpha = 0.05, replicate_id = 1L,
      method_seed = study2_method_seed(
        replicate_seed = cluster_seed, method_index = method_index
      ),
      realized_mean_slope = NA_real_,
      realized_random_slope_sd = NA_real_
    )
    full_est <- full_comparison$estimate[
      full_comparison$method == method
    ][1L]
    data.frame(
      method = method,
      method_label = cluster_diag_method_labels(method),
      omitted_cluster = omitted,
      full_estimate = full_est,
      leave_one_out_estimate = z$estimate[1L],
      estimate_change = z$estimate[1L] - full_est,
      fit_success = z$fit_success[1L],
      warning = z$warning[1L],
      error = z$error[1L],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

study3c_loo_path <- function(dir, dataset, cluster_index, omitted_cluster) {
  safe <- gsub("[^A-Za-z0-9._-]", "_", omitted_cluster)
  file.path(
    dir,
    sprintf("%s__subject_%02d_%s.rds", dataset, cluster_index, safe)
  )
}

study3c_checkpoint_matches <- function(
    x, dataset, input_md5, methods, seed, cluster_index, omitted_cluster) {
  if (!is.list(x)) return(FALSE)
  needed <- c(
    "status", "dataset", "input_md5", "methods", "seed",
    "cluster_index", "omitted_cluster"
  )
  if (!all(needed %in% names(x))) return(FALSE)
  identical(x$dataset, dataset) &&
    identical(x$input_md5, input_md5) &&
    identical(x$methods, methods) &&
    identical(as.integer(x$seed), as.integer(seed)) &&
    identical(as.integer(x$cluster_index), as.integer(cluster_index)) &&
    identical(x$omitted_cluster, as.character(omitted_cluster))
}

study3c_run_loo_checkpoint <- function(
    dat, methods, seed, full_comparison, cluster_index,
    dataset, input_md5, checkpoint_dir) {
  omitted <- levels(dat$cluster)[cluster_index]
  path <- study3c_loo_path(
    checkpoint_dir, dataset, cluster_index, omitted
  )

  if (file.exists(path)) {
    old <- tryCatch(readRDS(path), error = function(e) e)
    if (inherits(old, "error")) {
      stop("Unreadable Study 3 checkpoint: ", path, call. = FALSE)
    }
    if (!study3c_checkpoint_matches(
      old, dataset, input_md5, methods, seed, cluster_index, omitted
    )) {
      stop("Mismatched Study 3 checkpoint: ", path, call. = FALSE)
    }
    if (identical(old$status, "complete")) {
      return(list(action = "skipped", path = path, checkpoint = old))
    }
    if (!identical(old$status, "error")) {
      stop("Unrecognized checkpoint status: ", path, call. = FALSE)
    }
  }

  started <- Sys.time()
  result <- tryCatch(
    study3c_fit_loo_subject(
      dat, methods, seed, full_comparison, cluster_index
    ),
    error = function(e) e
  )
  completed <- Sys.time()

  if (inherits(result, "error")) {
    checkpoint <- list(
      status = "error", dataset = dataset, input_md5 = input_md5,
      methods = methods, seed = as.integer(seed),
      cluster_index = as.integer(cluster_index),
      omitted_cluster = as.character(omitted),
      results = NULL, error = conditionMessage(result),
      started_at = started, completed_at = completed,
      elapsed_sec = as.numeric(difftime(completed, started, units = "secs"))
    )
    definitive_save_rds_atomic(checkpoint, path)
    return(list(action = "error", path = path, checkpoint = checkpoint))
  }

  checkpoint <- list(
    status = "complete", dataset = dataset, input_md5 = input_md5,
    methods = methods, seed = as.integer(seed),
    cluster_index = as.integer(cluster_index),
    omitted_cluster = as.character(omitted),
    results = result, error = NA_character_,
    started_at = started, completed_at = completed,
    elapsed_sec = as.numeric(difftime(completed, started, units = "secs"))
  )
  definitive_save_rds_atomic(checkpoint, path)
  list(action = "completed", path = path, checkpoint = checkpoint)
}

study3c_collect_loo <- function(
    dat, methods, seed, dataset, input_md5, checkpoint_dir) {
  clusters <- levels(dat$cluster)
  results <- vector("list", length(clusters))
  status <- vector("list", length(clusters))

  for (i in seq_along(clusters)) {
    path <- study3c_loo_path(checkpoint_dir, dataset, i, clusters[i])
    if (!file.exists(path)) {
      status[[i]] <- data.frame(
        dataset = dataset, cluster_index = i, omitted_cluster = clusters[i],
        status = "not_started", elapsed_sec = NA_real_, error = NA_character_,
        stringsAsFactors = FALSE
      )
      next
    }
    x <- tryCatch(readRDS(path), error = function(e) e)
    if (inherits(x, "error")) {
      status[[i]] <- data.frame(
        dataset = dataset, cluster_index = i, omitted_cluster = clusters[i],
        status = "unreadable", elapsed_sec = NA_real_,
        error = conditionMessage(x), stringsAsFactors = FALSE
      )
      next
    }
    match_ok <- study3c_checkpoint_matches(
      x, dataset, input_md5, methods, seed, i, clusters[i]
    )
    this_status <- if (match_ok) x$status else "invalid"
    status[[i]] <- data.frame(
      dataset = dataset, cluster_index = i, omitted_cluster = clusters[i],
      status = this_status,
      elapsed_sec = if (!is.null(x$elapsed_sec)) x$elapsed_sec else NA_real_,
      error = if (!is.null(x$error)) x$error else NA_character_,
      stringsAsFactors = FALSE
    )
    if (match_ok && identical(x$status, "complete")) results[[i]] <- x$results
  }

  status <- do.call(rbind, status)
  rownames(status) <- NULL
  complete <- all(status$status == "complete")
  rows <- if (complete) do.call(rbind, results) else NULL
  if (!is.null(rows)) rownames(rows) <- NULL
  list(complete = complete, status = status, results = rows)
}

study3c_make_comparison <- function(observed, perturbed) {
  ids <- c("method", "method_label", "method_order")
  metric <- setdiff(intersect(names(observed), names(perturbed)), ids)

  o <- observed[, c(ids, metric), drop = FALSE]
  p <- perturbed[, c(ids, metric), drop = FALSE]
  names(o)[match(metric, names(o))] <- paste0("observed_", metric)
  names(p)[match(metric, names(p))] <- paste0("perturbed_", metric)

  out <- merge(o, p, by = ids, all = TRUE, sort = FALSE)
  out$observed_ci_width <- out$observed_conf_high - out$observed_conf_low
  out$perturbed_ci_width <- out$perturbed_conf_high - out$perturbed_conf_low
  out$estimate_change <- out$perturbed_estimate - out$observed_estimate
  out$absolute_estimate_change <- abs(out$estimate_change)
  out$ci_width_change <- out$perturbed_ci_width - out$observed_ci_width
  out <- out[order(out$method_order), , drop = FALSE]
  rownames(out) <- NULL
  out
}

study3c_cross_dataset_plot <- function(comparison) {
  make_rows <- function(prefix, label) {
    data.frame(
      method_label = comparison$method_label,
      method_order = comparison$method_order,
      dataset = label,
      estimate = comparison[[paste0(prefix, "_estimate")]],
      conf_low = comparison[[paste0(prefix, "_conf_low")]],
      conf_high = comparison[[paste0(prefix, "_conf_high")]],
      fit_success = comparison[[paste0(prefix, "_fit_success")]],
      stringsAsFactors = FALSE
    )
  }
  d <- rbind(make_rows("observed", "Observed"), make_rows("perturbed", "Perturbed"))
  d <- d[
    d$fit_success %in% TRUE &
      is.finite(d$estimate) & is.finite(d$conf_low) & is.finite(d$conf_high),
    , drop = FALSE
  ]
  labels <- comparison$method_label[order(comparison$method_order)]
  d$method_label <- factor(d$method_label, levels = rev(labels))

  ggplot2::ggplot(
    d, ggplot2::aes(x = estimate, y = method_label, shape = dataset)
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = conf_low, xmax = conf_high),
      orientation = "y",
      position = ggplot2::position_dodge(width = 0.5),
      height = 0
    ) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::labs(
      x = "Estimated change in Reaction per Day", y = NULL, shape = "Dataset",
      title = "Study 3 observed and perturbed estimates"
    ) +
    ggplot2::theme_minimal()
}

study3c_loo_plot <- function(x) {
  d <- x[is.finite(x$estimate_change), , drop = FALSE]
  d$method_label <- factor(
    d$method_label,
    levels = cluster_diag_method_labels(study3c_methods())
  )
  ggplot2::ggplot(
    d, ggplot2::aes(x = omitted_cluster, y = estimate_change)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(dataset ~ method_label, scales = "free_y") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "Omitted Subject",
      y = "Leave-one-Subject-out estimate minus full estimate",
      title = "Study 3 leave-one-Subject-out influence"
    ) +
    ggplot2::theme_minimal()
}

study3c_save_plot <- function(plot, path, width, height) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".pdf")
  on.exit(if (file.exists(tmp)) unlink(tmp, force = TRUE), add = TRUE)
  ggplot2::ggsave(
    tmp, plot = plot, device = "pdf",
    width = width, height = height, units = "in", limitsize = FALSE
  )
  if (!file.exists(tmp) || file.info(tmp)$size <= 0) {
    stop("Plot save failed: ", path, call. = FALSE)
  }
  if (file.exists(path)) unlink(path, force = TRUE)
  if (!file.rename(tmp, path)) stop("Could not move plot into place.", call. = FALSE)
  invisible(path)
}

study3c_output_checksums <- function(paths, project_root) {
  p <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  prefix <- paste0(root, "/")
  if (!all(startsWith(p, prefix))) {
    stop("Study 3 output outside project root.", call. = FALSE)
  }
  data.frame(
    file = basename(p),
    relative_path = substring(p, nchar(prefix) + 1L),
    md5 = unname(tools::md5sum(p)),
    stringsAsFactors = FALSE
  )
}
