# Internal deterministic sharding/checkpoint infrastructure for definitive manuscript studies.
# Migrated from data-raw during Phase 6D-B1 without changing implementation.

# Definitive simulation sharding helpers
#
# Phase 5B engineering layer for the mmiCATs manuscript-version simulations.
# These helpers do not change any estimator, DGP, or inferential rule.
# They manage deterministic seed vectors, shard plans, atomic checkpoints,
# restart/resume behavior, and disk-space guards.

definitive_save_rds_atomic <- function(object,
                                       path,
                                       compress = "gzip") {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  temp_path <- tempfile(
    pattern = paste0(basename(path), "_"),
    tmpdir = dirname(path),
    fileext = ".tmp"
  )

  on.exit(
    if (file.exists(temp_path)) {
      unlink(temp_path, force = TRUE)
    },
    add = TRUE
  )

  saveRDS(
    object,
    temp_path,
    version = 3,
    compress = compress
  )

  # Verify that the temporary file is readable before replacing anything.
  tryCatch(
    readRDS(temp_path),
    error = function(e) {
      stop(
        paste(
          "Atomic checkpoint verification failed for:",
          temp_path,
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  if (file.exists(path)) {
    backup_path <- paste0(path, ".previous")

    if (file.exists(backup_path)) {
      unlink(backup_path, force = TRUE)
    }

    if (!file.rename(path, backup_path)) {
      stop(
        paste(
          "Could not move the existing checkpoint aside:",
          path
        ),
        call. = FALSE
      )
    }

    replaced <- file.rename(temp_path, path)

    if (!replaced) {
      file.rename(backup_path, path)
      stop(
        paste(
          "Could not replace checkpoint:",
          path
        ),
        call. = FALSE
      )
    }

    unlink(backup_path, force = TRUE)
  } else {
    if (!file.rename(temp_path, path)) {
      stop(
        paste(
          "Could not save checkpoint:",
          path
        ),
        call. = FALSE
      )
    }
  }

  invisible(path)
}


definitive_write_csv_atomic <- function(data,
                                        path) {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  temp_path <- tempfile(
    pattern = paste0(basename(path), "_"),
    tmpdir = dirname(path),
    fileext = ".tmp"
  )

  on.exit(
    if (file.exists(temp_path)) {
      unlink(temp_path, force = TRUE)
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
    backup_path <- paste0(path, ".previous")

    if (file.exists(backup_path)) {
      unlink(backup_path, force = TRUE)
    }

    if (!file.rename(path, backup_path)) {
      stop(
        paste(
          "Could not move the existing CSV aside:",
          path
        ),
        call. = FALSE
      )
    }

    replaced <- file.rename(temp_path, path)

    if (!replaced) {
      file.rename(backup_path, path)
      stop(
        paste(
          "Could not replace CSV:",
          path
        ),
        call. = FALSE
      )
    }

    unlink(backup_path, force = TRUE)
  } else {
    if (!file.rename(temp_path, path)) {
      stop(
        paste(
          "Could not save CSV:",
          path
        ),
        call. = FALSE
      )
    }
  }

  invisible(path)
}


definitive_make_replicate_seeds <- function(condition_seed,
                                             total_reps) {
  if (!is.numeric(condition_seed) ||
      length(condition_seed) != 1L ||
      is.na(condition_seed) ||
      !is.finite(condition_seed) ||
      condition_seed < 0 ||
      condition_seed > .Machine$integer.max ||
      condition_seed != floor(condition_seed)) {
    stop(
      "condition_seed must be one non-negative integer seed.",
      call. = FALSE
    )
  }

  if (!is.numeric(total_reps) ||
      length(total_reps) != 1L ||
      is.na(total_reps) ||
      !is.finite(total_reps) ||
      total_reps < 1L ||
      total_reps != floor(total_reps)) {
    stop(
      "total_reps must be one positive integer.",
      call. = FALSE
    )
  }

  set.seed(as.integer(condition_seed))

  sample.int(
    .Machine$integer.max,
    size = as.integer(total_reps),
    replace = FALSE
  )
}


definitive_make_shard_plan <- function(total_reps,
                                       shard_size) {
  if (!is.numeric(total_reps) ||
      length(total_reps) != 1L ||
      is.na(total_reps) ||
      total_reps < 1L ||
      total_reps != floor(total_reps)) {
    stop(
      "total_reps must be one positive integer.",
      call. = FALSE
    )
  }

  if (!is.numeric(shard_size) ||
      length(shard_size) != 1L ||
      is.na(shard_size) ||
      shard_size < 1L ||
      shard_size != floor(shard_size)) {
    stop(
      "shard_size must be one positive integer.",
      call. = FALSE
    )
  }

  total_reps <- as.integer(total_reps)
  shard_size <- as.integer(shard_size)

  starts <- seq.int(
    from = 1L,
    to = total_reps,
    by = shard_size
  )

  ends <- pmin(
    starts + shard_size - 1L,
    total_reps
  )

  data.frame(
    shard_id = sprintf(
      "R%04d-R%04d",
      starts,
      ends
    ),
    shard_index = seq_along(starts),
    replicate_start = starts,
    replicate_end = ends,
    shard_reps = ends - starts + 1L,
    stringsAsFactors = FALSE
  )
}


definitive_shard_checkpoint_path <- function(shard_dir,
                                             condition_id,
                                             shard_id) {
  file.path(
    shard_dir,
    paste0(
      "condition_",
      condition_id,
      "__shard_",
      shard_id,
      ".rds"
    )
  )
}


definitive_read_checkpoint <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch(
    readRDS(path),
    error = function(e) {
      structure(
        list(
          path = path,
          error = conditionMessage(e)
        ),
        class = "definitive_unreadable_checkpoint"
      )
    }
  )
}




definitive_checkpoint_spec_matches <- function(checkpoint,
                                               condition_id,
                                               shard_row,
                                               expected_seeds,
                                               expected_methods) {
  if (inherits(checkpoint, "definitive_unreadable_checkpoint") ||
      !is.list(checkpoint)) {
    return(FALSE)
  }

  required_names <- c(
    "condition_id", "shard_id", "replicate_start",
    "replicate_end", "replicate_seeds", "methods"
  )

  if (!all(required_names %in% names(checkpoint))) {
    return(FALSE)
  }

  identical(checkpoint$condition_id, as.character(condition_id)) &&
    identical(checkpoint$shard_id, as.character(shard_row$shard_id)) &&
    identical(as.integer(checkpoint$replicate_start),
              as.integer(shard_row$replicate_start)) &&
    identical(as.integer(checkpoint$replicate_end),
              as.integer(shard_row$replicate_end)) &&
    identical(as.integer(checkpoint$replicate_seeds),
              as.integer(expected_seeds)) &&
    identical(checkpoint$methods, expected_methods)
}

definitive_validate_complete_checkpoint <- function(checkpoint,
                                                    condition_id,
                                                    shard_row,
                                                    expected_seeds,
                                                    expected_methods) {
  if (!definitive_checkpoint_spec_matches(
    checkpoint = checkpoint,
    condition_id = condition_id,
    shard_row = shard_row,
    expected_seeds = expected_seeds,
    expected_methods = expected_methods
  ) || !identical(checkpoint$status, "complete")) {
    return(FALSE)
  }

  if (!is.data.frame(checkpoint$replicates)) {
    return(FALSE)
  }

  expected_rep_ids <- seq.int(
    shard_row$replicate_start,
    shard_row$replicate_end
  )

  observed_rep_ids <- sort(
    unique(
      as.integer(
        checkpoint$replicates$replicate
      )
    )
  )

  identical(
    observed_rep_ids,
    as.integer(expected_rep_ids)
  )
}


definitive_get_free_gb <- function(path) {
  path <- normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )

  if (.Platform$OS.type == "windows") {
    drive <- substr(path, 1L, 2L)

    disk <- tryCatch(
      system2(
        "powershell",
        c(
          "-NoProfile",
          "-Command",
          shQuote(
            paste0(
              "(Get-CimInstance Win32_LogicalDisk -Filter ",
              "\"DeviceID='", drive, "'\").FreeSpace"
            )
          )
        ),
        stdout = TRUE,
        stderr = TRUE
      ),
      error = function(e) character()
    )

    value <- suppressWarnings(
      as.numeric(
        utils::tail(
          trimws(disk),
          1L
        )
      )
    )

    if (length(value) == 1L &&
        is.finite(value)) {
      return(value / 1024^3)
    }

    return(NA_real_)
  }

  disk <- tryCatch(
    system2(
      "df",
      c("-Pk", shQuote(path)),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) character()
  )

  if (length(disk) < 2L) {
    return(NA_real_)
  }

  fields <- strsplit(
    trimws(utils::tail(disk, 1L)),
    "[[:space:]]+"
  )[[1L]]

  if (length(fields) < 4L) {
    return(NA_real_)
  }

  available_kb <- suppressWarnings(
    as.numeric(fields[4L])
  )

  if (!is.finite(available_kb)) {
    return(NA_real_)
  }

  available_kb / 1024^2
}


definitive_disk_guard <- function(path,
                                  minimum_free_gb = 2.0,
                                  free_gb = NULL) {
  if (!is.numeric(minimum_free_gb) ||
      length(minimum_free_gb) != 1L ||
      is.na(minimum_free_gb) ||
      !is.finite(minimum_free_gb) ||
      minimum_free_gb < 0) {
    stop(
      "minimum_free_gb must be one non-negative finite number.",
      call. = FALSE
    )
  }

  if (is.null(free_gb)) {
    free_gb <- definitive_get_free_gb(path)
  }

  if (!is.numeric(free_gb) ||
      length(free_gb) != 1L ||
      is.na(free_gb) ||
      !is.finite(free_gb)) {
    stop(
      paste(
        "Could not determine free disk space.",
        "The definitive runner will not continue without a valid disk-space check."
      ),
      call. = FALSE
    )
  }

  if (free_gb < minimum_free_gb) {
    stop(
      sprintf(
        paste0(
          "Free disk space is %.3f GB, below the %.3f GB safety threshold. ",
          "No new shard was started."
        ),
        free_gb,
        minimum_free_gb
      ),
      call. = FALSE
    )
  }

  invisible(free_gb)
}


definitive_offset_shard_replicates <- function(replicates,
                                                replicate_start) {
  if (!is.data.frame(replicates) ||
      !"replicate" %in% names(replicates)) {
    stop(
      "replicates must contain a replicate column.",
      call. = FALSE
    )
  }

  local_ids <- as.integer(replicates$replicate)

  if (anyNA(local_ids) ||
      any(local_ids < 1L)) {
    stop(
      "Local replicate identifiers are invalid.",
      call. = FALSE
    )
  }

  replicates$replicate <-
    local_ids + as.integer(replicate_start) - 1L

  replicates
}


definitive_run_study1_shard <- function(condition,
                                        shard_row,
                                        replicate_seeds,
                                        methods) {
  function_contamination_size <- if (
    condition$contamination == "none"
  ) {
    1
  } else {
    condition$contamination_size
  }

  function_leverage_size <- if (
    condition$contamination == "bad_leverage"
  ) {
    condition$leverage_size
  } else {
    1
  }

  result <- suppressWarnings(
    pwr_func_study1(
      n_clusters = condition$n_clusters,
      cluster_size = condition$cluster_size,
      beta = condition$beta,
      intercept = condition$intercept,
      random_intercept_sd =
        condition$random_intercept_sd,
      residual_sd = condition$residual_sd,
      x_sd = condition$x_sd,
      contamination = condition$contamination,
      contamination_prop =
        condition$contamination_prop,
      contamination_size =
        function_contamination_size,
      leverage_size =
        function_leverage_size,
      reps = length(replicate_seeds),
      alpha = condition$alpha,
      methods = methods,
      seed = NULL,
      replicate_seeds = replicate_seeds,
      keep_replicates = TRUE
    )
  )

  result$replicates <-
    definitive_offset_shard_replicates(
      result$replicates,
      shard_row$replicate_start
    )

  result
}


definitive_run_study2_shard <- function(condition,
                                        shard_row,
                                        replicate_seeds,
                                        methods) {
  function_contamination_size <- if (
    condition$contamination == "none"
  ) {
    1
  } else {
    condition$contamination_size
  }

  result <- suppressWarnings(
    suppressMessages(
      pwr_func_study2(
        n_clusters = condition$n_clusters,
        cluster_size = condition$cluster_size,
        beta = condition$beta,
        intercept = condition$intercept,
        random_intercept_sd =
          condition$random_intercept_sd,
        random_slope_sd =
          condition$random_slope_sd,
        residual_sd = condition$residual_sd,
        x_sd = condition$x_sd,
        contamination = condition$contamination,
        contamination_prop =
          condition$contamination_prop,
        contamination_size =
          function_contamination_size,
        reps = length(replicate_seeds),
        alpha = condition$alpha,
        methods = methods,
        seed = NULL,
        replicate_seeds = replicate_seeds,
        keep_replicates = TRUE
      )
    )
  )

  result$replicates <-
    definitive_offset_shard_replicates(
      result$replicates,
      shard_row$replicate_start
    )

  result
}


definitive_run_shard_checkpoint <- function(study,
                                            condition,
                                            shard_row,
                                            replicate_seed_vector,
                                            methods,
                                            shard_dir,
                                            minimum_free_gb = 2.0,
                                            overwrite_completed = FALSE) {
  study <- match.arg(
    study,
    c("study1", "study2")
  )

  rep_ids <- seq.int(
    shard_row$replicate_start,
    shard_row$replicate_end
  )

  shard_seeds <- as.integer(
    replicate_seed_vector[rep_ids]
  )

  checkpoint_path <-
    definitive_shard_checkpoint_path(
      shard_dir = shard_dir,
      condition_id =
        condition$condition_id,
      shard_id = shard_row$shard_id
    )

  existing <- definitive_read_checkpoint(
    checkpoint_path
  )

  if (!is.null(existing) &&
      !overwrite_completed) {
    if (definitive_validate_complete_checkpoint(
      checkpoint = existing,
      condition_id = condition$condition_id,
      shard_row = shard_row,
      expected_seeds = shard_seeds,
      expected_methods = methods
    )) {
      return(list(
        action = "skipped",
        path = checkpoint_path,
        checkpoint = existing
      ))
    }

    matching_error <-
      definitive_checkpoint_spec_matches(
        checkpoint = existing,
        condition_id = condition$condition_id,
        shard_row = shard_row,
        expected_seeds = shard_seeds,
        expected_methods = methods
      ) &&
      identical(existing$status, "error")

    if (!matching_error) {
      stop(
        paste(
          "An existing shard checkpoint is present but does not match",
          "the current frozen shard specification:",
          checkpoint_path
        ),
        call. = FALSE
      )
    }
  }

  definitive_disk_guard(
    path = shard_dir,
    minimum_free_gb =
      minimum_free_gb
  )

  started_at <- Sys.time()

  result <- tryCatch(
    {
      if (study == "study1") {
        definitive_run_study1_shard(
          condition = condition,
          shard_row = shard_row,
          replicate_seeds = shard_seeds,
          methods = methods
        )
      } else {
        definitive_run_study2_shard(
          condition = condition,
          shard_row = shard_row,
          replicate_seeds = shard_seeds,
          methods = methods
        )
      }
    },
    error = function(e) e
  )

  completed_at <- Sys.time()
  elapsed_sec <- as.numeric(
    difftime(
      completed_at,
      started_at,
      units = "secs"
    )
  )

  if (inherits(result, "error")) {
    checkpoint <- list(
      status = "error",
      study = study,
      condition_id =
        as.character(condition$condition_id),
      shard_id =
        as.character(shard_row$shard_id),
      replicate_start =
        as.integer(shard_row$replicate_start),
      replicate_end =
        as.integer(shard_row$replicate_end),
      replicate_seeds = shard_seeds,
      methods = methods,
      replicates = NULL,
      error = conditionMessage(result),
      started_at = started_at,
      completed_at = completed_at,
      elapsed_sec = elapsed_sec
    )

    definitive_save_rds_atomic(
      checkpoint,
      checkpoint_path
    )

    return(
      list(
        action = "error",
        path = checkpoint_path,
        checkpoint = checkpoint
      )
    )
  }

  checkpoint <- list(
    status = "complete",
    study = study,
    condition_id =
      as.character(condition$condition_id),
    shard_id =
      as.character(shard_row$shard_id),
    replicate_start =
      as.integer(shard_row$replicate_start),
    replicate_end =
      as.integer(shard_row$replicate_end),
    replicate_seeds = shard_seeds,
    methods = methods,
    replicates = result$replicates,
    settings = result$settings,
    error = NA_character_,
    started_at = started_at,
    completed_at = completed_at,
    elapsed_sec = elapsed_sec
  )

  definitive_save_rds_atomic(
    checkpoint,
    checkpoint_path
  )

  list(
    action = "completed",
    path = checkpoint_path,
    checkpoint = checkpoint
  )
}


definitive_collect_condition_shards <- function(condition,
                                                shard_plan,
                                                replicate_seed_vector,
                                                methods,
                                                shard_dir) {
  checkpoints <- vector(
    "list",
    nrow(shard_plan)
  )

  status_rows <- vector(
    "list",
    nrow(shard_plan)
  )

  for (i in seq_len(nrow(shard_plan))) {
    shard_row <- shard_plan[
      i,
      ,
      drop = FALSE
    ]

    rep_ids <- seq.int(
      shard_row$replicate_start,
      shard_row$replicate_end
    )

    expected_seeds <- as.integer(
      replicate_seed_vector[rep_ids]
    )

    path <- definitive_shard_checkpoint_path(
      shard_dir = shard_dir,
      condition_id =
        condition$condition_id,
      shard_id = shard_row$shard_id
    )

    checkpoint <- definitive_read_checkpoint(
      path
    )

    valid <- !is.null(checkpoint) &&
      definitive_validate_complete_checkpoint(
        checkpoint = checkpoint,
        condition_id =
          condition$condition_id,
        shard_row = shard_row,
        expected_seeds = expected_seeds,
        expected_methods = methods
      )

    status_rows[[i]] <- data.frame(
      condition_id =
        condition$condition_id,
      shard_id = shard_row$shard_id,
      shard_index =
        shard_row$shard_index,
      replicate_start =
        shard_row$replicate_start,
      replicate_end =
        shard_row$replicate_end,
      status = if (valid) {
        "complete"
      } else if (is.null(checkpoint)) {
        "not_started"
      } else if (inherits(
        checkpoint,
        "definitive_unreadable_checkpoint"
      )) {
        "unreadable"
      } else if (is.list(checkpoint) &&
                 identical(
                   checkpoint$status,
                   "error"
                 )) {
        "error"
      } else {
        "invalid"
      },
      elapsed_sec = if (
        is.list(checkpoint) &&
        !inherits(
          checkpoint,
          "definitive_unreadable_checkpoint"
        ) &&
        !is.null(checkpoint$elapsed_sec)
      ) {
        checkpoint$elapsed_sec
      } else {
        NA_real_
      },
      error = if (
        is.list(checkpoint) &&
        !inherits(
          checkpoint,
          "definitive_unreadable_checkpoint"
        ) &&
        !is.null(checkpoint$error)
      ) {
        checkpoint$error
      } else if (inherits(
        checkpoint,
        "definitive_unreadable_checkpoint"
      )) {
        checkpoint$error
      } else {
        NA_character_
      },
      stringsAsFactors = FALSE
    )

    if (valid) {
      checkpoints[[i]] <- checkpoint
    }
  }

  status <- do.call(
    rbind,
    status_rows
  )
  rownames(status) <- NULL

  complete <- all(
    status$status == "complete"
  )

  replicates <- if (complete) {
    do.call(
      rbind,
      lapply(
        checkpoints,
        function(x) x$replicates
      )
    )
  } else {
    NULL
  }

  if (!is.null(replicates)) {
    rownames(replicates) <- NULL
  }

  list(
    complete = complete,
    status = status,
    checkpoints = checkpoints,
    replicates = replicates
  )
}
