# Internal helpers for the prospective Study 1/2 freeze and registration gate.
#
# This file does not fit models and does not run simulation replications. Its
# purpose is to capture, validate, and later verify the exact manuscript-version
# source state, frozen Study 1/2 designs, deterministic seeds, sharding rules,
# software environment, and external registration record.

study12f_find_project_root <- function(path = getwd()) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

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


study12f_default_freeze_dir <- function(project_root) {
  file.path(
    project_root,
    "data-raw",
    "study12-results",
    "pre-results-freeze"
  )
}


study12f_default_protocol_path <- function(project_root) {
  file.path(
    project_root,
    "data-raw",
    "study12_definitive_protocol_approved_20260902.txt"
  )
}


study12f_normalize_root <- function(project_root) {
  normalizePath(
    project_root,
    winslash = "/",
    mustWork = TRUE
  )
}


study12f_relative_path <- function(paths,
                                   project_root) {
  normalize_allow_missing <- function(path) {
    path <- path.expand(path)

    if (file.exists(path) || dir.exists(path)) {
      return(
        normalizePath(
          path,
          winslash = "/",
          mustWork = TRUE
        )
      )
    }

    parent <- dirname(path)

    if (identical(parent, path)) {
      return(
        gsub(
          "\\\\",
          "/",
          path
        )
      )
    }

    paste0(
      normalize_allow_missing(parent),
      "/",
      basename(path)
    )
  }

  normalized_paths <- vapply(
    paths,
    normalize_allow_missing,
    FUN.VALUE = character(1)
  )

  normalized_root <- study12f_normalize_root(
    project_root
  )
  prefix <- paste0(normalized_root, "/")

  if (!all(startsWith(normalized_paths, prefix))) {
    stop(
      "All recorded paths must be inside the project root.",
      call. = FALSE
    )
  }

  unname(
    substring(
      normalized_paths,
      nchar(prefix) + 1L
    )
  )
}


study12f_write_csv_atomic <- function(data,
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
    stop(
      paste(
        "Refusing to overwrite frozen Study 1/2 artifact:",
        path
      ),
      call. = FALSE
    )
  }

  if (!file.rename(temp_path, path)) {
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


study12f_save_rds_atomic <- function(object,
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

  saveRDS(
    object,
    temp_path,
    version = 3,
    compress = "gzip"
  )

  # Verify readability before the final rename.
  readRDS(temp_path)

  if (file.exists(path)) {
    stop(
      paste(
        "Refusing to overwrite frozen Study 1/2 artifact:",
        path
      ),
      call. = FALSE
    )
  }

  if (!file.rename(temp_path, path)) {
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


study12f_write_lines_atomic <- function(text,
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

  writeLines(
    text,
    con = temp_path,
    useBytes = TRUE
  )

  if (file.exists(path)) {
    stop(
      paste(
        "Refusing to overwrite frozen Study 1/2 artifact:",
        path
      ),
      call. = FALSE
    )
  }

  if (!file.rename(temp_path, path)) {
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


study12f_copy_atomic <- function(from,
                                 to) {
  if (!file.exists(from)) {
    stop(
      paste(
        "Source file does not exist:",
        from
      ),
      call. = FALSE
    )
  }

  dir.create(
    dirname(to),
    recursive = TRUE,
    showWarnings = FALSE
  )

  if (file.exists(to)) {
    stop(
      paste(
        "Refusing to overwrite frozen Study 1/2 artifact:",
        to
      ),
      call. = FALSE
    )
  }

  temp_path <- tempfile(
    pattern = paste0(basename(to), "_"),
    tmpdir = dirname(to),
    fileext = ".tmp"
  )

  on.exit(
    if (file.exists(temp_path)) {
      unlink(temp_path, force = TRUE)
    },
    add = TRUE
  )

  copied <- file.copy(
    from,
    temp_path,
    overwrite = FALSE,
    copy.mode = TRUE,
    copy.date = TRUE
  )

  if (!isTRUE(copied)) {
    stop(
      paste(
        "Could not copy frozen artifact:",
        from
      ),
      call. = FALSE
    )
  }

  if (!file.rename(temp_path, to)) {
    stop(
      paste(
        "Could not atomically save:",
        to
      ),
      call. = FALSE
    )
  }

  invisible(to)
}


study12f_file_md5 <- function(paths,
                              project_root) {
  paths <- normalizePath(
    paths,
    winslash = "/",
    mustWork = TRUE
  )

  data.frame(
    file = basename(paths),
    relative_path = study12f_relative_path(
      paths,
      project_root
    ),
    md5 = unname(
      tools::md5sum(paths)
    ),
    stringsAsFactors = FALSE
  )
}


study12f_run_git <- function(args,
                             project_root,
                             allow_failure = FALSE) {
  out <- suppressWarnings(
    system2(
      "git",
      c(
        "-C",
        shQuote(
          study12f_normalize_root(
            project_root
          )
        ),
        args
      ),
      stdout = TRUE,
      stderr = TRUE
    )
  )

  status <- attr(out, "status")

  if (is.null(status)) {
    status <- 0L
  }

  if (!identical(as.integer(status), 0L) &&
      !isTRUE(allow_failure)) {
    stop(
      paste(
        "Git command failed:",
        paste(args, collapse = " "),
        paste(out, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  attr(out, "status") <- as.integer(status)
  out
}


study12f_git_value <- function(args,
                               project_root,
                               allow_failure = FALSE) {
  out <- study12f_run_git(
    args = args,
    project_root = project_root,
    allow_failure = allow_failure
  )

  status <- attr(out, "status")

  if (!identical(status, 0L)) {
    return(NA_character_)
  }

  out <- trimws(out)
  out <- out[nzchar(out)]

  if (length(out) == 0L) {
    return(NA_character_)
  }

  paste(out, collapse = "\n")
}


study12f_git_record <- function(project_root,
                                require_clean = FALSE,
                                require_pushed = FALSE) {
  project_root <- study12f_normalize_root(
    project_root
  )

  git_root <- study12f_git_value(
    c("rev-parse", "--show-toplevel"),
    project_root
  )

  git_root <- normalizePath(
    git_root,
    winslash = "/",
    mustWork = TRUE
  )

  if (!identical(git_root, project_root)) {
    stop(
      paste(
        "The project root is not the Git repository root.",
        "Project root:", project_root,
        "Git root:", git_root
      ),
      call. = FALSE
    )
  }

  commit <- study12f_git_value(
    c("rev-parse", "HEAD"),
    project_root
  )

  branch <- study12f_git_value(
    c("branch", "--show-current"),
    project_root,
    allow_failure = TRUE
  )

  status_lines <- study12f_run_git(
    c(
      "status",
      "--porcelain=v1",
      "--untracked-files=all"
    ),
    project_root
  )

  clean <- length(
    trimws(status_lines)[
      nzchar(trimws(status_lines))
    ]
  ) == 0L

  upstream <- study12f_git_value(
    c(
      "rev-parse",
      "--abbrev-ref",
      "--symbolic-full-name",
      "@{u}"
    ),
    project_root,
    allow_failure = TRUE
  )

  upstream_commit <- if (
    !is.na(upstream) &&
      nzchar(upstream)
  ) {
    study12f_git_value(
      c("rev-parse", upstream),
      project_root,
      allow_failure = TRUE
    )
  } else {
    NA_character_
  }

  pushed <- !is.na(upstream_commit) &&
    identical(commit, upstream_commit)

  remote_name <- if (
    !is.na(upstream) &&
      nzchar(upstream) &&
      grepl("/", upstream, fixed = TRUE)
  ) {
    sub("/.*$", "", upstream)
  } else {
    NA_character_
  }

  remote_url <- if (
    !is.na(remote_name) &&
      nzchar(remote_name)
  ) {
    study12f_git_value(
      c(
        "config",
        "--get",
        paste0(
          "remote.",
          remote_name,
          ".url"
        )
      ),
      project_root,
      allow_failure = TRUE
    )
  } else {
    NA_character_
  }

  commit_time <- study12f_git_value(
    c(
      "show",
      "-s",
      "--format=%cI",
      "HEAD"
    ),
    project_root,
    allow_failure = TRUE
  )

  if (isTRUE(require_clean) && !clean) {
    stop(
      paste(
        "The Git worktree is not clean.",
        "Commit or intentionally remove all tracked/untracked changes",
        "before creating the prospective Study 1/2 source freeze."
      ),
      call. = FALSE
    )
  }

  if (isTRUE(require_pushed) && !pushed) {
    stop(
      paste(
        "The current Git commit is not verified as matching its upstream.",
        "Push the exact final manuscript-version source state and ensure",
        "the current branch has an upstream before creating the freeze."
      ),
      call. = FALSE
    )
  }

  data.frame(
    repository_root = project_root,
    commit = commit,
    branch = branch,
    upstream = upstream,
    upstream_commit = upstream_commit,
    pushed_to_upstream = pushed,
    worktree_clean_before_freeze = clean,
    remote_url = remote_url,
    commit_time = commit_time,
    stringsAsFactors = FALSE
  )
}


study12f_package_versions <- function(project_root) {
  package_names <- c(
    "mmiCATs",
    "lme4",
    "lmerTest",
    "pbkrtest",
    "clubSandwich",
    "clusterSEs",
    "robust",
    "robustbase",
    "robustlmm",
    "MASS",
    "ggplot2"
  )

  description <- base::read.dcf(
    file.path(
      project_root,
      "DESCRIPTION"
    )
  )

  versions <- vapply(
    package_names,
    function(package_name) {
      if (identical(
        package_name,
        "mmiCATs"
      )) {
        return(
          unname(
            description[1L, "Version"]
          )
        )
      }

      if (!requireNamespace(
        package_name,
        quietly = TRUE
      )) {
        return(NA_character_)
      }

      as.character(
        utils::packageVersion(
          package_name
        )
      )
    },
    FUN.VALUE = character(1)
  )

  missing <- package_names[
    is.na(versions) |
      !nzchar(versions)
  ]

  if (length(missing) > 0L) {
    stop(
      paste(
        "Required Study 1/2 package(s) are not available:",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  data.frame(
    package = package_names,
    version = versions,
    stringsAsFactors = FALSE
  )
}


study12f_source_paths <- function(project_root,
                                  protocol_path) {
  r_files <- sort(
    list.files(
      file.path(
        project_root,
        "R"
      ),
      pattern = "[.]R$",
      full.names = TRUE
    )
  )

  fixed_paths <- c(
    DESCRIPTION = file.path(
      project_root,
      "DESCRIPTION"
    ),
    NAMESPACE = file.path(
      project_root,
      "NAMESPACE"
    ),
    study1_wrapper = file.path(
      project_root,
      "data-raw",
      "study1_final_simulation.R"
    ),
    study2_wrapper = file.path(
      project_root,
      "data-raw",
      "study2_final_simulation.R"
    ),
    study12_freeze_wrapper = file.path(
      project_root,
      "data-raw",
      "study12_prepare_pre_results_freeze.R"
    ),
    study12_protocol = protocol_path
  )

  test_paths <- c(
    file.path(
      project_root,
      "tests",
      "testthat",
      "test-study12-freeze.R"
    ),
    file.path(
      project_root,
      "tests",
      "testthat",
      "test-manuscript-runner-architecture.R"
    )
  )

  test_paths <- test_paths[
    file.exists(test_paths)
  ]

  paths <- c(
    fixed_paths,
    stats::setNames(
      r_files,
      paste0(
        "R/",
        basename(r_files)
      )
    ),
    stats::setNames(
      test_paths,
      paste0(
        "test/",
        basename(test_paths)
      )
    )
  )

  missing <- names(paths)[
    !file.exists(paths)
  ]

  if (length(missing) > 0L) {
    stop(
      paste(
        "Required source file(s) are missing:",
        paste(
          missing,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  paths
}


study12f_source_checksums <- function(project_root,
                                      protocol_path) {
  paths <- study12f_source_paths(
    project_root = project_root,
    protocol_path = protocol_path
  )

  data.frame(
    source = names(paths),
    relative_path = study12f_relative_path(
      paths,
      project_root
    ),
    md5 = unname(
      tools::md5sum(paths)
    ),
    stringsAsFactors = FALSE
  )
}


study12f_method_schedule <- function() {
  study1_methods <- c(
    "ri",
    "cr2",
    "cats",
    "cats_trunc",
    "cats_robust",
    "cats_robustbase",
    "robust_ri"
  )

  study2_methods <- c(
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

  rbind(
    data.frame(
      study = "Study 1",
      method_order = seq_along(
        study1_methods
      ),
      method = study1_methods,
      stringsAsFactors = FALSE
    ),
    data.frame(
      study = "Study 2",
      method_order = seq_along(
        study2_methods
      ),
      method = study2_methods,
      stringsAsFactors = FALSE
    )
  )
}


study12f_seed_blocks <- function() {
  rbind(
    data.frame(
      study = "Study 1",
      n_clusters = c(
        10L,
        20L,
        40L
      ),
      condition_seed = c(
        20260815L,
        20260816L,
        20260817L
      ),
      total_reps = 2000L,
      stringsAsFactors = FALSE
    ),
    data.frame(
      study = "Study 2",
      n_clusters = c(
        10L,
        20L,
        40L
      ),
      condition_seed = c(
        20260905L,
        20260906L,
        20260907L
      ),
      total_reps = 2000L,
      stringsAsFactors = FALSE
    )
  )
}


study12f_preserve_rng <- function(code) {
  rng_kind_before <- RNGkind()

  had_random_seed <- exists(
    ".Random.seed",
    envir = .GlobalEnv,
    inherits = FALSE
  )

  if (had_random_seed) {
    random_seed_before <- get(
      ".Random.seed",
      envir = .GlobalEnv,
      inherits = FALSE
    )
  }

  on.exit(
    {
      do.call(
        RNGkind,
        as.list(
          rng_kind_before
        )
      )

      if (had_random_seed) {
        assign(
          ".Random.seed",
          random_seed_before,
          envir = .GlobalEnv
        )
      } else if (exists(
        ".Random.seed",
        envir = .GlobalEnv,
        inherits = FALSE
      )) {
        rm(
          ".Random.seed",
          envir = .GlobalEnv
        )
      }
    },
    add = TRUE
  )

  eval(
    substitute(code),
    envir = parent.frame()
  )
}


study12f_reference_replicate_seeds <- function(condition_seed,
                                                total_reps) {
  study12f_preserve_rng(
    {
      set.seed(
        as.integer(
          condition_seed
        )
      )

      sample.int(
        .Machine$integer.max,
        size = as.integer(
          total_reps
        ),
        replace = FALSE
      )
    }
  )
}


study12f_replicate_seed_table <- function() {
  blocks <- study12f_seed_blocks()

  study12f_preserve_rng(
    {
      out <- lapply(
        seq_len(
          nrow(blocks)
        ),
        function(i) {
          seeds <- definitive_make_replicate_seeds(
            condition_seed =
              blocks$condition_seed[i],
            total_reps =
              blocks$total_reps[i]
          )

          data.frame(
            study = blocks$study[i],
            n_clusters =
              blocks$n_clusters[i],
            condition_seed =
              blocks$condition_seed[i],
            replicate =
              seq_len(
                blocks$total_reps[i]
              ),
            replicate_seed =
              as.integer(seeds),
            stringsAsFactors = FALSE
          )
        }
      )

      seeds <- do.call(
        rbind,
        out
      )
      rownames(seeds) <- NULL
      seeds
    }
  )
}


study12f_shard_plan <- function() {
  definitive_make_shard_plan(
    total_reps = 2000L,
    shard_size = 10L
  )
}


study12f_scientific_checks <- function(study1_design,
                                       study2_design,
                                       method_schedule,
                                       seed_blocks,
                                       replicate_seeds,
                                       shard_plan) {
  checks <- list()

  add_check <- function(check,
                        passed,
                        details) {
    checks[[length(checks) + 1L]] <<-
      data.frame(
        check = check,
        passed = isTRUE(passed),
        details = details,
        stringsAsFactors = FALSE
      )
  }

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

  observed_study1_methods <-
    method_schedule$method[
      method_schedule$study ==
        "Study 1"
    ]

  observed_study2_methods <-
    method_schedule$method[
      method_schedule$study ==
        "Study 2"
    ]

  add_check(
    "study1_condition_count",
    nrow(study1_design) == 18L,
    paste(
      nrow(study1_design),
      "conditions; expected 18"
    )
  )

  add_check(
    "study2_condition_count",
    nrow(study2_design) == 24L,
    paste(
      nrow(study2_design),
      "conditions; expected 24"
    )
  )

  add_check(
    "study1_condition_ids",
    identical(
      as.character(
        study1_design$condition_id
      ),
      sprintf(
        "S1C%03d",
        seq_len(18L)
      )
    ),
    "Study 1 IDs must be S1C001-S1C018 in frozen order."
  )

  add_check(
    "study2_condition_ids",
    identical(
      as.character(
        study2_design$condition_id
      ),
      sprintf(
        "S2C%03d",
        seq_len(24L)
      )
    ),
    "Study 2 IDs must be S2C001-S2C024 in frozen order."
  )

  add_check(
    "study1_method_order",
    identical(
      observed_study1_methods,
      expected_study1_methods
    ) &&
      identical(
        study1d_methods(),
        expected_study1_methods
      ),
    paste(
      expected_study1_methods,
      collapse = ","
    )
  )

  add_check(
    "study2_method_order",
    identical(
      observed_study2_methods,
      expected_study2_methods
    ) &&
      identical(
        study2d_methods(),
        expected_study2_methods
      ),
    paste(
      expected_study2_methods,
      collapse = ","
    )
  )

  add_check(
    "study1_core_design",
    setequal(
      unique(
        study1_design$n_clusters
      ),
      c(10L, 20L, 40L)
    ) &&
      setequal(
        unique(
          study1_design$beta
        ),
        c(0, 0.10)
      ) &&
      setequal(
        unique(
          study1_design$contamination
        ),
        c(
          "none",
          "vertical",
          "bad_leverage"
        )
      ) &&
      all(
        study1_design$cluster_size ==
          40L
      ) &&
      all(
        study1_design$random_intercept_sd ==
          1
      ) &&
      all(
        study1_design$residual_sd ==
          1
      ) &&
      all(
        study1_design$x_sd ==
          1
      ) &&
      all(
        study1_design$contamination_prop ==
          0.05
      ) &&
      all(
        study1_design$reps ==
          2000L
      ),
    paste(
      "G=10/20/40; m=40; beta=0/.10;",
      "RI SD=1; residual SD=1; x SD=1;",
      "none/vertical/bad_leverage; 5%; 2000 reps."
    )
  )

  add_check(
    "study1_contamination_sizes",
    identical(
      unique(
        study1_design$contamination_size[
          study1_design$contamination ==
            "vertical"
        ]
      ),
      6
    ) &&
      identical(
        unique(
          study1_design$leverage_size[
            study1_design$contamination ==
              "bad_leverage"
          ]
        ),
        4
      ) &&
      identical(
        unique(
          study1_design$contamination_size[
            study1_design$contamination ==
              "bad_leverage"
          ]
        ),
        0.375
      ),
    paste(
      "Vertical outcome displacement=6;",
      "bad-leverage x size=4 and outcome size=.375."
    )
  )

  add_check(
    "study2_core_design",
    setequal(
      unique(
        study2_design$n_clusters
      ),
      c(10L, 20L, 40L)
    ) &&
      setequal(
        unique(
          study2_design$beta
        ),
        c(0, 0.10)
      ) &&
      setequal(
        unique(
          study2_design$random_slope_sd
        ),
        c(0.05, 0.10)
      ) &&
      setequal(
        unique(
          study2_design$contamination
        ),
        c(
          "none",
          "vertical"
        )
      ) &&
      all(
        study2_design$cluster_size ==
          40L
      ) &&
      all(
        study2_design$random_intercept_sd ==
          1
      ) &&
      all(
        study2_design$residual_sd ==
          1
      ) &&
      all(
        study2_design$x_sd ==
          1
      ) &&
      all(
        study2_design$contamination_prop ==
          0.05
      ) &&
      all(
        study2_design$reps ==
          2000L
      ),
    paste(
      "G=10/20/40; m=40; beta=0/.10;",
      "RS SD=.05/.10; RI SD=1; residual SD=1; x SD=1;",
      "none/vertical; 5%; 2000 reps."
    )
  )

  add_check(
    "study2_vertical_contamination_size",
    identical(
      unique(
        study2_design$contamination_size[
          study2_design$contamination ==
            "vertical"
        ]
      ),
      6
    ),
    "Study 2 vertical outcome displacement must equal 6 residual SD."
  )

  add_check(
    "study2_minimum_usable_reps",
    identical(
      study2d_frozen_config()$
        minimum_usable_reps,
      1900L
    ),
    "Study 2 adequacy threshold must remain 1900 of 2000 usable fits."
  )

  expected_seed_blocks <-
    study12f_seed_blocks()

  add_check(
    "condition_seed_blocks",
    identical(
      seed_blocks,
      expected_seed_blocks
    ),
    paste(
      "Study 1: 20260815/16/17;",
      "Study 2: 20260905/06/07."
    )
  )

  study1_seed_map <- tapply(
    study1_design$condition_seed,
    study1_design$n_clusters,
    unique
  )

  study2_seed_map <- tapply(
    study2_design$condition_seed,
    study2_design$n_clusters,
    unique
  )

  add_check(
    "design_condition_seed_blocks",
    identical(
      as.integer(
        study1_seed_map
      ),
      c(
        20260815L,
        20260816L,
        20260817L
      )
    ) &&
      identical(
        as.integer(
          study2_seed_map
        ),
        c(
          20260905L,
          20260906L,
          20260907L
        )
      ),
    "Frozen designs must preserve the six prespecified cluster-count seed blocks."
  )

  seed_split <- split(
    replicate_seeds,
    interaction(
      replicate_seeds$study,
      replicate_seeds$n_clusters,
      drop = TRUE
    )
  )

  seed_vector_checks <- vapply(
    seed_split,
    function(x) {
      expected <-
        study12f_reference_replicate_seeds(
          condition_seed =
            x$condition_seed[1L],
          total_reps =
            nrow(x)
        )

      identical(
        as.integer(
          x$replicate_seed
        ),
        as.integer(
          expected
        )
      )
    },
    logical(1)
  )

  add_check(
    "exact_2000_rep_seed_vectors",
    nrow(replicate_seeds) ==
      6L * 2000L &&
      length(seed_split) == 6L &&
      all(
        vapply(
          seed_split,
          nrow,
          integer(1)
        ) == 2000L
      ) &&
      all(seed_vector_checks),
    paste(
      "Six exact 2000-rep seed vectors must equal",
      "set.seed(condition_seed); sample.int(.Machine$integer.max, 2000, replace=FALSE)."
    )
  )

  add_check(
    "replicate_seeds_unique_within_block",
    all(
      vapply(
        seed_split,
        function(x) {
          anyDuplicated(
            x$replicate_seed
          ) == 0L
        },
        logical(1)
      )
    ),
    "Replication seeds must be unique within each 2000-rep cluster-count block."
  )

  add_check(
    "shard_plan",
    nrow(shard_plan) == 200L &&
      all(
        shard_plan$shard_reps ==
          10L
      ) &&
      identical(
        shard_plan$replicate_start,
        seq.int(
          1L,
          1991L,
          by = 10L
        )
      ) &&
      identical(
        shard_plan$replicate_end,
        seq.int(
          10L,
          2000L,
          by = 10L
        )
      ),
    "200 deterministic shards of exactly 10 replications."
  )

  out <- do.call(
    rbind,
    checks
  )
  rownames(out) <- NULL
  out
}


study12f_definitive_output_dirs <- function(project_root) {
  c(
    study1 = file.path(
      project_root,
      "data-raw",
      "study1-results",
      "definitive-study"
    ),
    study2 = file.path(
      project_root,
      "data-raw",
      "study2-results",
      "definitive-study"
    )
  )
}


study12f_definitive_outputs_absent <- function(project_root) {
  paths <- study12f_definitive_output_dirs(
    project_root
  )

  vapply(
    paths,
    function(path) {
      if (!dir.exists(path)) {
        return(TRUE)
      }

      length(
        list.files(
          path,
          all.files = TRUE,
          no.. = TRUE
        )
      ) == 0L
    },
    logical(1)
  )
}


study12f_validate_registration_location <- function(registration_location) {
  if (!is.character(registration_location) ||
      length(registration_location) != 1L ||
      is.na(registration_location) ||
      !nzchar(trimws(registration_location))) {
    stop(
      "registration_location must be one non-empty character string.",
      call. = FALSE
    )
  }

  bad_placeholder <- grepl(
    "tbd|todo|pending|placeholder|replace[ _-]*me|paste[ _-]*link",
    registration_location,
    ignore.case = TRUE
  )

  if (bad_placeholder) {
    stop(
      "registration_location still looks like a placeholder.",
      call. = FALSE
    )
  }

  trimws(registration_location)
}


study12f_required_freeze_files <- function() {
  c(
    "FREEZE_COMPLETE.txt",
    "study1_frozen_design.csv",
    "study1_frozen_design.rds",
    "study2_frozen_design.csv",
    "study2_frozen_design.rds",
    "study12_method_schedule.csv",
    "study12_seed_blocks.csv",
    "study12_replicate_seeds.csv",
    "study12_replicate_seeds.rds",
    "study12_shard_plan.csv",
    "study12_shard_plan.rds",
    "study12_git_record.csv",
    "study12_source_checksums.csv",
    "study12_package_versions.csv",
    "study12_rng_record.rds",
    "study12_scientific_checks.csv",
    "study12_freeze_metadata.csv",
    "study12_registration_manifest.txt",
    "study12_definitive_protocol.txt",
    "study12_frozen_artifact_checksums.csv",
    "study12_freeze_record.rds",
    "session_info.txt"
  )
}


study12f_verify_checksum_record <- function(checksum_path,
                                            project_root) {
  if (!file.exists(checksum_path)) {
    stop(
      paste(
        "Checksum record is missing:",
        checksum_path
      ),
      call. = FALSE
    )
  }

  recorded <- utils::read.csv(
    checksum_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  required_columns <- c(
    "file",
    "relative_path",
    "md5"
  )

  if (!all(
    required_columns %in%
      names(recorded)
  )) {
    stop(
      paste(
        "Checksum record has an invalid schema:",
        checksum_path
      ),
      call. = FALSE
    )
  }

  paths <- file.path(
    project_root,
    recorded$relative_path
  )

  missing <- recorded$relative_path[
    !file.exists(paths)
  ]

  if (length(missing) > 0L) {
    stop(
      paste(
        "Frozen Study 1/2 artifact(s) are missing:",
        paste(
          missing,
          collapse = "; "
        )
      ),
      call. = FALSE
    )
  }

  current <- unname(
    tools::md5sum(paths)
  )

  if (!identical(
    current,
    recorded$md5
  )) {
    mismatch <- recorded$file[
      current != recorded$md5
    ]

    stop(
      paste(
        "Frozen Study 1/2 artifact checksum mismatch:",
        paste(
          mismatch,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  invisible(recorded)
}


study12f_verify_source_checksums <- function(source_path,
                                             project_root) {
  recorded <- utils::read.csv(
    source_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  paths <- file.path(
    project_root,
    recorded$relative_path
  )

  missing <- recorded$relative_path[
    !file.exists(paths)
  ]

  if (length(missing) > 0L) {
    stop(
      paste(
        "Frozen source file(s) are missing:",
        paste(
          missing,
          collapse = "; "
        )
      ),
      call. = FALSE
    )
  }

  current <- unname(
    tools::md5sum(paths)
  )

  if (!identical(
    current,
    recorded$md5
  )) {
    mismatch <- recorded$source[
      current != recorded$md5
    ]

    stop(
      paste(
        "Current source differs from the prospective Study 1/2 freeze:",
        paste(
          mismatch,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  invisible(recorded)
}


study12f_verify_package_versions <- function(package_path,
                                             project_root) {
  frozen <- utils::read.csv(
    package_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  current <- study12f_package_versions(
    project_root
  )

  current <- current[
    match(
      frozen$package,
      current$package
    ),
    ,
    drop = FALSE
  ]

  if (!identical(
    unname(current$version),
    unname(frozen$version)
  )) {
    bad <- frozen$package[
      is.na(current$version) |
        current$version !=
          frozen$version
    ]

    stop(
      paste(
        "Package versions differ from the prospective Study 1/2 freeze:",
        paste(
          bad,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  invisible(frozen)
}


study12f_verify_freeze <- function(
    project_root = NULL,
    freeze_dir = NULL,
    verify_current_source = TRUE,
    verify_current_git = TRUE,
    verify_package_versions = TRUE,
    verify_rng = TRUE) {
  if (is.null(project_root)) {
    project_root <- study12f_find_project_root()
  }

  project_root <- study12f_normalize_root(
    project_root
  )

  if (is.null(freeze_dir)) {
    freeze_dir <- study12f_default_freeze_dir(
      project_root
    )
  }

  if (!dir.exists(freeze_dir)) {
    stop(
      paste(
        "Prospective Study 1/2 freeze directory does not exist:",
        freeze_dir
      ),
      call. = FALSE
    )
  }

  freeze_dir <- normalizePath(
    freeze_dir,
    winslash = "/",
    mustWork = TRUE
  )

  required <- study12f_required_freeze_files()
  required_paths <- file.path(
    freeze_dir,
    required
  )

  missing <- required[
    !file.exists(required_paths)
  ]

  if (length(missing) > 0L) {
    stop(
      paste(
        "Prospective Study 1/2 freeze is incomplete; missing:",
        paste(
          missing,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  study12f_verify_checksum_record(
    file.path(
      freeze_dir,
      "study12_frozen_artifact_checksums.csv"
    ),
    project_root
  )

  completion <- readLines(
    file.path(
      freeze_dir,
      "FREEZE_COMPLETE.txt"
    ),
    warn = FALSE
  )

  freeze_record_line <- grep(
    "^Freeze record MD5:",
    completion,
    value = TRUE
  )

  if (length(freeze_record_line) != 1L) {
    stop(
      "FREEZE_COMPLETE.txt does not contain exactly one freeze-record MD5.",
      call. = FALSE
    )
  }

  expected_freeze_record_md5 <- trimws(
    sub(
      "^Freeze record MD5:",
      "",
      freeze_record_line
    )
  )

  freeze_record_path <- file.path(
    freeze_dir,
    "study12_freeze_record.rds"
  )

  current_freeze_record_md5 <- unname(
    tools::md5sum(
      freeze_record_path
    )
  )

  if (!identical(
    current_freeze_record_md5,
    expected_freeze_record_md5
  )) {
    stop(
      "Prospective Study 1/2 freeze-record checksum mismatch.",
      call. = FALSE
    )
  }

  checks <- utils::read.csv(
    file.path(
      freeze_dir,
      "study12_scientific_checks.csv"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (!all(
    checks$passed %in% TRUE
  )) {
    stop(
      "At least one frozen Study 1/2 scientific-design check is not passed.",
      call. = FALSE
    )
  }

  if (isTRUE(
    verify_current_source
  )) {
    study12f_verify_source_checksums(
      file.path(
        freeze_dir,
        "study12_source_checksums.csv"
      ),
      project_root
    )
  }

  git_record <- utils::read.csv(
    file.path(
      freeze_dir,
      "study12_git_record.csv"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (isTRUE(
    verify_current_git
  )) {
    current_commit <- study12f_git_value(
      c("rev-parse", "HEAD"),
      project_root
    )

    if (!identical(
      current_commit,
      git_record$commit[1L]
    )) {
      stop(
        paste(
          "Current Git HEAD differs from the prospective Study 1/2 freeze.",
          "Frozen:", git_record$commit[1L],
          "Current:", current_commit
        ),
        call. = FALSE
      )
    }
  }

  if (isTRUE(
    verify_package_versions
  )) {
    study12f_verify_package_versions(
      file.path(
        freeze_dir,
        "study12_package_versions.csv"
      ),
      project_root
    )
  }

  if (isTRUE(
    verify_rng
  )) {
    rng_record <- readRDS(
      file.path(
        freeze_dir,
        "study12_rng_record.rds"
      )
    )

    if (!identical(
      RNGkind(),
      rng_record$rng_kind
    )) {
      stop(
        "Current RNGkind() differs from the prospective Study 1/2 freeze.",
        call. = FALSE
      )
    }
  }

  invisible(
    readRDS(
      freeze_record_path
    )
  )
}


study12f_verify_registration <- function(freeze_dir) {
  registration_csv <- file.path(
    freeze_dir,
    "study12_registration_record.csv"
  )
  registration_marker <- file.path(
    freeze_dir,
    "REGISTRATION_COMPLETE.txt"
  )

  if (!file.exists(
    registration_csv
  ) ||
      !file.exists(
        registration_marker
      )) {
    stop(
      paste(
        "The prospective Study 1/2 source freeze exists, but the",
        "external pre-results registration has not been recorded.",
        "Do not run definitive Study 1 or Study 2."
      ),
      call. = FALSE
    )
  }

  completion <- readLines(
    registration_marker,
    warn = FALSE
  )

  record_line <- grep(
    "^Registration record MD5:",
    completion,
    value = TRUE
  )

  if (length(record_line) != 1L) {
    stop(
      "REGISTRATION_COMPLETE.txt does not contain exactly one registration-record MD5.",
      call. = FALSE
    )
  }

  expected_md5 <- trimws(
    sub(
      "^Registration record MD5:",
      "",
      record_line
    )
  )

  current_md5 <- unname(
    tools::md5sum(
      registration_csv
    )
  )

  if (!identical(
    current_md5,
    expected_md5
  )) {
    stop(
      "Prospective Study 1/2 registration-record checksum mismatch.",
      call. = FALSE
    )
  }

  registration <- utils::read.csv(
    registration_csv,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  study12f_validate_registration_location(
    registration$registration_location[1L]
  )

  invisible(registration)
}


study12f_verify_gate <- function(
    project_root = NULL,
    freeze_dir = NULL,
    verify_current_source = TRUE,
    verify_current_git = TRUE,
    verify_package_versions = TRUE,
    verify_rng = TRUE) {
  if (is.null(project_root)) {
    project_root <- study12f_find_project_root()
  }

  project_root <- study12f_normalize_root(
    project_root
  )

  if (is.null(freeze_dir)) {
    freeze_dir <- study12f_default_freeze_dir(
      project_root
    )
  }

  freeze_record <- study12f_verify_freeze(
    project_root = project_root,
    freeze_dir = freeze_dir,
    verify_current_source =
      verify_current_source,
    verify_current_git =
      verify_current_git,
    verify_package_versions =
      verify_package_versions,
    verify_rng = verify_rng
  )

  registration <- study12f_verify_registration(
    freeze_dir
  )

  invisible(
    list(
      freeze = freeze_record,
      registration = registration
    )
  )
}
