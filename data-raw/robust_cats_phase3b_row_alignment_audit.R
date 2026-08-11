# Robust CATs audit: Phase 3B row-name/model-frame alignment
#
# Purpose:
#   Investigate A-08 without changing production code.
#
#   info() identifies observations used by the full robust model with:
#
#     dat[which(rownames(dat) %in% rownames(robmod$model)), ]
#
#   This audit tests whether that logic remains correct after harmless row
#   reordering, with and without preserving row names, when the fitted model
#   omitted observations because of missing values.
#
# This script does not modify production source files.

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
  "phase3b-row-alignment"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

phase3b_make_data <- function(seed = 20265001L,
                              include_missing = TRUE) {
  set.seed(seed)

  n_clusters <- 6L
  cluster_size <- 20L

  cluster <- factor(
    rep(
      seq_len(n_clusters),
      each = cluster_size
    ),
    levels = seq_len(n_clusters)
  )

  x <- stats::rnorm(
    n_clusters * cluster_size
  )
  random_intercept <- rep(
    stats::rnorm(
      n_clusters,
      mean = 0,
      sd = 0.8
    ),
    each = cluster_size
  )
  residual <- stats::rnorm(
    n_clusters * cluster_size,
    mean = 0,
    sd = 0.7
  )

  dat <- data.frame(
    row_id = seq_along(x),
    cluster = cluster,
    x = x,
    out = 0.3 +
      0.25 * x +
      random_intercept +
      residual
  )

  if (isTRUE(include_missing)) {
    dat$x[c(5L, 47L, 88L)] <- NA_real_
    dat$out[c(22L, 69L)] <- NA_real_
  }

  dat
}

phase3b_fit_full <- function(dat,
                             engine) {
  switch(
    engine,
    "robust" = robust::lmRob(
      out ~ x,
      data = dat,
      na.action = stats::na.omit
    ),
    "robustbase" = robustbase::lmrob(
      out ~ x,
      data = dat,
      na.action = stats::na.omit
    ),
    stop(
      "Unknown robust engine.",
      call. = FALSE
    )
  )
}

phase3b_active_row_ids <- function(fit,
                                   original_dat) {
  model_rows <- rownames(fit$model)

  row_positions <- match(
    model_rows,
    rownames(original_dat)
  )

  if (anyNA(row_positions)) {
    return(integer(0))
  }

  original_dat$row_id[row_positions]
}

phase3b_capture_public <- function(fit,
                                   dat,
                                   engine) {
  rca_capture(
    cluster_im_lmRob(
      robmod = fit,
      formula = out ~ x,
      dat = dat,
      cluster = ~ cluster,
      ci.level = 0.95,
      drop = TRUE,
      return.vcv = TRUE,
      engine = engine
    )
  )
}

phase3b_extract_public <- function(captured) {
  if (is.null(captured$value)) {
    return(c(
      estimate = NA_real_,
      p_value = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_
    ))
  }

  c(
    estimate = unname(
      captured$value$beta.bar["x"]
    ),
    p_value = unname(
      captured$value$p.values["x", 1L]
    ),
    conf_low = unname(
      captured$value$ci["x", 1L]
    ),
    conf_high = unname(
      captured$value$ci["x", 2L]
    )
  )
}

alignment_rows <- list()
public_rows <- list()
row_index <- 0L
public_index <- 0L

for (engine in c("robust", "robustbase")) {
  dat <- phase3b_make_data(
    seed = 20265001L,
    include_missing = TRUE
  )

  fit_capture <- rca_capture(
    phase3b_fit_full(
      dat = dat,
      engine = engine
    )
  )

  if (is.null(fit_capture$value)) {
    stop(
      paste(
        "The Phase 3B full-data fit failed for",
        engine,
        ":",
        fit_capture$error
      ),
      call. = FALSE
    )
  }

  fit <- fit_capture$value

  reference_ids <- phase3b_active_row_ids(
    fit = fit,
    original_dat = dat
  )

  baseline_info <- info(
    formula = out ~ x,
    cluster = ~ cluster,
    dat = dat,
    robmod = fit
  )

  set.seed(20265002L)
  shuffled_index <- sample(
    seq_len(nrow(dat))
  )

  shuffled_preserved <- dat[
    shuffled_index,
    ,
    drop = FALSE
  ]

  preserved_info <- info(
    formula = out ~ x,
    cluster = ~ cluster,
    dat = shuffled_preserved,
    robmod = fit
  )

  shuffled_reset <- shuffled_preserved
  rownames(shuffled_reset) <- NULL

  reset_info <- info(
    formula = out ~ x,
    cluster = ~ cluster,
    dat = shuffled_reset,
    robmod = fit
  )

  complete_control <- phase3b_make_data(
    seed = 20265003L,
    include_missing = FALSE
  )

  complete_fit <- phase3b_fit_full(
    dat = complete_control,
    engine = engine
  )

  set.seed(20265004L)
  complete_reset <- complete_control[
    sample(seq_len(nrow(complete_control))),
    ,
    drop = FALSE
  ]
  rownames(complete_reset) <- NULL

  complete_reset_info <- info(
    formula = out ~ x,
    cluster = ~ cluster,
    dat = complete_reset,
    robmod = complete_fit
  )

  baseline_ids <- baseline_info$dat$row_id
  preserved_ids <- preserved_info$dat$row_id
  reset_ids <- reset_info$dat$row_id
  complete_reset_ids <-
    complete_reset_info$dat$row_id

  row_index <- row_index + 1L

  alignment_rows[[row_index]] <- data.frame(
    engine = engine,
    reference_active_rows =
      length(reference_ids),
    baseline_active_rows =
      length(baseline_ids),
    preserved_active_rows =
      length(preserved_ids),
    reset_active_rows =
      length(reset_ids),
    baseline_matches_reference =
      setequal(
        baseline_ids,
        reference_ids
      ),
    preserved_rownames_match_reference =
      setequal(
        preserved_ids,
        reference_ids
      ),
    reset_rownames_match_reference =
      setequal(
        reset_ids,
        reference_ids
      ),
    reset_rownames_false_inclusions =
      length(
        setdiff(
          reset_ids,
          reference_ids
        )
      ),
    reset_rownames_false_exclusions =
      length(
        setdiff(
          reference_ids,
          reset_ids
        )
      ),
    complete_data_reset_keeps_all_rows =
      setequal(
        complete_reset_ids,
        complete_control$row_id
      ),
    a08_reproduced =
      !setequal(
        reset_ids,
        reference_ids
      ),
    stringsAsFactors = FALSE
  )

  baseline_public <- phase3b_capture_public(
    fit = fit,
    dat = dat,
    engine = engine
  )

  preserved_public <- phase3b_capture_public(
    fit = fit,
    dat = shuffled_preserved,
    engine = engine
  )

  reset_public <- phase3b_capture_public(
    fit = fit,
    dat = shuffled_reset,
    engine = engine
  )

  baseline_values <- phase3b_extract_public(
    baseline_public
  )
  preserved_values <- phase3b_extract_public(
    preserved_public
  )
  reset_values <- phase3b_extract_public(
    reset_public
  )

  public_index <- public_index + 1L

  public_rows[[public_index]] <- data.frame(
    engine = engine,
    baseline_returned =
      !is.null(baseline_public$value),
    preserved_returned =
      !is.null(preserved_public$value),
    reset_returned =
      !is.null(reset_public$value),
    baseline_error =
      baseline_public$error,
    preserved_error =
      preserved_public$error,
    reset_error =
      reset_public$error,
    preserved_max_absolute_difference =
      if (
        all(is.finite(baseline_values)) &&
        all(is.finite(preserved_values))
      ) {
        max(
          abs(
            preserved_values -
              baseline_values
          )
        )
      } else {
        NA_real_
      },
    reset_max_absolute_difference =
      if (
        all(is.finite(baseline_values)) &&
        all(is.finite(reset_values))
      ) {
        max(
          abs(
            reset_values -
              baseline_values
          )
        )
      } else {
        NA_real_
      },
    stringsAsFactors = FALSE
  )
}

alignment_results <- do.call(
  rbind,
  alignment_rows
)
rownames(alignment_results) <- NULL

public_results <- do.call(
  rbind,
  public_rows
)
rownames(public_results) <- NULL

checks <- data.frame(
  check = c(
    "baseline_alignment_matches_model_frame",
    "shuffling_with_preserved_rownames_preserves_active_rows",
    "complete_data_reset_rownames_keeps_all_rows",
    "reset_rownames_with_omitted_rows_reproduces_a08"
  ),
  passed = c(
    all(
      alignment_results$
        baseline_matches_reference
    ),
    all(
      alignment_results$
        preserved_rownames_match_reference
    ),
    all(
      alignment_results$
        complete_data_reset_keeps_all_rows
    ),
    any(
      alignment_results$
        a08_reproduced
    )
  ),
  required_for_interpretation = c(
    TRUE,
    TRUE,
    TRUE,
    TRUE
  ),
  details = c(
    paste(
      alignment_results$engine,
      alignment_results$
        baseline_matches_reference,
      sep = "=",
      collapse = "; "
    ),
    paste(
      alignment_results$engine,
      alignment_results$
        preserved_rownames_match_reference,
      sep = "=",
      collapse = "; "
    ),
    paste(
      alignment_results$engine,
      alignment_results$
        complete_data_reset_keeps_all_rows,
      sep = "=",
      collapse = "; "
    ),
    paste(
      alignment_results$engine,
      alignment_results$a08_reproduced,
      sep = "=",
      collapse = "; "
    )
  ),
  stringsAsFactors = FALSE
)

a08_reproduced <- any(
  alignment_results$a08_reproduced
)

issue_summary <- data.frame(
  issue_id = "A-08",
  issue = paste(
    "info() identifies active observations by matching row names",
    "between dat and robmod$model; harmless reordering followed by",
    "row-name reset can misidentify active rows after omissions."
  ),
  reproduced = a08_reproduced,
  engines_reproduced = paste(
    alignment_results$engine[
      alignment_results$a08_reproduced
    ],
    collapse = ","
  ),
  disposition = if (a08_reproduced) {
    paste(
      "Reproduced.",
      "Do not patch until a minimal model-frame/row-index alignment",
      "strategy is specified and regression-tested."
    )
  } else {
    paste(
      "Not reproduced.",
      "Review the audit harness before changing production code."
    )
  },
  stringsAsFactors = FALSE
)

source_files <- c(
  phase3b_audit =
    file.path(
      project_root,
      "data-raw",
      "robust_cats_phase3b_row_alignment_audit.R"
    ),
  audit_helpers =
    file.path(
      project_root,
      "data-raw",
      "robust_cats_audit_helpers.R"
    ),
  helpers_cimrob =
    file.path(
      project_root,
      "R",
      "helpers_cimrob.R"
    ),
  cluster_im_lmRob =
    file.path(
      project_root,
      "R",
      "cluster_im_lmRob.R"
    )
)

source_checksums <- rca_source_checksums(
  source_files
)

results <- list(
  checks = checks,
  alignment_results =
    alignment_results,
  public_results = public_results,
  issue_summary = issue_summary,
  source_checksums = source_checksums
)

rca_write_csv_atomic(
  checks,
  file.path(
    output_dir,
    "phase3b_checks.csv"
  )
)

rca_write_csv_atomic(
  alignment_results,
  file.path(
    output_dir,
    "phase3b_alignment_results.csv"
  )
)

rca_write_csv_atomic(
  public_results,
  file.path(
    output_dir,
    "phase3b_public_results.csv"
  )
)

rca_write_csv_atomic(
  issue_summary,
  file.path(
    output_dir,
    "phase3b_issue_summary.csv"
  )
)

rca_write_csv_atomic(
  source_checksums,
  file.path(
    output_dir,
    "phase3b_source_checksums.csv"
  )
)

rca_save_rds_atomic(
  results,
  file.path(
    output_dir,
    "phase3b_results.rds"
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
message("Phase 3B checks:")
print(
  checks,
  row.names = FALSE
)

message("")
message("Phase 3B alignment results:")
print(
  alignment_results,
  row.names = FALSE
)

message("")
message("Phase 3B public-path results:")
print(
  public_results,
  row.names = FALSE
)

message("")
message("Phase 3B issue summary:")
print(
  issue_summary,
  row.names = FALSE
)

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
      "required Phase 3B audit precondition(s) failed.",
      "Review the evidence before changing production code."
    ),
    call. = FALSE
  )
}

message("")
message(
  "Phase 3B adversarial audit completed."
)
