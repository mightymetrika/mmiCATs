# Phase 6D corrected Study 3 old-vs-new substantive equivalence audit
#
# Run ONLY after the corrected Study 3 analysis has completed in:
#   data-raw/study3-results/definitive-study3-phase6d-corrected
#
# This script never fits a model and never modifies either Study 3 result
# directory. It reads old/new outputs, writes audit evidence to a separate
# validation directory, and stops if any substantive difference is unexpected.

library(devtools)
load_all()

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

old_dir <- file.path(
  project_root,
  "data-raw",
  "study3-results",
  "definitive-study3"
)

new_dir <- file.path(
  project_root,
  "data-raw",
  "study3-results",
  "definitive-study3-phase6d-corrected"
)

audit_dir <- file.path(
  project_root,
  "data-raw",
  "study3-results",
  "phase6d-corrected-equivalence"
)

dir.create(
  audit_dir,
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
      passed = isTRUE(passed),
      details = as.character(details),
      stringsAsFactors = FALSE
    )
}

read_csv <- function(path) {
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
}

normalize_text <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(trimws(x))] <- NA_character_
  x
}

same_text <- function(x,
                      y) {
  identical(
    normalize_text(x),
    normalize_text(y)
  )
}

numeric_equal <- function(x,
                          y,
                          tolerance = 1e-10) {
  if (length(x) != length(y)) {
    return(FALSE)
  }

  x <- as.numeric(x)
  y <- as.numeric(y)

  same_na <- identical(
    is.na(x),
    is.na(y)
  )

  if (!same_na) {
    return(FALSE)
  }

  keep <- !is.na(x)

  if (!any(keep)) {
    return(TRUE)
  }

  isTRUE(
    all.equal(
      x[keep],
      y[keep],
      tolerance = tolerance,
      check.attributes = FALSE
    )
  )
}

column_equal <- function(x,
                         y,
                         tolerance = 1e-10) {
  if (is.numeric(x) || is.integer(x) ||
      is.numeric(y) || is.integer(y)) {
    return(
      numeric_equal(
        x,
        y,
        tolerance = tolerance
      )
    )
  }

  if (is.logical(x) || is.logical(y)) {
    return(
      identical(
        as.logical(x),
        as.logical(y)
      )
    )
  }

  same_text(x, y)
}

align_by_key <- function(old,
                         new,
                         key) {
  if (!all(key %in% names(old)) ||
      !all(key %in% names(new))) {
    stop(
      paste(
        "Missing key column(s):",
        paste(
          setdiff(
            key,
            intersect(names(old), names(new))
          ),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  old_key <- do.call(
    paste,
    c(
      old[key],
      sep = "\r"
    )
  )

  new_key <- do.call(
    paste,
    c(
      new[key],
      sep = "\r"
    )
  )

  if (anyDuplicated(old_key) ||
      anyDuplicated(new_key)) {
    stop(
      paste(
        "Duplicate comparison key detected:",
        paste(key, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!setequal(old_key, new_key)) {
    stop(
      paste(
        "Old/new key sets differ for:",
        paste(key, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  new <- new[
    match(old_key, new_key),
    ,
    drop = FALSE
  ]

  rownames(old) <- NULL
  rownames(new) <- NULL

  list(
    old = old,
    new = new
  )
}

warning_equivalent <- function(old_warning,
                               new_warning) {
  old_warning <- normalize_text(old_warning)
  new_warning <- normalize_text(new_warning)

  if (length(old_warning) != length(new_warning)) {
    return(FALSE)
  }

  ok <- logical(length(old_warning))

  for (i in seq_along(old_warning)) {
    if (identical(
      old_warning[i],
      "Cluster : "
    )) {
      ok[i] <- is.na(new_warning[i])
    } else {
      ok[i] <- identical(
        old_warning[i],
        new_warning[i]
      )
    }
  }

  all(ok)
}

compare_table <- function(old_path,
                          new_path,
                          key,
                          exclude = character(0),
                          warning_columns = character(0),
                          label,
                          tolerance = 1e-10) {
  old <- read_csv(old_path)
  new <- read_csv(new_path)

  aligned <- align_by_key(
    old,
    new,
    key
  )

  old <- aligned$old
  new <- aligned$new

  add_check(
    paste0(label, "__row_count"),
    nrow(old) == nrow(new),
    paste(
      "old =", nrow(old),
      "; new =", nrow(new)
    )
  )

  common <- intersect(
    names(old),
    names(new)
  )

  required_same_names <- setdiff(
    union(names(old), names(new)),
    exclude
  )

  add_check(
    paste0(label, "__column_set"),
    setequal(
      setdiff(names(old), exclude),
      setdiff(names(new), exclude)
    ),
    paste(
      "old columns =", ncol(old),
      "; new columns =", ncol(new)
    )
  )

  compare_columns <- setdiff(
    common,
    c(
      exclude,
      warning_columns
    )
  )

  for (column in compare_columns) {
    add_check(
      paste0(
        label,
        "__",
        column
      ),
      column_equal(
        old[[column]],
        new[[column]],
        tolerance = tolerance
      )
    )
  }

  for (column in intersect(
    warning_columns,
    common
  )) {
    add_check(
      paste0(
        label,
        "__",
        column,
        "__allowed_warning_change"
      ),
      warning_equivalent(
        old[[column]],
        new[[column]]
      )
    )
  }

  invisible(
    list(
      old = old,
      new = new
    )
  )
}

# -------------------------------------------------------------------------
# Completion and immutable original-output checks
# -------------------------------------------------------------------------

required_dirs <- c(
  old_dir,
  new_dir
)

add_check(
  "old_and_corrected_result_directories_exist",
  all(dir.exists(required_dirs))
)

add_check(
  "old_completion_marker_exists",
  file.exists(
    file.path(
      old_dir,
      "STUDY3_COMPLETE.txt"
    )
  )
)

add_check(
  "corrected_completion_marker_exists",
  file.exists(
    file.path(
      new_dir,
      "STUDY3_COMPLETE.txt"
    )
  )
)

old_checksum_path <- file.path(
  old_dir,
  "study3_output_checksums.csv"
)

add_check(
  "original_checksum_record_exists",
  file.exists(old_checksum_path)
)

if (!file.exists(old_checksum_path)) {
  stop(
    "Original Study 3 checksum record is missing.",
    call. = FALSE
  )
}

old_checksums <- read_csv(
  old_checksum_path
)

old_checksum_files <- file.path(
  project_root,
  old_checksums$relative_path
)

old_checksum_ok <- all(
  file.exists(old_checksum_files)
) &&
  identical(
    unname(
      tools::md5sum(
        old_checksum_files
      )
    ),
    old_checksums$md5
  )

add_check(
  "original_study3_outputs_still_match_archived_checksums",
  old_checksum_ok
)

# -------------------------------------------------------------------------
# Frozen input / execution metadata equivalence
# -------------------------------------------------------------------------

old_meta <- read_csv(
  file.path(
    old_dir,
    "study3_execution_metadata.csv"
  )
)

new_meta <- read_csv(
  file.path(
    new_dir,
    "study3_execution_metadata.csv"
  )
)

old_meta_map <- stats::setNames(
  old_meta$value,
  old_meta$field
)

new_meta_map <- stats::setNames(
  new_meta$value,
  new_meta$field
)

metadata_fields <- c(
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
)

for (field in metadata_fields) {
  add_check(
    paste0(
      "execution_metadata__",
      field
    ),
    identical(
      unname(old_meta_map[field]),
      unname(new_meta_map[field])
    ),
    paste(
      "old =",
      old_meta_map[field],
      "; new =",
      new_meta_map[field]
    )
  )
}

# -------------------------------------------------------------------------
# Full-data method results
# -------------------------------------------------------------------------

observed_full <- compare_table(
  file.path(
    old_dir,
    "study3_observed_method_comparison.csv"
  ),
  file.path(
    new_dir,
    "study3_observed_method_comparison.csv"
  ),
  key = "method",
  exclude = "runtime_sec",
  warning_columns = "warning",
  label = "observed_full"
)

perturbed_full <- compare_table(
  file.path(
    old_dir,
    "study3_perturbed_method_comparison.csv"
  ),
  file.path(
    new_dir,
    "study3_perturbed_method_comparison.csv"
  ),
  key = "method",
  exclude = "runtime_sec",
  warning_columns = "warning",
  label = "perturbed_full"
)

# Explicit real-warning preservation gate.
obs_rb <- observed_full$new[
  observed_full$new$method ==
    "cats_robustbase",
  ,
  drop = FALSE
]

pert_rb <- perturbed_full$new[
  perturbed_full$new$method ==
    "cats_robustbase",
  ,
  drop = FALSE
]

add_check(
  "observed_robustbase_real_warning_preserved",
  nrow(obs_rb) == 1L &&
    identical(
      normalize_text(obs_rb$warning),
      "Cluster 334: M-step did NOT converge. Returning unconverged SM-estimate"
    )
)

add_check(
  "perturbed_robustbase_real_warning_preserved",
  nrow(pert_rb) == 1L &&
    identical(
      normalize_text(pert_rb$warning),
      "Cluster 371: find_scale() did not converge in 'maxit.scale' (= 200) iterations with tol=1e-10, last rel.diff=0"
    )
)

# Explicit phantom-warning removal gate.
new_warning_text <- c(
  normalize_text(
    observed_full$new$warning
  ),
  normalize_text(
    perturbed_full$new$warning
  )
)

add_check(
  "no_full_data_phantom_cluster_warning",
  !any(
    new_warning_text ==
      "Cluster : ",
    na.rm = TRUE
  )
)

# -------------------------------------------------------------------------
# Derived observed-vs-perturbed table
# -------------------------------------------------------------------------

compare_table(
  file.path(
    old_dir,
    "study3_observed_vs_perturbed.csv"
  ),
  file.path(
    new_dir,
    "study3_observed_vs_perturbed.csv"
  ),
  key = "method",
  exclude = c(
    "observed_runtime_sec",
    "perturbed_runtime_sec"
  ),
  warning_columns = c(
    "observed_warning",
    "perturbed_warning"
  ),
  label = "observed_vs_perturbed"
)

# -------------------------------------------------------------------------
# Cluster diagnostics
# -------------------------------------------------------------------------

for (dataset in c(
  "observed",
  "perturbed"
)) {
  compare_table(
    file.path(
      old_dir,
      paste0(
        "study3_",
        dataset,
        "_cluster_fits.csv"
      )
    ),
    file.path(
      new_dir,
      paste0(
        "study3_",
        dataset,
        "_cluster_fits.csv"
      )
    ),
    key = c(
      "cluster",
      "engine"
    ),
    label = paste0(
      dataset,
      "_cluster_fits"
    )
  )

  compare_table(
    file.path(
      old_dir,
      paste0(
        "study3_",
        dataset,
        "_cluster_slope_differences.csv"
      )
    ),
    file.path(
      new_dir,
      paste0(
        "study3_",
        dataset,
        "_cluster_slope_differences.csv"
      )
    ),
    key = c(
      "cluster",
      "comparison"
    ),
    label = paste0(
      dataset,
      "_cluster_slope_differences"
    )
  )
}

# -------------------------------------------------------------------------
# Observation diagnostics: only lmRob robust_weight is allowed to change
# -------------------------------------------------------------------------

for (dataset in c(
  "observed",
  "perturbed"
)) {
  old_path <- file.path(
    old_dir,
    paste0(
      "study3_",
      dataset,
      "_observation_diagnostics.csv"
    )
  )

  new_path <- file.path(
    new_dir,
    paste0(
      "study3_",
      dataset,
      "_observation_diagnostics.csv"
    )
  )

  old <- read_csv(old_path)
  new <- read_csv(new_path)

  aligned <- align_by_key(
    old,
    new,
    "row_id"
  )

  old <- aligned$old
  new <- aligned$new

  compare_columns <- setdiff(
    intersect(
      names(old),
      names(new)
    ),
    "robust_weight"
  )

  for (column in compare_columns) {
    add_check(
      paste0(
        dataset,
        "_observation_diagnostics__",
        column
      ),
      column_equal(
        old[[column]],
        new[[column]]
      )
    )
  }

  add_check(
    paste0(
      dataset,
      "_old_lmRob_weights_were_missing"
    ),
    all(
      is.na(
        suppressWarnings(
          as.numeric(
            old$robust_weight
          )
        )
      )
    )
  )

  new_weights <- suppressWarnings(
    as.numeric(
      new$robust_weight
    )
  )

  add_check(
    paste0(
      dataset,
      "_corrected_lmRob_weights_populated_and_finite"
    ),
    length(new_weights) == nrow(new) &&
      all(is.finite(new_weights)),
    paste(
      "rows =",
      nrow(new),
      "; finite weights =",
      sum(is.finite(new_weights))
    )
  )
}

# Contaminated-observation diagnostics: same allowed weight change.
old_contam <- read_csv(
  file.path(
    old_dir,
    "study3_contaminated_observation_diagnostics.csv"
  )
)

new_contam <- read_csv(
  file.path(
    new_dir,
    "study3_contaminated_observation_diagnostics.csv"
  )
)

contam_aligned <- align_by_key(
  old_contam,
  new_contam,
  "row_id"
)

old_contam <- contam_aligned$old
new_contam <- contam_aligned$new

for (column in setdiff(
  intersect(
    names(old_contam),
    names(new_contam)
  ),
  "robust_weight"
)) {
  add_check(
    paste0(
      "contaminated_observation_diagnostics__",
      column
    ),
    column_equal(
      old_contam[[column]],
      new_contam[[column]]
    )
  )
}

new_contam_weights <- suppressWarnings(
  as.numeric(
    new_contam$robust_weight
  )
)

add_check(
  "corrected_contaminated_lmRob_weights_populated_and_finite",
  nrow(new_contam) == 18L &&
    all(is.finite(new_contam_weights)),
  paste(
    "rows =",
    nrow(new_contam),
    "; finite weights =",
    sum(is.finite(new_contam_weights))
  )
)

# -------------------------------------------------------------------------
# Leave-one-Subject-out substantive equivalence
# -------------------------------------------------------------------------

loo <- compare_table(
  file.path(
    old_dir,
    "study3_leave_one_subject_out.csv"
  ),
  file.path(
    new_dir,
    "study3_leave_one_subject_out.csv"
  ),
  key = c(
    "dataset",
    "method",
    "omitted_cluster"
  ),
  warning_columns = "warning",
  label = "leave_one_subject_out"
)

add_check(
  "loo_row_count_324",
  nrow(loo$new) == 324L,
  paste(
    "rows =",
    nrow(loo$new)
  )
)

add_check(
  "no_loo_phantom_cluster_warning",
  !any(
    normalize_text(
      loo$new$warning
    ) ==
      "Cluster : ",
    na.rm = TRUE
  )
)

# LOO status: elapsed time is descriptive and allowed to differ.
for (dataset in c(
  "observed",
  "perturbed"
)) {
  compare_table(
    file.path(
      old_dir,
      paste0(
        "study3_",
        dataset,
        "_loo_status.csv"
      )
    ),
    file.path(
      new_dir,
      paste0(
        "study3_",
        dataset,
        "_loo_status.csv"
      )
    ),
    key = c(
      "dataset",
      "cluster_index",
      "omitted_cluster"
    ),
    exclude = "elapsed_sec",
    label = paste0(
      dataset,
      "_loo_status"
    )
  )
}

# -------------------------------------------------------------------------
# Corrected-output completeness / new checksum self-verification
# -------------------------------------------------------------------------

new_checksum_path <- file.path(
  new_dir,
  "study3_output_checksums.csv"
)

add_check(
  "corrected_checksum_record_exists",
  file.exists(new_checksum_path)
)

if (file.exists(new_checksum_path)) {
  new_checksums <- read_csv(
    new_checksum_path
  )

  new_checksum_files <- file.path(
    project_root,
    new_checksums$relative_path
  )

  new_checksum_ok <- all(
    file.exists(new_checksum_files)
  ) &&
    identical(
      unname(
        tools::md5sum(
          new_checksum_files
        )
      ),
      new_checksums$md5
    )

  add_check(
    "corrected_outputs_match_their_checksums",
    new_checksum_ok
  )
}

# -------------------------------------------------------------------------
# Save audit evidence and stop on unexpected differences
# -------------------------------------------------------------------------

checks_df <- do.call(
  rbind,
  checks
)

rownames(checks_df) <- NULL

utils::write.csv(
  checks_df,
  file.path(
    audit_dir,
    "phase6d_study3_equivalence_checks.csv"
  ),
  row.names = FALSE,
  na = ""
)

summary_lines <- c(
  "mmiCATs Phase 6D corrected Study 3 equivalence audit",
  paste(
    "Created:",
    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S %Z"
    )
  ),
  paste(
    "Checks:",
    nrow(checks_df)
  ),
  paste(
    "Passed:",
    sum(checks_df$passed)
  ),
  paste(
    "Failed:",
    sum(!checks_df$passed)
  ),
  "",
  "Allowed differences:",
  "- runtime fields",
  "- phantom 'Cluster : ' warning -> missing when no genuine warning exists",
  "- lmRob robustness weights populated in corrected diagnostics",
  "- plots/checksums/session/source metadata affected by those documented changes",
  "",
  "Substantive numerical quantities are required to match within tolerance 1e-10."
)

writeLines(
  summary_lines,
  file.path(
    audit_dir,
    "phase6d_study3_equivalence_summary.txt"
  )
)

utils::capture.output(
  sessionInfo(),
  file = file.path(
    audit_dir,
    "session_info.txt"
  )
)

message("")
message(
  "Phase 6D corrected Study 3 equivalence checks:"
)

print(
  checks_df,
  row.names = FALSE
)

if (!all(checks_df$passed)) {
  stop(
    paste(
      sum(!checks_df$passed),
      "Phase 6D Study 3 equivalence check(s) failed.",
      "Stop and review before accepting corrected Study 3."
    ),
    call. = FALSE
  )
}

message("")
message(
  paste(
    "All Phase 6D corrected Study 3 substantive equivalence checks passed.",
    "The original Study 3 results remain preserved."
  )
)
