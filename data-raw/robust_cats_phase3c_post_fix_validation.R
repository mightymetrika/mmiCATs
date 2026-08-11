# Robust CATs audit: Phase 3C post-fix validation
#
# Purpose:
#   Validate the minimal corrections for A-05 and A-08 while preserving
#   normal-case CATs behavior and the completed Phase 2 fixes.
#
# Run from the mmiCATs project root after replacing the Phase 3C files and
# running devtools::document().

library(devtools)

load_all()

source(
  "data-raw/robust_cats_audit_helpers.R"
)

message("")
message("1. Running focused Phase 3C regression tests...")

test(
  filter = "study1-robust-diagnostics",
  stop_on_failure = TRUE
)

test(
  filter = "robust-cats-failure-retention",
  stop_on_failure = TRUE
)

message("")
message("2. Verifying A-05 template failure no longer blocks viable inference...")

set.seed(20266001L)

a05_dat <- study1_simulate_data(
  n_clusters = 6L,
  cluster_size = 20L,
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

a05_fit <- function(formula, data, engine) {
  if (length(unique(as.character(data$cluster))) > 1L) {
    stop(
      "Synthetic Phase 3C template-only failure.",
      call. = FALSE
    )
  }

  stats::lm(
    formula = formula,
    data = data
  )
}

a05_rows <- lapply(
  c("robust", "robustbase"),
  function(engine) {
    observed <- study1_fit_robust_cats(
      dat = a05_dat,
      alpha = 0.05,
      engine = engine,
      fit_function = a05_fit
    )

    data.frame(
      engine = engine,
      finite_inference = all(is.finite(c(
        observed$estimate,
        observed$std_error,
        observed$df,
        observed$p_value,
        observed$conf_low,
        observed$conf_high
      ))),
      retained_clusters = observed$retained_clusters,
      template_error_recorded =
        rca_has_text(observed$template_error) &&
        grepl(
          "Synthetic Phase 3C template-only failure",
          observed$template_error,
          fixed = TRUE
        ),
      cluster_error_count = observed$cluster_error_count,
      stringsAsFactors = FALSE
    )
  }
)

a05_validation <- do.call(
  rbind,
  a05_rows
)
rownames(a05_validation) <- NULL

print(
  a05_validation,
  row.names = FALSE
)

if (!all(
  a05_validation$finite_inference &
    a05_validation$retained_clusters == 6L &
    a05_validation$template_error_recorded &
    a05_validation$cluster_error_count == 0L
)) {
  stop(
    "A-05 post-fix validation failed.",
    call. = FALSE
  )
}

message("")
message("3. Verifying normal-case robust CATs remains unchanged...")

normal_dat <- rca_make_validation_data(
  seed = 20266002L,
  n_clusters = 6L,
  cluster_size = 20L
)

normal_rows <- list()
normal_index <- 0L

for (engine in c("robust", "robustbase")) {
  seed <- if (engine == "robust") {
    20266003L
  } else {
    20266004L
  }

  set.seed(seed)

  expected <- rca_oracle(
    dat = normal_dat,
    engine = engine,
    alpha = 0.05,
    truncation_rule = "none",
    consume_template = TRUE
  )

  set.seed(seed)

  observed <- study1_fit_robust_cats(
    dat = normal_dat,
    alpha = 0.05,
    engine = engine
  )

  comparison <- rca_compare_results(
    reference = expected$aggregate,
    observed = observed,
    comparison = paste0(
      "Phase 3C normal-case preservation: ",
      engine
    ),
    tolerance = 1e-8
  )

  normal_index <- normal_index + 1L
  normal_rows[[normal_index]] <- comparison
}

normal_comparison <- do.call(
  rbind,
  normal_rows
)
rownames(normal_comparison) <- NULL

print(
  normal_comparison,
  row.names = FALSE
)

if (!all(normal_comparison$passed)) {
  stop(
    "Normal-case robust CATs preservation failed.",
    call. = FALSE
  )
}

message("")
message("4. Verifying A-08 active-row alignment after row-name reset...")

set.seed(20266005L)

a08_dat <- data.frame(
  row_id = seq_len(120L),
  cluster = factor(rep(1:6, each = 20)),
  x = stats::rnorm(120L)
)

a08_dat$out <- 0.25 + 0.40 * a08_dat$x +
  rep(seq(-0.3, 0.3, length.out = 6), each = 20) +
  stats::rnorm(120L, sd = 0.5)

a08_dat$x[c(5L, 47L, 88L)] <- NA_real_
a08_dat$out[c(22L, 69L)] <- NA_real_

expected_ids <- a08_dat$row_id[
  stats::complete.cases(
    a08_dat[, c("out", "x")]
  )
]

set.seed(20266006L)

a08_shuffled <- a08_dat[
  sample(seq_len(nrow(a08_dat))),
  ,
  drop = FALSE
]
rownames(a08_shuffled) <- NULL

a08_rows <- list()
a08_index <- 0L

for (engine in c("robust", "robustbase")) {
  set.seed(20266007L)

  full_fit <- switch(
    engine,
    "robust" = robust::lmRob(
      out ~ x,
      data = a08_dat,
      na.action = stats::na.omit
    ),
    "robustbase" = robustbase::lmrob(
      out ~ x,
      data = a08_dat,
      na.action = stats::na.omit
    )
  )

  aligned <- info(
    formula = out ~ x,
    cluster = ~ cluster,
    dat = a08_shuffled,
    robmod = full_fit
  )

  set.seed(20266008L)

  baseline <- cluster_im_lmRob(
    robmod = full_fit,
    formula = out ~ x,
    dat = a08_dat,
    cluster = ~ cluster,
    return.vcv = TRUE,
    engine = engine
  )

  set.seed(20266008L)

  observed <- cluster_im_lmRob(
    robmod = full_fit,
    formula = out ~ x,
    dat = a08_shuffled,
    cluster = ~ cluster,
    return.vcv = TRUE,
    engine = engine
  )

  quantities <- c(
    estimate = abs(
      unname(observed$beta.bar["x"]) -
        unname(baseline$beta.bar["x"])
    ),
    p_value = abs(
      unname(observed$p.values["x", 1L]) -
        unname(baseline$p.values["x", 1L])
    )
  )

  a08_index <- a08_index + 1L
  a08_rows[[a08_index]] <- data.frame(
    engine = engine,
    active_rows_match = setequal(
      aligned$dat$row_id,
      expected_ids
    ),
    false_inclusions = length(
      setdiff(
        aligned$dat$row_id,
        expected_ids
      )
    ),
    false_exclusions = length(
      setdiff(
        expected_ids,
        aligned$dat$row_id
      )
    ),
    maximum_public_difference = max(quantities),
    within_row_order_tolerance = max(quantities) <= 1e-4,
    stringsAsFactors = FALSE
  )
}

a08_validation <- do.call(
  rbind,
  a08_rows
)
rownames(a08_validation) <- NULL

print(
  a08_validation,
  row.names = FALSE
)

if (!all(
  a08_validation$active_rows_match &
    a08_validation$false_inclusions == 0L &
    a08_validation$false_exclusions == 0L &
    a08_validation$within_row_order_tolerance
)) {
  stop(
    "A-08 post-fix validation failed.",
    call. = FALSE
  )
}

message("")
message("5. Rerunning the completed Phase 2D preservation validator...")

source(
  "data-raw/robust_cats_phase2d_post_fix_validation.R"
)

message("")
message("All Phase 3C targeted validations passed.")
