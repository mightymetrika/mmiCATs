test_that("definitive Study 1 frozen design remains exact", {
  config <- study1d_frozen_config()
  design <- study1d_frozen_design()

  expect_equal(config$final_reps, 2000L)
  expect_equal(config$final_seed_base, 20260815L)
  expect_equal(config$shard_size, 10L)
  expect_equal(config$minimum_free_gb, 2.0)
  expect_false(config$retain_completed_shards)

  expect_identical(
    study1d_methods(),
    c(
      "ri",
      "cr2",
      "cats",
      "cats_trunc",
      "cats_robust",
      "cats_robustbase",
      "robust_ri"
    )
  )

  expect_equal(nrow(design), 18L)
  expect_identical(
    design$condition_id,
    sprintf("S1C%03d", seq_len(18L))
  )
  expect_identical(
    sort(unique(design$n_clusters)),
    c(10L, 20L, 40L)
  )
  expect_equal(
    sort(unique(design$beta)),
    c(0, 0.10)
  )
  expect_identical(
    unique(design$cluster_size),
    40L
  )
  expect_equal(
    unique(design$contamination_prop),
    0.05
  )
  expect_equal(
    unique(design$reps),
    2000L
  )
  expect_equal(
    unique(design$shard_size),
    10L
  )

  expected_seed <- c(
    `10` = 20260815L,
    `20` = 20260816L,
    `40` = 20260817L
  )

  observed_seed <- tapply(
    design$condition_seed,
    design$n_clusters,
    unique
  )

  expect_identical(
    as.integer(observed_seed),
    as.integer(expected_seed)
  )

  expect_true(
    all(
      design$method_set ==
        paste(
          study1d_methods(),
          collapse = ","
        )
    )
  )
})


test_that("definitive Study 2 frozen design remains exact", {
  config <- study2d_frozen_config()
  design <- study2d_frozen_design()

  expect_equal(config$final_reps, 2000L)
  expect_equal(config$minimum_usable_reps, 1900L)
  expect_equal(config$final_seed_base, 20260905L)
  expect_equal(config$shard_size, 10L)
  expect_equal(config$minimum_free_gb, 2.0)
  expect_false(config$retain_completed_shards)

  expect_identical(
    study2d_methods(),
    c(
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
  )

  expect_equal(nrow(design), 24L)
  expect_identical(
    design$condition_id,
    sprintf("S2C%03d", seq_len(24L))
  )
  expect_identical(
    sort(unique(design$n_clusters)),
    c(10L, 20L, 40L)
  )
  expect_equal(
    sort(unique(design$beta)),
    c(0, 0.10)
  )
  expect_equal(
    sort(unique(design$random_slope_sd)),
    c(0.05, 0.10)
  )
  expect_equal(
    sort(unique(design$random_slope_variance)),
    c(0.0025, 0.01)
  )
  expect_identical(
    unique(design$cluster_size),
    40L
  )
  expect_equal(
    unique(design$contamination_prop),
    0.05
  )
  expect_equal(
    unique(design$reps),
    2000L
  )
  expect_equal(
    unique(design$shard_size),
    10L
  )

  expected_seed <- c(
    `10` = 20260905L,
    `20` = 20260906L,
    `40` = 20260907L
  )

  observed_seed <- tapply(
    design$condition_seed,
    design$n_clusters,
    unique
  )

  expect_identical(
    as.integer(observed_seed),
    as.integer(expected_seed)
  )

  expect_true(
    all(
      design$method_set ==
        paste(
          study2d_methods(),
          collapse = ","
        )
    )
  )
})


test_that("public definitive runners are package-owned and frozen", {
  s1 <- paste(
    deparse(
      body(run_study1_definitive),
      width.cutoff = 500L
    ),
    collapse = "\n"
  )

  s2 <- paste(
    deparse(
      body(run_study2_definitive),
      width.cutoff = 500L
    ),
    collapse = "\n"
  )

  expect_false(grepl("source(", s1, fixed = TRUE))
  expect_false(grepl("source(", s2, fixed = TRUE))
  expect_false(grepl("pkgload::load_all", s1, fixed = TRUE))
  expect_false(grepl("pkgload::load_all", s2, fixed = TRUE))
  expect_false(grepl("mmiCATs:::", s1, fixed = TRUE))
  expect_false(grepl("mmiCATs:::", s2, fixed = TRUE))

  expect_true(
    grepl(
      "study1d_frozen_design()",
      s1,
      fixed = TRUE
    )
  )
  expect_true(
    grepl(
      "study2d_frozen_design()",
      s2,
      fixed = TRUE
    )
  )

  expect_true(
    grepl(
      "definitive_make_replicate_seeds",
      s1,
      fixed = TRUE
    )
  )
  expect_true(
    grepl(
      "definitive_make_replicate_seeds",
      s2,
      fixed = TRUE
    )
  )
  expect_true(
    grepl(
      "definitive_run_shard_checkpoint",
      s1,
      fixed = TRUE
    )
  )
  expect_true(
    grepl(
      "definitive_run_shard_checkpoint",
      s2,
      fixed = TRUE
    )
  )
})


test_that("definitive runners expose scheduling but not scientific-design overrides", {
  expect_identical(
    names(formals(run_study1_definitive)),
    c(
      "project_root",
      "output_dir",
      "condition_ids_to_run",
      "overwrite_completed"
    )
  )

  expect_identical(
    names(formals(run_study2_definitive)),
    c(
      "project_root",
      "output_dir",
      "condition_ids_to_run",
      "overwrite_completed"
    )
  )
})


test_that("definitive source checksums point to package R infrastructure", {
  s1 <- paste(
    deparse(
      body(study1d_make_source_checksums),
      width.cutoff = 500L
    ),
    collapse = "\n"
  )

  s2 <- paste(
    deparse(
      body(study2d_make_source_checksums),
      width.cutoff = 500L
    ),
    collapse = "\n"
  )

  expect_true(
    grepl(
      '"R", "definitive_sharding_helpers.R"',
      s1,
      fixed = TRUE
    )
  )
  expect_true(
    grepl(
      '"R", "definitive_study1.R"',
      s1,
      fixed = TRUE
    )
  )
  expect_true(
    grepl(
      '"R", "definitive_study2.R"',
      s2,
      fixed = TRUE
    )
  )

  expect_false(
    grepl(
      '"data-raw", "definitive_sharding_helpers.R"',
      s1,
      fixed = TRUE
    )
  )
  expect_false(
    grepl(
      '"data-raw", "definitive_sharding_helpers.R"',
      s2,
      fixed = TRUE
    )
  )
})
