test_that("Study 1/2 freeze method schedules are exact and ordered", {
  schedule <- study12f_method_schedule()

  expect_identical(
    schedule$method[
      schedule$study == "Study 1"
    ],
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

  expect_identical(
    schedule$method[
      schedule$study == "Study 2"
    ],
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
})


test_that("Study 1/2 freeze seed blocks are exact", {
  expected <- rbind(
    data.frame(
      study = "Study 1",
      n_clusters = c(10L, 20L, 40L),
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
      n_clusters = c(10L, 20L, 40L),
      condition_seed = c(
        20260905L,
        20260906L,
        20260907L
      ),
      total_reps = 2000L,
      stringsAsFactors = FALSE
    )
  )

  expect_identical(
    study12f_seed_blocks(),
    expected
  )
})


test_that("Study 1/2 freeze reconstructs exact 2000-rep seed vectors without changing caller RNG", {
  set.seed(20260902L)
  kind_before <- RNGkind()
  seed_before <- .Random.seed

  seed_table <- study12f_replicate_seed_table()

  expect_identical(
    RNGkind(),
    kind_before
  )
  expect_identical(
    .Random.seed,
    seed_before
  )

  expect_equal(
    nrow(seed_table),
    6L * 2000L
  )

  split_table <- split(
    seed_table,
    interaction(
      seed_table$study,
      seed_table$n_clusters,
      drop = TRUE
    )
  )

  expect_equal(
    length(split_table),
    6L
  )

  for (x in split_table) {
    expected <-
      study12f_reference_replicate_seeds(
        condition_seed =
          x$condition_seed[1L],
        total_reps = 2000L
      )

    expect_identical(
      as.integer(
        x$replicate_seed
      ),
      as.integer(
        expected
      )
    )

    expect_false(
      anyDuplicated(
        x$replicate_seed
      ) > 0L
    )
  }
})


test_that("Study 1/2 freeze shard plan is exactly 200 ten-rep shards", {
  plan <- study12f_shard_plan()

  expect_equal(
    nrow(plan),
    200L
  )
  expect_true(
    all(
      plan$shard_reps == 10L
    )
  )
  expect_identical(
    plan$replicate_start,
    seq.int(
      1L,
      1991L,
      by = 10L
    )
  )
  expect_identical(
    plan$replicate_end,
    seq.int(
      10L,
      2000L,
      by = 10L
    )
  )
})


test_that("Study 1/2 prospective scientific checks pass against package frozen designs", {
  study1_design <- study1d_frozen_design()
  study2_design <- study2d_frozen_design()

  checks <- study12f_scientific_checks(
    study1_design = study1_design,
    study2_design = study2_design,
    method_schedule =
      study12f_method_schedule(),
    seed_blocks =
      study12f_seed_blocks(),
    replicate_seeds =
      study12f_replicate_seed_table(),
    shard_plan =
      study12f_shard_plan()
  )

  expect_true(
    all(
      checks$passed
    ),
    info = paste(
      checks$check[
        !checks$passed
      ],
      collapse = ", "
    )
  )
})


test_that("Study 1/2 registration location rejects placeholders", {
  expect_error(
    study12f_validate_registration_location(
      "TBD"
    ),
    "placeholder"
  )

  expect_error(
    study12f_validate_registration_location(
      "paste link here"
    ),
    "placeholder"
  )

  expect_identical(
    study12f_validate_registration_location(
      "https://example.org/permanent-registration"
    ),
    "https://example.org/permanent-registration"
  )
})


test_that("prospective Study 1/2 freeze does not fit or launch definitive simulations", {
  body_text <- paste(
    deparse(
      body(
        prepare_study12_freeze
      ),
      width.cutoff = 500L
    ),
    collapse = "\n"
  )

  forbidden <- c(
    "run_study1_definitive(",
    "run_study2_definitive(",
    "pwr_func_study1(",
    "pwr_func_study2(",
    "definitive_run_shard_checkpoint("
  )

  for (pattern in forbidden) {
    expect_false(
      grepl(
        pattern,
        body_text,
        fixed = TRUE
      ),
      info = paste(
        "Freeze function must not call",
        pattern
      )
    )
  }
})


test_that("definitive Study 1/2 runners require the prospective gate", {
  study1_text <- paste(
    deparse(
      body(
        run_study1_definitive
      ),
      width.cutoff = 500L
    ),
    collapse = "\n"
  )

  study2_text <- paste(
    deparse(
      body(
        run_study2_definitive
      ),
      width.cutoff = 500L
    ),
    collapse = "\n"
  )

  expect_true(
    grepl(
      "study12f_verify_gate",
      study1_text,
      fixed = TRUE
    )
  )

  expect_true(
    grepl(
      "study12f_verify_gate",
      study2_text,
      fixed = TRUE
    )
  )
})
