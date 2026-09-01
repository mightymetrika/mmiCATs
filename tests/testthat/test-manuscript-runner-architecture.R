test_that("all manuscript execution entry points are exported", {
  exports <- getNamespaceExports(
    "mmiCATs"
  )

  expect_true(
    all(
      c(
        "run_study1_definitive",
        "run_study2_definitive",
        "prepare_study3_empirical",
        "run_study3_empirical"
      ) %in% exports
    )
  )
})


test_that("package manuscript runners do not source project scripts", {
  functions <- list(
    run_study1_definitive =
      run_study1_definitive,
    run_study2_definitive =
      run_study2_definitive,
    prepare_study3_empirical =
      prepare_study3_empirical,
    run_study3_empirical =
      run_study3_empirical
  )

  for (name in names(functions)) {
    body_text <- paste(
      deparse(
        body(
          functions[[name]]
        ),
        width.cutoff = 500L
      ),
      collapse = "\n"
    )

    expect_false(
      grepl(
        "source(",
        body_text,
        fixed = TRUE
      ),
      info = paste(
        name,
        "should not source project scripts."
      )
    )

    expect_false(
      grepl(
        "pkgload::load_all",
        body_text,
        fixed = TRUE
      ),
      info = paste(
        name,
        "should not depend on pkgload::load_all()."
      )
    )
  }
})


test_that("definitive Study 1 and Study 2 runners use package sharding", {
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
      "definitive_run_shard_checkpoint",
      study1_text,
      fixed = TRUE
    )
  )

  expect_true(
    grepl(
      "definitive_run_shard_checkpoint",
      study2_text,
      fixed = TRUE
    )
  )

  expect_true(
    grepl(
      "definitive_make_replicate_seeds",
      study1_text,
      fixed = TRUE
    )
  )

  expect_true(
    grepl(
      "definitive_make_replicate_seeds",
      study2_text,
      fixed = TRUE
    )
  )
})


test_that("Study 3 package runner consumes a frozen analysis rather than creating contamination", {
  body_text <- paste(
    deparse(
      body(
        run_study3_empirical
      ),
      width.cutoff = 500L
    ),
    collapse = "\n"
  )

  expect_true(
    grepl(
      "sleepstudy_canonical.rds",
      body_text,
      fixed = TRUE
    )
  )

  expect_true(
    grepl(
      "sleepstudy_perturbed.rds",
      body_text,
      fixed = TRUE
    )
  )

  expect_false(
    grepl(
      "sample(",
      body_text,
      fixed = TRUE
    )
  )

  expect_false(
    grepl(
      "sample.int(",
      body_text,
      fixed = TRUE
    )
  )
})
