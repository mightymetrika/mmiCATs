test_that("shard plan covers every replication exactly once", {
  plan <- definitive_make_shard_plan(
    total_reps = 23L,
    shard_size = 10L
  )

  expect_equal(
    plan$replicate_start,
    c(1L, 11L, 21L)
  )
  expect_equal(
    plan$replicate_end,
    c(10L, 20L, 23L)
  )
  expect_equal(
    unlist(
      Map(
        seq.int,
        plan$replicate_start,
        plan$replicate_end
      ),
      use.names = FALSE
    ),
    1:23
  )
})


test_that("condition seed reproduces monolithic replicate seed vector", {
  expected <- {
    set.seed(20261030L)
    sample.int(
      .Machine$integer.max,
      11L,
      replace = FALSE
    )
  }

  observed <- definitive_make_replicate_seeds(
    condition_seed = 20261030L,
    total_reps = 11L
  )

  expect_identical(
    observed,
    expected
  )
})


test_that("global shard replicate numbering is correct", {
  x <- data.frame(
    replicate = rep(1:3, each = 2),
    method = rep(c("a", "b"), 3),
    stringsAsFactors = FALSE
  )

  observed <- definitive_offset_shard_replicates(
    x,
    replicate_start = 11L
  )

  expect_equal(
    observed$replicate,
    rep(11:13, each = 2)
  )
})


test_that("disk guard stops below threshold", {
  expect_invisible(
    definitive_disk_guard(
      path = tempdir(),
      minimum_free_gb = 2,
      free_gb = 3
    )
  )

  expect_error(
    definitive_disk_guard(
      path = tempdir(),
      minimum_free_gb = 2,
      free_gb = 1.5
    ),
    "No new shard was started"
  )
})


test_that("checkpoint validator rejects changed seeds", {
  plan <- definitive_make_shard_plan(
    total_reps = 2L,
    shard_size = 2L
  )

  checkpoint <- list(
    status = "complete",
    condition_id = "TEST",
    shard_id = plan$shard_id[1L],
    replicate_start = 1L,
    replicate_end = 2L,
    replicate_seeds = c(10L, 11L),
    methods = c("a", "b"),
    replicates = data.frame(
      replicate = rep(1:2, each = 2),
      method = rep(c("a", "b"), 2)
    )
  )

  expect_true(
    definitive_validate_complete_checkpoint(
      checkpoint,
      condition_id = "TEST",
      shard_row = plan[1L, , drop = FALSE],
      expected_seeds = c(10L, 11L),
      expected_methods = c("a", "b")
    )
  )

  expect_false(
    definitive_validate_complete_checkpoint(
      checkpoint,
      condition_id = "TEST",
      shard_row = plan[1L, , drop = FALSE],
      expected_seeds = c(10L, 12L),
      expected_methods = c("a", "b")
    )
  )
})

test_that("matching error checkpoint is recognized as the same frozen shard", {
  plan <- definitive_make_shard_plan(
    total_reps = 1L,
    shard_size = 1L
  )

  checkpoint <- list(
    status = "error",
    condition_id = "ERR",
    shard_id = plan$shard_id[1L],
    replicate_start = 1L,
    replicate_end = 1L,
    replicate_seeds = 123L,
    methods = c("a", "b"),
    replicates = NULL,
    error = "synthetic"
  )

  expect_true(
    definitive_checkpoint_spec_matches(
      checkpoint = checkpoint,
      condition_id = "ERR",
      shard_row = plan[1L, , drop = FALSE],
      expected_seeds = 123L,
      expected_methods = c("a", "b")
    )
  )

  expect_false(
    definitive_validate_complete_checkpoint(
      checkpoint = checkpoint,
      condition_id = "ERR",
      shard_row = plan[1L, , drop = FALSE],
      expected_seeds = 123L,
      expected_methods = c("a", "b")
    )
  )
})
