drop_runtime_for_shard_test <- function(data) {
  data[
    ,
    setdiff(names(data), "runtime_sec"),
    drop = FALSE
  ]
}

sort_shard_test_replicates <- function(data) {
  data <- data[
    order(data$replicate, data$method),
    ,
    drop = FALSE
  ]
  rownames(data) <- NULL
  data
}

bind_two_test_shards <- function(first, second) {
  second$replicates$replicate <-
    second$replicates$replicate +
    max(first$replicates$replicate)

  out <- rbind(
    first$replicates,
    second$replicates
  )
  rownames(out) <- NULL
  out
}


test_that("Study 1 explicit replication seeds reproduce deterministic shards", {
  methods <- c("cr2", "cats")

  monolithic <- pwr_func_study1(
    n_clusters = 6,
    cluster_size = 20,
    beta = 0.10,
    contamination = "vertical",
    contamination_size = 6,
    reps = 4,
    alpha = 0.05,
    methods = methods,
    seed = 20261020L,
    keep_replicates = TRUE
  )

  seeds <- monolithic$settings$replicate_seeds

  first <- pwr_func_study1(
    n_clusters = 6,
    cluster_size = 20,
    beta = 0.10,
    contamination = "vertical",
    contamination_size = 6,
    reps = 2,
    alpha = 0.05,
    methods = methods,
    keep_replicates = TRUE,
    replicate_seeds = seeds[1:2]
  )

  second <- pwr_func_study1(
    n_clusters = 6,
    cluster_size = 20,
    beta = 0.10,
    contamination = "vertical",
    contamination_size = 6,
    reps = 2,
    alpha = 0.05,
    methods = methods,
    keep_replicates = TRUE,
    replicate_seeds = seeds[3:4]
  )

  sharded <- bind_two_test_shards(first, second)

  expect_equal(
    drop_runtime_for_shard_test(
      sort_shard_test_replicates(monolithic$replicates)
    ),
    drop_runtime_for_shard_test(
      sort_shard_test_replicates(sharded)
    ),
    tolerance = 1e-12
  )

  sharded_summary <- study1_summarize_results(
    replicate_results = sharded,
    methods = methods,
    reps = 4L
  )

  summary_columns <- setdiff(
    names(monolithic$summary),
    "mean_runtime_sec"
  )

  expect_equal(
    monolithic$summary[, summary_columns, drop = FALSE],
    sharded_summary[, summary_columns, drop = FALSE],
    tolerance = 1e-12
  )
})


test_that("Study 2 explicit replication seeds reproduce deterministic shards", {
  methods <- c("cr2", "cats")

  monolithic <- pwr_func_study2(
    n_clusters = 6,
    cluster_size = 20,
    beta = 0.10,
    random_slope_sd = 0.10,
    contamination = "vertical",
    contamination_size = 6,
    reps = 4,
    alpha = 0.05,
    methods = methods,
    seed = 20261021L,
    keep_replicates = TRUE
  )

  seeds <- monolithic$settings$replicate_seeds

  first <- pwr_func_study2(
    n_clusters = 6,
    cluster_size = 20,
    beta = 0.10,
    random_slope_sd = 0.10,
    contamination = "vertical",
    contamination_size = 6,
    reps = 2,
    alpha = 0.05,
    methods = methods,
    keep_replicates = TRUE,
    replicate_seeds = seeds[1:2]
  )

  second <- pwr_func_study2(
    n_clusters = 6,
    cluster_size = 20,
    beta = 0.10,
    random_slope_sd = 0.10,
    contamination = "vertical",
    contamination_size = 6,
    reps = 2,
    alpha = 0.05,
    methods = methods,
    keep_replicates = TRUE,
    replicate_seeds = seeds[3:4]
  )

  sharded <- bind_two_test_shards(first, second)

  expect_equal(
    drop_runtime_for_shard_test(
      sort_shard_test_replicates(monolithic$replicates)
    ),
    drop_runtime_for_shard_test(
      sort_shard_test_replicates(sharded)
    ),
    tolerance = 1e-12
  )

  sharded_summary <- study1_summarize_results(
    replicate_results = sharded,
    methods = methods,
    reps = 4L
  )

  summary_columns <- setdiff(
    names(monolithic$summary),
    "mean_runtime_sec"
  )

  expect_equal(
    monolithic$summary[, summary_columns, drop = FALSE],
    sharded_summary[, summary_columns, drop = FALSE],
    tolerance = 1e-12
  )
})


test_that("explicit replication-seed input is validated", {
  expect_error(
    pwr_func_study1(
      n_clusters = 5,
      cluster_size = 20,
      reps = 2,
      methods = "cr2",
      seed = 1L,
      replicate_seeds = c(10L, 11L)
    ),
    "seed must be NULL"
  )

  expect_error(
    pwr_func_study1(
      n_clusters = 5,
      cluster_size = 20,
      reps = 2,
      methods = "cr2",
      replicate_seeds = c(10L, 10L)
    ),
    "unique"
  )

  expect_error(
    pwr_func_study2(
      n_clusters = 5,
      cluster_size = 20,
      reps = 2,
      methods = "cr2",
      replicate_seeds = 10L
    ),
    "length reps"
  )
})
