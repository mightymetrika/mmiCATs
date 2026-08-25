# Run definitive sharding unit tests outside the installed-package test suite.
#
# The sharding engine is manuscript execution infrastructure stored under
# data-raw/, which is intentionally excluded from R CMD build by .Rbuildignore.
# These tests therefore belong with the data-raw validation infrastructure,
# not tests/testthat/ for the installed package.

library(devtools)
library(testthat)

load_all()

source(
  "data-raw/definitive_sharding_helpers.R"
)

test_file(
  "data-raw/definitive-sharding-tests/test-definitive-sharding-runner.R",
  stop_on_failure = TRUE
)
