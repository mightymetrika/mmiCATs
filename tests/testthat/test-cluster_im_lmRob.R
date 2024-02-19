# return.vcv = TRUE

test_that("cluster_im_lmRob works with the robust engine", {
  # Run robust model
  robout <- robust::lmRob(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)

  # Fit individual models
  imout <- cluster_im_lmRob(robout,
                            formula = Sepal.Length ~ Petal.Length + Petal.Width,
                            dat = iris,
                            cluster = ~ Species,
                            return.vcv = TRUE,
                            engine = "robust")

  # Test length of imout$p.values
  expect_equal(length(imout$p.values), 3)

  # Test dimensions of imout$ci
  expect_equal(ncol(imout$ci), 2)
  expect_equal(nrow(imout$ci), 3)

  # Test length of imout$beta.bar
  expect_equal(length(imout$beta.bar), 3)

  # Test that the output is of length 4
  expect_equal(length(imout), 4)

  # # Test the p.value for Petal.Width
  # expect_true(imout$p.values[3] > 0.71 & imout$p.values[3] < 0.72)
  #
  # # Test the lower ci for Petal.Length
  # expect_true(imout$ci[2,1] > 0.27 & imout$ci[2,1] < 0.28)
  #
  # # Test the upper ci for Petal.Length
  # expect_true(imout$ci[2,2] > 1.40 & imout$ci[2,2] < 1.41)
  #
  # # Test the beta.bar for Petal.Width
  # expect_true(imout$beta.bar[[3]] > 0.12 & imout$beta.bar[[3]] < 0.13)
  #
  # # Test the length of the cluster_im_lmRob output (with return.vcv = TRUE)
  # expect_equal(length(imout), 4)

})

test_that("cluster_im_lmRob works with the robustbase engine", {
  # Run robust model
  robout <- robustbase::lmrob(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)

  # Fit individual models
  imout <- cluster_im_lmRob(robout,
                            formula = Sepal.Length ~ Petal.Length + Petal.Width,
                            dat = iris,
                            cluster = ~ Species,
                            return.vcv = TRUE,
                            engine = "robustbase")

  # Test length of imout$p.values
  expect_equal(length(imout$p.values), 3)

  # Test dimensions of imout$ci
  expect_equal(ncol(imout$ci), 2)
  expect_equal(nrow(imout$ci), 3)

  # Test length of imout$beta.bar
  expect_equal(length(imout$beta.bar), 3)

  # Test that the output is of length 4
  expect_equal(length(imout), 4)

  # # Test the p.value for Petal.Width
  # expect_true(imout$p.values[3] > 0.72 & imout$p.values[3] < 0.73)
  #
  # # Test the lower ci for Petal.Length
  # expect_true(imout$ci[2,1] > 0.09 & imout$ci[2,1] < 0.10)
  #
  # # Test the upper ci for Petal.Length
  # expect_true(imout$ci[2,2] > 1.50 & imout$ci[2,2] < 1.51)
  #
  # # Test the beta.bar for Petal.Width
  # expect_true(imout$beta.bar[[3]] > 0.11 & imout$beta.bar[[3]] < 0.12)
  #
  # # Test the length of the cluster_im_lmRob output (with return.vcv = TRUE)
  # expect_equal(length(imout), 4)

})

# return.vcv = FALSE

test_that("cluster_im_lmRob works with the robust engine and return.vcv = FALSE", {
  # Run robust model
  robout <- robust::lmRob(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)

  # Fit individual models
  imout <- cluster_im_lmRob(robout,
                            formula = Sepal.Length ~ Petal.Length + Petal.Width,
                            dat = iris,
                            cluster = ~ Species,
                            return.vcv = FALSE,
                            engine = "robust")

  # Test length of imout$p.values
  expect_equal(length(imout$p.values), 3)

  # Test dimensions of imout$ci
  expect_equal(ncol(imout$ci), 2)
  expect_equal(nrow(imout$ci), 3)

  # Test length of imout$beta.bar
  expect_true(is.null(imout$beta.bar))

  # Test that the output is of length 4
  expect_equal(length(imout), 4)

})

test_that("cluster_im_lmRob works with the robustbase engine and return.vcv - FALSE", {
  # Run robust model
  robout <- robustbase::lmrob(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)

  # Fit individual models
  imout <- cluster_im_lmRob(robout,
                            formula = Sepal.Length ~ Petal.Length + Petal.Width,
                            dat = iris,
                            cluster = ~ Species,
                            return.vcv = FALSE,
                            engine = "robustbase")

  # Test length of imout$p.values
  expect_equal(length(imout$p.values), 3)

  # Test dimensions of imout$ci
  expect_equal(ncol(imout$ci), 2)
  expect_equal(nrow(imout$ci), 3)

  # Test length of imout$beta.bar
  expect_true(is.null(imout$beta.bar))

  # Test that the output is of length 4
  expect_equal(length(imout), 4)

})
