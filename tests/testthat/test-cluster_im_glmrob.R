# return.vcv = TRUE
test_that("cluster_im_glmRob works with the robust engine", {
  mtcars2 <- mtcars

  # Create a binary variable for MPG (e.g., MPG > 20)
  mtcars2$high_mpg = as.factor(ifelse(mtcars2$mpg > 20, 1, 0))
  mtcars2$cyl_f <- as.factor(mtcars2$cyl)

  robout <- robust::glmRob(formula = high_mpg ~ disp + wt, data = mtcars2)
  imout <- cluster_im_glmRob(robout, dat = mtcars2, ~cyl_f, return.vcv = TRUE,
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

})

test_that("cluster_im_glmRob works with the robustbase engine", {
  iris_bin <- iris

  # Create a binary variable for MPG (e.g., MPG > 20)
  iris_bin$high_Sepal.Length = as.factor(ifelse(iris_bin$Sepal.Length > 5.8, 1, 0))


  robout <- robustbase::glmrob(formula = high_Sepal.Length ~ Petal.Length + Petal.Width,
                               family = binomial,
                               data = iris_bin)
  imout <- cluster_im_glmRob(robout, dat = iris_bin, ~Species, return.vcv = TRUE,
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

})

# return.vcv = FALSE
test_that("cluster_im_glmRob works with the robust engine and return.cvc = FALSE", {
  mtcars2 <- mtcars

  # Create a binary variable for MPG (e.g., MPG > 20)
  mtcars2$high_mpg = as.factor(ifelse(mtcars2$mpg > 20, 1, 0))
  mtcars2$cyl_f <- as.factor(mtcars2$cyl)

  robout <- robust::glmRob(formula = high_mpg ~ disp + wt, data = mtcars2)
  imout <- cluster_im_glmRob(robout, dat = mtcars2, ~cyl_f, return.vcv = FALSE,
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

test_that("cluster_im_glmRob works with the robustbase engine and return.vcv = FALSE", {
  iris_bin <- iris

  # Create a binary variable for MPG (e.g., MPG > 20)
  iris_bin$high_Sepal.Length = as.factor(ifelse(iris_bin$Sepal.Length > 5.8, 1, 0))


  robout <- robustbase::glmrob(formula = high_Sepal.Length ~ Petal.Length + Petal.Width,
                               family = binomial,
                               data = iris_bin)
  imout <- cluster_im_glmRob(robout, dat = iris_bin, ~Species, return.vcv = FALSE,
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
