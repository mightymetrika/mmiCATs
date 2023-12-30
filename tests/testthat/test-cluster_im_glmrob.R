test_that("cluster_im_glmRob works with the robust engine", {
  mtcars2 <- mtcars

  # Create a binary variable for MPG (e.g., MPG > 20)
  mtcars2$high_mpg = as.factor(ifelse(mtcars2$mpg > 20, 1, 0))
  mtcars2$cyl_f <- as.factor(mtcars2$cyl)

  robout <- robust::glmRob(formula = high_mpg ~ disp + wt, data = mtcars2)
  imout <- cluster_im_glmRob(robout, dat = mtcars2, ~cyl_f, return.vcv = TRUE,
                             engine = "robust")

  # Test that the p.value for wt is between 0.42 and 0.43
  expect_true(imout$p.values[3] > 0.42 & imout$p.values[3] < 0.43)

  # Test that beta.bar for disp is between 1.2 and 1.3
  expect_true(imout$beta.bar[[2]] > 1.2 & imout$beta.bar[[2]] < 1.3)

  # Test that the output is of length 4 when return.vcv = TRUE
  expect_equal(length(imout), 4)

})

test_that("cluster_im_glmRob works with the robust engine", {
  iris_bin <- iris

  # Create a binary variable for MPG (e.g., MPG > 20)
  iris_bin$high_Sepal.Length = as.factor(ifelse(iris_bin$Sepal.Length > 5.8, 1, 0))


  robout <- robustbase::glmrob(formula = high_Sepal.Length ~ Petal.Length + Petal.Width,
                               family = binomial,
                               data = iris_bin)
  imout <- cluster_im_glmRob(robout, dat = iris_bin, ~Species, return.vcv = TRUE,
                             engine = "robustbase")

  # Test that the p.value for Petal.Width is between 0.97 and 0.98
  expect_true(imout$p.values[3] > 0.97 & imout$p.values[3] < 0.98)

  # Test that beta.bar for Petal.Length is between 6.1 and 6.2
  expect_true(imout$beta.bar[[2]] > 6.1 & imout$beta.bar[[2]] < 6.2)

  # Test that the output is of length 4 when return.vcv = TRUE
  expect_equal(length(imout), 4)

})
