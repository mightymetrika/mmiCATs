test_that("Study 3 cross-dataset plot uses current horizontal errorbar argument", {
  body_text <- paste(
    deparse(
      body(
        mmiCATs:::study3c_cross_dataset_plot
      ),
      width.cutoff = 500L
    ),
    collapse = "\n"
  )

  expect_true(
    grepl(
      "width = 0",
      body_text,
      fixed = TRUE
    )
  )

  expect_false(
    grepl(
      "height = 0",
      body_text,
      fixed = TRUE
    )
  )

  expect_true(
    grepl(
      'orientation = "y"',
      body_text,
      fixed = TRUE
    )
  )
})
