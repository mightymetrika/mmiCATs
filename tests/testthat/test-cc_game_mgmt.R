test_that("close cats game flow works", {
  cc_matrix <- deal_cards_to_cc_grid(n=2)
  cc_matrix <- cc_swapper(cc_matrix, 2)
  hand1 <- process_hand(cc_matrix, 1)

  expect_true(all(names(hand1) == c("lme_coef", "mod_coef", "mispec_dist")))
})
