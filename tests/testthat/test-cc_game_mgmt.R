test_that("close cats game flow works", {
  cc_matrix <- deal_cards_to_cc_grid(n=2)
  cc_matrix <- cc_swapper(cc_matrix, 2)
  hand1 <- process_hand(cc_matrix, 1)

  expect_true(all(names(hand1) == c("mispec_dist", "results")))

  # boundary (singular) fit: see help('isSingular')
  # Warning messages:
  #   1: In test.lmRob(object) :
  #   Denominator smaller than tl= 1e-06  in test for bias.
  # 2: In is.na(fit) : is.na() applied to non-(list or vector) of type 'S4'
  # 3: In is.na(fit) : is.na() applied to non-(list or vector) of type 'S4'
})
