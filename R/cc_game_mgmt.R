deal_cards_to_cc_grid <- function(deck = mmcards::i_deck(deck = mmcards::shuffle_deck(),
                                                         i_path = "www",
                                                         i_names = c("2_of_clubs", "2_of_diamonds", "2_of_hearts", "2_of_spades",
                                                                     "3_of_clubs", "3_of_diamonds", "3_of_hearts", "3_of_spades",
                                                                     "4_of_clubs", "4_of_diamonds", "4_of_hearts", "4_of_spades",
                                                                     "5_of_clubs", "5_of_diamonds", "5_of_hearts", "5_of_spades",
                                                                     "6_of_clubs", "6_of_diamonds", "6_of_hearts", "6_of_spades",
                                                                     "7_of_clubs", "7_of_diamonds", "7_of_hearts", "7_of_spades",
                                                                     "8_of_clubs", "8_of_diamonds", "8_of_hearts", "8_of_spades",
                                                                     "9_of_clubs", "9_of_diamonds", "9_of_hearts", "9_of_spades",
                                                                     "10_of_clubs", "10_of_diamonds", "10_of_hearts", "10_of_spades",
                                                                     "jack_of_clubs", "jack_of_diamonds", "jack_of_hearts", "jack_of_spades",
                                                                     "queen_of_clubs", "queen_of_diamonds", "queen_of_hearts", "queen_of_spades",
                                                                     "king_of_clubs", "king_of_diamonds", "king_of_hearts", "king_of_spades",
                                                                     "ace_of_clubs", "ace_of_diamonds", "ace_of_hearts", "ace_of_spades"
                                                         )), n) {

  # Ensure there are enough cards in the deck to deal
  if(nrow(deck) < 2*n) {
    stop("Not enough cards in the deck to deal")
  }

  # Deal 2*n cards from the deck
  dealt_cards <- vector("list", 2*n)

  for(i in 1:(2*n)) {
    deck <- mmcards::deal_card(deck)
    dealt_cards[[i]] <- deck$dealt_card
  }

  # Reshape the data frame to the desired format
  cards_matrix <- matrix(dealt_cards, nrow = 2, ncol = n, byrow = TRUE)

  # Return results
  return(cards_matrix)
}

cc_swapper <- function(cards_matrix, swap_in_col = NULL) {

  if (!is.null(swap_in_col) && swap_in_col == 2){
    cards_matrix[, swap_in_col] <- rev(cards_matrix[, swap_in_col])
  }

  return(cards_matrix)
}

process_hand <- function(x, process_col,
                         beta_int = 0,
                         beta_x1 = 0.25,
                         beta_x2 = 1.5,
                         mean_x1 = 0,
                         sd_x1 = 1,
                         mean_x2 = 0,
                         sd_x2 = 4,
                         N = 20,
                         alpha = 0.05,
                         n_time = 20,
                         mean_i = 0,
                         var_i = 1,
                         mean_s = 0,
                         mean_r = 0,
                         var_r = 1,
                         cor_pred = NULL,
                         truncate = FALSE,
                         var_s_factor = 1,
                         cov_is_factor = 14.75){

  # Set correlation between x1 and x2
  if (is.null(cor_pred)){
    corvars <- NULL
    cor_mat <- NULL
  } else {
    corvars <- list(c("x1", "x2"))
    cor_mat <- matrix(c(1,cor_pred,cor_pred,1), 2, 2)

  }

  # Extract random slope variance and covariance
  var_s <- x[[1, process_col]]$value/var_s_factor
  cov_is <- x[[2, process_col]]$value/cov_is_factor

  # Generate data and fit models
  pwr_out <- pwr_func_lmer(betas = list("int" = beta_int, "x1" = beta_x1, "x2" = beta_x2),
                           dists = list("x1" = stats::rnorm, "x2" = stats::rnorm),
                           distpar = list("x1" = list(mean = mean_x1, sd = sd_x1),
                                          "x2" = list(mean = mean_x2, sd = sd_x2)),
                           N = N,
                           reps = 1,
                           alpha = alpha,
                           var_intr = "x1",
                           grp = "ID",
                           mod = paste0("out ~ x1 + x2 + (x2|ID)"),
                           catsmod = "out ~ x1 + x2",
                           r_slope = "x2",
                           r_int = "int",
                           n_time = n_time,
                           mean_i = mean_i,
                           var_i = var_i,
                           mean_s = mean_s,
                           var_s = var_s,
                           cov_is = cov_is,
                           mean_r = mean_r,
                           var_r = var_r,
                           cor_mat = cor_mat,
                           corvars = corvars)

  # Extract mispecification distance
  if (truncate == TRUE){
    mispec_dist <-pwr_out[pwr_out$model == "cats_trunc",][[5]] - pwr_out[pwr_out$model == "ri",][[5]]
  } else {
    mispec_dist <- pwr_out[pwr_out$model == "cats",][[5]] - pwr_out[pwr_out$model == "ri",][[5]]
  }

  # Extract wanted columns from pwr_func_lmer output
  results <- pwr_out[1:4, c(1,2,5,6,8)]

  # Return results
  return(list(mispec_dist =  mispec_dist,
              results = results))
}

render_card_grid <- function(new_card_grid) {
  rep_card_images <- unlist(apply(new_card_grid, 1, function(row) sapply(row, function(card) {
    shiny::renderImage({
      list(src = system.file(card$icard, package = "mmiCATs"), contentType = "image/png", width = 200, height = "auto")
    }, deleteFile = FALSE)
  })))

  rep_matrix_layout <- matrix(rep_card_images, nrow = 2, byrow = TRUE)
  card_ui <- apply(rep_matrix_layout, 1, function(row) {
    shiny::fluidRow(lapply(row, shiny::column, width = floor(12/length(row))))
  })
  return(card_ui)
}
