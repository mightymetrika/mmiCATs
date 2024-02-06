#' Deal Cards to CloseCATs Game Grid
#'
#' This internal function deals cards to a 2 x 2 grid for the CloseCATs game.
#' The game involves a deck of cards, where cards are dealt to both the computer
#' and the player. The matrix format of the game grid is such that column 1 is
#' for the computer, and column 2 is for the player. The first row of cards
#' contributes to the random slope variance, and the second row contributes to
#' the covariance between the random slope and the random intercept.
#'
#' @param deck A dataframe representing a deck of cards. If not provided, a
#'    shuffled deck is generated using `mmcards::i_deck()` and
#'    `mmcards::shuffle_deck()`. The deck should contain at least 2*n cards.
#' @param n The number of players. It defines the number of columns in the grid.
#'    Each player, including the computer, will be dealt two cards.
#'
#' @return A matrix representing the game grid with dealt cards. Each cell of the
#'    matrix contains a card dealt to either the computer (column 1) or the player
#'    (column 2).
#'
#' @keywords internal
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

#' Column Swap in CloseCATs Game Grid
#'
#' This internal function performs a column swap in the CloseCATs game grid.
#' It is designed to allow the player to swap the cards in their column (column 2).
#' This swap changes the contribution of the cards to the random slope variance
#' and the covariance between the random slope and the random intercept. The
#' function modifies the order of cards in the specified column by reversing their
#' positions.
#'
#' @param cards_matrix A matrix representing the current state of the game grid.
#'    The matrix should have 2 rows and a number of columns equal to the number
#'    of players (including the computer). Each cell of the matrix contains a card.
#' @param swap_in_col The column number where the swap should be performed. If
#'    this parameter is NULL or not equal to 2, no action is taken. The default
#'    value is NULL. Typically, this parameter should be set to 2 to perform a
#'    swap for the player's column.
#'
#' @return The modified game grid matrix after performing the swap in the specified
#'    column.
#'
#' @keywords internal
cc_swapper <- function(cards_matrix, swap_in_col = NULL) {

  if (!is.null(swap_in_col) && swap_in_col == 2){
    cards_matrix[, swap_in_col] <- rev(cards_matrix[, swap_in_col])
  }

  return(cards_matrix)
}

#' Process Hand and Calculate Mispecification Distance in CloseCATs Game
#'
#' This function processes hands in the CloseCATs game and calculates the
#' mispecification distance. It performs statistical computations based on the
#' dealt cards and specified parameters, evaluating the performance of mixed effects
#' models and cluster adjusted t-statistics models in the context of the game.
#' The function considers various statistical parameters and model specifications
#' to compute the results.
#'
#' @param x A matrix representing the current hand in the game grid.
#' @param process_col The column number (1 for computer, 2 for player) to process.
#' @param beta_int Intercept for the mixed effects model.
#' @param beta_x1 Coefficient for the first predictor in the mixed effects model.
#' @param beta_x2 Coefficient for the second predictor in the mixed effects model.
#' @param mean_x1 Mean of the first predictor.
#' @param sd_x1 Standard deviation of the first predictor.
#' @param mean_x2 Mean of the second predictor.
#' @param sd_x2 Standard deviation of the second predictor.
#' @param N Number of observations.
#' @param reps Number of replications for the power analysis.
#' @param alpha Significance level for the power analysis.
#' @param n_time Number of time points.
#' @param mean_i Mean of the random intercept.
#' @param var_i Variance of the random intercept.
#' @param mean_s Mean of the random slope.
#' @param mean_r Mean of the residual.
#' @param var_r Variance of the residual.
#' @param cor_pred Correlation predictor, NULL if not specified.
#' @param truncate Boolean to determine if truncation is applied in the model.
#' @param var_s_factor Factor to adjust the variance of the random slope.
#' @param cov_is_factor Factor to adjust the covariance between the random intercept
#'    and slope.
#'
#' @return A list containing two elements: 'mispec_dist', the mispecification
#'    distance, and 'results', a summary of model results and their statistical
#'    parameters.
#'
#' @keywords internal
process_hand <- function(x, process_col,
                         beta_int = 0,
                         beta_x1 = 0.25,
                         beta_x2 = 1.5,
                         mean_x1 = 0,
                         sd_x1 = 1,
                         mean_x2 = 0,
                         sd_x2 = 4,
                         N = 20,
                         reps = 1,
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
                           reps = reps,
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
  results <- pwr_out[1:4, c(1,2,5,6,8,9)]

  # Return results
  return(list(mispec_dist =  mispec_dist,
              results = results))
}

#' Render Card Grid in Shiny App
#'
#' This function takes a grid of card information, generates image tags for
#' each card, and organizes them into a responsive grid layout for display in a
#' Shiny application.
#'
#' @param new_card_grid A matrix or data frame where each row represents a card and
#' each card has a property `icard` pointing to the image file relative to the
#' `mmiCATs` package's `www` directory. The function expects this parameter to be
#' structured with named columns where `icard` is one of the column names.
#'
#' @return A Shiny UI element (`tagList`) representing a grid of card images.
#'
#' @keywords internal
render_card_grid <- function(new_card_grid) {

  rep_card_images <- unlist(apply(new_card_grid, 1, function(row) sapply(row, function(card) {
    shiny::renderImage({
      list(src = system.file(card$icard, package = "mmiCATs"), contentType = "image/png", width = 200, height = "auto")
    }, deleteFile = FALSE)
  })))

  rep_matrix_layout <- matrix(rep_card_images, nrow = 2, byrow = TRUE)
  card_ui <- apply(rep_matrix_layout, 1, function(row) {
    shiny::fluidRow(lapply(row, shiny::column, width = floor(10/length(row))))
  })
  return(card_ui)
}
