% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cc_game_mgmt.R
\name{cc_swapper}
\alias{cc_swapper}
\title{Column Swap in CloseCATs Game Grid}
\usage{
cc_swapper(cards_matrix, swap_in_col = NULL)
}
\arguments{
\item{cards_matrix}{A matrix representing the current state of the game grid.
The matrix should have 2 rows and a number of columns equal to the number
of players (including the computer). Each cell of the matrix contains a card.}

\item{swap_in_col}{The column number where the swap should be performed. If
this parameter is NULL or not equal to 2, no action is taken. The default
value is NULL. Typically, this parameter should be set to 2 to perform a
swap for the player's column.}
}
\value{
The modified game grid matrix after performing the swap in the specified
column.
}
\description{
This internal function performs a column swap in the CloseCATs game grid.
It is designed to allow the player to swap the cards in their column (column 2).
This swap changes the contribution of the cards to the random slope variance
and the covariance between the random slope and the random intercept. The
function modifies the order of cards in the specified column by reversing their
positions.
}
\keyword{internal}
