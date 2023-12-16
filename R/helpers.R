#' Convert a Character String to a List
#'
#' This internal function converts a character string representing R arguments
#' into a list of arguments. It is primarily used to facilitate the passing of
#' additional arguments from the Shiny UI to internal functions within the app.
#'
#' @param arg_str A character string representing R arguments.
#'
#' @return A list containing the arguments represented by \code{arg_str}. If
#'   \code{arg_str} is not a valid representation of R arguments, the function
#'   will throw an error.
#'
#' @keywords internal
str2list <- function(arg_str) {
  # Evaluate the string in a new environment to convert it to a list
  eval(parse(text = paste0("list(", arg_str, ")")), envir = new.env())
}
