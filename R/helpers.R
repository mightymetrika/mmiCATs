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

#' Format Citation
#'
#' This internal function formats a citation object into a readable string.
#' The function extracts relevant information such as the title, author,
#' year, address, note, and URL from the citation object and formats it into a
#' standardized citation format.
#'
#' @param cit A citation object typically obtained from `citation()`.
#'
#' @return A character string with the formatted citation.
#'
#' @keywords internal
format_citation <- function(cit) {
  title <- cit$title
  author <- if (is.null(cit$author)) {
    cit$organization
  } else {
    paste(sapply(cit$author, function(a) paste(a$given, a$family)), collapse = ", ")
  }
  year <- cit$year
  address <- cit$address
  url <- cit$url
  note <- cit$note

  formatted_cit <- paste0(
    author, " (", year, "). ",
    title, ". ",
    note, ", ",
    "Retrieved from ", url, ". ",
    address
  )

  formatted_cit
}
