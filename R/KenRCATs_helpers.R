getUIParams <- function() {
  shiny::tagList(
    shiny::tags$div(
      title = "Named list of true coefficient values for the fixed effects.",
      shiny::textInput("betas", "Coefficients", 'int = 0, x1 = -5, x2 = 2, x3 = 10')
    ),
    shiny::tags$div(
      title = "Named list of functions to generate random distributions for each predictor.",
      shiny::textInput("dists", "Distributions", 'x1 = stats::rnorm, x2 = stats::rbinom, x3 = stats::rnorm')
    ),
    shiny::tags$div(
      title = "Named list of parameter lists for each distribution function in `dists`.",
      shiny::textInput("distpar", "Distribution Parameters", 'x1 = list(mean = 0, sd = 1), x2 = list(size = 1, prob = 0.4), x3 = list(mean = 1, sd = 2)')
    ),
    shiny::tags$div(
      title = "Integer specifying the number of groups.",
      shiny::numericInput("N", "N", 25)
    )
  )
}

parse_input <- function(input_string) {
  parsed <- eval(parse(text = paste0("list(", input_string, ")")))
  return(parsed)
}
