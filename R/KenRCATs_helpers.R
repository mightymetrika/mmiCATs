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
      shiny::numericInput("N", "Number of Groups (N)", 25)
    ),
    shiny::tags$div(
      title = "Integer specifying the number of replications for the simulation.",
      shiny::numericInput("reps", "Number of Replications", 100)
    ),
    shiny::tags$div(
      title = "Significance level for hypothesis testing.",
      shiny::numericInput("alpha", "Alpha", 0.05, min = 0, max = 1, step = 0.01)
    ),
    shiny::tags$div(
      title = "Variable of interest (for power calculations).",
      shiny::textInput("var_intr", "Variable of Interest", 'x1')
    ),
    shiny::tags$div(
      title = "Name of the grouping variable.",
      shiny::textInput("grp", "Grouping Variable", 'ID')
    ),
    shiny::tags$div(
      title = "Formula for the mixed-effects model.",
      shiny::textInput("mod", "Mixed-Effects Model Formula", 'out ~ x1 + x2 + x3 + (1|ID)')
    ),
    shiny::tags$div(
      title = "Formula for the CATs model.",
      shiny::textInput("catsmod", "CATs Model Formula", 'out ~ x1 + x2 + x3')
    ),
    shiny::tags$div(
      title = "Name of the random slope variable.",
      shiny::textInput("r_slope", "Random Slope Variable", 'x1')
    ),
    shiny::tags$div(
      title = "Name of the random intercept.",
      shiny::textInput("r_int", "Random Intercept", 'int')
    ),
    shiny::tags$div(
      title = "Number of time points per group.",
      shiny::numericInput("n_time", "Number of Time Points", 20)
    ),
    shiny::tags$div(
      title = "Mean for the random intercept.",
      shiny::numericInput("mean_i", "Mean of Random Intercept", 0)
    ),
    shiny::tags$div(
      title = "Variance for the random intercept.",
      shiny::numericInput("var_i", "Variance of Random Intercept", 1)
    ),
    shiny::tags$div(
      title = "Mean for the random slope.",
      shiny::numericInput("mean_s", "Mean of Random Slope", 0)
    ),
    shiny::tags$div(
      title = "Variance for the random slope.",
      shiny::numericInput("var_s", "Variance of Random Slope", 1)
    ),
    shiny::tags$div(
      title = "Covariance between the random intercept and slope.",
      shiny::numericInput("cov_is", "Covariance of Intercept and Slope", 0)
    ),
    shiny::tags$div(
      title = "Mean for the residual error.",
      shiny::numericInput("mean_r", "Mean of Residual Error", 0)
    ),
    shiny::tags$div(
      title = "Variance for the residual error.",
      shiny::numericInput("var_r", "Variance of Residual Error", 1)
    ),
    shiny::tags$div(
      title = "Correlation matrix for correlated predictors, if any",
      shiny::textInput("cor_mat", "Cor Matrix", "")
    ),
    shiny::tags$div(
      title = "List of vectors, each vector containing names of correlated variables.",
      shiny::textInput("corvars", "Correlated Variables", "")
    )
  )
}

parse_list_input <- function(input_string) {
  parsed <- eval(parse(text = paste0("list(", input_string, ")")))
  return(parsed)
}

# Handle null values for text to vector
vec_null <- function(par_input = "") {
  if (is.na(par_input) || par_input == "") {
    return(NULL)
  } else {
    return(text_to_vector(par_input))
  }
}

list_null <- function(par_input = "") {
  if (is.na(par_input) || par_input == "") {
    return(NULL)
  } else {
    return(parse_list_input(par_input))
  }
}


# Handle null values for sigma matrix
sig_null <- function(par_input = "") {
  if (is.na(par_input) || par_input == "") {
    return(NULL)
  } else {
    return(eval(parse(text = par_input)))
  }
}

# Helper function to
text_to_vector <- function(text_input) {
  as.numeric(unlist(strsplit(text_input, ",")))
}
