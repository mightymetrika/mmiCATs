#' Generate UI Parameters for KenRCATs Shiny App
#'
#' This internal function creates a list of UI elements for the KenRCATs() 'shiny'
#' application. It generates input fields for various parameters used in the power
#' analysis simulation.
#'
#' @return A shiny::tagList containing UI elements.
#'
#' @note This function is intended for internal use within the KenRCATs() 'shiny'
#' application.
#'
#' @keywords internal
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
      title = "Number of observations per group. Enter an integer or a comma-separated list of N integers.",
      shiny::textInput("n_time", "Observations per Group", "20")
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
      title = "Enter 'linear', leave blank for no time trend, or provide a custom function (e.g., function(n) sqrt(seq_len(n)))",
      shiny::textInput("time_index", "Time Index Variable", "")
    ),
    shiny::tags$div(
      title = "Formula for fitting mixed-effects model to simulated data.",
      shiny::textInput("mod", "Mixed-Effects Model Formula", 'out ~ x1 + x2 + x3 + (x3|ID)')
    ),
    shiny::tags$div(
      title = "Formula for fiiting the CATs model to simulated data.",
      shiny::textInput("catsmod", "CATs Model Formula", 'out ~ x1 + x2 + x3')
    ),
    shiny::tags$div(
      title = "Name of the random slope variable for simulating data.",
      shiny::textInput("r_slope", "Random Slope Variable", 'x3')
    ),
    shiny::tags$div(
      title = "Name of the random intercept.",
      shiny::textInput("r_int", "Random Intercept", 'int')
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
    ),
    shiny::tags$div(
      title = "Integer specifying the number of replications for the simulation.",
      shiny::numericInput("reps", "Number of Replications", 20)
    )
  )
}

#' Parse String Input to List
#'
#' @param input_string A string representation of a list.
#' @return A parsed list object.
#' @keywords internal
parse_list_input <- function(input_string) {
  parsed <- eval(parse(text = paste0("list(", input_string, ")")))
  return(parsed)
}

#' Handle Null Values for Text to Vector Conversion
#'
#' @param par_input A string input, default is "".
#' @return NULL if input is NA or empty, otherwise a vector.
#' @keywords internal
vec_null <- function(par_input = "") {
  if (is.na(par_input) || par_input == "") {
    return(NULL)
  } else {
    return(text_to_vector(par_input))
  }
}

#' Handle Null Values for List Input
#'
#' @param par_input A string input, default is "".
#' @return NULL if input is NA or empty, otherwise a parsed list.
#' @keywords internal
list_null <- function(par_input = "") {
  if (is.na(par_input) || par_input == "") {
    return(NULL)
  } else {
    return(parse_list_input(par_input))
  }
}


#' Handle Null Values for Sigma Matrix
#'
#' @param par_input A string input, default is "".
#' @return NULL if input is NA or empty, otherwise an evaluated expression.
#' @keywords internal
sig_null <- function(par_input = "") {
  if (is.na(par_input) || par_input == "") {
    return(NULL)
  } else {
    return(eval(parse(text = par_input)))
  }
}

#' Convert Text Input to Vector
#'
#' @param text_input A string input to be converted to a vector.
#' @return A vector parsed from the input string.
#' @keywords internal
text_to_vector <- function(text_input) {
  # Check if the input is a simple comma-separated string
  if (!grepl("^[^,]+$", text_input) && !grepl("[()]", text_input)) {
    text_input <- paste0("c(", text_input, ")")
  }
  eval(parse(text = text_input))
}

#' Append KenRCATs Results with Input Parameters
#'
#' @param df A data frame containing simulation results.
#' @param input A list of input parameters from the Shiny app.
#' @return A list containing two data frames: the original results and the results
#' with appended input parameters.
#' @keywords internal
append_KenRCATs <- function(df, input) {

  # Helper function to generate a unique run code
  generateRunCode <- function() {
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    random_string <- paste(sample(c(letters, LETTERS, 0:9), 5, replace = TRUE), collapse = "")
    paste0(timestamp, "_", random_string)
  }

  # Generate a unique code for the simulation run
  run_code <- generateRunCode()

  # Add the RunCode to the original df
  df$run_code <- run_code

  # Create a data frame of input parameters
  params_df <- data.frame(
    betas = input$betas,
    dists = input$dists,
    distpar = input$distpar,
    N = input$N,
    reps = input$reps,
    alpha = input$alpha,
    var_intr = input$var_intr,
    grp = input$grp,
    mod = input$mod,
    catsmod = input$catsmod,
    r_slope = input$r_slope,
    r_int = input$r_int,
    n_time = input$n_time,
    mean_i = input$mean_i,
    var_i = input$var_i,
    mean_s = input$mean_s,
    var_s = input$var_s,
    cov_is = input$cov_is,
    mean_r = input$mean_r,
    var_r = input$var_r,
    cor_mat = if (!is.null(input$cor_mat) && input$cor_mat != "") input$cor_mat else NA,
    corvars = if (!is.null(input$corvars) && input$corvars != "") input$corvars else NA,
    time_index = if (!is.null(input$time_index) && input$time_index != "" && input$time_index != "NA") input$time_index else NA,
    stringsAsFactors = FALSE
  )

  # Repeat the parameters data frame to match the number of rows in df
  params_df <- params_df[rep(1, nrow(df)), ]

  # Combine with the simulation results
  params_df <- cbind(df, params_df)

  return(list(res = df,
              input_params = params_df))
}

