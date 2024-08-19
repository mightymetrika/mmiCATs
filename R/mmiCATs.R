#' Set Up CATs Analysis in Shiny Application
#'
#' This function creates a Shiny application for performing CATs (Cluster-Adjusted
#' t-statistics) analysis. It provides a user interface for uploading a CSV file,
#' specifying the model and additional arguments, and running the analysis. The
#' output includes variable selection, GLM (Generalized Linear Model) summary,
#' and results of the CATs analysis.
#'
#' @details
#' The application allows the user to upload a dataset, specify a GLM model and
#' additional arguments, and run CATs analysis. The UI consists of various input
#' elements like file upload, text input, numeric input, and action buttons. The
#' server part handles the data processing, model fitting, and execution of the
#' CATs analysis. The application outputs include the list of variables,
#' GLM model summary, and the results from the CATs analysis.
#'
#' @return A Shiny app object which can be run to start the application.
#'
#' @examples
#' # To run the Shiny app
#' if(interactive()){
#'   mmiCATs()
#' }
#'
#' @references
#' Esarey J, Menger A. Practical and Effective Approaches to Dealing With Clustered
#' Data. Political Science Research and Methods. 2019;7(3):541-559.
#' doi:10.1017/psrm.2017.42
#'
#' @export
mmiCATs <- function(){
  # UI
  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("lumen"),
    shiny::titlePanel("Set Up CATs Analysis"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::tags$div(
          title = "Upload a CSV file with your data. The file should have headers corresponding to variable names.",
          shiny::fileInput("datafile", "Choose CSV File",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv"))
        ),
        shiny::uiOutput("model_input_ui"),
        shiny::br(),  # Add a line break
        shiny::uiOutput("cluster_input_ui"),
        shiny::br(),  # Add a line break
        shiny::actionButton("show_citations", "Citations")
      ),
      shiny::mainPanel(
        shiny::uiOutput("variables_title"),  # Placeholder for the title
        DT::dataTableOutput("variables_table"),
        shiny::uiOutput("model_summ_header"),
        DT::DTOutput("model_summ"),
        DT::DTOutput("glance_model"),
        shiny::uiOutput("CATs_results_header"),
        shiny::verbatimTextOutput("print_CATs_output"),
        shiny::uiOutput("citation_header"),
        shiny::verbatimTextOutput("citations_output")
      )
    )
  )

  # Server
  server <- function(input, output, session) {

    # Reactive: Read the uploaded CSV file
    uploaded_data <- shiny::reactiveVal()

    shiny::observe({
      inFile <- input$datafile
      if (!is.null(inFile)) {
        data <- utils::read.csv(inFile$datapath, stringsAsFactors = TRUE)
        uploaded_data(data)
      }
    })

    output$variables_title <- shiny::renderUI({
      if (!is.null(uploaded_data()) && nrow(uploaded_data()) > 0) {
        shiny::tags$h2("Available Variables")
      }
    })


    output$variables_table <- DT::renderDataTable({
      shiny::req(uploaded_data())
      data <- uploaded_data()
      df <- data.frame(Variable = names(data), Type = sapply(data, class))
      DT::datatable(df, editable = 'cell', options = list(pageLength = 5),
                    rownames = FALSE)
    })

    shiny::observeEvent(input$variables_table_cell_edit, {
      info <- input$variables_table_cell_edit
      shiny::req(uploaded_data())
      data <- uploaded_data()

      row_number <- info$row
      new_value <- info$value

      if (info$col == 0){
        tryCatch({
          names(data)[row_number] <- new_value
          # Update the reactive data frame
          uploaded_data(data)
        }, error = function(e) {
          shiny::showNotification(
            paste("Error in changing variable name:", e$message),
            type = "error",
            duration = NULL
          )
        })
      }

      if (info$col == 1) {  # Assuming the 'Type' column is the second column
        variable_name <- names(data)[row_number]  # Fetch the variable name using row_number
        tryCatch({
          if (new_value == "factor") {
            data[[variable_name]] <- as.factor(data[[variable_name]])
          } else if (new_value == "numeric") {
            data[[variable_name]] <- as.numeric(data[[variable_name]])
          } else if (new_value == "integer") {
            data[[variable_name]] <- as.integer(data[[variable_name]])
          } else if (new_value == "double") {
            data[[variable_name]] <- as.double(data[[variable_name]])
          } else if (new_value == "character") {
            data[[variable_name]] <- as.character(data[[variable_name]])
          } else {
            stop("New data type must be one of the following: factor, numeric, integer, double, character")
          }
          # Update the reactive data frame
          uploaded_data(data)
        }, error = function(e) {
          shiny::showNotification(
            paste("Error in changing data type:", e$message),
            type = "error",
            duration = NULL
          )
        })
      }
    })

    # Select Engine and enter glm model information
    shiny::observe({
      if (!is.null(uploaded_data())) {
        output$model_input_ui <- shiny::renderUI({
          shiny::tagList(
            shiny::tags$div(
              title = "Enter the formula for your model using R's formula syntax (e.g., response ~ predictor1 + predictor2).",
              shiny::textInput("formula_or_model", "Formula", value = "")
            ),
            shiny::tags$div(
              title = "Optional: Pass additional arguments to the glm function in key=value format (e.g., family=binomial).",
              shiny::textInput("additional_args", "Additional Arguments")
            ),
            shiny::tags$div(
              title = "Set the confidence level for interval estimates. Default is 0.95 (95%).",
              shiny::numericInput("ci_value", "Confidence Interval", value = 0.95, min = 0, max = 1)
            ),
            shiny::tags$div(
              title = "Specify the number of decimal places to round numeric results. Default is 2.",
              shiny::numericInput("round", "Round", value = 2, min = 0, max = 10)
            ),
            shiny::tags$div(
              title = "Click to fit the GLM model using the specified formula and arguments.",
              shiny::actionButton("fit", "Fit Model")
            )
          )
        })
      }
    })

    # Logic to fit the model
    model <- NULL
    model_fitted <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$fit, {
      shiny::req(uploaded_data(), input$formula_or_model)
      args_list <- list(formula = stats::as.formula(input$formula_or_model), data = uploaded_data())


      # Add extra arguments if provided
      if (nzchar(input$additional_args)) {
        extra_args <- tryCatch({
          str2list(input$additional_args)
        }, error = function(e) {
          shiny::showNotification(
            paste("Error in additional arguments:", e$message),
            type = "error",
            duration = NULL
          )
          return(NULL)
        })

        if (!is.null(extra_args)) {
          args_list <- c(args_list, extra_args)
        }
      }

      tryCatch({
        model <<- do.call(stats::glm, args_list)
        model_fitted(TRUE)

        output$model_summ <- DT::renderDT({
          shiny::req(model_fitted())
          tidy_model <- broom::tidy(model, conf.int = TRUE, conf.level = input$ci_value)

          # Round numeric columns to 3 decimal places
          numeric_columns <- sapply(tidy_model, is.numeric)
          tidy_model[numeric_columns] <- lapply(tidy_model[numeric_columns], round, input$round)

          DT::datatable(tidy_model, options = list(pageLength = 5))
        })


        output$glance_model <- DT::renderDT({
          shiny::req(model_fitted())
          gmod <- broom::glance(model)

          # Round numeric columns to 3 decimal places
          numeric_columns <- sapply(gmod, is.numeric)
          gmod[numeric_columns] <- lapply(gmod[numeric_columns], round, input$round)

          DT::datatable(gmod, options = list(pageLength = 5))
        })


      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", e$message),
          type = "error",
          duration = NULL
        )
        model_fitted(FALSE)
      })
    })

    # Enter clustering information UI
    shiny::observe({
      if (model_fitted()) {
        output$cluster_input_ui <- shiny::renderUI({
          shiny::tagList(
            shiny::tags$div(
              title = "Specify the cluster variable for CATs analysis (e.g. ~ City).",
              shiny::textInput("cluster", "Cluster Variable")
            ),
            shiny::tags$div(
              title = "Check this box to drop clusters with only one observation from the analysis.",
              shiny::checkboxInput("drop", "Drop", value = FALSE)
            ),
            shiny::tags$div(
              title = "Check this box to truncate extreme values in the variance-covariance matrix.",
              shiny::checkboxInput("truncate", "Truncate", value = FALSE)
            ),
            shiny::tags$div(
              title = "Check this box to display the cluster-robust variance-covariance matrix.",
              shiny::checkboxInput("return.vcv", "Show Cluster Estimates", value = FALSE)
            ),
            shiny::tags$div(
              title = "Optional: Set a seed for reproducibility of the analysis.",
              shiny::numericInput("seed_value", "Set seed (optional)", value = NA, min = 1, max = .Machine$integer.max)
            ),
            shiny::tags$div(
              title = "Click to run the CATs analysis using the specified model and clustering options.",
              shiny::actionButton("run_analysis", "Run Analysis")
            )
          )
        })
      }
    })

    output$model_summ_header <- shiny::renderUI({
      if(model_fitted()) {
        shiny::tags$h2("GLM Summary")
      }
    })

    # Logic to run CATs analysis on the fitted model

    # Set reactive value to flag when analysis is done
   CATs_analysis_done <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$run_analysis, {
      shiny::req(model_fitted())

      # If seed value is provided, set it
      if (!is.na(input$seed_value)) {
        set.seed(input$seed_value)
      }

      tryCatch({
        # Run CATs analysis
       CATs_result <- do.call(clusterSEs::cluster.im.glm,
                              args = list(mod = model,
                                          dat = uploaded_data(),
                                          cluster = stats::as.formula(input$cluster),
                                          ci.level = input$ci_value,
                                          report = TRUE,
                                          drop = input$drop,
                                          truncate = input$truncate,
                                          return.vcv = input$return.vcv))

        # Set outputs for the CATs analysis
        output$print_CATs_output <- shiny::renderPrint({ print(CATs_result) })

        # Flag that CATs analysis is done
        CATs_analysis_done(TRUE)

      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", e$message),
          type = "error",
          duration = NULL
        )

        # Flag that CATs analysis is not done
        CATs_analysis_done(FALSE)
      })
    })

    output$CATs_results_header <- shiny::renderUI({
      if(CATs_analysis_done()) {
        shiny::tags$h2("Cluster-Adjusted P-values & Confidence Intervals")
      }
    })

    # Initialize citations_text as an empty string
    citations_text <- shiny::reactiveVal("")

    shiny::observeEvent(input$show_citations, {
      # Get the formatted citations
      mmiCATs_citation <- format_citation(utils::citation("mmiCATs"))
      cluster_ses_citation <- format_citation(utils::citation("clusterSEs"))

      citations <- paste(
        "Methodology:",
        "Esarey J, Menger A. Practical and Effective Approaches to Dealing With Clustered Data. Political Science Research and Methods. 2019;7(3):541-559. <doi:10.1017/psrm.2017.42>.",
        "",
        "Software Implementing Methodology",
        cluster_ses_citation,
        "",
        "Web Application:",
        mmiCATs_citation,
        sep = "\n"
      )
      citations_text(citations)
    })

    # Render the citations output
    output$citations_output <- shiny::renderText({
      citations_text()
    })

    output$citation_header <- shiny::renderUI({
      shiny::req(citations_text())
      shiny::tags$h2("Citations")
    })

  }
  shiny::shinyApp(ui = ui, server = server)
}
