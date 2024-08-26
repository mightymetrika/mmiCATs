#' Kenward-Roger Analysis Shiny Application
#'
#' A Shiny application that allows users to upload a dataset, modify variable types,
#' and fit a mixed-effects model using the Kenward-Roger approximation for small
#' sample inference.
#'
#' The application provides an interactive interface for setting up and running
#' a mixed-effects model analysis with the Kenward-Roger method for estimating
#' the degrees of freedom in linear mixed-effects models.
#'
#' @references
#' Kenward, M. G., & Roger, J. H. (1997). Small Sample Inference for Fixed Effects
#' from Restricted Maximum Likelihood. Biometrics, 53, 983-997.
#'
#' Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. B. (2017). lmerTest
#' Package: Tests in Linear Mixed Effects Models. Journal of Statistical Software,
#' 82(13), 1-26. <doi: 10.18637/jss.v082.i13>.
#'
#' @return A Shiny application object.
#'
#' @examples
#' if (interactive()) {
#'   kenward_roger()
#' }
#'
#' @importFrom lmerTest lmer
#' @export
kenward_roger <- function(){
  # UI
  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("lumen"),
    shiny::titlePanel("Set Up REML with Kenward-Roger Correction Analysis"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("datafile", "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
        shiny::uiOutput("model_input_ui"),
        shiny::actionButton("show_citations", "Citations")
      ),
      shiny::mainPanel(
        shiny::uiOutput("variables_title"),
        DT::dataTableOutput("variables_table"),
        shiny::uiOutput("model_summ_header"),
        shiny::verbatimTextOutput("model_summ"),
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

    # Select Engine
    shiny::observe({
      if (!is.null(uploaded_data())) {
        output$model_input_ui <- shiny::renderUI({
          shiny::tagList(
            shiny::tags$div(title = "Enter formula using lme4 style for mixed-effects models",
                            shiny::textInput("formula_or_model", "Formula", value = "")
                            ),
            shiny::tags$div(title = "Pass additional arguments to lmerTest::lmer (key=value format)",
                          shiny::textInput("additional_args", "Additional Arguments")
                          ),
            shiny::tags$div(title = "Fit model and get summary",
                            shiny::actionButton("fit", "Fit Model")
                            ),
            )
        })
      }
    })

    # Logic to fit the model
    model <- NULL
    model_fitted <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$fit, {
      shiny::req(uploaded_data(), input$formula_or_model)
      args_list <- list(formula = stats::as.formula(input$formula_or_model),
                        data = quote(uploaded_data()))


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
        model <<- do.call("lmer", args_list)
        model_fitted(TRUE)

        output$model_summ <- shiny::renderPrint({
          summary(model, ddf = "Kenward-Roger")
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

    output$model_summ_header <- shiny::renderUI({
      if(model_fitted()) {
        shiny::tags$h2("Summary")
      }
    })

    # Initialize citations_text as an empty string
    citations_text <- shiny::reactiveVal("")

    shiny::observeEvent(input$show_citations, {
      # Get the formatted citations
      mmiCATs_citation <- format_citation(utils::citation("mmiCATs"))

      citations <- paste(
        "Original Kenward_Roger Paper:",
        "Kenward, M. G. and Roger, J. H. (1997), Small Sample Inference for Fixed Effects from Restricted Maximum Likelihood, Biometrics 53: 983-997.",
        "",
        "Software Implementing the Primary Statistical Methods",
        "Kuznetsova A, Brockhoff PB, Christensen RHB (2017). 'lmerTest Package: Tests in Linear Mixed Effects Models.' Journal of Statistical Software, 82(13), 1-26. <doi:10.18637/jss.v082.i13>.",
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
