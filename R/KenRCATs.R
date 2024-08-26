#' Launch KenRCATs Shiny Application
#'
#' This function launches a 'shiny' application for conducting power analysis
#' simulations using CATs (Clustered Adjusted t-statistics) and Kenward-Roger
#' methods. The app allows users to input simulation parameters, run simulations,
#' view results, and manage data in a PostgreSQL database.
#'
#' @param dbname Character string specifying the name of the PostgreSQL database.
#' @param datatable Character string specifying the name of the table in the database.
#' @param host Character string specifying the host name or IP address of the database server.
#' @param port Integer specifying the port number on which the database is running.
#' @param user Character string specifying the username for database connection.
#' @param password Character string specifying the password for database connection.
#'
#' @return A 'shiny' app object.
#'
#' @details
#' The KenRCATs function sets up a Shiny application with the following features:
#' \itemize{
#'   \item Input fields for various simulation parameters
#'   \item Ability to run power analysis simulations
#'   \item Display of simulation results
#'   \item Option to submit results to a PostgreSQL database
#'   \item Functionality to download data from the database
#'   \item Display of relevant citations
#' }
#'
#'
#' @examples
#' if(interactive()){
#'   KenRCATs(
#'     dbname = "your_database_name",
#'     datatable = "your_table_name",
#'     host = "localhost",
#'     port = 5432,
#'     user = "your_username",
#'     password = "your_password"
#'   )
#' }
#'
#' @export
KenRCATs <- function(dbname, datatable, host, port, user, password){

  # Helper function to save data to the database
  saveData <- function(data) {
    # Connect to the database
    pool <- pool::dbPool(
      drv = RPostgres::Postgres(),
      dbname = dbname,
      host = host,
      user = user,
      password = password,
      port = port
    )

    # Close pool on stop
    shiny::onStop(function() {
      pool::poolClose(pool)
    })

    # Convert NA to NaN in the data frame
    data[is.na(data)] <- NaN

    # Loop through rows of data and save to databse
    lapply(1:nrow(data), function(i){

      # get row i of the data
      row_data <- data[i, ]

      # Construct the update query by looping over the data fields
      query <- sprintf(
        "INSERT INTO %s (%s) VALUES ('%s')",
        datatable,
        paste(names(row_data), collapse = ", "),
        paste(row_data, collapse = "', '")
      )

      # Execute the query
      tryCatch({
        pool::dbExecute(pool, query)
      }, error = function(e) {
        print(paste("Error inserting row", i, ":", e))
      })
    })


  }

  # Helper function to load data from database
  loadData <- function() {

    # Connect to the database
    pool <- pool::dbPool(
      drv = RPostgres::Postgres(),
      dbname = dbname,
      host = host,
      user = user,
      password = password,
      port = port
    )

    # Close pool on stop
    shiny::onStop(function() {
      pool::poolClose(pool)
    })

    # Construct the fetching query
    sql <- sprintf("SELECT * FROM %s", datatable)
    query <- pool::sqlInterpolate(pool, sql)

    # Submit the fetch query and disconnect
    pool::dbGetQuery(pool, query)

  }

  ui <- shiny::fluidPage(
    shiny::titlePanel("CATs & Kenward-Roger Simulation"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("paramsUI"),
        shiny::tags$div(
          title = "Click to run simulation with specified arguments.",
          shiny::actionButton("run", "Run Simulation")
        ),
        shiny::br(),  # Add a line break
        shiny::tags$div(
          title = "Submit simulation results to database.",
          shiny::actionButton("submit", "Submit")
        ),
        shiny::br(),  # Add a line break
        shiny::tags$div(
          title = "Download database to csv file.",
          shiny::downloadButton("downloadBtn", "Download Data")
        ),
        shiny::br(),  # Add a line break
        shiny::actionButton("show_citations", "Citations")
      ),
      shiny::mainPanel(
        shiny::uiOutput("sim_header"),
        DT::DTOutput("sim_output"),
        shiny::br(),  # Add a line break
        shiny::br(),  # Add a line break
        # Add a header for the responses table
        shiny::div(
          shiny::h2("Database"),
          DT::DTOutput("responses")
        ),
        shiny::uiOutput("citation_header"),
        shiny::verbatimTextOutput("citations_output")
      )
    )
  )

  server <- function(input, output, session) {

    # Get parameters for simulation
    output$paramsUI <- shiny::renderUI({
      getUIParams()
    })

    # Load data from the database on app start
    output$responses <- DT::renderDT({
      loadData()
    }, options = list(pageLength = 5))

    sim_complete <- shiny::reactiveVal(FALSE)
    sim_results <- shiny::reactiveVal()
    sim_results_exp <- shiny::reactiveVal()
    shiny::observeEvent(input$run, {

      # Parse inputs
      betas <- parse_list_input(input$betas)
      dists <- parse_list_input(input$dists)
      distpar <- parse_list_input(input$distpar)

      res <- pwr_func_lmer(betas = betas,
                           dists = dists,
                           distpar = distpar,
                           N = input$N,
                           reps = input$reps,
                           alpha = input$alpha,
                           var_intr = input$var_intr,
                           grp = input$grp,
                           mod = input$mod,
                           catsmod = input$catsmod,
                           r_slope = input$r_slope,
                           r_int = input$r_int,
                           n_time = text_to_vector(input$n_time),
                           mean_i = input$mean_i,
                           var_i = input$var_i,
                           mean_s = input$mean_s,
                           var_s = input$var_s,
                           cov_is = input$cov_is,
                           mean_r = input$mean_r,
                           var_r = input$var_r,
                           cor_mat = sig_null(input$cor_mat),
                           corvars = list_null(input$corvars),
                           time_index = if(input$time_index == "" |
                                           input$time_index == "NA" |
                                           is.na(input$time_index)) NULL else input$time_index
      )

      # Append inputs to the results
      res_app <- append_KenRCATs(res, input)

      # Set outputs for the CATs analysis
      sim_results(res_app$res) # display
      sim_results_exp(res_app$input_params) # for database

      #Output the results table
      output$sim_output <- DT::renderDT({
        sim_results()
      }, options = list(pageLength = 8))
      sim_complete(TRUE)

      })

    output$sim_header <- shiny::renderUI({
      shiny::req(sim_complete())
      shiny::tags$h2("Simulation Results")
    })

    # When the Submit button is clicked, save the form data
    shiny::observeEvent(input$submit, {
      # Prevent submitting if results are empty
      if(nrow(sim_results_exp()) == 0) {
        shiny::showModal(shiny::modalDialog(
          title = "Error",
          "No results to submit. Please run the simulation first.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }

      # add additional simulation setting information to the results before
      # exporting to database
      saveData(sim_results_exp())

      # Clear the results after submission
      sim_results_exp(data.frame())

      # Update the responses table with new data
      output$responses <- DT::renderDT({
        loadData()
      }, options = list(pageLength = 5))
    })

    # Download handler for exporting data
    output$downloadBtn <- shiny::downloadHandler(
      filename = function() {
        paste0("Simulation_Results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        # Write the data to a CSV file
        utils::write.csv(loadData(), file, row.names = FALSE)
      }
    )

    # Initialize citations_text as an empty string
    citations_text <- shiny::reactiveVal("")

    shiny::observeEvent(input$show_citations, {
      # Get the formatted citations
      mmiCATs_citation <- format_citation(utils::citation("mmiCATs"))
      cluster_ses_citation <- format_citation(utils::citation("clusterSEs"))

      citations <- paste(
        "CATs Methodology:",
        "Esarey J, Menger A. Practical and Effective Approaches to Dealing With Clustered Data. Political Science Research and Methods. 2019;7(3):541-559. <doi:10.1017/psrm.2017.42>.",
        "",
        "Software Implementing CATs Methodology",
        cluster_ses_citation,
        "",
        "Kenward-Roger Methodology:",
        "Kenward, M. G. and Roger, J. H. (1997), Small Sample Inference for Fixed Effects from Restricted Maximum Likelihood, Biometrics 53: 983-997.",
        "",
        "Software Implementing Kenward-Roger",
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

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
