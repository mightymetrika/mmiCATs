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
          shiny::actionButton("run", "Run Simulation"),
          shiny::actionButton("submit", "Submit")
        )
      ),
      shiny::mainPanel(
        shiny::uiOutput("sim_header"),
        DT::DTOutput("sim_output"),
        shiny::br(),  # Add a line break
        shiny::br(),  # Add a line break
        # Add a header for the responses table
        shiny::div(
          shiny::h4("All Responses"),
          DT::DTOutput("responses")
        )
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
                           n_time = input$n_time,
                           mean_i = input$mean_i,
                           var_i = input$var_i,
                           mean_s = input$mean_s,
                           var_s = input$var_s,
                           cov_is = input$cov_is,
                           mean_r = input$mean_r,
                           var_r = input$var_r,
                           cor_mat = sig_null(input$cor_mat),
                           corvars = list_null(input$corvars)
      )

      # Append inputs to the results
      res_app <- append_KenRCATs(res, input)

      # Set outputs for the CATs analysis
      sim_results(res_app$res) # display
      sim_results_exp(res_app$input_params) # for database

      #Output the results table
      output$sim_output <- DT::renderDT({
        sim_results()
      }, options = list(pageLength = 5))
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

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
