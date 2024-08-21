KenRCATs <- function(){

  ui <- shiny::fluidPage(
    shiny::titlePanel("CATs & Kenward-Roger Simulation"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("paramsUI"),
        shiny::tags$div(
          title = "Click to run simulation with specified arguments.",
          shiny::actionButton("run", "Run Simulation")
        )
      ),
      shiny::mainPanel(
        shiny::verbatimTextOutput("sim_output")
      )
    )
  )

  server <- function(input, output, session) {

    # Get parameters for simulation
    output$paramsUI <- shiny::renderUI({
      getUIParams()
    })

    # sim_complete <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$run, {

      # Parse inputs
      betas <- parse_input(input$betas)
      dists <- parse_input(input$dists)
      distpar <- parse_input(input$distpar)

      res <- pwr_func_lmer(betas = betas,
                           dists = dists,
                           distpar = distpar,
                           N = input$N)

      # Set outputs for the CATs analysis
      output$sim_output <- shiny::renderPrint({ print(res) })
      # sim_complete(TRUE)

      })

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
