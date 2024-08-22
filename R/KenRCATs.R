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

      # Set outputs for the CATs analysis
      output$sim_output <- shiny::renderPrint({ print(res) })
      # sim_complete(TRUE)

      })

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
