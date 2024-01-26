CloseCATd <- function(){
  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("paper"),
    shiny::titlePanel("CloseCATs"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # Game setup
        shiny::numericInput("beta_int", "Beta Intercept", value = 0, min = -500, max = 500),
        shiny::numericInput("beta_x1", "Beta X1", value = 0.25, min = -500, max = 500),
        shiny::numericInput("mean_x1", "Mean X1", value = 0, min = -500, max = 500),
        shiny::numericInput("sd_x1", "SD X1", value = 1, min = 0, max = 500),
        shiny::numericInput("beta_x2", "Beta X2", value = 1.5, min = -500, max = 500),
        shiny::numericInput("mean_x2", "Mean X2", value = 0, min = -500, max = 500),
        shiny::numericInput("sd_x2", "SD X2", value = 4, min = 0, max = 500),
        shiny::numericInput("N", "Number of Clusters", value = 20, min = 1, max = 500),
        shiny::numericInput("n_time", "Observations Within Cluster", value = 20, min = 1, max = 500),
        shiny::numericInput("alpha_level", "Select Alpha Level", value = 0.05, min = 0.01, max = 0.10, step = 0.01),
        shiny::numericInput("mean_i", "Mean for Random Intercept", value = 0, min = -500, max = 500),
        shiny::numericInput("var_i", "Variance for Random Intercept", value = 1, min = 0, max = 500),
        shiny::numericInput("mean_s", "Mean for Random Slope", value = 0, min = -500, max = 500),
        shiny::numericInput("mean_r", "Mean for Residual Error", value = 0, min = -500, max = 500),
        shiny::numericInput("var_r", "Variance for Residual Error", value = 1, min = 0, max = 500),
        shiny::numericInput("cor_pred", "Set correlation between predictors", value = NA, min = 0, max = 500),
        shiny::checkboxInput("truncate", "Exclude Outlying Cluster-Specific Beta Estimates", value = FALSE),
        shiny::numericInput("var_s_factor", "Reduction Factor (Random Slope Variance)", value = 1, min = 1, max = 14.75),
        shiny::numericInput("cov_is_factor", "Reduction Factor (Random Effect Covariance)", value = 14.75, min = 1, max = 14.75),
      # Original study
      shiny::fluidRow(
        shiny::column(12, shiny::actionButton("deal", "Deal Cards"))
      )
    ),

    shiny::mainPanel(
      shiny::uiOutput("game_title"),
      shiny::uiOutput("swap_cards_title")
    )
  )
  )


  server <- function(input, output, session) {

    # Set up reactive values
    cards_dealt <- shiny::reactiveVal(FALSE)
    game_cards <- shiny::reactiveVal(NULL)

    # Deal Cards
    shiny::observeEvent(input$deal, {

      # Deal cards
      card_grid <- deal_cards_to_cc_grid(n = 2)

      print(paste0("Print comp row 1: ", "/n", card_grid[1,1])) ### DEBUG
      print(paste0("Print comp row 2: ", "/n", card_grid[2,1])) ### DEBUG
      print(paste0("Print plyr row 1: ", "/n", card_grid[1,2])) ### DEBUG
      print(paste0("Print plyr row 2: ", "/n", card_grid[2,2])) ### DEBUG

      # Update the reactive value
      if (!cards_dealt()){
        game_cards(card_grid)
        cards_dealt(TRUE)
      }

      # # Rendering the UI for the card grid
      output$card_display <- shiny::renderUI({
        render_card_grid(card_grid)
      })

    })

    # Display Card UI
    output$game_title <- shiny::renderUI({
      if(cards_dealt()) {
        shiny::tagList(
          shiny::tags$h3("Game Cards"),
          shiny::uiOutput("card_display")
        )
      }
    })

    # Swap Cards
    shiny::observeEvent(input$swap_inside_col, {
      # Extract the game cards grid from the reactive value
      cards <- game_cards()

      # Swap within the column using the swapper function
      tryCatch({
        new_card_grid <- cc_swapper(cards, swap_in_col = 2)

        # Update the reactive value to hold the new card grid
        game_cards(new_card_grid)

        # Rerender the UI for the card grid
        output$card_display <- shiny::renderUI({
          render_card_grid(new_card_grid)
        })
      }, error = function(e) {
        # Handle the error by displaying a message
        shiny::showNotification(paste("An error occurred:", e$message), type = "error")
      })
    })

    # Setup Swap UI
    output$swap_controls_ui <- shiny::renderUI({
      if (cards_dealt()) {
        shiny::fluidRow(
          shiny::column(4,
                        shiny::actionButton("swap_inside_col", "Execute Inside Column Swap")
          )
        )
      }
    })

    # Display Swap UI
    output$swap_cards_title <- shiny::renderUI({
      if(cards_dealt()) {
        shiny::tagList(
          shiny::tags$h3("Swap Cards"),
          shiny::uiOutput("swap_controls_ui")
        )
      }
    })

  }

  shiny::shinyApp(ui, server)
}
