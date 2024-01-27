CloseCATs <- function(){
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
        # Action Buttons
        shiny::fluidRow(
          shiny::column(12, shiny::actionButton("deal", "Deal Cards")),
          shiny::column(12, shiny::actionButton("score", "Score Game"))
        )
      ),

      shiny::mainPanel(
        shiny::uiOutput("game_title"),
        shiny::uiOutput("swap_cards_title"),
        shiny::uiOutput("results_title")
      )
    )
  )


  server <- function(input, output, session) {

    # Set up reactive values
    cards_dealt <- shiny::reactiveVal(FALSE)
    game_cards <- shiny::reactiveVal(NULL)
    comp_results <- shiny::reactiveVal(NULL)
    ply_results <- shiny::reactiveVal(NULL)
    game_scored <- shiny::reactiveVal(FALSE)

    # Deal Cards
    shiny::observeEvent(input$deal, {

      # Reset the game state
      cards_dealt(FALSE)
      game_cards(NULL)
      comp_results(NULL)
      ply_results(NULL)
      game_scored(FALSE)

      # Deal cards
      card_grid <- deal_cards_to_cc_grid(n = 2)

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

    # Score Game
    shiny::observeEvent(input$score, {

      # Extract the current card deck
      cards <- game_cards()

      # If there's no deck, exit early
      if (is.null(cards)) return(NULL)

      # Convert cor_pred from NA to NULL
      if (is.na(input$cor_pred)){
        cor_pred <- NULL
      } else {
        cor_pred <- input$cor_pred
      }

      # Process computer results
      computer_results <- process_hand(cards,
                                       process_col = 1,
                                       beta_int = input$beta_int,
                                       beta_x1 = input$beta_x1,
                                       beta_x2 = input$beta_x2,
                                       mean_x1 = input$mean_x1,
                                       sd_x1 = input$sd_x1,
                                       mean_x2 = input$mean_x2,
                                       sd_x2 = input$sd_x2,
                                       N = input$N,
                                       alpha = input$alpha_level,
                                       n_time = input$n_time,
                                       mean_i = input$mean_i,
                                       var_i = input$var_i,
                                       mean_s = input$mean_s,
                                       mean_r = input$mean_r,
                                       var_r = input$var_r,
                                       cor_pred = cor_pred,
                                       truncate = input$truncate,
                                       var_s_factor = input$var_s_factor,
                                       cov_is_factor = input$cov_is_factor)

      comp_results(computer_results)

      output$comp_results_summary <- shiny::renderPrint({
        print(comp_results()$results)
      })

      # Process player results
      player_results <-   process_hand(cards,
                                       process_col = 2,
                                       beta_int = input$beta_int,
                                       beta_x1 = input$beta_x1,
                                       beta_x2 = input$beta_x2,
                                       mean_x1 = input$mean_x1,
                                       sd_x1 = input$sd_x1,
                                       mean_x2 = input$mean_x2,
                                       sd_x2 = input$sd_x2,
                                       N = input$N,
                                       alpha = input$alpha_level,
                                       n_time = input$n_time,
                                       mean_i = input$mean_i,
                                       var_i = input$var_i,
                                       mean_s = input$mean_s,
                                       mean_r = input$mean_r,
                                       var_r = input$var_r,
                                       cor_pred = cor_pred,
                                       truncate = input$truncate,
                                       var_s_factor = input$var_s_factor,
                                       cov_is_factor = input$cov_is_factor)

      ply_results(player_results)

      output$ply_results_summary <- shiny::renderPrint({
        print(ply_results()$results)
      })

      # Interpret results
      output$interpretation <- shiny::renderUI({
        if (ply_results()$mispec_dist <  comp_results()$mispec_dist) {
          shiny::tags$div(
            shiny::tags$h1("You Win!", style = "color: green; font-size: 48px;")
          )
        } else {
          shiny::tags$div(
            shiny::tags$h1("You Lose!", style = "color: red; font-size: 48px;")
          )
        }
      })

      # Change game_scored to TRUE
      game_scored(TRUE)

    })

    # Results Title
    output$results_title <- shiny::renderUI({
      if(game_scored()) {
        shiny::tagList(
          shiny::fluidRow(
            # Column for Computer Results
            shiny::column(6,
                          shiny::tags$h4("Computer Results"),
                          shiny::verbatimTextOutput("comp_results_summary")
            ),
            # Column for Player Results
            shiny::column(6,
                          shiny::tags$h4("Player Results"),
                          shiny::verbatimTextOutput("ply_results_summary")
            ),
            shiny::uiOutput("interpretation")
          )
        )
      }
    })
  }

  shiny::shinyApp(ui, server)
}
