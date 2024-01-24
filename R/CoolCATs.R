CoolCATs <- function(){
  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("paper"),
    shiny::titlePanel("CoolCATs"),

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
        shiny::numericInput("cov_is_factor", "Reduction Factor (Random Effect Covariance)", value = 14.75, min = 1, max = 14.75)),
        # Original study
        shiny::fluidRow(
          shiny::column(12, shiny::actionButton("start", "Start Game")),
          shiny::column(12, shiny::actionButton("swap", "Swap Roles")),
          shiny::column(12, shiny::actionButton("score", "Score Game"))
        )
      ),

      shiny::mainPanel(
        shiny::uiOutput("original_study_title"),
        shiny::uiOutput("model_summary_title"),
        shiny::uiOutput("original_hypothesis_title"),
        shiny::uiOutput("diagnostics_title"),
        shiny::uiOutput("descriptive_stats_title"),
        shiny::uiOutput("replication_study_title"),
        shiny::uiOutput("swap_cards_title"),
        shiny::uiOutput("results_title"),
      )
    )

  server <- function(input, output, session) {

    # Set up reactive values
    game_started <- shiny::reactiveVal(FALSE)
    study_results <- shiny::reactiveVal(NULL)
    diagnostics_requested <- shiny::reactiveVal(FALSE)
    descriptives_requested <- shiny::reactiveVal(FALSE)
    replication_cards <- shiny::reactiveVal(NULL)
    replication_conducted <- shiny::reactiveVal(FALSE)
    replication_results <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$original_study, {

      # Reset reactive values
      original_conducted(FALSE)
      study_results(NULL)
      diagnostics_requested(FALSE)
      descriptives_requested(FALSE)
      replication_cards(NULL)
      replication_conducted(FALSE)
      replication_results(NULL)

      # If seed value is provided, set it
      if (!is.na(input$seed_value)) {
        set.seed(input$seed_value)
      }

      # Deal cards
      card_grid <- deal_cards_to_rc_grid(n = input$difficulty)

      # Rendering the UI for the card grid
      output$card_display <- shiny::renderUI({
        render_card_grid(card_grid)
      })

      # Generate study data
      study_data <- generate_study_data(card_grid, sample_size = input$sample_size)

      # Process original study
      study_results <- process_original_study(study_data, alpha = input$alpha_level)

      # Update study_results to hold the generated data
      study_results(study_results)

      # Display fit summary and hypothesis
      output$fit_summary <- shiny::renderTable(broom::tidy(study_results$fit))
      output$pair_t <- shiny::renderTable(broom::tidy(study_results$pairwise_t))
      output$hypothesis <- shiny::renderText(study_results$hypothesis)

      # Update study conducted reactiveVal
      original_conducted(TRUE)
    })

    # Conditional UI for fit summary
    output$fit_summary_ui <- shiny::renderUI({
      if(input$original_study) {
        shiny::tableOutput("fit_summary")
      }
    })

    # Conditional UI for pairwise t-test results
    output$pair_t_ui <- shiny::renderUI({
      if(input$original_study) {
        shiny::tableOutput("pair_t")
      }
    })

    # Conditional UI for hypothesis
    output$hypothesis_ui <- shiny::renderUI({
      if(input$original_study) {
        shiny::textOutput("hypothesis")
      }
    })

    shiny::observeEvent(input$show_diagnostics, {
      # Update reactiveVal
      diagnostics_requested(TRUE)

      # Display diagnostics for the original study: fit plot, Shapiro test, and Levene test

      # Extract study_results
      results <- study_results()

      output$fit_plot <- shiny::renderPlot({

        # Setup on.exit to reset graphical parameters
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar))

        # Set up a 2x2 grid for 4 plots
        graphics::par(mfrow = c(2, 2))

        # Plot 1: Residuals vs Fitted
        plot(results$fit, which = 1)

        # Plot 2: Normal Q-Q
        plot(results$fit, which = 2)

        # Plot 3: Scale-Location (Standardized residuals vs Fitted values)
        plot(results$fit, which = 3)

        # Plot 4: Cook's distance
        plot(results$fit, which = 4)
      })

      # Get diagnostic statistical tests
      output$shapiro_test <- shiny::renderTable(broom::tidy(results$shapiro_test))
      lev_tst <- broom::tidy(results$levene_test)
      lev_tst$method <- "Levene Test"
      output$levene_test <- shiny::renderTable(lev_tst)

    })

    # Conditional UI for diagnostics
    output$fit_plot_ui <- shiny::renderUI({
      if(input$show_diagnostics) {
        shiny::plotOutput("fit_plot")
      }
    })

    output$shapiro_test_ui <- shiny::renderUI({
      if(input$show_diagnostics) {
        shiny::tableOutput("shapiro_test")
      }
    })

    output$levene_test_ui <- shiny::renderUI({
      if(input$show_diagnostics) {
        shiny::tableOutput("levene_test")
      }
    })

    shiny::observeEvent(input$show_descriptives, {

      #Update reactiveVal
      descriptives_requested(TRUE)

      # Display descriptive statistics for the original study

      # Extract study_results
      results <- study_results()

      output$descriptives <- shiny::renderTable(results$descriptives)
    })

    output$descriptives_ui <- shiny::renderUI({
      if(input$show_descriptives) {
        shiny::tableOutput("descriptives")
      }
    })

    shiny::observeEvent(input$replication_study, {
      # Deal cards for replication study
      card_grid_replication <- deal_cards_to_rc_grid(n = input$difficulty)

      # Update the reactive value
      if (!replication_conducted()){
        replication_cards(card_grid_replication)
      }


      # Rendering the UI for the card grid for replication study
      output$rep_card_display <- shiny::renderUI({
        render_card_grid(replication_cards())
      })

      # Set the reactive value to TRUE since the replication study is now conducted
      replication_conducted(TRUE)
    })

    shiny::observeEvent(input$swap_cols, {
      # Extract the replication card grid from the reactive value
      rep_cards <- replication_cards()

      # Check for NULL values in user input and exit early if found
      if (is.null(input$swap_col1) || is.null(input$swap_col2)) return(NULL)

      # Swap the columns using the swapper function
      tryCatch({
        new_card_grid <- swapper(rep_cards, swap_cols = c(input$swap_col1, input$swap_col2))

        # Update the reactive value to hold the new card grid
        replication_cards(new_card_grid)

        # Rerender the UI for the card grid
        output$rep_card_display <- shiny::renderUI({
          render_card_grid(new_card_grid)
        })
      }, error = function(e) {
        # Handle the error by displaying a message
        shiny::showNotification(paste("An error occurred:", e$message), type = "error")
      })
    })

    shiny::observeEvent(input$swap_inside_col, {
      # Extract the replication card grid from the reactive value
      rep_cards <- replication_cards()

      # Check for NULL values in user input and exit early if found
      if (is.null(input$swap_in_col)) return(NULL)

      # Swap within the column using the swapper function
      tryCatch({
        new_card_grid <- swapper(rep_cards, swap_in_col = input$swap_in_col)

        # Update the reactive value to hold the new card grid
        replication_cards(new_card_grid)

        # Rerender the UI for the card grid
        output$rep_card_display <- shiny::renderUI({
          render_card_grid(new_card_grid)
        })
      }, error = function(e) {
        # Handle the error by displaying a message
        shiny::showNotification(paste("An error occurred:", e$message), type = "error")
      })
    })

    shiny::observeEvent(input$swap_inside_row, {
      # Extract the replication card grid from the reactive value
      rep_cards <- replication_cards()

      # Check for NULL values in user input and exit early if found
      if (is.null(input$swap_in_row_num) || is.null(input$swap_in_row_col1) || is.null(input$swap_in_row_col2)) return(NULL)

      # Swap within the row using the swapper function
      tryCatch({
        new_card_grid <- swapper(rep_cards, swap_in_row = c(input$swap_in_row_num, input$swap_in_row_col1, input$swap_in_row_col2))

        # Update the reactive value to hold the new card grid
        replication_cards(new_card_grid)

        # Rerender the UI for the card grid
        output$rep_card_display <- shiny::renderUI({
          render_card_grid(new_card_grid)
        })
      }, error = function(e) {
        # Handle the error by displaying a message
        shiny::showNotification(paste("An error occurred:", e$message), type = "error")
      })
    })
    # Render the swap controls only when the replication study has been conducted
    output$swap_controls_ui <- shiny::renderUI({
      if (replication_conducted()) {
        shiny::fluidRow(
          # First column: Swap Columns
          shiny::column(4,
                        shiny::tagList(
                          shiny::numericInput("swap_col1", "Swap Column 1", value = NULL),
                          shiny::numericInput("swap_col2", "Swap Column 2", value = NULL),
                          shiny::actionButton("swap_cols", "Swap Columns")
                        )
          ),
          # Second column: Swap Inside Column
          shiny::column(4,
                        shiny::tagList(
                          shiny::numericInput("swap_in_col", "Swap Inside Column", value = NULL),
                          shiny::actionButton("swap_inside_col", "Execute Inside Column Swap")
                        )
          ),
          # Third column: Swap Inside Row
          shiny::column(4,
                        shiny::tagList(
                          shiny::numericInput("swap_in_row_num", "Row Number", value = NULL),
                          shiny::numericInput("swap_in_row_col1", "Swap Row Column 1", value = NULL),
                          shiny::numericInput("swap_in_row_col2", "Swap Row Column 2", value = NULL),
                          shiny::actionButton("swap_inside_row", "Execute Inside Row Swap")
                        )
          )
        )
      }
    })



    shiny::observeEvent(input$run_replication, {
      # Extract the current card deck for replication study from the reactive value
      rep_cards <- replication_cards()

      # If there's no replication card deck, exit early
      if (is.null(rep_cards)) return(NULL)

      # Generate replication study data
      replication_data <- generate_study_data(rep_cards, sample_size = input$sample_size)

      # Process replication study
      replication_results <- process_replication_study(replication_data, study_results())

      # Update reactive value
      replication_results(replication_results)

      # Display the replication study results in the UI
      output$bain_results_summary <- shiny::renderPrint({
        summary(replication_results()$bain_results)
      })

      output$bain_results <- shiny::renderPrint({
        print(replication_results()$bain_results)
      })

      # Interpret the replication results using the thresholds provided by the user
      interpretation <- interpret_replication_results(
        replication_results(),
        bf_threshold = input$bf_threshold,
        pmpb_threshold = input$pmpb_threshold
      )

      # Display the interpretation and result message
      output$interpretation <- shiny::renderUI({
        if (interpretation$result == "win") {
          shiny::tags$div(
            shiny::tags$h1("You Win!", style = "color: green; font-size: 48px;"),
            shiny::tags$p(interpretation$interpretation)
          )
        } else {
          shiny::tags$div(
            shiny::tags$h1("You Lose!", style = "color: red; font-size: 48px;"),
            shiny::tags$p(interpretation$interpretation)
          )
        }
      })

      # Display the disclaimer
      output$disclaimer <- shiny::renderText({
        interpretation$disclaimer
      })
    })

    # Original Study Title
    output$original_study_title <- shiny::renderUI({
      if(input$original_study) {
        shiny::tagList(
          shiny::tags$h3("Original Study"),
          shiny::uiOutput("card_display")
        )
      }
    })

    # Model Summary Title
    output$model_summary_title <- shiny::renderUI({
      if(input$original_study & original_conducted()) {
        shiny::tagList(
          shiny::tags$h3("Model Summary"),
          shiny::uiOutput("fit_summary_ui"),
          shiny::uiOutput("pair_t_ui")
        )
      }
    })

    # Original Hypothesis Title
    output$original_hypothesis_title <- shiny::renderUI({
      if(input$original_study & original_conducted()) {
        shiny::tagList(
          shiny::tags$h3("Original Hypothesis"),
          shiny::uiOutput("hypothesis_ui")
        )
      }
    })

    # Diagnostics Title
    output$diagnostics_title <- shiny::renderUI({
      if(diagnostics_requested() & original_conducted()) {
        shiny::tagList(
          shiny::tags$h3("Diagnostics"),
          shiny::uiOutput("fit_plot_ui"),
          shiny::uiOutput("shapiro_test_ui"),
          shiny::uiOutput("levene_test_ui")
        )
      }
    })

    # Descriptive Statistics Title
    output$descriptive_stats_title <- shiny::renderUI({
      if(descriptives_requested() & original_conducted()) {
        shiny::tagList(
          shiny::tags$h3("Descriptive Statistics"),
          shiny::uiOutput("descriptives_ui")
        )
      }
    })

    # Replication Study Title
    output$replication_study_title <- shiny::renderUI({
      if(replication_conducted() & original_conducted()) {
        shiny::tagList(
          shiny::tags$h3("Replication Study"),
          shiny::uiOutput("rep_card_display")
        )
      }
    })

    # Swap Cards Title
    output$swap_cards_title <- shiny::renderUI({
      if(replication_conducted() & original_conducted()) {
        shiny::tagList(
          shiny::tags$h3("Swap Cards"),
          shiny::uiOutput("swap_controls_ui")
        )
      }
    })

    # Results Title
    output$results_title <- shiny::renderUI({
      if(replication_conducted() & original_conducted() & !is.null(replication_results())) {
        shiny::tagList(
          shiny::tags$h3("Results"),
          shiny::verbatimTextOutput("bain_results_summary"),
          shiny::verbatimTextOutput("bain_results"),
          shiny::uiOutput("interpretation"),
          shiny::textOutput("disclaimer")
        )
      }
    })

  }

  shiny::shinyApp(ui, server)
}
