#--- PREAMBLE (RUNS ONLY ONCE WHEN APP LOADS) --------------------------------#

# Load MASS package
require(MASS)

#--- SERVER SCRIPT -----------------------------------------------------------#

function(input, output, session) {
  
  # Execute model-specific power analysis code
  calc_power <- eventReactive(input$action, {
    out <- tryCatch(
      {source(paste0("./code/", input$model, ".R"), local = TRUE)$value},
      error = function(e) {return(e$message)}
    )
  })  

  # Display output or display error messages
  observeEvent(calc_power(),
    if (class(calc_power()) != "data.frame") {
      output$power <- renderText({
        paste0("<div class=\"alert alert-dismissible alert-danger\">",
               calc_power(), "</div>")})
    } else {
      output$power <- renderTable({ 
        calc_power()
      }, include.rownames=FALSE)
    }
  )
  
  # Add the download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("power_analysis_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- calc_power()
      if (class(data) == "data.frame") {
        
        # Add  parameters from the analysis
        data$Model <- input$model
        data$Objective <- input$obj
        data$Input_Method <- input$input_method
        data$Replications <- input$powReps
        data$MC_Draws_per_Rep <- input$mcmcReps
        data$Random_Seed <- input$seed
        data$Confidence_Level <- input$conf
        data$Analysis_Date <- Sys.Date()
        
        # get input names and parameters from list of all inputs using string detection
        # this was the easiest way to find them all? (hopefully) without having to create
        # crazy conditional logic
        all_inputs <- names(input)
        target_inputs <- all_inputs[grepl("^(cor|SD)", all_inputs)]
        
        # Add all matching inputs
        if (length(target_inputs) > 0) {
          for (input_name in target_inputs) {
            clean_name <- gsub("_", " ", input_name) 
            data[[clean_name]] <- input[[input_name]]
          }
        }
        
        write.csv(data, file, row.names = FALSE)
      }
    }
  )
  
  # Conditional button:
  output$showDownload <- reactive({
    class(calc_power()) == "data.frame"
  })
  outputOptions(output, "showDownload", suspendWhenHidden = FALSE)
  
  
  # base R plot fo rthe power curve
  output$powerCurve <- renderPlot({
    # Get the power analysis results
    data <- calc_power()
    
    # Check if we have valid data and a selected parameter
    if (class(data) == "data.frame" && nrow(data) > 0 && 
        !is.null(input$selected_parameter) && input$selected_parameter != "") {
      
      # Filter data for selected parameter
      plot_data <- data[data$Parameter == input$selected_parameter, ]
      
      # Check if filtered data exists
      if (nrow(plot_data) == 0) return(NULL)
      
      # Model name lookup using your existing choices
      model_lookup <- list(
        "one_mediator" = "One Mediator",
        "two_parallel_mediators" = "Two Parallel Mediators",
        "two_serial_mediators" = "Two Serial Mediators", 
        "three_parallel_mediators" = "Three Parallel Mediators"
      )
      
      # Get model display name
      model_display <- model_lookup[[input$model]]
      if (is.null(model_display)) {
        model_display <- input$model  # fallback to raw value
      }
      
      conf_level <- if("Confidence_Level" %in% names(plot_data) && length(plot_data$Confidence_Level) > 0) {
        as.character(plot_data$Confidence_Level[1])
      } else {
        as.character(input$conf)
      }
      
      input_method <- if("Input_Method" %in% names(plot_data) && length(plot_data$Input_Method) > 0) {
        as.character(plot_data$Input_Method[1])
      } else {
        input$input_method
      }
      
      input_display <- switch(as.character(input_method),
                              "correlations" = "Correlations",
                              "stdcoef" = "Standardized Coefficients",
                              input_method
      )
      
      # Create the plot
      plot(plot_data$N, plot_data$Power, 
           type = "l", 
           lwd = 2, 
           col = "blue",
           xlab = "Sample Size (N)", 
           ylab = "Statistical Power",
           ylim = c(0, 1),
           main = paste("Power Analysis:", model_display, 
                        "\nParameter:", input$selected_parameter,
                        "| Confidence Level:", paste0(conf_level, "%"),
                        "| Input Method:", input_display))
      
      # Add confidence interval lines
      lines(plot_data$N, plot_data$LL, lty = 2, col = "red", lwd = 1.5)
      lines(plot_data$N, plot_data$UL, lty = 2, col = "red", lwd = 1.5)
      
      # Add horizontal line at 80% power
      abline(h = 0.8, col = "gray", lty = 3)
      
      # Add grid
      grid(col = "lightgray", lty = 1)
      
      # Add legend
      legend("bottomright", 
             legend = c("Power", "95% CI", "80% Power"),
             col = c("blue", "red", "gray"),
             lty = c(1, 2, 3),
             lwd = c(2, 1.5, 1),
             bg = "white")
    }
  })
  
  # Plot options: 
  observeEvent(calc_power(), {
    data <- calc_power()
    if (class(data) == "data.frame" && nrow(data) > 0 && "Parameter" %in% names(data)) {
      # Get unique parameters
      unique_params <- unique(data$Parameter)
      
      # Create named list for dropdown
      param_choices <- setNames(unique_params, unique_params)
      
      # Update the selectInput choices
      updateSelectInput(session, "selected_parameter", 
                        choices = param_choices,
                        selected = unique_params[1])
    } else {
      # Clear choices if no valid data
      updateSelectInput(session, "selected_parameter", 
                        choices = NULL)
    }
  })

  # Render Objective Input Options
  output$obj_options <- renderUI({
    if (input$obj == "choose_n") {
      list(
        
        # Sample size input form
        withTags(
          table(
            td(nowrap = NA, label("Sample Size (N)")),
            td(numericInput(inputId = "N", label = NULL, value = 100))
          )
        )
      )
    } else {
      list(
        
        # Target power input form
        withTags(
          table(
            td(nowrap = NA, label("Target Power")),
            td(numericInput(inputId = "TarPow", label = NULL, value = 0.80,
                            width = "100%"))
          )
        ),
        
        # Minimum sample size input form
        withTags(
          table(
            td(nowrap = NA, label("Minimum N")),
            td(numericInput(inputId = "Nlow", label = NULL, value = 50))
          )
        ),
        
        # Maximum sample size input form
        withTags(
          table(
            td(nowrap = NA, label("Maximum N")),
            td(numericInput(inputId = "Nhigh", label = NULL, value = 200))
          )
        ),
        
        # Sample size steps input form
        withTags(
          table(
              td(nowrap = NA, label("Sample Size Steps")),
              td(numericInput(inputId = "Nsteps", label = NULL, value = 1))
            )
        )
      )
    }
  })
  
  # Render Image of Mediation Model 
  output$model_image <- renderImage({
    filename <- normalizePath(file.path(paste0('./images/', input$model, '.jpg'))) # Note: This can be reactive
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Mediation model path diagram"))
    }, deleteFile = FALSE)

  # Render Input Options UI
  
  # NOTE 1: The '$value' is included because source() returns a logical in addition
  # to the UI output when running source in this way.
  
  # NOTE 2: If you examine the ui.R files - containing the model-specific UI
  # input code, you'll notice that text input is obtained. There doesn't seem
  # to be a straightforward way to get rid of the step selector in the
  # numericInput() widgets, so the suggested solution is to import text and
  # convert to numeric (which is done in the model-specific power analysis
  # files)
  
  observeEvent(input$input_method, {
    if (input$input_method == "correlations") {
      output$input_options <- renderUI({
        source(paste0("./code/", input$model, "_correlations_ui.R"), local = TRUE)$value  
      })
    }
    if (input$input_method == "stdcoef") {
      output$input_options <- renderUI({
        source(paste0("./code/", input$model, "_stdcoef_ui.R"), local = TRUE)$value  
      })
    }
    # Other input methods could go here
  })
}