Strains_longevity_data <- read.csv(file = "Summary_of_data_MASTER.csv", header = TRUE, skip = 2)

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinyjs)  # Added library for JavaScript interaction.

# Data preprocessing: Convert columns to appropriate data types
Strains_longevity_data$lab <- as.factor(Strains_longevity_data$Where.mice.maintained)
Strains_longevity_data$Sex <- as.factor(Strains_longevity_data$Sex)
Strains_longevity_data$First.author <- as.factor(Strains_longevity_data$First.Author)

Strains_longevity_data$mean  <- as.numeric(Strains_longevity_data$mean)
Strains_longevity_data$SE  <- as.numeric(Strains_longevity_data$SE)
Strains_longevity_data$Maximum.lifespan  <- as.numeric(Strains_longevity_data$Maximum.lifespan)
Strains_longevity_data$median  <- as.numeric(Strains_longevity_data$median)
Strains_longevity_data$years  <- as.numeric(Strains_longevity_data$Year.paper.published)
Strains_longevity_data$SD  <- as.numeric(Strains_longevity_data$SD)
Strains_longevity_data$CV  <- as.numeric(Strains_longevity_data$CV)
Strains_longevity_data$Age.started..estimated.approx..days.  <- as.numeric(Strains_longevity_data$Age.started..estimated.approx..days.)
Strains_longevity_data$Year.paper.published  <- as.numeric(Strains_longevity_data$Year.paper.published)

Strains_longevity_data <- Strains_longevity_data[order(Strains_longevity_data$Strain),]

# Define the Shiny UI layout
ui <- fluidPage(
  titlePanel("Mouse Longevity Data Explorer"),
  fluidRow(
    column(width = 2,
           selectInput("x_axis", "X-Axis Column:", choices = colnames(Strains_longevity_data), selected = "Strain")
    ),
    column(width = 2,
           selectInput("y_axis", "Y-Axis Column:", choices = colnames(Strains_longevity_data), selected = "mean")
    ),
    column(width = 2,
           selectInput("error_bar", "Error Bar Column:", choices = c("SE", "SD", "CV"))
    ),
    column(width = 2,
           selectInput("plot_type", "Plot Type:", choices = c("scatter", "boxplot"))
    ),
    column(width = 2,
           selectInput("filter_column", "Filter Column:", choices = colnames(Strains_longevity_data), selected = "Where.mice.maintained")
    ),
    column(width = 2,
           selectInput("filter_value", "Filter Value:", choices = NULL)
    ),
    column(width = 2,
           selectInput("color_by", "Color By:", choices = colnames(Strains_longevity_data), selected = "Sex")
    ),
    column(width = 2,
           checkboxInput("filter_checkbox", "Turn Filtering On", value = FALSE)
    ),
    column(width = 2,
           selectInput("intervention_filter_column", "Intervention Filter Column:", choices = colnames(Strains_longevity_data), selected = "Intervention")
    ),
    column(width = 2,
           selectInput("intervention_filter_value", "Intervention Filter Value:", choices = NULL, selected = "Control")
    ),
    column(width = 2,
           checkboxInput("intervention_filter_checkbox", "Turn Intervention Filtering On", value = TRUE)
    ),
    column(width = 2,
           selectInput("shape_by", "Shape By:", choices = colnames(Strains_longevity_data), selected = "Where.mice.maintained")
    ),
    column(width = 2,
           actionButton("reset_button", "Reset to Defaults")
    ),
    column(width = 2,
           textInput("search_term", "Search Term:")
    ),
    # Add a checkbox to enable/disable the Age Cutoff (Days) filter
    column(width = 2,
           checkboxInput("age_cutoff_checkbox", "Use Age Cutoff Filter", value = FALSE)
    ),
    # Add a numeric input for age cutoff with a default value of 3650
    column(width = 2,
           numericInput("age_cutoff", "Max age at start cutoff (Approx. Days):", value = 3650, min = 0, step = 1)
    ),
    
    column(width = 2,
           sliderInput("year_range", "Select Year Range:",
                       min = min(Strains_longevity_data$Year.paper.published, na.rm = TRUE),
                       max = max(Strains_longevity_data$Year.paper.published, na.rm = TRUE),
                       value = c(min(Strains_longevity_data$Year.paper.published, na.rm = TRUE), 
                                 max(Strains_longevity_data$Year.paper.published, na.rm = TRUE)),
                       step = 1,
                       sep = "")
    )
  ),
  
  fluidRow(
    column(width = 12,
           textOutput("doi_url_display"),  # Move this line above the plot
           plotlyOutput("plot")
    )
  )
)

# Define the Shiny server logic
# Define the Shiny server logic
server <- function(input, output, session) {
  
  # Define default values for input fields
  default_values <- reactive({
    list(
      x_axis = "Strain",
      y_axis = "mean",
      error_bar = "SE",
      plot_type = "scatter",
      filter_column = "Where.mice.maintained",
      filter_value = NULL,
      color_by = "Sex",
      filter_checkbox = FALSE,
      intervention_filter_column = "Intervention",
      intervention_filter_value = "Control",
      intervention_filter_checkbox = TRUE,
      shape_by = "Where.mice.maintained"
    )
  })
  
  # Reset button click event
  observeEvent(input$reset_button, {
    default <- default_values()
    updateSelectInput(session, "x_axis", selected = default$x_axis)
    updateSelectInput(session, "y_axis", selected = default$y_axis)
    updateSelectInput(session, "error_bar", selected = default$error_bar)
    updateSelectInput(session, "plot_type", selected = default$plot_type)
    updateSelectInput(session, "filter_column", selected = default$filter_column)
    updateSelectInput(session, "filter_value", selected = default$filter_value)
    updateSelectInput(session, "color_by", selected = default$color_by)
    updateCheckboxInput(session, "filter_checkbox", value = default$filter_checkbox)
    updateSelectInput(session, "intervention_filter_column", selected = default$intervention_filter_column)
    updateSelectInput(session, "intervention_filter_value", selected = default$intervention_filter_value)
    updateCheckboxInput(session, "intervention_filter_checkbox", value = default$intervention_filter_checkbox)
    updateSelectInput(session, "shape_by", selected = default$shape_by)
    updateNumericInput(session, "age_cutoff", value = NULL)
  })
  
  observe({
    # Update filter values based on selected filter column
    filter_column_values <- unique(Strains_longevity_data[, input$filter_column])
    updateSelectInput(session, "filter_value", choices = filter_column_values)
  })
  
  observe({
    # Update intervention filter values based on selected intervention filter column
    intervention_filter_column_values <- unique(Strains_longevity_data[, input$intervention_filter_column])
    updateSelectInput(session, "intervention_filter_value", choices = intervention_filter_column_values)
  })
  
  filtered_data <- reactive({
    filtered_data <- Strains_longevity_data
    
    if (input$filter_checkbox) {
      if (!is.null(input$filter_column) && input$filter_column != "" && !is.null(input$filter_value) && input$filter_value != "") {
        filtered_data <- filtered_data %>%
          filter(!!sym(input$filter_column) == input$filter_value)
      }
    }
    
    # Apply intervention filter if the checkbox is checked
    if (input$intervention_filter_checkbox) {
      if (!is.null(input$intervention_filter_column) && input$intervention_filter_column != "" && 
          !is.null(input$intervention_filter_value) && input$intervention_filter_value != "") {
        filtered_data <- filtered_data %>%
          filter(!!sym(input$intervention_filter_column) == input$intervention_filter_value)
      }
    }
    
    # Filter the data to exclude rows with NA or blank values in x and y axes
    filtered_data <- filtered_data %>%
      filter(!is.na(!!sym(input$x_axis)) & !is.na(!!sym(input$y_axis)) & !!sym(input$x_axis) != "" & !!sym(input$y_axis) != "")
    
    filtered_data <- filtered_data %>%
      filter(Year.paper.published >= input$year_range[1] & Year.paper.published <= input$year_range[2])
    
    # Add the search term filter
    search_term <- input$search_term
    if (!is.null(search_term) && search_term != "") {
      filtered_data <- filtered_data %>%
        filter(grepl(search_term, Strain) | grepl(search_term, Maternal.strain) | grepl(search_term, Paternal.strain))
    }
    
    # Add the age cutoff filter if the checkbox is checked and a value is entered
    if (input$age_cutoff_checkbox) {
      age_cutoff <- input$age_cutoff
      if (!is.null(age_cutoff)) {
        filtered_data <- filtered_data %>%
          filter(!!sym("Age.started..estimated.approx..days.") <= age_cutoff)
      }
    }
    
    # Check the order of columns in filtered_data
    print(names(filtered_data))
    
    # Select columns in the desired order
    select_cols <- c("DOI", colnames(filtered_data)[-which(names(filtered_data) %in% c("DOI"))])
    filtered_data <- filtered_data %>%
      select(select_cols)
    
    filtered_data
  })
  
  output$plot <- renderPlotly({
    x_axis_col <- input$x_axis
    y_axis_col <- input$y_axis
    error_bar_col <- input$error_bar
    color_by <- input$color_by
    shape_by <- input$shape_by
    
    p <- ggplot(filtered_data(), aes_string(x = x_axis_col, y = y_axis_col, color = color_by, shape = shape_by))
    
    if (input$plot_type == "scatter") {
      p <- p + geom_point()
    } else if (input$plot_type == "boxplot") {
      p <- p + geom_boxplot()
    }
    
    if (error_bar_col == "SE") {
      p <- p + geom_errorbar(aes_string(ymin = paste0(y_axis_col, " - SE"), ymax = paste0(y_axis_col, " + SE")))
    } else if (error_bar_col == "SD") {
      p <- p + geom_errorbar(aes_string(ymin = paste0(y_axis_col, " - SD"), ymax = paste0(y_axis_col, " + SD")))
    } else if (error_bar_col == "CV") {
      p <- p + geom_errorbar(aes_string(ymin = paste0(y_axis_col, " - CV"), ymax = paste0(y_axis_col, " + CV")))
    }
    
    p <- p + labs(x = x_axis_col, y = y_axis_col)
    
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    unique_shapes <- seq_len(length(unique(filtered_data()[[shape_by]])))
    
    p <- p + scale_shape_manual(values = unique_shapes)
    
    p <- ggplotly(p, key = ~DOI)  # Use the DOI column as the key
    
    # Observe the plotly click event and register the event once the plot is rendered
    observe({
      observeEvent(input$plot_click, {
        event_register("plot", "plotly_click")
      })
    })
    
    p <- p %>% 
      layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          y = -0.5,
          traceorder = "normal",
          title = list(text = "Legend"),
          xanchor = "center",
          yanchor = "top",
          itemsizing = "constant"
        )
      )
    
    p <- p %>% 
      layout(height = 800)
    
    p
  })
  
  observe({
    event_data <- event_data("plotly_click")
    if (!is.null(event_data)) {
      # Extract selected values from the event data
      selected_strain <- event_data$x
      selected_mean <- event_data$y
      
      # Tolerance level for matching means
      tolerance <- 0.0000001  # Adjust this value based on your preference
      
      # Check if selected_strain is a valid index
      if (!is.na(selected_strain) && selected_strain >= 1 && selected_strain <= length(unique(filtered_data()$Strain))) {
        # Extract the selected strain based on the index
        selected_strain_value <- sort(unique(filtered_data()$Strain))[selected_strain]
        
        # Filter the data based on the selected values
        clicked_point_data <- Strains_longevity_data %>%
          filter(Strain == selected_strain_value)
        
        # Find the row with the closest mean value to the clicked point
        closest_rows <- clicked_point_data[which.min(abs(clicked_point_data$mean - selected_mean)), ]
        
        # If there are multiple rows with the same closest mean, prioritize based on "Sex" and "Where.mice.maintained."
        if (nrow(closest_rows) > 1) {
          # Prioritize based on "Sex"
          closest_rows <- closest_rows[which.min(abs(closest_rows$Sex - unique(closest_rows$Sex))), ]
          
          # If there are still multiple rows with the same "Sex," prioritize based on "Where.mice.maintained."
          if (nrow(closest_rows) > 1) {
            closest_rows <- closest_rows[which.min(abs(closest_rows$Where.mice.maintained - unique(closest_rows$Where.mice.maintained))), ]
          }
        }
        
        # Extract the DOI from the closest row
        doi <- closest_rows$DOI
        
        # Check if DOI is non-empty before rendering
        if (!is.null(doi) && length(doi) > 0) {
          # Return the DOI as text to be rendered
          output$doi_url_display <- renderText({
            paste("Clicked point link:\n", doi)
          })
        } else {
          output$doi_url_display <- renderText({
            "DOI not found for the clicked point."
          })
        }
      } else {
        # Use the first index as a fallback
        selected_strain_value <- sort(unique(filtered_data()$Strain))[1]
        clicked_point_data <- Strains_longevity_data %>%
          filter(Strain == selected_strain_value)
        
        # Find the row with the closest mean value to the clicked point
        closest_rows <- clicked_point_data[which.min(abs(clicked_point_data$mean - selected_mean)), ]
        
        # If there are multiple rows with the same closest mean, prioritize based on "Sex" and "Where.mice.maintained."
        if (nrow(closest_rows) > 1) {
          # Prioritize based on "Sex"
          closest_rows <- closest_rows[which.min(abs(closest_rows$Sex - unique(closest_rows$Sex))), ]
          
          # If there are still multiple rows with the same "Sex," prioritize based on "Where.mice.maintained."
          if (nrow(closest_rows) > 1) {
            closest_rows <- closest_rows[which.min(abs(closest_rows$Where.mice.maintained - unique(closest_rows$Where.mice.maintained))), ]
          }
        }
        
        # Extract the DOI from the closest row
        doi <- closest_rows$DOI
        
        # Check if DOI is non-empty before rendering
        if (!is.null(doi) && length(doi) > 0) {
          # Return the DOI as text to be rendered
          output$doi_url_display <- renderText({
            paste("Clicked point link:\n", doi)
          })
        } else {
          output$doi_url_display <- renderText({
            "DOI not found for the clicked point."
          })
        }
      }
    }
  })
}


# Run the Shiny app
shinyApp(ui, server)
