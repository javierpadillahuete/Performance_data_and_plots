
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# https://javier-nursa.shinyapps.io/test_spider_plot/


if (!require("shiny")) install.packages("shiny")
if (!require("fmsb")) install.packages("fmsb")
if (!require("ggplot2")) install.packages("ggplot2") 
if (!require("plotly")) install.packages("plotly") 
if (!require("lubridate")) install.packages("lubridate") 
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("RPostgres")) install.packages("RPostgres") #If you use database
if (!require("paletteer")) install.packages("paletteer")
if (!require("RColorBrewer")) install.packages("RColorBrewer") 
if (!require("DBI")) install.packages("DBI")

library(rsconnect)
library(shiny) 
library(ggplot2)
library(plotly)
library(fmsb) 
library(tidyverse)
library(lubridate)
library(paletteer)
library(RColorBrewer)
library(DBI)
library(RPostgres)


## Connection to DB: ---------

DB_NAME <- Sys.getenv("DB_NAME")
A_value <- Sys.getenv("A_value")
B_Value <- Sys.getenv("B_Value") 

A_value <- as.numeric(A_value)
B_Value <- as.numeric(B_Value)

## Not needed in this case

##Cread the mocking or test data frame:

# SIMULATED DATA:

# Function to generate a single facility ID
generate_facility_id <- function() {
  letters <- sample(LETTERS, 3, replace = TRUE)  # 3 random uppercase letters
  numbers <- sample(1000:2000, 1)                # 1 random number between 1000 and 2000
  paste0(paste0(letters, collapse = ""), numbers) # Combine letters and numbers
}

set.seed(42)
scoring_df <- data.frame(
  facility_id = c(DB_NAME , replicate(50, generate_facility_id())), # Generate 50 unique IDs
  post_rate = c(A_value, runif(50, 0.2, 1)),
  scheduled_rate = c(A_value, runif(50, 0, 0.8)),
  cancellation_rate = c(B_Value, runif(50, 0, 0.4)),
  response_rate = c(A_value, runif(50, 0.5, 1)),
  reengagement_rate = c(A_value, runif(50, 0, 1)),
  dso = c(A_value, runif(50, 20, 100)),
  name = paste("Facility", 1:51),
  state_code = sample(state.abb, 51, replace = TRUE),
  metro = sample(c("Metro A", "Metro B", "Metro C", "Metro D"), 51, replace = TRUE)
)

# scoring_df <- dbGetQuery(aws_con, scoring_query)



# UI 

# Define UI for application that draws a plot  ----
ui <- fluidPage(
  
  # Application title
  titlePanel(title=div("Test Plots for Metrics", 
                       style = "font-family: Arial; font-weight: bold;")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout( 
    sidebarPanel(
      # You can add inputs here if needed
      radioButtons("include_avg", "Include Average Performance:", 
                   choices = c("Yes" = "yes", "No" = "no"),
                   selected = "yes"),  # Default selection
      
      selectInput("facilities_id_in_plot", "Facilities:", 
                  choices = c("NULL", 
                              paste(sort(unique(scoring_df$facility_id)),
                                    scoring_df$name[scoring_df$facility_id %in% 
                                                      sort(unique(scoring_df$facility_id))], sep = " || ")
                  ),
                  selected = "NULL",
                  multiple = TRUE),
      
      selectInput("metro_name", "Metro city:", 
                  choices = c("NULL", sort(unique(scoring_df$metro))), 
                  selected = "NULL",
                  multiple = TRUE),
      
      selectInput("state_name", "State:", 
                  choices = c("NULL", sort(unique(scoring_df$state_code))), 
                  selected = "NULL", 
                  multiple = TRUE)
    ),
    
    # Main Panel: Increased size of plot outputs!       ----------------------
    mainPanel(
      # Use plotOutput with specified height for the base R plot
      plotlyOutput("radarPlot", height = "680px")  ,   
      tags$br(), # Crucial for adding space
      tags$br(), # Add more space
      tags$br(), # Add even more
      tags$br(), # Add even more 
      tags$br(),
      tags$br(),
      # Use plotlyOutput with specified height for the plotly plots
      plotlyOutput("barPlot", height = "520px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  
  # Reactive expression to prepare data
  intermediate <- reactive({
    
    # Create the first Data Frame:  -----
    
    ## Data frame for Spider Plot   ------
    
    ## Fix the facilities ids:
    # Extracting entries before the " ||" delimiter
    facilities_id_select <- sub(" \\|\\|.*", "", input$facilities_id_in_plot)
    
    
    # Calculate statistics
    max_values <- apply(scoring_df[, c("post_rate", "scheduled_rate", 
                                       "cancellation_rate", 
                                       "response_rate", "reengagement_rate")], 2, max, na.rm = TRUE)
    max_values <- c("max_value", rep(1, 5))
    names(max_values) <- c("label","post_rate", "scheduled_rate", "cancellation_rate", 
                           "response_rate", "reengagement_rate")
    
    min_values <- c("min_value", rep(0, 5))  # Assuming minimum is 0 for all metrics
    names(min_values) <- c("label","post_rate", "scheduled_rate", "cancellation_rate", 
                           "response_rate", "reengagement_rate")
    
    mean_values <- colMeans(scoring_df[, c("post_rate", "scheduled_rate",
                                           "cancellation_rate", 
                                           "response_rate", "reengagement_rate")], na.rm = TRUE)
    mean_values <- c("mean_value", mean_values)
    names(mean_values) <- c("label","post_rate", "scheduled_rate", "cancellation_rate", 
                            "response_rate", "reengagement_rate")
    
    # Select rows based on facility IDs and calculate means for metro and state
    selected_rows_facility <- scoring_df[scoring_df$facility_id %in% facilities_id_select, 
                                         c("facility_id","post_rate", "scheduled_rate", "cancellation_rate", 
                                           "response_rate", "reengagement_rate")]
    names(selected_rows_facility) <- c("label","post_rate", "scheduled_rate", "cancellation_rate", 
                                       "response_rate", "reengagement_rate")
    
    # Calculate means for each metro (group by metro)
    selected_rows_metro <- scoring_df %>%
      dplyr::filter(metro %in% input$metro_name) %>%
      group_by(metro) %>%
      summarise(across(c("post_rate", "scheduled_rate", "cancellation_rate", 
                         "response_rate", "reengagement_rate"), mean, na.rm = TRUE))
    names(selected_rows_metro) <- c("label","post_rate", "scheduled_rate", "cancellation_rate", 
                                    "response_rate", "reengagement_rate")
    
    # Calculate means for each state (group by state_code)
    selected_rows_state <- scoring_df %>%
      dplyr::filter(state_code %in% input$state_name) %>%
      group_by(state_code) %>%
      summarise(across(c("post_rate", "scheduled_rate", "cancellation_rate", 
                         "response_rate", "reengagement_rate"), mean, na.rm = TRUE))
    names(selected_rows_state) <- c("label","post_rate", "scheduled_rate", "cancellation_rate", 
                                    "response_rate", "reengagement_rate")
    
    
    # Create data frame in required radarchart format
    if (input$include_avg == "yes") {
      plot_data <- rbind(
        max_values, 
        min_values, 
        mean_values, ## mean value
        selected_rows_state,
        selected_rows_metro,
        selected_rows_facility
      ) %>% 
        as.data.frame() %>%
        `colnames<-`(c("Label", "Post rate", "Scheduled rate", "Cancellation rate", 
                       "Response rate", "Reengagement rate"))
      
    } else {
      plot_data <- rbind(
        max_values, 
        min_values,
        selected_rows_state,
        selected_rows_metro,
        selected_rows_facility
      ) %>% 
        as.data.frame() %>%
        `colnames<-`(c("Label", "Post rate", "Scheduled rate", "Cancellation rate", 
                       "Response rate", "Reengagement rate"))
    }
    
    # Convert all columns except the first to numeric
    plot_data <- plot_data %>% mutate(across(-1, ~ as.numeric(.)))
    
    # Generate legend labels
    legend_labels <- c(
      if (input$include_avg == "yes") "Average Performance",
      if (!all(is.na(selected_rows_state))) paste("State:", selected_rows_state$label),
      if (!all(is.na(selected_rows_metro))) paste("Metro:", selected_rows_metro$label),
      if (!all(is.na(selected_rows_facility))) paste(selected_rows_facility$label,
             scoring_df$name[scoring_df$facility_id %in% selected_rows_facility$label], sep = "\n")
    )
    
    # Round all columns except the first to 4 decimal places
    plot_data <- plot_data %>% mutate(across(-1, ~ round(., 4)))
    
    # Create the second Data Frame, plot_data_long:   ------ 
    
    plot_data$Label <- c("max_value", "min_value", legend_labels)
    
    plot_data_long <- plot_data %>%
      pivot_longer(cols = -1, names_to = "Performance", values_to = "Score")
    
    plot_data_long <- plot_data_long %>% dplyr::filter(!Label %in% c("max_value", "min_value"))
    
    # Specify the desired order for Performance
    desired_order_performance <- c("Post rate", "Scheduled rate", "Cancellation rate", 
                                   "Response rate", "Reengagement rate")
    
    # Convert Performance to a factor with specified levels
    plot_data_long$Performance <- factor(plot_data_long$Performance, levels = desired_order_performance)
    
    # Specify the desired order for Label
    desired_order_label <- unique(plot_data_long$Label)
    
    # Convert Label to a factor with specified levels
    plot_data_long$Label <- factor(plot_data_long$Label, levels = desired_order_label) 
    
    # Return as a list
    list(
      plot_data = plot_data,
      legend_labels = legend_labels,
      plot_data_long = plot_data_long
    )
    
  })
  
  
  # BAR Plot:   ------
  output$barPlot <- renderPlotly({
    
    plot_data_long <- intermediate()$plot_data_long
    
    plot_data2 <- intermediate()$plot_data
    
    # Generate colors dynamically based on the number of rows in plot_data
    num_rows <- nrow(plot_data2) - 2  # Subtract max and min rows
    colors <- paletteer_d("ggsci::category20_d3", num_rows)  # Automatic color palette 
    
    # Create bar plot 
    p_bar <- ggplot(plot_data_long, aes(x = Performance, y = Score, fill = Label)) +
      geom_bar(stat = "identity", position = position_dodge(), 
               alpha = 0.8, width = 0.55) + 
      scale_fill_manual(values = colors) +
      labs(title = "Performance Metrics Bar Plot", 
           x = "Performance", y = "Score") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 30, vjust = 1, color = "grey25"),  # Adjusted text alignment
        panel.grid.minor = element_blank()
      )
    
    # Convert to Plotly
    plotly_p_bar <- ggplotly(p_bar)
    
    # Modify layout to add dashed grid lines
    plotly_p_bar <- plotly::layout(
      plotly_p_bar,
      xaxis = list(
        gridcolor = 'grey70',
        gridwidth = 0.8,
        showgrid = TRUE,
        zeroline = TRUE 
      ),
      yaxis = list(
        gridcolor = 'grey20',
        gridwidth = 0.8,
        showgrid = TRUE,
        zeroline = FALSE,
        tickmode = "linear",  # Ensures evenly spaced tick marks
        dtick = 0.1  # Forces grid lines every 0.1
      ),
      title = list(      #  <---  Move title styling to plotly::layout
        text = "<b>Performance Metrics Bar Plot</b>",  # Bold here
        font = list(size = 24),  # Set title font size here!
        x = 0.10,
        y = 0.90
      ), 
      hoverlabel = list(
        font = list(family = "Arial", size = 15 )
      ),
      margin = list(t = 150) #Set the top margin to 150
      
    )
    
    # Display the plot
    plotly_p_bar
  })
  
  
  # Radar/Spider Plot:   ------
  output$radarPlot <- renderPlotly({
    
    ## Call the data:
    plot_data_long <- intermediate()$plot_data_long
    
    # Reshape the data for Plotly (wide format, one row per group)
    plot_data_wide <- plot_data_long %>%
      pivot_wider(names_from = Performance, values_from = Score) %>%
      column_to_rownames(var = "Label")   #  Row names are essential for plotly radial charts.
    
    # Add max and min values for radial scaling (using fmsb for convenience, even if not using chart2)
    max_min <- data.frame(
      "Post rate" = c(1, 0),         # Maximum and minimum for each performance metric
      "Scheduled rate" = c(1, 0),
      "Cancellation rate" = c(1, 0), # Consider a different upper limit if >1 is impossible
      "Response rate" = c(1, 0),
      "Reengagement rate" = c(1, 0)
    )
    rownames(max_min) <- c("Max", "Min")
    
    
    # 2. Plotly Implementation
    
    fig <- plot_ly() 
    
    num_rows <- nrow(plot_data_wide)  # Get number of groups for colors and linetypes.
    colors <- paletteer_d("ggsci::category20_d3", num_rows)  # Automatic color palette
    linetypes <- rep(c("solid", "dash", "dot", "dashdot", "longdashdot"), length.out = num_rows) #Repeat if needed 
    
    # --- Add each "group" (Label) as a separate trace ---
    for (i in 1:num_rows) {
      
      # --- CRITICAL CHANGE: Close the loop ---
      r_values <- c(as.numeric(plot_data_wide[i, ]), plot_data_wide[i, 1]) # Append first value to the end
      theta_values <- c(colnames(plot_data_wide), colnames(plot_data_wide)[1]) # Append first name to the end
      
      fig <- fig %>% add_trace(
        type = 'scatterpolar',
        mode = "lines+markers",
        r = r_values,          # Use the closed r_values
        theta = theta_values,    # Use the closed theta_values
        name = rownames(plot_data_wide)[i],
        line = list(color = colors[i], width = 4.2, dash = linetypes[i]),  # Color, width, and linetype
        opacity = 0.7 , 
        marker = list(size = 9),
        hovertemplate = paste( #This IS the important part
          "<b>%{fullData.name}</b><br><br>",
          "Metric: %{theta}<br>",
          "<b>Score: %{r}<extra></extra><b>"  # <extra></extra> removes the "trace" label
        )
      )
    }
    
    # --- Customize Radial Axis ---
    
    fig <- fig %>% layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          showgrid = TRUE,
          range = c(0, 1),       # Set radial axis range
          tickvals = seq(0, 1, by = 0.1),  # Grid lines every 0.1
          # ticktext = seq(0, 1, by = 0.1),
          gridcolor = "grey",
          gridwidth = 0.8,
          griddash = "dash",  #  MAKE GRID LINES DOTTED
          showline = TRUE,
          linecolor="grey30",
          tickfont = list(size = 12),
          side = "clockwise", # Labels on opposite side
          tickangle = 0         # Rotate labels to be "vertical" at the bottom
          
        ),
        angularaxis = list(
          rotation = 45,
          direction = "clockwise", # You might or might not want this.
          tickfont = list(size = 16), #  INCREASE THETA LABEL FONT SIZE
          showgrid = TRUE,
          gridshape = "linear",
          griddash = "dash"
        )
      ),
      title = list(
        text = "<b>Performance Metrics Spider Plot</b>",
        x = 0.10,
        y = 0.96,
        font = list(size = 24)
      ),
      legend = list(
        title = list(text = "<b>Label</b>" ),  # can Increase title size
        font = list(size = 16)  # Increase legend font size
      ),
      margin = list(t = 160   ## Changes the space for the title
      ), 
      hoverlabel = list(
        font = list(family = "Arial", size = 15, color = "grey20")
      ),
      showlegend = TRUE  # Also ensure showlegend is TRUE here
    )
    
    # --- Display the plot ---
    fig
    
    
  })
} 


# Run the application 
shinyApp(ui = ui, server = server)



## Deploy your application:       ----------------------------------------------
## https://www.shinyapps.io/admin/#/dashboard
## library(rsconnect)

## This one seems to work:

# rsconnect::deployApp(
#   appPrimaryDoc = "app.R"
# )

## rsconnect::deployApp()


