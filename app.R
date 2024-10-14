library(shiny)
library(tidyverse)
library(readxl)
library(psych)
library(gt)
library(cowplot)
library(PerformanceAnalytics)
library(lattice)
library(plotly)

# # # Read in data:
# dat <- data.frame(read_excel('Data/Pinellas_County_Water_Quality_Data_2003_2024.xlsx',
#                              guess_max = 50000)) |>
#   mutate(TN = TKN + NOX) |>
#   select(Site, Date, Level, Secchi, Temp_Water, pH, DO_sat = DO.,
#          DO, Conductivity=Sp.Conductivity, Salinity, TN, TP, OP, Chl.a, TSS,
#          Turbidity, BOD5, Color, Aluminum, Alkalinity_total, Enterococci, E_coli) |>
#   mutate(Site = ifelse(substr(Site,1,1) %in% c('W','E','L','S','R'),
#                        substr(Site, 1, 2), Site)) |>
#   group_by(Site, Date, Level) |>
#   summarise(across(everything(), mean, na.rm = TRUE)) |>
#   ungroup() |>
#   as.data.frame()
# 
# saveRDS(dat, "Data/grouped_data.RData")

dat <- readRDS("Data/grouped_data.RData")

# Get site names
sites <- unique(dat$Site)
sLevel <- c(paste0(sites,": Surface"),paste0(sites,": Bottom"))
sLevel <- tibble(sLevel) |>
  arrange(str_extract(sLevel, "^[^:]+")) |>
  as.vector() 




ui <- fluidPage(
  titlePanel("Pinellas County Water Quality Exploratory Data Analysis"),
  sidebarLayout(
    sidebarPanel(width=3,
      selectInput("site", "Select a site:",
                  choices = sLevel, selected=sLevel[1], multiple=TRUE),
      # checkboxGroupInput("plots","Select plots to display:",
      #                    choices = c("Summary Stats","Time Series", "Boxplot",
      #                                "Histogram", "Correlation Matrix"),
      #                    selected = c("Summary Stats","Time Series", "Boxplot",
      #                                 "Histogram", "Correlation Matrix")),
      sliderInput("yr", "Select Years of Interest",
                  min = 2003, max = 2024,value=c(2003,2024),sep=""),
      # Add instructions for the user
      br(),
      tags$div(
        style = "font-size: 24px; color: black; font-weight: bold;",
        "Viewing the water quality data:",
      ),
      tags$div(
        style = "font-size: 18px; color: black;",
        "1. Select one or more sites from the dropdown menu",
        br(),
        "2. Click the tabs to view data for the selected site",
        br(),
        "3. Select the year range of the data by adjusting the slider"
      ),
      br(),
      tags$div(
        style = "font-size: 24px; color: black; font-weight: bold;",
        "Interpreting the plots:",
      ),
      tags$div(
        style = "font-size: 18px; color: black;",
        "1. Summary Statistics: Provides an overview of the data distribution",
        br(),
        "2. Time Series: Shows trends and seasonal patterns in the data over time",
        br(),
        "3. Boxplot: Visualizes data distribution and quartiles, helpful for identifying outliers",
        br(),
        "4. Histogram: Shows frequency of data values and the shape of the distribution",
        br(),
        "5. Correlation Matrix: Table of pairwise correlation coeffecients. Bottom left shows correlation
        plots and the uppper right shows associated correlation coefficient"
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabs"),
    )
  )
)


server <- function(input, output, session) {
  
  # Reactive value to keep track of currently existing tabs
  existing_tabs <- reactiveVal(character())
  
  observeEvent(input$site, {
    
    # Get the currently selected sites
    selected_sites <- input$site
    
    # Get currently existing tabs from the reactive value
    current_tabs <- existing_tabs()
    
    # Add new tabs for newly selected sites
    new_tabs <- setdiff(selected_sites, current_tabs)
    lapply(new_tabs, function(site) {
      # Filter the data for the current site
      site_name <- str_extract(site, "^[^:]+")
      level <- str_extract(site, '\\b\\w+$')
      
      filt <- dat |> 
        filter(
          Site == site_name,
          Level == level,
          year(Date) >= input$yr[1] & year(Date) <= input$yr[2]
        ) |>
        select(
          where(~!all(is.na(.)))
        )
      
      # Get numeric column names from the filtered data
      numeric_cols <- select(filt, where(is.numeric)) |> colnames()
      table_id <- paste0("table_", gsub("[: ]", "_", site))
      
      # Create plot IDs for each parameter
      time_series_ids <- paste0("time_series_", gsub("[: ]", "_", site), "_", numeric_cols)
      boxplot_ids <- paste0("boxplot_", gsub("[: ]", "_", site), "_", numeric_cols)
      histogram_ids <- paste0("histogram_", gsub("[: ]", "_", site), "_", numeric_cols)
      correlation_matrix_id <- paste0("correlation_matrix_", gsub("[: ]", "_", site))  # Correlation matrix ID
      
      # Insert the tab with the table, plots, and correlation matrix
      insertTab(inputId = "tabs",
                tabPanel(
                  title = site,
                  gt_output(outputId = table_id),
                  fluidRow(
                    # Loop over each parameter and create three plots in a row
                    lapply(seq_along(numeric_cols), function(i) {
                      param <- numeric_cols[i]
                      
                      fluidRow(
                        # Time Series Plot
                        column(4, plotlyOutput(outputId = time_series_ids[i])),
                        # Boxplot
                        column(4, plotlyOutput(outputId = boxplot_ids[i])),
                        # Histogram
                        column(4, plotlyOutput(outputId = histogram_ids[i]))
                      )
                    })
                  ),
                  # Correlation matrix at the bottom
                  fluidRow(
                    plotOutput(outputId = correlation_matrix_id, height = "600px")
                  ),
                  value = paste0("tab_", site)
                ),
                target = NULL)
      
      # Render the summary table filtered for the specific site
      output[[table_id]] <- render_gt({
        numeric_data <- select(filt |> filter(year(Date) >= input$yr[1] & 
                                                year(Date) <= input$yr[2]), where(is.numeric))
        summary_stats <- describe(numeric_data) |>
          select(-c(vars, trimmed, mad, range))
        
        vars <- colnames(numeric_data)
        summary_stats <- summary_stats |>
          add_column(Parameter = vars, .before = 1) |>
          drop_na() |>
          relocate(sd, .after=median)
        
        gt(summary_stats) |>
          tab_header(title = paste(site, "Water Quality Summary Statistics")) |>
          fmt_number(columns = 3:ncol(summary_stats), decimals = 2) |>
          cols_align(align = c("center"), columns = everything())
      })
      
      # Render the three plots (time series, boxplot, histogram) for each numeric parameter
      lapply(seq_along(numeric_cols), function(i) {
        param <- numeric_cols[i]
        
        # Time Series Plot
        output[[time_series_ids[i]]] <- renderPlotly({
          plot_ly(filt |> filter(year(Date) >= input$yr[1] & 
                                   year(Date) <= input$yr[2]), x = ~Date, y = as.formula(paste0("~", param)), 
                  type = 'scatter', mode = 'lines') %>%
            layout(title = paste(param, "Time Series"), xaxis = list(title = "Date"), 
                   yaxis = list(title = param))
        })
        
        # Boxplot
        output[[boxplot_ids[i]]] <- renderPlotly({
          plot_ly(filt |> filter(year(Date) >= input$yr[1] & 
                                   year(Date) <= input$yr[2]), y = as.formula(paste0("~", param)), 
                  type = 'box') %>%
            layout(title = paste(param, "Boxplot"), yaxis = list(title = param))
        })
        
        # Histogram
        output[[histogram_ids[i]]] <- renderPlotly({
          plot_ly(filt |> filter(year(Date) >= input$yr[1] & 
                                   year(Date) <= input$yr[2]), x = as.formula(paste0("~", param)), 
                  type = 'histogram',
                  marker = list(
                    color = 'lightblue',
                    line = list(color = 'black', width = 1)
                  )) %>%
            layout(title = paste(param, "Histogram"), xaxis = list(title = param), 
                   yaxis = list(title = "Count"))
        })
      })
      
      # Render the correlation matrix using PerformanceAnalytics package
      output[[correlation_matrix_id]] <- renderPlot({
        numeric_data <- select(filt |> filter(year(Date) >= input$yr[1] & 
                                                year(Date) <= input$yr[2]) , where(is.numeric))
        req(numeric_data)  # Ensure data exists before rendering the correlation matrix
        
        # Use PerformanceAnalytics' chart.Correlation function
        chart.Correlation(numeric_data, histogram = TRUE, pch = 19, method = 'spearman')
      })
    })
    
    # Remove tabs for deselected sites
    tabs_to_remove <- setdiff(current_tabs, selected_sites)
    lapply(tabs_to_remove, function(site) {
      removeTab(inputId = "tabs", target = paste0("tab_", site))
    })
    
    # Update the reactive value with the current list of tabs
    existing_tabs(selected_sites)
  })
}

shinyApp(ui, server)