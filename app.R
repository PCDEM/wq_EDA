library(shiny)
library(tidyverse)
library(readxl)
library(psych)
library(gt)
library(cowplot)
library(PerformanceAnalytics)
library(lattice)
library(plotly)

# Read in data:
dat <- data.frame(read_excel('Data/Pinellas_County_Water_Quality_Data_2003_2024.xlsx', 
                             guess_max = 50000)) |>
  mutate(TN = TKN + NOX) |>
  select(Site, Date, Level, Secchi, Temp_Water, pH, DO_sat = DO.,
         DO, Conductivity=Sp.Conductivity, Salinity, TN, TP, OP, Chl.a, TSS,
         Turbidity, BOD5, Color, Aluminum, Alkalinity_total, Enterococci, E_coli) |>
  mutate(Site = ifelse(substr(Site,1,1) %in% c('W','E','L','S','R'), 
                       substr(Site, 1, 2), Site)) |>
  group_by(Site, Date, Level) |>
  summarise(across(everything(), mean, na.rm = TRUE)) 

# Get site names
sites <- unique(dat$Site)
sLevel <- c(paste0(sites,": Surface"),paste0(sites,": Bottom"))
sLevel <- tibble(sLevel) %>%
  arrange(str_extract(sLevel, "^[^:]+")) %>%
  pull(sLevel)




ui <- fluidPage(
  titlePanel("Pinellas County Water Quality Exploratory Data Analysis"),
  sidebarLayout(
    sidebarPanel(width=3,
      selectInput("site", "Select a site:",
                  choices = sLevel, selected=sLevel[1], multiple=TRUE),
      checkboxGroupInput("plots","Select plots to display:",
                         choices = c("Summary Stats","Time Series", "Boxplot",
                                     "Histogram", "Correlation Matrix"),
                         selected = c("Summary Stats","Time Series", "Boxplot",
                                      "Histogram", "Correlation Matrix")),
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
      filtered_data <- dat |> filter(Site == site_name)
      
      # Get numeric column names from the filtered data
      numeric_cols <- select(filtered_data, where(is.numeric)) |> colnames()
      table_id <- paste0("table_", gsub("[: ]", "_", site))
      plot_ids <- paste0("plot_", gsub("[: ]", "_", site), "_", numeric_cols)
      
      # Insert the tab with the table and plots
      insertTab(inputId = "tabs",
                tabPanel(
                  title = site,
                  gt_output(outputId = table_id),
                  fluidRow(
                    lapply(plot_ids, function(id) {
                      column(6, plotlyOutput(outputId = id))
                    })
                  ),
                  value = paste0("tab_", site)
                ),
                target = NULL)
      
      # Render the summary table filtered for the specific site
      output[[table_id]] <- render_gt({
        # Generate summary statistics
        numeric_data <- select(filtered_data, where(is.numeric))
        summary_stats <- describe(numeric_data) |>
          select(-c(vars, trimmed, mad, range))
        
        vars <- colnames(numeric_data)
        summary_stats <- summary_stats |>
          add_column(Vars = vars, .before = 1) |>
          drop_na()
        
        gt(summary_stats) |>
          tab_header(title = paste(site, "Water Quality Summary Statistics")) |>
          fmt_number(columns = 3:ncol(summary_stats), decimals = 2) |>
          cols_align(align = c("center"), columns = everything())
      })
      
      # Render the time series plots for each numeric parameter
      lapply(seq_along(numeric_cols), function(i) {
        param <- numeric_cols[i]
        plot_id <- paste0("plot_", gsub("[: ]", "_", site), "_", param)
        
        output[[plot_id]] <- renderPlotly({
          plot_ly(filtered_data, x = ~Date, y = as.formula(paste0("~", param)), 
                  type = 'scatter', mode = 'lines') %>%
            layout(title = paste(site, "-", param), xaxis = list(title = "Date"), 
                   yaxis = list(title = param))
        })
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