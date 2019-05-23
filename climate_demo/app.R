library(shiny)
library(tidyverse)
library(pls)
library(shinyWidgets)
library(shinyBS)
library(shinydashboard)
library(snakecase)
library(plotly)
library(DT)

##################
# READ IN THE DATA
##################

# Only export data in countries of interest.

trade <- read_rds("trade_shiny.rds") %>% 
  
  # Remove unnecessary trade options.
  
  filter(! category == "commodities not specified according to kind") %>% 
  filter(! category == "all commodities")
  
climate <- read_rds("climate.rds")

##################
### USER INTERFACE
##################

header <- dashboardHeader(
  title = "Climate Vulnerability and International Trade Flows"
)

ui <- fluidPage(

  # A tab panel with the graphs of interest on it.

  titlePanel("Climate Vulnerability and International Trade Flows"),

  fluidRow(

    # Select a country.

    column(
      2,
      selectInput("country",
        h3("Country"),
        choices = unique(trade$country),
        selected = "Brazil"
      )
    ),

    # Select a trade category and flow (export/import).

    column(
      2,
      radioButtons("flow",
        h3("Flow type"),
        choices = list(
          "Export" = "Export",
          "Import" = "Import"
        ),
        selected = "Export"
      )
    ),

    # Select a trade category.

    column(
      2,
      selectInput(
        inputId = "category",
        label = h3("Commodity category"),
        choices = "Cereals",
        selected = "Cereals"
      )
    ),

    # Select years to create projections for.

    column(
      2,
      selectInput("year",
        h3("Projection"),
        choices = list(
          "2025" = 2025,
          "2030" = 2030,
          "2035" = 2035,
          "2040" = 2040,
          "2045" = 2045,
          "2050" = 2050
        ),
        selected = 2025
      )
    ),

    # Select a climate metric to adjust + degree of adjustment.

    column(
      2,
      selectInput("metric",
        h3("Climate metric"),
        choices = list(
          "Gain" = "gain",
          "Vulnerability" = "vulnerability",
          "Water" = "water",
          "Economic" = "economic",
          "Political stability" = "political_stability",
          "Ecological footprint" = "eco_footprint"
        ),
        selected = "gain"
      )
    ),

    column(
      2,
      numericInput("degree",
        h3("Annual change (%)"),
        value = 1
      )
    )
  ),

  # Display the plots.

  mainPanel(

    # Display a plot of the entire trend of the trade metric of interest.

    plotlyOutput("full_chart"),
    
    # Display a sentence about how the trade metric will change.
    
    ## goes here.
    
    # Display a plot of the projected trend.

    plotlyOutput("projected_chart"),

    # Display a table of the generated values.
    
    dataTableOutput("projections_table")
  )
)


##########
### SERVER
##########

server <- function(input, output, session) {
  # toggleModal(session, "startupModal", toggle = "open")
  #
  # output$introduction <- renderText({
  #   HTML("This is a test message.")
  # })

  ##########
  # MAIN TAB
  ##########

  # Get the reactive values from user input.
  # Use data only for the country of interest.

  # Filter the trade data for country, export/import, and trade category.

  dft_filtered <- reactive({
    trade %>%
      
      # Filter based on user input.
      
      filter(country == input$country) %>%
      filter(flow == input$flow) %>%
      filter(category == tolower(input$category))
  })

  # Filter the climate data for the country of interest.

  dfc_filtered <- reactive({
    climate %>%
      mutate(year = as.numeric(year)) %>%
      filter(name == input$country)
  })

  # Make a list of categories appropriate for each country.

  observe({
    categories <- trade %>%
      filter(country == input$country) %>%
      filter(flow == input$flow)

    categories <- unique(categories$category) %>%
      to_sentence_case()

    updateSelectInput(session, "category", choices = c(categories), selected = "Cereals")
  })

  ### Plot the projections.
  # Merge the two datasets and generate predictions.

  projections <- reactive({

    ### Merge the two datasets.

    dfm <- dft_filtered() %>%

      # Filter for the overlapping years (1995-2015).

      filter(year < 2016 & year >= 1995) %>%

      # Join by year.

      left_join(dfc_filtered(), by = "year") %>% 

      # Drop non-numeric variables.

      select(-commodity, -flow, -quantity_name, -category, -country, -health_external, -iso3, -name, -quantity)

    # Add a t-1 predictor to emulate a time series.

    dfm <- dfm %>%
      mutate(tm1 = c(dfm$trade_usd[2:nrow(dfm)], NA))

    ### Generate the predictions.
    # First generate the business as usual scenario as a linear estimation of the past five years.

    recent <- dfm[1:5, 2:67] %>%
      mutate(year = c(2015:2011))

    # Choose a custom time span.

    span <- c(2020:input$year)
    bau <- c()
    results <- tibble(rep(NA, length(2020:input$year)))

    for (var in names(recent)) {

      # Linearly predict the future values of all of the climate scores.

      m <- lm(get(var) ~ year, data = recent)

      bau <- c(bau, predict(m, newdata = tibble(year = span)))

      prediction <- as_tibble(bau)
      colnames(prediction) <- var

      results <- bind_cols(results, prediction)
      bau <- c()
    }

    # Ignore the initiation column and the year column.

    results <- results[, 2:67]

    # Fit the PLS model.

    mod_pls <- plsr(trade_usd ~ .,
      10,
      data = dfm,
      validation = "LOO"
    )

    # Manually find the optimal number of components.
    # This is equivalent to performing a visual test.
    # Start by assigning 1 component as the minimum RMsEP.
    
    min <- RMSEP(mod_pls)$val[3]
    m <- 1
    
    # Test the other RMSEs.
    
    for (n in seq(3, 21, 2)) {
      
      test <- RMSEP(mod_pls)$val[n]
      
      if (test < min) {
        min <- test
        m <- floor(n / 2)
      }
    }
    n <- m
    
    # Predict the new trade amount.
    # Add the new variables to a blank df.

    predictions <- predict(mod_pls, ncomp = n, newdata = results) %>%
      as_tibble()

    colnames(predictions) <- "trade_usd"

    # Visualize the changes in trade for the predictions.
    # First combine all of the results into one dataframe.
    # Label the predictions.

    predictions <- predictions %>%
      bind_cols(tibble(value = span)) %>%
      mutate(year = value) %>%
      select(-value) %>%
      mutate(group = "Predicted (business as usual)")

    # Make a new dataframe with the actual values used for the projections.

    actual <- tibble(
      trade_usd = dfm$trade_usd[1:26],
      year = 1991:2016,
      group = "Actual"
    )

    # Combine both to plot.

    final <- bind_rows(actual, predictions)
    
    # Adjust negative values to be zero.
    
    for (n in 1:nrow(final)) {
      if (final$trade_usd[n] < 0) {
        final$trade_usd[n] <- 0
      }
    }
    
    # Return the final plottable values.
    
    final
  })

  ###
  # GRAPHS
  ###

  # Plot a graph of the entire trade metric of interest.

  output$full_chart <- renderPlotly({

    # Filter the trade data

    trade %>%
      filter(country == input$country) %>%
      filter(flow == input$flow) %>%
      filter(category == tolower(input$category)) %>%
      plot_ly(
        x = ~year,
        y = ~trade_usd,
        type = "scatter",
        mode = "lines",
        color = ~commodity
        )
  })
  
  # Plot the projections. 
  
  output$projected_chart <- renderPlotly({
    projections <- projections()
    
    plot_ly(
      data = projections,
      x = ~year,
      y = ~trade_usd,
      type = "scatter",
      mode = "lines",
      color = ~group
    )
  })
  
  # Projections table.
  
  output$projections_table <- renderDataTable({
    
    projections <- projections()
    
    datatable(projections)
  })
  
}

# Run the application

shinyApp(ui = ui, server = server)