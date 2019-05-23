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
      1,
      selectInput("country",
        h3("Country"),
        choices = unique(trade$country),
        selected = "Brazil"
      )
    ),

    # Select a trade category and flow (export/import).

    column(
      1,
      radioButtons("flow",
        h3("Flow type"),
        choices = c("Export", "Import"),
        selected = "Export"
      )
    ),

    # Select a trade category.

    column(
      3,
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
        selected = 2035
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
        h3("Annual magnification (%)"),
        value = 1
      )
    )
  ),

  # Display the plots.

  mainPanel(

    # Display a plot of the entire trend of the trade metric of interest.

    plotlyOutput("full_chart"),
    
    # Display a sentence about how the trade metric will change.
    
    textOutput("summary"),
    
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

      select(-flow, -quantity_name, -category, -country, -health_external, -iso3, -name, -quantity)

    # Eliminate duplicate years.
    
    recent <- dfm[!duplicated(dfm$year), ]
    
    ### Generate the predictions.
    # First generate the business as usual scenario as a linear estimation of the past five years.

    recent <- recent[1:5, 4:68] %>%
      mutate(year = c(2015:2011))

    # Choose a custom time span.

    span <- c(2020:input$year)
    
    ###
    # Make predictions based on a business as usual scenario.
    ###
    
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

    results <- results[, 2:66]
    
    ###
    # Do the same for predictions under climate change.
    ###
    
    cc <- c()
    results_climate <- tibble(rep(NA, length(2020:input$year)))
    
    for (var in names(recent)) {
      
      # Linearly predict the future values of all of the climate scores.
      
      m <- lm(get(var) ~ year, data = recent)
      
      cc <- c(cc, predict(m, newdata = tibble(year = span)))
      
      # Add the climate change distortion manually.
      
      if (as.character(var) == input$metric) {
        for (n in 1:length(cc)) {
         cc[n] <- cc[n] * (1 + .01 * input$degree)^n 
        }
      }
      
      prediction <- as_tibble(cc)
      colnames(prediction) <- var
      
      results_climate <- bind_cols(results_climate, prediction)
      cc <- c()
    }
    
    # Ignore the initiation column and the year column as above.
    
    results_climate <- results_climate[, 2:66]

    # Calculate the total value sold (among many commodity types) during a given year.
    
    total <- dfm %>% 
      group_by(year) %>% 
      summarise(trade_usd = sum(trade_usd))
      
    single <- dfm[!duplicated(dfm$year), ] %>% 
      select(-trade_usd, -commodity) %>% 
      left_join(total, by = "year") %>% 
      select(-year)
    
    # Fit the PLS model.

    mod_pls <- plsr(trade_usd ~ .,
      10,
      data = single,
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
    
    # Make predictions for the climate change data as well.
    
    predictions_climate <- predict(mod_pls, ncomp = n, newdata = results_climate) %>%
      as_tibble()
    
    colnames(predictions_climate) <- "trade_usd"    

    # Visualize the changes in trade for the predictions.
    # First combine all of the results into one dataframe.
    # Label the predictions.

    predictions <- predictions %>%
      bind_cols(tibble(value = span)) %>%
      mutate(year = value) %>%
      select(-value) %>%
      mutate(group = "Predicted (business as usual)")

    # Do the same for the climate change predictions.
    
    predictions_climate <- predictions_climate %>%
      bind_cols(tibble(value = span)) %>%
      mutate(year = value) %>%
      select(-value) %>%
      mutate(group = "Predicted (climate change)")
    
    # Make a new dataframe with the actual values used for the projections.

    # First capture the actual values.
    
    original <- dft_filtered() %>% 
      group_by(year) %>% 
      summarise(trade_usd = sum(trade_usd))
    
    actual <- tibble(
      trade_usd = original$trade_usd[1:28],
      year = 1989:2016,
      group = "Actual"
    )

    # Combine all three to plot.

    final <- bind_rows(actual, predictions, predictions_climate)
    
    # Adjust negative values to be zero.
    
    for (n in 1:nrow(final)) {
      if (final$trade_usd[n] < 0 & !is.na(final$trade_usd[n])) {
        final$trade_usd[n] <- 0
      }
    }
    
    # Return the final plottable values.
    
    final
  })

  ##########################
  # GRAPHS AND OTHER OUTPUTS
  ##########################

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
  
  # Summarize the effect of climate change.
  
  output$summary <- renderText({
    
    # Identify the magnitude of the difference.
    
    delta <- projections() %>% 
      group_by(group) %>% 
      summarise(total = sum(trade_usd))
    
    delta <- (delta$total[3] - delta$total[2]) / (as.numeric(input$year) - 2020)
    
    # Print the full summary sentence.
    
    paste0("A ", input$degree, " percent increase in the magnitude of this effect will be associated with an average ",
           ifelse(delta > 0, "increase", "decrease"), " of $", formatC(abs(delta), format = "f", digits = 0, big.mark = ","),
           " in these", ifelse(input$flow == "Export", " exports", " imports"), " per year until ", input$year, ".")
  })
  
  # Plot the projections. 
  
  output$projected_chart <- renderPlotly({

    plot_ly(
      data = projections(),
      x = ~year,
      y = ~trade_usd,
      type = "scatter",
      mode = "lines",
      color = ~group,
      linetype = ~group
    )
  })
  
  # Projections table.
  
  output$projections_table <- renderDataTable({
    
    datatable(projections())
  })
  
}

# Run the application

shinyApp(ui = ui, server = server)