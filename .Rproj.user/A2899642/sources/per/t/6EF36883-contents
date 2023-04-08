library(readr)
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(lubridate)
library(agricolae)


function(input, output) {
  custData <- read_csv("data.csv")
  
  glimpse(custData)
  
  custData <- na.omit(custData)
  dim(custData)
  
  # separate date and time components of invoice date
  custData$date <-
    sapply(
      custData$InvoiceDate,
      FUN = function(x) {
        strsplit(x, split = '[ ]')[[1]][1]
      }
    )
  custData$time <-
    sapply(
      custData$InvoiceDate,
      FUN = function(x) {
        strsplit(x, split = '[ ]')[[1]][2]
      }
    )
  
  # create month, year and hour of day variables
  custData$month <-
    sapply(
      custData$date,
      FUN = function(x) {
        strsplit(x, split = '[/]')[[1]][1]
      }
    )
  custData$year <-
    sapply(
      custData$date,
      FUN = function(x) {
        strsplit(x, split = '[/]')[[1]][3]
      }
    )
  custData$hourOfDay <-
    sapply(
      custData$time,
      FUN = function(x) {
        strsplit(x, split = '[:]')[[1]][1]
      }
    )
  
  custData$date <- as.Date(custData$date, "%m/%d/%Y")
  
  custData$dayOfWeek <- wday(custData$date, label = TRUE)
  
  custData <- custData %>% mutate(lineTotal = Quantity * UnitPrice)
  
  custData$Country <- as.factor(custData$Country)
  custData$month <- as.factor(custData$month)
  custData$year <- as.factor(custData$year)
  levels(custData$year) <- c(2010, 2011)
  custData$hourOfDay <- as.factor(custData$hourOfDay)
  custData$dayOfWeek <- as.factor(custData$dayOfWeek)
  
  
  #plot 1
  output$revenue_plot <- renderPlot({
    custData %>%
      group_by(date) %>%
      summarise(revenue = sum(lineTotal)) %>%
      ggplot(aes(x = date, y = revenue)) +
      geom_line() +
      geom_smooth(method = 'auto', se = FALSE) +
      labs(x = 'Date', y = 'Revenue (£)')
  })
  
  #plot selected
  weekdaySummary <- custData %>%
    group_by(date, dayOfWeek) %>%
    summarise(revenue = sum(lineTotal),
              transactions = n_distinct(InvoiceNo)) %>%
    mutate(aveOrdVal = (round((
      revenue / transactions
    ), 2))) %>%
    ungroup()
  
  dayOfWeekPlot <- reactive({
    if (input$plotType_dayOfweek == "Revenue") {
      ggplot(weekdaySummary, aes(x = dayOfWeek, y = revenue)) + geom_boxplot() +
        labs(x = 'Day of the Week', y = 'Revenue', title = 'Revenue by Day of the Week')
    } else if (input$plotType_dayOfweek == "Number of Transactions") {
      ggplot(weekdaySummary, aes(x = dayOfWeek, y = transactions)) + geom_boxplot() +
        labs(x = 'Day of the Week', y = 'Number of Daily Transactions', title = 'Number of Transactions by Day of the Week')
    } else {
      ggplot(weekdaySummary, aes(x = dayOfWeek, y = aveOrdVal)) + geom_boxplot() +
        labs(x = 'Day of the Week', y = 'Average Order Value', title = 'Average Order Value by Day of the Week')
    }
  })
  
  output$dayOfWeek <- renderPlot({
    dayOfWeekPlot()
  })
  
  dayOfWeekDensity <- reactive({
    if (input$plotType_dayOfweek == "Revenue") {
      ggplot(weekdaySummary, aes(revenue, fill = dayOfWeek)) + geom_density(alpha = 0.2)
    } else if (input$plotType_dayOfweek == "Number of Transactions") {
      ggplot(weekdaySummary, aes(transactions, fill = dayOfWeek)) + geom_density(alpha = 0.2)
    } else {
      ggplot(weekdaySummary, aes(aveOrdVal, fill = dayOfWeek)) + geom_density(alpha = 0.2)
    }
  })
  
  output$dayOfWeekDensity <- renderPlot({
    dayOfWeekDensity()
  })
  
  kruskalUi <- reactive({
    if (input$plotType_dayOfweek == "Revenue") {
      kruskal(weekdaySummary$revenue,
              weekdaySummary$dayOfWeek,
              console = TRUE)
    } else if (input$plotType_dayOfweek == "Number of Transactions") {
      kruskal(weekdaySummary$transactions,
              weekdaySummary$dayOfWeek,
              console = TRUE)
    } else {
      kruskal(weekdaySummary$aveOrdVal,
              weekdaySummary$dayOfWeek,
              console = TRUE)
    }
  })
  output$kruskal_output <- renderPrint({
    kruskalUi()
  })
  
  
  #Hour of day analysis
  hourOfDay <- reactive({
    if (input$plotType_hourofday == "Revenue") {
      custData %>%
        group_by(hourOfDay) %>%
        summarise(revenue = sum(lineTotal)) %>%
        ggplot(aes(x = hourOfDay, y = revenue)) + geom_col() + labs(x = 'Hour Of Day', y = 'Revenue (£)', title = 'Revenue by Hour Of Day')
    } else {
      custData %>%
        group_by(hourOfDay) %>%
        summarise(transactions = n_distinct(InvoiceNo)) %>%
        ggplot(aes(x = hourOfDay, y = transactions)) + geom_col() + labs(x = 'Hour Of Day', y = 'Number of Transactions', title = 'Transactions by Hour Of Day')
    }
  })
  output$hour_of_day <- renderPlot({
    hourOfDay()
  })
  
  # Country Summary
  countrySummary <- custData %>%
    group_by(Country) %>%
    summarise(
      revenue = sum(lineTotal),
      transactions = n_distinct(InvoiceNo),
      customers = n_distinct(CustomerID)
    ) %>%
    mutate(aveOrdVal = (round((
      revenue / transactions
    ), 2)), aveCustVal = (round((
      revenue / customers
    ), 2))) %>%
    ungroup() %>%
    arrange(desc(revenue))
  
  output$countrySummaryTable <- renderDT({
    print(countrySummary)
  })
  
  output$country_select <- renderUI({
    selectInput("country",
                "Select Country",
                choices = unique(countrySummary$Country))
  })
  
  output$table_country <- renderDT({
    selectCountries <- custData %>%
      filter(Country == input$country) %>%
      group_by(Country, date) %>%
      summarise(
        revenue = sum(lineTotal),
        transactions = n_distinct(InvoiceNo),
        customers = n_distinct(CustomerID)
      ) %>%
      mutate(aveOrdVal = (round((
        revenue / transactions
      ), 2))) %>%
      ungroup() %>%
      arrange(desc(revenue))
    
    print(selectCountries)
  })
  
  output$compare_country_1 <- renderUI({
    selectInput("compareSelect_1",
                "Select Country 1:",
                choices = unique(countrySummary$Country))
  })
  
  output$compare_country_2 <- renderUI({
    selectInput("compareSelect_2",
                "Select Country 2:",
                choices = unique(countrySummary$Country))
  })
  
  selectedCountries <- reactive({
    selectCountries <- custData %>%
      filter(Country == input$compareSelect_1 |
               Country == input$compareSelect_2) %>%
      group_by(Country, date) %>%
      summarise(
        revenue = sum(lineTotal),
        transactions = n_distinct(InvoiceNo),
        customers = n_distinct(CustomerID)
      ) %>%
      mutate(aveOrdVal = round(revenue / transactions, 2)) %>%
      ungroup() %>%
      arrange(desc(revenue))
    
    selectCountries
  })
  
  output$compare_plot_country_1 <- renderPlot({
    ggplot(selectedCountries(),
           aes(x = date, y = revenue, colour = Country)) +
      geom_smooth(method = 'auto', se = FALSE) +
      labs(x = ' Country', y = 'Revenue (£)', title = 'Revenue by Country over Time')
  })
  
  output$compare_plot_country_2 <- renderPlot({
    ggplot(selectedCountries(), aes(x = Country, y = revenue)) +
      geom_col() +
      labs(x = ' Country', y = 'Revenue (£)', title = 'Revenue by Country')
  })
  
  custSummary <- custData %>%
    group_by(CustomerID) %>%
    summarise(revenue = sum(lineTotal), transactions = n_distinct(InvoiceNo)) %>%
    mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
    ungroup() %>%
    arrange(desc(revenue))
  
  
  output$table_customer <- renderDT({
    print(custSummary)
  })
  
  
}
