library(readr)
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(lubridate)
library(agricolae)

function(input, output) {
  custData <- read_csv("data.csv")
  
  custData <- na.omit(custData)
  
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
  
  # data csv
  output$dataCsv <- renderDT({
    custData_selected <- custData %>%
      select(InvoiceNo, Description, Quantity, UnitPrice, CustomerID, Country, date, time, month, year, dayOfWeek)
    print(custData_selected)
  })
  
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
  output$revenue_month <- renderPlot({
    custData %>%
      group_by(year, month) %>%
      summarise(revenue = sum(lineTotal)) %>%
      ggplot(aes(x = month, y = revenue, fill = year)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Month", y = "Revenue (£)", fill = "Year") +
      theme(legend.position = "bottom")
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
  
  topCountries <- countrySummary %>%
    slice(1:7)
  
  
  output$topCountry_revenue <- renderPlot({
    ggplot(topCountries, aes(x = Country, y = revenue)) +
      geom_col() +
      labs(x = 'Country', y = 'Revenue (£)', title = 'Top 7 Countries by Revenue')
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
  
  
  #Customer Segmentation
  custSummary <- custData %>%
    group_by(CustomerID) %>%
    summarise(revenue = sum(lineTotal),
              transactions = n_distinct(InvoiceNo)) %>%
    mutate(aveOrdVal = (round((
      revenue / transactions
    ), 2))) %>%
    ungroup() %>%
    arrange(desc(revenue))
  
  custSummaryB <- custData %>%
    group_by(CustomerID, InvoiceNo) %>%
    summarise(revenue = sum(lineTotal),
              transactions = n_distinct(InvoiceNo)) %>%
    mutate(aveOrdVal = (round((
      revenue / transactions
    ), 2))) %>%
    ungroup() %>%
    arrange(revenue) %>%
    mutate(cumsum = cumsum(revenue))
  
  custSummaryB <- custData %>%
    group_by(InvoiceNo,
             CustomerID,
             Country,
             date,
             month,
             year,
             hourOfDay,
             dayOfWeek) %>%
    summarise(orderVal = sum(lineTotal)) %>%
    mutate(recent = Sys.Date() - date) %>%
    ungroup()
  
  custSummaryB$recent <- as.character(custSummaryB$recent)
  custSummaryB$recentDays <-
    sapply(
      custSummaryB$recent,
      FUN = function(x) {
        strsplit(x, split = '[ ]')[[1]][1]
      }
    )
  custSummaryB$recentDays <- as.integer(custSummaryB$recentDays)
  
  customerBreakdown <- custSummaryB %>%
    group_by(CustomerID, Country) %>%
    summarise(
      orders = n_distinct(InvoiceNo),
      revenue = sum(orderVal),
      meanRevenue = round(mean(orderVal), 2),
      medianRevenue = median(orderVal),
      mostDay = names(which.max(table(dayOfWeek))),
      mostHour = names(which.max(table(hourOfDay))),
      recency = min(recentDays)
    ) %>%
    ungroup()
  
  custBreakSum <- customerBreakdown %>%
    filter(orders > 5, revenue > 100)
  
  output$tableCustomer_plot <- renderDT({
    print(custBreakSum)
  })
  
  
  output$heatmapCust <- renderPlot({
    custMat <- custBreakSum %>%
      select(recency, revenue, meanRevenue, medianRevenue, orders) %>%
      as.matrix()
    
    rownames(custMat) <- custBreakSum$CustomerID
    options(repr.plot.width = 7, repr.plot.height = 6)
    heatmap(scale(custMat), cexCol = 0.7)
  })
  
  output$table_customer <- renderDT({
    print(custSummary)
  })
  
  
  histogramPlot <- reactive({
    if (input$historam_plot == "Revenue") {
      ggplot(custSummary, aes(revenue)) + geom_histogram() + scale_x_log10() + labs(x = 'Revenue', y = 'Count of Customers', title = 'Histogram of Revenue per customer (Log Scale)')
    } else {
      ggplot(custSummary, aes(transactions)) + geom_histogram() + scale_x_log10() + labs(x = 'Number of Transactions', y = 'Count of Customers', title = 'Histogram of Transactions per customer')
    }
  })
  
  output$histogram_customer <- renderPlot({
    histogramPlot()
  })
  
}
