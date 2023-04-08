library(shinydashboard)
library(DT)

dashboardPage(
  dashboardHeader(title = "Kelompok 8"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      menuItem(
        "Day Of week analysis",
        tabName = "dayOfWeek",
        icon = icon("calendar")
      ),
      menuItem(
        "Hour of day analysis",
        tabName = "hourOfDay",
        icon = icon("clock")
      ),
      menuItem(
        "Country Summary",
        tabName = "countrySummary",
        icon = icon("globe")
      ),
      menuItem(
        "Customer Segmentation",
        tabName = "customer",
        icon = icon("users")
      )
      
    )
  ),
  dashboardBody(# Boxes need to be put in a row (or column)
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(column(
                width = 6,
                box(
                  title = "Revenue by Date",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("revenue_plot", height = 450)
                )
              ))),
      # Second tab content
      tabItem(tabName = "dayOfWeek",
              fluidRow(
                box(
                  title = "Day of week analysis",
                  width = 12,
                  solidHeader = TRUE,
                  status = "danger",
                  column(width = 12,
                         fluidRow(column(
                           width = 6,
                           selectInput(
                             "plotType_dayOfweek",
                             "Choose plot type:",
                             choices = c("Revenue", "Number of Transactions", "Average Order Value"),
                             selected = "Revenue"
                           )
                         )),
                         fluidRow(
                           column(width = 6,
                                  plotOutput("dayOfWeek", height = 300)),
                           column(width = 6,
                                  plotOutput("dayOfWeekDensity", height = 300)),
                           column(width = 12,
                                  verbatimTextOutput("kruskal_output"))
                         ))
                )
              )),
      # 3 tab content
      tabItem(tabName = "hourOfDay",
              fluidRow(
                box(
                  title = "Hour of day",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  selectInput(
                    "plotType_hourofday",
                    "Plot by day of week:",
                    choices = c("Revenue", "Number of Transactions"),
                    selected = "Revenue"
                  ),
                  plotOutput("hour_of_day", height = 450)
                )
              )),
      
      # 4 tab content
      tabItem(tabName = "countrySummary",
              fluidRow(
                box(
                  title = "Country Summary",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  DTOutput("countrySummaryTable")
                )
              ),
              fluidRow(
                box(
                  title = "Country Summary per Day",
                  width = 12,
                  solidHeader = TRUE,
                  status = "success",
                  uiOutput("country_select"),
                  DTOutput("table_country")
                )
              ),
              fluidRow(
                box(
                  title = "Compare Country",
                  width = 12,
                  solidHeader = TRUE,
                  status = "success",
                  column(width = 12,
                         fluidRow(
                           column(width = 6,
                                  uiOutput("compare_country_1")),
                           column(width = 6,
                                  uiOutput("compare_country_2"))
                         ),
                         fluidRow(
                           column(
                             width = 12,
                             plotOutput("compare_plot_country_1", height = 350),
                             plotOutput("compare_plot_country_2", height = 350)
                           )
                         ))
                )
              )),
      # 5 tab content
      tabItem(tabName = "customer",
              fluidRow(column(
                width = 12,
                box(
                  title = "Customer Segmentation",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  DTOutput("table_customer")
                )
              )))
    ))
)