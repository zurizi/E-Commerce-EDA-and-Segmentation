library(shinydashboard)

# Load UI
source("ui.R")

# Load server
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)
