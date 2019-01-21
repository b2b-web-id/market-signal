# StockIDs
sectoral <- "Consumer"
stockIDs <- c('INDF')

# Source
source("../source.R")

# Run the application
shinyApp(ui = ui, server = server)
