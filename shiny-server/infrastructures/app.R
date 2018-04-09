# Sectoral
sectoral <- "Infrastructures"

# StockIDs
stockIDs <- c('TLKM')

# Source
source("../source.R")

# Run the application
shinyApp(ui = ui, server = server)
