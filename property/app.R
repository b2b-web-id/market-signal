# StockIDs
stockIDs <- c('WSKT','PTPP','WIKA',
              'SMRA','ASRI','CTRA')

# Source
source("../source.R")

# Run the application
shinyApp(ui = ui, server = server)
