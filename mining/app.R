# StockIDs
stockIDs <- c('INCO','PTBA','ADRO')

# Source
source("../source.R")

# Run the application
shinyApp(ui = ui, server = server)
