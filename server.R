library(folderfun)
library(readxl)
# folderfun::setff("In", "H:/Shared drives/GEA Project Workspace (USDA)/")

# people  <- read_excel(ffIn("Survey Results and Comments/GEA_Digital_Inventory_Draft_stakeholder.xlsm"), "GEA_Digital_Inventory_Draft_0")
# resources  <- read_excel(ffIn("Survey Results and Comments/GEA_Digital_Inventory_Draft_stakeholder.xlsm"), "resourceinfobeg_1")


function(input, output, session) {
  output$number_responses <- renderText({
    paste0("Total Responses: ", n_respon)
  })
  output$number_resources <- renderText({
    paste0("Total Reources: ", n_res)
  })
  output$number_agencies <- renderText({
    paste0("Agencies Reporting: ", agencies)
  })

  } # server closing bracket