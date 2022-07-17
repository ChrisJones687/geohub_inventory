library(folderfun)
library(readxl)
library("shiny")
library("shinycssloaders")
library("shinythemes")
folderfun::setff("In", "H:/Shared drives/GEA Project Workspace (USDA)/")

n_res <- 50
# people  <- read_excel(ffIn("Survey Results and Comments/GEA_Digital_Inventory_Draft_stakeholder.xlsm"), "GEA_Digital_Inventory_Draft_0")
# resources  <- read_excel(ffIn("Survey Results and Comments/GEA_Digital_Inventory_Draft_stakeholder.xlsm"), "resourceinfobeg_1")

ui <- fluidPage(
navbarPage("GeoHUB survey", theme = shinytheme("lumen"),

  tabPanel("Summary Overview", icon = icon("globe-americas"),
           sidebarPanel(
             print(paste0("Total Responses: ", n_res)),
             print(paste0("\nUSDA Agencies Represented: ", agencies)),
             print(paste0("\nDigital Resources: ", n_res))

           )),

  tabPanel("Datasets", icon = icon("database")),

  tabPanel("In-house Application", icon = icon("desktop")),

  tabPanel("Off-The_Shelf", icon = icon("chart-bar")),

)
)

