library(folderfun)
library(readxl)
library("shiny")
library("shinycssloaders")
library("shinythemes")
folderfun::setff("In", "H:/Shared drives/GEA Project Workspace (USDA)/")

# n_res <- 50
# people  <- read_excel(ffIn("Survey Results and Comments/GEA_Digital_Inventory_Draft_stakeholder.xlsm"), "GEA_Digital_Inventory_Draft_0")
# resources  <- read_excel(ffIn("Survey Results and Comments/GEA_Digital_Inventory_Draft_stakeholder.xlsm"), "resourceinfobeg_1")
fluidPage(
  navbarPage("GeoHUB survey", theme = shinytheme("lumen"),

             tabPanel("Summary Overview", icon = icon("globe-americas"),
                      sidebarPanel(
                        textOutput("number_responses"),
                        textOutput("number_agencies"),
                        textOutput("number_resources"),

                      ), # side bar panel end
                      mainPanel(

                      ), # main panel end
              ), # tab panel end

             tabPanel("Datasets", icon = icon("database")),

             tabPanel("In-house Application", icon = icon("desktop")),

             tabPanel("Off-The_Shelf", icon = icon("chart-bar")),

  ) # end navbar page

) # end fluid page