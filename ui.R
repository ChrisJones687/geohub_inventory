library(folderfun)
library(readxl)
library("shiny")
library("shinycssloaders")
library("shinythemes")

dataset_cols <- c("Ownership Status", "Storage Location", "Data Format", "Is the data continuously collected?",
                  "Is the data imagery?", "Is the data observational?", "Data Size", "Is the data Public or Private",
                  "Data Type")

inhouse_cols <- c("Ownership Status", "Is the App Public or Private", "App Type", "App Purpose")

ots_cols <- c("Ownership Status", "Is the App Public or Private", "App Type")

ui <- fluidPage(
  navbarPage("GeoHUB survey", theme = shinytheme("lumen"),

             tabPanel("Summary Overview", icon = icon("globe-americas"),
                      sidebarPanel(
                        textOutput("number_responses"),
                        textOutput("number_agencies"),
                        textOutput("number_resources"),

                        # selectInput("inSelect", "Select input",
                        #             c("Item A", "Item B", "Item C")),
                      ), # end side bar panel
                      plotOutput("Asset_type")
                      ), # end tab panel

             tabPanel("Datasets", icon = icon("database"),
                      selectInput("question_id", "Select response type",
                                  dataset_cols),
                      plotOutput("data_graph")
                      ), # end tab panel

             tabPanel("In-house Application", icon = icon("desktop"),
                      selectInput("ihs_id", "Select response type",
                                  inhouse_cols),
                      plotOutput("app_graph")
                      ), # end tab panel

             tabPanel("Off-The_Shelf", icon = icon("chart-bar"),
                      selectInput("ots_id", "Select response type",
                                  ots_cols),
                      plotOutput("ots_graph")
                      ), # end tab panel

  ) # end navbar page

) # end fluid page