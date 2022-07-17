library("shiny")
library("shinycssloaders")
library("shinythemes")
library(folderfun)
library(readxl)
library("ggthemes")
library(extrafont)

# font_import()
fonts()
folderfun::setff("In", "H:/Shared drives/GEA Project Workspace (USDA)/")
resources  <- read_excel(ffIn("Survey Results and Comments/GEA_Digital_Inventory_Draft_stakeholder5.xlsm"), "resourceinfobeg_1")
agencies <- length(unique(resources$`Managing Agency or Business Center:`))
n_res <- nrow(resources)
n_respon <- length(unique(resources$`Respondent Full Name`))
res_names_f <- names(resources)
c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 18,  27, 28, 29, 30, 31)
datasets <- resources[resources$`Which of the following best describes the digital resource?` == "dataset", c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 18,  27, 28, 29, 30, 31)]
ots <-  resources[resources$`Which of the following best describes the digital resource?` == "ots", c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 18,  27, 28, 29, 30, 31)]
in_house <-  resources[resources$`Which of the following best describes the digital resource?` == "inhouse", c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 18,  27, 28, 29, 30, 31)]


u_datasets <- data.frame(table(datasets[,1]))
u_ots <- data.frame(table(ots[,1]))
u_inhouse <- data.frame(table(in_house[,1]))

datasets$name <- datasets$`Name of Resource:`

dataset_names <- read.csv("lookup_tables/datasets_name.csv")

s <- data.frame(table(datasets$name))

for (i in seq_len(nrow(dataset_names))) {
  datasets$name[datasets$name %in% dataset_names$Var1[i]] <- dataset_names$name[i]
}



t <- data.frame(table(datasets$name))

myColors = c("#A6611A", "#DFC27D", "#6e6c6b", "#80CDC1", "#018571")

ui <- fluidPage(
  navbarPage("GeoHUB survey", theme = shinytheme("lumen"),

             tabPanel("Summary Overview", icon = icon("globe-americas"),
                      sidebarPanel(
                        textOutput("number_res"),

                        # cat(paste("Total Responses: ", n_respon,
                        # "\nUSDA Agencies Represented: ", agencies,
                        # "\nDigital Resources: ", n_res), sep = " ")
                      )),

             tabPanel("Datasets", icon = icon("database")),

             tabPanel("In-house Application", icon = icon("desktop")),

             tabPanel("Off-The_Shelf", icon = icon("chart-bar")),

  ) # end navbar page

) # end fluid page


server <- function(input, output, session) {
  output$number_res <- renderText({
    paste0("Total Responses: ", n_respon)
  })
} # end server function


shinyApp(ui = ui, server = server)