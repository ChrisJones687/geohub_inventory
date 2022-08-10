library(folderfun)
library(readxl)
library(ggplot2)
library("shiny")
library("shinycssloaders")
library("shinythemes")
library(folderfun)
library(readxl)
library("ggthemes")
library(extrafont)

resources  <- read_excel("data/GEA_Digital_Inventory_Draft_stakeholder5.xlsm", "resourceinfobeg_1")
agencies <- length(unique(resources$`Managing Agency or Business Center:`))
n_res <- nrow(resources)
n_respon <- length(unique(resources$`Respondent Full Name`))
datasets2 <- read.csv("data/datasets_edit.csv")
inhouse <- read.csv("data/ihs_edit.csv")
ots <- read.csv("data/ots_edit.csv")

function(input, output, session) {
  output$number_responses <- renderText({
    paste0("Total Responses: ", n_respon)
  })
  output$number_agencies <- renderText({
    paste0("Number of agencies: ", agencies)
  })
  output$number_resources <- renderText({
    paste0("Total Resources: ", n_res)
  })

  # Plots for Overview Page
  output$Asset_type <- renderPlot({
    ggplot(resources, aes(x = `Which of the following best describes the digital resource?`)) +
      geom_bar(aes(fill = `Which of the following best describes the digital resource?`)) +
      theme_classic() +
      labs(title = "Digital Assets by type",
           x = "Digital Asset Type",
           y = "Number of Resources",
           fill = "Resource Type") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            text = element_text(family = "Rubik"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      geom_text(aes(label = ..count..),
                stat = "count",
                vjust = 1.5,
                size = 8.0) +
      scale_fill_brewer(palette = "Dark2")
  })


  # Plots for data set page

  storage <- reactive({
    ggplot(datasets2, aes(x = storage_location)) +
      geom_bar(aes(fill = storage_location)) +
      theme_classic() +
      labs(title = "Data Storage Location",
           x = "Storage Location",
           y = "Number of Datasets",
           fill = "Storage Location") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            text = element_text(family = "Rubik"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      geom_text(aes(label = ..count..),
                stat = "count",
                vjust = 1.5,
                size = 8.0) +
      scale_fill_brewer(palette = "PuOr")
  })

  format <- reactive({
    ggplot(datasets2, aes(x = format)) +
      geom_bar(aes(fill = format)) +
      theme_classic() +
      labs(title = "Is the data collected observational",
           x = "Observational",
           y = "Number of Datasets",
           fill = "Ownership/license status") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            text = element_text(family = "Rubik"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      geom_text(aes(label = ..count..),
                stat = "count",
                vjust = 1.5,
                size = 8.0) +
      scale_fill_brewer(palette = "PuOr")
  })

  ownership <- reactive({
    ggplot(datasets2, aes(x = ownership_status)) +
      geom_bar(aes(fill = ownership_status)) +
      theme_classic() +
      labs(title = "Data Ownership status",
           x = "Ownership/license status",
           y = "Number of Datasets",
           fill = "Ownership/license status") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            text = element_text(family = "Rubik"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      geom_text(aes(label = ..count..),
                stat = "count",
                vjust = 1.5,
                size = 8.0) +
      scale_fill_brewer(palette = "PuOr")
  })

  size <- reactive({
    ggplot(datasets2, aes(x = size)) +
      geom_bar(aes(fill = size)) +
      theme_classic() +
      labs(title = "Data Size",
           x = "Data Size",
           y = "Number of Datasets",
           fill = "Ownership/license status") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            text = element_text(family = "Rubik"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      geom_text(aes(label = ..count..),
                stat = "count",
                vjust = 1.5,
                size = 8.0) +
      scale_fill_brewer(palette = "PuOr")
  })

  continuous <- reactive({
    ggplot(datasets2, aes(x = Continuous)) +
      geom_bar(aes(fill = Continuous)) +
      theme_classic() +
      labs(title = "Is the data continuously collected?",
           x = "Continuous",
           y = "Number of Datasets",
           fill = "Ownership/license status") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            text = element_text(family = "Rubik"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      geom_text(aes(label = ..count..),
                stat = "count",
                vjust = 1.5,
                size = 8.0) +
      scale_fill_brewer(palette = "Dark2")
  })

  imagery <- reactive({
    ggplot(datasets2, aes(x = Imagery)) +
      geom_bar(aes(fill = Imagery)) +
      theme_classic() +
      labs(title = "Is the data collected imagery",
           x = "Imagery",
           y = "Number of Datasets",
           fill = "Ownership/license status") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            text = element_text(family = "Rubik"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      geom_text(aes(label = ..count..),
                stat = "count",
                vjust = 1.5,
                size = 8.0) +
      scale_fill_brewer(palette = "Dark2")
  })

  observational <- reactive({
    ggplot(datasets2, aes(x = Observational)) +
      geom_bar(aes(fill = Observational)) +
      theme_classic() +
      labs(title = "Is the data collected observational",
           x = "Observational",
           y = "Number of Datasets",
           fill = "Ownership/license status") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            text = element_text(family = "Rubik"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      geom_text(aes(label = ..count..),
                stat = "count",
                vjust = 1.5,
                size = 8.0) +
      scale_fill_brewer(palette = "Dark2")
  })

  public <- reactive({
    ggplot(datasets2, aes(x = private_public)) +
      geom_bar(aes(fill = private_public)) +
      theme_classic() +
      labs(title = "Dataset Privacy Status",
           x = "Privacy Status",
           y = "Number of Datasets",
           fill = "Ownership/license status") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            text = element_text(family = "Rubik"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      geom_text(aes(label = ..count..),
                stat = "count",
                vjust = 1.5,
                size = 8.0) +
      scale_fill_brewer(palette = "Dark2")
  })

  type <- reactive({
    ggplot(datasets2, aes(x = data_type), na.rm = TRUE) +
      geom_bar(aes(fill = data_type), na.rm = TRUE) +
      theme_classic() +
      labs(title = "Is the data collected observational",
           x = "Observational",
           y = "Number of Datasets",
           fill = "Ownership/license status") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            text = element_text(family = "Rubik"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      geom_text(aes(label = ..count..),
                stat = "count",
                vjust = 1.5,
                size = 8.0)
  })
  # Return the requested graph
  graphInput <- reactive({
    switch(input$question_id,
           "Storage Location" = storage(),
           "Data Format" = format(),
           "Ownership Status" = ownership(),
           "Is the data continuously collected?" = continuous(),
           "Is the data imagery?" = imagery(),
           "Is the data observational?" = observational(),
           "Data Size" = size(),
           "Is the data Public or Private" = public(),
           "Data Type" = type(),
    )
  })

  output$data_graph <- renderPlot({
    graphInput()
  })

  # Plots for IHS page
  ihs_type <- reactive({
    ggplot(inhouse, aes(x = app_type)) +
      geom_bar(aes(fill = app_type)) +
      theme_classic() +
      labs(title = "Is the data collected observational",
           x = "Observational",
           y = "Number of Datasets",
           fill = "Ownership/license status") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            text = element_text(family = "Rubik"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      geom_text(aes(label = ..count..),
                stat = "count",
                vjust = 1.5,
                size = 8.0) +
      scale_fill_brewer(palette = "Purples")
  })

  ihs_ownership <- reactive({
    ggplot(inhouse, aes(x = ownership_status)) +
      geom_bar(aes(fill = ownership_status)) +
      theme_classic() +
      labs(title = "App Ownership status",
           x = "Ownership/license status",
           y = "Number of Datasets",
           fill = "Ownership/license status") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            text = element_text(family = "Rubik"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      geom_text(aes(label = ..count..),
                stat = "count",
                vjust = 1.5,
                size = 8.0) +
      scale_fill_brewer(palette = "PuOr")
  })

  ihs_public <- reactive({
    ggplot(inhouse, aes(x = private_public)) +
      geom_bar(aes(fill = private_public)) +
      theme_classic() +
      labs(title = "App Privacy Status",
           x = "Privacy Status",
           y = "Number of Datasets",
           fill = "Ownership/license status") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            text = element_text(family = "Rubik"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      geom_text(aes(label = ..count..),
                stat = "count",
                vjust = 1.5,
                size = 8.0) +
      scale_fill_brewer(palette = "Dark2")
  })

  ihs_purpose <- reactive({
    ggplot(inhouse, aes(x = app_purpose)) +
      geom_bar(aes(fill = app_purpose)) +
      theme_classic() +
      labs(title = "Is the data collected observational",
           x = "Observational",
           y = "Number of Datasets",
           fill = "Ownership/license status") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            text = element_text(family = "Rubik"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      geom_text(aes(label = ..count..),
                stat = "count",
                vjust = 1.5,
                size = 8.0) +
      scale_fill_brewer(palette = "Purples")
  })

  appInput <- reactive({
    switch(input$ihs_id,
           "Ownership Status" = ihs_ownership(),
           "App Type" = ihs_type(),
           "Is the App Public or Private" = ihs_public(),
           "App Purpose" = ihs_purpose(),
    )
  })

  output$app_graph <- renderPlot({
    appInput()
  })

  # Plots for OTS page
  ots_type <- reactive({
    ggplot(ots, aes(x = app_type)) +
      geom_bar(aes(fill = app_type)) +
      theme_classic() +
      labs(title = "Is the data collected observational",
           x = "Observational",
           y = "Number of Datasets",
           fill = "Ownership/license status") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            text = element_text(family = "Rubik"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      geom_text(aes(label = ..count..),
                stat = "count",
                vjust = 1.5,
                size = 8.0) +
      scale_fill_brewer(palette = "Purples")
  })

  ots_ownership <- reactive({
    ggplot(ots, aes(x = ownership_status)) +
      geom_bar(aes(fill = ownership_status)) +
      theme_classic() +
      labs(title = "App Ownership status",
           x = "Ownership/license status",
           y = "Number of Datasets",
           fill = "Ownership/license status") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            text = element_text(family = "Rubik"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      geom_text(aes(label = ..count..),
                stat = "count",
                vjust = 1.5,
                size = 8.0) +
      scale_fill_brewer(palette = "PuOr")
  })

  ots_public <- reactive({
    ggplot(ots, aes(x = private_public)) +
      geom_bar(aes(fill = private_public)) +
      theme_classic() +
      labs(title = "App Privacy Status",
           x = "Privacy Status",
           y = "Number of Datasets",
           fill = "Ownership/license status") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            text = element_text(family = "Rubik"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      geom_text(aes(label = ..count..),
                stat = "count",
                vjust = 1.5,
                size = 8.0) +
      scale_fill_brewer(palette = "Dark2")
  })

  otsInput <- reactive({
    switch(input$ots_id,
           "Ownership Status" = ots_ownership(),
           "App Type" = ots_type(),
           "Is the App Public or Private" = ots_public(),
    )
  })

  output$ots_graph <- renderPlot({
    otsInput()
  })

} # server closing bracket