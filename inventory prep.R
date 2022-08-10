library(folderfun)
library(readxl)
library(ggplot2)
folderfun::setff("In", "H:/Shared drives/GEA Project Workspace (USDA)/")

# inv_people <- read.csv(ffIn("Survey Results and Comments/GEA_Digital_Inventory_06_01_22/GEA_Digital_Inventory_Draft_0.csv"))
# inv_resources <- read.csv(ffIn("Survey Results and Comments/GEA_Digital_Inventory_06_01_22/resourceinfobeg_2.csv"))

people  <- read_excel(ffIn("Survey Results and Comments/GEA_Digital_Inventory_Draft_stakeholder.xlsm"), "GEA_Digital_Inventory_Draft_0")
resources  <- read_excel(ffIn("Survey Results and Comments/GEA_Digital_Inventory_Draft_stakeholder.xlsm"), "resourceinfobeg_1")



library(ggplot2)
# counts
ggplot(resources, aes(x = `Which of the following best describes the digital resource?`)) +
  geom_bar()

ggplot(resources, aes(x = `Which of the following best describes the type of application?`)) +
  geom_bar()

ggplot(resources[resources$`Which of the following best describes the digital resource?` == "inhouse", ], aes(x = `Which of the following best describes the type of application?`)) +
  geom_bar()


ggplot(resources[resources$`Which of the following best describes the digital resource?` == "dataset", ], aes(x = `Where is the data stored?`)) +
  geom_bar()


ggplot(resources[resources$`Which of the following best describes the digital resource?` == "ots", ], aes(x = `Where is the data stored?`)) +
  geom_bar()

ggplot(resources[resources$`Which of the following best describes the digital resource?` == "inhouse", ], aes(x = `Where is the data stored?`)) +
  geom_bar()

t <- data.frame(table(resources[,3]))
name_fixer <- lapply(t$Var1, agrep, t$Var1, value  = TRUE)
name_fix <- list()
x = 1

for (i in seq_len(length(name_fixer))) {
  if (length(name_fixer[[i]]) > 1) {
    name_fix[[x]] <- name_fixer[[i]]
    print(i)
    x = x + 1
  }
}


sizes2 <- data.frame(table(datasets$`Which of the following best represents the method of data collection?`))
# write.csv(sizes, "lookup_tables/sizes.csv")
sizes <- read.csv("lookup_tables/sizes.csv")
datasets$continuous <- "No"
datasets$imagery <- "No"
datasets$observational <- "No"
for (i in seq_len(nrow(sizes))) {
  datasets$continuous[datasets$`Which of the following best represents the method of data collection?` %in% sizes$Var1[i]] <- sizes$Continous[i]
  datasets$imagery[datasets$`Which of the following best represents the method of data collection?` %in% sizes$Var1[i]] <- sizes$Imagery[i]
  datasets$observational[datasets$`Which of the following best represents the method of data collection?` %in% sizes$Var1[i]] <- sizes$Obersvational[i]

  }


ggplot(resources, aes(x = `Which of the following best describes the digital resource?`)) +
  geom_bar(aes(fill = `Managing Agency or Business Center:`)) +
  theme_classic() +
  labs(title = "Digital Assets by type",
       x = "Digital Asset Type",
       y = "Number of Resources",
       fill = "Resource Type") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5),
        # legend.position = "none"
        ) +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 1.5,
            size = 8.0) +
  scale_fill_brewer(palette = "Dark2")



resource_type <- data.frame(table(resources$`Which of the following best describes the digital resource?`))
ggplot(resource_type,
       aes(x = "",
           y = "Freq",
           fill = "Var1")) +
  # geom_bar(stat = "identity",
  #          width = 1) +
  # theme_classic() +
  # labs(title = "Digital Assets by type",
  #      fill = "Resource Type") +
  # theme_fivethirtyeight() +
  # theme(axis.title = element_text(),
  #       text = element_text(family = "Rubik"),
  #       plot.title = element_text(hjust = 0.5)) +
  # # geom_text(aes(label = ..count..),
  # #           stat = "count",
  # #           vjust = 1.5,
  # #           size = 8.0) +
  # scale_fill_brewer(palette = "Dark2") +
  geom_col() +
  coord_polar(theta = "y")

ggsave("outputs/asset-type.jpeg")

ggplot(datasets, aes(x = `Where is the data stored?`)) +
  geom_bar(aes(fill = `Where is the data stored?`)) +
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

ggsave("outputs/storage-location.jpeg")


ggplot(in_house, aes(x = `Which of the following best describes the type of application?`)) +
  geom_bar(aes(fill = `Which of the following best describes the type of application?`)) +
  theme_classic() +
  labs(title = "In House Application Type",
       x = "Application Type",
       y = "Number of Applications",
       fill = "Application Type") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 1.1,
            size = 7.0) +
  scale_fill_brewer(palette = "Dark2")

ggsave("outputs/application-type.jpeg")


ggplot(datasets, aes(x = `name`)) +
  geom_bar() +
  coord_polar("y", start = 0)


ggplot(datasets, aes(x = `Which of the following best describes the digital resource?`)) +
  geom_bar()

ggplot(datasets, aes(x = `Describe the resource or product in detail. If there is additional information you wish to provide, please include the URL of any relevant documents or websites:`)) +
  geom_bar()

ggplot(datasets, aes(x = `Which of the following best describes the license/ownership status of the resource?`)) +
  geom_bar(aes(fill = `Which of the following best describes the license/ownership status of the resource?`)) +
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
ggsave("outputs/ownership_status.jpeg")


ggplot(datasets, aes(x = `In what format is data stored?`)) +
  geom_bar(aes(fill = `In what format is data stored?`)) +
  theme_classic() +
  labs(title = "Data Types",
       x = "Data Type",
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
ggsave("outputs/data_format.jpeg")


ggplot(datasets, aes(x = `What is the approximate size of the dataset?`)) +
  geom_bar(aes(fill = `What is the approximate size of the dataset?`)) +
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
ggsave("outputs/data_size.jpeg")

ggplot(datasets, aes(x = `continuous`)) +
  geom_bar(aes(fill = `continuous`)) +
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
ggsave("outputs/data_continuous.jpeg")


ggplot(datasets, aes(x = `imagery`)) +
  geom_bar(aes(fill = `imagery`)) +
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
ggsave("outputs/data_imagery.jpeg")


ggplot(datasets, aes(x = `observational`)) +
  geom_bar(aes(fill = `observational`)) +
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
ggsave("outputs/data_observational.jpeg")





ggplot(in_house, aes(x = `Is this item kept private or made publicly accessible?`)) +
  geom_bar(aes(fill = `Is this item kept private or made publicly accessible?`)) +
  theme_classic() +
  labs(title = "In-House Application Privacy Status",
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
ggsave("outputs/in_house_privacy.jpeg")


ggplot(datasets, aes(x = `In what format is data stored?`)) +
  geom_bar(aes(fill = `In what format is data stored?`)) +
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

ggsave("outputs/dataset_privacy.jpeg")

ggplot(ots, aes(x = `Is this item kept private or made publicly accessible?`)) +
  geom_bar(aes(fill = `Is this item kept private or made publicly accessible?`)) +
  theme_classic() +
  labs(title = "OTS Application Privacy Status",
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
ggsave("outputs/ots_privacy.jpeg")

