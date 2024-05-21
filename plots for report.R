library(readxl)
library(ggplot2)
library("ggthemes")
library(extrafont)
library(RColorBrewer)
library(dplyr)

contact <- resources  <- read_excel("data/GEA_survey_contacts.xlsx", "Summary")

resources  <- read_excel("data/GEA_Digital_Inventory_Draft_stakeholder5.xlsm", "resourceinfobeg_1")

resources2 <- read_excel("data/GEA_Digital_Inventory.xlsm", "resourceinfobeg_1")
resources2$`Managing Agency or Business Center:`[resources2$`Managing Agency or Business Center:` == "RHS"] <- "RD"
# table(resources2$Survey)
# reps <-
#   data.frame(table(resources2$`Managing Agency or Business Center:`[resources2$`Managing Agency or Business Center:` != "RMA"]))


agencies <- length(unique(resources$`Managing Agency or Business Center:`))
n_res <- nrow(resources)
n_respon <- length(unique(resources$`Respondent Full Name`))
datasets2 <- read.csv("data/datasets_edit.csv")
inhouse <- read.csv("data/ihs_edit.csv")
ots <- read.csv("data/ots_edit.csv")

datasets2$data_type[datasets2$X == 106] <- "Climate"

respon_agen <- data.frame(table(resources2[,c('Respondent Full Name', 'Managing Agency or Business Center:')]))
respon_agen <- respon_agen[respon_agen$Freq > 0, ]
reps <-
  data.frame(table(respon_agen$`Managing.Agency.or.Business.Center.`[respon_agen$`Managing.Agency.or.Business.Center.` != "RMA"]))
reps <- reps[reps$Var1 != "RMA", ]

contact$respondents <- reps$Freq
contact$resp_rate <- contact$respondents / contact$`Sum of # of contacts` * 100
## write out supplemental tables
webapps <- inhouse[inhouse$app_type == 'webapp', c('Name', 'app_purpose', 'private_public', 'agency', 'ownership_status', 'Details')]

desktop <- inhouse[inhouse$app_type == 'desktop', c('Name', 'app_purpose', 'private_public', 'agency', 'ownership_status', 'Details')]
#
# write.csv(webapps, 'report_outputs/webapps.csv')
# write.csv(desktop, 'report_outputs/desktop.csv')

num_colors <- 11
spectralplus <- colorRampPalette(brewer.pal(11, "Spectral"))(num_colors)
names(contact)[2] <- "contacts"

contact <- contact %>%
  arrange(desc(Agency)) %>%
  mutate(prop = contacts / sum(contact$contacts) * 100) %>%
  mutate(ypos = cumsum(prop) - 0.5 * prop )

contact <- contact[contact$contacts > 4, ]


pie(contact$contacts, labels = contact$Agency, border = "white", col = spectralplus) +
  theme(axis.text.x = element_blank())

ggsave("report_outputs/contact_by_agency.jpeg")

pie(contact$respondents, labels = contact$Agency, border = "white", col = spectralplus)

# Basic piechart
ggplot(contact, aes(x = "", y = prop, fill = Agency)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "none") +
  geom_text(aes(y = ypos, label = Agency), color = "white", size = 6) +
  scale_fill_manual(values = spectralplus)


num_colors <- 14
spectralplus <- colorRampPalette(brewer.pal(11, "Spectral"))(num_colors)


ggplot(respon_agen, aes(x = `Managing.Agency.or.Business.Center.`)) +
  geom_bar(aes(fill = `Managing.Agency.or.Business.Center.`)) +
  theme_classic() +
  labs(
       x = "Respondants per Agency",
       y = "Number of Respondents",
       color = "Survey") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 0.05,
            size = 8.0) +
  scale_fill_manual(values = spectralplus) +
  guides(fill = guide_legend(title = "Agency"))

ggsave("report_outputs/respondants_agency.jpeg")


ggplot(resources2, aes(x = `Managing Agency or Business Center:`)) +
  geom_bar(aes(fill = `Survey`)) +
  theme_classic() +
  labs(title = "Digital Assets by type",
       x = "Digital Asset Type",
       y = "Number of Resources",
       color = "Survey") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 1.5,
            size = 8.0) +
  scale_fill_brewer(palette = "Dark2")

ggplot(resources2, aes(x = `Which of the following best describes the digital resource?`)) +
  geom_bar(aes(fill = `Survey`)) +
  theme_classic() +
  labs(x = "Digital Asset Type",
       y = "Number of Resources",
       color = "Survey") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 1.5,
            size = 8.0) +
  scale_fill_brewer(palette = "Dark2")

ggsave("report_outputs/asset-type.jpeg")

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

ggsave("report_outputs/storage.jpeg")


ggplot(datasets2, aes(x = storage_location)) +
  geom_bar(aes(fill = format)) +
  theme_classic() +
  labs(x = "Storage Location",
       y = "Number of Datasets",
       fill = "Data Format") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 0,
            size = 8.0) +
  scale_fill_brewer(palette = "PuOr")

ggsave("report_outputs/storage_format.jpeg")

ggplot(datasets2, aes(x = storage_location)) +
  geom_bar(aes(fill = size)) +
  theme_classic() +
  labs(x = "Storage Location",
       y = "Number of Datasets",
       fill = "Data Size") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 0,
            size = 8.0) +
  scale_fill_brewer(palette = "PuOr")

ggsave("report_outputs/storage_size.jpeg")

datasets2$data_type[datasets2$data_type == ''] <- NA
datasets2$data_type[datasets2$data_type == 'Animal'] <- NA
datasets2$data_type[datasets2$data_type == 'Soil Water'] <- NA
datasets2$data_type[datasets2$data_type == 'Yield'] <- 'Crop'

datasets3 <- datasets2[!is.na(datasets2$data_type), ]
datasets3$data_type[datasets3$data_type == 'Aerial Photo'] <- NA
datasets3$data_type[datasets3$data_type == 'Publications'] <- NA
datasets3$data_type[datasets3$data_type == 'Timber'] <- NA
datasets3$data_type[datasets3$data_type == 'Fungal'] <- NA
datasets3$data_type[datasets3$data_type == 'LU/LC'] <- NA
datasets3$data_type[datasets3$data_type == 'Maps'] <- NA
datasets3$data_type[datasets3$data_type == 'Conservation'] <- NA
datasets3$data_type[datasets3$data_type == 'Nutrients'] <- NA
datasets3$data_type[datasets3$data_type == 'Pollinators'] <- NA
datasets3$data_type[datasets3$data_type == 'Photographs'] <- NA
datasets3$data_type[datasets3$data_type == 'Large Data Repo'] <- NA

datasets3 <- datasets3[!is.na(datasets3$data_type), ]


ggplot(datasets3, aes(x = data_type)) +
  geom_bar(aes(fill = data_type)) +
  theme_classic() +
  labs(x = "Dataset Type",
       y = "Number of Datasets",
       fill = "data_type") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 0,
            size = 8.0) +
  scale_fill_brewer(palette = "Spectral")

ggsave("report_outputs/data_type.jpeg")

ggplot(datasets3, aes(x = data_type)) +
  geom_bar(aes(fill = storage_location)) +
  theme_classic() +
  labs(x = "Dataset Type",
       y = "Number of Datasets",
       fill = "data_type") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 0,
            size = 8.0) +
  scale_fill_brewer(palette = "PuOr")

ggsave("report_outputs/data_type_storage.jpeg")


ggplot(datasets2, aes(x = format)) +
  geom_bar(aes(fill = format)) +
  theme_classic() +
  labs(title = "Data Format",
       x = "Data Format",
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

ggsave("report_outputs/format.jpeg")


ggplot(datasets2, aes(x = ownership_status)) +
  geom_bar(aes(fill = ownership_status)) +
  theme_classic() +
  labs(x = "Ownership/license status",
       y = "Number of Datasets",
       fill = "Ownership/license status") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 0.5,
            size = 8.0) +
  scale_fill_brewer(palette = "PuOr")

ggsave("report_outputs/ownership.jpeg")


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


ggsave("report_outputs/size.jpeg")

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

ggsave("report_outputs/continuous.jpeg")


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

ggsave("report_outputs/imagery.jpeg")


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

ggsave("report_outputs/observational.jpeg")


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

ggsave("report_outputs/privacy.jpeg")

q <- data.frame(table(datasets2$collection_method))
## Inhouse applications

ggplot(inhouse, aes(x = app_type)) +
  geom_bar(aes(fill = app_type)) +
  theme_classic() +
  labs(x = "Application type",
       y = "Number of Applications",
       fill = "Ownership/license status") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 1.0,
            size = 8.0) +
  scale_fill_brewer(palette = "Purples")



ggplot(inhouse, aes(x = app_type)) +
  geom_bar(aes(fill = ownership_status)) +
  theme_classic() +
  labs(x = "Application type",
       y = "Number of Applications",
       fill = "Application Purpose") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 1.0,
            size = 8.0) +
  scale_fill_brewer(palette = "PuOr")


inhouse_nona <- inhouse[!is.na(inhouse$app_purpose), ]

ggplot(inhouse_nona, aes(x = app_type)) +
  geom_bar(aes(fill = app_purpose)) +
  theme_classic() +
  labs(x = "Application type",
       y = "Number of Applications",
       fill = "Application Purpose") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 0,
            size = 8.0) +
  scale_fill_brewer(palette = "PuOr")


ggsave("report_outputs/app_type_purpose.jpeg")


ggplot(inhouse, aes(x = ownership_status)) +
  geom_bar(aes(fill = ownership_status)) +
  theme_classic() +
  labs(x = "Ownership/license status",
       y = "Number of Applications",
       fill = "Ownership/license status") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 0.0,
            size = 8.0) +
  scale_fill_brewer(palette = "PuOr")

ggsave("report_outputs/app_ownership.jpeg")

ggplot(inhouse, aes(x = private_public)) +
  geom_bar(aes(fill = private_public)) +
  theme_classic() +
  labs(x = "Privacy Status",
       y = "Number of Applications",
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


ggsave("report_outputs/app_privacy.jpeg")


ggplot(inhouse, aes(x = private_public)) +
  geom_bar(aes(fill = ownership_status)) +
  theme_classic() +
  labs(x = "Privacy Status",
       y = "Number of Applications",
       fill = "Ownership/license status") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 1.5,
            size = 8.0) +
  scale_fill_brewer(palette = "PuOr")



ggplot(inhouse_nona, aes(x = app_purpose)) +
  geom_bar(aes(fill = app_purpose)) +
  theme_classic() +
  labs(x = "Application Purpose",
       y = "Number of Datasets",
       fill = "Application Purpose") +
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

ggsave("report_outputs/app_purpose.jpeg")

ggplot(inhouse, aes(x = app_purpose)) +
  geom_bar(aes(fill = ownership_status)) +
  theme_classic() +
  labs(x = "Application Purpose",
       y = "Number of Datasets",
       fill = "Application Purpose") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 1.5,
            size = 8.0) +
  scale_fill_brewer(palette = "PuOr")

models <- inhouse[inhouse$app_purpose == "Model", ]

nas <- models[is.na(models$app_purpose), ]
models <- models[!is.na(models$app_purpose), ]
