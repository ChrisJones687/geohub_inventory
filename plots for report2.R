library(readxl)
library(ggplot2)
library("ggthemes")
library(extrafont)
library(RColorBrewer)
library(dplyr)
library(readr)

# Survey 2 personnel
personnel <- read.csv("data/Survey2/GEA_Digital_Inventory_2023_v5_0.csv")
contact <- personnel
contact$`Primary.Agency.or.Business.Center.`[contact$`Primary.Agency.or.Business.Center.` == "RHS"] <- "RD"
contact <- contact %>%
  rename(Agency = Primary.Agency.or.Business.Center.) %>%
  filter(Agency != "RMA") %>%
  group_by(Agency) %>%
  summarise('Sum of # of contacts' = n())

# resources  <- read_excel("data/GEA_Digital_Inventory_Draft_stakeholder5.xlsm", "resourceinfobeg_1")

# Survey 2 resources
resources <- read.csv("data/Survey2/resourceinfobeg_1.csv")
resources <- resources %>%
  rename(metadata_status = Does.the.resource.have.associated.metadata.,
         metadata_format = What.format.is.the.metadata.,
         metadata_standard_status = Is.the.metadata.designed.with.a.standard..e.g...ISO.19115..in.mind.,
         metadata_standard = Which.metadata.standard.does.this.resource.use.,
         metadata_standard_measure = In.your.estimation..how.closely.does.the.metadata.currently.meet.that.standard..Choose.a.value.between.0.10..10.meaning.the.metadata.most.closely.meets.the.standard.,
         storage_location = Where.is.the.dataset.stored.,
         format = In.what.format.is.the.dataset.stored.,
         size = What.is.the.approximate.size.of.the.dataset.,
         collection_method = Which.of.the.following.best.represents.the.method.of.data.collection.,
         ownership_status = Which.of.the.following.best.describes.the.license.ownership.status.of.the.resource.,
         private_public = Is.this.item.kept.private.or.made.publicly.accessible.,
         app_type = Which.of.the.following.best.describes.the.type.of.application.,
         digital_resource = Which.of.the.following.best.describes.this.digital.resource.) %>%
  mutate(observational = if_else(grepl("OBSERVATIONAL", toupper(collection_method)),TRUE,FALSE),
         imagery = if_else(grepl("IMAGERY", toupper(collection_method)),TRUE,FALSE),
         other = if_else(grepl("OTHER", toupper(collection_method)),TRUE,FALSE),
         continuous = if_else(grepl("CONTINUOUS", toupper(collection_method)),TRUE,FALSE))

resources$Managing.Agency.or.Business.Center.[resources$Managing.Agency.or.Business.Center. == "RHS"] <- "RD"
resources$metadata_standard_status[resources$metadata_standard_status == ""] <- NA
resources$metadata_standard[resources$metadata_standard == ""] <- NA
resources$metadata_standard[grepl("do not know",resources$metadata_standard)] <- NA
resources$metadata_standard[resources$metadata_standard == "EGMO Directive"] <- "ISO 19115"
resources$metadata_standard[grepl("19115",resources$metadata_standard)] <- "ISO 19115"
resources$metadata_standard[grepl("ISO",resources$metadata_standard)] <- "ISO 19115"

resources$metadata_format[grepl("ISO",resources$metadata_format)] <- "ISO 19115"
resources$metadata_format[grepl("19115",resources$metadata_format)] <- "ISO 19115"
resources$metadata_format[grepl("PDF",toupper(resources$metadata_format))] <- "PDF"
resources$metadata_format[grepl("CSV",toupper(resources$metadata_format))] <- "CSV"
resources$metadata_format[resources$metadata_format == ""] <- NA
resources$metadata_format[grepl("UNKNOWN",toupper(resources$metadata_format))] <- NA
resources$metadata_format[grepl("Not sure",resources$metadata_format)] <- NA
resources$metadata_format[grepl("VARIETY",resources$metadata_format)] <- NA
resources$metadata_format[grepl("VARIOUS",resources$metadata_format)] <- NA
resources$metadata_format[grepl("VARIES",resources$metadata_format)] <- NA
resources$metadata_format[resources$metadata_format == "?"] <- NA
resources$metadata_format[grepl("DON'T KNOW",toupper(resources$metadata_format))] <- NA
resources$metadata_format[grepl("IDK",toupper(resources$metadata_format))] <- NA
resources$metadata_format[resources$metadata_format == "I have no idea"] <- NA
resources$metadata_format[resources$metadata_format == "Not certain.  "] <- NA
resources$metadata_format[grepl("EXCEL",toupper(resources$metadata_format))] <- "EXCEL"
resources$metadata_format[grepl("XML",toupper(resources$metadata_format))] <- "XML"
resources$metadata_format[grepl("HTML",toupper(resources$metadata_format))] <- "HTML"
resources$metadata_format[grepl("ESRI",toupper(resources$metadata_format))] <- "ESRI"


resources$`Managing Agency or Business Center:`[resources$`Managing.Agency.or.Business.Center.` == "RHS"] <- "RD"
# table(resources2$Survey)
# reps <-
#   data.frame(table(resources2$`Managing Agency or Business Center:`[resources2$`Managing Agency or Business Center:` != "RMA"]))

inhouse <- resources %>%
  filter(digital_resource == "inhouse")

dataset <- resources %>%
  filter(digital_resource == "dataset")

agencies <- length(unique(resources$Managing.Agency.or.Business.Center.))
n_res <- nrow(resources)
n_respon <- length(unique(resources$Respondent.Full.Name))

respon_agen <- data.frame(table(resources[,c('Respondent.Full.Name', 'Managing.Agency.or.Business.Center.')]))
respon_agen <- respon_agen[respon_agen$Freq > 0, ]
reps <- data.frame(table(respon_agen$`Managing.Agency.or.Business.Center.`[respon_agen$`Managing.Agency.or.Business.Center.` != "RMA"]))
reps <- reps[reps$Var1 != "RMA", ]

contact$respondents <- reps$Freq
contact$resp_rate <- contact$respondents / contact$`Sum of # of contacts` * 100
## write out supplemental tables
webapps <- inhouse[inhouse$app_type == 'webapp', c('Name', 'app_purpose', 'private_public', 'agency', 'ownership_status', 'Details')]

desktop <- inhouse[inhouse$app_type == 'desktop', c('Name', 'app_purpose', 'private_public', 'agency', 'ownership_status', 'Details')]
#
# write.csv(webapps, 'report2_outputs/webapps.csv')
# write.csv(desktop, 'report2_outputs/desktop.csv')

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

ggsave("report2_outputs/contact_by_agency.jpeg")

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

ggsave("report2_outputs/respondants_agency.jpeg")

## Fixing Errors
# ggplot(resources, aes(x = Managing.Agency.or.Business.Center.)) +
#   geom_bar(aes(fill = Name.of.Digital.Resource.)) +
#   theme_classic() +
#   labs(title = "Digital Assets by type",
#        x = "Digital Asset Type",
#        y = "Number of Resources",
#        color = "Survey") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text(),
#         text = element_text(family = "Rubik"),
#         plot.title = element_text(hjust = 0.5)) +
#   geom_text(aes(label = ..count..),
#             stat = "count",
#             vjust = 1.5,
#             size = 8.0) +
#   scale_fill_brewer(palette = "Dark2")

## Fixing Errors
# ggplot(resources, aes(x = digital_resource)) +
#   geom_bar(aes(fill = `Survey`)) +
#   theme_classic() +
#   labs(x = "Digital Asset Type",
#        y = "Number of Resources",
#        color = "Survey") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text(),
#         text = element_text(family = "Rubik"),
#         plot.title = element_text(hjust = 0.5)) +
#   geom_text(aes(label = ..count..),
#             stat = "count",
#             vjust = 1.5,
#             size = 8.0) +
#   scale_fill_brewer(palette = "Dark2")
#
# ggsave("report2_outputs/asset-type.jpeg")

ggplot(resources, aes(x = storage_location)) +
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

ggsave("report2_outputs/storage.jpeg")


ggplot(resources, aes(x = storage_location)) +
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

ggsave("report2_outputs/storage_format.jpeg")

ggplot(resources, aes(x = storage_location)) +
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

ggsave("report2_outputs/storage_size.jpeg")

# datasets2$data_type[datasets2$data_type == ''] <- NA
# datasets2$data_type[datasets2$data_type == 'Animal'] <- NA
# datasets2$data_type[datasets2$data_type == 'Soil Water'] <- NA
# datasets2$data_type[datasets2$data_type == 'Yield'] <- 'Crop'
#
# datasets3 <- datasets2[!is.na(datasets2$data_type), ]
# datasets3$data_type[datasets3$data_type == 'Aerial Photo'] <- NA
# datasets3$data_type[datasets3$data_type == 'Publications'] <- NA
# datasets3$data_type[datasets3$data_type == 'Timber'] <- NA
# datasets3$data_type[datasets3$data_type == 'Fungal'] <- NA
# datasets3$data_type[datasets3$data_type == 'LU/LC'] <- NA
# datasets3$data_type[datasets3$data_type == 'Maps'] <- NA
# datasets3$data_type[datasets3$data_type == 'Conservation'] <- NA
# datasets3$data_type[datasets3$data_type == 'Nutrients'] <- NA
# datasets3$data_type[datasets3$data_type == 'Pollinators'] <- NA
# datasets3$data_type[datasets3$data_type == 'Photographs'] <- NA
# datasets3$data_type[datasets3$data_type == 'Large Data Repo'] <- NA
#
# datasets3 <- datasets3[!is.na(datasets3$data_type), ]

## Fixing Errors
# ggplot(datasets3, aes(x = data_type)) +
#   geom_bar(aes(fill = data_type)) +
#   theme_classic() +
#   labs(x = "Dataset Type",
#        y = "Number of Datasets",
#        fill = "data_type") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text(),
#         text = element_text(family = "Rubik"),
#         plot.title = element_text(hjust = 0.5),
#         legend.position = "none") +
#   geom_text(aes(label = ..count..),
#             stat = "count",
#             vjust = 0,
#             size = 8.0) +
#   scale_fill_brewer(palette = "Spectral")
#
# ggsave("report2_outputs/data_type.jpeg")
#
# ggplot(datasets3, aes(x = data_type)) +
#   geom_bar(aes(fill = storage_location)) +
#   theme_classic() +
#   labs(x = "Dataset Type",
#        y = "Number of Datasets",
#        fill = "data_type") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text(),
#         text = element_text(family = "Rubik"),
#         plot.title = element_text(hjust = 0.5)) +
#   geom_text(aes(label = ..count..),
#             stat = "count",
#             vjust = 0,
#             size = 8.0) +
#   scale_fill_brewer(palette = "PuOr")
#
# ggsave("report2_outputs/data_type_storage.jpeg")


ggplot(resources, aes(x = format)) +
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

ggsave("report2_outputs/format.jpeg")


ggplot(resources, aes(x = ownership_status)) +
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

ggsave("report2_outputs/ownership.jpeg")


ggplot(resources, aes(x = size)) +
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


ggsave("report2_outputs/size.jpeg")

ggplot(resources, aes(x = continuous)) +
  geom_bar(aes(fill = continuous)) +
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

ggsave("report2_outputs/continuous.jpeg")


ggplot(resources, aes(x = imagery)) +
  geom_bar(aes(fill = imagery)) +
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

ggsave("report2_outputs/imagery.jpeg")


ggplot(resources, aes(x = observational)) +
  geom_bar(aes(fill = observational)) +
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

ggsave("report2_outputs/observational.jpeg")


ggplot(resources, aes(x = private_public)) +
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

ggsave("report2_outputs/privacy.jpeg")

# Metadata charts
# Does the resource have associated metadata?
ggplot(resources, aes(x = metadata_status)) +
  geom_bar(aes(fill = metadata_status)) +
  theme_classic() +
  labs(title = "Dataset Metadata Status",
       x = "Metadata Status (Yes/No)",
       y = "Number of Datasets",
       fill = "Dataset Metadata Association Status") +
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

ggsave("report2_outputs/metadata_status.jpeg")

#Metadata ownership status
ggplot(resources, aes(x = ownership_status)) +
  geom_bar(aes(fill = metadata_status)) +
  theme_classic() +
  labs(title = "Dataset Metadata Status by Ownership",
       x = "Ownership Status",
       y = "Number of Datasets",
       fill = "Metadata Status (Yes/No)") +
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

ggsave("report2_outputs/metadata_ownership.jpeg")

# What format is the metadata?
ggplot(resources, aes(x = metadata_format)) +
  geom_bar(aes(fill = metadata_format)) +
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

ggsave("report2_outputs/metadata_format.jpeg")
# Metadata link?
# Is the metadata designed with a standard (e.g., ISO) in mind?
ggplot(resources, aes(x = metadata_standard_status)) +
  geom_bar(aes(fill = metadata_standard_status)) +
  theme_classic() +
  labs(title = "Dataset Metadata Standard Status",
       x = "Metadata Standard Status",
       y = "Number of Datasets",
       fill = "Dataset Associated Metadata Standard Assigned") +
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

ggsave("report2_outputs/metadata_standard_status.jpeg")
# Which metadata standard does this resource use?
# In your estimation, how closely does the metadata currently meet standard?
ggplot(resources, aes(x = metadata_standard_measure)) +
  geom_bar(aes(fill = metadata_standard_measure)) +
  theme_classic() +
  labs(title = "Measure of Dataset Metadata to Standard",
       x = "Measure of Dataset Metadata Standard",
       y = "Number of Datasets",
       fill = "1-10 How Well Metadata to Standard") +
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

ggsave("report2_outputs/metadata_standard_measure.jpeg")
# What is needed to increase alignment with metadata standards?
# What is need to meet standard?

# q <- data.frame(table(datasets2$collection_method))
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

## Fixing Errors
# inhouse_nona <- inhouse[!is.na(inhouse$app_purpose), ]
#
# ggplot(inhouse_nona, aes(x = app_type)) +
#   geom_bar(aes(fill = app_purpose)) +
#   theme_classic() +
#   labs(x = "Application type",
#        y = "Number of Applications",
#        fill = "Application Purpose") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text(),
#         text = element_text(family = "Rubik"),
#         plot.title = element_text(hjust = 0.5)) +
#   geom_text(aes(label = ..count..),
#             stat = "count",
#             vjust = 0,
#             size = 8.0) +
#   scale_fill_brewer(palette = "PuOr")
#
#
# ggsave("report2_outputs/app_type_purpose.jpeg")


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

ggsave("report2_outputs/app_ownership.jpeg")

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


ggsave("report2_outputs/app_privacy.jpeg")


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

ggsave("report2_outputs/inhouse_privacy_ownership.jpeg")

## Fixing Errors
# ggplot(inhouse_nona, aes(x = app_purpose)) +
#   geom_bar(aes(fill = app_purpose)) +
#   theme_classic() +
#   labs(x = "Application Purpose",
#        y = "Number of Datasets",
#        fill = "Application Purpose") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text(),
#         text = element_text(family = "Rubik"),
#         plot.title = element_text(hjust = 0.5),
#         legend.position = "none") +
#   geom_text(aes(label = ..count..),
#             stat = "count",
#             vjust = 1.5,
#             size = 8.0) +
#   scale_fill_brewer(palette = "Purples")
#
# ggsave("report2_outputs/app_purpose.jpeg")

# ggplot(inhouse, aes(x = app_purpose)) +
#   geom_bar(aes(fill = ownership_status)) +
#   theme_classic() +
#   labs(x = "Application Purpose",
#        y = "Number of Datasets",
#        fill = "Application Purpose") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text(),
#         text = element_text(family = "Rubik"),
#         plot.title = element_text(hjust = 0.5)) +
#   geom_text(aes(label = ..count..),
#             stat = "count",
#             vjust = 1.5,
#             size = 8.0) +
#   scale_fill_brewer(palette = "PuOr")
#
# models <- inhouse[inhouse$app_purpose == "Model", ]
#
# nas <- models[is.na(models$app_purpose), ]
# models <- models[!is.na(models$app_purpose), ]
