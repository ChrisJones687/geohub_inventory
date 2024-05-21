# library(readxl)
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

# Survey 2 resources
resources <- read.csv("data/Survey2/resourceinfobeg_1.csv")
resources <- resources %>%
  rename(data_type = Describe.the.resource.or.product.in.detail..If.there.is.additional.information.you.wish.to.provide..please.include.the.URL.of.any.relevant.documents.or.websites.,
         app_purpose = Please.provide.additional.detail.regarding.the.type.of.application.,
         Details = Please.provide.additional.detail.regarding.the.method.of.data.collection.,
         Name = Name.of.Digital.Resource.,
         metadata_status = Does.the.resource.have.associated.metadata.,
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

# Classify and clean answers to metadata standard
resources$metadata_standard_status[resources$metadata_standard_status == ""] <- NA
resources$metadata_standard[resources$metadata_standard == ""] <- NA
resources$metadata_standard[grepl("do not know",resources$metadata_standard)] <- NA
resources$metadata_standard[resources$metadata_standard == "EGMO Directive"] <- "ISO 19115"
resources$metadata_standard[grepl("19115",resources$metadata_standard)] <- "ISO 19115"
resources$metadata_standard[grepl("ISO",toupper(resources$metadata_standard))] <- "ISO 19115"

# Classify and clean answers to metadata format
resources$metadata_format[resources$metadata_format == ""] <- NA
resources$metadata_format[grepl("UNKNOWN",toupper(resources$metadata_format))] <- NA
resources$metadata_format[grepl("NOT",resources$metadata_format)] <- NA
resources$metadata_format[grepl("VARIETY",toupper(resources$metadata_format))] <- NA
resources$metadata_format[grepl("VARIOUS",toupper(resources$metadata_format))] <- NA
resources$metadata_format[grepl("VARIES",toupper(resources$metadata_format))] <- NA
resources$metadata_format[resources$metadata_format == "?"] <- NA
resources$metadata_format[grepl("DON'T KNOW",toupper(resources$metadata_format))] <- NA
resources$metadata_format[grepl("UNSURE",toupper(resources$metadata_format))] <- NA
resources$metadata_format[grepl("IDK",toupper(resources$metadata_format))] <- NA
resources$metadata_format[resources$metadata_format == "I have no idea"] <- NA
resources$metadata_format[resources$metadata_format == "Not certain.  "] <- NA
resources$metadata_format[grepl("HTTP",toupper(resources$metadata_format))] <- "WEBSITE"
resources$metadata_format[grepl("ONLINE",toupper(resources$metadata_format))] <- "WEBSITE"
resources$metadata_format[grepl("WEBSITE",toupper(resources$metadata_format))] <- "WEBSITE"
resources$metadata_format[grepl("ISO",resources$metadata_format)] <- "ISO 19115"
resources$metadata_format[grepl("19115",resources$metadata_format)] <- "ISO 19115"
resources$metadata_format[grepl("PDF",toupper(resources$metadata_format))] <- "PDF"
resources$metadata_format[grepl("CSV",toupper(resources$metadata_format))] <- "CSV"
resources$metadata_format[grepl("EXCEL",toupper(resources$metadata_format))] <- "EXCEL"
resources$metadata_format[grepl("XML",toupper(resources$metadata_format))] <- "XML"
resources$metadata_format[grepl("HTML",toupper(resources$metadata_format))] <- "HTML"
resources$metadata_format[grepl("ESRI",toupper(resources$metadata_format))] <- "ESRI"


resources$`Managing Agency or Business Center:`[resources$`Managing.Agency.or.Business.Center.` == "RHS"] <- "RD"
# table(resources2$Survey)
# reps <-
#   data.frame(table(resources2$`Managing Agency or Business Center:`[resources2$`Managing Agency or Business Center:` != "RMA"]))

# Listing of records with inhouse listed as digital resource
inhouse <- resources %>%
  filter(digital_resource == "inhouse")

# Listing of records with dataset listed as digital resource
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
inhouse <- inhouse %>%
  rename(Agency = Managing.Agency.or.Business.Center.)
## write out supplemental tables
webapps <- inhouse[inhouse$app_type == 'webapp', c('Name', 'app_purpose', 'private_public', 'Agency', 'ownership_status', 'Details')]

desktop <- inhouse[inhouse$app_type == 'desktop', c('Name', 'app_purpose', 'private_public', 'Agency', 'ownership_status', 'Details')]
#
write.csv(webapps, 'report2_outputs/webapps.csv')
write.csv(desktop, 'report2_outputs/desktop.csv')

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

# Fixing Errors
ggplot(resources, aes(x = Managing.Agency.or.Business.Center.)) +
  geom_bar(aes(fill = Name)) +
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

resources2 <- resources %>%
  filter(size != '')

ggplot(resources2, aes(x = storage_location)) +
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

resources3 <- resources %>%
  filter(storage_location != '')

ggplot(resources3, aes(x = storage_location)) +
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

ggplot(resources3, aes(x = storage_location)) +
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

resources$data_type[resources$data_type == ''] <- NA
resources$data_type[resources$data_type == 'l'] <- NA
resources$data_type[resources$data_type == 'No'] <- NA
resources$data_type[resources$data_type == 'click to sign'] <- NA
resources$data_type[resources$data_type == 'abcd123'] <- NA
resources$data_type[resources$data_type == 'MIDAS, CARS'] <- NA
resources$data_type[resources$data_type == 'dd'] <- NA
resources$data_type[resources$data_type == 'Mission critical to Program Administration.'] <- NA
resources$data_type[grepl("TOO MANY",toupper(resources$data_type))] <- NA
resources$data_type[grepl("VARIETY",toupper(resources$data_type))] <- NA
resources$data_type[grepl("ASSESS DVMO",toupper(resources$data_type))] <- NA
resources$data_type[grepl("HELP OPTION",toupper(resources$data_type))] <- NA
resources$data_type[grepl("ELIGIBILITY",toupper(resources$data_type))] <- NA
resources$data_type[grepl("NOT SURE",toupper(resources$data_type))] <- NA
resources$data_type[grepl("N/A",toupper(resources$data_type))] <- NA
resources$data_type[grepl("DON'T KNOW",toupper(resources$data_type))] <- NA
resources$data_type[grepl("DO NOT",toupper(resources$data_type))] <- NA
resources$data_type[grepl("NA",toupper(resources$data_type))] <- NA
resources$data_type[grepl("NONE",toupper(resources$data_type))] <- NA

resources$data_type[grepl("SOIL",toupper(resources$data_type))] <- "Soil"
resources$data_type[grepl("WSS",toupper(resources$data_type))] <- "Soil"

resources$data_type[grepl("IMAGERY",toupper(resources$data_type))] <- "Imagery"
resources$data_type[grepl("IMAGES",toupper(resources$data_type))] <- "Imagery"
resources$data_type[grepl("RASTER",toupper(resources$data_type))] <- "Imagery"
resources$data_type[grepl("DIGITALGLOBE",toupper(resources$data_type))] <- "Imagery"

resources$data_type[grepl("CLIMATE",toupper(resources$data_type))] <- "Climate"
resources$data_type[grepl("TEMPERATURE",toupper(resources$data_type))] <- "Climate"

resources$data_type[grepl("CROP",toupper(resources$data_type))] <- "Crop"
resources$data_type[grepl("FARM",toupper(resources$data_type))] <- "Crop"
resources$data_type[grepl("USDA",toupper(resources$data_type))] <- "Crop"

resources$data_type[grepl("TRANSPORT",toupper(resources$data_type))] <- "Transportation"

resources$data_type[grepl("PEST",toupper(resources$data_type))] <- "Pest/Disease"
resources$data_type[grepl("DISEASE",toupper(resources$data_type))] <- "Pest/Disease"

resources$data_type[grepl("VEGETATION",toupper(resources$data_type))] <- "Vegetation"
resources$data_type[grepl("FOREST",toupper(resources$data_type))] <- "Vegetation"
resources$data_type[grepl("FIA",toupper(resources$data_type))] <- "Genomic"

resources$data_type[grepl("WATER",toupper(resources$data_type))] <- "Water"
resources$data_type[grepl("USFWS NWI",toupper(resources$data_type))] <- "Water"
resources$data_type[grepl("FLOOD",toupper(resources$data_type))] <- "Water"

resources$data_type[grepl("TOPOGRAPHY",toupper(resources$data_type))] <- "Land"
resources$data_type[grepl("LIDAR",toupper(resources$data_type))] <- "Land"
resources$data_type[grepl("ELEVATION",toupper(resources$data_type))] <- "Land"
resources$data_type[grepl("LAND",toupper(resources$data_type))] <- "Land"
resources$data_type[grepl("ROADS",toupper(resources$data_type))] <- "Land"
resources$data_type[grepl("USGS",toupper(resources$data_type))] <- "Land"
resources$data_type[grepl("WEPP",toupper(resources$data_type))] <- "Land"
resources$data_type[grepl("EARTH",toupper(resources$data_type))] <- "Land"
resources$data_type[grepl("SITE",toupper(resources$data_type))] <- "Land"


resources$data_type[grepl("PUBLIC HEAL",toupper(resources$data_type))] <- "Health"

resources$data_type[grepl("BOUNDARY",toupper(resources$data_type))] <- "Boundary"
resources$data_type[grepl("SHAPEFILE",toupper(resources$data_type))] <- "Boundary"
resources$data_type[grepl("LAYER",toupper(resources$data_type))] <- "Boundary"
resources$data_type[grepl("AGOL",toupper(resources$data_type))] <- "Boundary"
resources$data_type[grepl("ARCMAP",toupper(resources$data_type))] <- "Boundary"
resources$data_type[grepl("ARCGIS",toupper(resources$data_type))] <- "Boundary"
resources$data_type[grepl("SPATIAL TABLES",toupper(resources$data_type))] <- "Boundary"
resources$data_type[grepl("SUBDIVISIONS",toupper(resources$data_type))] <- "Boundary"

resources$data_type[grepl("GENOMIC",toupper(resources$data_type))] <- "Genomic"
resources$data_type[grepl("BEE BIOLOGY",toupper(resources$data_type))] <- "Genomic"
resources$data_type[grepl("SPECIES",toupper(resources$data_type))] <- "Genomic"
resources$data_type[grepl("WILDLIFE",toupper(resources$data_type))] <- "Genomic"

resources$data_type[grepl("ACCESS",toupper(resources$data_type))] <- "Collection\nStorage"
resources$data_type[grepl("DATABASE",toupper(resources$data_type))] <- "Collection\nStorage"
resources$data_type[grepl("COLLECTION",toupper(resources$data_type))] <- "Collection\nStorage"
resources$data_type[grepl("SURVEY 123",toupper(resources$data_type))] <- "Collection\nStorage"
resources$data_type[grepl("RD APPLY",toupper(resources$data_type))] <- "Collection\nStorage"
resources$data_type[grepl("CSV",toupper(resources$data_type))] <- "Collection\nStorage"
resources$data_type[grepl("XCEL",toupper(resources$data_type))] <- "Collection\nStorage"
resources$data_type[grepl("LOCATIONS",toupper(resources$data_type))] <- "Collection\nStorage"
resources$data_type[grepl("TOOLS",toupper(resources$data_type))] <- "Collection\nStorage"
resources$data_type[grepl("PLANNING TOOL",toupper(resources$data_type))] <- "Collection\nStorage"
resources$data_type[grepl("HERITAGE",toupper(resources$data_type))] <- "Collection\nStorage"
resources$data_type[grepl("CITRIX",toupper(resources$data_type))] <- "Collection\nStorage"
resources$data_type[grepl("COLLECT",toupper(resources$data_type))] <- "Collection\nStorage"
resources$data_type[grepl("CONTACT",toupper(resources$data_type))] <- "Collection\nStorage"
resources$data_type[grepl("DATA.GOV",toupper(resources$data_type))] <- "Collection\nStorage"

# Dataset for the purpose of plotting data_type independently
dataset2 <- resources[!is.na(resources$data_type), ]

# Fixing Errors
ggplot(dataset2, aes(x = data_type)) +
  geom_bar(aes(fill = data_type), position = position_dodge(0.5)) +
  ylim(0, 30) +
  theme_classic() +
  labs(x = "Dataset Type",
       y = "Number of Datasets",
       fill = "data_type") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        axis.text.x = element_text(size = 8),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 0,
            size = 8.0) +
  scale_fill_brewer(palette = "Spectral")

ggsave("report2_outputs/data_type.jpeg")

ggplot(dataset2, aes(x = data_type)) +
  geom_bar(aes(fill = storage_location)) +
  ylim(0, 30) +
  theme_classic() +
  labs(x = "Dataset Type",
       y = "Number of Datasets",
       fill = "data_type") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        axis.text.x = element_text(size = 8),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 0,
            size = 8.0) +
  scale_fill_brewer(palette = "PuOr")

ggsave("report2_outputs/data_type_storage.jpeg")

resources4 <- resources %>%
  filter(format != '')

ggplot(resources4, aes(x = format)) +
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


ggplot(resources2, aes(x = size)) +
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

## Metadata charts
# 1. Does the resource have associated metadata?
ggplot(resources, aes(x = metadata_status)) +
  geom_bar(aes(fill = metadata_status)) +
  theme_classic() +
  labs(title = "Dataset Metadata",
       x = "Metadata with Dataset",
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

#2. Which Agency has metadata associated with data?
ggplot(resources, aes(x = Managing.Agency.or.Business.Center.)) +
  geom_bar(aes(fill = metadata_status)) +
  theme_classic() +
  labs(title = "Datasets with Metadata by Agency",
       x = "Agency",
       y = "Number of Datasets",
       fill = "Dataset has Metadata?") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(fill = metadata_status,label = ..count..),
            stat = "count",
            vjust = 1.5,
            size = 5.0,
            position = "stack") +
  scale_fill_brewer(palette = "Dark2")

ggsave("report2_outputs/agency_metadata.jpeg")

#3. Which data types have associated metadata?
ggplot(dataset2, aes(x = data_type)) +
  geom_bar(aes(fill = metadata_status)) +
  theme_classic() +
  ylim(0,25) +
  labs(title = "Datasets with Metadata by Type of Data",
       x = "data_type",
       y = "Number of Datasets",
       fill = "Dataset has Metadata?") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        axis.text.x = element_text(size = 8),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(fill = metadata_status,label = ..count..),
            stat = "count",
            vjust = 1.5,
            size = 5.0,
            position = "stack") +
  scale_fill_brewer(palette = "Dark2")

ggsave("report2_outputs/data_type_metadata.jpeg")

dataset3 <- dataset2[dataset2$app_type != '',]

# 4. Which app types have associated metadata?
ggplot(dataset3, aes(x = app_type)) +
  geom_bar(aes(fill = metadata_status)) +
  theme_classic() +
  labs(title = "Datasets with Metadata by Type of Application",
       x = "Type of Application",
       y = "Number of Datasets",
       fill = "Dataset has Metadata?") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        axis.text.x = element_text(size = 8),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(fill = metadata_status,label = ..count..),
            stat = "count",
            vjust = 1.5,
            size = 8.0,
            position = "stack") +
  scale_fill_brewer(palette = "Dark2")

ggsave("report2_outputs/app_type_metadata.jpeg")

metadata_stnd <- dataset2[!is.na(dataset2$metadata_standard_status),]

# 5. Is the metadata designed with a standard (e.g., ISO) in mind?
ggplot(metadata_stnd, aes(x = metadata_standard_status)) +
  geom_bar(aes(fill = metadata_standard_status)) +
  theme_classic() +
  labs(title = "Datasets with Standard Associated with Metadata",
       x = "Does Metadata have a Standard?",
       y = "Number of Datasets",
       fill = "Datasets with Standard Associated with Metadata") +
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

# 6. Does the storage location correlate to metadata presence?
ggplot(resources, aes(x = storage_location)) +
  geom_bar(aes(fill = metadata_status)) +
  theme_classic() +
  labs(title = "Datasets by Storage Location and Metadata",
       x = "Dataset Storage Location",
       y = "Number of Datasets",
       fill = "Metadata Associated with Dataset?") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(fill = metadata_status, label = ..count..),
            stat = "count",
            vjust = 1.5,
            size = 5.0,
            position = "stack") +
  scale_fill_brewer(palette = "Dark2")

ggsave("report2_outputs/metadata_storage.jpeg")

# Metadata formats
metadata_fmts <- resources %>%
  filter(metadata_status == "yes",
         metadata_format != '')
metadata_fmts <- metadata_fmts[c("Managing.Agency.or.Business.Center.",
                       "Name",
                       "metadata_format")]

respon_fmts <- data.frame(table(metadata_fmts[,c('metadata_format')]))
respon_fmts <- respon_fmts %>%
  rename(metadata_format = Var1)
metadata_fmts <- data.frame(metadata_fmts[c("Managing.Agency.or.Business.Center.","metadata_format")])

metadata_compl_fmts <- inner_join(metadata_fmts, respon_fmts, by = join_by(metadata_format))
metadata_compl_fmts <- metadata_compl_fmts %>%
  dplyr::group_by(Managing.Agency.or.Business.Center.,
                  metadata_format) %>%
  summarise(.groups = "drop")

write.csv(metadata_compl_fmts, "report2_outputs/metadata_format.csv")

# 5. Which metadata standard does this resource use?
standard <- resources %>%
  filter(metadata_status == "yes",
         metadata_standard != '')
standard <- standard[c("Managing.Agency.or.Business.Center.",
                       "Name",
                       "metadata_standard")]

write.csv(standard, "report2_outputs/metadata_standard.csv")

# 6. In your estimation, how closely does the metadata currently meet standard?
ggplot(resources, aes(x = metadata_standard_measure)) +
  geom_bar(aes(fill = metadata_standard_measure)) +
  theme_classic() +
  labs(title = "Measure of Dataset Metadata Conforming to Standard",
       x = "Estimation of Fit to Standard (10=Closely Meets Standard)",
       y = "Number of Datasets",
       fill = "Measure of Fit to Standard (10=Closely Meets Standard") +
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
alignment <- resources %>%
  filter(metadata_status == "yes",
         metadata_standard_status == "yes",
         metadata_standard_measure != '',
         What.is.needed.to.increase.the.resource.s.alignment.with.the.metadata.standard. != '')
alignment <- alignment[c("Managing.Agency.or.Business.Center.",
                         "Name",
                         "metadata_standard_measure",
                         "What.is.needed.to.increase.the.resource.s.alignment.with.the.metadata.standard.")]

write.csv(alignment, "report2_outputs/metadata_alignment.csv")

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

ggsave("report2_outputs/app_type.jpeg")

ggplot(inhouse, aes(x = app_type)) +
  geom_bar(aes(fill = ownership_status)) +
  theme_classic() +
  labs(x = "Application type",
       y = "Number of Applications",
       fill = "Application Ownership") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = 1.0,
            size = 8.0) +
  scale_fill_brewer(palette = "PuOr")

ggsave("report2_outputs/app_type_ownership.jpeg")

# Fixing Errors
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


ggsave("report2_outputs/app_type_purpose.jpeg")


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

ggsave("report2_outputs/app_purpose.jpeg")

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

