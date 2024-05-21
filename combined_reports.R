library(readxl)
library(ggplot2)
library("ggthemes")
library(extrafont)
library(RColorBrewer)
library(dplyr)

contact_s1 <- resources  <- read_excel("data/GEA_survey_contacts.xlsx", "Summary")

resources_s1  <- read_excel("data/GEA_Digital_Inventory_Draft_stakeholder5.xlsm", "resourceinfobeg_1")

resources_s1_2 <- read_excel("data/GEA_Digital_Inventory.xlsm", "resourceinfobeg_1")
resources_s1_2$`Managing Agency or Business Center:`[resources_s1_2$`Managing Agency or Business Center:` == "RHS"] <- "RD"

agencies_s1 <- length(unique(resources_s1$`Managing Agency or Business Center:`))
n_res_s1 <- nrow(resources_s1)
n_respon_s1 <- length(unique(resources_s1$`Respondent Full Name`))
datasets_s1 <- read.csv("data/datasets_edit.csv")
inhouse_s1 <- read.csv("data/ihs_edit.csv")

datasets_s1$data_type[datasets_s1$X == 106] <- "Climate"

respon_agen_s1 <- data.frame(table(resources_s1_2[,c('Respondent Full Name', 'Managing Agency or Business Center:')]))
respon_agen_s1 <- respon_agen_s1[respon_agen_s1$Freq > 0, ]
reps_s1 <-
  data.frame(table(respon_agen_s1$`Managing.Agency.or.Business.Center.`[respon_agen_s1$`Managing.Agency.or.Business.Center.` != "RMA"]))
reps_s1 <- reps_s1[reps_s1$Var1 != "RMA", ]

contact_s1$respondents <- reps_s1$Freq
contact_s1$resp_rate <- contact_s1$respondents / contact_s1$`Sum of # of contacts` * 100
## write out supplemental tables
webapps <- inhouse_s1[inhouse_s1$app_type == 'webapp', c('Name', 'app_purpose', 'private_public', 'agency', 'ownership_status', 'Details')]

desktop <- inhouse_s1[inhouse_s1$app_type == 'desktop', c('Name', 'app_purpose', 'private_public', 'agency', 'ownership_status', 'Details')]
names(contact_s1)[2] <- "contacts"

contact_s1 <- contact_s1 %>%
  arrange(desc(Agency)) %>%
  mutate(prop = contact_s1$contacts / sum(contact_s1$contacts) * 100) %>%
  mutate(ypos = cumsum(prop) - 0.5 * prop )

contact_s1 <- contact_s1[contact_s1$contacts > 4, ]

# Survey 2 personnel
personnel_s2 <- read.csv("data/Survey2/GEA_Digital_Inventory_2023_v5_0.csv")
contact_s2 <- personnel_s2
contact_s2$`Primary.Agency.or.Business.Center.`[contact_s2$`Primary.Agency.or.Business.Center.` == "RHS"] <- "RD"
contact_s2 <- contact_s2 %>%
  rename(Agency = Primary.Agency.or.Business.Center.) %>%
  filter(Agency != "RMA") %>%
  group_by(Agency) %>%
  summarise('Sum of # of contacts' = n())

# Survey 2 resources
resources_s2 <- read.csv("data/Survey2/resourceinfobeg_1.csv")
resources_s2 <- resources_s2 %>%
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

resources_s2$Managing.Agency.or.Business.Center.[resources_s2$Managing.Agency.or.Business.Center. == "RHS"] <- "RD"

# Classify and clean answers to metadata standard
resources_s2$metadata_standard_status[resources_s2$metadata_standard_status == ""] <- NA
resources_s2$metadata_standard[resources_s2$metadata_standard == ""] <- NA
resources_s2$metadata_standard[grepl("do not know",resources_s2$metadata_standard)] <- NA
resources_s2$metadata_standard[resources_s2$metadata_standard == "EGMO Directive"] <- "ISO 19115"
resources_s2$metadata_standard[grepl("19115",resources_s2$metadata_standard)] <- "ISO 19115"
resources_s2$metadata_standard[grepl("ISO",toupper(resources_s2$metadata_standard))] <- "ISO 19115"

# Classify and clean answers to metadata format
resources_s2$metadata_format[resources_s2$metadata_format == ""] <- NA
resources_s2$metadata_format[grepl("UNKNOWN",toupper(resources_s2$metadata_format))] <- NA
resources_s2$metadata_format[grepl("NOT",resources_s2$metadata_format)] <- NA
resources_s2$metadata_format[grepl("VARIETY",toupper(resources_s2$metadata_format))] <- NA
resources_s2$metadata_format[grepl("VARIOUS",toupper(resources_s2$metadata_format))] <- NA
resources_s2$metadata_format[grepl("VARIES",toupper(resources_s2$metadata_format))] <- NA
resources_s2$metadata_format[resources_s2$metadata_format == "?"] <- NA
resources_s2$metadata_format[grepl("DON'T KNOW",toupper(resources_s2$metadata_format))] <- NA
resources_s2$metadata_format[grepl("UNSURE",toupper(resources_s2$metadata_format))] <- NA
resources_s2$metadata_format[grepl("IDK",toupper(resources_s2$metadata_format))] <- NA
resources_s2$metadata_format[resources_s2$metadata_format == "I have no idea"] <- NA
resources_s2$metadata_format[resources_s2$metadata_format == "Not certain.  "] <- NA
resources_s2$metadata_format[grepl("HTTP",toupper(resources_s2$metadata_format))] <- "WEBSITE"
resources_s2$metadata_format[grepl("ONLINE",toupper(resources_s2$metadata_format))] <- "WEBSITE"
resources_s2$metadata_format[grepl("WEBSITE",toupper(resources_s2$metadata_format))] <- "WEBSITE"
resources_s2$metadata_format[grepl("ISO",resources_s2$metadata_format)] <- "ISO 19115"
resources_s2$metadata_format[grepl("19115",resources_s2$metadata_format)] <- "ISO 19115"
resources_s2$metadata_format[grepl("PDF",toupper(resources_s2$metadata_format))] <- "PDF"
resources_s2$metadata_format[grepl("CSV",toupper(resources_s2$metadata_format))] <- "CSV"
resources_s2$metadata_format[grepl("EXCEL",toupper(resources_s2$metadata_format))] <- "EXCEL"
resources_s2$metadata_format[grepl("XML",toupper(resources_s2$metadata_format))] <- "XML"
resources_s2$metadata_format[grepl("HTML",toupper(resources_s2$metadata_format))] <- "HTML"
resources_s2$metadata_format[grepl("ESRI",toupper(resources_s2$metadata_format))] <- "ESRI"


resources_s2$`Managing Agency or Business Center:`[resources_s2$`Managing.Agency.or.Business.Center.` == "RHS"] <- "RD"

# Listing of records with inhouse_s2 listed as digital resource
inhouse_s2 <- resources_s2 %>%
  filter(digital_resource == "inhouse")

# Listing of records with dataset listed as digital resource
dataset_s2 <- resources_s2 %>%
  filter(digital_resource == "dataset")

agencies_s2 <- length(unique(resources_s2$Managing.Agency.or.Business.Center.))
n_res_s2 <- nrow(resources_s2)
n_respon_s2 <- length(unique(resources_s2$Respondent.Full.Name))

respon_agen_s2 <- data.frame(table(resources_s2[,c('Respondent.Full.Name', 'Managing.Agency.or.Business.Center.')]))
respon_agen_s2 <- respon_agen_s2[respon_agen_s2$Freq > 0, ]
respon_agen_s2$Managing.Agency.or.Business.Center.[respon_agen_s2$Managing.Agency.or.Business.Center. %in% c("RUS", "RBS")] <- "RD"
reps_s2 <- data.frame(table(respon_agen_s2$`Managing.Agency.or.Business.Center.`[respon_agen_s2$`Managing.Agency.or.Business.Center.` != "RMA"]))
reps_s2 <- reps_s2[reps_s2$Var1 != "RMA", ]

contact_s2$respondents <- reps_s2$Freq
contact_s2$resp_rate <- contact_s2$respondents / contact_s2$`Sum of # of contacts` * 100
inhouse_s2 <- inhouse_s2 %>%
  rename(Agency = Managing.Agency.or.Business.Center.)
## write out supplemental tables
webapps_s2 <- inhouse_s2[inhouse_s2$app_type == 'webapp', c('Name', 'app_purpose', 'private_public', 'Agency', 'ownership_status', 'Details')]

desktop_s2 <- inhouse_s2[inhouse_s2$app_type == 'desktop', c('Name', 'app_purpose', 'private_public', 'Agency', 'ownership_status', 'Details')]

num_colors <- 11
spectralplus <- colorRampPalette(brewer.pal(11, "Spectral"))(num_colors)
names(contact_s2)[2] <- "contacts"

contact_s2 <- contact_s2 %>%
  arrange(desc(Agency)) %>%
  mutate(prop = contact_s2$contacts / sum(contact_s2$contacts) * 100) %>%
  mutate(ypos = cumsum(prop) - 0.5 * prop )

contact_s2 <- contact_s2[contact_s2$contacts > 4, ]


contact <- contact_s1
for (agency in contact_s1$Agency) {
  if (agency %in% contact_s2$Agency) {
    contact$respondents[contact$Agency == agency] <- contact$respondents[contact$Agency == agency] +
      contact_s2$respondents[contact_s2$Agency == agency]
  }

}


pie(contact$contacts, labels = contact_s2$Agency, border = "white", col = spectralplus) +
  theme(axis.text.x = element_blank())

ggsave("combined_survey_outputs/contact_by_agency.jpeg")


num_colors <- 17
spectralplus <- colorRampPalette(brewer.pal(11, "Spectral"))(num_colors)

respon_agen <- rbind(respon_agen_s1, respon_agen_s2)
respon_agen[respon_agen$Managing.Agency.or.Business.Center. %in% c()]

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

ggsave("combined_survey_outputs/respondants_agency.jpeg", width = 7, height = 5)


## combine datasets
datasets <- bind_rows(datasets_s1, dataset_s2)


ggplot(datasets, aes(x = private_public)) +
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

ggsave("combined_survey_outputs/privacy.jpeg")


# ggplot(datasets, aes(x = data_type)) +
#   geom_bar(aes(fill = data_type), position = position_dodge(0.5)) +
#   ylim(0, 30) +
#   theme_classic() +
#   labs(x = "Dataset Type",
#        y = "Number of Datasets",
#        fill = "data_type") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text(),
#         axis.text.x = element_text(size = 8),
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