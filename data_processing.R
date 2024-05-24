## Preprocess data, write TAF data tables

## Before:
## After: report_template.docx, dist.csv,
##        ad_long.csv, ad_long_ex.csv,
##        ad_wide.csv, ad_wide_ex.csv

library(icesTAF)
library(jsonlite)
#library(tidyr)
#library(lubridate)
library(plyr)
library(dplyr)
library(tidyverse)
taf.library(ragree)
#devtools::install_github("raredd/ragree")
#library(ragree)
library(tidyr)

library(ggplot2)

#library(icesTAF)
#library(jsonlite)
#library(tidyr)
#library(lubridate)
#library(plyr)
#library(dplyr)
#library(tidyverse)
library(tidyselect) #function all_of
#taf.library(ragree)
#devtools::install_github("raredd/ragree")
#library(ragree)

# create data directory
#mkdir("data")

# get utility functions
source("utilities.R")
source("utilities_data.R")

# load configuration
#config <- read_json("bootstrap/data/config.json", simplifyVector = TRUE)
config <- read_json("bootstrap/initial/data/config.json", simplifyVector = TRUE)


# get data from bootstrap folder  -------------------------------
ad <- read.taf("bootstrap/data/smartdots_db/ad.csv")
#ad <- read.taf("bootstrap/smartdots_db/ad.csv")
#ad <- read.taf("bootstrap/ad.csv")

# prepare data -------------------------------

# keep only approved annotations
if (config$onlyApproved) {
  ad <- ad[ad$IsApproved == 1, ]
}

# add date columns
 ad <-
  within(ad, {
     year <- year(parse_date_time(catch_date, "%d/%m/%Y %H:%M:%S"))
     qtr <- quarter(parse_date_time(catch_date, "%d/%m/%Y %H:%M:%S"))
     month <- month(parse_date_time(catch_date, "%d/%m/%Y %H:%M:%S"))
   })


 # if variables are missing add "missing"
 ad$ices_area[is.na(ad$ices_area) | ad$ices_area == ""] <- "missing"
 ad$stock[is.na(ad$stock) | ad$stock == ""] <- "missing"
 ad$prep_method[is.na(ad$prep_method) | ad$prep_method == ""] <- "missing"




 # if no advanced readers! make them all advanced
 if (all(ad$expertise == 0)) {
   msg("NOTE: all readers were Basic - all have been converted to Advanced")
   ad$expertise[] <- 1
 }

 # convert reader expertise
 ad$expertise <- c("Basic", "Advanced")[ad$expertise + 1]

 # Assign weight to the readers based in their ranking-experience
 # Add different weight columns to data
 weight <- length(sort(unique(ad$reader_number))):1
 reader_number <- sort(unique(ad$reader_number))
 reader <- data.frame(reader_number = reader_number, weight_I = weight, weight_II = 1 / (1 + log(sort(weight, decreasing = FALSE) + 0.0000000001)))
 ad <- merge(ad, reader, by.x = "reader_number", by.y = "reader_number", all.x = TRUE)

 ad$TypeAnnotation[ad$TypeAnnotation=="Reader"]<-"reader"
 ad$TypeAnnotation[ad$TypeAnnotation=="Country coordinator"]<-"reader"
 ad$expertise[ad$expertise=="1"]<-"Advanced"
 ad$TypeAnnotation[ad$TypeAnnotation=="Delegate"]<-"eventOrganizer"
 ad$TypeAnnotation[ad$TypeAnnotation=="eventDelegate"]<-"eventOrganizer"
 ad$TypeAnnotation[ad$TypeAnnotation=="Organizer"]<-"eventOrganizer"
 ad$reader[ad$reader==""]<-"eventOrganizer" ## to add a name to the Reader column from the Event Organizer
 #ad$reader[ad$TypeAnnotation=="eventOrganizer"]<-"eventOrganizer"
# ad$reader_number[ad$TypeAnnotation=="eventOrganizer"]<-1
 ad$expertise[ad$TypeAnnotation=="eventOrganizer"]<-"Advanced"
 ad$reader[ad$TypeAnnotation=="eventOrganizer"]<-"R01 EO"
 ad$Sex[is.na(ad$Sex) | ad$Sex==""]<-"NI"
 ad<-ad[ad$Sex!="NI",]
 ad$Maturity[is.na(ad$Maturity) | ad$Maturity==""]<-"NI"
 ad<-ad[ad$Maturity!="NI",]
 ad$SampleID<- ad$FishID  ### 6/02/2024

 ad4webgr <- ad



 # Before calculating the mode, give to the sampleID of readings by eventOrganizer (histological sample) the same name as the samples analyzed by the other readers, so the maturity defined for the histological samples is assigned as mode to the other samples of the same FishID
 #fishid <- sort(unique(ad$FishID))
 #for (i in 1:length(fishid))
 # {
 #   nohist <- ad[ad$FishID == fishid[i] & ad$TypeAnnotation != "eventOrganizer", ]
 #   sampleid_nohist <- unique(nohist$SampleID)
 #   yeshist <- ad[ad$FishID == fishid[i] & ad$TypeAnnotation == "eventOrganizer", ]
 #   if (dim(yeshist)[1] > 0) {
 #     yeshist <- yeshist[rep(row.names(yeshist), length(sampleid_nohist)), ]
 #     yeshist$SampleID <- sampleid_nohist
 #   }
 #
 #   temp <- rbind(nohist, yeshist)
 #
 #   if (i == 1) {
 #     result <- temp
 #   } else {
 #     result <- rbind(result, temp)
 #   }
 # }

 #ad <- result

 # Calculate modal maturity stage and coefficient of unalikability of maturity stage
 ad_long <- ad %>%
   add_modal_trad(varmod = "Maturity", config$ma_method) %>%
   add_modal_linearweight(varmod = "Maturity", config$ma_method) %>%
   add_modal_negexpweight(varmod = "Maturity", config$ma_method)

 ad_long_adv <- ad[ad$expertise == "Advanced", ] %>%
   add_modal_trad(varmod = "Maturity", config$ma_method) %>%
   add_modal_linearweight(varmod = "Maturity", config$ma_method) %>%
   add_modal_negexpweight(varmod = "Maturity", config$ma_method)


 # Calculate modal maturity stage and coefficient of unalikability of sex category
 ad_long <- ad_long %>%
   add_modal_trad(varmod = "Sex", config$ma_method) %>%
   add_modal_linearweight(varmod = "Sex", config$ma_method) %>%
   add_modal_negexpweight(varmod = "Sex", config$ma_method)

 ad_long_adv <- ad_long_adv %>%
   add_modal_trad(varmod = "Sex", config$ma_method) %>%
   add_modal_linearweight(varmod = "Sex", config$ma_method) %>%
   add_modal_negexpweight(varmod = "Sex", config$ma_method)

 # Choose the final mode (traditional, readers linear weight or negative exponential linear weight) based in the existence of histological samples or not, and, in case there are no histological samples, depending if there is multimodality or not.
 ad_long <- select_mode(ad_long, config$ma_method, config$mode_definition)
 ad_long_adv <- select_mode(ad_long_adv, config$ma_method, config$mode_definition)

 # prepare data in wbgr output format
 # IMAGE,1,2,3,4,5,6,7,8,9,10,11,12,13
 # Expertise level,-,-,-,-,-,-,-,-,-,-,-,-,-
 # Stock assessment,no,no,no,no,no,no,no,no,no,no,no,no,no
 # 6698256.jpg,1,1,1,1,1,-,2,1,-,2,-,1,-
 # 6698257.jpg,3,3,3,3,2,1,3,3,-,3,-,3,-
 readers <- sort(unique(ad$reader))

 webgr_maturity <- spread(ad4webgr[c("FishID", "reader", "Maturity")], key = reader, value = Maturity)
 webgr_maturity[] <- paste(unlist(webgr_maturity))
 webgr_maturity[webgr_maturity == "NA"] <- "-"
 webgr_maturity <-
   rbind(
     c("Expertise level", rep("-", length(readers))),
     c("Stock assessment", rep("no", length(readers))),
     webgr_maturity
   )
 names(webgr_maturity) <- c("IMAGE", 1:length(readers))
 head(webgr_maturity)

 webgr_sex <- spread(ad4webgr[c("FishID", "reader", "Sex")], key = reader, value = Sex)
 webgr_sex[] <- paste(unlist(webgr_sex))
 webgr_sex[webgr_sex == "NA"] <- "-"
 webgr_sex <-
   rbind(
     c("Expertise level", rep("-", length(readers))),
     c("Stock assessment", rep("no", length(readers))),
     webgr_sex
   )
 names(webgr_sex) <- c("IMAGE", 1:length(readers))
 head(webgr_sex)



 # write out input data tables for use later
 write.taf(ad, file = "data.csv", dir = "data", quote = TRUE)
 write.taf(ad_long, dir = "data", quote = TRUE)
 write.taf(ad_long_adv, dir = "data", quote = TRUE)
 write.taf(webgr_maturity, file = "WebGR_maturity_all.csv", dir = "data", quote = TRUE)
 write.taf(webgr_sex, file = "WebGR_sex_all.csv", dir = "data", quote = TRUE)
