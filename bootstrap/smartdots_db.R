## Preprocess data, write TAF data tables

## Before:
## After: report_template.docx, dist.csv,
##        ad_long.csv, ad_long_ex.csv,
##        ad_wide.csv, ad_wide_ex.csv

library(icesTAF)
library(jsonlite)

# load configuration
config <- read_json(taf.data.path("config.json"), simplifyVector = TRUE)

# get data from api --------
zipfile <- "smartdots_data.zip"
download.file(
  paste0("https://smartdots.ices.dk/download/DownloadMaturityEvent.ashx?tblEventID=", config$event_id),
  zipfile,
  mode = "wb"
)


files <- unzip(zipfile, list = TRUE)$Name
files <- files[grep("*.csv", files)]
unzip(zipfile, files = files, exdir = ".")


# read in and write out again
ad <- read.csv(files[grep("Annotations",  files)], stringsAsFactors = FALSE)

# delete downloaded data
unlink(files)
unlink(zipfile)

# drop comments feild
ad <- ad[,names(ad) != "Comment"]

# set experise of event organiser to 1
if (all(is.na(ad$expertise[ad$TypeAnnotation == "Organizer"]))) {
  message("setting eventOrganiser expertise to 'expert' as none was provided.")
  ad$expertise[ad$TypeAnnotation == "Organizer"] <- 1
}

# Change the Strata variable in the ad database if needed
#if(unique(!select_strata == "strata")) {
#  for(i in 1:length(select_strata))
#  {
#    temp1=paste0("eval(parse(text=", "'ad$", select_strata[i],"'))")
#    if(i==1) {nombre=temp1} else {nombre=paste0("paste0(", nombre, ",'-'," , temp1, ")")}
#  }
#  ad$strata=eval(parse(text=nombre))
#}

# write out 'bootstrap' data tables
write.taf(ad, quote = TRUE)
