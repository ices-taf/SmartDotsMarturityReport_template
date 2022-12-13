
library(icesTAF)
library(jsonlite)

if (FALSE) {
  rm(list = ls())

  # set current WD if required
  current_dir <- rstudioapi::getActiveDocumentContext()$path
  setwd(dirname(current_dir))
  getwd()

  # Load all the libraries that will be needed.
  library(lubridate)
  library(icesTAF)
  library(jsonlite) # necesary to read and modify the json configuration file
  library(rjson) # necesary to read and modify the json configuration file
  # library(tidyverse)
  library(tidyr)
  library(plyr)
  library(dplyr)
  library(tibble) # bias_test
  library(ggplot2)
  library(scales) # rescale_none
  library(ragree) # to estimate the coefficient of unalikeability, devtools::install_github("raredd/ragree")
  # Libraries for reporting with Rmarkdown
  library(rmarkdown)
  library(knitr)
  library(pander)
  require(Hmisc)
  library(tidyselect)
}


# Define some elements needed for the configuration file
event_number <- 398
tokens <- "tokens goes here"
OnlyApproved <- TRUE
select_strata <- "prep_method" # c("stock", "prep_method", "ices_area")   # Here the strata can be defined if it has not been defined already in the Smartdots app. By default it is set to "strata", so the strata defined in the data downloaded from ICES database is not changed. However, it can be changed for example to "stock", or "ices_area", or "prep_method", or a combination of them by using the function paste, like paste0(ad$stock, "-", ad$prep_method).
mode_definition <- "standard" # it must be set as "multistage" or "standard" the default way is multistage approach, that can be changed to the "standard" mode calculation.

# create config file in intial data folder
config <-
  list(
    event_id = unbox(event_number),
    ma_method = unbox("Mode"),
    onlyApproved = unbox(OnlyApproved),
    summary_name = unbox(paste0("SmartDots_Summary_Event_", event_number)),
    summary_title = unbox(paste0("SmartDots Summary for event ", event_number)),
    report_name = unbox(paste0("SmartDots_Report_Event_", event_number)),
    report_title = unbox(paste0("SmartDots Report for event ", event_number)),
    report_tokens = unbox(tokens),
    mode_definition = unbox(mode_definition), # the default way is "multistage" approach, that can be changed to the "standard" mode calculation.
    strata = "strata" # strata defined in smartdots reporting web page
  )

write_json(
  config, "bootstrap/initial/data/config.json",
  pretty = TRUE
)

# fetch data
taf.bootstrap(taf = TRUE)

# either run each in turn:
if (FALSE) {
  sourceTAF("data")
  sourceTAF("model")
  sourceTAF("report")
}

# or all at once
sourceAll()
