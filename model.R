## Run analysis, write model results

## Before: data/ad_long.csv, data/ad_long_ex.csv,
##         data/ad_wide.csv, data/ad_wide_ex.csv
## After:

library(icesTAF)
library(jsonlite)
#unloadNamespace("dplyr")
library(plyr) # age error matrix
library(dplyr)
library(tidyr)
library(tibble) # bias_test

library(ggplot2)
library(scales) # rescale_none

# library ragree contains the function unalike, that is used to estimate the coefficient of unalikeability
library(ragree)
#

# make model directory
mkdir("model")

# # load configuration
config <- read_json("bootstrap/data/config.json", simplifyVector = TRUE)

# load utilities
source("utilities.R")
source("utilities_model.R")

# read input data
ad_long_all <- read.taf("data/ad_long.csv")
ad_long_ex <- read.taf("data/ad_long_ex.csv")


# model maturity range
modal_matur_unique_all <-  sort(unique(ad_long_all$modal_maturity))
modal_matur_unique_ex <-  sort(unique(ad_long_ex$modal_maturity))

# model sex range
modal_sex_unique_all <-  sort(unique(ad_long_all$modal_sex))
modal_sex_unique_ex <-  sort(unique(ad_long_ex$modal_sex))

# set strata to NULL if all are NA
# if(length(setdiff("strata", names(config)))==0) {if(all(is.na(ad_long_all[["strata"]]))) config$strata <- NULL}

# Overview of samples and stagers ##############################################

# Sample overview
sample_data_overview <- sample_data_overview_table(ad_long_all, strata=config$strata)
write.taf(sample_data_overview, dir = "model")

# Participants table
stager_data <- reader_data_table(ad_long_all, strata=config$strata)
write.taf(stager_data, dir = "model")

# Results ##############################################

# repeat for all and for experts only

for (group in c("all", "ex")) {
 #group <- "all"
 #group <- "ex"

  # get the appropriate dataset
  ad_long <- get(vname("ad_long"))
  modal_matur_unique <- get(vname("modal_matur_unique"))
  modal_sex_unique <- get(vname("modal_sex_unique"))

  ad_long$modal_maturity <- factor(ad_long$modal_maturity, levels = modal_matur_unique)
  ad_long$modal_sex <- factor(ad_long$modal_sex, levels = modal_sex_unique)
  ad_long$reader <- factor(ad_long$reader)

  ##################################################################
  # number read table
  assign(
    vname("num_read_tab_modal_matur_by_reader"),
    num_read_table_modal_maturity(ad_long, by = "reader")
  )
  write.taf(vname("num_read_tab_modal_matur_by_reader"), dir = "model")

  assign(
    vname("num_read_tab_modal_sex_by_reader"),
    num_read_table_modal_sex(ad_long, by = "reader")
  )
  write.taf(vname("num_read_tab_modal_sex_by_reader"), dir = "model")

  ################################################################################################################################################################
  # Calculate the number of cases with multiple modes when the traditional method is used, the linear weight or the negative exponential weighting.

  # First the Maturity stage
  assign(
    vname("multimode_cases_tab_traditional_Maturity"),
    multimode_cases_table_traditional(ad_long, "Maturity")
  )
  write.taf(vname("multimode_cases_tab_traditional_Maturity"), dir = "model")

  assign(
    vname("multimode_cases_tab_linear_Maturity"),
    multimode_cases_table_linear(ad_long, "Maturity")
  )
  write.taf(vname("multimode_cases_tab_linear_Maturity"), dir = "model")

  assign(
    vname("multimode_cases_tab_negexp_Maturity"),
    multimode_cases_table_negexp(ad_long, "Maturity")
  )
  write.taf(vname("multimode_cases_tab_negexp_Maturity"), dir = "model")

  assign(
    vname("multimode_cases_tab_multistage_Maturity"),
    multimode_cases_table_multistage(ad_long, "Maturity")
  )
  write.taf(vname("multimode_cases_tab_multistage_Maturity"), dir = "model")

  # Next the sex category
  assign(
    vname("multimode_cases_tab_traditional_Sex"),
    multimode_cases_table_traditional(ad_long, "Sex")
  )
  write.taf(vname("multimode_cases_tab_traditional_Sex"), dir = "model")

  assign(
    vname("multimode_cases_tab_linear_Sex"),
    multimode_cases_table_linear(ad_long, "Sex")
  )
  write.taf(vname("multimode_cases_tab_linear_Sex"), dir = "model")

  assign(
    vname("multimode_cases_tab_negexp_Sex"),
    multimode_cases_table_negexp(ad_long, "Sex")
  )
  write.taf(vname("multimode_cases_tab_negexp_Sex"), dir = "model")

  assign(
    vname("multimode_cases_tab_multistage_Sex"),
    multimode_cases_table_multistage(ad_long, "Sex")
  )
  write.taf(vname("multimode_cases_tab_multistage_Sex"), dir = "model")


  ##############################################################################################################################
  # CU table (coefficient of unalikeability)
  assign(
    vname("cu_tab_maturity"),
    cu_table(ad_long, "Maturity", by = "reader")
  )
  write.taf(vname("cu_tab_maturity"), dir = "model")

  assign(
    vname("cu_tab_sex"),
    cu_table(ad_long, "Sex", by = "reader")
  )
  write.taf(vname("cu_tab_sex"), dir = "model")


  ##############################################################################################################################
  # Percent agreement between maturity stagings and modal maturity stage.
  assign(
    vname("pa_tab_maturity"),
    pa_table(ad_long, "Maturity", by = "reader")
  )
  write.taf(vname("pa_tab_maturity"), dir = "model")

  assign(
    vname("pa_tab_sex"),
    pa_table(ad_long, "Sex", by = "reader")
  )
  write.taf(vname("pa_tab_sex"), dir = "model")

  ##################################################################################################################################################
  #  Frequency table (Number for each maturity stage per modal_maturity for each ). This is the equivalent to the relative bias table in the ageing.
  assign(
    vname("stager_bias_freq_tab_maturity"),
    reader_freq_table(ad_long, "Maturity")
  )
  write.taf(vname("stager_bias_freq_tab_maturity"), dir = "model")

  assign(
    vname("stager_bias_freq_tab_sex"),
    reader_freq_table(ad_long, "Sex")
  )
  write.taf(vname("stager_bias_freq_tab_sex"), dir = "model")

  #########################################################################################################################################################
  # general Frequency table (Number for each maturity stage per modal_maturity for all s). This is the equivalent to the relative bias table in the ageing.
  assign(
    vname("general_bias_freq_tab_maturity"),
    general_freq_table(ad_long, "Maturity")
  )
  write.taf(vname("general_bias_freq_tab_maturity"), dir = "model")

  assign(
    vname("general_bias_freq_tab_sex"),
    general_freq_table(ad_long, "Sex")
  )
  write.taf(vname("general_bias_freq_tab_sex"), dir = "model")



  ##################################################################
  ## Annex tables###################################################
  ##################################################################

  #################
  # data overview
  if (FALSE) {
    # fails if a fish has no readings
    assign(
      vname("data_overview_tab_maturity"),
      data_overview_table(ad_long, "Maturity", config$report_tokens)
    )
    write.taf(vname("data_overview_tab_maturity"), dir = "model")
  }

  assign(
    vname("data_overview_tab_sex"),
    data_overview_table(ad_long, "Sex", config$report_tokens)
  )
  write.taf(vname("data_overview_tab_sex"), dir = "model")

  ########################
  # maturity composition
  assign(
    vname("maturity_composition_tab"),
    maturity_composition_table(ad_long, by = "reader")
  )
  write.taf(vname("maturity_composition_tab"), dir = "model")

  ####################
  # sex composition
  assign(
    vname("sex_composition_tab"),
    sex_composition_table(ad_long, by = "reader")
  )
  write.taf(vname("sex_composition_tab"), dir = "model")

  #############################################################
  # maturity staging error matrix (MSEM) only for advanced s
  assign(
    vname("msem"),
    mat_stage_error_matrix(ad_long, by = config$strata)
  )

  saveRDS(get(vname("msem")),  file = file.path("model", paste0(vname("msem"), ".rds")))

  #########################################################
  # sex category error matrix (SSEM) only for advanced s
  assign(
    vname("ssem"),
    sex_stage_error_matrix(ad_long, by = config$strata)
  )

  saveRDS(get(vname("ssem")),  file = file.path("model", paste0(vname("ssem"), ".rds")))



  ##################################################################
  # Results by strata ##############################################
  ##################################################################

  ##################################################################################################################################################
  #  Frequency table (Number for each maturity stage per modal_maturity for each ). This is the equivalent to the relative bias table in the ageing.
  if (is.null(config$strata)) config$strata <- numeric()
  for (stratum in config$strata) {
    assign(
      vsname("stager_bias_freq_tab_sex_by"),
      reader_freq_table(ad_long, varmod="Sex", by = stratum)
    )
    write.taf(vsname("stager_bias_freq_tab_sex_by"), dir = "model")


    ##################################################################
    # number read table
    assign(
      vsname("num_read_tab_modal_matur_by"),
      num_read_table_modal_maturity(ad_long, by = stratum)
    )
    write.taf(vsname("num_read_tab_modal_matur_by"), dir = "model")

    assign(
      vsname("num_read_tab_modal_sex_by"),
      num_read_table_modal_sex(ad_long, by = stratum)
    )
    write.taf(vsname("num_read_tab_modal_sex_by"), dir = "model")


    ##############################################################################################################################
    # CU table (coefficient of unalikeability)
    assign(
      vsname("cu_tab_maturity_by"),
      cu_table(ad_long, "Maturity", by = stratum)
    )
    write.taf(vsname("cu_tab_maturity_by"), dir = "model")

    assign(
      vsname("cu_tab_sex_by"),
      cu_table(ad_long, "Sex", by = stratum)
    )
    write.taf(vsname("cu_tab_sex_by"), dir = "model")


    ##############################################################################################################################
    # Percent agreement between maturity stagings and modal maturity stage.
    assign(
      vsname("pa_tab_maturity_by"),
      pa_table(ad_long, "Maturity", by = stratum)
    )
    write.taf(vsname("pa_tab_maturity_by"), dir = "model")

    assign(
      vsname("pa_tab_sex_by"),
      pa_table(ad_long, "Sex", by = stratum)
    )
    write.taf(vsname("pa_tab_sex_by"), dir = "model")

    ##################################
    ## Annex tables  #################
    ##################################

    ########################
    # maturity composition
  # #  assign(
  #
  # #    vname("maturity_composition_tab"),
  # #    maturity_composition_table(ad_long, by=stratum)
  # #  )
  # #  write.taf(vname("maturity_composition_tab"), dir = "model")

    ############################################################
    # maturity staging error matrix (MSEM) only for advanced s
    assign(
      vname("msem"),
      mat_stage_error_matrix(ad_long, by = stratum)
    )
    saveRDS(get(vname("msem")), file = file.path("model", paste0(vname("msem"), ".rds")))

    #########################################################
    # sex category error matrix (MSEM) only for advanced s
    assign(
      vname("ssem"),
      sex_stage_error_matrix(ad_long, by = stratum)
    )
    saveRDS(get(vname("ssem")), file = file.path("model", paste0(vname("ssem"), ".rds")))

  }

}



#config$strata=select_strata
