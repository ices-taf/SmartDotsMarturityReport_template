## Check, preprocess data, write TAF data tables

# # Set directory
# # setwd(getwd())
# setwd("C:/IMR/SmartDots/maturity_staging/Template_gonad_staging_Multistage_approach_IV/")

# create data directory
mkdir("data")

sourceTAF("data_checker.R")
sourceTAF("data_processing.R")

# done

