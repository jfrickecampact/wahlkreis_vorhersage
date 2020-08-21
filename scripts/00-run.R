### ----------------------------------------------------------
### Replication Material for
### "An Approach to Predict the District Vote Shares in German Federal Elections"
###
###
### Marcel Neunhoeffer
###
### Run all files to get the results of the paper.
###
### ----------------------------------------------------------

# Last tested 20 Jan 2020 on:
### ----------------------------------------------------------
# platform       x86_64-apple-darwin15.6.0
# arch           x86_64
# os             darwin15.6.0
# system         x86_64, darwin15.6.0
# status
# major          3
# minor          6.1
# year           2019
# month          07
# day            05
# svn rev        76782
# language       R
# version.string R version 3.6.1 (2019-07-05)
# nickname       Action of the Toes
### ----------------------------------------------------------
# RStudio       1.2.1335


# Do you want to re-train the models or work with the stored results?
# If TRUE you will re-train the models.

retrain <- FALSE

# Save figures to the folder or show them in the plot window?
# If TRUE you will save the figures to the folder.

save_figures <- TRUE


# Source all the libraries needed for this analysis.
# Automatically installs if the package isn't installed.

source("01-load-packages.R")

# Also source some helpful functions

source("zz-functions.R")

# If the main data file does not yet exist, this will preprocess the data from the
# original source data files.

if (!"btw_candidates_1983-2017.csv" %in% list.files("../raw-data")) {
  source("02-data-preprocessing.R")
}

# Load the data to train the models.

source("03-load-data.R")

if (retrain) {
  source("04-train-models.R")
}

source("05-results-tables.R")

source("06-figure-1.R")

source("07-figure-2.R")

source("08-figure-3.R")