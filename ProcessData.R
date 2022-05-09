# jonashaslbeck@gmail.com; May 5, 2022

# ------------------------------------------------------------------
# ------------- Load Packages --------------------------------------
# ------------------------------------------------------------------

library(tidyverse)
library(haven)
library(plyr)
library(dplyr)

# ------------------------------------------------------------------
# ------------- Run all Scripts ------------------------------------
# ------------------------------------------------------------------

# Get all preprocessing scripts
v_files <- list.files("ProcessingFiles/")
n_files <- length(v_files)

# Run all scripts
for(DF in 1:n_files) {
  source(paste0("ProcessingFiles/", v_files[DF]))
  print(paste0(v_files[DF], " [Processed]"))
}


# ------------------------------------------------------------------
# ------------- Create List of Between-person datasets -------------
# ------------------------------------------------------------------

source("ProcessBetween.R")


