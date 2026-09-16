# Main script to reproduce data-based figures and tables
# fvfmPy and fvfmR Project
# Garen, Arnold, and Crous 2026

# Load libraries
library(tidyverse)
library(freqTLS)
library(cowplot)

# Load scripts for figures and tables
source("R/make_Fig3.R")
source("R/make_Fig4.R")
source("R/make_Table2.R")

# Make directories for outputs if none exist
if(!dir.exists("Figures")) dir.create("Figures")
if(!dir.exists("Tables")) dir.create("Tables")

# Produce figures and tables
make_Fig3()
make_Fig4()
make_Table2()

#######
# END #
#######
