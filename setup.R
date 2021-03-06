#-------------------------------------------
# This script sets out to load all 
# things required for the project
#-------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 17 September 2020
#-------------------------------------------

# Load packages

library(tidyverse)
library(readxl)
library(janitor)
library(scales)
library(Cairo)
library(ggpubr)
library(mgcv)
library(sjPlot)
library(lubridate)
library(tidyLPA)
library(rstan)
library(bayesplot)

# Turn off scientific notation

options(scipen = 999)

# Create an output folder if none exists:

if(!dir.exists('output')) dir.create('output')

# Run processing script to prep data for modelling

r_files <- list.files("processing", full.names = TRUE, pattern = "\\.[Rr]")
for(f in r_files){
  source(f)
}
