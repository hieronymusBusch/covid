##########################################################
#                                                        #
#                                                        #
# 2021/10                                                #
#                                                        #
# Alexander Busch (a.busch@lse.ac.uk)                    #
#                                                        #
# Data Manipulation                                      #
#                                                        #
##########################################################


## read in libraries
library(sf)
library(readxl)
library(plyr)
library(stringr) 
library(ggpubr)
library(stargazer)
library(spdep)
library(spatialreg)

# tidyverse
library(dplyr)
library(ggplot2)
library(purrr)

library(ggthemes)

# disable scientific notation (1.25e+2 => 125)
options(scipen = 99)  

# set system messages to english
Sys.setenv(lang = "en_US")

## read in data frames & manipulate
setwd("C:/Users/alexa/Documents/GitHub/covid")

## read in functions 
source("functions.R")

## read in variables
source("manipulation_ind.R")
source("manipulation_dep.R")
source("manipulation_comb.R")


