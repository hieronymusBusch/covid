##########################################################
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
library(gridExtra)

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
source("rscripts/functions.R")

## read in variables
# source("manipulation_ind.R")
# source("manipulation_dep.R")
# source("manipulation_depNew.R")
# source("manipulation_comb.R")

load(file = "rdata/dfdd.Rda")  
load(file = "rdata/dfds.Rda")  
load(file = "rdata/dfdsQ.Rda")  
load(file = "rdata/dfddnew.Rda")  
load(file = "rdata/dfdsnew.Rda") 
load(file = "rdata/dfddfnew.Rda")  
load(file = "rdata/dfdsfnew.Rda") 
load(file = "rdata/dfddmnew.Rda")  
load(file = "rdata/dfdsmnew.Rda") 



