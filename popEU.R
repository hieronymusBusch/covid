##########################################################
#                                                        #
# BA Sociology:                                          #
# "COVID-19 in Germany, a social-geographic perspective" #
#                                                        #
# 2021/04                                                #
#                                                        #
# Alexander Busch (alexander.busch@stud.uni-hd.de)       #
#                                                        #
# Data Manipulation - Creating NUTS 3 data set           #
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

library(httr)

# tidyverse contents

library(dplyr)
library(ggplot2)
library(purrr)

# disable scientific notation (1.25e+2 => 125)
options(scipen = 99)  

# set system messages to english
Sys.setenv(lang = "en_US")

## read in data frames & manipulate
url1 <- "https://github.com/hieronymusBusch/covid/blob/master/data/PROJ_19RP3__custom_13386841632821589108.xlsx"
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))

eupop <- read_excel(tf, sheet = 3, range = "A11:CY413", col_types = c("guess", rep("numeric",101)))













