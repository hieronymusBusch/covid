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

# tidyverse contents

library(dplyr)
library(ggplot2)
library(purrr)

# disable scientific notation (1.25e+2 => 125)
options(scipen = 99)  

# set system messages to english
Sys.setenv(lang = "en_US")

# set wd 
setwd("C:/Users/alexa/Documents/GitHub/covid")


## read in data frames & manipulate


dfeu <- read_excel("data/PROJ_19RP3__custom_13386841632821589108.xlsx", sheet = 3, range = "A11:CY413", col_types = c("guess", rep("numeric",102)))
dfeu <- dfeu[-1,]

# sum data in each pop. bracket 
dfeu$y0004 <- as.numeric(apply(dfeu[,4:8], 1, sum))
dfeu$y0514 <- as.numeric(apply(dfeu[,9:18], 1, sum))
dfeu$y1534 <- as.numeric(apply(dfeu[,19:38], 1, sum))
dfeu$y3559 <- as.numeric(apply(dfeu[,39:63], 1, sum))
dfeu$y6079 <- as.numeric(apply(dfeu[,64:83], 1, sum))
dfeu$y80 <- as.numeric(apply(dfeu[,84:103], 1, sum))

# formula for changing total cases to cases per eu standard population bracket (e.g. 0-4): 
  # cases per eu 0-4 = cases * (pop 0-4 eu) / (pop 0-4 county)
  # latter part is a factor that is calculated for each county and bracket below
  # (eu standard population as proposed by eurostat 2013)
dfeu$fy0004 <- 5000 / (dfeu$y0004) 
dfeu$fy0514 <- 11000 / (dfeu$y0514) 
dfeu$fy1534 <- 24000 / (dfeu$y1534) 
dfeu$fy3559 <- 34500 / (dfeu$y3559) 
dfeu$fy6079 <- 20500 / (dfeu$y6079) 
dfeu$fy80 <- 5000 / (dfeu$y80) 

dfeu2 <- dfeu[,c(1,2,110:115)]
dfeu2 <- rename(dfeu2, name = "AGE (Labels)", total = Total)








dfeu3 <- data.frame(t(dfeu2))
names(dfeu3) <- lapply(dfeu3[1, ], as.character)
dfeu3 <- dfeu3[-1,] 


