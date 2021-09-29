##########################################################
#                                                        #
# BA Sociology:                                          #
# "COVID-19 in Germany, a social-geographic perspective" #
#                                                        #
# 2021/04                                                #
#                                                        #
# Alexander Busch (alexander.busch@stud.uni-hd.de)       #
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

# tidyverse
library(dplyr)
library(ggplot2)
library(purrr)

# disable scientific notation (1.25e+2 => 125)
options(scipen = 99)  

# set system messages to english
Sys.setenv(lang = "en_US")

## read in data frames & manipulate
setwd("C:/Users/alexa/Documents/GitHub/covid")

## read in functions in order to perform aggregateTransform
source("functions.R")

## dfdd contains demographic/political/... data on county-level
dfdd <- read_excel("data/inkar.xls", col_types = c("guess", "guess", "guess", 
                                                   rep("numeric",11)))
dfdd <- dfdd[-1,]
names(dfdd) <- c("KRS", "countyName", "countyCity", 
                 "unemployment", "workersNoEdu", "workersAcadem", "shareWomen", 
                 "shareForeign", "AfD", "hhInc", "medInc", "hospBeds", "popPerDoc",
                 "popDensity")

# east-west dummy and state variables 
dfdd$east <- ifelse(
  str_detect(dfdd$KRS, "^12")|str_detect(dfdd$KRS, "^13")|
    str_detect(dfdd$KRS, "^14")|str_detect(dfdd$KRS, "^15")|
    str_detect(dfdd$KRS, "^16")|str_detect(dfdd$KRS, "^11")
  ,1,0)
dfdd$state <- 0
dfdd$state[str_detect(dfdd$KRS, "^01")] <- "SH"
dfdd$state[str_detect(dfdd$KRS, "^02")] <- "HH"
dfdd$state[str_detect(dfdd$KRS, "^03")] <- "NI"
dfdd$state[str_detect(dfdd$KRS, "^04")] <- "HB"
dfdd$state[str_detect(dfdd$KRS, "^13")] <- "MV"
dfdd$state[str_detect(dfdd$KRS, "^12")] <- "BB"
dfdd$state[str_detect(dfdd$KRS, "^11")] <- "BE"
dfdd$state[str_detect(dfdd$KRS, "^15")] <- "ST"
dfdd$state[str_detect(dfdd$KRS, "^14")] <- "SN"
dfdd$state[str_detect(dfdd$KRS, "^16")] <- "TH"
dfdd$state[str_detect(dfdd$KRS, "^05")] <- "NW"
dfdd$state[str_detect(dfdd$KRS, "^06")] <- "HE"
dfdd$state[str_detect(dfdd$KRS, "^07")] <- "RP"
dfdd$state[str_detect(dfdd$KRS, "^09")] <- "BY"
dfdd$state[str_detect(dfdd$KRS, "^10")] <- "SL"
dfdd$state[str_detect(dfdd$KRS, "^08")] <- "BW"

dfdd$SH <- ifelse(dfdd$state == "SH",1,0)
dfdd$HH <- ifelse(dfdd$state == "HH",1,0)
dfdd$NI <- ifelse(dfdd$state == "NI",1,0)
dfdd$HB <- ifelse(dfdd$state == "HB",1,0)
dfdd$MV <- ifelse(dfdd$state == "MV",1,0)
dfdd$BB <- ifelse(dfdd$state == "BB",1,0)
dfdd$BE <- ifelse(dfdd$state == "BE",1,0)
dfdd$ST <- ifelse(dfdd$state == "ST",1,0)
dfdd$SN <- ifelse(dfdd$state == "SN",1,0)
dfdd$TH <- ifelse(dfdd$state == "TH",1,0)
dfdd$NW <- ifelse(dfdd$state == "NW",1,0)
dfdd$HE <- ifelse(dfdd$state == "HE",1,0)
dfdd$RP <- ifelse(dfdd$state == "RP",1,0)
dfdd$BY <- ifelse(dfdd$state == "BY",1,0)
dfdd$SL <- ifelse(dfdd$state == "SL",1,0)

# replace 2 missing values in hospBeds by state averages
hospBedsMeans <- aggregate(dfdd[, "hospBeds"], list(dfdd$state), mean, na.rm = TRUE)
dfdd[288,"hospBeds"] <- hospBedsMeans[4,2]
dfdd[392,"hospBeds"] <- hospBedsMeans[16,2]


## dfgisd contains German Index of Social Deprivation by RKI
# 0 needs to be added to some KRS that start with 0 and were read in as integers
# two counties merged in 2016, thus their weighted avg GISD is calculated and the old county deleted
dfgisd <- read.csv("data/Kreis_2014.csv", colClasses = c("NULL", "character", "NULL", "numeric","numeric","NULL","NULL","NULL"), 
                   col.names = c("","KRS","","population","GISD","","",""))
dfgisd$KRS <- ifelse(str_length(dfgisd$KRS)==4,str_c("0", dfgisd$KRS, sep = "", collapse = NULL),dfgisd$KRS)
dfgisd[21,2] <- (dfgisd[21,2]*dfgisd[21,3]+dfgisd[25,2]*dfgisd[25,3])/(dfgisd[21,2]+dfgisd[25,2])
dfgisd[21,1] <- "03159"
dfgisd <- dfgisd[-c(25),]
dfgisd[,"population"] <- NULL


## dfeu2 contains population + factor to age-standardise (EU Standard Population 2013)
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
  # (eu standard population by eurostat 2013)
dfeu$fy0004 <- 5000 / (dfeu$y0004) 
dfeu$fy0514 <- 11000 / (dfeu$y0514) 
dfeu$fy1534 <- 24000 / (dfeu$y1534) 
dfeu$fy3559 <- 34500 / (dfeu$y3559) 
dfeu$fy6079 <- 20500 / (dfeu$y6079) 
dfeu$fy80 <- 5000 / (dfeu$y80) 

dfeu2 <- dfeu[,c(1,2,110:115)]
dfeu2 <- rename(dfeu2, name = "AGE (Labels)", pop = Total)


## dfds contains shapefile for German county-data, but also waterways to be dropped 
dfds <- st_read("data/shapefiles/250_NUTS3.shp")
dfds <- subset(dfds, select = c(3,4))
dfds <- dfds[-c(402:428),]


## dfmerge contains 2 referencing systems for counties used by previous data frames
dfmerge <- read_excel("Data/04_KreiseVorjahr.xlsx", sheet = 2, range = "A6:F478", 
                      col_types = c("guess", "skip", "guess", "guess", "skip", "numeric"))
names(dfmerge) <- c("KRS","name", "NUTS_CODE", "population")

# delete observations that are states, not counties, and other rows without data
dfmerge$KRSnew <- dfmerge$KRS
dfmerge$KRSnew <- as.numeric(as.character(dfmerge$KRSnew))
dfmerge <- subset(dfmerge, KRSnew > 999)
dfmerge$KRSnew <- NULL


## Combine county data
# Combine dfdd and dfgisd 
dfdd <- merge(dfdd, dfgisd, by.dfdd = KRS, by.dfgisd = KRS)

# Combine dfdd and dfmerge for data analysis 
dfdd <- merge(dfmerge, dfdd, by.dfmerge = KRS, by.dfdd = KRS)

# Combine dfdd and dfeu
dfdd <- dfdd[order(dfdd$countyName),]

# in order to make similarly-named counties appear before city-districts 
  # as is the order in dfdd, delete the marker "Landkreis" 
  # which will result in counties being named similarly as in dfdd, leading to 
  # the same alphabetical order once the df is sorted 
dfeu2$name <- str_replace_all(dfeu2$name, ", Landkreis", "")
dfeu2 <- dfeu2[order(dfeu2$name),]
dfdd <- cbind(dfdd,dfeu2)
dfdd <- dfdd[, c(1,3,2,5,36,6,4,37,7:35,38:43)]

## deleting all but one name variable and one pop var (as they are the same)
dfdd$name.1 <- NULL
dfdd$pop <- NULL
dfdd$countyName <- NULL


### Working with COVID-19 Data
## as file to big for github, it needs to be downloaded seperapte via
## https://npgeo-corona-npgeo-de.hub.arcgis.com/

# Reading in RKI COVID-19 data from Germany, reading in Reported Date as Date
dfrki <- read.csv("C:/Users/alexa/Documents/Uni/RKI-COVID/RKI_COVID19.csv",
                  colClasses=c(
                    "IdBundesland" = "NULL", "Datenstand" = "NULL", 
                    "Meldedatum" = "Date", "IdLandkreis" = "character"
                  ))

# How many people without known age? (Altersgruppe "unbekannt", age group "unknown")
count(dfrki, Altersgruppe)
  # > only 2103 unknown entries, in comparison to 2074321 known. 
  # > multiple cases in one entry possible, but extremly scarce 
  # > still, unknown age group is magnitudes below all other groups
  # > thus, unknowns are ignored
dfrki <- dfrki[!(dfrki$Altersgruppe=="unbekannt"),]

## Creating subsets for each month of the pandemic with IR, CFR, and EU standardised values
# some lists to set up for-loops and functions
rkilist1 <- c("2020-01-01", "2020-02-28", "2020-03-31", "2020-04-30", "2020-05-31", 
              "2020-06-30", "2020-07-31", "2020-08-31", "2020-09-30", "2020-10-31", 
              "2020-11-30", "2020-12-31", "2021-01-31", "2021-02-28", "2021-03-31", 
              "2021-04-30", "2021-05-31", "2021-06-30")
rkilist2 <- c("20.02", "20.03", "20.04", "20.05", "20.06", 
              "20.07", "20.08", "20.09", "20.10", "20.11",
              "20.12", "21.01", "21.02", "21.03", "21.04", 
              "21.05", "21.06", "")
rkilist3 <- c("", "0004", "0514", "1534", "3559", "6079", "80", "0004", "0514", "1534", "3559", "6079", "80")

# first, create subset for each month, 
# then apply aggregateTransform (see function.R) to calculate IR, IREU, CFR, CFREU
# (as this loops through >2M data points each iteration, performs transformations, etc., section takes some time)
for (i in 1:17) {
  a <- subset(dfrki, Meldedatum <= rkilist1[[i+1]] & Meldedatum > rkilist1[[i]])
  assign(paste("dfrki", rkilist2[[i]], sep=""), a)
}
rkilist4 <- list(dfrki20.02, dfrki20.03, dfrki20.04, dfrki20.05, dfrki20.06, 
                   dfrki20.07, dfrki20.08, dfrki20.09, dfrki20.10, dfrki20.11,
                   dfrki20.12, dfrki21.01, dfrki21.02, dfrki21.03, dfrki21.04, 
                   dfrki21.05, dfrki21.06, dfrki)
for (i in 1:18) {
  a <- aggregateTransform(a,rkilist4[[i]],rkilist2[[i]])
  assign(paste("dfrkiaggr", rkilist2[[i]], sep=""), a)
}

### Combining data 

listrkiaggr <- list(dfrkiaggr20.02,dfrkiaggr20.03,dfrkiaggr20.04,dfrkiaggr20.05,dfrkiaggr20.06,dfrkiaggr20.07,
                    dfrkiaggr20.08,dfrkiaggr20.09,dfrkiaggr20.10,dfrkiaggr20.11,dfrkiaggr20.12,
                    dfrkiaggr21.01,dfrkiaggr21.02,dfrkiaggr21.03,dfrkiaggr21.04,dfrkiaggr21.05,dfrkiaggr21.06)

for (a in listrkiaggr) {
  dfrkiaggr <- merge(dfrkiaggr, a, by = "KRS",all = TRUE)
}
dfrkiaggr[is.na(dfrkiaggr)] <- 0


# CFR with 1 month lag
dfrkiaggr$CFRlag20.04 <- (dfrkiaggr$deaths20.04 / dfrkiaggr$cases20.03)
dfrkiaggr$CFRlag20.05 <- (dfrkiaggr$deaths20.05 / dfrkiaggr$cases20.04)
dfrkiaggr$CFRlag20.06 <- (dfrkiaggr$deaths20.06 / dfrkiaggr$cases20.05)
dfrkiaggr$CFRlag20.07 <- (dfrkiaggr$deaths20.07 / dfrkiaggr$cases20.06)
dfrkiaggr$CFRlag20.08 <- (dfrkiaggr$deaths20.08 / dfrkiaggr$cases20.07)
dfrkiaggr$CFRlag20.09 <- (dfrkiaggr$deaths20.09 / dfrkiaggr$cases20.08)
dfrkiaggr$CFRlag20.10 <- (dfrkiaggr$deaths20.10 / dfrkiaggr$cases20.09)
dfrkiaggr$CFRlag20.11 <- (dfrkiaggr$deaths20.11 / dfrkiaggr$cases20.10)
dfrkiaggr$CFRlag20.12 <- (dfrkiaggr$deaths20.12 / dfrkiaggr$cases20.11)
dfrkiaggr$CFRlag21.01 <- (dfrkiaggr$deaths21.01 / dfrkiaggr$cases20.12)
dfrkiaggr$CFRlag21.02 <- (dfrkiaggr$deaths21.02 / dfrkiaggr$cases21.01)
dfrkiaggr$CFRlag21.03 <- (dfrkiaggr$deaths21.03 / dfrkiaggr$cases21.02)
dfrkiaggr$CFRlag21.04 <- (dfrkiaggr$deaths21.04 / dfrkiaggr$cases21.03)
dfrkiaggr$CFRlag21.05 <- (dfrkiaggr$deaths21.05 / dfrkiaggr$cases21.04)
dfrkiaggr$CFRlag21.06 <- (dfrkiaggr$deaths21.06 / dfrkiaggr$cases21.05)

dfrkiaggr$CFREUlag20.04 <- (dfrkiaggr$deathsEU20.04 / dfrkiaggr$IREU20.03)
dfrkiaggr$CFREUlag20.05 <- (dfrkiaggr$deathsEU20.05 / dfrkiaggr$IREU20.04)
dfrkiaggr$CFREUlag20.06 <- (dfrkiaggr$deathsEU20.06 / dfrkiaggr$IREU20.05)
dfrkiaggr$CFREUlag20.07 <- (dfrkiaggr$deathsEU20.07 / dfrkiaggr$IREU20.06)
dfrkiaggr$CFREUlag20.08 <- (dfrkiaggr$deathsEU20.08 / dfrkiaggr$IREU20.07)
dfrkiaggr$CFREUlag20.09 <- (dfrkiaggr$deathsEU20.09 / dfrkiaggr$IREU20.08)
dfrkiaggr$CFREUlag20.10 <- (dfrkiaggr$deathsEU20.10 / dfrkiaggr$IREU20.09)
dfrkiaggr$CFREUlag20.11 <- (dfrkiaggr$deathsEU20.11 / dfrkiaggr$IREU20.10)
dfrkiaggr$CFREUlag20.12 <- (dfrkiaggr$deathsEU20.12 / dfrkiaggr$IREU20.11)
dfrkiaggr$CFREUlag21.01 <- (dfrkiaggr$deathsEU21.01 / dfrkiaggr$IREU20.12)
dfrkiaggr$CFREUlag21.02 <- (dfrkiaggr$deathsEU21.02 / dfrkiaggr$IREU21.01)
dfrkiaggr$CFREUlag21.03 <- (dfrkiaggr$deathsEU21.03 / dfrkiaggr$IREU21.02)
dfrkiaggr$CFREUlag21.04 <- (dfrkiaggr$deathsEU21.04 / dfrkiaggr$IREU21.03)
dfrkiaggr$CFREUlag21.05 <- (dfrkiaggr$deathsEU21.05 / dfrkiaggr$IREU21.04)
dfrkiaggr$CFREUlag21.06 <- (dfrkiaggr$deathsEU21.06 / dfrkiaggr$IREU21.05)


# recode undefined (x/0) CFR to 0 as described in Thesis
dfrkiaggr[is.na(dfrkiaggr)] <- 0
dfrkiaggr[dfrkiaggr == Inf] <- 0

## Combinig aggr data and spatial / geographic data
dfdd <- merge(dfdd, dfrkiaggr, by.dfdd = KRS, by.dfrkiaggr = KRS)

# ln
dfdd$LNIR20.02 <- log(dfdd$IR20.02)
dfdd$LNIR20.03 <- log(dfdd$IR20.03)
dfdd$LNIR20.04 <- log(dfdd$IR20.04)
dfdd$LNIR20.05 <- log(dfdd$IR20.05)
dfdd$LNIR20.06 <- log(dfdd$IR20.06)
dfdd$LNIR20.07 <- log(dfdd$IR20.07)
dfdd$LNIR20.08 <- log(dfdd$IR20.08)
dfdd$LNIR20.09 <- log(dfdd$IR20.09)
dfdd$LNIR20.10 <- log(dfdd$IR20.10)
dfdd$LNIR20.11 <- log(dfdd$IR20.11)
dfdd$LNIR20.12 <- log(dfdd$IR20.12)
dfdd$LNIR21.01 <- log(dfdd$IR21.01)
dfdd$LNIR21.02 <- log(dfdd$IR21.02)
dfdd$LNIR21.03 <- log(dfdd$IR21.03)
dfdd$LNIR21.04 <- log(dfdd$IR21.04)
dfdd$LNIR21.05 <- log(dfdd$IR21.05)
dfdd$LNIR21.06 <- log(dfdd$IR21.06)
dfdd$LNIR <- log(dfdd$IR)

dfdd$LNIREU20.02 <- log(dfdd$IREU20.02)
dfdd$LNIREU20.03 <- log(dfdd$IREU20.03)
dfdd$LNIREU20.04 <- log(dfdd$IREU20.04)
dfdd$LNIREU20.05 <- log(dfdd$IREU20.05)
dfdd$LNIREU20.06 <- log(dfdd$IREU20.06)
dfdd$LNIREU20.07 <- log(dfdd$IREU20.07)
dfdd$LNIREU20.08 <- log(dfdd$IREU20.08)
dfdd$LNIREU20.09 <- log(dfdd$IREU20.09)
dfdd$LNIREU20.10 <- log(dfdd$IREU20.10)
dfdd$LNIREU20.11 <- log(dfdd$IREU20.11)
dfdd$LNIREU20.12 <- log(dfdd$IREU20.12)
dfdd$LNIREU21.01 <- log(dfdd$IREU21.01)
dfdd$LNIREU21.02 <- log(dfdd$IREU21.02)
dfdd$LNIREU21.03 <- log(dfdd$IREU21.03)
dfdd$LNIREU21.04 <- log(dfdd$IREU21.04)
dfdd$LNIREU21.05 <- log(dfdd$IREU21.05)
dfdd$LNIREU21.06 <- log(dfdd$IREU21.06)
dfdd$LNIREU <- log(dfdd$IREU)

dfdd$LNmedInc <- log(dfdd$medInc)
dfdd$LNpopPerDoc <- log(dfdd$popPerDoc)
dfdd$GISDforeign <- dfdd$GISD * dfdd$shareForeign
dfdd$GISDeast <- dfdd$GISD * dfdd$east

# to prevent 0s to become neg. infinite (by ln(0)), replace those with -2.3 (incedence of 1 per 1,000,000)
dfdd[dfdd < -10000] <- -2.3

# Combine dfds and dfdd for mapping / SAR Modeling
dfds <- merge(dfds, dfdd, by.dfds = NUTS_CODE, by.dfdd = NUTS_CODE)

dfds$NUTS_NAME <- NULL
dfds$name <- NULL
dfdd$name <- NULL
dfdd[,34:39] <- NULL # EU weights
dfds[,34:39] <- NULL # EU weights


