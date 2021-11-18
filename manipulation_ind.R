##########################################################
#                                                        #
#                                                        #
# 2021/10                                                #
#                                                        #
# Alexander Busch (a.busch@lse.ac.uk)                    #
#                                                        #
# Data Manipulation Independent Variables                #
#                                                        #
##########################################################


## dfdd contains demographic/political/... data on county-level
dfdd <- read_excel("data/inkar.xls", col_types = c("guess", "guess", "guess", 
                                                   rep("numeric",15)))
dfdd <- dfdd[-1,]
names(dfdd) <- c("KRS", "countyName", "countyCity", 
                 "unemployment",  "workersAcadem","workersNoEdu", "hhInc", "debtQuota", 
                 "grossInc", "businessTax", "employment", "shareWomen", 
                 "shareForeign", "AfD", "medInc", "hospBeds", "popPerDoc",
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


## dfmerge contains 2 referencing systems for counties used by previous data frames
dfmerge <- read_excel("Data/04_KreiseVorjahr.xlsx", sheet = 2, range = "A6:F478", 
                      col_types = c("guess", "skip", "guess", "guess", "skip", "skip"))
names(dfmerge) <- c("KRS","name", "NUTS_CODE")
# delete observations that are states, not counties, and other rows without data
dfmerge$KRSnew <- dfmerge$KRS
dfmerge$KRSnew <- as.numeric(as.character(dfmerge$KRSnew))
dfmerge <- subset(dfmerge, KRSnew > 999)
dfmerge$KRSnew <- NULL
# Combine dfdd and dfmerge for data analysis 
dfdd <- merge(dfmerge, dfdd, by = "KRS")


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
# Combine dfdd and dfgisd 
dfdd <- merge(dfdd, dfgisd, by = "KRS")


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
# dfeu2 with relevant columns
dfeu2 <- dfeu[,c(1,2,110:115)]
dfeu2 <- rename(dfeu2, nameEU = "AGE (Labels)", population = Total)
# in order to make similarly-named counties appear before city-districts 
# as is the order in dfdd, delete the marker "Landkreis" 
# which will result in counties being named similarly as in dfdd, leading to 
# the same alphabetical order once the df is sorted 
dfdd <- dfdd[order(dfdd$countyName),]
dfeu2$nameEU <- str_replace_all(dfeu2$nameEU, ", Landkreis", "")
dfeu2 <- dfeu2[order(dfeu2$nameEU),]
dfdd <- cbind(dfdd,dfeu2)
dfdd$countyName <- NULL
dfdd$nameEU <- NULL
dfpop <- dfdd[,c("KRS","population")]


## Data for Religion
# some county-merges need to be accounted for
# share of people of certain faiths computed
dfrel <- read_excel("data/data-religion.xlsx", sheet = 3, range = "A11:DS12553", 
                    col_types = c("text", rep("skip",5), "text", rep("skip",112),rep("numeric",4)))
names(dfrel) <- c("KRS", "name", "zenPop", "catholic", "protestant","other")
dfrel <- subset(dfrel, str_length(KRS) == 5)

dfrel[21,3:6] <- dfrel[21,3:6] + dfrel[25,3:6]
dfrel[362,3:6] <- dfrel[362,3:6] + dfrel[359,3:6] + dfrel[345,3:6]
dfrel[361,3:6] <- dfrel[361,3:6] + dfrel[349,3:6] + dfrel[357,3:6]
dfrel[354,3:6] <- dfrel[354,3:6] + dfrel[360,3:6]
dfrel[352,3:6] <- dfrel[352,3:6] + dfrel[356,3:6] + dfrel[355,3:6] + dfrel[346,3:6]
dfrel[351,3:6] <- dfrel[351,3:6] + dfrel[353,3:6]
dfrel[358,3:6] <- dfrel[358,3:6] + dfrel[350,3:6]

dfrel[21,1] <- "03159"
dfrel[362,1] <- "13075"
dfrel[361,1] <- "13073"
dfrel[354,1] <- "13076"
dfrel[352,1] <- "13071"
dfrel[351,1] <- "13072"
dfrel[358,1] <- "13074"

dfrel <- dfrel[-c(359,345,349,357,360,356,355,346,353,350,25),]
# converting to share of population (zenPop used as measure is from 2011)
dfrel$relCath <- 100 * dfrel$catholic / dfrel$zenPop
dfrel$relProt <- 100 * dfrel$protestant / dfrel$zenPop
dfrel$relOth <- 100 * dfrel$other / dfrel$zenPop

dfrel <- dfrel[,-c(2:6)]
# Combine dfdd and dfrel 
dfdd <- merge(dfdd, dfrel, by = "KRS")


## dfvacc2 contains vaccines per population on German County scale
# read in data
dfvacc <- read.csv("data/rki_vacc.csv", 
                     colClasses = c("character", "character", "character","numeric","numeric"), 
                   col.names = c("date","KRS","age","vaccNum","amount"))
# delete entries after June 2021
# dfvacc <- subset(dfvacc, date <= "2021-06-30")
# delete entries without valid KRS (43 in June 2021) or with KRS 17000 (vaccinated by the federal gov, ca 250k)
dfvacc <- subset(dfvacc, KRS != "17000")
# create subsets of each month
dfvacc$month <- ifelse(dfvacc$date >= "2021-10-01", 
                       "21-10", 
                ifelse(dfvacc$date >= "2021-09-01", 
                       "21-09", 
                ifelse(dfvacc$date >= "2021-08-01", 
                       "21-08", 
                ifelse(dfvacc$date >= "2021-07-01", 
                       "21-07", 
                ifelse(dfvacc$date >= "2021-06-01", 
                       "21-06", 
                ifelse(dfvacc$date >= "2021-05-01", 
                       "21-05", 
                ifelse(dfvacc$date >= "2021-04-01", 
                       "21-04", 
                ifelse(dfvacc$date >= "2021-03-01", 
                       "21-03", 
                ifelse(dfvacc$date >= "2021-02-01", 
                       "21-02", 
                ifelse(dfvacc$date >= "2021-01-01", 
                       "21-01", 
                ifelse(dfvacc$date >= "2020-12-01", 
                       "20-12",                
                )))))))))))
# aggregating vaccines for each county & month
dfvacc2 <- ddply(dfvacc, .(KRS, month), summarise, vacc=sum(amount))
dfvacc2 <- reshape(dfvacc2, idvar = "KRS", timevar = "month", direction = "wide")
# renaming
colnames(dfvacc2) <- c("KRS","vacc20.12","vacc21.01","vacc21.02","vacc21.03","vacc21.04","vacc21.05","vacc21.06",
                       "vacc21.07","vacc21.08","vacc21.09","vacc21.10") 
# create zeros for non-included KRS
dfvacc2[is.na(dfvacc2)] <- 0
# cummulate vaccines and transform to relative measure of population (share vaccinated)
dfvacc2 <- merge(dfpop,dfvacc2,by="KRS")
dfvacc2$VR20.12 <- (dfvacc2$vacc20.12) / dfvacc2$population
dfvacc2$VR21.01 <- (dfvacc2$vacc21.01) / dfvacc2$population + dfvacc2$VR20.12
dfvacc2$VR21.02 <- (dfvacc2$vacc21.02) / dfvacc2$population + dfvacc2$VR21.01
dfvacc2$VR21.03 <- (dfvacc2$vacc21.03) / dfvacc2$population + dfvacc2$VR21.02
dfvacc2$VR21.04 <- (dfvacc2$vacc21.04) / dfvacc2$population + dfvacc2$VR21.03
dfvacc2$VR21.05 <- (dfvacc2$vacc21.05) / dfvacc2$population + dfvacc2$VR21.04
dfvacc2$VR21.06 <- (dfvacc2$vacc21.06) / dfvacc2$population + dfvacc2$VR21.05
dfvacc2$VR21.07 <- (dfvacc2$vacc21.07) / dfvacc2$population + dfvacc2$VR21.06
dfvacc2$VR21.08 <- (dfvacc2$vacc21.08) / dfvacc2$population + dfvacc2$VR21.07
dfvacc2$VR21.09 <- (dfvacc2$vacc21.09) / dfvacc2$population + dfvacc2$VR21.08
dfvacc2$VR21.10 <- (dfvacc2$vacc21.10) / dfvacc2$population + dfvacc2$VR21.09
dfvacc2$population <- NULL
# Combine dfdd and dfvacc2 
dfdd <- merge(dfdd, dfvacc2, by = "KRS")


## dfds contains shapefile for German county-data, but also waterways to be dropped 
dfds <- st_read("data/shapefiles/250_NUTS3.shp")
dfds <- subset(dfds, select = c(3,4))
dfds <- dfds[-c(402:428),]




