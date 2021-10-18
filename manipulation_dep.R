##########################################################
#                                                        #
#                                                        #
# 2021/10                                                #
#                                                        #
# Alexander Busch (a.busch@lse.ac.uk)                    #
#                                                        #
# Data Manipulation Dependent Variables                  #
#                                                        #
##########################################################

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
# first aggregating cases and deaths by age group
for (i in 1:18) {
  a <- ddply(rkilist4[[i]], .(IdLandkreis, Altersgruppe), summarise, cases=sum(AnzahlFall))
  a <- reshape(a, idvar = "IdLandkreis", timevar = "Altersgruppe", direction = "wide")
  dfdeaths <- ddply(rkilist4[[i]], .(IdLandkreis, Altersgruppe), summarise, deaths=sum(AnzahlTodesfall))
  dfdeaths <- reshape(dfdeaths, idvar = "IdLandkreis", timevar = "Altersgruppe", direction = "wide")
  a <- merge(a, dfdeaths, by.dfmerge = IdLandkreis, by.dfdd = IdLandkreis)
  names(a)[names(a)=="IdLandkreis"] <- "KRS"
  namescol <- colnames(a)
  a[is.na(a)] <- 0
  assign(paste("dfrkiaggr", rkilist2[[i]], sep=""), a)
}
rkilist5 <- list(dfrkiaggr20.02, dfrkiaggr20.03, dfrkiaggr20.04, dfrkiaggr20.05, dfrkiaggr20.06, 
                 dfrkiaggr20.07, dfrkiaggr20.08, dfrkiaggr20.09, dfrkiaggr20.10, dfrkiaggr20.11,
                 dfrkiaggr20.12, dfrkiaggr21.01, dfrkiaggr21.02, dfrkiaggr21.03, dfrkiaggr21.04, 
                 dfrkiaggr21.05, dfrkiaggr21.06, dfrkiaggr)



# create df with all KRS reference numbers
KRS <- dfrkiaggr20.03[,"KRS"]
dfKRS <- data.frame(KRS)
dfKRS$KRS <- as.character(dfKRS$KRS)

# creating df with aggregate Berlin cases/deaths with correct geographic references
for (i in 1:18){
  a <- rkilist5[[i]]
  a <- merge(dfKRS, a, by.dfKRS = KRS, by.a = KRS, all = TRUE)
  a <- a[, c("KRS", sort(setdiff(names(a), "KRS")))]
  dfberlin <- subset(a, KRS == "11001"|KRS =="11002"|KRS =="11003"|KRS =="11004"|KRS =="11005"|KRS =="11006"|
                       KRS =="11007"|KRS =="11008"|KRS =="11009"|
                       KRS =="11010"|KRS =="11011"|KRS =="11012")
  dfberlin2 <- data.frame(KRS = "11000")
  for(j in 2:13){
    b <- sum(dfberlin[,j])
    dfberlin2 <- cbind(b,dfberlin2)
  }
  dfberlin2 <- setNames(rev(dfberlin2), names(a))
  a <- rbind(a, dfberlin2)
  a <- subset(a, KRS != "11001"&KRS !="11002"&KRS !="11003"&KRS !="11004"&KRS !="11005"&KRS !="11006"&
                KRS !="11007"&KRS !="11008"&KRS !="11009"&
                KRS !="11010"&KRS !="11011"&KRS !="11012")
  a[is.na(a)] <- 0
  a <- a[order(a$KRS),]
  assign(paste("dfrkiaggr", rkilist2[[i]], sep=""), a)
}
rkilist5 <- list(dfrkiaggr20.02, dfrkiaggr20.03, dfrkiaggr20.04, dfrkiaggr20.05, dfrkiaggr20.06, 
                 dfrkiaggr20.07, dfrkiaggr20.08, dfrkiaggr20.09, dfrkiaggr20.10, dfrkiaggr20.11,
                 dfrkiaggr20.12, dfrkiaggr21.01, dfrkiaggr21.02, dfrkiaggr21.03, dfrkiaggr21.04, 
                 dfrkiaggr21.05, dfrkiaggr21.06, dfrkiaggr)

# giving names in order to later match
namescol <- colnames(dfrkiaggr)
for (i in 1:18){
  a <- rkilist5[[i]]
  for(j in 2:13){
    ifelse(str_detect(namescol[[j]], "^c"), 
           names(a)[names(a)==namescol[[j]]] <- paste("cases", rkilist3[[j]], sep=""),
           names(a)[names(a)==namescol[[j]]] <- paste("deaths", rkilist3[[j]], sep="")
    ) 
  }
  assign(paste("dfrkiaggr", rkilist2[[i]], sep=""), a)
}
rkilist5 <- list(dfrkiaggr20.02, dfrkiaggr20.03, dfrkiaggr20.04, dfrkiaggr20.05, dfrkiaggr20.06, 
                 dfrkiaggr20.07, dfrkiaggr20.08, dfrkiaggr20.09, dfrkiaggr20.10, dfrkiaggr20.11,
                 dfrkiaggr20.12, dfrkiaggr21.01, dfrkiaggr21.02, dfrkiaggr21.03, dfrkiaggr21.04, 
                 dfrkiaggr21.05, dfrkiaggr21.06, dfrkiaggr)



### !!! Some IR Are falsely calculated (e.g. Hamburg, Berlin despite pop seemingly correct)
### sort statement above seems correct order... seems to be only a problem for Hamburg & Berlin


dfpop <- dfdd[, c("KRS", "population", "fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")]

# calculating sums: one crude sum, one standardised EU sum, IR and CFR both with and without EU weights
for (i in 1:18){
  a <- rkilist5[[i]]
  a$cases <- as.numeric(apply(a[,2:7], 1, sum))
  a$deaths <- as.numeric(apply(a[,8:13], 1, sum))
  # reading in pop and EU pop variables from indep. vars
#    a$population <- dfdd$population
#    a$fy0004 <- dfdd$fy0004
#    a$fy0514 <- dfdd$fy0514
#    a$fy1534 <- dfdd$fy1534
#    a$fy3559 <- dfdd$fy3559
#    a$fy6079 <- dfdd$fy6079
#    a$fy80 <- dfdd$fy80
  a <- merge(a, dfpop, by.a = KRS, by.dfpop = KRS, all = TRUE)
  a$IR <- (a$cases / a$population) * 100000
  a$IREU <- a$cases0004 * a$fy0004 + a$cases0514 * a$fy0514 +
    a$cases1534 * a$fy1534 + a$cases3559 * a$fy3559 +
    a$cases6079 * a$fy6079 + a$cases80 * a$fy80
  a$CFR <- (a$deaths / a$cases)
  a$deathsEU <- a$deaths0004 * a$fy0004 + a$deaths0514 * a$fy0514 +
    a$deaths1534 * a$fy1534 + a$deaths3559 * a$fy3559 +
    a$deaths6079 * a$fy6079 + a$deaths80 * a$fy80
  a$CFREU <- (a$deathsEU / a$IREU)
  colnames(a) <- paste(colnames(a), rkilist2[[i]], sep = "")
  namescol <- colnames(a)
  names(a)[names(a)==namescol[[1]]] <- "KRS"
  a <- a[,c(1,14,15,23:27)]
  assign(paste("dfrkiaggr", rkilist2[[i]], sep=""), a)
}
























