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
count(dfrki, Geschlecht)

# > only 2103 unknown entries in age, in comparison to 2074321 known. 
# > only 16664 unknown entries in gender, in comparison to over 2000000 known
# > multiple cases in one entry possible, but extremly scarce 
# > still, unknown age group is magnitudes below all other groups
# > thus, unknowns are ignored
dfrki <- dfrki[!(dfrki$Altersgruppe=="unbekannt"),]
dfrki <- dfrki[!(dfrki$Geschlecht=="unbekannt"),]


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
# (as this loops through >2M data points each iteration, performs transformations, etc., section takes some time)
for (i in 1:17) {
  a <- subset(dfrki, Meldedatum <= rkilist1[[i+1]] & Meldedatum > rkilist1[[i]])
  assign(paste("dfrki", rkilist2[[i]], sep=""), a)
}
rkilist4 <- list(dfrki20.02, dfrki20.03, dfrki20.04, dfrki20.05, dfrki20.06, 
                 dfrki20.07, dfrki20.08, dfrki20.09, dfrki20.10, dfrki20.11,
                 dfrki20.12, dfrki21.01, dfrki21.02, dfrki21.03, dfrki21.04, 
                 dfrki21.05, dfrki21.06, dfrki)

# first aggregating cases and deaths by age group (one time for all, once for each gender)
for (i in 1:18) {
  rki <- rkilist4[[i]]
  # create general df with sums of cases and deaths
  rkiCases <- ddply(rki, .(IdLandkreis, Altersgruppe, Geschlecht), summarise, cases=sum(AnzahlFall))
  rkiDeaths <- ddply(rki, .(IdLandkreis, Altersgruppe, Geschlecht), summarise, deaths=sum(AnzahlTodesfall))
  # sum over all genders: cases and deaths
  ac <- ddply(rkiCases, .(IdLandkreis, Altersgruppe), summarise, cases=sum(cases))
  ac <- reshape(ac, idvar = c("IdLandkreis"), timevar = "Altersgruppe", direction = "wide")
  ad <- ddply(rkiDeaths, .(IdLandkreis, Altersgruppe), summarise, deaths=sum(deaths))
  ad <- reshape(ad, idvar = c("IdLandkreis"), timevar = "Altersgruppe", direction = "wide")
  a <- merge(ac, ad, by.ac = IdLandkreis, by.ad = IdLandkreis)
  names(a)[names(a)=="IdLandkreis"] <- "KRS"
  a[is.na(a)] <- 0
  # sum over female
  fc <- subset(rkiCases, !(Geschlecht=="M"))
  fd <- subset(rkiDeaths, !(Geschlecht=="M"))
  fc <- ddply(fc, .(IdLandkreis, Altersgruppe), summarise, cases=sum(cases))
  fc <- reshape(fc, idvar = c("IdLandkreis"), timevar = "Altersgruppe", direction = "wide")
  fd <- ddply(fd, .(IdLandkreis, Altersgruppe), summarise, deaths=sum(deaths))
  fd <- reshape(fd, idvar = c("IdLandkreis"), timevar = "Altersgruppe", direction = "wide")
  f <- merge(fc, fd, by.fc = IdLandkreis, by.fd = IdLandkreis)
  names(f)[names(f)=="IdLandkreis"] <- "KRS"
  f[is.na(f)] <- 0
  # sum over male
  mc <- subset(rkiCases, Geschlecht=="M")
  md <- subset(rkiDeaths, Geschlecht=="M")
  mc <- ddply(mc, .(IdLandkreis, Altersgruppe), summarise, cases=sum(cases))
  mc <- reshape(mc, idvar = c("IdLandkreis"), timevar = "Altersgruppe", direction = "wide")
  md <- ddply(md, .(IdLandkreis, Altersgruppe), summarise, deaths=sum(deaths))
  md <- reshape(md, idvar = c("IdLandkreis"), timevar = "Altersgruppe", direction = "wide")
  m <- merge(mc, md, by.mc = IdLandkreis, by.md = IdLandkreis)
  names(m)[names(m)=="IdLandkreis"] <- "KRS"
  m[is.na(m)] <- 0
  # save data frames
  assign(paste("dfrkiaggrf", rkilist2[[i]], sep=""), f)
  assign(paste("dfrkiaggrm", rkilist2[[i]], sep=""), m)
  assign(paste("dfrkiaggr", rkilist2[[i]], sep=""), a)
}
# no observations for two age groups in dfrkiaggrm21.06, need to add zero columns
dfrkiaggrm20.02$"cases.A05-A14" <- 0
dfrkiaggrm20.02$"deaths.A05-A14" <- 0
namescol <- colnames(dfrkiaggr)
dfrkiaggrm20.02 <- dfrkiaggrm20.02[,namescol]

rkilist5 <- list(dfrkiaggr20.02, dfrkiaggr20.03, dfrkiaggr20.04, dfrkiaggr20.05, dfrkiaggr20.06, 
                 dfrkiaggr20.07, dfrkiaggr20.08, dfrkiaggr20.09, dfrkiaggr20.10, dfrkiaggr20.11,
                 dfrkiaggr20.12, dfrkiaggr21.01, dfrkiaggr21.02, dfrkiaggr21.03, dfrkiaggr21.04, 
                 dfrkiaggr21.05, dfrkiaggr21.06, dfrkiaggr, 
                 dfrkiaggrf20.02, dfrkiaggrf20.03, dfrkiaggrf20.04, dfrkiaggrf20.05, dfrkiaggrf20.06, 
                 dfrkiaggrf20.07, dfrkiaggrf20.08, dfrkiaggrf20.09, dfrkiaggrf20.10, dfrkiaggrf20.11,
                 dfrkiaggrf20.12, dfrkiaggrf21.01, dfrkiaggrf21.02, dfrkiaggrf21.03, dfrkiaggrf21.04, 
                 dfrkiaggrf21.05, dfrkiaggrf21.06, dfrkiaggrf, 
                 dfrkiaggrm20.02, dfrkiaggrm20.03, dfrkiaggrm20.04, dfrkiaggrm20.05, dfrkiaggrm20.06, 
                 dfrkiaggrm20.07, dfrkiaggrm20.08, dfrkiaggrm20.09, dfrkiaggrm20.10, dfrkiaggrm20.11,
                 dfrkiaggrm20.12, dfrkiaggrm21.01, dfrkiaggrm21.02, dfrkiaggrm21.03, dfrkiaggrm21.04, 
                 dfrkiaggrm21.05, dfrkiaggrm21.06, dfrkiaggrm)

# create df with all KRS reference numbers
KRS <- dfrkiaggr20.03[,"KRS"]
dfKRS <- data.frame(KRS)
dfKRS$KRS <- as.character(dfKRS$KRS)

## Berlin units are not on county level, but district level > Needs to be summarised
# creating df with aggregate Berlin cases/deaths with correct geographic references
for (i in 1:54){
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
  ifelse(i<=18, assign(paste("dfrkiaggr", rkilist2[[i]], sep=""), a), 
         ifelse(i<=36, assign(paste("dfrkiaggrf", rkilist2[[i-18]], sep=""), a), 
                assign(paste("dfrkiaggrm", rkilist2[[i-36]], sep=""), a))
    )
}
rkilist5 <- list(dfrkiaggr20.02, dfrkiaggr20.03, dfrkiaggr20.04, dfrkiaggr20.05, dfrkiaggr20.06, 
                 dfrkiaggr20.07, dfrkiaggr20.08, dfrkiaggr20.09, dfrkiaggr20.10, dfrkiaggr20.11,
                 dfrkiaggr20.12, dfrkiaggr21.01, dfrkiaggr21.02, dfrkiaggr21.03, dfrkiaggr21.04, 
                 dfrkiaggr21.05, dfrkiaggr21.06, dfrkiaggr, 
                 dfrkiaggrf20.02, dfrkiaggrf20.03, dfrkiaggrf20.04, dfrkiaggrf20.05, dfrkiaggrf20.06, 
                 dfrkiaggrf20.07, dfrkiaggrf20.08, dfrkiaggrf20.09, dfrkiaggrf20.10, dfrkiaggrf20.11,
                 dfrkiaggrf20.12, dfrkiaggrf21.01, dfrkiaggrf21.02, dfrkiaggrf21.03, dfrkiaggrf21.04, 
                 dfrkiaggrf21.05, dfrkiaggrf21.06, dfrkiaggrf, 
                 dfrkiaggrm20.02, dfrkiaggrm20.03, dfrkiaggrm20.04, dfrkiaggrm20.05, dfrkiaggrm20.06, 
                 dfrkiaggrm20.07, dfrkiaggrm20.08, dfrkiaggrm20.09, dfrkiaggrm20.10, dfrkiaggrm20.11,
                 dfrkiaggrm20.12, dfrkiaggrm21.01, dfrkiaggrm21.02, dfrkiaggrm21.03, dfrkiaggrm21.04, 
                 dfrkiaggrm21.05, dfrkiaggrm21.06, dfrkiaggrm)

# giving names in order to later match
namescol <- colnames(dfrkiaggr)
for (i in 1:54){
  a <- rkilist5[[i]]
  for(j in 2:13){
    ifelse(str_detect(namescol[[j]], "^c"), 
           names(a)[names(a)==namescol[[j]]] <- paste("cases", rkilist3[[j]], sep=""),
           names(a)[names(a)==namescol[[j]]] <- paste("deaths", rkilist3[[j]], sep="")
    ) 
  }
  a <- a[order(a$KRS),]
  ifelse(i<=18, assign(paste("dfrkiaggr", rkilist2[[i]], sep=""), a), 
         ifelse(i<=36, assign(paste("dfrkiaggrf", rkilist2[[i-18]], sep=""), a), 
                assign(paste("dfrkiaggrm", rkilist2[[i-36]], sep=""), a))
  )
}
rkilist5 <- list(dfrkiaggr20.02, dfrkiaggr20.03, dfrkiaggr20.04, dfrkiaggr20.05, dfrkiaggr20.06, 
                 dfrkiaggr20.07, dfrkiaggr20.08, dfrkiaggr20.09, dfrkiaggr20.10, dfrkiaggr20.11,
                 dfrkiaggr20.12, dfrkiaggr21.01, dfrkiaggr21.02, dfrkiaggr21.03, dfrkiaggr21.04, 
                 dfrkiaggr21.05, dfrkiaggr21.06, dfrkiaggr, 
                 dfrkiaggrf20.02, dfrkiaggrf20.03, dfrkiaggrf20.04, dfrkiaggrf20.05, dfrkiaggrf20.06, 
                 dfrkiaggrf20.07, dfrkiaggrf20.08, dfrkiaggrf20.09, dfrkiaggrf20.10, dfrkiaggrf20.11,
                 dfrkiaggrf20.12, dfrkiaggrf21.01, dfrkiaggrf21.02, dfrkiaggrf21.03, dfrkiaggrf21.04, 
                 dfrkiaggrf21.05, dfrkiaggrf21.06, dfrkiaggrf, 
                 dfrkiaggrm20.02, dfrkiaggrm20.03, dfrkiaggrm20.04, dfrkiaggrm20.05, dfrkiaggrm20.06, 
                 dfrkiaggrm20.07, dfrkiaggrm20.08, dfrkiaggrm20.09, dfrkiaggrm20.10, dfrkiaggrm20.11,
                 dfrkiaggrm20.12, dfrkiaggrm21.01, dfrkiaggrm21.02, dfrkiaggrm21.03, dfrkiaggrm21.04, 
                 dfrkiaggrm21.05, dfrkiaggrm21.06, dfrkiaggrm)

dfpop <- dfdd[, c("KRS", "population", "fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80", "shareWomen")]

# calculating sums: one crude sum, one standardised EU sum, IR and CFR both with and without EU weights
# assumption: EU age brackets same across genders in counties (no data on m/f per county & age bracket)
for (i in 1:54){
  a <- rkilist5[[i]]
  a$cases <- as.numeric(apply(a[,2:7], 1, sum))
  a$deaths <- as.numeric(apply(a[,8:13], 1, sum))
  # reading in pop and EU pop variables from indep. vars
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
  ifelse(i<=18, colnames(a) <- paste(colnames(a), rkilist2[[i]], sep = ""), 
         ifelse(i<=36, colnames(a) <- paste(colnames(a), rkilist2[[i-18]], sep = ""), 
                colnames(a) <- paste(colnames(a), rkilist2[[i-36]], sep = ""))
         )
  namescol <- colnames(a)
  names(a)[names(a)==namescol[[1]]] <- "KRS"
  a <- a[,c(1,14,15,23:27)]
  ifelse(i<=18, assign(paste("dfrkiaggr", rkilist2[[i]], sep=""), a), 
         ifelse(i<=36, assign(paste("dfrkiaggrf", rkilist2[[i-18]], sep=""), a), 
                assign(paste("dfrkiaggrm", rkilist2[[i-36]], sep=""), a))
  )
}

rkilist5 <- list(dfrkiaggr20.02, dfrkiaggr20.03, dfrkiaggr20.04, dfrkiaggr20.05, dfrkiaggr20.06, 
                 dfrkiaggr20.07, dfrkiaggr20.08, dfrkiaggr20.09, dfrkiaggr20.10, dfrkiaggr20.11,
                 dfrkiaggr20.12, dfrkiaggr21.01, dfrkiaggr21.02, dfrkiaggr21.03, dfrkiaggr21.04, 
                 dfrkiaggr21.05, dfrkiaggr21.06, dfrkiaggr, 
                 dfrkiaggrf20.02, dfrkiaggrf20.03, dfrkiaggrf20.04, dfrkiaggrf20.05, dfrkiaggrf20.06, 
                 dfrkiaggrf20.07, dfrkiaggrf20.08, dfrkiaggrf20.09, dfrkiaggrf20.10, dfrkiaggrf20.11,
                 dfrkiaggrf20.12, dfrkiaggrf21.01, dfrkiaggrf21.02, dfrkiaggrf21.03, dfrkiaggrf21.04, 
                 dfrkiaggrf21.05, dfrkiaggrf21.06, dfrkiaggrf, 
                 dfrkiaggrm20.02, dfrkiaggrm20.03, dfrkiaggrm20.04, dfrkiaggrm20.05, dfrkiaggrm20.06, 
                 dfrkiaggrm20.07, dfrkiaggrm20.08, dfrkiaggrm20.09, dfrkiaggrm20.10, dfrkiaggrm20.11,
                 dfrkiaggrm20.12, dfrkiaggrm21.01, dfrkiaggrm21.02, dfrkiaggrm21.03, dfrkiaggrm21.04, 
                 dfrkiaggrm21.05, dfrkiaggrm21.06, dfrkiaggrm)

# merge aggregated monthly data sets to one 
for(i in 1:17) {
  dfrkiaggr <- merge(dfrkiaggr, rkilist5[i], by = "KRS",all = TRUE)
}
dfrkiaggr[is.na(dfrkiaggr)] <- 0
for(i in 19:35) {
  dfrkiaggrf <- merge(dfrkiaggrf, rkilist5[i], by = "KRS",all = TRUE)
}
dfrkiaggrf[is.na(dfrkiaggrf)] <- 0
for(i in 37:53) {
  dfrkiaggrm <- merge(dfrkiaggrm, rkilist5[i], by = "KRS",all = TRUE)
}
dfrkiaggrm[is.na(dfrkiaggrm)] <- 0
























