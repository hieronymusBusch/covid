##########################################################
#                                                        #
#                                                        #
# 2021/10                                                #
#                                                        #
# Alexander Busch (a.busch@lse.ac.uk)                    #
#                                                        #
# Data Manipulation Dependent Variables NEW              #
#                                                        #
##########################################################

### Working with COVID-19 Data
## as file to big for github, it needs to be downloaded seperapte via
## https://npgeo-corona-npgeo-de.hub.arcgis.com/

load(file = "dfdd.Rda")  
load(file = "dfds.Rda")  

# Reading in RKI COVID-19 data from Germany, reading in Reported Date as Date
dfrki <- read.csv("C:/Users/alexa/Documents/Uni/RKI-COVID/RKI_COVID19_neu.csv",
                  colClasses=c(
                    "ï..IdBundesland" = "NULL", "Bundesland" = "NULL", "Landkreis" = "NULL",
                    "ObjectId" = "NULL", "Meldedatum" = "Date", "IdLandkreis" = "character", 
                    "NeuerFall" = "NULL", "NeuerTodesfall" = "NULL", "Refdatum" = "NULL", 
                    "NeuGenesen" = "NULL", "AnzahlGenesen" = "NULL", "IstErkrankungsbeginn" = "NULL", 
                    "Altersgruppe2" = "NULL", "Datenstand" = "NULL"
                  ))
dfrki$Mededatum <- as.Date(dfrki$Meldedatum)

# How many people without known age? (Altersgruppe "unbekannt", age group "unknown")
count(dfrki, Altersgruppe)

# > only <2600 unknown entries in age, in comparison to more than 2,000,000 known. 
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
              "2021-04-30", "2021-05-31", "2021-06-30", "2021-07-31", "2021-08-31", 
              "2021-09-30", "2021-10-31")
rkilist2 <- c("20.02", "20.03", "20.04", "20.05", "20.06", 
              "20.07", "20.08", "20.09", "20.10", "20.11",
              "20.12", "21.01", "21.02", "21.03", "21.04", 
              "21.05", "21.06", "21.07", "21.08", "21.09", "21.10", "")
rkilist3 <- c("", "0004", "0514", "1534", "3559", "6079", "80", "0004", "0514", "1534", "3559", "6079", "80")

# first, create subset for each month, 
# (as this loops through >2M data points each iteration, performs transformations, etc., section takes some time)
for (i in 1:21) {
  a <- subset(dfrki, Meldedatum <= rkilist1[[i+1]] & Meldedatum > rkilist1[[i]])
  assign(paste("dfrki", rkilist2[[i]], sep=""), a)
}
rkilist4 <- list(dfrki20.02, dfrki20.03, dfrki20.04, dfrki20.05, dfrki20.06, 
                 dfrki20.07, dfrki20.08, dfrki20.09, dfrki20.10, dfrki20.11,
                 dfrki20.12, dfrki21.01, dfrki21.02, dfrki21.03, dfrki21.04, 
                 dfrki21.05, dfrki21.06, dfrki21.07, dfrki21.08, dfrki21.09, 
                 dfrki21.10, dfrki)

## aggregating cases and deaths by age group (one time for all, once for each gender)
## Eisenach and Wartburgkreis merged in July 2021, thus cases are only reported for W 
# > as all other data still has Eisenach as independent county, cases / deaths are duplicated for E
# with adjusted population and weighted avg of ind variables
for (i in 1:22) {
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
  if(nrow(a[a$KRS=="16063",])>0){ # if condition as some months have 0 obs. for w / e county
    a[nrow(a)+1,1] <- "16056" # copying Eisenach - Wartburg county cases
    a[nrow(a),c(2:ncol(a))] <- a[a$KRS=="16063",c(2:ncol(a))] # ncol as some months might not have an entry for each age group
  }
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
  if(nrow(f[a$KRS=="16063",])>0){
    f[nrow(f)+1,1] <- "16056"
    f[nrow(f),c(2:ncol(f))] <- f[a$KRS=="16063",c(2:ncol(f))]
  }
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
  if(nrow(m[a$KRS=="16063",])>0){
    m[nrow(m)+1,1] <- "16056" 
    m[nrow(m),c(2:ncol(m))] <- m[a$KRS=="16063",c(2:ncol(m))]
  }
  m[is.na(m)] <- 0
  # save data frames
  assign(paste("dfrkiaggrf", rkilist2[[i]], sep=""), f)
  assign(paste("dfrkiaggrm", rkilist2[[i]], sep=""), m)
  assign(paste("dfrkiaggr", rkilist2[[i]], sep=""), a)
}

# no observations for two age groups in dfrkiaggrm20.02, need to add zero columns
dfrkiaggrm20.02$"cases.A05-A14" <- 0
dfrkiaggrm20.02$"deaths.A05-A14" <- 0
namescol <- colnames(dfrkiaggr)
dfrkiaggrm20.02 <- dfrkiaggrm20.02[,namescol]

rkilist5 <- list(dfrkiaggr20.02, dfrkiaggr20.03, dfrkiaggr20.04, dfrkiaggr20.05, dfrkiaggr20.06, 
                 dfrkiaggr20.07, dfrkiaggr20.08, dfrkiaggr20.09, dfrkiaggr20.10, dfrkiaggr20.11,
                 dfrkiaggr20.12, dfrkiaggr21.01, dfrkiaggr21.02, dfrkiaggr21.03, dfrkiaggr21.04, 
                 dfrkiaggr21.05, dfrkiaggr21.06, dfrkiaggr21.07, dfrkiaggr21.08, dfrkiaggr21.09, 
                 dfrkiaggr21.10, dfrkiaggr, 
                 dfrkiaggrf20.02, dfrkiaggrf20.03, dfrkiaggrf20.04, dfrkiaggrf20.05, dfrkiaggrf20.06, 
                 dfrkiaggrf20.07, dfrkiaggrf20.08, dfrkiaggrf20.09, dfrkiaggrf20.10, dfrkiaggrf20.11,
                 dfrkiaggrf20.12, dfrkiaggrf21.01, dfrkiaggrf21.02, dfrkiaggrf21.03, dfrkiaggrf21.04, 
                 dfrkiaggrf21.05, dfrkiaggrf21.06, dfrkiaggrf21.07, dfrkiaggrf21.08, dfrkiaggrf21.09,
                 dfrkiaggrf21.10, dfrkiaggrf, 
                 dfrkiaggrm20.02, dfrkiaggrm20.03, dfrkiaggrm20.04, dfrkiaggrm20.05, dfrkiaggrm20.06, 
                 dfrkiaggrm20.07, dfrkiaggrm20.08, dfrkiaggrm20.09, dfrkiaggrm20.10, dfrkiaggrm20.11,
                 dfrkiaggrm20.12, dfrkiaggrm21.01, dfrkiaggrm21.02, dfrkiaggrm21.03, dfrkiaggrm21.04, 
                 dfrkiaggrm21.05, dfrkiaggrm21.06, dfrkiaggrm21.07, dfrkiaggrm21.08, dfrkiaggrm21.09,
                 dfrkiaggrm21.10, dfrkiaggrm)

# create df with all KRS reference numbers
KRS <- dfrkiaggr20.03[,"KRS"]
dfKRS <- data.frame(KRS)
dfKRS$KRS <- as.character(dfKRS$KRS)

## Berlin units are not on county level, but district level > Needs to be summarized
# creating df with aggregate Berlin cases/deaths with correct geographic references
for (i in 1:66){
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
  ifelse(i<=22, assign(paste("dfrkiaggr", rkilist2[[i]], sep=""), a), 
         ifelse(i<=44, assign(paste("dfrkiaggrf", rkilist2[[i-22]], sep=""), a), 
                assign(paste("dfrkiaggrm", rkilist2[[i-44]], sep=""), a))
  )
}
rkilist5 <- list(dfrkiaggr20.02, dfrkiaggr20.03, dfrkiaggr20.04, dfrkiaggr20.05, dfrkiaggr20.06, 
                 dfrkiaggr20.07, dfrkiaggr20.08, dfrkiaggr20.09, dfrkiaggr20.10, dfrkiaggr20.11,
                 dfrkiaggr20.12, dfrkiaggr21.01, dfrkiaggr21.02, dfrkiaggr21.03, dfrkiaggr21.04, 
                 dfrkiaggr21.05, dfrkiaggr21.06, dfrkiaggr21.07, dfrkiaggr21.08, dfrkiaggr21.09, 
                 dfrkiaggr21.10, dfrkiaggr, 
                 dfrkiaggrf20.02, dfrkiaggrf20.03, dfrkiaggrf20.04, dfrkiaggrf20.05, dfrkiaggrf20.06, 
                 dfrkiaggrf20.07, dfrkiaggrf20.08, dfrkiaggrf20.09, dfrkiaggrf20.10, dfrkiaggrf20.11,
                 dfrkiaggrf20.12, dfrkiaggrf21.01, dfrkiaggrf21.02, dfrkiaggrf21.03, dfrkiaggrf21.04, 
                 dfrkiaggrf21.05, dfrkiaggrf21.06, dfrkiaggrf21.07, dfrkiaggrf21.08, dfrkiaggrf21.09,
                 dfrkiaggrf21.10, dfrkiaggrf, 
                 dfrkiaggrm20.02, dfrkiaggrm20.03, dfrkiaggrm20.04, dfrkiaggrm20.05, dfrkiaggrm20.06, 
                 dfrkiaggrm20.07, dfrkiaggrm20.08, dfrkiaggrm20.09, dfrkiaggrm20.10, dfrkiaggrm20.11,
                 dfrkiaggrm20.12, dfrkiaggrm21.01, dfrkiaggrm21.02, dfrkiaggrm21.03, dfrkiaggrm21.04, 
                 dfrkiaggrm21.05, dfrkiaggrm21.06, dfrkiaggrm21.07, dfrkiaggrm21.08, dfrkiaggrm21.09,
                 dfrkiaggrm21.10, dfrkiaggrm)

# giving names in order to later match
namescol <- colnames(dfrkiaggr)
for (i in 1:66){
  a <- rkilist5[[i]]
  for(j in 2:13){
    ifelse(str_detect(namescol[[j]], "^c"), 
           names(a)[names(a)==namescol[[j]]] <- paste("cases", rkilist3[[j]], sep=""),
           names(a)[names(a)==namescol[[j]]] <- paste("deaths", rkilist3[[j]], sep="")
    ) 
  }
  a <- a[order(a$KRS),]
  ifelse(i<=22, assign(paste("dfrkiaggr", rkilist2[[i]], sep=""), a), 
         ifelse(i<=44, assign(paste("dfrkiaggrf", rkilist2[[i-22]], sep=""), a), 
                assign(paste("dfrkiaggrm", rkilist2[[i-44]], sep=""), a))
  )
}
rkilist5 <- list(dfrkiaggr20.02, dfrkiaggr20.03, dfrkiaggr20.04, dfrkiaggr20.05, dfrkiaggr20.06, 
                 dfrkiaggr20.07, dfrkiaggr20.08, dfrkiaggr20.09, dfrkiaggr20.10, dfrkiaggr20.11,
                 dfrkiaggr20.12, dfrkiaggr21.01, dfrkiaggr21.02, dfrkiaggr21.03, dfrkiaggr21.04, 
                 dfrkiaggr21.05, dfrkiaggr21.06, dfrkiaggr21.07, dfrkiaggr21.08, dfrkiaggr21.09, 
                 dfrkiaggr21.10, dfrkiaggr, 
                 dfrkiaggrf20.02, dfrkiaggrf20.03, dfrkiaggrf20.04, dfrkiaggrf20.05, dfrkiaggrf20.06, 
                 dfrkiaggrf20.07, dfrkiaggrf20.08, dfrkiaggrf20.09, dfrkiaggrf20.10, dfrkiaggrf20.11,
                 dfrkiaggrf20.12, dfrkiaggrf21.01, dfrkiaggrf21.02, dfrkiaggrf21.03, dfrkiaggrf21.04, 
                 dfrkiaggrf21.05, dfrkiaggrf21.06, dfrkiaggrf21.07, dfrkiaggrf21.08, dfrkiaggrf21.09,
                 dfrkiaggrf21.10, dfrkiaggrf, 
                 dfrkiaggrm20.02, dfrkiaggrm20.03, dfrkiaggrm20.04, dfrkiaggrm20.05, dfrkiaggrm20.06, 
                 dfrkiaggrm20.07, dfrkiaggrm20.08, dfrkiaggrm20.09, dfrkiaggrm20.10, dfrkiaggrm20.11,
                 dfrkiaggrm20.12, dfrkiaggrm21.01, dfrkiaggrm21.02, dfrkiaggrm21.03, dfrkiaggrm21.04, 
                 dfrkiaggrm21.05, dfrkiaggrm21.06, dfrkiaggrm21.07, dfrkiaggrm21.08, dfrkiaggrm21.09,
                 dfrkiaggrm21.10, dfrkiaggrm)

# calculating sums: one crude sum, one standardised EU sum, IR and CFR both with and without EU weights
# assumption: EU age brackets same across genders in counties (as no data on m/f per county & age bracket)
dfpop <- dfdd[, c("KRS", "population", "fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80", "shareWomen")]
for (i in 1:66){
  a <- rkilist5[[i]]
  a$cases <- as.numeric(apply(a[,2:7], 1, sum))
  a$deaths <- as.numeric(apply(a[,8:13], 1, sum))
  # reading in pop and EU pop variables from indep. vars
  a <- merge(a, dfpop, by = "KRS", all = TRUE)
  a$IR <- (a$cases / a$population) * 100000
  a$IREU <- a$cases0004 * a$fy0004 + a$cases0514 * a$fy0514 +
    a$cases1534 * a$fy1534 + a$cases3559 * a$fy3559 +
    a$cases6079 * a$fy6079 + a$cases80 * a$fy80
  a$CFR <- (a$deaths / a$cases)
  a$deathsEU <- a$deaths0004 * a$fy0004 + a$deaths0514 * a$fy0514 +
    a$deaths1534 * a$fy1534 + a$deaths3559 * a$fy3559 +
    a$deaths6079 * a$fy6079 + a$deaths80 * a$fy80
  a$CFREU <- (a$deathsEU / a$IREU)
  # weight measures by female / male 
  if(22<i & i <=44){
    a[,c(24:28)] <- a[,c(24:28)] * (1/(a$shareWomen/100))
  }
  if(44<i & i<=66){
    a[,c(24:28)] <- a[,c(24:28)] * (1/(1-a$shareWomen/100))
  }
  a$shareWomen <- NULL
  # giving unique names to variables
  ifelse(i<=22, colnames(a) <- paste(colnames(a), rkilist2[[i]], sep = ""), 
         ifelse(i<=44, colnames(a) <- paste(colnames(a), rkilist2[[i-22]], sep = ""), 
                colnames(a) <- paste(colnames(a), rkilist2[[i-44]], sep = ""))
  )
  namescol <- colnames(a)
  names(a)[names(a)==namescol[[1]]] <- "KRS"
  a <- a[,c(1,14,15,23:27)]
  # saving output
  ifelse(i<=22, assign(paste("dfrkiaggr", rkilist2[[i]], sep=""), a), 
         ifelse(i<=44, assign(paste("dfrkiaggrf", rkilist2[[i-22]], sep=""), a), 
                assign(paste("dfrkiaggrm", rkilist2[[i-44]], sep=""), a))
  )
}

rkilist5 <- list(dfrkiaggr20.02, dfrkiaggr20.03, dfrkiaggr20.04, dfrkiaggr20.05, dfrkiaggr20.06, 
                 dfrkiaggr20.07, dfrkiaggr20.08, dfrkiaggr20.09, dfrkiaggr20.10, dfrkiaggr20.11,
                 dfrkiaggr20.12, dfrkiaggr21.01, dfrkiaggr21.02, dfrkiaggr21.03, dfrkiaggr21.04, 
                 dfrkiaggr21.05, dfrkiaggr21.06, dfrkiaggr21.07, dfrkiaggr21.08, dfrkiaggr21.09, 
                 dfrkiaggr21.10, dfrkiaggr, 
                 dfrkiaggrf20.02, dfrkiaggrf20.03, dfrkiaggrf20.04, dfrkiaggrf20.05, dfrkiaggrf20.06, 
                 dfrkiaggrf20.07, dfrkiaggrf20.08, dfrkiaggrf20.09, dfrkiaggrf20.10, dfrkiaggrf20.11,
                 dfrkiaggrf20.12, dfrkiaggrf21.01, dfrkiaggrf21.02, dfrkiaggrf21.03, dfrkiaggrf21.04, 
                 dfrkiaggrf21.05, dfrkiaggrf21.06, dfrkiaggrf21.07, dfrkiaggrf21.08, dfrkiaggrf21.09,
                 dfrkiaggrf21.10, dfrkiaggrf, 
                 dfrkiaggrm20.02, dfrkiaggrm20.03, dfrkiaggrm20.04, dfrkiaggrm20.05, dfrkiaggrm20.06, 
                 dfrkiaggrm20.07, dfrkiaggrm20.08, dfrkiaggrm20.09, dfrkiaggrm20.10, dfrkiaggrm20.11,
                 dfrkiaggrm20.12, dfrkiaggrm21.01, dfrkiaggrm21.02, dfrkiaggrm21.03, dfrkiaggrm21.04, 
                 dfrkiaggrm21.05, dfrkiaggrm21.06, dfrkiaggrm21.07, dfrkiaggrm21.08, dfrkiaggrm21.09,
                 dfrkiaggrm21.10, dfrkiaggrm)

# merge aggregated monthly data sets to one 
for(i in 1:21) {
  dfrkiaggr <- merge(dfrkiaggr, rkilist5[i], by = "KRS",all = TRUE)
}
dfrkiaggr[is.na(dfrkiaggr)] <- 0
for(i in 23:43) {
  dfrkiaggrf <- merge(dfrkiaggrf, rkilist5[i], by = "KRS",all = TRUE)
}
dfrkiaggrf[is.na(dfrkiaggrf)] <- 0
for(i in 45:65) {
  dfrkiaggrm <- merge(dfrkiaggrm, rkilist5[i], by = "KRS",all = TRUE)
}
dfrkiaggrm[is.na(dfrkiaggrm)] <- 0

dfpop <- dfdd[, c("KRS", "population")]
dfrkiaggr <- merge(dfrkiaggr, dfpop, by = "KRS")
dfrkiaggrf <- merge(dfrkiaggrf, dfpop, by = "KRS")
dfrkiaggrm <- merge(dfrkiaggrm, dfpop, by = "KRS")

### LOG transformations
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
dfrkiaggr$CFRlag21.07 <- (dfrkiaggr$deaths21.07 / dfrkiaggr$cases21.06)
dfrkiaggr$CFRlag21.08 <- (dfrkiaggr$deaths21.08 / dfrkiaggr$cases21.07)
dfrkiaggr$CFRlag21.09 <- (dfrkiaggr$deaths21.09 / dfrkiaggr$cases21.08)
dfrkiaggr$CFRlag21.10 <- (dfrkiaggr$deaths21.10 / dfrkiaggr$cases21.09)

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
dfrkiaggr$CFREUlag21.07 <- (dfrkiaggr$deathsEU21.07 / dfrkiaggr$IREU21.06)
dfrkiaggr$CFREUlag21.08 <- (dfrkiaggr$deathsEU21.08 / dfrkiaggr$IREU21.07)
dfrkiaggr$CFREUlag21.09 <- (dfrkiaggr$deathsEU21.09 / dfrkiaggr$IREU21.08)
dfrkiaggr$CFREUlag21.10 <- (dfrkiaggr$deathsEU21.10 / dfrkiaggr$IREU21.09)

# recode undefined (x/0) CFR to 0 as described in Thesis
dfrkiaggr[is.na(dfrkiaggr)] <- 0
dfrkiaggr[dfrkiaggr == Inf] <- 0

# logs 
dfrkiaggr$LNIR20.02 <- log(dfrkiaggr$IR20.02)
dfrkiaggr$LNIR20.03 <- log(dfrkiaggr$IR20.03)
dfrkiaggr$LNIR20.04 <- log(dfrkiaggr$IR20.04)
dfrkiaggr$LNIR20.05 <- log(dfrkiaggr$IR20.05)
dfrkiaggr$LNIR20.06 <- log(dfrkiaggr$IR20.06)
dfrkiaggr$LNIR20.07 <- log(dfrkiaggr$IR20.07)
dfrkiaggr$LNIR20.08 <- log(dfrkiaggr$IR20.08)
dfrkiaggr$LNIR20.09 <- log(dfrkiaggr$IR20.09)
dfrkiaggr$LNIR20.10 <- log(dfrkiaggr$IR20.10)
dfrkiaggr$LNIR20.11 <- log(dfrkiaggr$IR20.11)
dfrkiaggr$LNIR20.12 <- log(dfrkiaggr$IR20.12)
dfrkiaggr$LNIR21.01 <- log(dfrkiaggr$IR21.01)
dfrkiaggr$LNIR21.02 <- log(dfrkiaggr$IR21.02)
dfrkiaggr$LNIR21.03 <- log(dfrkiaggr$IR21.03)
dfrkiaggr$LNIR21.04 <- log(dfrkiaggr$IR21.04)
dfrkiaggr$LNIR21.05 <- log(dfrkiaggr$IR21.05)
dfrkiaggr$LNIR21.06 <- log(dfrkiaggr$IR21.06)
dfrkiaggr$LNIR21.07 <- log(dfrkiaggr$IR21.07)
dfrkiaggr$LNIR21.08 <- log(dfrkiaggr$IR21.08)
dfrkiaggr$LNIR21.09 <- log(dfrkiaggr$IR21.09)
dfrkiaggr$LNIR21.10 <- log(dfrkiaggr$IR21.10)
dfrkiaggr$LNIR <- log(dfrkiaggr$IR)

dfrkiaggr$LNIREU20.02 <- log(dfrkiaggr$IREU20.02)
dfrkiaggr$LNIREU20.03 <- log(dfrkiaggr$IREU20.03)
dfrkiaggr$LNIREU20.04 <- log(dfrkiaggr$IREU20.04)
dfrkiaggr$LNIREU20.05 <- log(dfrkiaggr$IREU20.05)
dfrkiaggr$LNIREU20.06 <- log(dfrkiaggr$IREU20.06)
dfrkiaggr$LNIREU20.07 <- log(dfrkiaggr$IREU20.07)
dfrkiaggr$LNIREU20.08 <- log(dfrkiaggr$IREU20.08)
dfrkiaggr$LNIREU20.09 <- log(dfrkiaggr$IREU20.09)
dfrkiaggr$LNIREU20.10 <- log(dfrkiaggr$IREU20.10)
dfrkiaggr$LNIREU20.11 <- log(dfrkiaggr$IREU20.11)
dfrkiaggr$LNIREU20.12 <- log(dfrkiaggr$IREU20.12)
dfrkiaggr$LNIREU21.01 <- log(dfrkiaggr$IREU21.01)
dfrkiaggr$LNIREU21.02 <- log(dfrkiaggr$IREU21.02)
dfrkiaggr$LNIREU21.03 <- log(dfrkiaggr$IREU21.03)
dfrkiaggr$LNIREU21.04 <- log(dfrkiaggr$IREU21.04)
dfrkiaggr$LNIREU21.05 <- log(dfrkiaggr$IREU21.05)
dfrkiaggr$LNIREU21.06 <- log(dfrkiaggr$IREU21.06)
dfrkiaggr$LNIREU21.07 <- log(dfrkiaggr$IREU21.07)
dfrkiaggr$LNIREU21.08 <- log(dfrkiaggr$IREU21.08)
dfrkiaggr$LNIREU21.09 <- log(dfrkiaggr$IREU21.09)
dfrkiaggr$LNIREU21.10 <- log(dfrkiaggr$IREU21.10)
dfrkiaggr$LNIREU <- log(dfrkiaggr$IREU)

# Sum of previous cases in county as proxy for immune people
dfrkiaggr$scases20.02 <- (dfrkiaggr$cases20.02) / dfrkiaggr$population
dfrkiaggr$scases20.03 <- (dfrkiaggr$cases20.03) / dfrkiaggr$population + dfrkiaggr$scases20.02 
dfrkiaggr$scases20.04 <- (dfrkiaggr$cases20.04) / dfrkiaggr$population + dfrkiaggr$scases20.03 
dfrkiaggr$scases20.05 <- (dfrkiaggr$cases20.05) / dfrkiaggr$population + dfrkiaggr$scases20.04 
dfrkiaggr$scases20.06 <- (dfrkiaggr$cases20.06) / dfrkiaggr$population + dfrkiaggr$scases20.05 
dfrkiaggr$scases20.07 <- (dfrkiaggr$cases20.07) / dfrkiaggr$population + dfrkiaggr$scases20.06 
dfrkiaggr$scases20.08 <- (dfrkiaggr$cases20.08) / dfrkiaggr$population + dfrkiaggr$scases20.07 
dfrkiaggr$scases20.09 <- (dfrkiaggr$cases20.09) / dfrkiaggr$population + dfrkiaggr$scases20.08 
dfrkiaggr$scases20.10 <- (dfrkiaggr$cases20.10) / dfrkiaggr$population + dfrkiaggr$scases20.09 
dfrkiaggr$scases20.11 <- (dfrkiaggr$cases20.11) / dfrkiaggr$population + dfrkiaggr$scases20.10 
dfrkiaggr$scases20.12 <- (dfrkiaggr$cases20.12) / dfrkiaggr$population + dfrkiaggr$scases20.11 
dfrkiaggr$scases21.01 <- (dfrkiaggr$cases21.01) / dfrkiaggr$population + dfrkiaggr$scases20.12 
dfrkiaggr$scases21.02 <- (dfrkiaggr$cases21.02) / dfrkiaggr$population + dfrkiaggr$scases21.01 
dfrkiaggr$scases21.03 <- (dfrkiaggr$cases21.03) / dfrkiaggr$population + dfrkiaggr$scases21.02 
dfrkiaggr$scases21.04 <- (dfrkiaggr$cases21.04) / dfrkiaggr$population + dfrkiaggr$scases21.03 
dfrkiaggr$scases21.05 <- (dfrkiaggr$cases21.05) / dfrkiaggr$population + dfrkiaggr$scases21.04 
dfrkiaggr$scases21.06 <- (dfrkiaggr$cases21.06) / dfrkiaggr$population + dfrkiaggr$scases21.05 
dfrkiaggr$scases21.07 <- (dfrkiaggr$cases21.07) / dfrkiaggr$population + dfrkiaggr$scases21.06 
dfrkiaggr$scases21.08 <- (dfrkiaggr$cases21.08) / dfrkiaggr$population + dfrkiaggr$scases21.07 
dfrkiaggr$scases21.09 <- (dfrkiaggr$cases21.09) / dfrkiaggr$population + dfrkiaggr$scases21.08 
dfrkiaggr$scases21.10 <- (dfrkiaggr$cases21.10) / dfrkiaggr$population + dfrkiaggr$scases21.09 

# logs female
dfrkiaggrf$LNIR20.02 <- log(dfrkiaggrf$IR20.02)
dfrkiaggrf$LNIR20.03 <- log(dfrkiaggrf$IR20.03)
dfrkiaggrf$LNIR20.04 <- log(dfrkiaggrf$IR20.04)
dfrkiaggrf$LNIR20.05 <- log(dfrkiaggrf$IR20.05)
dfrkiaggrf$LNIR20.06 <- log(dfrkiaggrf$IR20.06)
dfrkiaggrf$LNIR20.07 <- log(dfrkiaggrf$IR20.07)
dfrkiaggrf$LNIR20.08 <- log(dfrkiaggrf$IR20.08)
dfrkiaggrf$LNIR20.09 <- log(dfrkiaggrf$IR20.09)
dfrkiaggrf$LNIR20.10 <- log(dfrkiaggrf$IR20.10)
dfrkiaggrf$LNIR20.11 <- log(dfrkiaggrf$IR20.11)
dfrkiaggrf$LNIR20.12 <- log(dfrkiaggrf$IR20.12)
dfrkiaggrf$LNIR21.01 <- log(dfrkiaggrf$IR21.01)
dfrkiaggrf$LNIR21.02 <- log(dfrkiaggrf$IR21.02)
dfrkiaggrf$LNIR21.03 <- log(dfrkiaggrf$IR21.03)
dfrkiaggrf$LNIR21.04 <- log(dfrkiaggrf$IR21.04)
dfrkiaggrf$LNIR21.05 <- log(dfrkiaggrf$IR21.05)
dfrkiaggrf$LNIR21.06 <- log(dfrkiaggrf$IR21.06)
dfrkiaggrf$LNIR21.07 <- log(dfrkiaggrf$IR21.07)
dfrkiaggrf$LNIR21.08 <- log(dfrkiaggrf$IR21.08)
dfrkiaggrf$LNIR21.09 <- log(dfrkiaggrf$IR21.09)
dfrkiaggrf$LNIR21.10 <- log(dfrkiaggrf$IR21.10)
dfrkiaggrf$LNIR <- log(dfrkiaggrf$IR)

dfrkiaggrf$LNIREU20.02 <- log(dfrkiaggrf$IREU20.02)
dfrkiaggrf$LNIREU20.03 <- log(dfrkiaggrf$IREU20.03)
dfrkiaggrf$LNIREU20.04 <- log(dfrkiaggrf$IREU20.04)
dfrkiaggrf$LNIREU20.05 <- log(dfrkiaggrf$IREU20.05)
dfrkiaggrf$LNIREU20.06 <- log(dfrkiaggrf$IREU20.06)
dfrkiaggrf$LNIREU20.07 <- log(dfrkiaggrf$IREU20.07)
dfrkiaggrf$LNIREU20.08 <- log(dfrkiaggrf$IREU20.08)
dfrkiaggrf$LNIREU20.09 <- log(dfrkiaggrf$IREU20.09)
dfrkiaggrf$LNIREU20.10 <- log(dfrkiaggrf$IREU20.10)
dfrkiaggrf$LNIREU20.11 <- log(dfrkiaggrf$IREU20.11)
dfrkiaggrf$LNIREU20.12 <- log(dfrkiaggrf$IREU20.12)
dfrkiaggrf$LNIREU21.01 <- log(dfrkiaggrf$IREU21.01)
dfrkiaggrf$LNIREU21.02 <- log(dfrkiaggrf$IREU21.02)
dfrkiaggrf$LNIREU21.03 <- log(dfrkiaggrf$IREU21.03)
dfrkiaggrf$LNIREU21.04 <- log(dfrkiaggrf$IREU21.04)
dfrkiaggrf$LNIREU21.05 <- log(dfrkiaggrf$IREU21.05)
dfrkiaggrf$LNIREU21.06 <- log(dfrkiaggrf$IREU21.06)
dfrkiaggrf$LNIREU21.07 <- log(dfrkiaggrf$IREU21.07)
dfrkiaggrf$LNIREU21.08 <- log(dfrkiaggrf$IREU21.08)
dfrkiaggrf$LNIREU21.09 <- log(dfrkiaggrf$IREU21.09)
dfrkiaggrf$LNIREU21.10 <- log(dfrkiaggrf$IREU21.10)
dfrkiaggrf$LNIREU <- log(dfrkiaggrf$IREU)

# Sum of previous cases in county as proxy for immune people
dfrkiaggrf$scases20.02 <- (dfrkiaggrf$cases20.02) / dfrkiaggrf$population
dfrkiaggrf$scases20.03 <- (dfrkiaggrf$cases20.03) / dfrkiaggrf$population + dfrkiaggrf$scases20.02 
dfrkiaggrf$scases20.04 <- (dfrkiaggrf$cases20.04) / dfrkiaggrf$population + dfrkiaggrf$scases20.03 
dfrkiaggrf$scases20.05 <- (dfrkiaggrf$cases20.05) / dfrkiaggrf$population + dfrkiaggrf$scases20.04 
dfrkiaggrf$scases20.06 <- (dfrkiaggrf$cases20.06) / dfrkiaggrf$population + dfrkiaggrf$scases20.05 
dfrkiaggrf$scases20.07 <- (dfrkiaggrf$cases20.07) / dfrkiaggrf$population + dfrkiaggrf$scases20.06 
dfrkiaggrf$scases20.08 <- (dfrkiaggrf$cases20.08) / dfrkiaggrf$population + dfrkiaggrf$scases20.07 
dfrkiaggrf$scases20.09 <- (dfrkiaggrf$cases20.09) / dfrkiaggrf$population + dfrkiaggrf$scases20.08 
dfrkiaggrf$scases20.10 <- (dfrkiaggrf$cases20.10) / dfrkiaggrf$population + dfrkiaggrf$scases20.09 
dfrkiaggrf$scases20.11 <- (dfrkiaggrf$cases20.11) / dfrkiaggrf$population + dfrkiaggrf$scases20.10 
dfrkiaggrf$scases20.12 <- (dfrkiaggrf$cases20.12) / dfrkiaggrf$population + dfrkiaggrf$scases20.11 
dfrkiaggrf$scases21.01 <- (dfrkiaggrf$cases21.01) / dfrkiaggrf$population + dfrkiaggrf$scases20.12 
dfrkiaggrf$scases21.02 <- (dfrkiaggrf$cases21.02) / dfrkiaggrf$population + dfrkiaggrf$scases21.01 
dfrkiaggrf$scases21.03 <- (dfrkiaggrf$cases21.03) / dfrkiaggrf$population + dfrkiaggrf$scases21.02 
dfrkiaggrf$scases21.04 <- (dfrkiaggrf$cases21.04) / dfrkiaggrf$population + dfrkiaggrf$scases21.03 
dfrkiaggrf$scases21.05 <- (dfrkiaggrf$cases21.05) / dfrkiaggrf$population + dfrkiaggrf$scases21.04 
dfrkiaggrf$scases21.06 <- (dfrkiaggrf$cases21.06) / dfrkiaggrf$population + dfrkiaggrf$scases21.05 
dfrkiaggrf$scases21.07 <- (dfrkiaggrf$cases21.07) / dfrkiaggrf$population + dfrkiaggrf$scases21.06 
dfrkiaggrf$scases21.08 <- (dfrkiaggrf$cases21.08) / dfrkiaggrf$population + dfrkiaggrf$scases21.07 
dfrkiaggrf$scases21.09 <- (dfrkiaggrf$cases21.09) / dfrkiaggrf$population + dfrkiaggrf$scases21.08 
dfrkiaggrf$scases21.10 <- (dfrkiaggrf$cases21.10) / dfrkiaggrf$population + dfrkiaggrf$scases21.09 

# logs male
dfrkiaggrm$LNIR20.02 <- log(dfrkiaggrm$IR20.02)
dfrkiaggrm$LNIR20.03 <- log(dfrkiaggrm$IR20.03)
dfrkiaggrm$LNIR20.04 <- log(dfrkiaggrm$IR20.04)
dfrkiaggrm$LNIR20.05 <- log(dfrkiaggrm$IR20.05)
dfrkiaggrm$LNIR20.06 <- log(dfrkiaggrm$IR20.06)
dfrkiaggrm$LNIR20.07 <- log(dfrkiaggrm$IR20.07)
dfrkiaggrm$LNIR20.08 <- log(dfrkiaggrm$IR20.08)
dfrkiaggrm$LNIR20.09 <- log(dfrkiaggrm$IR20.09)
dfrkiaggrm$LNIR20.10 <- log(dfrkiaggrm$IR20.10)
dfrkiaggrm$LNIR20.11 <- log(dfrkiaggrm$IR20.11)
dfrkiaggrm$LNIR20.12 <- log(dfrkiaggrm$IR20.12)
dfrkiaggrm$LNIR21.01 <- log(dfrkiaggrm$IR21.01)
dfrkiaggrm$LNIR21.02 <- log(dfrkiaggrm$IR21.02)
dfrkiaggrm$LNIR21.03 <- log(dfrkiaggrm$IR21.03)
dfrkiaggrm$LNIR21.04 <- log(dfrkiaggrm$IR21.04)
dfrkiaggrm$LNIR21.05 <- log(dfrkiaggrm$IR21.05)
dfrkiaggrm$LNIR21.06 <- log(dfrkiaggrm$IR21.06)
dfrkiaggrm$LNIR21.07 <- log(dfrkiaggrm$IR21.07)
dfrkiaggrm$LNIR21.08 <- log(dfrkiaggrm$IR21.08)
dfrkiaggrm$LNIR21.09 <- log(dfrkiaggrm$IR21.09)
dfrkiaggrm$LNIR21.10 <- log(dfrkiaggrm$IR21.10)
dfrkiaggrm$LNIR <- log(dfrkiaggrm$IR)

dfrkiaggrm$LNIREU20.02 <- log(dfrkiaggrm$IREU20.02)
dfrkiaggrm$LNIREU20.03 <- log(dfrkiaggrm$IREU20.03)
dfrkiaggrm$LNIREU20.04 <- log(dfrkiaggrm$IREU20.04)
dfrkiaggrm$LNIREU20.05 <- log(dfrkiaggrm$IREU20.05)
dfrkiaggrm$LNIREU20.06 <- log(dfrkiaggrm$IREU20.06)
dfrkiaggrm$LNIREU20.07 <- log(dfrkiaggrm$IREU20.07)
dfrkiaggrm$LNIREU20.08 <- log(dfrkiaggrm$IREU20.08)
dfrkiaggrm$LNIREU20.09 <- log(dfrkiaggrm$IREU20.09)
dfrkiaggrm$LNIREU20.10 <- log(dfrkiaggrm$IREU20.10)
dfrkiaggrm$LNIREU20.11 <- log(dfrkiaggrm$IREU20.11)
dfrkiaggrm$LNIREU20.12 <- log(dfrkiaggrm$IREU20.12)
dfrkiaggrm$LNIREU21.01 <- log(dfrkiaggrm$IREU21.01)
dfrkiaggrm$LNIREU21.02 <- log(dfrkiaggrm$IREU21.02)
dfrkiaggrm$LNIREU21.03 <- log(dfrkiaggrm$IREU21.03)
dfrkiaggrm$LNIREU21.04 <- log(dfrkiaggrm$IREU21.04)
dfrkiaggrm$LNIREU21.05 <- log(dfrkiaggrm$IREU21.05)
dfrkiaggrm$LNIREU21.06 <- log(dfrkiaggrm$IREU21.06)
dfrkiaggrm$LNIREU21.07 <- log(dfrkiaggrm$IREU21.07)
dfrkiaggrm$LNIREU21.08 <- log(dfrkiaggrm$IREU21.08)
dfrkiaggrm$LNIREU21.09 <- log(dfrkiaggrm$IREU21.09)
dfrkiaggrm$LNIREU21.10 <- log(dfrkiaggrm$IREU21.10)
dfrkiaggrm$LNIREU <- log(dfrkiaggrm$IREU)

# Sum of previous cases in county as proxy for immune people
dfrkiaggrm$scases20.02 <- (dfrkiaggrm$cases20.02) / dfrkiaggrm$population
dfrkiaggrm$scases20.03 <- (dfrkiaggrm$cases20.03) / dfrkiaggrm$population + dfrkiaggrm$scases20.02 
dfrkiaggrm$scases20.04 <- (dfrkiaggrm$cases20.04) / dfrkiaggrm$population + dfrkiaggrm$scases20.03 
dfrkiaggrm$scases20.05 <- (dfrkiaggrm$cases20.05) / dfrkiaggrm$population + dfrkiaggrm$scases20.04 
dfrkiaggrm$scases20.06 <- (dfrkiaggrm$cases20.06) / dfrkiaggrm$population + dfrkiaggrm$scases20.05 
dfrkiaggrm$scases20.07 <- (dfrkiaggrm$cases20.07) / dfrkiaggrm$population + dfrkiaggrm$scases20.06 
dfrkiaggrm$scases20.08 <- (dfrkiaggrm$cases20.08) / dfrkiaggrm$population + dfrkiaggrm$scases20.07 
dfrkiaggrm$scases20.09 <- (dfrkiaggrm$cases20.09) / dfrkiaggrm$population + dfrkiaggrm$scases20.08 
dfrkiaggrm$scases20.10 <- (dfrkiaggrm$cases20.10) / dfrkiaggrm$population + dfrkiaggrm$scases20.09 
dfrkiaggrm$scases20.11 <- (dfrkiaggrm$cases20.11) / dfrkiaggrm$population + dfrkiaggrm$scases20.10 
dfrkiaggrm$scases20.12 <- (dfrkiaggrm$cases20.12) / dfrkiaggrm$population + dfrkiaggrm$scases20.11 
dfrkiaggrm$scases21.01 <- (dfrkiaggrm$cases21.01) / dfrkiaggrm$population + dfrkiaggrm$scases20.12 
dfrkiaggrm$scases21.02 <- (dfrkiaggrm$cases21.02) / dfrkiaggrm$population + dfrkiaggrm$scases21.01 
dfrkiaggrm$scases21.03 <- (dfrkiaggrm$cases21.03) / dfrkiaggrm$population + dfrkiaggrm$scases21.02 
dfrkiaggrm$scases21.04 <- (dfrkiaggrm$cases21.04) / dfrkiaggrm$population + dfrkiaggrm$scases21.03 
dfrkiaggrm$scases21.05 <- (dfrkiaggrm$cases21.05) / dfrkiaggrm$population + dfrkiaggrm$scases21.04 
dfrkiaggrm$scases21.06 <- (dfrkiaggrm$cases21.06) / dfrkiaggrm$population + dfrkiaggrm$scases21.05 
dfrkiaggrm$scases21.07 <- (dfrkiaggrm$cases21.07) / dfrkiaggrm$population + dfrkiaggrm$scases21.06 
dfrkiaggrm$scases21.08 <- (dfrkiaggrm$cases21.08) / dfrkiaggrm$population + dfrkiaggrm$scases21.07 
dfrkiaggrm$scases21.09 <- (dfrkiaggrm$cases21.09) / dfrkiaggrm$population + dfrkiaggrm$scases21.08 
dfrkiaggrm$scases21.10 <- (dfrkiaggrm$cases21.10) / dfrkiaggrm$population + dfrkiaggrm$scases21.09 

dfrkiaggr$population <- NULL
dfrkiaggrm$population <- NULL
dfrkiaggrf$population <- NULL

# safe data
dfrkiaggrnew <- dfrkiaggr
save(dfrkiaggrnew, file = "dfrkiaggrnew.Rda")
dfrkiaggrfnew <- dfrkiaggrf
save(dfrkiaggrfnew, file = "dfrkiaggrfnew.Rda")
dfrkiaggrmnew <- dfrkiaggrm
save(dfrkiaggrmnew, file = "dfrkiaggrmnew.Rda")





















