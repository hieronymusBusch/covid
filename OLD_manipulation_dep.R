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
dfcd <- read.csv("C:/Users/alexa/Documents/Uni/RKI-COVID/RKI_COVID19.csv",
                  colClasses=c(
                    "IdBundesland" = "NULL", "Datenstand" = "NULL", 
                    "Meldedatum" = "Date", "IdLandkreis" = "character"
                  ))

# How many people without known age? (Altersgruppe "unbekannt", age group "unknown")
count(dfcd, Altersgruppe)
# > only 2103 unknown entries, in comparison to 2074321 known. 
# > multiple cases in one entry possible, but extremly scarce 
# > still, unknown age group is magnitudes below all other groups
# > thus, unknowns are ignored
dfcd <- dfcd[!(dfcd$Altersgruppe=="unbekannt"),]

# first, create subset for each month, 
# then apply aggregateTransform (see function.R) to calculate IR, IREU, CFR, CFREU
# (as this loops through >2M data points each iteration, performs transformations, etc., section takes some time)
dfcd <- dfrki
dfcd$month <- ifelse(dfcd$Meldedatum >= "2021-06-01", 
                       "21-06", 
                ifelse(dfcd$Meldedatum >= "2021-05-01", 
                       "21-05", 
                ifelse(dfcd$Meldedatum >= "2021-04-01", 
                       "21-04", 
                ifelse(dfcd$Meldedatum >= "2021-03-01", 
                       "21-03", 
                ifelse(dfcd$Meldedatum >= "2021-02-01", 
                       "21-02", 
                ifelse(dfcd$Meldedatum >= "2021-01-01", 
                       "21-01", 
                ifelse(dfcd$Meldedatum >= "2020-12-01", 
                       "20-12", 
                ifelse(dfcd$Meldedatum >= "2020-11-01", 
                       "20-11", 
                ifelse(dfcd$Meldedatum >= "2020-10-01", 
                       "20-10", 
                ifelse(dfcd$Meldedatum >= "2020-09-01", 
                       "20-09", 
                ifelse(dfcd$Meldedatum >= "2020-08-01", 
                       "20-08",  
                ifelse(dfcd$Meldedatum >= "2020-07-01", 
                       "20-07", 
                ifelse(dfcd$Meldedatum >= "2020-06-01", 
                       "20-06", 
                ifelse(dfcd$Meldedatum >= "2020-05-01", 
                       "20-05", 
                ifelse(dfcd$Meldedatum >= "2020-04-01", 
                       "20-04", 
                ifelse(dfcd$Meldedatum >= "2020-03-01", 
                       "20-03", 
                       "20-02"  
                ))))))))))))))))
dfc <- dfcd[,c("Altersgruppe","AnzahlFall","IdLandkreis","month")]
dfc <- ddply(dfc, .(IdLandkreis, month, Altersgruppe), summarise, cases=sum(AnzahlFall))
dfc <- reshape(dfc, idvar = c("IdLandkreis", "Altersgruppe"), timevar = "month", direction = "wide")
dfc <- reshape(dfc, idvar = c("IdLandkreis"), timevar = "Altersgruppe", direction = "wide")
dfc[is.na(dfc)] <- 0


dfd <- dfcd[,c("Altersgruppe","AnzahlTodesfall","IdLandkreis","month")]
dfd <- ddply(dfd, .(IdLandkreis, month, Altersgruppe), summarise, deaths=sum(AnzahlTodesfall))
dfd <- reshape(dfd, idvar = c("IdLandkreis", "Altersgruppe"), timevar = "month", direction = "wide")
dfd <- reshape(dfd, idvar = c("IdLandkreis"), timevar = "Altersgruppe", direction = "wide")
dfd[is.na(dfd)] <- 0
# creating df with aggregate Berlin cases/deaths with correct geographic references
dfberlin <- subset(dfc, IdLandkreis == "11001"|IdLandkreis =="11002"|IdLandkreis =="11003"|IdLandkreis =="11004"|IdLandkreis =="11005"|IdLandkreis =="11006"|
                     IdLandkreis =="11007"|IdLandkreis =="11008"|IdLandkreis =="11009"|
                     IdLandkreis =="11010"|IdLandkreis =="11011"|IdLandkreis =="11012")
dfberlin[13,] = c("11000", colSums(dfberlin[,2:103]))
dfberlin <- dfberlin[13,]
dfc <- rbind(dfc,dfberlin)
dfc <- dfc[!(dfc$IdLandkreis=="11001" | dfc$IdLandkreis=="11002" | dfc$IdLandkreis=="11003"
             | dfc$IdLandkreis=="11004" | dfc$IdLandkreis=="11005" | dfc$IdLandkreis=="11006"
             | dfc$IdLandkreis=="11007" | dfc$IdLandkreis=="11008" | dfc$IdLandkreis=="11009"
             | dfc$IdLandkreis=="11010" | dfc$IdLandkreis=="11011" | dfc$IdLandkreis=="11012"),]

dfberlin <- subset(dfd, IdLandkreis == "11001"|IdLandkreis =="11002"|IdLandkreis =="11003"|IdLandkreis =="11004"|IdLandkreis =="11005"|IdLandkreis =="11006"|
                     IdLandkreis =="11007"|IdLandkreis =="11008"|IdLandkreis =="11009"|
                     IdLandkreis =="11010"|IdLandkreis =="11011"|IdLandkreis =="11012")
dfberlin[13,] = c("11000", colSums(dfberlin[,2:103]))
dfberlin <- dfberlin[13,]
dfd <- rbind(dfd,dfberlin)
dfd <- dfd[!(dfd$IdLandkreis=="11001" | dfd$IdLandkreis=="11002" | dfd$IdLandkreis=="11003"
               | dfd$IdLandkreis=="11004" | dfd$IdLandkreis=="11005" | dfd$IdLandkreis=="11006"
               | dfd$IdLandkreis=="11007" | dfd$IdLandkreis=="11008" | dfd$IdLandkreis=="11009"
               | dfd$IdLandkreis=="11010" | dfd$IdLandkreis=="11011" | dfd$IdLandkreis=="11012"),]
# merge deaths and cases
dfdc <- merge(dfd, dfc, by = "IdLandkreis")



dfpop <- dfdd[, c("KRS", "population", "fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")]
dfdc <- merge(dfdc, dfpop, by.dfpop = "KRS", by.dfdc = "IdLandkreis")

# re-change columns to numerics
i <- c(2:205)
dfdc[ , i] <- apply(dfdc[ , i], 2, function(x) as.numeric(as.character(x)))

# aggregate cases
dfdc$deaths <- rowSums(dfdc[,c(2:103)])
dfdc$cases <- rowSums(dfdc[,c(104:205)])

list1 <- c("cases20.02","cases20.03","cases20.04","cases20.05","cases20.06",
           "cases20.07","cases20.08","cases20.09","cases20.10","cases20.11","cases20.12",
           "cases21.01","cases21.02","cases21.03","cases21.04","cases21.05","cases21.06")
list2 <- c("deaths20.02","deaths20.03","deaths20.04","deaths20.05","deaths20.06",
           "deaths20.07","deaths20.08","deaths20.09","deaths20.10","deaths20.11","deaths20.12",
           "deaths21.01","deaths21.02","deaths21.03","deaths21.04","deaths21.05","deaths21.06")










for(i in 17){
#  a <- c(i+1,i+1+17,i+1+17*2,i+1+17*3,i+1+17*4,i+1+17*5)
  a <- dfdc[,i+1] + dfdc[,i+1+17] + dfdc[,i+1+17*2] + dfdc[,i+1+17*3] + dfdc[,i+1+17*4] + dfdc[,i+1+17*5] + dfdc[,i+1+17*6]
  a <- as.data.frame(a)
  c <- rbind(c,a)
}


dfdc$cases20.02 <- dfdc[,1+1] + dfdc[,1+1+17] + dfdc[,1+1+17*2] + dfdc[,1+1+17*3] + dfdc[,1+1+17*4] + dfdc[,1+1+17*5] + dfdc[,1+1+17*6]
dfdc$cases20.03 <- dfdc[,2+1] + dfdc[,2+1+17] + dfdc[,2+1+17*2] + dfdc[,2+1+17*3] + dfdc[,2+1+17*4] + dfdc[,2+1+17*5] + dfdc[,2+1+17*6]
dfdc$cases20.04 <- dfdc[,3+1] + dfdc[,3+1+17] + dfdc[,3+1+17*2] + dfdc[,3+1+17*3] + dfdc[,3+1+17*4] + dfdc[,3+1+17*5] + dfdc[,3+1+17*6]
dfdc$cases20.05 <- dfdc[,4+1] + dfdc[,4+1+17] + dfdc[,4+1+17*2] + dfdc[,4+1+17*3] + dfdc[,4+1+17*4] + dfdc[,4+1+17*5] + dfdc[,4+1+17*6]
dfdc$cases20.06 <- dfdc[,5+1] + dfdc[,5+1+17] + dfdc[,5+1+17*2] + dfdc[,5+1+17*3] + dfdc[,5+1+17*4] + dfdc[,5+1+17*5] + dfdc[,5+1+17*6]
dfdc$cases20.07 <- dfdc[,1+1] + dfdc[,1+1+17] + dfdc[,1+1+17*2] + dfdc[,1+1+17*3] + dfdc[,1+1+17*4] + dfdc[,1+1+17*5] + dfdc[,1+1+17*6]
dfdc$cases20.08 <- dfdc[,2+1] + dfdc[,2+1+17] + dfdc[,2+1+17*2] + dfdc[,2+1+17*3] + dfdc[,2+1+17*4] + dfdc[,2+1+17*5] + dfdc[,2+1+17*6]
dfdc$cases20.09 <- dfdc[,3+1] + dfdc[,3+1+17] + dfdc[,3+1+17*2] + dfdc[,3+1+17*3] + dfdc[,3+1+17*4] + dfdc[,3+1+17*5] + dfdc[,3+1+17*6]
dfdc$cases20.10 <- dfdc[,4+1] + dfdc[,4+1+17] + dfdc[,4+1+17*2] + dfdc[,4+1+17*3] + dfdc[,4+1+17*4] + dfdc[,4+1+17*5] + dfdc[,4+1+17*6]
dfdc$cases20.11 <- dfdc[,5+1] + dfdc[,5+1+17] + dfdc[,5+1+17*2] + dfdc[,5+1+17*3] + dfdc[,5+1+17*4] + dfdc[,5+1+17*5] + dfdc[,5+1+17*6]
dfdc$cases20.12 <- dfdc[,1+1] + dfdc[,1+1+17] + dfdc[,1+1+17*2] + dfdc[,1+1+17*3] + dfdc[,1+1+17*4] + dfdc[,1+1+17*5] + dfdc[,1+1+17*6]
dfdc$cases21.01 <- dfdc[,2+1] + dfdc[,2+1+17] + dfdc[,2+1+17*2] + dfdc[,2+1+17*3] + dfdc[,2+1+17*4] + dfdc[,2+1+17*5] + dfdc[,2+1+17*6]
dfdc$cases21.02 <- dfdc[,3+1] + dfdc[,3+1+17] + dfdc[,3+1+17*2] + dfdc[,3+1+17*3] + dfdc[,3+1+17*4] + dfdc[,3+1+17*5] + dfdc[,3+1+17*6]
dfdc$cases21.03 <- dfdc[,4+1] + dfdc[,4+1+17] + dfdc[,4+1+17*2] + dfdc[,4+1+17*3] + dfdc[,4+1+17*4] + dfdc[,4+1+17*5] + dfdc[,4+1+17*6]
dfdc$cases21.04 <- dfdc[,5+1] + dfdc[,5+1+17] + dfdc[,5+1+17*2] + dfdc[,5+1+17*3] + dfdc[,5+1+17*4] + dfdc[,5+1+17*5] + dfdc[,5+1+17*6]
dfdc$cases21.05 <- dfdc[,1+1] + dfdc[,1+1+17] + dfdc[,1+1+17*2] + dfdc[,1+1+17*3] + dfdc[,1+1+17*4] + dfdc[,1+1+17*5] + dfdc[,1+1+17*6]
dfdc$cases21.06 <- dfdc[,2+1] + dfdc[,2+1+17] + dfdc[,2+1+17*2] + dfdc[,2+1+17*3] + dfdc[,2+1+17*4] + dfdc[,2+1+17*5] + dfdc[,2+1+17*6]







# names
colnames(dfvacc2) <- c("KRS","vacc20.12","vacc21.01","vacc21.02","vacc21.03","vacc21.04","vacc21.05","vacc21.06",
                       "vacc21.07","vacc21.08","vacc21.09","vacc21.10") 



# calculating sums: one crude sum, one standardised EU sum, IR and CFR both with and without EU weights
for (i in 1:18){
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
  colnames(a) <- paste(colnames(a), rkilist2[[i]], sep = "")
  namescol <- colnames(a)
  names(a)[names(a)==namescol[[1]]] <- "KRS"
  a <- a[,c(1,14,15,23:27)]
  assign(paste("dfrkiaggr", rkilist2[[i]], sep=""), a)
}
























