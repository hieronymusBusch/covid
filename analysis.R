##########################################################
#                                                        #
# BA Sociology:                                          #
# "COVID-19 in Germany, a social-geographic perspective" #
#                                                        #
# 2021/04                                                #
#                                                        #
# Alexander Busch (alexander.busch@stud.uni-hd.de)       #
#                                                        #
# Data Analysis                                          #
#                                                        #
##########################################################

source("manipulation.R")

# Varlist for Models
varlistLNIR <- c("LNIR20.02","LNIR20.03","LNIR20.04","LNIR20.05","LNIR20.06",
                   "LNIR20.07","LNIR20.08","LNIR20.09","LNIR20.10",
                   "LNIR20.11","LNIR20.12","LNIR21.01","LNIR21.02",
                   "LNIR21.03","LNIR21.04","LNIR21.05","LNIR21.06")
varlistLNIREU <- c("LNIREU20.02","LNIREU20.03","LNIREU20.04","LNIREU20.05","LNIREU20.06",
                 "LNIREU20.07","LNIREU20.08","LNIREU20.09","LNIREU20.10",
                 "LNIREU20.11","LNIREU20.12","LNIREU21.01","LNIREU21.02",
                 "LNIREU21.03","LNIREU21.04","LNIREU21.05","LNIREU21.06")
varlistCFREU <- c("CFREU20.02","CFREU20.03","CFREU20.04","CFREU20.05","CFREU20.06",
                "CFREU20.07","CFREU20.08","CFREU20.09","CFREU20.10",
                "CFREU20.11","CFREU20.12","CFREU21.01","CFREU21.02",
                "CFREU21.03","CFREU21.04","CFREU21.05","CFREU21.06")
varlistCFREUlag <- c("CFREUlag20.04","CFREUlag20.05","CFREUlag20.06",
                   "CFREUlag20.07","CFREUlag20.08","CFREUlag20.09","CFREUlag20.10",
                   "CFREUlag20.11","CFREUlag20.12","CFREUlag21.01","CFREUlag21.02",
                   "CFREUlag21.03","CFREUlag21.04","CFREUlag21.05","CFREUlag21.06")
varlistControlStates <- c("SH","HH","NI","HB","MV","BB","BE","ST","SN","TH","NW","HE","RP","BY","SL")
varlistControlForeign <- c("GISD","shareForeign","shareWomen")
varlistControlEast <- c("GISD","east","shareWomen")
varlistControlForeign2 <- c("GISD","shareWomen")
varlistControlEast2 <- c("GISD","shareWomen")
month <- c("02/20","03/20","04/20","05/20","06/20","07/20","08/20","09/20","10/20",
           "11/20","12/20","01/21","02/21","03/21","04/21","05/21", "06/21")

# Create lists needed for analysis
neighbors <- poly2nb(dfds)
weighted_neighbors <- nb2listw(neighbors, zero.policy=T)
weighted_neighbors






### Analysis

## Test for spatial Autocorrelation in data
moran.test(dfds$IREU, weighted_neighbors, zero.policy=T)
moran.test(dfds$IREU20.04, weighted_neighbors, zero.policy=T)
moran.test(dfds$IREU21.04, weighted_neighbors, zero.policy=T)
moran.plot(dfds$IREU, weighted_neighbors, zero.policy=T,xlab="Incidence Rates", ylab="spatially lagged IR")

moran.test(dfds$CFREU, weighted_neighbors, zero.policy=T)
moran.test(dfds$CFREU20.04, weighted_neighbors, zero.policy=T)
moran.test(dfds$CFREU21.04, weighted_neighbors, zero.policy=T)
moran.plot(dfds$CFREU, weighted_neighbors, zero.policy=T)
moran.plot(dfds$CFREU21.04, weighted_neighbors, zero.policy=T,xlab="Case fatality ratio", ylab="spatially lagged CFR")



## Hypothesis I, IR ~ GISD
dfIREU.GISD <- SARvarlist("GISD",varlistLNIREU)
dfIR.GISD <- SARvarlist("GISD",varlistLNIR)
dfIREUols.GISD <- OLSvarlist("GISD",varlistLNIREU)
dfIR <- merge(dfIREU.GISD, dfIR.GISD, by.dfIREU.GISD = month, by.dfIR.GISD = month)
dfIR <- merge(dfIR, dfIREUols.GISD, by.dfIR = month, by.dfIREUols.GISD = month)
dfIR











displayCoeff(dfIR.GISD,"LN IR ~ GISD, Average Total Effects (SAR)", "Average Total Effect")
dfIR.GISD

dfIR.unempl <- SARvarlist2("unemployment",varlistLNIR, varlistControl)
displayCoeff(dfIR.unempl,"LN IR ~ unemployment, Average Total Effects (SAR)", "Average Total Effect")
dfIR.unempl

dfIR.LNmedInc <- SARvarlist2("LNmedInc",varlistLNIR, varlistControl)
displayCoeff(dfIR.LNmedInc,"LN IR ~ LN Median Income, Average Total Effects (SAR)", "Average Total Effect")
dfIR.LNmedInc

dfIR.workersAcadem <- SARvarlist2("workersAcadem",varlistLNIR, varlistControl)
displayCoeff(dfIR.workersAcadem,"LN IR ~ Workers Academic Education, Average Total Effects (SAR)", "Average Total Effect")
dfIR.workersAcadem


ggplot(dfdd, aes(x = GISD, y = LNIR20.04)) +
  geom_point() +
  xlab("GISD") +
  ylab("LN IR") +
  theme_bw()

## Hypothesis I, CFR ~ GISD
dfCFR.GISD <- SARvarlist("GISD",varlistCFR)
displayCoeff(dfCFR.GISD,"CFR ~ GISD, Average Total Effects (SAR)", "Average Total Effect")
dfCFR.GISD

dfCFR.GISD2 <- OLSvarlist2("GISD",varlistCFR, varlistControl)
displayCoeff(dfCFR.GISD2,"CFR ~ GISD, OLS Coefficients", "Coefficient")
dfCFR.GISD2

dfCFR.unempl <- OLSvarlist2("unemployment",varlistCFR, varlistControl)
displayCoeff(dfCFR.unempl,"CFR ~ unemployment, Regression Coefficient (OLS)", "Regression Coefficient")
dfCFR.unempl

dfCFR.LNmedInc <- OLSvarlist2("LNmedInc",varlistCFR, varlistControl)
displayCoeff(dfCFR.LNmedInc,"CFR ~ LN Median Income, Regression Coefficient (OLS)", "Regression Coefficient")
dfCFR.LNmedInc

dfCFR.workersAcadem <- OLSvarlist2("workersAcadem",varlistCFR, varlistControl)
displayCoeff(dfCFR.workersAcadem,"CFR ~ Workers Academic Education, Regression Coefficient (OLS)", "Regression Coefficient")
dfCFR.workersAcadem


## Hypothesis E2, CFR ~ Healthcare facilities
dfCFR.popPerDoc <- OLSvarlist2("LNpopPerDoc",varlistCFR, varlistControl)
displayCoeff(dfCFR.popPerDoc,"CFR ~ LN People per Doctor, Regression Coefficient (OLS)", "Regression Coefficient")
dfCFR.popPerDoc

dfCFR.hospBeds <- OLSvarlist2("hospBeds",varlistCFR, varlistControl)
displayCoeff(dfCFR.hospBeds,"CFR ~ Hospital Beds p.c., Regression Coefficient (OLS)", "Regression Coefficient")
dfCFR.hospBeds

## Robustness Check 
#CFR lagged
dfCFRlag.GISD <- OLSvarlist4("GISD",varlistCFRlag, varlistControl)
displayCoeff(dfCFRlag.GISD,"CFR (lagged) ~ GISD, Average Total Effects (SAR)", "Average Total Effect")
dfCFRlag.GISD
#IR with states
dfIR.GISD2 <- SARvarlist2("GISD",varlistLNIR, varlistControlStates)
displayCoeff(dfIR.GISD2,"LN IR ~ GISD, Average Total Effects (SAR)", "Average Total Effect")
dfIR.GISD2
#CFR with states
dfCFR.GISD3 <- SARvarlist2("GISD",varlistCFR, varlistControlStates)
displayCoeff(dfCFR.GISD3,"CFR ~ GISD, Average Total Effects (SAR)", "Average Total Effect")
dfCFR.GISD3


# normal distribution assumption
hist(dfdd$IR, breaks = c(500,1000,1500,2000,2500,3000,3500,4000,4500,
                         5000,5500,6000,6500,7000,7500,8000,8500,9000)) 
hist(dfdd$CFR) 
hist(dfdd$IR) 
hist(dfdd$IR21.04) 
shapiro.test(dfdd$IR) 

plot(dfdd$GISD,dfdd$IR)





### Tables etc. for LaTeX Output

# table with independent variables 
stargazer(dfdd[c("GISD","unemployment","medInc","workersAcadem","popDensity",
                 "population", "shareWomen","avgAge","shareForeign",
                 "AfD","hospBeds","popPerDoc")], 
          type = "latex", digits=1,flip = FALSE, omit.summary.stat = 
            c("p25","p75"))

# table with dependent variables LONG
stargazer(dfdd[c("CFR","IR","aggrCases","aggrDeaths","CFR20.02",
                 "CFR20.03","CFR20.04","CFR20.05","CFR20.06",
                 "CFR20.07","CFR20.08","CFR20.09","CFR20.10",
                 "CFR20.11","CFR20.12","CFR21.01","CFR21.02",
                 "CFR21.03","CFR21.04","CFR21.05",
                 "IR20.02","IR20.03","IR20.04","IR20.05","IR20.06",
                 "IR20.07","IR20.08","IR20.09","IR20.10",
                 "IR20.11","IR20.12","IR21.01","IR21.02",
                 "IR21.03","IR21.04","IR21.05")], 
          type = "latex", digits=2,flip = FALSE, omit.summary.stat = 
            c("median","p25","p75"))

# SAR effects LN IR ~ GISD
effects_LNIR.GISD <- SARvarlist3("GISD",varlistLNIR, varlistControl)
effects_LNIR.GISD[["LNIR20.02"]]
effects_LNIR.GISD[["LNIR20.03"]]
effects_LNIR.GISD[["LNIR20.04"]]
effects_LNIR.GISD[["LNIR20.05"]]
effects_LNIR.GISD[["LNIR20.06"]]
effects_LNIR.GISD[["LNIR20.07"]]
effects_LNIR.GISD[["LNIR20.08"]]
effects_LNIR.GISD[["LNIR20.09"]]
effects_LNIR.GISD[["LNIR20.10"]]
effects_LNIR.GISD[["LNIR20.11"]]
effects_LNIR.GISD[["LNIR20.12"]]
effects_LNIR.GISD[["LNIR21.01"]]
effects_LNIR.GISD[["LNIR21.02"]]
effects_LNIR.GISD[["LNIR21.03"]]
effects_LNIR.GISD[["LNIR21.04"]]
effects_LNIR.GISD[["LNIR21.05"]]

#Maps
plot(dfds["IREU"], key.pos = 4, nbreaks = 10,border="white")
plot(dfds["CFREU"], key.pos = 4, nbreaks = 10,border="white")
plot(dfds["GISD"], key.pos = 4, nbreaks = 10,border="white")

#Course of Pandemic in Germany
dfaggrCases <- dfrkiaggr[,c("aggrCases20.02","aggrCases20.03","aggrCases20.04","aggrCases20.05","aggrCases20.06",
                            "aggrCases20.07","aggrCases20.08","aggrCases20.09","aggrCases20.10",
                            "aggrCases20.11","aggrCases20.12","aggrCases21.01","aggrCases21.02",
                            "aggrCases21.03","aggrCases21.04","aggrCases21.05")]
dfaggrCases$population <- dfdd$population
dfaggrCasesSum <- colSums(dfaggrCases)
dfaggrCasesSum <- data.frame(dfaggrCasesSum)
popSum <- as.numeric(dfaggrCasesSum[17])
dfaggrCasesSum <- dfaggrCasesSum[-17]
dfaggrCasesSum <- data.frame(dfaggrCasesSum)
dfaggrCasesSum$month <- month
names(dfaggrCasesSum)[names(dfaggrCasesSum)=="dfaggrCasesSum"] <- "IR"
dfaggrCasesSum$IR <- (dfaggrCasesSum$IR / popSum) * 100000
dfaggrCasesSum

dfaggrCFR <- dfrkiaggr[,c("aggrCases20.02","aggrCases20.03","aggrCases20.04","aggrCases20.05","aggrCases20.06",
                          "aggrCases20.07","aggrCases20.08","aggrCases20.09","aggrCases20.10",
                          "aggrCases20.11","aggrCases20.12","aggrCases21.01","aggrCases21.02",
                          "aggrCases21.03","aggrCases21.04","aggrCases21.05", 
                          "aggrDeaths20.02","aggrDeaths20.03","aggrDeaths20.04","aggrDeaths20.05","aggrDeaths20.06",
                          "aggrDeaths20.07","aggrDeaths20.08","aggrDeaths20.09","aggrDeaths20.10",
                          "aggrDeaths20.11","aggrDeaths20.12","aggrDeaths21.01","aggrDeaths21.02",
                          "aggrDeaths21.03","aggrDeaths21.04","aggrDeaths21.05"
)]
dfaggrCFRSum <- colSums(dfaggrCFR)
dfaggrCFRSum <- data.frame(dfaggrCFRSum)
dfaggrCFRSum <- data.frame(dfaggrCFRSum)
dfaggrCFRSum$cases <- dfaggrCFRSum[1:16,1]
dfaggrCFRSum$deaths <- dfaggrCFRSum[17:32,1]
dfaggrCFRSum$dfaggrCFRSum <- NULL
dfaggrCFRSum <- dfaggrCFRSum[-(17:32),]
dfaggrCFRSum$month <- month
dfaggrCFRSum$CFR <- dfaggrCFRSum$deaths / dfaggrCFRSum$cases
dfaggrCFRSum

ggplot(dfaggrCFRSum, aes(x=month, y=CFR, group=1)) +
  geom_line() +
  geom_point() +
  scale_y_continuous()+ 
  scale_x_discrete(limits=dfaggrCFRSum$month)+
  labs(title="Average county-CFR per month",x="Month", y = "CFR")+
  theme_bw()


ggplot(dfaggrCasesSum, aes(x=month, y=IR, group=1)) +
  geom_line() +
  geom_point() +
  scale_y_continuous()+ 
  scale_x_discrete(limits=dfaggrCasesSum$month)+
  labs(title="Average county-IR per month",x="Month", y = "IR")+
  theme_bw()

