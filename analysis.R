##########################################################
#                                                        #
# BA Sociology:                                          #
# "COVID-19 in Germany, a socioeconomic-geographic perspective" #
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
varlistCFR <- c("CFR20.02","CFR20.03","CFR20.04","CFR20.05","CFR20.06",
                  "CFR20.07","CFR20.08","CFR20.09","CFR20.10",
                  "CFR20.11","CFR20.12","CFR21.01","CFR21.02",
                  "CFR21.03","CFR21.04","CFR21.05","CFR21.06")
varlistCFREU_ <- c("CFREU20.04","CFREU20.05","CFREU20.06",
                  "CFREU20.07","CFREU20.08","CFREU20.09","CFREU20.10",
                  "CFREU20.11","CFREU20.12","CFREU21.01","CFREU21.02",
                  "CFREU21.03","CFREU21.04","CFREU21.05","CFREU21.06")
varlistCFR_ <- c("CFR20.04","CFR20.05","CFR20.06",
                "CFR20.07","CFR20.08","CFR20.09","CFR20.10",
                "CFR20.11","CFR20.12","CFR21.01","CFR21.02",
                "CFR21.03","CFR21.04","CFR21.05","CFR21.06")
varlistCFREUlag <- c("CFREUlag20.04","CFREUlag20.05","CFREUlag20.06",
                   "CFREUlag20.07","CFREUlag20.08","CFREUlag20.09","CFREUlag20.10",
                   "CFREUlag20.11","CFREUlag20.12","CFREUlag21.01","CFREUlag21.02",
                   "CFREUlag21.03","CFREUlag21.04","CFREUlag21.05","CFREUlag21.06")
varlistControl <- c("popDensity","SH","HH","NI","HB","MV","BB","BE","ST","SN","TH","NW","HE","RP","BY","SL")
varlistControl2 <- c("SH","HH","NI","HB","MV","BB","BE","ST","SN","TH","NW","HE","RP","BY","SL")

month <- c("20-02", "20-03", "20-04", "20-05", "20-06", "20-07", "20-08", "20-09", 
           "20-10", "20-11", "20-12", "21-01", "21-02", "21-03", "21-04", "21-05", "21-06")

# Create lists needed for analysis
neighbors <- poly2nb(dfds)
weighted_neighbors <- nb2listw(neighbors, zero.policy=T)
weighted_neighbors






### Analysis

## Test for spatial Autocorrelation in data
moran.test(dfds$IREU, weighted_neighbors, zero.policy=T)
moran.test(dfds$IREU20.04, weighted_neighbors, zero.policy=T)
moran.test(dfds$IREU21.04, weighted_neighbors, zero.policy=T)
moran.plot(dfds$IREU, weighted_neighbors, zero.policy=T,xlab="incidence rates, age standardised", ylab="spatially lagged IR")

moran.test(dfds$CFREU, weighted_neighbors, zero.policy=T)
moran.test(dfds$CFREU20.04, weighted_neighbors, zero.policy=T)
moran.test(dfds$CFREU21.04, weighted_neighbors, zero.policy=T)
moran.plot(dfds$CFREU, weighted_neighbors, zero.policy=T)
moran.plot(dfds$CFREU21.04, weighted_neighbors, zero.policy=T,xlab="case fatality ratios, age standardised", ylab="spatially lagged CFR")


## Hypothesis I, IR ~ GISD
dfIREU <- SARvarlistC("GISD",varlistLNIREU,varlistControl)
names(dfIREU)[names(dfIREU)=="coefficientReg"] <- "SAR, age standardised"
dfIR2 <- SARvarlistC("GISD",varlistLNIR,varlistControl)
names(dfIR2)[names(dfIR2)=="coefficientReg"] <- "SAR, not age standardised"
dfIREUOLS <- OLSvarlistC("GISD",varlistLNIREU,varlistControl)
names(dfIREUOLS)[names(dfIREUOLS)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfIR <- merge(dfIREU, dfIR2, by.dfIREU = month, by.dfIR2 = month)
dfIR <- merge(dfIR, dfIREUOLS, by.dfIR = month, by.dfIREUOLS = month)
dfIR <- reshape(dfIR, times = c("SAR, age standardised", "SAR, not age standardised", 
                                "Lin. Reg., age standardised"), 
                varying = c("SAR, age standardised", "SAR, not age standardised", 
                            "Lin. Reg., age standardised"),  
                idvar = "month", v.name="coefficients", direction = "long")
names(dfIR)[names(dfIR)=="time"] <- "model"


ggplot(dfIR, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8) +
  geom_hline(aes(yintercept = 0)) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "log. Incidence Rates ~ German Index of socioeconomic Deprivation + pop. Density + state dummies", 
       caption = "Read: ''The most socioeconomicly deprived
                  county has a 1.5% higher incidence rate compared
                 to the least socioeconomicly deprived county as of March 21''")+
  theme_bw()

## Hypothesis I, IR ~ unemployment
dfIREUu <- SARvarlistC("unemployment",varlistLNIREU,varlistControl)
names(dfIREUu)[names(dfIREUu)=="coefficientReg"] <- "SAR, age standardised"
dfIRu <- SARvarlistC("unemployment",varlistLNIR,varlistControl)
names(dfIRu)[names(dfIRu)=="coefficientReg"] <- "SAR, not age standardised"
dfIREUOLSu <- OLSvarlistC("unemployment",varlistLNIREU,varlistControl)
names(dfIREUOLSu)[names(dfIREUOLSu)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfIRu <- merge(dfIREUu, dfIRu, by.dfIREUu = month, by.dfIRu = month)
dfIRu <- merge(dfIRu, dfIREUOLSu, by.dfIRu = month, by.dfIREUOLSu = month)
dfIRu <- reshape(dfIRu, times = c("SAR, age standardised", "SAR, not age standardised", 
                                "Lin. Reg., age standardised"), 
                varying = c("SAR, age standardised", "SAR, not age standardised", 
                            "Lin. Reg., age standardised"),  
                idvar = "month", v.name="coefficients", direction = "long")
names(dfIRu)[names(dfIRu)=="time"] <- "model"

ggplot(dfIRu, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8) +
  geom_hline(aes(yintercept = 0)) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "log. Incidence Rates ~ unemployment + pop. Density + state dummies", 
       caption = "Read: ''In the SAR models, a 1 percent point increase in the unemployment rate
                  is associated with a 0.15% increase in incidence rates as of 
                  March 21''")+
  theme_bw()


## Hypothesis I, IR ~ education
dfIREUe <- SARvarlistC("workersAcadem",varlistLNIREU,varlistControl)
names(dfIREUe)[names(dfIREUe)=="coefficientReg"] <- "SAR, age standardised"
dfIRe <- SARvarlistC("workersAcadem",varlistLNIR,varlistControl)
names(dfIRe)[names(dfIRe)=="coefficientReg"] <- "SAR, not age standardised"
dfIREUOLSe <- OLSvarlistC("workersAcadem",varlistLNIREU,varlistControl)
names(dfIREUOLSe)[names(dfIREUOLSe)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfIRe <- merge(dfIREUe, dfIRe, by.dfIREUe = month, by.dfIRe = month)
dfIRe <- merge(dfIRe, dfIREUOLSe, by.dfIRe = month, by.dfIREUOLSe = month)
dfIRe <- reshape(dfIRe, times = c("SAR, age standardised", "SAR, not age standardised", 
                                  "Lin. Reg., age standardised"), 
                 varying = c("SAR, age standardised", "SAR, not age standardised", 
                             "Lin. Reg., age standardised"),  
                 idvar = "month", v.name="coefficients", direction = "long")
names(dfIRe)[names(dfIRe)=="time"] <- "model"

ggplot(dfIRe, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8) +
  geom_hline(aes(yintercept = 0)) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "log. Incidence Rates ~ workers with college education + pop. Density + state dummies", 
       caption = "Read: ''In the SAR models, a 1 percentage point increase in workers 
                  with college education is associated with a 0.05% decrease in incidence rates as of 
                  March 21''" )+
  theme_bw()

## Hypothesis I, IR ~ income
dfIREUi <- SARvarlistC("LNmedInc",varlistLNIREU,varlistControl)
names(dfIREUi)[names(dfIREUi)=="coefficientReg"] <- "SAR, age standardised"
dfIRi <- SARvarlistC("LNmedInc",varlistLNIR,varlistControl)
names(dfIRi)[names(dfIRi)=="coefficientReg"] <- "SAR, not age standardised"
dfIREUOLSi <- OLSvarlistC("LNmedInc",varlistLNIREU,varlistControl)
names(dfIREUOLSi)[names(dfIREUOLSi)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfIRi <- merge(dfIREUi, dfIRi, by.dfIREUi = month, by.dfIRi = month)
dfIRi <- merge(dfIRi, dfIREUOLSi, by.dfIRi = month, by.dfIREUOLSi = month)
dfIRi <- reshape(dfIRi, times = c("SAR, age standardised", "SAR, not age standardised", 
                                  "Lin. Reg., age standardised"), 
                 varying = c("SAR, age standardised", "SAR, not age standardised", 
                             "Lin. Reg., age standardised"),  
                 idvar = "month", v.name="coefficients", direction = "long")
names(dfIRi)[names(dfIRi)=="time"] <- "model"

ggplot(dfIRi, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8) +
  geom_hline(aes(yintercept = 0)) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "log. Incidence Rates ~ log. median income + pop. Density + state dummies", 
       caption = "Read: ''In the SAR models, a 1% increase in the county median wage
                  is associated with a 0.5% decrease in incidence rates as of 
                  March 21''" )+
  theme_bw()





## Hypothesis I, CFR ~ GISD
dfCFREU <- SARvarlist("GISD",varlistCFREU)
names(dfCFREU)[names(dfCFREU)=="coefficientReg"] <- "SAR, age standardised"
dfCFR <- SARvarlist("GISD",varlistCFR)
names(dfCFR)[names(dfCFR)=="coefficientReg"] <- "SAR, not age standardised"
dfCFREUOLS <- OLSvarlist("GISD",varlistCFREU)
names(dfCFREUOLS)[names(dfCFREUOLS)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfCFR <- merge(dfCFREU, dfCFR, by.dfCFREU = month, by.dfCFR = month)
dfCFR <- merge(dfCFR, dfCFREUOLS, by.dfCFR = month, by.dfCFREUOLS = month)
dfCFR <- reshape(dfCFR, times = c("SAR, age standardised", "SAR, not age standardised", 
                                  "Lin. Reg., age standardised"), 
                 varying = c("SAR, age standardised", "SAR, not age standardised", 
                             "Lin. Reg., age standardised"), 
                 idvar = "month", v.name="coefficients", direction = "long")
names(dfCFR)[names(dfCFR)=="time"] <- "model"

ggplot(dfCFR, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8) +
  geom_hline(aes(yintercept = 0)) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "case fatality ratio ~ German Index of Socioeconomic Deprivation", 
       caption = "Read: ''In the age standardised models, the most socioeconomicly deprived
                  county has a 2 percentage points higher case fatality ratio compared
                  to the least socioeconomicly deprived county as of March 21''" )+
  theme_bw()

## Hypothesis I, CFR ~ unemployment
dfCFREUu <- SARvarlist("unemployment",varlistCFREU)
names(dfCFREUu)[names(dfCFREUu)=="coefficientReg"] <- "SAR, age standardised"
dfCFRu <- SARvarlist("unemployment",varlistCFR)
names(dfCFRu)[names(dfCFRu)=="coefficientReg"] <- "SAR, not age standardised"
dfCFREUOLSu <- OLSvarlist("unemployment",varlistCFREU)
names(dfCFREUOLSu)[names(dfCFREUOLSu)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfCFRu <- merge(dfCFREUu, dfCFRu, by.dfCFREUu = month, by.dfCFRu = month)
dfCFRu <- merge(dfCFRu, dfCFREUOLSu, by.dfCFRu = month, by.dfCFREUOLSu = month)
dfCFRu <- reshape(dfCFRu, times = c("SAR, age standardised", "SAR, not age standardised", 
                                  "Lin. Reg., age standardised"), 
                 varying = c("SAR, age standardised", "SAR, not age standardised", 
                             "Lin. Reg., age standardised"),  
                 idvar = "month", v.name="coefficients", direction = "long")
names(dfCFRu)[names(dfCFRu)=="time"] <- "model"

ggplot(dfCFRu, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8) +
  geom_hline(aes(yintercept = 0)) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "case fatality ratio ~ unemployment in %", 
       caption = "Read: ''In the age standardised models, a 1 percentage point increase 
                  in unemployment is associated with a 0.05 percentage point increase 
                  in case fatality ratio as of March 21''" )+
  theme_bw()


## Hypothesis I, CFR ~ education
dfCFREUe <- SARvarlist("workersAcadem",varlistCFREU)
names(dfCFREUe)[names(dfCFREUe)=="coefficientReg"] <- "SAR, age standardised"
dfCFRe <- SARvarlist("workersAcadem",varlistCFR)
names(dfCFRe)[names(dfCFRe)=="coefficientReg"] <- "SAR, not age standardised"
dfCFREUOLSe <- OLSvarlist("workersAcadem",varlistCFREU)
names(dfCFREUOLSe)[names(dfCFREUOLSe)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfCFRe <- merge(dfCFREUe, dfCFRe, by.dfCFREUe = month, by.dfCFRe = month)
dfCFRe <- merge(dfCFRe, dfCFREUOLSe, by.dfCFRe = month, by.dfCFREUOLSe = month)
dfCFRe <- reshape(dfCFRe, times = c("SAR, age standardised", "SAR, not age standardised", 
                                  "Lin. Reg., age standardised"), 
                 varying = c("SAR, age standardised", "SAR, not age standardised", 
                             "Lin. Reg., age standardised"),  
                 idvar = "month", v.name="coefficients", direction = "long")
names(dfCFRe)[names(dfCFRe)=="time"] <- "model"

ggplot(dfCFRe, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8) +
  geom_hline(aes(yintercept = 0)) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "case fatality ratio ~ workers with college education in %", 
       caption = "Read: ''In the age standardised models, a 1 percentage point
                  increase in college educated workers is associated with a 0.01 
                  percentage point decrease in case fatality ratio as of March 21''" )+
  theme_bw()

## Hypothesis I, CFR ~ income
dfCFREUi <- SARvarlist("LNmedInc",varlistCFREU)
names(dfCFREUi)[names(dfCFREUi)=="coefficientReg"] <- "SAR, age standardised"
dfCFRi <- SARvarlist("LNmedInc",varlistCFR)
names(dfCFRi)[names(dfCFRi)=="coefficientReg"] <- "SAR, not age standardised"
dfCFREUOLSi <- OLSvarlist("LNmedInc",varlistCFREU)
names(dfCFREUOLSi)[names(dfCFREUOLSi)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfCFRi <- merge(dfCFREUi, dfCFRi, by.dfCFREUi = month, by.dfCFRi = month)
dfCFRi <- merge(dfCFRi, dfCFREUOLSi, by.dfCFRi = month, by.dfCFREUOLSi = month)
dfCFRi <- reshape(dfCFRi, times = c("SAR, age standardised", "SAR, not age standardised", 
                                  "Lin. Reg., age standardised"), 
                 varying = c("SAR, age standardised", "SAR, not age standardised", 
                             "Lin. Reg., age standardised"),  
                 idvar = "month", v.name="coefficients", direction = "long")
names(dfCFRi)[names(dfCFRi)=="time"] <- "model"

ggplot(dfCFRi, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8) +
  geom_hline(aes(yintercept = 0)) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "case fatality ratio ~ log. median income", 
       caption = "Read: ''In the age standardised models, a 1% increase county median income
                  is associated with a 0.0005 percentage point decrease in case fatality ratio as of 
                  March 21''" ) +
  theme_bw()


lag <- lagsarlm(CFREU21.02 ~ LNmedInc, data=dfds, listw = weighted_neighbors,
               tol.solve=1.0e-30, zero.policy=T)
impacts(lag, listw = weighted_neighbors)

#### Robustness checks

## lagged CFR

## path dependency













### Tables etc. for LaTeX Output

# table with independent variables 
stargazer(dfdd[c("GISD","unemployment","medInc","workersAcadem","popDensity",
                 "population")], 
          type = "latex", digits=1,flip = FALSE, omit.summary.stat = 
            c("p25","p75"))

# table with dependent variables LONG
stargazer(dfdd[c("CFREU","IR","cases","deaths","CFREU20.02",
                 "CFREU20.03","CFREU20.04","CFREU20.05","CFREU20.06",
                 "CFREU20.07","CFREU20.08","CFREU20.09","CFREU20.10",
                 "CFREU20.11","CFREU20.12","CFREU21.01","CFREU21.02",
                 "CFREU21.03","CFREU21.04","CFREU21.05",
                 "IREU20.02","IREU20.03","IREU20.04","IREU20.05","IREU20.06",
                 "IREU20.07","IREU20.08","IREU20.09","IREU20.10",
                 "IREU20.11","IREU20.12","IREU21.01","IREU21.02",
                 "IREU21.03","IREU21.04","IREU21.05")], 
          type = "latex", digits=2,flip = FALSE, omit.summary.stat = 
            c("median","p25","p75"))

#Maps
dfdsMAP <- dfds[,c("geometry", "KRS", "IREU", "IREU20.04", "IREU21.04", "GISD")]
names(dfdsMAP) <- c("geometry", "KRS", "Incidence Rates, age standardised", 
                    "Incidence Rates, age standardised, April 2020", 
                    "Incidence Rates, age standardised, April 2021", 
                    "German Index of Socioeconomic Deprivation")


plot(dfdsMAP["Incidence Rates, age standardised"], key.pos = 4, nbreaks = 10,border="white")
plot(dfdsMAP["Incidence Rates, age standardised, April 2020"], key.pos = 4, nbreaks = 10,border="white")
plot(dfdsMAP["Incidence Rates, age standardised, April 2021"], key.pos = 4, nbreaks = 10,border="white")
plot(dfdsMAP["German Index of Socioeconomic Deprivation"], key.pos = 4, nbreaks = 10,border="white")

#Course of Pandemic in Germany
dfcases <- dfrkiaggr[,c("cases20.02","cases20.03","cases20.04","cases20.05","cases20.06",
                            "cases20.07","cases20.08","cases20.09","cases20.10",
                            "cases20.11","cases20.12","cases21.01","cases21.02",
                            "cases21.03","cases21.04","cases21.05","cases21.06")]
dfcases$population <- dfdd$population
dfcasesSum <- colSums(dfcases)
popSum <- as.numeric(dfcasesSum[[18]])
dfcasesSum <- dfcasesSum[-18]
dfcasesSum <- data.frame(dfcasesSum)
dfcasesSum$month <- month
names(dfcasesSum)[names(dfcasesSum)=="dfcasesSum"] <- "IR"
dfcasesSum$IR <- (dfcasesSum$IR / popSum) * 100000
dfcasesSum

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


ggplot(dfcasesSum, aes(x=month, y=IR, group=1)) +
  geom_line() +
  geom_point() +
  scale_y_continuous()+ 
  scale_x_discrete(limits=dfcasesSum$month)+
  labs(title="Average country-wide incidence rate per month",x="Month", y = "incidence rate")+
  theme_bw()























### Medical indicator (no clear trend)

## Hypothesis I, CFR ~ population per doctor
dfCFREUm1 <- SARvarlist("LNpopPerDoc",varlistCFREU)
names(dfCFREUm1)[names(dfCFREUm1)=="coefficientReg"] <- "SAR, age standardised"
dfCFRm1 <- SARvarlist("LNpopPerDoc",varlistCFR)
names(dfCFRm1)[names(dfCFRm1)=="coefficientReg"] <- "SAR, not age standardised"
dfCFREUOLSm1 <- OLSvarlist("popPerDoc",varlistCFREU)
names(dfCFREUOLSm1)[names(dfCFREUOLSm1)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfCFRm1 <- merge(dfCFREUm1, dfCFRm1, by.dfCFREUm1 = month, by.dfCFRm1 = month)
dfCFRm1 <- merge(dfCFRm1, dfCFREUOLSm1, by.dfCFRm1 = month, by.dfCFREUOLSm1 = month)
dfCFRm1 <- reshape(dfCFRm1, times = c("SAR, age standardised", "SAR, not age standardised", 
                                      "Lin. Reg., age standardised"), 
                   varying = c("SAR, age standardised", "SAR, not age standardised", 
                               "Lin. Reg., age standardised"),  
                   idvar = "month", v.name="coefficients", direction = "long")
names(dfCFRm1)[names(dfCFRm1)=="time"] <- "model"

ggplot(dfCFRm1, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8) +
  geom_hline(aes(yintercept = 0)) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "case fatality ratio ~ log. median income", 
       caption = "Read: ''In the age standardised models, a 1% increase county median income
                  is associated with a 0.0005 percentage point decrease in case fatality ratio as of 
                  March 21''" ) +
  theme_bw()

## Hypothesis I, CFR ~ population per hospital bed
dfCFREUm2 <- SARvarlist("hospBeds",varlistCFREU)
names(dfCFREUm2)[names(dfCFREUm2)=="coefficientReg"] <- "SAR, age standardised"
dfCFRm2 <- SARvarlist("hospBeds",varlistCFR)
names(dfCFRm2)[names(dfCFRm2)=="coefficientReg"] <- "SAR, not age standardised"
dfCFREUOLSm2 <- OLSvarlist("hospBeds",varlistCFREU)
names(dfCFREUOLSm2)[names(dfCFREUOLSm2)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfCFRm2 <- merge(dfCFREUm2, dfCFRm2, by.dfCFREUm2 = month, by.dfCFRm2 = month)
dfCFRm2 <- merge(dfCFRm2, dfCFREUOLSm2, by.dfCFRm2 = month, by.dfCFREUOLSm2 = month)
dfCFRm2 <- reshape(dfCFRm2, times = c("SAR, age standardised", "SAR, not age standardised", 
                                      "Lin. Reg., age standardised"), 
                   varying = c("SAR, age standardised", "SAR, not age standardised", 
                               "Lin. Reg., age standardised"),  
                   idvar = "month", v.name="coefficients", direction = "long")
names(dfCFRm2)[names(dfCFRm2)=="time"] <- "model"

ggplot(dfCFRm2, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8) +
  geom_hline(aes(yintercept = 0)) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "case fatality ratio ~ log. median income", 
       caption = "Read: ''In the age standardised models, a 1% increase county median income
                  is associated with a 0.0005 percentage point decrease in case fatality ratio as of 
                  March 21''" ) +
  theme_bw()

## lagged CFR (repeat with 2 weeks, one month above average time to death)

dfCFREU_ <- SARvarlistLAG("GISD",varlistCFREU_)
names(dfCFREU_)[names(dfCFREU_)=="coefficientReg"] <- "SAR, age standardised"
dfCFR_ <- OLSvarlistLAG("GISD",varlistCFR_)
names(dfCFR_)[names(dfCFR_)=="coefficientReg"] <- "Lin. Reg., age standardised"
dfCFREUlag <- OLSvarlistLAG("GISD",varlistCFREUlag)
names(dfCFREUlag)[names(dfCFREUlag)=="coefficientReg"] <- "Lin. Reg., age standardised, lagged"


dfCFRlag <- merge(dfCFREU_, dfCFR_, by.dfCFREU_ = month, by.dfCFR_ = month)
dfCFRlag <- merge(dfCFRlag, dfCFREUlag, by.dfCFRlag = month, by.dfCFREUlag = month)
dfCFRlag <- reshape(dfCFRlag, times = c("SAR, age standardised", "Lin. Reg., age standardised", 
                                        "Lin. Reg., age standardised, lagged"), 
                    varying = c("SAR, age standardised", "Lin. Reg., age standardised", 
                                "Lin. Reg., age standardised, lagged"), 
                    idvar = "month", v.name="coefficients", direction = "long")
names(dfCFRlag)[names(dfCFRlag)=="time"] <- "model"

ggplot(dfCFRlag, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8) +
  geom_hline(aes(yintercept = 0)) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "case fatality ratio ~ German Index of socioeconomic Deprivation", 
       caption = "Read: ''In the age standardised models, the most socioeconomicly deprived
                  county has a 2 percentage points higher case fatality ratio compared
                  to the least socioeconomicly deprived county as of March 21''" )+
  theme_bw()







