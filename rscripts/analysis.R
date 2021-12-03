##########################################################
#                                                        #
#                                                        #
# 2021/10                                                #
#                                                        #
# Alexander Busch (a.busch@lse.ac.uk)                    #
#                                                        #
# Data Analysis                                          #
#                                                        #
##########################################################

source("rscripts/manipulation.R")

# Varlist for Models
varlistLNIR <- c("LNIR20.02","LNIR20.03","LNIR20.04","LNIR20.05","LNIR20.06",
                   "LNIR20.07","LNIR20.08","LNIR20.09","LNIR20.10",
                   "LNIR20.11","LNIR20.12","LNIR21.01","LNIR21.02",
                   "LNIR21.03","LNIR21.04","LNIR21.05","LNIR21.06")
varlistLNIREU <- c("LNIREU20.02","LNIREU20.03","LNIREU20.04","LNIREU20.05","LNIREU20.06",
                 "LNIREU20.07","LNIREU20.08","LNIREU20.09","LNIREU20.10",
                 "LNIREU20.11","LNIREU20.12","LNIREU21.01","LNIREU21.02",
                 "LNIREU21.03","LNIREU21.04","LNIREU21.05","LNIREU21.06")
varlistLNIREUlag1 <- c("LNIREU20.03","LNIREU20.04","LNIREU20.05","LNIREU20.06",
                   "LNIREU20.07","LNIREU20.08","LNIREU20.09","LNIREU20.10",
                   "LNIREU20.11","LNIREU20.12","LNIREU21.01","LNIREU21.02",
                   "LNIREU21.03","LNIREU21.04","LNIREU21.05","LNIREU21.06")
varlistLNIREUlag2 <- c("LNIREU20.02","LNIREU20.03","LNIREU20.04","LNIREU20.05","LNIREU20.06",
                      "LNIREU20.07","LNIREU20.08","LNIREU20.09","LNIREU20.10",
                      "LNIREU20.11","LNIREU20.12","LNIREU21.01","LNIREU21.02",
                      "LNIREU21.03","LNIREU21.04","LNIREU21.05")
varlistLNcases <- c("LNcases20.02","LNcases20.03","LNcases20.04","LNcases20.05","LNcases20.06",
                 "LNcases20.07","LNcases20.08","LNcases20.09","LNcases20.10",
                 "LNcases20.11","LNcases20.12","LNcases21.01","LNcases21.02",
                 "LNcases21.03","LNcases21.04","LNcases21.05","LNcases21.06")
varlistCFR <- c("CFR20.02","CFR20.03","CFR20.04","CFR20.05","CFR20.06",
                 "CFR20.07","CFR20.08","CFR20.09","CFR20.10",
                 "CFR20.11","CFR20.12","CFR21.01","CFR21.02",
                 "CFR21.03","CFR21.04","CFR21.05","CFR21.06")
varlistCFREU <- c("CFREU20.02","CFREU20.03","CFREU20.04","CFREU20.05","CFREU20.06",
                   "CFREU20.07","CFREU20.08","CFREU20.09","CFREU20.10",
                   "CFREU20.11","CFREU20.12","CFREU21.01","CFREU21.02",
                   "CFREU21.03","CFREU21.04","CFREU21.05","CFREU21.06")
varlistControl <- c("LNpopDensity","SH","HH","NI","HB","MV","BB","BE","ST","SN","TH","NW","HE","RP","BY","SL")
varlistStates <- c("SH","HH","NI","HB","MV","BB","BE","ST","SN","TH","NW","HE","RP","BY","SL")
varlistSumCases <- c("scases20.02","scases20.03","scases20.04","scases20.05","scases20.06",
                    "scases20.07","scases20.08","scases20.09","scases20.10",
                    "scases20.11","scases20.12","scases21.01","scases21.02",
                    "scases21.03","scases21.04","scases21.05")
varlistControl2 <- c("LNpopDensity")
varlistControl3 <- c("LNpopDensity","relCath","AfD","shareWomen","SH","HH","NI","HB","MV","BB","BE","ST","SN","TH","NW","HE","RP","BY","SL")
month1 <- c("20-02", "20-03", "20-04", "20-05", "20-06", "20-07", "20-08", "20-09", 
            "20-10", "20-11", "20-12", "21-01", "21-02", "21-03", "21-04", "21-05", "21-06")
month2 <- c("20-03", "20-04", "20-05", "20-06", "20-07", "20-08", "20-09", 
            "20-10", "20-11", "20-12", "21-01", "21-02", "21-03", "21-04", "21-05", "21-06")

# Create lists needed for analysis
neighbors <- poly2nb(dfds)
weighted_neighbors <- nb2listw(neighbors, zero.policy=T)

### Analysis

## Incidence Rates

# distribution of LNIR
hist20.02 <- ggplot(dfdd, aes(x=LNIR20.02)) +
  geom_histogram(aes(y = ..density..), colour = "grey", fill = "grey", bins = 15) +
  stat_function(fun = dnorm,
                args = list(mean = mean(dfdd$LNIR20.02),
                            sd = sd(dfdd$LNIR20.02)),
                col = "blue",
                size = 1) +
  labs(title="20-02",x="LN IR", y = "density") +
  theme_bw()
hist20.02
hist20.04 <- ggplot(dfdd, aes(x=LNIR20.04)) +
  geom_histogram(aes(y = ..density..), colour = "grey", fill = "grey", bins = 15) +
  stat_function(fun = dnorm,
                args = list(mean = mean(dfdd$LNIR20.04),
                            sd = sd(dfdd$LNIR20.04)),
                col = "blue",
                size = 1) +
  labs(title="20-04",x="LN IR", y = "density") +
  theme_bw()
hist20.04
hist20.08 <- ggplot(dfdd, aes(x=LNIR20.08)) +
  geom_histogram(aes(y = ..density..), colour = "grey", fill = "grey", bins = 15) +
  stat_function(fun = dnorm,
                args = list(mean = mean(dfdd$LNIR20.08),
                            sd = sd(dfdd$LNIR20.08)),
                col = "blue",
                size = 1) +
  labs(title="20-08",x="LN IR", y = "density") +
  theme_bw()
hist20.08
hist20.12 <- ggplot(dfdd, aes(x=LNIR20.12)) +
  geom_histogram(aes(y = ..density..), colour = "grey", fill = "grey", bins = 15) +
  stat_function(fun = dnorm,
                args = list(mean = mean(dfdd$LNIR20.12),
                            sd = sd(dfdd$LNIR20.12)),
                col = "blue",
                size = 1) +
  labs(title="20-12",x="LN IR", y = "density") +
  theme_bw()
hist20.12
hist21.04 <- ggplot(dfdd, aes(x=LNIR21.04)) +
  geom_histogram(aes(y = ..density..), colour = "grey", fill = "grey", bins = 15) +
  stat_function(fun = dnorm,
                args = list(mean = mean(dfdd$LNIR21.04),
                            sd = sd(dfdd$LNIR21.04)),
                col = "blue",
                size = 1) +
  labs(title="21-04",x="LN IR", y = "density") +
  theme_bw()
hist21.04
hist <- ggplot(dfdd, aes(x=LNIR)) +
  geom_histogram(aes(y = ..density..), colour = "grey", fill = "grey", bins = 15) +
  stat_function(fun = dnorm,
                args = list(mean = mean(dfdd$LNIR),
                            sd = sd(dfdd$LNIR)),
                col = "blue",
                size = 1) +
  labs(title="all",x="LN IR", y = "density") +
  theme_bw()
hist

grid.arrange(hist, hist20.02, hist20.04, hist20.08, hist20.12, hist21.04, ncol=2, nrow=3)
# > except for February, normal distribution can be assumed, maybe a bit skewed to right? 

## Test for spatial Autocorrelation in data
moran.test(dfds$IREU, weighted_neighbors, zero.policy=T)
moran.test(dfds$IREU20.04, weighted_neighbors, zero.policy=T)
moran.test(dfds$IREU20.08, weighted_neighbors, zero.policy=T)
moran.test(dfds$IREU20.12, weighted_neighbors, zero.policy=T)
moran.test(dfds$IREU21.04, weighted_neighbors, zero.policy=T)
moran.plot(dfds$IREU, weighted_neighbors, zero.policy=T,xlab="incidence rates, age standardised", ylab="spatially lagged IR")
moran.plot(dfds$IREU20.04, weighted_neighbors, zero.policy=T,xlab="incidence rates, age standardised", ylab="spatially lagged IR")
moran.plot(dfds$IREU21.04, weighted_neighbors, zero.policy=T,xlab="incidence rates, age standardised", ylab="spatially lagged IR")


## Hypothesis I, IR ~ GISD
# plotting shows linear relationship, however, heteroscedasticity in the last 2? 
ggplot(dfdd, aes(x=GISD, y=LNIREU20.04)) +
  geom_point() +
  labs(title="LNIR ~ GISD",x="German Index of Socioeconomic Deprivation", y = "log incidence rates 20-04") +
  theme_bw() 
ggplot(dfdd, aes(x=GISD, y=LNIREU20.08)) +
  geom_point() +
  labs(title="LNIR ~ GISD",x="German Index of Socioeconomic Deprivation", y = "log incidence rates 20-08") +
  theme_bw() 
ggplot(dfdd, aes(x=GISD, y=LNIREU20.12)) +
  geom_point() +
  labs(title="LNIR ~ GISD",x="German Index of Socioeconomic Deprivation", y = "log incidence rates 20-12") +
  theme_bw() 
ggplot(dfdd, aes(x=GISD, y=LNIREU21.04)) +
  geom_point() +
  labs(title="LNIR ~ GISD",x="German Index of Socioeconomic Deprivation", y = "log incidence rates 21-04") +
  theme_bw() 



#### model GISD
listIRGISD1 <- regvarlistM("GISD",varlistLNIREU,varlistControl,month,"SAR",dfds)
dfIRGISD1 <- as.data.frame(listIRGISD1[1:2])
modelsIRGISD1 <- listIRGISD1[3:19]
names(dfIRGISD1)[names(dfIRGISD1)=="coefficientReg"] <- "SAR, age standardised"

listIRGISD2 <- regvarlistM("GISD",varlistLNIR,varlistControl,month,"SAR",dfds)
dfIRGISD2 <- as.data.frame(listIRGISD2[1:2])
modelsIRGISD2 <- listIRGISD2[3:19]
names(dfIRGISD2)[names(dfIRGISD2)=="coefficientReg"] <- "SAR, not age standardised"

listIRGISD3 <- regvarlistM("GISD",varlistLNIREU,varlistControl,month,"OLS",dfds)
dfIRGISD3 <- as.data.frame(listIRGISD3[1:2])
modelsIRGISD3 <- listIRGISD3[3:19]
names(dfIRGISD3)[names(dfIRGISD3)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfIRGISD <- merge(dfIRGISD1, dfIRGISD2, by.dfIRGISD1 = month, by.dfIRGISD2 = month)
dfIRGISD <- merge(dfIRGISD, dfIRGISD3, by.dfIRGISD = month, by.dfIRGISD3 = month)
dfIRGISD <- reshape(dfIRGISD, times = c("SAR, age standardised", "SAR, not age standardised", 
                                "Lin. Reg., age standardised"), 
                varying = c("SAR, age standardised", "SAR, not age standardised", 
                            "Lin. Reg., age standardised"),  
                idvar = "month", v.name="coefficients", direction = "long")
names(dfIRGISD)[names(dfIRGISD)=="time"] <- "model"
# define labels to skip every second month in axis (every second label empty)
xlabels <- sort(unique(dfIRGISD$month))
xlabels[seq(2, length(xlabels), 2)] <- ""


graphGISD <- ggplot(dfIRGISD, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_discrete(labels = xlabels) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "log. Incidence Rates ~ German Index of socioeconomic Deprivation + pop. Density + state dummies", 
       caption = "Read: ''The most socioeconomicly deprived
                  county has a 1.5% higher incidence rate compared
                 to the least socioeconomicly deprived county as of March 21''") +
  theme_bw()
graphGISD

# latex output reg models
stargazer(modelsIRGISD1[1:6], type = "latex", omit = varlistStates,
          omit.stat=c("f", "ser"), align=TRUE,digits=1)
stargazer(modelsIRGISD1[7:12], type = "latex", omit = varlistStates,
          omit.stat=c("f", "ser"), align=TRUE,digits=1)
stargazer(modelsIRGISD1[13:17], type = "latex", omit = varlistStates,
          omit.stat=c("f", "ser"), align=TRUE,digits=1)

stargazer(modelsIRGISD3[1:6], type = "latex", omit = varlistStates,
          omit.stat=c("f", "ser"), align=TRUE,digits=1)
stargazer(modelsIRGISD3[7:12], type = "latex", omit = varlistStates,
          omit.stat=c("f", "ser"), align=TRUE,digits=1)
stargazer(modelsIRGISD3[13:17], type = "latex", omit = varlistStates,
          omit.stat=c("f", "ser"), align=TRUE,digits=1)

## Without state dummies
dfIRGISDnostSAR <- regvarlist("GISD",varlistLNIREU,varlistControl2,month1,"SAR",dfds)
names(dfIRGISDnostSAR)[names(dfIRGISDnostSAR)=="coefficientReg"] <- "SAR, age standardised, no states"
dfIRGISDnostOLS <- regvarlist("GISD",varlistLNIREU,varlistControl2,month1,"OLS",dfds)
names(dfIRGISDnostOLS)[names(dfIRGISDnostOLS)=="coefficientReg"] <- "Lin. Reg., age standardised, no states"
dfIRGISDstSAR <- regvarlist("GISD",varlistLNIREU,varlistControl,month1,"SAR",dfds)
names(dfIRGISDstSAR)[names(dfIRGISDstSAR)=="coefficientReg"] <- "SAR, age standardised"
dfIRGISDstOLS <- regvarlist("GISD",varlistLNIREU,varlistControl,month1,"OLS",dfds)
names(dfIRGISDstOLS)[names(dfIRGISDstOLS)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfIRGISDnost <- merge(dfIRGISDnostSAR, dfIRGISDnostOLS, by = "month")
dfIRGISDnost <- merge(dfIRGISDnost, dfIRGISDstSAR, by = "month")
dfIRGISDnost <- merge(dfIRGISDnost, dfIRGISDstOLS, by = "month")
dfIRGISDnost <- reshape(dfIRGISDnost, times = c("SAR, age standardised, no states", 
                                                "Lin. Reg., age standardised, no states", 
                                                "SAR, age standardised",
                                                "Lin. Reg., age standardised"), 
                 varying = c("SAR, age standardised, no states", 
                             "Lin. Reg., age standardised, no states", 
                             "SAR, age standardised",
                             "Lin. Reg., age standardised"),  
                 idvar = "month", v.name="coefficients", direction = "long")
names(dfIRGISDnost)[names(dfIRGISDnost)=="time"] <- "model"
xlabels <- sort(unique(dfIRGISDnost$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

ggplot(dfIRGISDnost, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8,aes(linetype=model,color=model)) +
  geom_hline(size = 1, aes(yintercept = 0)) +
  scale_linetype_manual(values=c("solid", "twodash", "solid", "twodash")) +
  scale_color_manual(values=c("red", "red", "blue", "blue")) +
  scale_x_discrete(labels = xlabels) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "log. Incidence Rate ~ German Index of socioeconomic Deprivation + pop. Density (+ state dummies)", 
       caption = "Read: ''The most socioeconomicly deprived
                  county has a 0.5% to 1.5% higher incidence rate compared
                 to the least socioeconomicly deprived county as of March 21''") +
  theme_bw()


### Testing Segments of GISD (without reg model output)

## Segment I, IR ~ unemployment
dfIREUu <- regvarlist("unemployment",varlistLNIREU,varlistControl,month1,"SAR",dfds)
names(dfIREUu)[names(dfIREUu)=="coefficientReg"] <- "SAR, age standardised"
dfIRu <- regvarlist("unemployment",varlistLNIR,varlistControl,month1,"SAR",dfds)
names(dfIRu)[names(dfIRu)=="coefficientReg"] <- "SAR, not age standardised"
dfIREUOLSu <- regvarlist("unemployment",varlistLNIREU,varlistControl,month1,"OLS",dfds)
names(dfIREUOLSu)[names(dfIREUOLSu)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfIRu <- merge(dfIREUu, dfIRu, by = "month")
dfIRu <- merge(dfIRu, dfIREUOLSu, by = "month")
dfIRu <- reshape(dfIRu, times = c("SAR, age standardised", "SAR, not age standardised", 
                                "Lin. Reg., age standardised"), 
                varying = c("SAR, age standardised", "SAR, not age standardised", 
                            "Lin. Reg., age standardised"),  
                idvar = "month", v.name="coefficients", direction = "long")
names(dfIRu)[names(dfIRu)=="time"] <- "model"
xlabels <- sort(unique(dfIREUu$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

graphI.1 <- ggplot(dfIRu, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_discrete(labels = xlabels) +
  labs(title = "Unemployment", x="Month", y = "Coefficients")+
  theme_bw()


## Segment I, IR ~ employment
dfIREUu2 <- regvarlist("employment",varlistLNIREU,varlistControl,month1,"SAR",dfds)
names(dfIREUu2)[names(dfIREUu2)=="coefficientReg"] <- "SAR, age standardised"
dfIRu2 <- regvarlist("employment",varlistLNIR,varlistControl,month1,"SAR",dfds)
names(dfIRu2)[names(dfIRu2)=="coefficientReg"] <- "SAR, not age standardised"
dfIREUOLSu2 <- regvarlist("employment",varlistLNIREU,varlistControl,month1,"OLS",dfds)
names(dfIREUOLSu2)[names(dfIREUOLSu2)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfIRu2 <- merge(dfIREUu2, dfIRu2, by = "month")
dfIRu2 <- merge(dfIRu2, dfIREUOLSu2, by = "month")
dfIRu2 <- reshape(dfIRu2, times = c("SAR, age standardised", "SAR, not age standardised", 
                                  "Lin. Reg., age standardised"), 
                 varying = c("SAR, age standardised", "SAR, not age standardised", 
                             "Lin. Reg., age standardised"),  
                 idvar = "month", v.name="coefficients", direction = "long")
names(dfIRu2)[names(dfIRu2)=="time"] <- "model"
xlabels <- sort(unique(dfIREUu2$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

graphI.2 <- ggplot(dfIRu2, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_discrete(labels = xlabels) +
  labs(title="Participation Rate",x="Month", y = "Coefficients")+
  theme_bw()


## Segment II, IR ~ education (share of academics in work force)
dfIREUe <- regvarlist("workersAcadem",varlistLNIREU,varlistControl,month1,"SAR",dfds)
names(dfIREUe)[names(dfIREUe)=="coefficientReg"] <- "SAR, age standardised"
dfIRe <- regvarlist("workersAcadem",varlistLNIR,varlistControl,month1,"SAR",dfds)
names(dfIRe)[names(dfIRe)=="coefficientReg"] <- "SAR, not age standardised"
dfIREUOLSe <- regvarlist("workersAcadem",varlistLNIREU,varlistControl,month1,"OLS",dfds)
names(dfIREUOLSe)[names(dfIREUOLSe)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfIRe <- merge(dfIREUe, dfIRe, by = "month")
dfIRe <- merge(dfIRe, dfIREUOLSe, by = "month")
dfIRe <- reshape(dfIRe, times = c("SAR, age standardised", "SAR, not age standardised", 
                                  "Lin. Reg., age standardised"), 
                 varying = c("SAR, age standardised", "SAR, not age standardised", 
                             "Lin. Reg., age standardised"),  
                 idvar = "month", v.name="coefficients", direction = "long")
names(dfIRe)[names(dfIRe)=="time"] <- "model"
xlabels <- sort(unique(dfIRe$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

graphII.1 <- ggplot(dfIRe, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_discrete(labels = xlabels) +
  labs(title="Academic Workers",x="Month", y = "Coefficients")+
  theme_bw()


## Segment II, IR ~ education (share of workers with no high school diploma)
dfIREUe2 <- regvarlist("workersNoEdu",varlistLNIREU,varlistControl,month1,"SAR",dfds)
names(dfIREUe2)[names(dfIREUe2)=="coefficientReg"] <- "SAR, age standardised"
dfIRe2 <- regvarlist("workersNoEdu",varlistLNIR,varlistControl,month1,"SAR",dfds)
names(dfIRe2)[names(dfIRe2)=="coefficientReg"] <- "SAR, not age standardised"
dfIREUOLSe2 <- regvarlist("workersNoEdu",varlistLNIREU,varlistControl,month1,"OLS",dfds)
names(dfIREUOLSe2)[names(dfIREUOLSe2)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfIRe2 <- merge(dfIREUe2, dfIRe2, by = "month")
dfIRe2 <- merge(dfIRe2, dfIREUOLSe2, by = "month")
dfIRe2 <- reshape(dfIRe2, times = c("SAR, age standardised", "SAR, not age standardised", 
                                  "Lin. Reg., age standardised"), 
                 varying = c("SAR, age standardised", "SAR, not age standardised", 
                             "Lin. Reg., age standardised"),  
                 idvar = "month", v.name="coefficients", direction = "long")
names(dfIRe2)[names(dfIRe2)=="time"] <- "model"
xlabels <- sort(unique(dfIRe2$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

graphII.2 <- ggplot(dfIRe2, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_discrete(labels = xlabels) +
  labs(title="No school diploma",x="Month", y = "Coefficients") +
  theme_bw()


## Segment III, IR ~  gross income
dfIREUi2 <- regvarlist("LNgrossInc",varlistLNIREU,varlistControl,month1,"SAR",dfds)
names(dfIREUi2)[names(dfIREUi2)=="coefficientReg"] <- "SAR, age standardised"
dfIRi2 <- regvarlist("LNgrossInc",varlistLNIR,varlistControl,month1,"SAR",dfds)
names(dfIRi2)[names(dfIRi2)=="coefficientReg"] <- "SAR, not age standardised"
dfIREUOLSi2 <- regvarlist("LNgrossInc",varlistLNIREU,varlistControl,month1,"OLS",dfds)
names(dfIREUOLSi2)[names(dfIREUOLSi2)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfIRi2 <- merge(dfIREUi2, dfIRi2, by = "month")
dfIRi2 <- merge(dfIRi2, dfIREUOLSi2, by = "month")
dfIRi2 <- reshape(dfIRi2, times = c("SAR, age standardised", "SAR, not age standardised", 
                                    "Lin. Reg., age standardised"), 
                  varying = c("SAR, age standardised", "SAR, not age standardised", 
                              "Lin. Reg., age standardised"),  
                  idvar = "month", v.name="coefficients", direction = "long")
names(dfIRi2)[names(dfIRi2)=="time"] <- "model"
xlabels <- sort(unique(dfIRi2$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

graphII.3 <- ggplot(dfIRi2, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8) +
  geom_hline(aes(yintercept = 0)) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_discrete(labels = xlabels) +
  labs(title="Log. Gross Income",x="Month", y = "Coefficients") +
  theme_bw()


## Segment III, IR ~  house hold income
dfIREUi <- regvarlist("LNhhInc",varlistLNIREU,varlistControl,month1,"SAR",dfds)
names(dfIREUi)[names(dfIREUi)=="coefficientReg"] <- "SAR, age standardised"
dfIRi <- regvarlist("LNhhInc",varlistLNIR,varlistControl,month1,"SAR",dfds)
names(dfIRi)[names(dfIRi)=="coefficientReg"] <- "SAR, not age standardised"
dfIREUOLSi <- regvarlist("LNhhInc",varlistLNIREU,varlistControl,month1,"OLS",dfds)
names(dfIREUOLSi)[names(dfIREUOLSi)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfIRi <- merge(dfIREUi, dfIRi, by = "month")
dfIRi <- merge(dfIRi, dfIREUOLSi, by = "month")
dfIRi <- reshape(dfIRi, times = c("SAR, age standardised", "SAR, not age standardised", 
                                  "Lin. Reg., age standardised"), 
                 varying = c("SAR, age standardised", "SAR, not age standardised", 
                             "Lin. Reg., age standardised"),  
                 idvar = "month", v.name="coefficients", direction = "long")
names(dfIRi)[names(dfIRi)=="time"] <- "model"
xlabels <- sort(unique(dfIRi$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

graphIII.1 <- ggplot(dfIRi, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8) +
  geom_hline(aes(yintercept = 0)) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_discrete(labels = xlabels) +
  labs(title="Log. Household Income",x="Month", y = "Coefficients") +
  theme_bw()


## Segment III, IR ~  debtor quota
dfIREUdq <- regvarlist("debtQuota",varlistLNIREU,varlistControl,month1,"SAR",dfds)
names(dfIREUdq)[names(dfIREUdq)=="coefficientReg"] <- "SAR, age standardised"
dfIRdq <- regvarlist("debtQuota",varlistLNIR,varlistControl,month1,"SAR",dfds)
names(dfIRdq)[names(dfIRdq)=="coefficientReg"] <- "SAR, not age standardised"
dfIREUOLSdq <- regvarlist("debtQuota",varlistLNIREU,varlistControl,month1,"OLS",dfds)
names(dfIREUOLSdq)[names(dfIREUOLSdq)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfIRdq <- merge(dfIREUdq, dfIRdq, by = "month")
dfIRdq <- merge(dfIRdq, dfIREUOLSdq, by = "month")
dfIRdq <- reshape(dfIRdq, times = c("SAR, age standardised", "SAR, not age standardised", 
                                  "Lin. Reg., age standardised"), 
                 varying = c("SAR, age standardised", "SAR, not age standardised", 
                             "Lin. Reg., age standardised"),  
                 idvar = "month", v.name="coefficients", direction = "long")
names(dfIRdq)[names(dfIRdq)=="time"] <- "model"
xlabels <- sort(unique(dfIRdq$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

graphIII.2 <- ggplot(dfIRdq, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8) +
  geom_hline(aes(yintercept = 0)) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_discrete(labels = xlabels) +
  labs(title="Debtor Quote",x="Month", y = "Coefficients") +
  theme_bw()


## Segment III, IR ~  business tax
dfIREUi2 <- regvarlist("LNbusinessTax",varlistLNIREU,varlistControl,month1,"SAR",dfds)
names(dfIREUi2)[names(dfIREUi2)=="coefficientReg"] <- "SAR, age standardised"
dfIRi2 <- regvarlist("LNbusinessTax",varlistLNIR,varlistControl,month1,"SAR",dfds)
names(dfIRi2)[names(dfIRi2)=="coefficientReg"] <- "SAR, not age standardised"
dfIREUOLSi2 <- regvarlist("LNbusinessTax",varlistLNIREU,varlistControl,month1,"OLS",dfds)
names(dfIREUOLSi2)[names(dfIREUOLSi2)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfIRi2 <- merge(dfIREUi2, dfIRi2, by = "month")
dfIRi2 <- merge(dfIRi2, dfIREUOLSi2, by = "month")
dfIRi2 <- reshape(dfIRi2, times = c("SAR, age standardised", "SAR, not age standardised", 
                                    "Lin. Reg., age standardised"), 
                  varying = c("SAR, age standardised", "SAR, not age standardised", 
                              "Lin. Reg., age standardised"),  
                  idvar = "month", v.name="coefficients", direction = "long")
names(dfIRi2)[names(dfIRi2)=="time"] <- "model"
xlabels <- sort(unique(dfIRi2$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

graphIII.3 <- ggplot(dfIRi2, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8) +
  geom_hline(aes(yintercept = 0)) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_discrete(labels = xlabels) +
  labs(title="Log. Business Tax",x="Month", y = "Coefficients") +
  theme_bw()

# combine plots
grid.arrange(graphI.1, graphI.2, ncol=1, nrow=2)
grid.arrange(graphII.1, graphII.2, graphII.3, ncol=1, nrow=3)
grid.arrange(graphIII.1, graphIII.2, graphIII.3, ncol=1, nrow=3)




#### Robustness checks

## differences in gender
# OLS model male
dfOLSIRGISDmale <- regvarlist("GISD",varlistLNIREU,varlistControl,month1, "OLS",dfdsm)
names(dfOLSIRGISDmale)[names(dfOLSIRGISDmale)=="coefficientReg"] <- "OLS, age stnd, male"
# SAR model male
dfSARIRGISDmale <- regvarlist("GISD",varlistLNIREU,varlistControl,month1, "SAR",dfdsm)
names(dfSARIRGISDmale)[names(dfSARIRGISDmale)=="coefficientReg"] <- "SAR, age stnd, male"
# OLS model female
dfOLSIRGISDfemale <- regvarlist("GISD",varlistLNIREU,varlistControl,month1,"SAR",dfdsf)
names(dfOLSIRGISDfemale)[names(dfOLSIRGISDfemale)=="coefficientReg"] <- "SAR, age stnd, female"
# SAR model female
dfSARIRGISDfemale <- regvarlist("GISD",varlistLNIREU,varlistControl,month1,"OLS",dfdsf)
names(dfSARIRGISDfemale)[names(dfSARIRGISDfemale)=="coefficientReg"] <- "OLS, age stnd, female"
# OLS model all
dfOLSIRGISD <- regvarlist("GISD",varlistLNIREU,varlistControl,month1,"SAR",dfds)
names(dfOLSIRGISD)[names(dfOLSIRGISD)=="coefficientReg"] <- "SAR, age standardised"
# SAR model all
dfSARIRGISD <- regvarlist("GISD",varlistLNIREU,varlistControl,month1,"OLS",dfds)
names(dfSARIRGISD)[names(dfSARIRGISD)=="coefficientReg"] <- "OLS, age standardised"
dfSARIRGISD

# merge model data frames with coefficients
dfIRgender <- merge(dfOLSIRGISDmale, dfSARIRGISDmale, by = "month")
dfIRgender <- merge(dfIRgender, dfOLSIRGISDfemale, by = "month")
dfIRgender <- merge(dfIRgender, dfSARIRGISDfemale, by = "month")
dfIRgender <- merge(dfIRgender, dfOLSIRGISD, by = "month")
dfIRgender <- merge(dfIRgender, dfSARIRGISD, by = "month")
dfIRgender 
dfIRgender <- reshape(dfIRgender, times = c("SAR, age stnd, female", "OLS, age stnd, female", 
                                            "OLS, age stnd, male", "SAR, age stnd, male", 
                                            "SAR, age standardised", "OLS, age standardised"), 
                       varying = c("SAR, age stnd, female", "OLS, age stnd, female", 
                                   "OLS, age stnd, male", "SAR, age stnd, male", 
                                   "SAR, age standardised", "OLS, age standardised"),  
                       idvar = "month", v.name="coefficients", direction = "long")
names(dfIRgender)[names(dfIRgender)=="time"] <- "model"
xlabels <- sort(unique(dfIRgender$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

ggplot(dfIRgender, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8,aes(linetype=model,color=model)) +
  geom_hline(aes(yintercept = 0)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotted", "solid", "dashed", "dotted")) +
  scale_color_manual(values=c("black", "red", "red", "black","blue", "blue")) +
  scale_x_discrete(labels = xlabels) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "log. Incidence Rates ~ German Index of socioeconomic Deprivation + pop. Density + state dummies", 
       caption = "Read: ''The most socioeconomicly deprived
                  county has a 0.5% to 1.5% higher incidence rate compared
                 to the least socioeconomicly deprived county as of March 21''") +
  theme_bw()


## path dependency
# OLS model with IR lag
dfOLSIRGISDpath <- regvarlistP("GISD",varlistLNIREUlag1,varlistControl,varlistLNIREUlag2,month2, "OLS",dfds)
names(dfOLSIRGISDpath)[names(dfOLSIRGISDpath)=="coefficientReg"] <- "OLS, age stnd + path"
dfOLSIRGISDpath
# SAR model with IR lag
dfSARIRGISDpath <- regvarlistP("GISD",varlistLNIREUlag1,varlistControl,varlistLNIREUlag2,month2, "SAR",dfds)
names(dfSARIRGISDpath)[names(dfSARIRGISDpath)=="coefficientReg"] <- "SAR, age stnd + path"
dfSARIRGISDpath
# OLS model without IR lag
dfOLSIRGISD <- regvarlist("GISD",varlistLNIREUlag1,varlistControl,month2,"SAR",dfds)
names(dfOLSIRGISD)[names(dfOLSIRGISD)=="coefficientReg"] <- "SAR, age standardised"
dfOLSIRGISD
# SAR model without IR lag
dfSARIRGISD <- regvarlist("GISD",varlistLNIREUlag1,varlistControl,month2,"OLS",dfds)
names(dfSARIRGISD)[names(dfSARIRGISD)=="coefficientReg"] <- "OLS, age standardised"
dfSARIRGISD

# merge model data frames with coefficients
dfIRpath <- merge(dfOLSIRGISDpath, dfSARIRGISDpath, by = "month")
dfIRpath <- merge(dfIRpath, dfOLSIRGISD, by = "month")
dfIRpath <- merge(dfIRpath, dfSARIRGISD, by = "month")
dfIRpath
dfIRpath <- reshape(dfIRpath, times = c("SAR, age standardised", "OLS, age standardised", 
                                  "OLS, age stnd + path", "SAR, age stnd + path"), 
                 varying = c("SAR, age standardised", "OLS, age standardised", 
                             "OLS, age stnd + path", "SAR, age stnd + path"),  
                 idvar = "month", v.name="coefficients", direction = "long")
names(dfIRpath)[names(dfIRpath)=="time"] <- "model"
xlabels <- sort(unique(dfIRpath$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

ggplot(dfIRpath, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8,aes(linetype=model,color=model)) +
  geom_hline(aes(yintercept = 0)) +
  scale_linetype_manual(values=c("solid", "twodash", "solid", "twodash")) +
  scale_color_manual(values=c("red", "red", "blue", "blue")) +
  scale_x_discrete(labels = xlabels) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "log. Incidence Rates ~ German Index of socioeconomic Deprivation + pop. Density + state dummies", 
       caption = "Read: ''The most socioeconomicly deprived
                  county has a 0.5% to 1.5% higher incidence rate compared
                 to the least socioeconomicly deprived county as of March 21''") +
  theme_bw()

## other factors
# OLS model with controls
dfOLSIRGISDcontrol <- regvarlist("GISD",varlistLNIREU,varlistControl3,month1, "OLS",dfds)
names(dfOLSIRGISDcontrol)[names(dfOLSIRGISDcontrol)=="coefficientReg"] <- "OLS, age stnd + control"
dfOLSIRGISDcontrol
# SAR model with controls
dfSARIRGISDcontrol <- regvarlist("GISD",varlistLNIREU,varlistControl3,month1, "SAR",dfds)
names(dfSARIRGISDcontrol)[names(dfSARIRGISDcontrol)=="coefficientReg"] <- "SAR, age stnd + control"
dfSARIRGISDcontrol
# OLS model without controls
dfOLSIRGISD <- regvarlist("GISD",varlistLNIREU,varlistControl,month1,"SAR",dfds)
names(dfOLSIRGISD)[names(dfOLSIRGISD)=="coefficientReg"] <- "SAR, age standardised"
dfOLSIRGISD
# SAR model without controls
dfSARIRGISD <- regvarlist("GISD",varlistLNIREU,varlistControl,month1,"OLS",dfds)
names(dfSARIRGISD)[names(dfSARIRGISD)=="coefficientReg"] <- "OLS, age standardised"
dfSARIRGISD

# merge model data frames with coefficients
dfIRcontrol <- merge(dfOLSIRGISDcontrol, dfSARIRGISDcontrol, by = "month")
dfIRcontrol <- merge(dfIRcontrol, dfOLSIRGISD, by = "month")
dfIRcontrol <- merge(dfIRcontrol, dfSARIRGISD, by = "month")
dfIRcontrol
dfIRcontrol <- reshape(dfIRcontrol, times = c("SAR, age standardised", "OLS, age standardised", 
                                        "OLS, age stnd + control", "SAR, age stnd + control"), 
                    varying = c("SAR, age standardised", "OLS, age standardised", 
                                "OLS, age stnd + control", "SAR, age stnd + control"),  
                    idvar = "month", v.name="coefficients", direction = "long")
names(dfIRcontrol)[names(dfIRcontrol)=="time"] <- "model"
xlabels <- sort(unique(dfIRcontrol$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

ggplot(dfIRcontrol, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8,aes(linetype=model,color=model)) +
  geom_hline(aes(yintercept = 0)) +
  scale_linetype_manual(values=c("solid", "twodash", "solid", "twodash")) +
  scale_color_manual(values=c("red", "red", "blue", "blue")) +
  scale_x_discrete(labels = xlabels) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "log. Incidence Rates ~ German Index of socioeconomic Deprivation + pop. Density + state dummies", 
       caption = "Read: ''The most socioeconomicly deprived
                  county has a 0.5% to 1.5% higher incidence rate compared
                 to the least socioeconomicly deprived county as of March 21''") +
  theme_bw()

## control for sum of previous cases
# OLS model with 
dfOLSIRGISDsum <- regvarlistP("GISD",varlistLNIREUlag1,varlistControl,varlistSumCases,month2, "OLS",dfds)
names(dfOLSIRGISDsum)[names(dfOLSIRGISDsum)=="coefficientReg"] <- "OLS, age stnd + sum"
dfOLSIRGISDsum
# SAR model with 
dfSARIRGISDsum <- regvarlistP("GISD",varlistLNIREUlag1,varlistControl,varlistSumCases,month2, "SAR",dfds)
names(dfSARIRGISDsum)[names(dfSARIRGISDsum)=="coefficientReg"] <- "SAR, age stnd + sum"
dfSARIRGISDsum
# OLS model without 
dfOLSIRGISD <- regvarlist("GISD",varlistLNIREUlag1,varlistControl,month2,"SAR",dfds)
names(dfOLSIRGISD)[names(dfOLSIRGISD)=="coefficientReg"] <- "SAR, age standardised"
dfOLSIRGISD
# SAR model without IR lag
dfSARIRGISD <- regvarlist("GISD",varlistLNIREUlag1,varlistControl,month2,"OLS",dfds)
names(dfSARIRGISD)[names(dfSARIRGISD)=="coefficientReg"] <- "OLS, age standardised"
dfSARIRGISD

# merge model data frames with coefficients
dfIRsum <- merge(dfOLSIRGISDsum, dfSARIRGISDsum, by = "month")
dfIRsum <- merge(dfIRsum, dfOLSIRGISD, by = "month")
dfIRsum <- merge(dfIRsum, dfSARIRGISD, by = "month")
dfIRsum
dfIRsum <- reshape(dfIRsum, times = c("SAR, age standardised", "OLS, age standardised", 
                                        "OLS, age stnd + sum", "SAR, age stnd + sum"), 
                    varying = c("SAR, age standardised", "OLS, age standardised", 
                                "OLS, age stnd + sum", "SAR, age stnd + sum"),  
                    idvar = "month", v.name="coefficients", direction = "long")
names(dfIRsum)[names(dfIRsum)=="time"] <- "model"
xlabels <- sort(unique(dfIRsum$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

ggplot(dfIRsum, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8,aes(linetype=model,color=model)) +
  geom_hline(aes(yintercept = 0)) +
  scale_linetype_manual(values=c("solid", "twodash", "solid", "twodash")) +
  scale_color_manual(values=c("red", "red", "blue", "blue")) +
  scale_x_discrete(labels = xlabels) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "log. Incidence Rates ~ German Index of socioeconomic Deprivation + pop. Density + state dummies", 
       caption = "Read: ''The most socioeconomicly deprived
                  county has a 0.5% to 1.5% higher incidence rate compared
                 to the least socioeconomicly deprived county as of March 21''") +
  theme_bw()

## differentiated by gender
# OLS model male 
dfOLSIRGISDm <- regvarlist("GISD",varlistLNIREU,varlistControl,month1, "OLS",dfdsm)
names(dfOLSIRGISDm)[names(dfOLSIRGISDm)=="coefficientReg"] <- "OLS, age stnd, male"
dfOLSIRGISDm
# SAR model male 
dfSARIRGISDm <- regvarlist("GISD",varlistLNIREU,varlistControl,month1, "SAR",dfdsm)
names(dfSARIRGISDm)[names(dfSARIRGISDm)=="coefficientReg"] <- "SAR, age stnd, male"
dfSARIRGISDm
# OLS model female 
dfOLSIRGISDf <- regvarlist("GISD",varlistLNIREU,varlistControl,month1,"SAR",dfdsf)
names(dfOLSIRGISDf)[names(dfOLSIRGISDf)=="coefficientReg"] <- "SAR, age stnd, female"
dfOLSIRGISDf
# SAR model female
dfSARIRGISDf <- regvarlist("GISD",varlistLNIREU,varlistControl,month1,"OLS",dfdsf)
names(dfSARIRGISDf)[names(dfSARIRGISDf)=="coefficientReg"] <- "OLS, age stnd, female"
dfSARIRGISDf

# merge model data frames with coefficients
dfIRgender <- merge(dfOLSIRGISDm, dfSARIRGISDm, by = "month")
dfIRgender <- merge(dfIRgender, dfOLSIRGISDf, by = "month")
dfIRgender <- merge(dfIRgender, dfSARIRGISDf, by = "month")
dfIRgender
dfIRgender <- reshape(dfIRgender, times = c("SAR, age stnd, male", "OLS, age stnd, male", 
                                      "OLS, age stnd, female", "SAR, age stnd, female"), 
                   varying = c("SAR, age stnd, male", "OLS, age stnd, male", 
                               "OLS, age stnd, female", "SAR, age stnd, female"),  
                   idvar = "month", v.name="coefficients", direction = "long")
names(dfIRgender)[names(dfIRgender)=="time"] <- "model"
xlabels <- sort(unique(dfIRgender$month))
xlabels[seq(2, length(xlabels), 2)] <- " "

ggplot(dfIRgender, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8,aes(linetype=model,color=model)) +
  geom_hline(aes(yintercept = 0)) +
  scale_linetype_manual(values=c("twodash", "twodash", "solid", "solid")) +
  scale_color_manual(values=c("blue", "red", "blue", "red")) +
  scale_x_discrete(labels = xlabels) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "log. Incidence Rates ~ German Index of socioeconomic Deprivation + pop. Density + state dummies", 
       caption = "Read: ''The most socioeconomicly deprived
                  county has a 1% to 1.5% higher incidence rate compared
                 to the least socioeconomicly deprived county as of March 21''") +
  theme_bw()












## population weights for counties / crude aggregated cases 
# as IR = cases / population * 100,000; pure cases can be used as a weighted measure of IR
  # factor 100,000 cancels out in regression as applied to all cases
# however, this means that these are not age standardised, but note that this had no great effect in previous models

# SAR model with weights
dfSARIRGISDweight <- regvarlist("GISD",varlistLNcases,varlistControl,month1, "SAR",dfds)
names(dfSARIRGISDweight)[names(dfSARIRGISDweight)=="coefficientReg"] <- "SAR, weighted by pop."
dfSARIRGISDweight
# OLS model without weights
dfOLSIRGISD <- regvarlist("GISD",varlistLNIR,varlistControl,month1,"OLS",dfds)
names(dfOLSIRGISD)[names(dfOLSIRGISD)=="coefficientReg"] <- "OLS, no weights"
dfOLSIRGISD
# SAR model without weights
dfSARIRGISD <- regvarlist("GISD",varlistLNIR,varlistControl,month1,"SAR",dfds)
names(dfSARIRGISD)[names(dfSARIRGISD)=="coefficientReg"] <- "SAR, no weights"
dfSARIRGISD
# OLS model with weights
dfOLSIRGISDweight <- regvarlist("GISD",varlistLNcases,varlistControl,month1,"OLS",dfds)
names(dfOLSIRGISDweight)[names(dfOLSIRGISDweight)=="coefficientReg"] <- "OLS, weighted by pop."
dfOLSIRGISDweight

# merge model data frames with coefficients
dfIRweight <- merge(dfOLSIRGISDweight, dfSARIRGISDweight, by = "month")
dfIRweight <- merge(dfIRweight, dfOLSIRGISD, by = "month")
dfIRweight <- merge(dfIRweight, dfSARIRGISD, by = "month")
dfIRweight
dfIRweight <- reshape(dfIRweight, times = c("SAR, weighted by pop.", "OLS, weighted by pop.", 
                                        "OLS, no weights", "SAR, no weights"), 
                    varying = c("SAR, weighted by pop.", "OLS, weighted by pop.", 
                                "OLS, no weights", "SAR, no weights"),  
                    idvar = "month", v.name="coefficients", direction = "long")
names(dfIRweight)[names(dfIRweight)=="time"] <- "model"
xlabels <- sort(unique(dfIRweight$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

ggplot(dfIRweight, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 0.8,aes(linetype=model,color=model)) +
  geom_hline(size = 1.5, aes(yintercept = 0)) +
  scale_linetype_manual(values=c("solid", "twodash", "solid", "twodash")) +
  scale_color_manual(values=c("red", "red", "blue", "blue")) +
  scale_x_discrete(labels = xlabels) +
  labs(title="Effect size comparision",x="Month", y = "Coefficients", 
       subtitle = "log. Incidence Rate ~ German Index of socioeconomic Deprivation + pop. Density + state dummies", 
       caption = "Read: ''The most socioeconomicly deprived
                  county has a 0.5% to 1.5% higher incidence rate compared
                 to the least socioeconomicly deprived county as of March 21''") +
  theme_bw()













#### CFR

## spatial correlation rather weak and not significant throughout the year
moran.test(dfds$CFREU, weighted_neighbors, zero.policy=T)
moran.test(dfds$CFREU20.04, weighted_neighbors, zero.policy=T)
moran.test(dfds$CFREU20.08, weighted_neighbors, zero.policy=T)
moran.test(dfds$CFREU20.12, weighted_neighbors, zero.policy=T)
moran.test(dfds$CFREU21.04, weighted_neighbors, zero.policy=T)
moran.plot(dfds$CFREU, weighted_neighbors, zero.policy=T,xlab="case fatality ratio, age standardised", ylab="spatially lagged CFR")
moran.plot(dfds$CFREU20.04, weighted_neighbors, zero.policy=T,xlab="case fatality ratio, age standardised", ylab="spatially lagged CFR")
moran.plot(dfds$CFREU21.04, weighted_neighbors, zero.policy=T,xlab="case fatality ratio, age standardised", ylab="spatially lagged CFR")


## linearity check > no clear trend
OLSCFREU21.04 <- lm(CFREU21.04 ~ GISD + LNpopDensity+SH+HH+NI+HB+MV+BB+BE+ST+SN+TH+NW+HE+RP+BY+SL, data=dfdd)
dfOLSCFREU21.04 <- data.frame(OLSCFREU21.04_pred = predict(OLSCFREU21.04, dfdd), GISD=dfdd$GISD)

ggplot(dfdd, aes(x=GISD, y=CFREU21.04)) +
  geom_smooth(method='lm', formula= y~x, se=FALSE, aes(color="no controls")) +
  geom_line(data = dfOLSCFREU21.04, aes(color="controls", x=GISD, y=OLSCFREU21.04_pred)) +
  geom_point() +
  labs(title="CFR ~ GISD",x="German Index of Socioeconomic Deprivation", y = "case fatality ratio 21-04") +
  theme_bw() +
  scale_color_manual(name="Regressions", 
                     breaks=c("no controls", "controls"), 
                     values=c("no controls"="red", "controls"="blue")) 
# no linear trend observable in CFR data
ggplot(dfdd, aes(x=GISD, y=CFREU20.04)) +
  geom_point() +
  labs(title="CFR ~ GISD",x="German Index of Socioeconomic Deprivation", y = "case fatality ratio 20-04") +
  theme_bw() 
ggplot(dfdd, aes(x=GISD, y=CFREU20.08)) +
  geom_point() +
  labs(title="CFR ~ GISD",x="German Index of Socioeconomic Deprivation", y = "case fatality ratio 20-08") +
  theme_bw() 
ggplot(dfdd, aes(x=GISD, y=CFREU20.12)) +
  geom_point() +
  labs(title="CFR ~ GISD",x="German Index of Socioeconomic Deprivation", y = "case fatality ratio 20-12") +
  theme_bw() 
ggplot(dfdd, aes(x=GISD, y=CFREU21.04)) +
  geom_point() +
  labs(title="CFR ~ GISD",x="German Index of Socioeconomic Deprivation", y = "case fatality ratio 21-04") +
  theme_bw() 

## -> no linear relationship, no models calculated

### lagged CFR
ggplot(dfdd, aes(x=GISD, y=CFREUlag20.04)) +
  geom_point() +
  labs(title="CFR ~ GISD",x="German Index of Socioeconomic Deprivation", y = "case fatality ratio 20-04") +
  theme_bw() 
ggplot(dfdd, aes(x=GISD, y=CFREUlag20.08)) +
  geom_point() +
  labs(title="CFR ~ GISD",x="German Index of Socioeconomic Deprivation", y = "case fatality ratio 20-08") +
  theme_bw() 
ggplot(dfdd, aes(x=GISD, y=CFREUlag20.12)) +
  geom_point() +
  labs(title="CFR ~ GISD",x="German Index of Socioeconomic Deprivation", y = "case fatality ratio 20-12") +
  theme_bw() 
ggplot(dfdd, aes(x=GISD, y=CFREUlag21.04)) +
  geom_point() +
  labs(title="CFR ~ GISD",x="German Index of Socioeconomic Deprivation", y = "case fatality ratio 21-04") +
  theme_bw() 

## -> no linear relationship, no models calculated




### Further Descriptive Tables etc. as LaTeX Output

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

stargazer(dfdd[c("CFR","IR","cases","deaths","CFR20.02",
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

#Maps
dfdsMAP <- dfds[,c("geometry", "KRS", "medInc", "IR", "IR20.04", "IR20.10", "IR21.04", "GISD", "VR21.10")]
names(dfdsMAP) <- c("geometry", "KRS", "Median Income",
                    "Incidence Rates, until June 2021", 
                    "Incidence Rates, April 2020", 
                    "Incidence Rates, October 2020",
                    "Incidence Rates, April 2021", 
                    "German Index of Socioeconomic Deprivation", 
                    "Vaccinations per Capita, October 2021")

plot(dfdsMAP["Incidence Rates, April 2020"], key.pos = 4, nbreaks = 10,border="white")
plot(dfdsMAP["Incidence Rates, October 2020"], key.pos = 4, nbreaks = 10,border="white")
plot(dfdsMAP["Incidence Rates, April 2021"], key.pos = 4, nbreaks = 10,border="white")
plot(dfdsMAP["German Index of Socioeconomic Deprivation"], key.pos = 4, nbreaks = 10,border="white")
plot(dfdsMAP["Vaccinations per Capita, October 2021"], key.pos = 4, nbreaks = 10,border="white")
plot(dfdsMAP["Incidence Rates, until June 2021"], key.pos = 4, nbreaks = 10,border="white")
plot(dfdsMAP["Median Income"], key.pos = 4, nbreaks = 10,border="white")


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
dfcasesSum$month <- month1
names(dfcasesSum)[names(dfcasesSum)=="dfcasesSum"] <- "IR"
dfcasesSum$IR <- (dfcasesSum$IR / popSum) * 100000
dfcasesSum

dfcasesf <- dfrkiaggrf[,c("cases20.02","cases20.03","cases20.04","cases20.05","cases20.06",
                        "cases20.07","cases20.08","cases20.09","cases20.10",
                        "cases20.11","cases20.12","cases21.01","cases21.02",
                        "cases21.03","cases21.04","cases21.05","cases21.06")]
dfcasesf$population <- dfdd$population
dfcasesf$shareWomen <- dfdd$shareWomen / 100
dfcasesf$population <- dfcasesf$population * dfcasesf$shareWomen
dfcasesf$shareWomen <- NULL
dfcasesSumf <- colSums(dfcasesf)
popSumf <- as.numeric(dfcasesSumf[[18]]) 
dfcasesSumf <- dfcasesSumf[-18]
dfcasesSumf <- data.frame(dfcasesSumf)
dfcasesSumf$month <- month1
names(dfcasesSumf)[names(dfcasesSumf)=="dfcasesSumf"] <- "IR, female"
dfcasesSumf$"IR, female" <- (dfcasesSumf$"IR, female" / popSumf) * 100000
dfcasesSumf

dfcasesm <- dfrkiaggrm[,c("cases20.02","cases20.03","cases20.04","cases20.05","cases20.06",
                          "cases20.07","cases20.08","cases20.09","cases20.10",
                          "cases20.11","cases20.12","cases21.01","cases21.02",
                          "cases21.03","cases21.04","cases21.05","cases21.06")]
dfcasesm$population <- dfdd$population
dfcasesm$shareWomen <- dfdd$shareWomen / 100
dfcasesm$population <- dfcasesm$population * (1-dfcasesm$shareWomen)
dfcasesm$shareWomen <- NULL
dfcasesSumm <- colSums(dfcasesm)
popSumm <- as.numeric(dfcasesSumm[[18]])
dfcasesSumm <- dfcasesSumm[-18]
dfcasesSumm <- data.frame(dfcasesSumm)
dfcasesSumm$month <- month1
names(dfcasesSumm)[names(dfcasesSumm)=="dfcasesSumm"] <- "IR, male"
dfcasesSumm$"IR, male" <- (dfcasesSumm$"IR, male" / popSumm) * 100000
dfcasesSumm

dfcasesSum <- merge(dfcasesSum, dfcasesSumf, by = "month")
dfcasesSum <- merge(dfcasesSum, dfcasesSumm, by = "month")

dfcasesSum <- reshape(dfcasesSum, times = c("IR", "IR, male", 
                                            "IR, female"), 
                      varying = c("IR", "IR, male", 
                                  "IR, female"),  
                      idvar = "month", v.name="IR", direction = "long")
names(dfcasesSum)[names(dfcasesSum)=="time"] <- "gender"
xlabels <- sort(unique(dfcasesSum$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

ggplot(dfcasesSum, aes(x=month, y=IR, group = gender, color = gender)) +
  geom_line(size = 0.8,aes(linetype=gender,color=gender)) +
  scale_linetype_manual(values=c("solid", "twodash", "dashed")) +
  scale_color_manual(values=c("black", "blue", "red")) +
  scale_x_discrete(labels = xlabels) +
  geom_point() +
  labs(title="Average country-wide incidence rate per month and per gender",x="Month", y = "incidence rate")+
  theme_bw()







# CFR comparison 
dfaggrCFR <- dfrkiaggr[,c("deaths20.02","deaths20.03","deaths20.04","deaths20.05","deaths20.06",
                          "deaths20.07","deaths20.08","deaths20.09","deaths20.10",
                          "deaths20.11","deaths20.12","deaths21.01","deaths21.02",
                          "deaths21.03","deaths21.04","deaths21.05","deaths21.06")]
dfaggrCFRSum <- colSums(dfaggrCFR)
dfaggrCFRSum <- data.frame(dfaggrCFRSum)
dfaggrCFRSum <- dfaggrCFRSum[-18]
dfaggrCFRSum$month <- month1
names(dfaggrCFRSum)[names(dfaggrCFRSum)=="dfaggrCFRSum"] <- "CFR"
dfaggrCFRSum$"CFR" <- (dfaggrCFRSum$"CFR" / popSumm) * 100000
dfaggrCFRSum











dfaggrCFRf <- dfrkiaggrf[,c("deaths20.02","deaths20.03","deaths20.04","deaths20.05","deaths20.06",
                            "deaths20.07","deaths20.08","deaths20.09","deaths20.10",
                            "deaths20.11","deaths20.12","deaths21.01","deaths21.02",
                            "deaths21.03","deaths21.04","deaths21.05","deaths21.06",
                          "cases20.02","cases20.03","cases20.04","cases20.05","cases20.06",
                          "cases20.07","cases20.08","cases20.09","cases20.10",
                          "cases20.11","cases20.12","cases21.01","cases21.02",
                          "cases21.03","cases21.04","cases21.05","cases21.06")]
dfaggrCFRSumf <- colSums(dfaggrCFRf)
dfaggrCFRSumf <- data.frame(dfaggrCFRSumf)
dfaggrCFRSumf <- data.frame(dfaggrCFRSumf)
dfaggrCFRSumf$cases <- dfaggrCFRSumf[1:16,1]
dfaggrCFRSumf$deaths <- dfaggrCFRSumf[17:32,1]
dfaggrCFRSumf$dfaggrCFRSumf <- NULL
dfaggrCFRSumf <- dfaggrCFRSumf[-(17:32),]
dfaggrCFRSumf$month <- month1
dfaggrCFRSumf$CFRf <- dfaggrCFRSumf$deaths / dfaggrCFRSumf$cases
dfaggrCFRSumf

dfaggrCFR <- dfrkiaggr[,c("deaths20.02","deaths20.03","deaths20.04","deaths20.05","deaths20.06",
                          "deaths20.07","deaths20.08","deaths20.09","deaths20.10",
                          "deaths20.11","deaths20.12","deaths21.01","deaths21.02",
                          "deaths21.03","deaths21.04","deaths21.05",
                          "cases20.02","cases20.03","cases20.04","cases20.05","cases20.06",
                          "cases20.07","cases20.08","cases20.09","cases20.10",
                          "cases20.11","cases20.12","cases21.01","cases21.02",
                          "cases21.03","cases21.04","cases21.05","cases21.06")]
dfaggrCFRSum <- colSums(dfaggrCFR)
dfaggrCFRSum <- data.frame(dfaggrCFRSum)
dfaggrCFRSum <- data.frame(dfaggrCFRSum)
dfaggrCFRSum$cases <- dfaggrCFRSum[1:16,1]
dfaggrCFRSum$deaths <- dfaggrCFRSum[17:32,1]
dfaggrCFRSum$dfaggrCFRSum <- NULL
dfaggrCFRSum <- dfaggrCFRSum[-(17:32),]
dfaggrCFRSum$month <- month1
dfaggrCFRSum$CFR <- dfaggrCFRSum$deaths / dfaggrCFRSum$cases
dfaggrCFRSum

dfcasesSum <- merge(dfcasesSum, dfcasesSumf, by = "month")
dfcasesSum <- merge(dfcasesSum, dfcasesSumm, by = "month")

dfcasesSum <- reshape(dfcasesSum, times = c("IR", "IR, male", 
                                            "IR, female"), 
                      varying = c("IR", "IR, male", 
                                  "IR, female"),  
                      idvar = "month", v.name="IR", direction = "long")
names(dfcasesSum)[names(dfcasesSum)=="time"] <- "gender"
xlabels <- sort(unique(dfcasesSum$month))
xlabels[seq(2, length(xlabels), 2)] <- ""


ggplot(dfcasesSum, aes(x=month, y=IR, group = gender, color = gender)) +
  geom_line(size = 0.8,aes(linetype=gender,color=gender)) +
  scale_linetype_manual(values=c("solid", "twodash", "dashed")) +
  scale_color_manual(values=c("black", "blue", "red")) +
  scale_x_discrete(labels = xlabels) +
  geom_point() +
  labs(title="Average country-wide incidence rate per month and per gender",x="Month", y = "incidence rate")+
  theme_bw()







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









