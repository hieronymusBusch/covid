##########################################################
#                                                        #
#                                                        #
# 2021/11                                                #
#                                                        #
# Alexander Busch (a.busch@lse.ac.uk)                    #
#                                                        #
# Data Analysis expanded time horizon                    #
#                                                        #
##########################################################

source("manipulation.R")

# expanded time horizon
varlistLNIRnew <- c("LNIR20.02","LNIR20.03","LNIR20.04","LNIR20.05","LNIR20.06",
                    "LNIR20.07","LNIR20.08","LNIR20.09","LNIR20.10",
                    "LNIR20.11","LNIR20.12","LNIR21.01","LNIR21.02",
                    "LNIR21.03","LNIR21.04","LNIR21.05","LNIR21.06", 
                    "LNIR21.07","LNIR21.08","LNIR21.09","LNIR21.10")
varlistLNIREUnew <- c("LNIREU20.02","LNIREU20.03","LNIREU20.04","LNIREU20.05","LNIREU20.06",
                      "LNIREU20.07","LNIREU20.08","LNIREU20.09","LNIREU20.10",
                      "LNIREU20.11","LNIREU20.12","LNIREU21.01","LNIREU21.02",
                      "LNIREU21.03","LNIREU21.04","LNIREU21.05","LNIREU21.06", 
                      "LNIREU21.07","LNIREU21.08","LNIREU21.09","LNIREU21.10")
monthnew <- c("20-02","20-03", "20-04", "20-05", "20-06", "20-07", "20-08", "20-09", 
              "20-10", "20-11", "20-12", "21-01", "21-02", "21-03", "21-04", "21-05", "21-06", 
              "21-07", "21-08", "21-09", "21-10")
varlistControl <- c("LNpopDensity","SH","HH","NI","HB","MV","BB","BE","ST","SN","TH","NW","HE","RP","BY","SL")

# Create lists needed for analysis
neighbors <- poly2nb(dfds)
weighted_neighbors <- nb2listw(neighbors, zero.policy=T)



# GISD LNIR(EU) 
dfIREU <- regvarlist("GISD",varlistLNIREUnew,varlistControl,monthnew,"SAR",dfdsnew)
names(dfIREU)[names(dfIREU)=="coefficientReg"] <- "SAR, age standardised"
dfIR <- regvarlist("GISD",varlistLNIRnew,varlistControl,monthnew,"SAR",dfdsnew)
names(dfIR)[names(dfIR)=="coefficientReg"] <- "SAR, not age standardised"
dfIREUOLS <- regvarlist("GISD",varlistLNIREUnew,varlistControl,monthnew,"OLS",dfdsnew)
names(dfIREUOLS)[names(dfIREUOLS)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfIR <- merge(dfIREU, dfIR, by = "month")
dfIR <- merge(dfIR, dfIREUOLS, by = "month")
dfIR <- reshape(dfIR, times = c("SAR, age standardised", "SAR, not age standardised", 
                                  "Lin. Reg., age standardised"), 
                 varying = c("SAR, age standardised", "SAR, not age standardised", 
                             "Lin. Reg., age standardised"),  
                 idvar = "month", v.name="coefficients", direction = "long")
names(dfIR)[names(dfIR)=="time"] <- "model"
xlabels <- sort(unique(dfIREU$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

graph <- ggplot(dfIR, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_discrete(labels = xlabels) +
  labs(title = "GISD", x="Month", y = "Coefficients")+
  theme_bw()
graph

# GISD LNIR(EU) female
dfIREU <- regvarlist("GISD",varlistLNIREUnew,varlistControl,monthnew,"SAR",dfdsfnew)
names(dfIREU)[names(dfIREU)=="coefficientReg"] <- "SAR, age standardised"
dfIR <- regvarlist("GISD",varlistLNIRnew,varlistControl,monthnew,"SAR",dfdsfnew)
names(dfIR)[names(dfIR)=="coefficientReg"] <- "SAR, not age standardised"
dfIREUOLS <- regvarlist("GISD",varlistLNIREUnew,varlistControl,monthnew,"OLS",dfdsfnew)
names(dfIREUOLS)[names(dfIREUOLS)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfIR <- merge(dfIREU, dfIR, by = "month")
dfIR <- merge(dfIR, dfIREUOLS, by = "month")
dfIR <- reshape(dfIR, times = c("SAR, age standardised", "SAR, not age standardised", 
                                "Lin. Reg., age standardised"), 
                varying = c("SAR, age standardised", "SAR, not age standardised", 
                            "Lin. Reg., age standardised"),  
                idvar = "month", v.name="coefficients", direction = "long")
names(dfIR)[names(dfIR)=="time"] <- "model"
xlabels <- sort(unique(dfIREU$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

graphf <- ggplot(dfIR, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_discrete(labels = xlabels) +
  labs(title = "GISD, only female cases", x="Month", y = "Coefficients")+
  theme_bw()
graphf

# GISD LNIR(EU) male 
dfIREU <- regvarlist("GISD",varlistLNIREUnew,varlistControl,monthnew,"SAR",dfdsmnew)
names(dfIREU)[names(dfIREU)=="coefficientReg"] <- "SAR, age standardised"
dfIR <- regvarlist("GISD",varlistLNIRnew,varlistControl,monthnew,"SAR",dfdsmnew)
names(dfIR)[names(dfIR)=="coefficientReg"] <- "SAR, not age standardised"
dfIREUOLS <- regvarlist("GISD",varlistLNIREUnew,varlistControl,monthnew,"OLS",dfdsmnew)
names(dfIREUOLS)[names(dfIREUOLS)=="coefficientReg"] <- "Lin. Reg., age standardised"

dfIR <- merge(dfIREU, dfIR, by = "month")
dfIR <- merge(dfIR, dfIREUOLS, by = "month")
dfIR <- reshape(dfIR, times = c("SAR, age standardised", "SAR, not age standardised", 
                                "Lin. Reg., age standardised"), 
                varying = c("SAR, age standardised", "SAR, not age standardised", 
                            "Lin. Reg., age standardised"),  
                idvar = "month", v.name="coefficients", direction = "long")
names(dfIR)[names(dfIR)=="time"] <- "model"
xlabels <- sort(unique(dfIREU$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

graphm <- ggplot(dfIR, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_discrete(labels = xlabels) +
  labs(title = "GISD, only male cases", x="Month", y = "Coefficients")+
  theme_bw()
graphm

# combine graphs
grid.arrange(graph, graphf, graphm, ncol=1, nrow=3)






# GISD LNIR(EU) with only extreme quintiles (no spatial model possible, as only subset of counties)
dfIRQ <- regvarlist("GISD",varlistLNIREUnew,varlistControl,monthnew,"OLS",dfdsQ)
names(dfIRQ)[names(dfIRQ)=="coefficientReg"] <- "Lin. Reg., age standardised"
dfIRQa <- regvarlist("GISD",varlistLNIRnew,varlistControl,monthnew,"OLS",dfdsQ)
names(dfIRQa)[names(dfIRQa)=="coefficientReg"] <- "Lin. Reg., not age standardised"

dfIRQ <- merge(dfIRQa, dfIRQ, by = "month")
dfIRQ <- reshape(dfIRQ, times = c("Lin. Reg., age standardised", "Lin. Reg., not age standardised"), 
                varying = c("Lin. Reg., age standardised", "Lin. Reg., not age standardised"),  
                idvar = "month", v.name="coefficients", direction = "long")
names(dfIRQ)[names(dfIRQ)=="time"] <- "model"
xlabels <- sort(unique(dfIRQ$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

graph <- ggplot(dfIRQ, aes(x=month, y=coefficients, group = model, color = model)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_discrete(labels = xlabels) +
  labs(title = "GISD", x="Month", y = "Coefficients")+
  theme_bw()
graph










#Course of Pandemic in Germany
dfcases <- dfddnew[,c("cases20.02","cases20.03","cases20.04","cases20.05","cases20.06",
                        "cases20.07","cases20.08","cases20.09","cases20.10",
                        "cases20.11","cases20.12","cases21.01","cases21.02",
                        "cases21.03","cases21.04","cases21.05","cases21.06", 
                        "cases21.07","cases21.08","cases21.09","cases21.10")]
popSum <- sum(dfddnew$population)
dfcasesSum <- colSums(dfcases)
dfcasesSum <- as.data.frame(dfcasesSum)
dfcasesSum$month <- monthnew
names(dfcasesSum)[names(dfcasesSum)=="dfcasesSum"] <- "IR"
dfcasesSum$IR <- (dfcasesSum$IR / popSum) * 100000
xlabels <- sort(unique(dfcasesSum$month))
xlabels[seq(2, length(xlabels), 2)] <- ""

ggplot(dfcasesSum, aes(x=month, y=IR, group = 1)) +
  geom_line() +
  scale_x_discrete(labels = xlabels) +
  geom_point() +
  labs(title="Average country-wide incidence rate per month",x="Month", y = "incidence rate")+
  theme_bw()

# maps
dfdsMAP <- dfdsnew[,c("geometry", "KRS", "medInc", "IR", "IR20.04", "IR20.10", 
                   "IR21.04", "IR21.10", "GISD", "VR21.10")]
names(dfdsMAP) <- c("geometry", "KRS", "Median Income",
                    "Incidence Rates, until October 2021", 
                    "Incidence Rates, April 2020", 
                    "Incidence Rates, October 2020",
                    "Incidence Rates, April 2021", 
                    "Incidence Rates, October 2021",
                    "German Index of Socioeconomic Deprivation", 
                    "Vaccinations per Capita, October 2021")

plot(dfdsMAP["Incidence Rates, April 2020"], key.pos = 4, nbreaks = 10,border="white")
plot(dfdsMAP["Incidence Rates, October 2020"], key.pos = 4, nbreaks = 10,border="white")
plot(dfdsMAP["Incidence Rates, April 2021"], key.pos = 4, nbreaks = 10,border="white")
plot(dfdsMAP["Incidence Rates, October 2021"], key.pos = 4, nbreaks = 10,border="white")
plot(dfdsMAP["German Index of Socioeconomic Deprivation"], key.pos = 4, nbreaks = 10,border="white")
plot(dfdsMAP["Vaccinations per Capita, October 2021"], key.pos = 4, nbreaks = 10,border="white")
plot(dfdsMAP["Incidence Rates, until October 2021"], key.pos = 4, nbreaks = 10,border="white")
plot(dfdsMAP["Median Income"], key.pos = 4, nbreaks = 10,border="white")



