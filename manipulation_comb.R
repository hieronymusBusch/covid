##########################################################
#                                                        #
#                                                        #
# 2021/10                                                #
#                                                        #
# Alexander Busch (a.busch@lse.ac.uk)                    #
#                                                        #
# Data Manipulation Combination Dep/Ind Variables        #
#                                                        #
##########################################################

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

# logs for all data
dfdd$LNhhInc <- log(dfdd$hhInc)
dfdd$LNmedInc <- log(dfdd$medInc)
dfdd$LNpopPerDoc <- log(dfdd$popPerDoc)
dfdd$LNpopDensity <- log(dfdd$popDensity)
dfdd$LNbusinessTax <- log(dfdd$businessTax)
dfdd$LNgrossInc <- log(dfdd$grossInc)

## Combinig aggr data and socioeconomical data
dfddf <- merge(dfdd, dfrkiaggrf, by = "KRS")
dfddm <- merge(dfdd, dfrkiaggrm, by = "KRS")
dfdd <- merge(dfdd, dfrkiaggr, by.dfdd = KRS, by.dfrkiaggr = KRS)

# logs 
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

# Sum of previous cases in county as proxy for immune people
dfdd$scases20.02 <- (dfdd$cases20.02) / dfdd$population
dfdd$scases20.03 <- (dfdd$cases20.03) / dfdd$population + dfdd$scases20.02 
dfdd$scases20.04 <- (dfdd$cases20.04) / dfdd$population + dfdd$scases20.03 
dfdd$scases20.05 <- (dfdd$cases20.05) / dfdd$population + dfdd$scases20.04 
dfdd$scases20.06 <- (dfdd$cases20.06) / dfdd$population + dfdd$scases20.05 
dfdd$scases20.07 <- (dfdd$cases20.07) / dfdd$population + dfdd$scases20.06 
dfdd$scases20.08 <- (dfdd$cases20.08) / dfdd$population + dfdd$scases20.07 
dfdd$scases20.09 <- (dfdd$cases20.09) / dfdd$population + dfdd$scases20.08 
dfdd$scases20.10 <- (dfdd$cases20.10) / dfdd$population + dfdd$scases20.09 
dfdd$scases20.11 <- (dfdd$cases20.11) / dfdd$population + dfdd$scases20.10 
dfdd$scases20.12 <- (dfdd$cases20.12) / dfdd$population + dfdd$scases20.11 
dfdd$scases21.01 <- (dfdd$cases21.01) / dfdd$population + dfdd$scases20.12 
dfdd$scases21.02 <- (dfdd$cases21.02) / dfdd$population + dfdd$scases21.01 
dfdd$scases21.03 <- (dfdd$cases21.03) / dfdd$population + dfdd$scases21.02 
dfdd$scases21.04 <- (dfdd$cases21.04) / dfdd$population + dfdd$scases21.03 
dfdd$scases21.05 <- (dfdd$cases21.05) / dfdd$population + dfdd$scases21.04 
dfdd$scases21.06 <- (dfdd$cases21.06) / dfdd$population + dfdd$scases21.05 

# logs 
dfddf$LNIR20.02 <- log(dfddf$IR20.02)
dfddf$LNIR20.03 <- log(dfddf$IR20.03)
dfddf$LNIR20.04 <- log(dfddf$IR20.04)
dfddf$LNIR20.05 <- log(dfddf$IR20.05)
dfddf$LNIR20.06 <- log(dfddf$IR20.06)
dfddf$LNIR20.07 <- log(dfddf$IR20.07)
dfddf$LNIR20.08 <- log(dfddf$IR20.08)
dfddf$LNIR20.09 <- log(dfddf$IR20.09)
dfddf$LNIR20.10 <- log(dfddf$IR20.10)
dfddf$LNIR20.11 <- log(dfddf$IR20.11)
dfddf$LNIR20.12 <- log(dfddf$IR20.12)
dfddf$LNIR21.01 <- log(dfddf$IR21.01)
dfddf$LNIR21.02 <- log(dfddf$IR21.02)
dfddf$LNIR21.03 <- log(dfddf$IR21.03)
dfddf$LNIR21.04 <- log(dfddf$IR21.04)
dfddf$LNIR21.05 <- log(dfddf$IR21.05)
dfddf$LNIR21.06 <- log(dfddf$IR21.06)
dfddf$LNIR <- log(dfddf$IR)

dfddf$LNIREU20.02 <- log(dfddf$IREU20.02)
dfddf$LNIREU20.03 <- log(dfddf$IREU20.03)
dfddf$LNIREU20.04 <- log(dfddf$IREU20.04)
dfddf$LNIREU20.05 <- log(dfddf$IREU20.05)
dfddf$LNIREU20.06 <- log(dfddf$IREU20.06)
dfddf$LNIREU20.07 <- log(dfddf$IREU20.07)
dfddf$LNIREU20.08 <- log(dfddf$IREU20.08)
dfddf$LNIREU20.09 <- log(dfddf$IREU20.09)
dfddf$LNIREU20.10 <- log(dfddf$IREU20.10)
dfddf$LNIREU20.11 <- log(dfddf$IREU20.11)
dfddf$LNIREU20.12 <- log(dfddf$IREU20.12)
dfddf$LNIREU21.01 <- log(dfddf$IREU21.01)
dfddf$LNIREU21.02 <- log(dfddf$IREU21.02)
dfddf$LNIREU21.03 <- log(dfddf$IREU21.03)
dfddf$LNIREU21.04 <- log(dfddf$IREU21.04)
dfddf$LNIREU21.05 <- log(dfddf$IREU21.05)
dfddf$LNIREU21.06 <- log(dfddf$IREU21.06)
dfddf$LNIREU <- log(dfddf$IREU)

# Sum of previous cases in county as proxy for immune people
dfddf$scases20.02 <- (dfddf$cases20.02) / dfddf$population
dfddf$scases20.03 <- (dfddf$cases20.03) / dfddf$population + dfddf$scases20.02 
dfddf$scases20.04 <- (dfddf$cases20.04) / dfddf$population + dfddf$scases20.03 
dfddf$scases20.05 <- (dfddf$cases20.05) / dfddf$population + dfddf$scases20.04 
dfddf$scases20.06 <- (dfddf$cases20.06) / dfddf$population + dfddf$scases20.05 
dfddf$scases20.07 <- (dfddf$cases20.07) / dfddf$population + dfddf$scases20.06 
dfddf$scases20.08 <- (dfddf$cases20.08) / dfddf$population + dfddf$scases20.07 
dfddf$scases20.09 <- (dfddf$cases20.09) / dfddf$population + dfddf$scases20.08 
dfddf$scases20.10 <- (dfddf$cases20.10) / dfddf$population + dfddf$scases20.09 
dfddf$scases20.11 <- (dfddf$cases20.11) / dfddf$population + dfddf$scases20.10 
dfddf$scases20.12 <- (dfddf$cases20.12) / dfddf$population + dfddf$scases20.11 
dfddf$scases21.01 <- (dfddf$cases21.01) / dfddf$population + dfddf$scases20.12 
dfddf$scases21.02 <- (dfddf$cases21.02) / dfddf$population + dfddf$scases21.01 
dfddf$scases21.03 <- (dfddf$cases21.03) / dfddf$population + dfddf$scases21.02 
dfddf$scases21.04 <- (dfddf$cases21.04) / dfddf$population + dfddf$scases21.03 
dfddf$scases21.05 <- (dfddf$cases21.05) / dfddf$population + dfddf$scases21.04 
dfddf$scases21.06 <- (dfddf$cases21.06) / dfddf$population + dfddf$scases21.05 

# logs 
dfddm$LNIR20.02 <- log(dfddm$IR20.02)
dfddm$LNIR20.03 <- log(dfddm$IR20.03)
dfddm$LNIR20.04 <- log(dfddm$IR20.04)
dfddm$LNIR20.05 <- log(dfddm$IR20.05)
dfddm$LNIR20.06 <- log(dfddm$IR20.06)
dfddm$LNIR20.07 <- log(dfddm$IR20.07)
dfddm$LNIR20.08 <- log(dfddm$IR20.08)
dfddm$LNIR20.09 <- log(dfddm$IR20.09)
dfddm$LNIR20.10 <- log(dfddm$IR20.10)
dfddm$LNIR20.11 <- log(dfddm$IR20.11)
dfddm$LNIR20.12 <- log(dfddm$IR20.12)
dfddm$LNIR21.01 <- log(dfddm$IR21.01)
dfddm$LNIR21.02 <- log(dfddm$IR21.02)
dfddm$LNIR21.03 <- log(dfddm$IR21.03)
dfddm$LNIR21.04 <- log(dfddm$IR21.04)
dfddm$LNIR21.05 <- log(dfddm$IR21.05)
dfddm$LNIR21.06 <- log(dfddm$IR21.06)
dfddm$LNIR <- log(dfddm$IR)

dfddm$LNIREU20.02 <- log(dfddm$IREU20.02)
dfddm$LNIREU20.03 <- log(dfddm$IREU20.03)
dfddm$LNIREU20.04 <- log(dfddm$IREU20.04)
dfddm$LNIREU20.05 <- log(dfddm$IREU20.05)
dfddm$LNIREU20.06 <- log(dfddm$IREU20.06)
dfddm$LNIREU20.07 <- log(dfddm$IREU20.07)
dfddm$LNIREU20.08 <- log(dfddm$IREU20.08)
dfddm$LNIREU20.09 <- log(dfddm$IREU20.09)
dfddm$LNIREU20.10 <- log(dfddm$IREU20.10)
dfddm$LNIREU20.11 <- log(dfddm$IREU20.11)
dfddm$LNIREU20.12 <- log(dfddm$IREU20.12)
dfddm$LNIREU21.01 <- log(dfddm$IREU21.01)
dfddm$LNIREU21.02 <- log(dfddm$IREU21.02)
dfddm$LNIREU21.03 <- log(dfddm$IREU21.03)
dfddm$LNIREU21.04 <- log(dfddm$IREU21.04)
dfddm$LNIREU21.05 <- log(dfddm$IREU21.05)
dfddm$LNIREU21.06 <- log(dfddm$IREU21.06)
dfddm$LNIREU <- log(dfddm$IREU)

# Sum of previous cases in county as proxy for immune people
dfddm$scases20.02 <- (dfddm$cases20.02) / dfddm$population
dfddm$scases20.03 <- (dfddm$cases20.03) / dfddm$population + dfddm$scases20.02 
dfddm$scases20.04 <- (dfddm$cases20.04) / dfddm$population + dfddm$scases20.03 
dfddm$scases20.05 <- (dfddm$cases20.05) / dfddm$population + dfddm$scases20.04 
dfddm$scases20.06 <- (dfddm$cases20.06) / dfddm$population + dfddm$scases20.05 
dfddm$scases20.07 <- (dfddm$cases20.07) / dfddm$population + dfddm$scases20.06 
dfddm$scases20.08 <- (dfddm$cases20.08) / dfddm$population + dfddm$scases20.07 
dfddm$scases20.09 <- (dfddm$cases20.09) / dfddm$population + dfddm$scases20.08 
dfddm$scases20.10 <- (dfddm$cases20.10) / dfddm$population + dfddm$scases20.09 
dfddm$scases20.11 <- (dfddm$cases20.11) / dfddm$population + dfddm$scases20.10 
dfddm$scases20.12 <- (dfddm$cases20.12) / dfddm$population + dfddm$scases20.11 
dfddm$scases21.01 <- (dfddm$cases21.01) / dfddm$population + dfddm$scases20.12 
dfddm$scases21.02 <- (dfddm$cases21.02) / dfddm$population + dfddm$scases21.01 
dfddm$scases21.03 <- (dfddm$cases21.03) / dfddm$population + dfddm$scases21.02 
dfddm$scases21.04 <- (dfddm$cases21.04) / dfddm$population + dfddm$scases21.03 
dfddm$scases21.05 <- (dfddm$cases21.05) / dfddm$population + dfddm$scases21.04 
dfddm$scases21.06 <- (dfddm$cases21.06) / dfddm$population + dfddm$scases21.05 

# to prevent 0s to become neg. infinite (by ln(0)), replace those with -2.3 (incedence of 1 per 1,000,000)
dfdd[dfdd < -10000] <- -2.3
dfddf[dfddf < -10000] <- -2.3
dfddm[dfddm < -10000] <- -2.3

# Combine dfds and dfdd for mapping / SAR Modeling
dfdsf <- merge(dfds, dfddf, by = "NUTS_CODE")
dfdsm <- merge(dfds, dfddm, by.dfds = NUTS_CODE, by.dfddm = NUTS_CODE)
dfds <- merge(dfds, dfdd, by = "NUTS_CODE")

dfds$NUTS_NAME <- NULL
dfdsf$NUTS_NAME <- NULL
dfdsm$NUTS_NAME <- NULL
dfdd[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights
dfds[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights
dfddf[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights
dfdsf[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights
dfddm[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights
dfdsm[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights

