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



### Combining data 

listrkiaggr <- list(dfrkiaggr20.02,dfrkiaggr20.03,dfrkiaggr20.04,dfrkiaggr20.05,dfrkiaggr20.06,dfrkiaggr20.07,
                    dfrkiaggr20.08,dfrkiaggr20.09,dfrkiaggr20.10,dfrkiaggr20.11,dfrkiaggr20.12,
                    dfrkiaggr21.01,dfrkiaggr21.02,dfrkiaggr21.03,dfrkiaggr21.04,dfrkiaggr21.05,dfrkiaggr21.06)

for (a in listrkiaggr) {
  dfrkiaggr <- merge(dfrkiaggr, a, by = "KRS",all = TRUE)
}
dfrkiaggr[is.na(dfrkiaggr)] <- 0


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

## Combinig aggr data and spatial / geographic data
dfdd <- merge(dfdd, dfrkiaggr, by.dfdd = KRS, by.dfrkiaggr = KRS)

# ln
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

dfdd$LNhhInc <- log(dfdd$hhInc)
dfdd$LNmedInc <- log(dfdd$medInc)
dfdd$LNpopPerDoc <- log(dfdd$popPerDoc)
dfdd$LNpopDensity <- log(dfdd$popDensity)

# to prevent 0s to become neg. infinite (by ln(0)), replace those with -2.3 (incedence of 1 per 1,000,000)
dfdd[dfdd < -10000] <- -2.3

# combine with religious data
dfdd <- merge(dfdd, dfrel, by.dfdd = KRS, by.dfrel = KRS)

# Combine dfds and dfdd for mapping / SAR Modeling
dfds <- merge(dfds, dfdd, by.dfds = NUTS_CODE, by.dfdd = NUTS_CODE)

dfds$NUTS_NAME <- NULL
dfdd[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights
dfds[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights

