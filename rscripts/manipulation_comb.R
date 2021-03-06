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

## load data from manipulation_dep/ind
load(file = "rdata/dfdd2.Rda")  
load(file = "rdata/dfds2.Rda")  
load(file = "rdata/dfrkiaggr.Rda")  
load(file = "rdata/dfrkiaggrf.Rda")  
load(file = "rdata/dfrkiaggrm.Rda") 
load(file = "rdata/dfrkiaggrnew.Rda")  
load(file = "rdata/dfrkiaggrfnew.Rda")  
load(file = "rdata/dfrkiaggrmnew.Rda") 

## Combinig aggr data and socioeconomical data
dfddf <- merge(dfdd2, dfrkiaggrf, by = "KRS")
dfddm <- merge(dfdd2, dfrkiaggrm, by = "KRS")
dfdd <- merge(dfdd2, dfrkiaggr, by = "KRS")

# to prevent 0s to become neg. infinite (by ln(0)), replace those with -2.3 (incedence of 1 per 1,000,000)
dfdd[dfdd < -10000] <- -2.3
dfddf[dfddf < -10000] <- -2.3
dfddm[dfddm < -10000] <- -2.3

# Combine dfds and dfdd for mapping / SAR Modeling
dfdsf <- merge(dfds2, dfddf, by = "NUTS_CODE")
dfdsm <- merge(dfds2, dfddm, by.dfds = NUTS_CODE, by.dfddm = NUTS_CODE)
dfds <- merge(dfds2, dfdd, by = "NUTS_CODE")

dfds$NUTS_NAME <- NULL
dfdsf$NUTS_NAME <- NULL
dfdsm$NUTS_NAME <- NULL
dfdd[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights
dfds[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights
dfddf[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights
dfdsf[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights
dfddm[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights
dfdsm[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights


### same for "new" data

## Combinig aggr data and socioeconomical data
dfddfnew <- merge(dfdd2, dfrkiaggrfnew, by = "KRS")
dfddmnew <- merge(dfdd2, dfrkiaggrmnew, by = "KRS")
dfddnew <- merge(dfdd2, dfrkiaggrnew, by = "KRS")

# to prevent 0s to become neg. infinite (by ln(0)), replace those with -2.3 (incedence of 1 per 1,000,000)
dfddnew[dfddnew < -10000] <- -2.3
dfddfnew[dfddfnew < -10000] <- -2.3
dfddmnew[dfddmnew < -10000] <- -2.3

# Combine dfds and dfdd for mapping / SAR Modeling
dfdsfnew <- merge(dfds2, dfddfnew, by = "NUTS_CODE")
dfdsmnew <- merge(dfds2, dfddmnew, by = "NUTS_CODE")
dfdsnew <- merge(dfds2, dfddnew, by = "NUTS_CODE")

dfdsnew$NUTS_NAME <- NULL
dfdsfnew$NUTS_NAME <- NULL
dfdsmnew$NUTS_NAME <- NULL
dfddnew[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights
dfdsnew[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights
dfddfnew[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights
dfdsfnew[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights
dfddmnew[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights
dfdsmnew[,c("fy0004", "fy0514", "fy1534", "fy3559", "fy6079", "fy80")] <- NULL # EU weights

# create df subset of only top and bottom 20% of GISD
quantile(dfdsnew$GISD, prob=c(.2,.8)) 
q1 <- quantile(dfdsnew$GISD, prob=c(.2)) 
q5 <- quantile(dfdsnew$GISD, prob=c(.8)) 
dfdsQ <- subset(dfdsnew, dfdsnew$GISD < q1 | dfdsnew$GISD > q5 )

# safe data
save(dfdsmnew, file = "rdata/dfdsmnew.Rda")
save(dfddmnew, file = "rdata/dfddmnew.Rda")
save(dfdsfnew, file = "rdata/dfdsfnew.Rda")
save(dfddfnew, file = "rdata/dfddfnew.Rda")
save(dfddnew, file = "rdata/dfddnew.Rda")
save(dfdsnew, file = "rdata/dfdsnew.Rda")
save(dfdsmnew, file = "rdata/dfdsm.Rda")
save(dfddm, file = "rdata/dfddm.Rda")
save(dfdsf, file = "rdata/dfdsf.Rda")
save(dfddf, file = "rdata/dfddf.Rda")
save(dfdd, file = "rdata/dfdd.Rda")
save(dfds, file = "rdata/dfds.Rda")
save(dfdsQ, file = "rdata/dfdsQ.Rda")








