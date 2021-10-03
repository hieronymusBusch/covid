##########################################################
#                                                        #
# BA Sociology:                                          #
# "COVID-19 in Germany, a socio-geographic perspective"  #
#                                                        #
# 2021/04                                                #
#                                                        #
# Alexander Busch (alexander.busch@stud.uni-hd.de)       #
#                                                        #
# Functions for Data Analysis                            #
#                                                        #
##########################################################

## In manipulation: 
# Aggregate Transform
# Aggregating cases and deaths for each month / age group + calculating EU stand. cases
# NOTE: This function relies on rkilist3, which is created in "manipulation" and contains suffixes; 
# as the incidence rate IR is defined as cases per 100,000 and the EU standard population is
# set at 100,000, both the aggregated EU standardised cases and IR are the same

aggregateTransform <- function(dfoutput,dfinput,date){
  # first aggregating cases and deaths by age group
  dfoutput <- ddply(dfinput, .(IdLandkreis, Altersgruppe), summarise, cases=sum(AnzahlFall))
  dfoutput <- reshape(dfoutput, idvar = "IdLandkreis", timevar = "Altersgruppe", direction = "wide")
  dfdeaths <- ddply(dfinput, .(IdLandkreis, Altersgruppe), summarise, deaths=sum(AnzahlTodesfall))
  dfdeaths <- reshape(dfdeaths, idvar = "IdLandkreis", timevar = "Altersgruppe", direction = "wide")
  dfoutput <- merge(dfoutput, dfdeaths, by.dfmerge = IdLandkreis, by.dfdd = IdLandkreis)
  names(dfoutput)[names(dfoutput)=="IdLandkreis"] <- "KRS"
  namescol <- colnames(dfoutput)
  dfoutput[is.na(dfoutput)] <- 0
  # giving unique names in order to later match
  for(i in 2:ncol(dfoutput)){
    ifelse(str_detect(namescol[[i]], "^c"), 
           names(dfoutput)[names(dfoutput)==namescol[[i]]] <- paste("cases", rkilist3[[i]], sep=""),
           names(dfoutput)[names(dfoutput)==namescol[[i]]] <- paste("deaths", rkilist3[[i]], sep="")
    ) 
  }
  # creating df with aggregate Berlin cases/deaths with correct geographic reference (KRS)
  dfberlin <- dfoutput %>% filter(
    KRS == "11001"|KRS =="11002"|KRS =="11003"|KRS =="11004"|KRS =="11005"|KRS =="11006"|
      KRS =="11007"|KRS =="11008"|KRS =="11009"|
      KRS =="11010"|KRS =="11011"|KRS =="11012") 
  dfoutput <- dfoutput[, c("KRS", sort(setdiff(names(dfoutput), "KRS")))]
  dfberlin2 <- data.frame(KRS = "11000")
  for(i in 2:ncol(dfoutput)){
    a <- sum(dfberlin[,i])
    dfberlin2 <- cbind(dfberlin2,a)
    ifelse(str_detect(namescol[[i]], "^c"), 
           names(dfberlin2)[names(dfberlin2)=="a"] <- paste("cases", rkilist3[[i]], sep=""),
           names(dfberlin2)[names(dfberlin2)=="a"] <- paste("deaths", rkilist3[[i]], sep="")
    ) 
  }
  dfoutput <- rbind(dfoutput,dfberlin2)
  dfoutput<- dfoutput %>% filter (!(
    KRS == "11001"|KRS =="11002"|KRS =="11003"|KRS =="11004"|KRS =="11005"|KRS =="11006"|
      KRS =="11007"|KRS =="11008"|KRS =="11009"|
      KRS =="11010"|KRS =="11011"|KRS =="11012")
  )
  # calculating sums: one crude sum, one standardised EU sum, IR and CFR both with and without EU weights
  dfoutput$cases <- as.numeric(apply(dfoutput[,2:7], 1, sum))
  dfoutput$deaths <- as.numeric(apply(dfoutput[,8:13], 1, sum))
  
  dfoutput <- merge(dfdd, dfoutput, by.dfdd = KRS, by.dfoutput = KRS, all = TRUE)
  
  dfoutput$IR <- (dfoutput$cases / dfoutput$population) * 100000
  dfoutput$IREU <- dfoutput$cases0004 * dfoutput$fy0004 + dfoutput$cases0514 * dfoutput$fy0514 +
    dfoutput$cases1534 * dfoutput$fy1534 + dfoutput$cases3559 * dfoutput$fy3559 +
    dfoutput$cases6079 * dfoutput$fy6079 + dfoutput$cases80 * dfoutput$fy80
  dfoutput$CFR <- (dfoutput$deaths / dfoutput$cases)
  dfoutput$deathsEU <- dfoutput$deaths0004 * dfoutput$fy0004 + dfoutput$deaths0514 * dfoutput$fy0514 +
    dfoutput$deaths1534 * dfoutput$fy1534 + dfoutput$deaths3559 * dfoutput$fy3559 +
    dfoutput$deaths6079 * dfoutput$fy6079 + dfoutput$deaths80 * dfoutput$fy80
  dfoutput$CFREU <- (dfoutput$deathsEU / dfoutput$IREU)
  
  dfoutput$IRdiff <- dfoutput$IR - dfoutput$IREU
  dfoutput$CFRdiff <- dfoutput$CFR - dfoutput$CFREU
  
  dfoutput <- dfoutput[,c(1,53:61)]
  colnames(dfoutput) <- paste(colnames(dfoutput), date, sep = "")
  namescol <- colnames(dfoutput)
  names(dfoutput)[names(dfoutput)==namescol[[1]]] <- "KRS"
  dfoutput
}





## In analysis: 
# these function rely on the month-list specified in the analysis
# OLS regress a var against a previously defined list of variables, output as df
OLSvarlist <- function(var, varList){
  dfoutput <- data.frame()
  for(a in varList) {
    inputvar1 <- a
    inputvar2 <- var
    f <- as.formula(paste(inputvar1,paste(inputvar2, collapse = " + "), sep = "~"))
    b <- summary(lm(f,dfdd,na.action=na.omit))$coefficients
    c <- data.frame(coefficientReg = b[2, 1])
    dfoutput <- rbind(dfoutput,c)
  }
  dfoutput$month <- month
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  dfoutput
}

# OLS regress as above, but with controllist
OLSvarlistC <- function(var, varList, controlList){
  dfoutput <- data.frame()
  for(a in varList) {
    inputvar1 <- a
    input2 <- c(var,controlList)
    f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
    b <- summary(lm(f,dfdd,na.action=na.omit))$coefficients
    c <- data.frame(coefficientReg = b[2, 1])
    dfoutput <- rbind(dfoutput,c)
  }
  dfoutput$month <- month
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  dfoutput
}

# OLS varlist for lagged (without feb and mar)
OLSvarlistLAG <- function(var, varList){
  dfoutput <- data.frame()
  for(a in varList) {
    inputvar1 <- a
    inputvar2 <- var
    f <- as.formula(paste(inputvar1,paste(inputvar2, collapse = " + "), sep = "~"))
    b <- summary(lm(f,dfdd,na.action=na.omit))$coefficients
    c <- data.frame(coefficientReg = b[2, 1])
    dfoutput <- rbind(dfoutput,c)
  }
  dfoutput$month <- c("20-04", "20-05", "20-06", "20-07", "20-08", "20-09", 
                       "20-10", "20-11", "20-12", "21-01", "21-02", "21-03", "21-04", "21-05", "21-06")
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  dfoutput
}

# OLS regress a var against a previously defined list of variables, output as df, including controls
OLSvarlist2 <- function(var, varList, controlList){
  dfoutput <- data.frame()
  input2 <- c(var,controlList)
  for(a in varList) {
    inputvar1 <- a
    f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
    b <- summary(lm(f,dfdd,na.action=na.omit))$coefficients
    c <- data.frame(coefficientReg = b[2, 1])
    dfoutput <- rbind(dfoutput,c)
  }
  dfoutput$month <- month
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  dfoutput
}

# OLS regress a var against a previously defined list of variables, 
# for 2 variables
OLSvarlist3 <- function(var1, var2, varList, controlList){
  dfoutput <- data.frame()
  dfoutput1 <- data.frame()
  dfoutput2 <- data.frame()
  input2 <- c(var1,var2,controlList)
  for(a in varList) {
    inputvar1 <- a
    f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
    f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
    b <- summary(lm(f,dfdd,na.action=na.omit))$coefficients
    c <- data.frame(coefficientReg1 = b[2, 1])
    d <- data.frame(coefficientReg2 = b[3, 1])
    dfoutput1 <- rbind(dfoutput1,c)
    dfoutput2 <- rbind(dfoutput2,d)
  }
  m <- data.frame(month = month)
  dfoutput <- rbind(dfoutput,m)
  dfoutput$coefficientReg <- round(dfoutput1$coefficientReg, digits = 5)
  dfoutput$coefficientReg2 <- round(dfoutput2$coefficientReg2, digits = 5)
  dfoutput
}

# OLS regress a var against a previously defined list of variables, output as df, including controls
# without february and march for lagged CFR robustnesstest
OLSvarlist4 <- function(var, varList, controlList){
  dfoutput <- data.frame()
  input2 <- c(var,controlList)
  for(a in varList) {
    inputvar1 <- a
    f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
    b <- summary(lm(f,dfdd,na.action=na.omit))$coefficients
    c <- data.frame(coefficientReg = b[2, 1])
    dfoutput <- rbind(dfoutput,c)
  }
  dfoutput$month <- month
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  dfoutput
}

# SAR regress a var against a previously defined list of variables, output as df
SARvarlist <- function(var, varList){
  dfoutput <- data.frame()
  for(a in varList) {
    inputvar1 <- a
    inputvar2 <- var
    f <- as.formula(paste(inputvar1,paste(inputvar2, collapse = " + "), sep = "~"))
    lag = lagsarlm(f, data=dfds, listw = weighted_neighbors,
                   tol.solve=1.0e-30, zero.policy=T)
    b <- impacts(lag, listw = weighted_neighbors)
    c <- data.frame(coefficientReg = as.numeric(b[3]))
    dfoutput <- rbind(dfoutput,c)
  }
  dfoutput$month <- month
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  dfoutput
}

# SAR regress as above, but with controls
SARvarlistC <- function(var, varList, controlList){
  dfoutput <- data.frame()
  input2 <- c(var,controlList)
  for(a in varList) {
    inputvar1 <- a
    f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
    lag = lagsarlm(f, data=dfds, listw = weighted_neighbors,
                   tol.solve=1.0e-30, zero.policy=T)
    b <- impacts(lag, listw = weighted_neighbors)
    b <- unlist(b)
    c <- data.frame(coefficientReg = as.numeric(b["total1"]))
    dfoutput <- rbind(dfoutput,c)
  }
  dfoutput$month <- month
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  dfoutput
}

# Controls for path dependency (previous var in the varList)
SARvarlistPath <- function(var, varList){
  dfoutput <- data.frame()
  for(a in 2:length(varList)) {
    inputvar1 <- a
    inputvar2 <- var
    f <- as.formula(paste(inputvar1,paste(inputvar2, collapse = " + "), sep = "~"))
    lag = lagsarlm(f, data=dfds, listw = weighted_neighbors,
                   tol.solve=1.0e-30, zero.policy=T)
    b <- impacts(lag, listw = weighted_neighbors)
    c <- data.frame(coefficientReg = as.numeric(b[3]))
    dfoutput <- rbind(dfoutput,c)
  }
  dfoutput$month <- month
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  dfoutput
}

# SAR lag for lagged (without feb and mar)
SARvarlistLAG <- function(var, varList){
  dfoutput <- data.frame()
  for(a in varList) {
    inputvar1 <- a
    inputvar2 <- var
    f <- as.formula(paste(inputvar1,paste(inputvar2, collapse = " + "), sep = "~"))
    lag = lagsarlm(f, data=dfds, listw = weighted_neighbors,
                   tol.solve=1.0e-30, zero.policy=T)
    b <- impacts(lag, listw = weighted_neighbors)
    c <- data.frame(coefficientReg = as.numeric(b[3]))
    dfoutput <- rbind(dfoutput,c)
  }
  dfoutput$month <- c("20-04", "20-05", "20-06", "20-07", "20-08", "20-09", 
                      "20-10", "20-11", "20-12", "21-01", "21-02", "21-03", "21-04", "21-05", "21-06")
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  dfoutput
}


# SAR regress a var against a previously defined list of variables, output as df of avg total effects & rho, including controls
SARvarlist2 <- function(var, varList, controlList){
  dfoutput <- data.frame()
  dfoutput2 <- data.frame()
  input2 <- c(var,controlList)
  for(a in varList) {
    inputvar1 <- a
    f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
    lag = lagsarlm(f, data=dfds, listw = weighted_neighbors,
                   tol.solve=1.0e-30, zero.policy=T)
    b <- impacts(lag, listw = weighted_neighbors)
    b <- unlist(b)
    c <- data.frame(coefficientReg = as.numeric(b["total1"]))
    dfoutput <- rbind(dfoutput,c)
    d <- data.frame(rho = as.numeric(summary(lag)$rho))
    dfoutput2 <- rbind(dfoutput2,d)
  }
  dfoutput$rho <- dfoutput2$rho
  dfoutput$month <- month
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  dfoutput
}

# SAR regress a var against a previously defined list of variables, 
# output as df of avg total effects & rho, including controls
# for 2 vars
SARvarlist3 <- function(var1, var2, varList, controlList){
  dfoutput <- data.frame()
  dfoutput1 <- data.frame()
  dfoutput2 <- data.frame()
  dfoutput3 <- data.frame()
  input2 <- c(var1,var2,controlList)
  for(a in varList) {
    inputvar1 <- a
    f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
    lag = lagsarlm(f, data=dfds, listw = weighted_neighbors,
                   tol.solve=1.0e-30, zero.policy=T)
    b <- impacts(lag, listw = weighted_neighbors)
    b <- unlist(b)
    c <- data.frame(coefficientReg1 = as.numeric(b["total1"]))
    d <- data.frame(coefficientReg2 = as.numeric(b["total2"]))
    dfoutput1 <- rbind(dfoutput1,c)
    dfoutput3 <- rbind(dfoutput3,d)
    g <- data.frame(rho = as.numeric(summary(lag)$rho))
    dfoutput2 <- rbind(dfoutput2,g)
  }
  m <- data.frame(month = month)
  dfoutput <- rbind(dfoutput,m)
  dfoutput$coefficientReg <- round(dfoutput1$coefficientReg, digits = 5)
  dfoutput$coefficientReg2 <- round(dfoutput3$coefficientReg2, digits = 5)
  dfoutput$rho <- dfoutput2$rho
  dfoutput
}

# for displaying scales with 4 digits after .+ display two barplots over time
scaledigits <- function(x) sprintf("%.4f", x)

displayRegCor <- function(df,text,textReg,textCor){
  plot1 <- ggplot(df, aes(x=month, y=coefficientReg)) +
    geom_bar(stat="identity",
             colour="black", 
             fill = "white",
             size=.5,
             width=0.5) +
    scale_y_continuous(limits=c(min(df[,"coefficientReg"]),max(df[,"coefficientReg"])),labels=scaledigits)+ 
    scale_x_discrete(limits=df$month)+
    theme_bw()
  plot2 <- ggplot(df, aes(x=month, y=coefficientCor)) +
    geom_bar(stat="identity",
             colour="black", 
             fill = "white",
             size=.5,
             width=0.5) +
    scale_y_continuous(limits=c(-1,1),labels=scaledigits)+ 
    scale_x_discrete(limits=df$month)+
    theme_bw() 
  plot3 <- ggarrange(plot1,plot2,labels = c(textReg,textCor),ncol=1,nrow=2)
  annotate_figure(plot3, top = text_grob(text))
}

# display time and coefficients in barplots
displayCoeff <- function(df, title, yaxis){
  plot1 <- ggplot(df, aes(x=month, y=coefficientReg)) +
    geom_bar(stat="identity",
             colour="black", 
             fill = "white",
             size=.5,
             width=0.5) +
    xlab("Month") +
    ylab(yaxis) +
    scale_y_continuous(limits=c(ifelse(min(df[,"coefficientReg"])<0,min(df[,"coefficientReg"]),0)
                                ,ifelse(max(df[,"coefficientReg"])>0,max(df[,"coefficientReg"]),0)),labels=scaledigits)+ 
    scale_x_discrete(limits=df$month)+
    theme_bw()
  annotate_figure(plot1, top = text_grob(title))
}



