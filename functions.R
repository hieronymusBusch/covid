##########################################################
#                                                        #
#                                                        #
# 2021/10                                                #
#                                                        #
# Alexander Busch (a.busch@lse.ac.uk)                    #
# Functions for Data Analysis                            #
#                                                        #
##########################################################

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
# additionally, model as output
OLSvarlistM <- function(var, varList, controlList){
  dfoutput <- data.frame()
  input2 <- c(var,controlList)
  modellist <- list()
  for(a in varList) {
    inputvar1 <- a
    f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
    regmodel <- lm(f,dfdd,na.action=na.omit)
    b <- summary(regmodel)$coefficients
    c <- data.frame(coefficientReg = b[2, 1])
    dfoutput <- rbind(dfoutput,c)
    len <- length(modellist)
    modellist[[len+1]] <- regmodel
  }
  dfoutput$month <- month
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  names(modellist) <- varList
  listoutput <- c(dfoutput, modellist)
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

# SAR regress as above, but with models in output
SARvarlistM <- function(var, varList, controlList){
  dfoutput <- data.frame()
  input2 <- c(var,controlList)
  modellist <- list()
  for(a in varList) {
    inputvar1 <- a
    lag <- lagsarlm(as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~")), data=dfds, listw = weighted_neighbors,
                    tol.solve=1.0e-30, zero.policy=T)
    b <- impacts(lag, listw = weighted_neighbors)
    b <- unlist(b)
    c <- data.frame(coefficientReg = as.numeric(b["total1"]))
    dfoutput <- rbind(dfoutput,c)
    len <- length(modellist)
    modellist[[len+1]] <- lag
  }
  dfoutput$month <- month
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  names(modellist) <- varList
  listoutput <- c(dfoutput, modellist)
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



