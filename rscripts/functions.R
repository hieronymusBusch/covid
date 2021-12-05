##########################################################
#                                                        #
#                                                        #
# 2021/10                                                #
#                                                        #
# Alexander Busch (a.busch@lse.ac.uk)                    #
# Functions for Data Analysis                            #
#                                                        #
##########################################################

# Function: estimates for each month a new model and saves coefficients for indvar
# General function for both linear OLS and SAR models: 
  # invar: independent variable which has its coefficients saved
  # depvarList: list of dependent variables (here: IR monthly)
  # controlList: list of controls 
  # model: either SAR or OLS
  # -> output as df of regression coefficients! / ATE

regvarlist <- function(indvar, depvarList, controlList, month, model, df){
  dfoutput <- data.frame()
  input2 <- c(indvar,controlList)
  if(model=="SAR"){
    for(a in 1:length(month)){
      inputvar1 <- depvarList[a]
      f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
      lag = lagsarlm(f, data=df, listw = weighted_neighbors,
                     tol.solve=1.0e-30, zero.policy=T)
      b <- impacts(lag, listw = weighted_neighbors)
      b <- unlist(b)
      c <- data.frame(coefficientReg = as.numeric(b["total1"]))
      dfoutput <- rbind(dfoutput,c)
    }
  }
  if(model=="SAC"){
    for(a in 1:length(month)){
      inputvar1 <- depvarList[a]
      f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
      lag = sacsarlm(f, data=df, listw = weighted_neighbors,
                     tol.solve=1.0e-30, zero.policy=T)
      b <- impacts(lag, listw = weighted_neighbors)
      b <- unlist(b)
      c <- data.frame(coefficientReg = as.numeric(b["total1"]))
      dfoutput <- rbind(dfoutput,c)
    }
  }
  if(model=="OLS"){
    for(a in 1:length(month)) {
      inputvar1 <- depvarList[a]
      f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
      regmodel <- lm(f,df,na.action=na.omit)
      b <- summary(regmodel)$coefficients
      c <- data.frame(coefficientReg = b[2, 1])
      dfoutput <- rbind(dfoutput,c)
    }
  }
  dfoutput$month <- month
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  dfoutput
}

# same function as above, but output is a list of coefficients AND the full models
# General functions for both linear OLS and SAR models: 
  # var: independent variable
  # varList: list of dependent variables (here: IR monthly)
  # controlList: list of controls 
  # model: either SAR or OLS
regvarlistM <- function(var, depvarList, controlList, month, model, df){
  dfoutput <- data.frame()
  input2 <- c(var,controlList)
  modellist <- list()
  if(model=="OLS"){ 
         for(a in 1:length(month)) {
           inputvar1 <- depvarList[a]
           f <- as.formula(paste(inputvar1,paste(input2, collapse = " + "), sep = "~"))
           regmodel <- lm(f,df,na.action=na.omit)
           b <- summary(regmodel)$coefficients
           c <- data.frame(coefficientReg = b[2, 1])
           dfoutput <- rbind(dfoutput,c)
           len <- length(modellist)
           modellist[[len+1]] <- regmodel
         }
  }
  if(model=="SAR"){
          for(a in 1:length(month)) {
            inputvar1 <- depvarList[a]
            lag <- lagsarlm(as.formula(paste(inputvar1,paste(input2, collapse = " + "), 
                                             sep = "~")), data=df, listw = weighted_neighbors,
                            tol.solve=1.0e-30, zero.policy=T)
            b <- impacts(lag, listw = weighted_neighbors)
            b <- unlist(b)
            c <- data.frame(coefficientReg = as.numeric(b["total1"]))
            dfoutput <- rbind(dfoutput,c)
            len <- length(modellist)
            modellist[[len+1]] <- lag
          }
  }
  if(model=="SAC"){
    for(a in 1:length(month)) {
      inputvar1 <- depvarList[a]
      lag <- sacsarlm(as.formula(paste(inputvar1,paste(input2, collapse = " + "), 
                                       sep = "~")), data=df, listw = weighted_neighbors,
                      tol.solve=1.0e-30, zero.policy=T)
      b <- impacts(lag, listw = weighted_neighbors)
      b <- unlist(b)
      c <- data.frame(coefficientReg = as.numeric(b["total1"]))
      dfoutput <- rbind(dfoutput,c)
      len <- length(modellist)
      modellist[[len+1]] <- lag
    }
  }
  dfoutput$month <- month
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  names(modellist) <- depvarList
  listoutput <- c(dfoutput, modellist)
  listoutput
}

# Function: Models with changeing dependnet var AND a changeing independent
  # var (here: cases of previous month) against a predefined set of controls (indvars)
# Controls for path dependency (previous var in the varList)
  # indvar: independent variable of which coefficients are saved
  # varList: list of dependent variables (here: IR monthly)
  # controlList: list of controls
  # indvarList: list of controls, each entry only for one model (here, previous IR)
  # model: either SAR or OLS
  # -> output as df of regression coefficients / ATE
  # month2 doesnt contain february, as no previous month to february to control for
regvarlistP <- function(indvar, depvarList, controlList, indvarList, month, model, df){
  dfoutput <- data.frame()
  inputvar2 <- c(indvar,controlList)
  if(model=="SAR"){
    for(a in 1:length(month)){
      inputvar1 <- depvarList[a]
      inputvar2 <- c(inputvar2, indvarList[a])
      f <- as.formula(paste(inputvar1,paste(inputvar2, collapse = " + "), sep = "~"))
      lag = lagsarlm(f, data=df, listw = weighted_neighbors,
                     tol.solve=1.0e-30, zero.policy=T)
      b <- impacts(lag, listw = weighted_neighbors)
      b <- unlist(b)
      c <- data.frame(coefficientReg = as.numeric(b["total1"]))
      dfoutput <- rbind(dfoutput,c)
    }
  }
  if(model=="SAC"){
    for(a in 1:length(month)){
      inputvar1 <- depvarList[a]
      inputvar2 <- c(inputvar2, indvarList[a])
      f <- as.formula(paste(inputvar1,paste(inputvar2, collapse = " + "), sep = "~"))
      lag = sacsarlm(f, data=df, listw = weighted_neighbors,
                     tol.solve=1.0e-30, zero.policy=T)
      b <- impacts(lag, listw = weighted_neighbors)
      b <- unlist(b)
      c <- data.frame(coefficientReg = as.numeric(b["total1"]))
      dfoutput <- rbind(dfoutput,c)
    }
  }
  if(model=="OLS"){
    for(a in 1:length(month)) {
      inputvar1 <- depvarList[a]
      inputvar2 <- c(inputvar2, indvarList[a])
      f <- as.formula(paste(inputvar1,paste(inputvar2, collapse = " + "), sep = "~"))
      regmodel <- lm(f,df,na.action=na.omit)
      b <- summary(regmodel)$coefficients
      c <- data.frame(coefficientReg = b[2, 1])
      dfoutput <- rbind(dfoutput,c)
    }
  }
  dfoutput$month <- month
  dfoutput$coefficientReg <- round(dfoutput$coefficientReg, digits = 10)
  dfoutput
}


