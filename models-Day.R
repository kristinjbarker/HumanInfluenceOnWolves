           ### ### ### ### ### ###  ### ### ### ### ### ### ### ### ### ### ### ### 
           #                                                                      #
           #                          DAYTIME MODELS                              #
           #  FOR ASSESSING HUMAN INFLUENCE ON WOLF DISTRIBUTIONS AND BEHAVIORS   #
           #                                                                      #
           #                      Kristin Barker | Summer 2019                    #
           #                        kristinjbarker@gmail.com                      #
           #                                                                      #
           ### ### ### ### ### ###  ### ### ### ### ### ### ### ### ### ### ### ### 


################################################################################################## #


### ### ### ### ### #
####  | SETUP |  ####
### ### ### ### ### #

  
  #### Set working directory and filepath ####

    wd_kjb <- "C:\\Users\\Kristin\\Box Sync\\Documents\\HumanInfluenceOnWolves"
    wd_greg <- "C:\\Users\\krist\\Documents\\HumanInfluenceOnWolves"
    if (file.exists(wd_kjb)) { setwd(wd_kjb); datName <- "C:\\Users\\Kristin\\Box Sync\\Documents\\Data"
    } else {
      setwd(wd_greg); datName <- "C:\\Users\\krist\\Documents\\Data"
    }
    rm(wd_kjb, wd_greg)


  #### Install and load any necessary packages you don't already have ####
    
    # list of packages needed
    packages <- c(
      "lme4",           ## regression models
      "arm",            ## binned residual plots
      "aods3",          ## quick gof tests
      "pROC",           ## roc plots
      "plotROC",        ## roc plots
      "caret",          ## confusion matrices (model predictive accuracy)
      "AICcmodavg",     ## model comparison
      "ggplot2",        ## totes adorbs plots
      "gridExtra",      ## arrange multiple plots
      "cowplot",        ## samesies
      "lubridate",      ## manipulate datetime data inside dplyr pipes
      "dplyr")          ## data manipulation and general awesomeness
    
    
    # Check whether the packages listed above are installed, 
    # install any you don't already have, then load them all 
    # (code by Steven Worthington: https://gist.github.com/stevenworthington/3178163)   
    ipak <- function(pkg){
      new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
      if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
      sapply(pkg, require, character.only = TRUE)
    }    
    ipak(packages) 
    rm(ipak, packages)

    
  #### Increase memory limit ####
    
    memory.limit(size = 9999999999999) 

    
    
################################################################################################## #  
  
    
    
### ### ### ### ### ### #
####  | DATA PREP |  ####
### ### ### ### ### ### #
 
    
    #### Raw data ####
    
      modDatRaw <- read.csv("modDat.csv")


    #### Formatted data ####    
      
      modDat <- modDatRaw %>% mutate(
        datetime = ymd_hms(datetime, tz = "America/Denver"),
        # handle datetimes and dates of course
        Date = ymd(Date),
        # year as numeric
        Year = as.numeric(Year),
        # order landcover from most to least available
        lcClass = factor(lcClass, levels = c("Forest", "Shrub", "Herbaceous", "Riparian", "NoVeg")))
  
      # only use daytime data for day models
      modDatDay <- filter(modDat, daytime == "day")


      
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### #
####   | ENVIRONMENTAL MODELS |  ####
### ### ### ### ### ### ### ### ### #
    
    
    # from models-Environment.R
        
    envtDay <- glmer(Used ~ 1 + canSt + slopeSt + elevSt + northnessSt + snowSt
                     + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                     + snowSt:canSt + snowSt:northnessSt + snowSt:elevSt + snowSt:I(elevSt*elevSt) 
                     + (1|Pack), family = binomial(logit), data = modDatDay,
                     control = glmerControl(optimizer = "bobyqa", 
                                            optCtrl=list(maxfun=3e4),
                                            calc.derivs = FALSE)) 


                  
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### #### #
#### | LANDSCAPE SHIELD MODELS |  ####
### ### ### ### ### ### ### ### #### #

    #### models ####      

      # roads & buildings, linear
      lsm1 <- update(envtDay, . ~ . + distRdPavSt + distStrucSt)

      # roads & buildings, quadratic
      lsm2 <- update(envtDay, . ~ . + distRdPavSt + distStrucSt + I(distRdPavSt^2) + I(distStrucSt^2))

      # buildings only, linear
      lsm3 <- update(envtDay, . ~ . + distStrucSt)

      # buildings only, quadratic
      lsm4 <- update(envtDay, . ~ . + distStrucSt + I(distStrucSt^2))

  #### competition ####
      
      # create list to store current subset of models in
      modsLSM <- list()
      # list model names
      modNamesLSM <- ls(envir = .GlobalEnv, pattern = "^lsm[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesLSM)) { modsLSM[[i]] <- get(modNamesLSM[i]) }
      # create a dataframe of aicc results...
      aicDayLSM <- data.frame(aictab(cand.set = modsLSM, modnames = modNamesLSM))
      # ...sorted from smallest to largest aicc value...
      aicDayLSM <- aicDayLSM[order(aicDayLSM$Delta_AICc), ]
      # ... and store
      write.csv(aicDayLSM, file = "aicDayLSM.csv", row.names = FALSE)      
      # identify moderately-supported models (deltaAICc < 4)
      aic4DayLSM <- subset(aicDayLSM, Delta_AICc < 4.0)
      aic4DayLSM <- droplevels(aic4DayLSM)
      aic4DayLSM$Modnames <- as.character(aic4DayLSM$Modnames)
      # save top-supported models
      topLSMs <- list()
      # list model names
      topLSMnames <- unique(aic4DayLSM$Modnames)
      # store the models in the list
      for (i in 1:length(topLSMnames)) { topLSMs[[i]] <- get(topLSMnames[i]) }      

      
      
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### # 
####  | DISTURBANCE SHIELD MODELS |  ####
### ### ### ### ### ### ### ### ### ### #
      
      
      #### all human influences ####
      
          # linear, predictable feedgrounds
          dsm5 <- update(envtDay, . ~ . + distRdSt + distStrucSt + activeFeedSt*hunt)
      
          # quadratic, predictable feedgrounds
          dsm6 <- update(envtDay, . ~ . + distRdSt + distStrucSt + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          dsm7 <- update(envtDay, . ~ . + distRdSt + distStrucSt + distFeedSt)
      
          # quadratic, all feedgrounds
          dsm8 <- update(envtDay, . ~ . + distRdSt + distStrucSt + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2))
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          dsm9 <- update(envtDay, . ~ . + distRdSt + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          dsm10 <- update(envtDay, . ~ . + distRdSt + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          dsm11 <- update(envtDay, . ~ . + distRdSt + distFeedSt)
      
          # quadratic, all feedgrounds
          dsm12 <- update(envtDay, . ~ . + distRdSt + distFeedSt + I(distRdSt^2) + I(distFeedSt^2))
                    
      
      
 
      
      
      #### living and working activity, not recreation per se ####      
      
          # linear, predictable feedgrounds
          dsm21 <- update(envtDay, . ~ . + distRdSt + distStrucSt + pvt + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          dsm22 <- update(envtDay, . ~ . + distRdSt + distStrucSt + pvt + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          dsm23 <- update(envtDay, . ~ . + distRdSt + distStrucSt + pvt + distFeedSt)
      
          # quadratic, all feedgrounds
          dsm24 <- update(envtDay, . ~ . + distRdSt + distStrucSt + pvt + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2))
          

            


  #### competition ####
      
      # create list to store current subset of models in
      modsDSM <- list()
      # list model names
      modNamesDSM <- ls(envir = .GlobalEnv, pattern = "^dsm[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesDSM)) { modsDSM[[i]] <- get(modNamesDSM[i]) }
      # create a dataframe of aicc results...
      aicDayDSM <- data.frame(aictab(cand.set = modsDSM, modnames = modNamesDSM))
      # ...sorted from smallest to largest aicc value...
      aicDayDSM <- aicDayDSM[order(aicDayDSM$Delta_AICc), ]
      # ... and store
      write.csv(aicDayDSM, file = "aicDayDSM.csv", row.names = FALSE)      
      # identify moderately-supported models (deltaAICc < 4)
      aic4DayDSM <- subset(aicDayDSM, Delta_AICc < 4.0)
      aic4DayDSM <- droplevels(aic4DayDSM)
      aic4DayDSM$Modnames <- as.character(aic4DayDSM$Modnames)
      # save top-supported models
      topDSMs <- list()
      # list model names
      topDSMnames <- unique(aic4DayDSM$Modnames)
      # store the models in the list
      for (i in 1:length(topDSMnames)) { topDSMs[[i]] <- get(topDSMnames[i]) }      
            
      
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### ### ##
#### | THREAT SHIELD MODELS Y/N |  ####
### ### ### ### ### ### ### ### ### ### ### ##
      
      
      #### all human influences ####
      
          # linear, predictable feedgrounds
          tsa28 <- update(envtDay, . ~ . + distRdSt + distStrucSt + activeFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          tsa29 <- update(envtDay, . ~ . + distRdSt + distStrucSt + activeFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*activeFeedSt
                       + hunt*canSt)          
      
          # quadratic, predictable feedgrounds
          tsa30 <- update(envtDay, . ~ . + distRdSt + distStrucSt + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  hunt*distRdSt +  hunt*distStrucSt +  hunt*activeFeedSt
                       +  hunt*I(distRdSt^2) +  hunt*I(distStrucSt^2) +  hunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          tsa31 <- update(envtDay, . ~ . + distRdSt + distStrucSt + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  hunt*distRdSt +  hunt*distStrucSt +  hunt*activeFeedSt
                       +  hunt*I(distRdSt^2) +  hunt*I(distStrucSt^2) +  hunt*I(activeFeedSt^2)
                       + hunt*canSt)
      
          # linear, all feedgrounds
          tsa32 <- update(envtDay, . ~ . + distRdSt + distStrucSt + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          tsa33 <- update(envtDay, . ~ . + distRdSt + distStrucSt + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*distFeedSt
                       + hunt*canSt)          
      
          # quadratic, all feedgrounds
          tsa34 <- update(envtDay, . ~ . + distRdSt + distStrucSt + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          tsa35 <- update(envtDay, . ~ . + distRdSt + distStrucSt + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2)
                       + hunt*canSt)          
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          tsa36 <- update(envtDay, . ~ . + distRdSt + activeFeedSt
                       + hunt*distRdSt + hunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          tsa37 <- update(envtDay, . ~ . + distRdSt + activeFeedSt
                       + hunt*distRdSt + hunt*activeFeedSt + hunt*canSt)          
          
      
          # quadratic, predictable feedgrounds
          tsa38 <- update(envtDay, . ~ . + distRdSt + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*activeFeedSt + hunt*I(distRdSt^2) + hunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          tsa39 <- update(envtDay, . ~ . + distRdSt + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*activeFeedSt + hunt*I(distRdSt^2) + hunt*I(activeFeedSt^2)
                       + hunt*canSt)
      
          # linear, all feedgrounds
          tsa40 <- update(envtDay, . ~ . + distRdSt + distFeedSt
                       + hunt*distRdSt + hunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          tsa41 <- update(envtDay, . ~ . + distRdSt + distFeedSt
                       + hunt*distRdSt + hunt*distFeedSt + hunt*canSt)          
      
          # quadratic, all feedgrounds
          tsa42 <- update(envtDay, . ~ . + distRdSt + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          tsa43 <- update(envtDay, . ~ . + distRdSt + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2)
                       + hunt*canSt)
                    

          
    #### change use of landscape but not response to people ####       
          
          # all environment
          tsa66 <- update(envtDay, . ~ . + hunt*canSt + hunt*slopeSt + hunt*elevSt + hunt*northnessSt
                        + hunt*snowSt)

          
          # canopy (i.e., hiding cover/escape terrain) only
          tsa67 <- update(envtDay, . ~ . + hunt*canSt)

          
  #### competition ####
      
      # create list to store current subset of models in
      modsTSA <- list()
      # list model names
      modNamesTSA <- ls(envir = .GlobalEnv, pattern = "^tsa[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesTSA)) { modsTSA[[i]] <- get(modNamesTSA[i]) }
      # create a dataframe of aicc results...
      aicDayTSA <- data.frame(aictab(cand.set = modsTSA, modnames = modNamesTSA))
      # ...sorted from smallest to largest aicc value...
      aicDayTSA <- aicDayTSA[order(aicDayTSA$Delta_AICc), ]
      # ... and store
      write.csv(aicDayTSA, file = "aicDayTSA.csv", row.names = FALSE)      
      # identify moderately-supported models (deltaAICc < 4)
      aic4DayTSA <- subset(aicDayTSA, Delta_AICc < 4.0)
      aic4DayTSA <- droplevels(aic4DayTSA)
      aic4DayTSA$Modnames <- as.character(aic4DayTSA$Modnames)
      # save top-supported models
      topTSAs <- list()
      # list model names
      topTSAnames <- unique(aic4DayTSA$Modnames)
      # store the models in the list
      for (i in 1:length(topTSAnames)) { topTSAs[[i]] <- get(topTSAnames[i]) }      
      
################################################################################################## #  
  
    
### ### ### ### ### ###  ### ### ### ### ### ### ### ### ###
#### | THREAT SHIELD MODELS previous year Y/N |  ####
### ### ### ### ### ###  ### ### ### ### ### ### ### ### ###
          
          
      
      #### all human influences ####
      
          # linear, predictable feedgrounds
          tsb68 <- update(envtDay, . ~ . + distRdSt + distStrucSt + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          tsb69 <- update(envtDay, . ~ . + distRdSt + distStrucSt + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*activeFeedSt
                       + prevHunt*canSt)
          
          # quadratic, predictable feedgrounds
          tsb70 <- update(envtDay, . ~ . + distRdSt + distStrucSt + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  prevHunt*distRdSt +  prevHunt*distStrucSt +  prevHunt*activeFeedSt
                       +  prevHunt*I(distRdSt^2) +  prevHunt*I(distStrucSt^2) +  prevHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          tsb71 <- update(envtDay, . ~ . + distRdSt + distStrucSt + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  prevHunt*distRdSt +  prevHunt*distStrucSt +  prevHunt*activeFeedSt
                       +  prevHunt*I(distRdSt^2) +  prevHunt*I(distStrucSt^2) +  prevHunt*I(activeFeedSt^2)
                       + prevHunt*canSt)
          
          # linear, all feedgrounds
          tsb72 <- update(envtDay, . ~ . + distRdSt + distStrucSt + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          tsb73 <- update(envtDay, . ~ . + distRdSt + distStrucSt + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*distFeedSt
                       + prevHunt*canSt)          
      
          # quadratic, all feedgrounds
          tsb74 <- update(envtDay, . ~ . + distRdSt + distStrucSt + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*distFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          tsb75 <- update(envtDay, . ~ . + distRdSt + distStrucSt + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*distFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2)
                       + prevHunt*canSt)          
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          tsb76 <- update(envtDay, . ~ . + distRdSt + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          tsb77 <- update(envtDay, . ~ . + distRdSt + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*activeFeedSt + prevHunt*canSt)          
          

          # quadratic, predictable feedgrounds
          tsb78 <- update(envtDay, . ~ . + distRdSt + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) 
                       + prevHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          tsb79 <- update(envtDay, . ~ . + distRdSt + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) 
                       + prevHunt*I(activeFeedSt^2)
                       + prevHunt*canSt)
      
          # linear, all feedgrounds
          tsb80 <- update(envtDay, . ~ . + distRdSt + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          tsb81 <- update(envtDay, . ~ . + distRdSt + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distFeedSt + prevHunt*canSt)          
      
          # quadratic, all feedgrounds
          tsb82 <- update(envtDay, . ~ . + distRdSt + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) 
                       + prevHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          tsb83 <- update(envtDay, . ~ . + distRdSt + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) 
                       + prevHunt*I(distFeedSt^2)
                       + prevHunt*canSt)
                    
      
      

          
    #### change use of landscape but not response to people ####       
          
          # all environment
          tsb106 <- update(envtDay, . ~ . + prevHunt*canSt + prevHunt*slopeSt + prevHunt*elevSt + prevHunt*northnessSt
                         + prevHunt*snowSt)
          
          
          # canopy (i.e., hiding cover/escape terrain) only
          tsb107 <- update(envtDay, . ~ . + prevHunt*canSt)          

          
          

  #### competition ####
      
      # create list to store current subset of models in
      modsTSB <- list()
      # list model names
      modNamesTSB <- ls(envir = .GlobalEnv, pattern = "^tsb[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesTSB)) { modsTSB[[i]] <- get(modNamesTSB[i]) }
      # create a dataframe of aicc results...
      aicDayTSB <- data.frame(aictab(cand.set = modsTSB, modnames = modNamesTSB))
      # ...sorted from smallest to largest aicc value...
      aicDayTSB <- aicDayTSB[order(aicDayTSB$Delta_AICc), ]
      # ... and store
      write.csv(aicDayTSB, file = "aicDayTSB.csv", row.names = FALSE)      
      # identify moderately-supported models (deltaAICc < 4)
      aic4DayTSB <- subset(aicDayTSB, Delta_AICc < 4.0)
      aic4DayTSB <- droplevels(aic4DayTSB)
      aic4DayTSB$Modnames <- as.character(aic4DayTSB$Modnames)
      # save top-supported models
      topTSBs <- list()
      # list model names
      topTSBnames <- unique(aic4DayTSB$Modnames)
      # store the models in the list
      for (i in 1:length(topTSBnames)) { topTSBs[[i]] <- get(topTSBnames[i]) }      
      
                
    
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ##
#### | RESOURCE SUBSIDY MODELS |  ####
### ### ### ### ### ### ### ### ### ##    
      
      # all feedgrounds, linear
      rsm108 <- update(envtDay, . ~ . + distFeedSt)
      
      # all feedgrounds, quadratic
      rsm109 <- update(envtDay, . ~ . + distFeedSt + I(distFeedSt^2))
      
      # active feedgrounds, linear
      rsm110 <- update(envtDay, . ~ . + activeFeedSt)
      
      # active feedgrounds, quadratic
      rsm111 <- update(envtDay, . ~ . + activeFeedSt + I(activeFeedSt^2))
      
      

  #### competition ####
      
      # create list to store current subset of models in
      modsRSM <- list()
      # list model names
      modNamesRSM <- ls(envir = .GlobalEnv, pattern = "^rsm[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesRSM)) { modsRSM[[i]] <- get(modNamesRSM[i]) }
      # create a dataframe of aicc results...
      aicDayRSM <- data.frame(aictab(cand.set = modsRSM, modnames = modNamesRSM))
      # ...sorted from smallest to largest aicc value...
      aicDayRSM <- aicDayRSM[order(aicDayRSM$Delta_AICc), ]
      # ... and store
      write.csv(aicDayRSM, file = "aicDayRSM.csv", row.names = FALSE)      
      # identify moderately-supported models (deltaAICc < 4)
      aic4DayRSM <- subset(aicDayRSM, Delta_AICc < 4.0)
      aic4DayRSM <- droplevels(aic4DayRSM)
      aic4DayRSM$Modnames <- as.character(aic4DayRSM$Modnames)
      # save top-supported models
      topRSMs <- list()
      # list model names
      topRSMnames <- unique(aic4DayRSM$Modnames)
      # store the models in the list
      for (i in 1:length(topRSMnames)) { topRSMs[[i]] <- get(topRSMnames[i]) }         
         
      
################################################################################################## #  

    
### ### ### ### ### ### ### ### 
#### | MODEL COMPARISON |  ####
### ### ### ### ### ### ### ###      
      

      # list all models and their names
      allMods <- c(modsLSM, modsDSM, modsTSA, modsTSB, modsRSM)
      allModNames <- c(modNamesLSM, modNamesDSM, modNamesTSA, modNamesTSB, modNamesRSM)
      
      # compete all with aicc; export results
      aicDay <- data.frame(aictab(cand.set = allMods, modnames = allModNames))
      aicDay <- aicDay[order(aicDay$Delta_AICc), ]
      write.csv(aicDay, file = "aicAllDay.csv", row.names = FALSE)    
      
      # identify moderately-supported models (deltaAICc < 4); export results
      aic4Day <- subset(aicDay, Delta_AICc < 4.0)
      aic4Day <- droplevels(aic4Day)
      write.csv(aic4Day, file = "aicModerateDay.csv", row.names = FALSE)           
      
      # identify best-supported models (deltaAICc < 2); export results
      aic2Day <- subset(aicDay, Delta_AICc < 2.0)
      aic2Day <- droplevels(aic2Day)
      write.csv(aic2Day, file = "aicTopDay.csv", row.names = FALSE)     

      
      
################################################################################################## #  

 
    
### ### ### ### ### ### ### ### ###
#### | TOP MODEL DIAGNOSTICS|  ####
### ### ### ### ### ### ### ### ###   
     
      ## top model
      
      topDay <- get(as.character(aic2Day[1,"Modnames"]))
    
      ## area under the roc curve ##
      invisible(plot(roc(factor(ifelse(modDatDay$Used == 1, 1, 0)), fitted(topDay)), 
                     print.thres = c(.1, .5), col = "red", print.auc = T)) # auc = 0.712
      
      ## predictive accuracy @ >50% ##  
      confusionMatrix(factor(as.character(ifelse(fitted(topDay) > 0.5, "Yes", "No"))), 
                      factor(ifelse(modDatDay$Used == 1, "Yes", "No")), positive = "Yes") # 66.45
    
      
      ## binned residual plots ##
      binnedplot(fitted(topDay), residuals(topDay, type = "response"), main = "Day - top model")

      
      ## other model
 
      midDay <- get(as.character(aic2Day[2,"Modnames"]))
    
      ## area under the roc curve ##
      invisible(plot(roc(factor(ifelse(modDatDay$Used == 1, 1, 0)), fitted(midDay)), 
                     print.thres = c(.1, .5), col = "red", print.auc = T)) # auc = 0.712
      
      ## predictive accuracy @ >50% ##  
      confusionMatrix(factor(as.character(ifelse(fitted(midDay) > 0.5, "Yes", "No"))), 
                      factor(ifelse(modDatDay$Used == 1, "Yes", "No")), positive = "Yes") # 66.45%
    
      
      ## binned residual plots ##
      binnedplot(fitted(midDay), residuals(midDay, type = "response"), main = "Day - 2nd model")
      
       
################################################################################################## #  

################################################################################################## #  
             
################################################################################################## #  
save.image(paste0("modelsHumanDay", today(), ".RData"))    
   
################################################################################################## #  
       
################################################################################################## #  

################################################################################################## #  
             
      

      
      
### ### ### ### ### ### ### ### ###
#### | OLDER CODE TO CUT/REAPPROPRIATE|  ####
### ### ### ### ### ### ### ### ###   
               
      
      
          # extract estimates from model summary
          topDayEsts <- data.frame(
            Covariate = rownames(coef(summary(topModDay))),
            Estimate = round(coef(summary(topModDay))[, "Estimate"], 3),
            stdError = round(coef(summary(topModDay))[, "z value"], 3),
            OR = round(exp(coef(summary(topModDay))[, "Estimate"]), 3),
            pVal = coef(summary(topModDay))[, "Pr(>|z|)"])
          topDayEsts$sig <- ifelse(topDayEsts$pVal <= 0.05, 1, 0)
          
          # estimate CIs separately bc not positive they map to correct covariate otherwise
          topDayCIs <- data.frame(
            ciLow = round(confint(topModDay, parm = "beta_", method = "Wald")[,1], 3),
            ciHigh = round(confint(topModDay, parm = "beta_", method = "Wald")[,2], 3))
          topDayCIs$Covariate <- rownames(topDayCIs)
          
          # combine estimates and CIs
          topDay <- left_join(topDayEsts, topDayCIs, by = "Covariate")
    
          # export
          write.csv(topDay, "topModDay.csv", row.names = F)
              
          
      
################################################################################################## #  

      
      save.image("modelsHumanDay.RData") 
      
      
          
################################################################################################## # 
      
    
### ### ### ### ### ### ### ### ##
#### | VISUALIZING RESULTS |  ####
### ### ### ### ### ### ### ### ##  
      
      
      
      #### create dataframe of things to plot ####
        
        d <- modDatDay %>%
          mutate(
            prWolf = fitted(topModDay),
            model = "Day",
            tSinceHunt = as.integer(tSinceHunt))

      
      
      
      #### quadratic interactions ####  
        
        # feedgrounds (previous year hunting y/n)
        pFeed <- ggplot(d, aes(x = distFeed, y = prWolf, colour = prevHunt)) +
          stat_smooth(aes(linetype = prevHunt), method = "lm", formula = y ~ poly(x, 2)) +
          # coord_cartesian(ylim = c(0, 1)) +
          labs(title = "Distance to Feedgrounds")
        
        # canopy (previous year hunting y/n)
        pCan <- ggplot(d, aes(x = can, y = prWolf, colour = prevHunt)) +
          stat_smooth(aes(linetype = prevHunt), method = "lm", formula = y ~ poly(x, 2)) +
          # coord_cartesian(ylim = c(0, 1)) +
          labs(title = "Canopy cover")
        

      
################################################################################################## # 
      
    
