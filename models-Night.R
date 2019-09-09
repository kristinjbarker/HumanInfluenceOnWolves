           ### ### ### ### ### ###  ### ### ### ### ### ### ### ### ### ### ### ### 
           #                                                                      #
           #                           NIGHTTIME MODELS                           #
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
  
      # only use nighttime data for night models
      modDatNight <- filter(modDat, daytime == "night")


      
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### #
####   | ENVIRONMENTAL MODELS |  ####
### ### ### ### ### ### ### ### ### #
    
    
    # from models-Environment.R
      
    envtNight <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                       + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                       + snowSt:canSt + snowSt:elevSt + snowSt:I(elevSt*elevSt) 
                       + (1|Pack), family = binomial(logit), data = modDatNight,
                       control = glmerControl(optimizer = "bobyqa", 
                                            optCtrl=list(maxfun=3e4),
                                            calc.derivs = FALSE))       

               
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### #### #
#### | LANDSCAPE SHIELD MODELS |  ####
### ### ### ### ### ### ### ### #### #

    #### models ####      

      # roads & buildings, linear
      lsm1 <- update(envtNight, . ~ . + distRdPavSt + distStrucSt)

      # roads & buildings, quadratic
      lsm2 <- update(envtNight, . ~ . + distRdPavSt + distStrucSt + I(distRdPavSt^2) + I(distStrucSt^2))

      # buildings only, linear
      lsm3 <- update(envtNight, . ~ . + distStrucSt)

      # buildings only, quadratic
      lsm4 <- update(envtNight, . ~ . + distStrucSt + I(distStrucSt^2))

  #### competition ####
      
      # create list to store current subset of models in
      modsLSM <- list()
      # list model names
      modNamesLSM <- ls(envir = .GlobalEnv, pattern = "^lsm[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesLSM)) { modsLSM[[i]] <- get(modNamesLSM[i]) }
      # create a dataframe of aicc results...
      aicNightLSM <- data.frame(aictab(cand.set = modsLSM, modnames = modNamesLSM))
      # ...sorted from smallest to largest aicc value...
      aicNightLSM <- aicNightLSM[order(aicNightLSM$Delta_AICc), ]
      # ... and store
      write.csv(aicNightLSM, file = "aicNightLSM.csv", row.names = FALSE)      
      # identify moderately-supported models (deltaAICc < 4)
      aic4NightLSM <- subset(aicNightLSM, Delta_AICc < 4.0)
      aic4NightLSM <- droplevels(aic4NightLSM)
      aic4NightLSM$Modnames <- as.character(aic4NightLSM$Modnames)
      # save top-supported models
      topLSMs <- list()
      # list model names
      topLSMnames <- unique(aic4NightLSM$Modnames)
      # store the models in the list
      for (i in 1:length(topLSMnames)) { topLSMs[[i]] <- get(topLSMnames[i]) }      

      
      
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### # 
####  | DISTURBANCE SHIELD MODELS |  ####
### ### ### ### ### ### ### ### ### ### #
      
      
      #### all human influences ####
      
          # linear, predictable feedgrounds
          dsm5 <- update(envtNight, . ~ . + distRdSt + distStrucSt + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          dsm6 <- update(envtNight, . ~ . + distRdSt + distStrucSt + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          dsm7 <- update(envtNight, . ~ . + distRdSt + distStrucSt + distFeedSt)
      
          # quadratic, all feedgrounds
          dsm8 <- update(envtNight, . ~ . + distRdSt + distStrucSt + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2))
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          dsm9 <- update(envtNight, . ~ . + distRdSt + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          dsm10 <- update(envtNight, . ~ . + distRdSt + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          dsm11 <- update(envtNight, . ~ . + distRdSt + distFeedSt)
      
          # quadratic, all feedgrounds
          dsm12 <- update(envtNight, . ~ . + distRdSt + distFeedSt + I(distRdSt^2) + I(distFeedSt^2))
                    
      
      
 
      
      
      #### living and working activity, not recreation per se ####      
      
          # linear, predictable feedgrounds
          dsm21 <- update(envtNight, . ~ . + distRdSt + distStrucSt + pvt + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          dsm22 <- update(envtNight, . ~ . + distRdSt + distStrucSt + pvt + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          dsm23 <- update(envtNight, . ~ . + distRdSt + distStrucSt + pvt + distFeedSt)
      
          # quadratic, all feedgrounds
          dsm24 <- update(envtNight, . ~ . + distRdSt + distStrucSt + pvt + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2))
          
          
  


  #### competition ####
      
      # create list to store current subset of models in
      modsDSM <- list()
      # list model names
      modNamesDSM <- ls(envir = .GlobalEnv, pattern = "^dsm[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesDSM)) { modsDSM[[i]] <- get(modNamesDSM[i]) }
      # create a dataframe of aicc results...
      aicNightDSM <- data.frame(aictab(cand.set = modsDSM, modnames = modNamesDSM))
      # ...sorted from smallest to largest aicc value...
      aicNightDSM <- aicNightDSM[order(aicNightDSM$Delta_AICc), ]
      # ... and store
      write.csv(aicNightDSM, file = "aicNightDSM.csv", row.names = FALSE)      
      # identify moderately-supported models (deltaAICc < 4)
      aic4NightDSM <- subset(aicNightDSM, Delta_AICc < 4.0)
      aic4NightDSM <- droplevels(aic4NightDSM)
      aic4NightDSM$Modnames <- as.character(aic4NightDSM$Modnames)
      # save top-supported models
      topDSMs <- list()
      # list model names
      topDSMnames <- unique(aic4NightDSM$Modnames)
      # store the models in the list
      for (i in 1:length(topDSMnames)) { topDSMs[[i]] <- get(topDSMnames[i]) }      
            
      
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### ### ##
#### | THREAT SHIELD MODELS Y/N |  ####
### ### ### ### ### ### ### ### ### ### ### ##
      
      
      #### all human influences ####
      
          # linear, predictable feedgrounds
          tsa28 <- update(envtNight, . ~ . + distRdSt + distStrucSt + activeFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          tsa29 <- update(envtNight, . ~ . + distRdSt + distStrucSt + activeFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*activeFeedSt
                       + hunt*canSt)          
      
          # quadratic, predictable feedgrounds
          tsa30 <- update(envtNight, . ~ . + distRdSt + distStrucSt +  activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  hunt*distRdSt +  hunt*distStrucSt +  hunt*activeFeedSt
                       +  hunt*I(distRdSt^2) +  hunt*I(distStrucSt^2) +  hunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          tsa31 <- update(envtNight, . ~ . + distRdSt + distStrucSt + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  hunt*distRdSt +  hunt*distStrucSt +  hunt*activeFeedSt
                       +  hunt*I(distRdSt^2) +  hunt*I(distStrucSt^2) +  hunt*I(activeFeedSt^2)
                       + hunt*canSt)
      
          # linear, all feedgrounds
          tsa32 <- update(envtNight, . ~ . + distRdSt + distStrucSt + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          tsa33 <- update(envtNight, . ~ . + distRdSt + distStrucSt + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*distFeedSt
                       + hunt*canSt)          
      
          # quadratic, all feedgrounds
          tsa34 <- update(envtNight, . ~ . + distRdSt + distStrucSt + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          tsa35 <- update(envtNight, . ~ . + distRdSt + distStrucSt + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2)
                       + hunt*canSt)          
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          tsa36 <- update(envtNight, . ~ . + distRdSt + activeFeedSt
                       + hunt*distRdSt + hunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          tsa37 <- update(envtNight, . ~ . + distRdSt + activeFeedSt
                       + hunt*distRdSt + hunt*activeFeedSt + hunt*canSt)          
          
      
          # quadratic, predictable feedgrounds
          tsa38 <- update(envtNight, . ~ . + distRdSt + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*activeFeedSt + hunt*I(distRdSt^2) + hunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          tsa39 <- update(envtNight, . ~ . + distRdSt + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*activeFeedSt + hunt*I(distRdSt^2) + hunt*I(activeFeedSt^2)
                       + hunt*canSt)
      
          # linear, all feedgrounds
          tsa40 <- update(envtNight, . ~ . + distRdSt + distFeedSt
                       + hunt*distRdSt + hunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          tsa41 <- update(envtNight, . ~ . + distRdSt + distFeedSt
                       + hunt*distRdSt + hunt*distFeedSt + hunt*canSt)          
      
          # quadratic, all feedgrounds
          tsa42 <- update(envtNight, . ~ . + distRdSt + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          tsa43 <- update(envtNight, . ~ . + distRdSt + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2)
                       + hunt*canSt)
                    
      
      
 
       
          
    #### change use of landscape but not response to people ####       
          
          # all environment
          tsa66 <- update(envtNight, . ~ . + hunt*canSt + hunt*slopeSt + hunt*elevSt + hunt*northnessSt
                        + hunt*snowSt)

          
          # canopy (i.e., hiding cover/escape terrain) only
          tsa67 <- update(envtNight, . ~ . + hunt*canSt)

          
  #### competition ####
      
      # create list to store current subset of models in
      modsTSA <- list()
      # list model names
      modNamesTSA <- ls(envir = .GlobalEnv, pattern = "^tsa[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesTSA)) { modsTSA[[i]] <- get(modNamesTSA[i]) }
      # create a dataframe of aicc results...
      aicNightTSA <- data.frame(aictab(cand.set = modsTSA, modnames = modNamesTSA))
      # ...sorted from smallest to largest aicc value...
      aicNightTSA <- aicNightTSA[order(aicNightTSA$Delta_AICc), ]
      # ... and store
      write.csv(aicNightTSA, file = "aicNightTSA.csv", row.names = FALSE)      
      # identify moderately-supported models (deltaAICc < 4)
      aic4NightTSA <- subset(aicNightTSA, Delta_AICc < 4.0)
      aic4NightTSA <- droplevels(aic4NightTSA)
      aic4NightTSA$Modnames <- as.character(aic4NightTSA$Modnames)
      # save top-supported models
      topTSAs <- list()
      # list model names
      topTSAnames <- unique(aic4NightTSA$Modnames)
      # store the models in the list
      for (i in 1:length(topTSAnames)) { topTSAs[[i]] <- get(topTSAnames[i]) }      
      
################################################################################################## #  
  
    
### ### ### ### ### ###  ### ### ### ### ### ### ### ### ###
#### | THREAT SHIELD MODELS previous year Y/N |  ####
### ### ### ### ### ###  ### ### ### ### ### ### ### ### ###
          
          
      
      #### all human influences ####
      
          # linear, predictable feedgrounds
          tsb68 <- update(envtNight, . ~ . + distRdSt + distStrucSt + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          tsb69 <- update(envtNight, . ~ . + distRdSt + distStrucSt + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*activeFeedSt
                       + prevHunt*canSt)
          
          # quadratic, predictable feedgrounds
          tsb70 <- update(envtNight, . ~ . + distRdSt + distStrucSt + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  prevHunt*distRdSt +  prevHunt*distStrucSt +  prevHunt*activeFeedSt
                       +  prevHunt*I(distRdSt^2) +  prevHunt*I(distStrucSt^2) +  prevHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          tsb71 <- update(envtNight, . ~ . + distRdSt + distStrucSt + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  prevHunt*distRdSt +  prevHunt*distStrucSt +  prevHunt*activeFeedSt
                       +  prevHunt*I(distRdSt^2) +  prevHunt*I(distStrucSt^2) +  prevHunt*I(activeFeedSt^2)
                       + prevHunt*canSt)
          
          # linear, all feedgrounds
          tsb72 <- update(envtNight, . ~ . + distRdSt + distStrucSt + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          tsb73 <- update(envtNight, . ~ . + distRdSt + distStrucSt + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*distFeedSt
                       + prevHunt*canSt)          
      
          # quadratic, all feedgrounds
          tsb74 <- update(envtNight, . ~ . + distRdSt + distStrucSt + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*distFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          tsb75 <- update(envtNight, . ~ . + distRdSt + distStrucSt + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*distFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2)
                       + prevHunt*canSt)          
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          tsb76 <- update(envtNight, . ~ . + distRdSt + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          tsb77 <- update(envtNight, . ~ . + distRdSt + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*activeFeedSt + prevHunt*canSt)          
          

          # quadratic, predictable feedgrounds
          tsb78 <- update(envtNight, . ~ . + distRdSt + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) 
                       + prevHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          tsb79 <- update(envtNight, . ~ . + distRdSt + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) 
                       + prevHunt*I(activeFeedSt^2)
                       + prevHunt*canSt)
      
          # linear, all feedgrounds
          tsb80 <- update(envtNight, . ~ . + distRdSt + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          tsb81 <- update(envtNight, . ~ . + distRdSt + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distFeedSt + prevHunt*canSt)          
      
          # quadratic, all feedgrounds
          tsb82 <- update(envtNight, . ~ . + distRdSt + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) 
                       + prevHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          tsb83 <- update(envtNight, . ~ . + distRdSt + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) 
                       + prevHunt*I(distFeedSt^2)
                       + prevHunt*canSt)
                    
      
      
  
    #### change use of landscape but not response to people ####       
          
          # all environment
          tsb106 <- update(envtNight, . ~ . + prevHunt*canSt + prevHunt*slopeSt + prevHunt*elevSt + prevHunt*northnessSt
                         + prevHunt*snowSt)
          
          
          # canopy (i.e., hiding cover/escape terrain) only
          tsb107 <- update(envtNight, . ~ . + prevHunt*canSt)          

          
          

  #### competition ####
      
      # create list to store current subset of models in
      modsTSB <- list()
      # list model names
      modNamesTSB <- ls(envir = .GlobalEnv, pattern = "^tsb[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesTSB)) { modsTSB[[i]] <- get(modNamesTSB[i]) }
      # create a dataframe of aicc results...
      aicNightTSB <- data.frame(aictab(cand.set = modsTSB, modnames = modNamesTSB))
      # ...sorted from smallest to largest aicc value...
      aicNightTSB <- aicNightTSB[order(aicNightTSB$Delta_AICc), ]
      # ... and store
      write.csv(aicNightTSB, file = "aicNightTSB.csv", row.names = FALSE)      
      # identify moderately-supported models (deltaAICc < 4)
      aic4NightTSB <- subset(aicNightTSB, Delta_AICc < 4.0)
      aic4NightTSB <- droplevels(aic4NightTSB)
      aic4NightTSB$Modnames <- as.character(aic4NightTSB$Modnames)
      # save top-supported models
      topTSBs <- list()
      # list model names
      topTSBnames <- unique(aic4NightTSB$Modnames)
      # store the models in the list
      for (i in 1:length(topTSBnames)) { topTSBs[[i]] <- get(topTSBnames[i]) }      
      
                
    
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ##
#### | RESOURCE SUBSIDY MODELS |  ####
### ### ### ### ### ### ### ### ### ##    
      
      # all feedgrounds, linear
      rsm108 <- update(envtNight, . ~ . + distFeedSt)
      
      # all feedgrounds, quadratic
      rsm109 <- update(envtNight, . ~ . + distFeedSt + I(distFeedSt^2))
      
      # active feedgrounds, linear
      rsm110 <- update(envtNight, . ~ . + activeFeedSt)
      
      # active feedgrounds, quadratic
      rsm111 <- update(envtNight, . ~ . + activeFeedSt + I(activeFeedSt^2))
      
      

  #### competition ####
      
      # create list to store current subset of models in
      modsRSM <- list()
      # list model names
      modNamesRSM <- ls(envir = .GlobalEnv, pattern = "^rsm[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesRSM)) { modsRSM[[i]] <- get(modNamesRSM[i]) }
      # create a dataframe of aicc results...
      aicNightRSM <- data.frame(aictab(cand.set = modsRSM, modnames = modNamesRSM))
      # ...sorted from smallest to largest aicc value...
      aicNightRSM <- aicNightRSM[order(aicNightRSM$Delta_AICc), ]
      # ... and store
      write.csv(aicNightRSM, file = "aicNightRSM.csv", row.names = FALSE)      
      # identify moderately-supported models (deltaAICc < 4)
      aic4NightRSM <- subset(aicNightRSM, Delta_AICc < 4.0)
      aic4NightRSM <- droplevels(aic4NightRSM)
      aic4NightRSM$Modnames <- as.character(aic4NightRSM$Modnames)
      # save top-supported models
      topRSMs <- list()
      # list model names
      topRSMnames <- unique(aic4NightRSM$Modnames)
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
      aicNight <- data.frame(aictab(cand.set = allMods, modnames = allModNames))
      aicNight <- aicNight[order(aicNight$Delta_AICc), ]
      write.csv(aicNight, file = "aicAllNight.csv", row.names = FALSE)    
      
      # identify moderately-supported models (deltaAICc < 4); export results
      aic4Night <- subset(aicNight, Delta_AICc < 4.0)
      aic4Night <- droplevels(aic4Night)
      write.csv(aic4Night, file = "aicModerateNight.csv", row.names = FALSE)           
      
      # identify best-supported models (deltaAICc < 2); export results
      aic2Night <- subset(aicNight, Delta_AICc < 2.0)
      aic2Night <- droplevels(aic2Night)
      write.csv(aic2Night, file = "aicTopNight.csv", row.names = FALSE)     

      
      
################################################################################################## #  

 
    
### ### ### ### ### ### ### ### ###
#### | TOP MODEL DIAGNOSTICS|  ####
### ### ### ### ### ### ### ### ###   
      
      topNight <- get(as.character(aic2Night[1,"Modnames"]))
    
      ## area under the roc curve ##
      invisible(plot(roc(factor(ifelse(modDatNight$Used == 1, 1, 0)), fitted(topNight)), 
                     print.thres = c(.1, .5), col = "red", print.auc = T)) # auc = 0.700
      
      ## predictive accuracy @ >50% ##  
      confusionMatrix(factor(as.character(ifelse(fitted(topNight) > 0.5, "Yes", "No"))), 
                      factor(ifelse(modDatNight$Used == 1, "Yes", "No")), positive = "Yes") # 66.05%
    
      
      ## binned residual plots ##
      binnedplot(fitted(topNight), residuals(topNight, type = "response"), main = "Night - top model")

 

       
################################################################################################## #  

################################################################################################## #  
             
################################################################################################## #  
save.image(paste0("modelsHumanNight", today(), ".RData"))    

################################################################################################## #  
       
################################################################################################## #  

################################################################################################## #  
             

