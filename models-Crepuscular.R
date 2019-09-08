           ### ### ### ### ### ###  ### ### ### ### ### ### ### ### ### ### ### ### 
           #                                                                      #
           #                           CREPUSCULAR MODELS                         #
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
        lcClass = factor(lcClass, levels = c("Forest", "Shrub", "Herbaceous", "Riparian", "NoVeg")),
        # use open recreation as baseline; reorder for more intuitive plot interpretation
        recClass = factor(recClass, levels = c("allOT", "nomotoOT", "noOT", "noRec")))
  
      # only use creptime data for crep models
      modDatCrep <- filter(modDat, daytime == "crep")


      
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### #
####   | ENVIRONMENTAL MODELS |  ####
### ### ### ### ### ### ### ### ### #
    
    
    # from models-Environment.R   
    envtCrep <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
               + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
               + snowSt:canSt + snowSt:northnessSt + snowSt:elevSt
               + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
               + (1|Pack), family = binomial(logit), data = modDatCrep,
               control = glmerControl(optimizer = "bobyqa", 
                                      optCtrl=list(maxfun=3e4),
                                      calc.derivs = FALSE))       

               
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### #### #
#### | LANDSCAPE SHIELD MODELS |  ####
### ### ### ### ### ### ### ### #### #

    #### models ####      

      # roads & buildings, linear
      lsm1 <- update(envtCrep, . ~ . + distRdPavSt + distStrucSt)

      # roads & buildings, quadratic
      lsm2 <- update(envtCrep, . ~ . + distRdPavSt + distStrucSt + I(distRdPavSt^2) + I(distStrucSt^2))

      # buildings only, linear
      lsm3 <- update(envtCrep, . ~ . + distStrucSt)

      # buildings only, quadratic
      lsm4 <- update(envtCrep, . ~ . + distStrucSt + I(distStrucSt^2))

  #### competition ####
      
      # create list to store current subset of models in
      modsLSM <- list()
      # list model names
      modNamesLSM <- ls(envir = .GlobalEnv, pattern = "^lsm[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesLSM)) { modsLSM[[i]] <- get(modNamesLSM[i]) }
      # create a dataframe of aicc results...
      aicCrepLSM <- data.frame(aictab(cand.set = modsLSM, modnames = modNamesLSM))
      # ...sorted from smallest to largest aicc value...
      aicCrepLSM <- aicCrepLSM[order(aicCrepLSM$Delta_AICc), ]
      # ... and store
      write.csv(aicCrepLSM, file = "aicCrepLSM.csv", row.names = FALSE)      
      # identify moderately-supported models (deltaAICc < 4)
      aic4CrepLSM <- subset(aicCrepLSM, Delta_AICc < 4.0)
      aic4CrepLSM <- droplevels(aic4CrepLSM)
      aic4CrepLSM$Modnames <- as.character(aic4CrepLSM$Modnames)
      # save top-supported models
      topLSMs <- list()
      # list model names
      topLSMnames <- unique(aic4CrepLSM$Modnames)
      # store the models in the list
      for (i in 1:length(topLSMnames)) { topLSMs[[i]] <- get(topLSMnames[i]) }      

      
      
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### # 
####  | DISTURBANCE SHIELD MODELS |  ####
### ### ### ### ### ### ### ### ### ### #
      
      
      #### all human influences ####
      
          # linear, predictable feedgrounds
          dsm5 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          dsm6 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          dsm7 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt)
      
          # quadratic, all feedgrounds
          dsm8 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2))
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          dsm9 <- update(envtCrep, . ~ . + distRdSt + recClass + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          dsm10 <- update(envtCrep, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          dsm11 <- update(envtCrep, . ~ . + distRdSt + recClass + distFeedSt)
      
          # quadratic, all feedgrounds
          dsm12 <- update(envtCrep, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2))
                    
      
      
      
      #### motorized recreation, and all others ####   
          
          # linear, predictable feedgrounds
          dsm13 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          dsm14 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          dsm15 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt)
      
          # quadratic, all feedgrounds
          dsm16 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2))
          
          
          
      #### motorized activity only ####   
          
          # linear, predictable feedgrounds
          dsm17 <- update(envtCrep, . ~ . + distRdSt + motoUse + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          dsm18 <- update(envtCrep, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          dsm19 <- update(envtCrep, . ~ . + distRdSt + motoUse + distFeedSt)
      
          # quadratic, all feedgrounds
          dsm20 <- update(envtCrep, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2))      
      

      
      
      #### living and working activity, not recreation per se ####      
      
          # linear, predictable feedgrounds
          dsm21 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + pvt + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          dsm22 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + pvt + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          dsm23 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + pvt + distFeedSt)
      
          # quadratic, all feedgrounds
          dsm24 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + pvt + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2))
          
          
      
      #### off-trail recreation - alone; with or without structures ####  
          
          # off-trail recreation only
          dsm25 <- update(envtCrep, . ~ . + otUse)
          
          # off-trail and structures, linear
          dsm26 <- update(envtCrep, . ~ . + otUse + distStrucSt)
          
          # offtrail and structures, quadratic
          dsm27 <- update(envtCrep, . ~ . + otUse + distStrucSt + I(distStrucSt^2))

            


  #### competition ####
      
      # create list to store current subset of models in
      modsDSM <- list()
      # list model names
      modNamesDSM <- ls(envir = .GlobalEnv, pattern = "^dsm[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesDSM)) { modsDSM[[i]] <- get(modNamesDSM[i]) }
      # create a dataframe of aicc results...
      aicCrepDSM <- data.frame(aictab(cand.set = modsDSM, modnames = modNamesDSM))
      # ...sorted from smallest to largest aicc value...
      aicCrepDSM <- aicCrepDSM[order(aicCrepDSM$Delta_AICc), ]
      # ... and store
      write.csv(aicCrepDSM, file = "aicCrepDSM.csv", row.names = FALSE)      
      # identify moderately-supported models (deltaAICc < 4)
      aic4CrepDSM <- subset(aicCrepDSM, Delta_AICc < 4.0)
      aic4CrepDSM <- droplevels(aic4CrepDSM)
      aic4CrepDSM$Modnames <- as.character(aic4CrepDSM$Modnames)
      # save top-supported models
      topDSMs <- list()
      # list model names
      topDSMnames <- unique(aic4CrepDSM$Modnames)
      # store the models in the list
      for (i in 1:length(topDSMnames)) { topDSMs[[i]] <- get(topDSMnames[i]) }      
            
      
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### ### ##
#### | THREAT SHIELD MODELS Y/N |  ####
### ### ### ### ### ### ### ### ### ### ### ##
      
      
      #### all human influences ####
      
          # linear, predictable feedgrounds
          tsa28 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          tsa29 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*activeFeedSt
                       + hunt*canSt)          
      
          # quadratic, predictable feedgrounds
          tsa30 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  hunt*distRdSt +  hunt*distStrucSt +  hunt*recClass +  hunt*activeFeedSt
                       +  hunt*I(distRdSt^2) +  hunt*I(distStrucSt^2) +  hunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          tsa31 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  hunt*distRdSt +  hunt*distStrucSt +  hunt*recClass +  hunt*activeFeedSt
                       +  hunt*I(distRdSt^2) +  hunt*I(distStrucSt^2) +  hunt*I(activeFeedSt^2)
                       + hunt*canSt)
      
          # linear, all feedgrounds
          tsa32 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          tsa33 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt
                       + hunt*canSt)          
      
          # quadratic, all feedgrounds
          tsa34 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          tsa35 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2)
                       + hunt*canSt)          
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          tsa36 <- update(envtCrep, . ~ . + distRdSt + recClass + activeFeedSt
                       + hunt*distRdSt + hunt*recClass + hunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          tsa37 <- update(envtCrep, . ~ . + distRdSt + recClass + activeFeedSt
                       + hunt*distRdSt + hunt*recClass + hunt*activeFeedSt + hunt*canSt)          
          
      
          # quadratic, predictable feedgrounds
          tsa38 <- update(envtCrep, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*recClass + hunt*activeFeedSt + hunt*I(distRdSt^2) + hunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          tsa39 <- update(envtCrep, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*recClass + hunt*activeFeedSt + hunt*I(distRdSt^2) + hunt*I(activeFeedSt^2)
                       + hunt*canSt)
      
          # linear, all feedgrounds
          tsa40 <- update(envtCrep, . ~ . + distRdSt + recClass + distFeedSt
                       + hunt*distRdSt + hunt*recClass + hunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          tsa41 <- update(envtCrep, . ~ . + distRdSt + recClass + distFeedSt
                       + hunt*distRdSt + hunt*recClass + hunt*distFeedSt + hunt*canSt)          
      
          # quadratic, all feedgrounds
          tsa42 <- update(envtCrep, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*recClass + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          tsa43 <- update(envtCrep, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*recClass + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2)
                       + hunt*canSt)
                    
      
      
      
      #### motorized recreation, and all others ####   
          
          
          # linear, predictable feedgrounds
          tsa44 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          tsa45 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*activeFeedSt
                       + hunt*canSt)          
      
          # quadratic, predictable feedgrounds
          tsa46 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*activeFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          tsa47 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*activeFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(activeFeedSt^2)
                       + hunt*canSt)
      
          # linear, all feedgrounds
          tsa48 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          tsa49 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*distFeedSt
                       + hunt*canSt)          
      
          # quadratic, all feedgrounds
          tsa50 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          tsa51 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2)
                       + hunt*canSt)          
          
          
          
      #### motorized activity only ####   
          
          # linear, predictable feedgrounds
          tsa52 <- update(envtCrep, . ~ . + distRdSt + motoUse + activeFeedSt
                       + hunt*distRdSt + hunt*motoUse + hunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          tsa53 <- update(envtCrep, . ~ . + distRdSt + motoUse + activeFeedSt
                       + hunt*distRdSt + hunt*motoUse + hunt*activeFeedSt + hunt*canSt)          
      
          # quadratic, predictable feedgrounds
          tsa54 <- update(envtCrep, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*motoUse + hunt*activeFeedSt + hunt*I(distRdSt^2) + hunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          tsa55 <- update(envtCrep, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*motoUse + hunt*activeFeedSt + hunt*I(distRdSt^2) + hunt*I(activeFeedSt^2)
                       + hunt*canSt)
      
          # linear, all feedgrounds
          tsa56 <- update(envtCrep, . ~ . + distRdSt + motoUse + distFeedSt
                       + hunt*distRdSt + hunt*motoUse + hunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          tsa57 <- update(envtCrep, . ~ . + distRdSt + motoUse + distFeedSt
                       + hunt*distRdSt + hunt*motoUse + hunt*distFeedSt + hunt*canSt)          
      
          # quadratic, all feedgrounds
          tsa58 <- update(envtCrep, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*motoUse + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2)) 
          
          # quadratic, all feedgrounds, canopy
          tsa59 <- update(envtCrep, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*motoUse + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2)
                       + hunt*canSt) 
      

        
      #### off-trail recreation - alone; with or without structures ####  
          
          # off-trail recreation only
          tsa60 <- update(envtCrep, . ~ . + otUse + hunt*otUse)
          
          # off-trail recreation only, canopy
          tsa61 <- update(envtCrep, . ~ . + otUse + hunt*otUse + hunt*canSt)          
          
          # off-trail and structures, linear
          tsa62 <- update(envtCrep, . ~ . + otUse + distStrucSt + hunt*otUse + hunt*distStrucSt)
          
          # off-trail and structures, linear, canopy
          tsa63 <- update(envtCrep, . ~ . + otUse + distStrucSt + hunt*otUse + hunt*distStrucSt + hunt*canSt)          
          
          # offtrail and structures, quadratic
          tsa64 <- update(envtCrep, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + hunt*otUse + hunt*distStrucSt + hunt*I(distStrucSt^2))
          
          # offtrail and structures, quadratic, canopy
          tsa65 <- update(envtCrep, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + hunt*otUse + hunt*distStrucSt + hunt*I(distStrucSt^2) + hunt*canSt)          
          
          
    #### change use of landscape but not response to people ####       
          
          # all environment
          tsa66 <- update(envtCrep, . ~ . + hunt*canSt + hunt*slopeSt + hunt*elevSt + hunt*northnessSt
                        + hunt*snowSt)

          
          # canopy (i.e., hiding cover/escape terrain) only
          tsa67 <- update(envtCrep, . ~ . + hunt*canSt)

          
  #### competition ####
      
      # create list to store current subset of models in
      modsTSA <- list()
      # list model names
      modNamesTSA <- ls(envir = .GlobalEnv, pattern = "^tsa[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesTSA)) { modsTSA[[i]] <- get(modNamesTSA[i]) }
      # create a dataframe of aicc results...
      aicCrepTSA <- data.frame(aictab(cand.set = modsTSA, modnames = modNamesTSA))
      # ...sorted from smallest to largest aicc value...
      aicCrepTSA <- aicCrepTSA[order(aicCrepTSA$Delta_AICc), ]
      # ... and store
      write.csv(aicCrepTSA, file = "aicCrepTSA.csv", row.names = FALSE)      
      # identify moderately-supported models (deltaAICc < 4)
      aic4CrepTSA <- subset(aicCrepTSA, Delta_AICc < 4.0)
      aic4CrepTSA <- droplevels(aic4CrepTSA)
      aic4CrepTSA$Modnames <- as.character(aic4CrepTSA$Modnames)
      # save top-supported models
      topTSAs <- list()
      # list model names
      topTSAnames <- unique(aic4CrepTSA$Modnames)
      # store the models in the list
      for (i in 1:length(topTSAnames)) { topTSAs[[i]] <- get(topTSAnames[i]) }      
      
################################################################################################## #  
  
    
### ### ### ### ### ###  ### ### ### ### ### ### ### ### ###
#### | THREAT SHIELD MODELS previous year Y/N |  ####
### ### ### ### ### ###  ### ### ### ### ### ### ### ### ###
          
          
      
      #### all human influences ####
      
          # linear, predictable feedgrounds
          tsb68 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          tsb69 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*activeFeedSt
                       + prevHunt*canSt)
          
          # quadratic, predictable feedgrounds
          tsb70 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  prevHunt*distRdSt +  prevHunt*distStrucSt +  prevHunt*recClass +  prevHunt*activeFeedSt
                       +  prevHunt*I(distRdSt^2) +  prevHunt*I(distStrucSt^2) +  prevHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          tsb71 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  prevHunt*distRdSt +  prevHunt*distStrucSt +  prevHunt*recClass +  prevHunt*activeFeedSt
                       +  prevHunt*I(distRdSt^2) +  prevHunt*I(distStrucSt^2) +  prevHunt*I(activeFeedSt^2)
                       + prevHunt*canSt)
          
          # linear, all feedgrounds
          tsb72 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          tsb73 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*distFeedSt
                       + prevHunt*canSt)          
      
          # quadratic, all feedgrounds
          tsb74 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*distFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          tsb75 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*distFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2)
                       + prevHunt*canSt)          
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          tsb76 <- update(envtCrep, . ~ . + distRdSt + recClass + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          tsb77 <- update(envtCrep, . ~ . + distRdSt + recClass + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*activeFeedSt + prevHunt*canSt)          
          

          # quadratic, predictable feedgrounds
          tsb78 <- update(envtCrep, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) 
                       + prevHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          tsb79 <- update(envtCrep, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) 
                       + prevHunt*I(activeFeedSt^2)
                       + prevHunt*canSt)
      
          # linear, all feedgrounds
          tsb80 <- update(envtCrep, . ~ . + distRdSt + recClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          tsb81 <- update(envtCrep, . ~ . + distRdSt + recClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*distFeedSt + prevHunt*canSt)          
      
          # quadratic, all feedgrounds
          tsb82 <- update(envtCrep, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) 
                       + prevHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          tsb83 <- update(envtCrep, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) 
                       + prevHunt*I(distFeedSt^2)
                       + prevHunt*canSt)
                    
      
      
      
      #### motorized recreation, and all others ####   
          
          
          # linear, predictable feedgrounds
          tsb84 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          tsb85 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*activeFeedSt
                       + prevHunt*canSt)          
      
          # quadratic, predictable feedgrounds
          tsb86 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*activeFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          tsb87 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*activeFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(activeFeedSt^2)
                       + prevHunt*canSt)
      
          # linear, all feedgrounds
          tsb88 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          tsb89 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*distFeedSt
                       + prevHunt*canSt)          
      
          # quadratic, all feedgrounds
          tsb90 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*distFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          tsb91 <- update(envtCrep, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*distFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2)
                       + prevHunt*canSt)          
          
          
          
      #### motorized activity only ####   
          
          # linear, predictable feedgrounds
          tsb92 <- update(envtCrep, . ~ . + distRdSt + motoUse + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          tsb93 <- update(envtCrep, . ~ . + distRdSt + motoUse + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*activeFeedSt + prevHunt*canSt)          
      
          # quadratic, predictable feedgrounds
          tsb94 <- update(envtCrep, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) 
                       + prevHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          tsb95 <- update(envtCrep, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) 
                       + prevHunt*I(activeFeedSt^2)
                       + prevHunt*canSt)
      
          # linear, all feedgrounds
          tsb96 <- update(envtCrep, . ~ . + distRdSt + motoUse + distFeedSt
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          tsb97 <- update(envtCrep, . ~ . + distRdSt + motoUse + distFeedSt
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*distFeedSt + prevHunt*canSt)          
      
          # quadratic, all feedgrounds
          tsb98 <- update(envtCrep, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) 
                       + prevHunt*I(distFeedSt^2)) 
          
          # quadratic, all feedgrounds, canopy
          tsb99 <- update(envtCrep, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) 
                       + prevHunt*I(distFeedSt^2)
                       + prevHunt*canSt) 
      

        
      #### off-trail recreation - alone; with or without structures ####  
          
          # off-trail recreation only
          tsb100 <- update(envtCrep, . ~ . + otUse + prevHunt*otUse)
          
          # off-trail recreation only, canopy
          tsb101 <- update(envtCrep, . ~ . + otUse + prevHunt*otUse + prevHunt*canSt)          
          
          # off-trail and structures, linear
          tsb102 <- update(envtCrep, . ~ . + otUse + distStrucSt + prevHunt*otUse + prevHunt*distStrucSt)
          
          # off-trail and structures, linear, canopy
          tsb103 <- update(envtCrep, . ~ . + otUse + distStrucSt + prevHunt*otUse + prevHunt*distStrucSt + prevHunt*canSt)          
          
          # offtrail and structures, quadratic
          tsb104 <- update(envtCrep, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + prevHunt*otUse + prevHunt*distStrucSt + prevHunt*I(distStrucSt^2))
          
          # offtrail and structures, quadratic, canopy
          tsb105 <- update(envtCrep, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + prevHunt*otUse + prevHunt*distStrucSt + prevHunt*I(distStrucSt^2) + prevHunt*canSt)          
          
          
    #### change use of landscape but not response to people ####       
          
          # all environment
          tsb106 <- update(envtCrep, . ~ . + prevHunt*canSt + prevHunt*slopeSt + prevHunt*elevSt + prevHunt*northnessSt
                         + prevHunt*snowSt)
          
          
          # canopy (i.e., hiding cover/escape terrain) only
          tsb107 <- update(envtCrep, . ~ . + prevHunt*canSt)          

          
          

  #### competition ####
      
      # create list to store current subset of models in
      modsTSB <- list()
      # list model names
      modNamesTSB <- ls(envir = .GlobalEnv, pattern = "^tsb[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesTSB)) { modsTSB[[i]] <- get(modNamesTSB[i]) }
      # create a dataframe of aicc results...
      aicCrepTSB <- data.frame(aictab(cand.set = modsTSB, modnames = modNamesTSB))
      # ...sorted from smallest to largest aicc value...
      aicCrepTSB <- aicCrepTSB[order(aicCrepTSB$Delta_AICc), ]
      # ... and store
      write.csv(aicCrepTSB, file = "aicCrepTSB.csv", row.names = FALSE)      
      # identify moderately-supported models (deltaAICc < 4)
      aic4CrepTSB <- subset(aicCrepTSB, Delta_AICc < 4.0)
      aic4CrepTSB <- droplevels(aic4CrepTSB)
      aic4CrepTSB$Modnames <- as.character(aic4CrepTSB$Modnames)
      # save top-supported models
      topTSBs <- list()
      # list model names
      topTSBnames <- unique(aic4CrepTSB$Modnames)
      # store the models in the list
      for (i in 1:length(topTSBnames)) { topTSBs[[i]] <- get(topTSBnames[i]) }      
      
                
    
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ##
#### | RESOURCE SUBSIDY MODELS |  ####
### ### ### ### ### ### ### ### ### ##    
      
      # all feedgrounds, linear
      rsm108 <- update(envtCrep, . ~ . + distFeedSt)
      
      # all feedgrounds, quadratic
      rsm109 <- update(envtCrep, . ~ . + distFeedSt + I(distFeedSt^2))
      
      # active feedgrounds, linear
      rsm110 <- update(envtCrep, . ~ . + activeFeedSt)
      
      # active feedgrounds, quadratic
      rsm111 <- update(envtCrep, . ~ . + activeFeedSt + I(activeFeedSt^2))
      
      

  #### competition ####
      
      # create list to store current subset of models in
      modsRSM <- list()
      # list model names
      modNamesRSM <- ls(envir = .GlobalEnv, pattern = "^rsm[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesRSM)) { modsRSM[[i]] <- get(modNamesRSM[i]) }
      # create a dataframe of aicc results...
      aicCrepRSM <- data.frame(aictab(cand.set = modsRSM, modnames = modNamesRSM))
      # ...sorted from smallest to largest aicc value...
      aicCrepRSM <- aicCrepRSM[order(aicCrepRSM$Delta_AICc), ]
      # ... and store
      write.csv(aicCrepRSM, file = "aicCrepRSM.csv", row.names = FALSE)      
      # identify moderately-supported models (deltaAICc < 4)
      aic4CrepRSM <- subset(aicCrepRSM, Delta_AICc < 4.0)
      aic4CrepRSM <- droplevels(aic4CrepRSM)
      aic4CrepRSM$Modnames <- as.character(aic4CrepRSM$Modnames)
      # save top-supported models
      topRSMs <- list()
      # list model names
      topRSMnames <- unique(aic4CrepRSM$Modnames)
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
      aicCrep <- data.frame(aictab(cand.set = allMods, modnames = allModNames))
      aicCrep <- aicCrep[order(aicCrep$Delta_AICc), ]
      write.csv(aicCrep, file = "aicAllCrep.csv", row.names = FALSE)    
      
      # identify moderately-supported models (deltaAICc < 4); export results
      aic4Crep <- subset(aicCrep, Delta_AICc < 4.0)
      aic4Crep <- droplevels(aic4Crep)
      write.csv(aic2Crep, file = "aicModerateCrep.csv", row.names = FALSE)           
      
      # identify best-supported models (deltaAICc < 2); export results
      aic2Crep <- subset(aicCrep, Delta_AICc < 2.0)
      aic2Crep <- droplevels(aic2Crep)
      write.csv(aic2Crep, file = "aicTopCrep.csv", row.names = FALSE)     

      
      
################################################################################################## #  

 
    
### ### ### ### ### ### ### ### ###
#### | TOP MODEL DIAGNOSTICS|  ####
### ### ### ### ### ### ### ### ###   
      
      topCrep <- get(as.character(aic2Crep[1,"Modnames"]))
    
      ## area under the roc curve ##
      invisible(plot(roc(factor(ifelse(modDatCrep$Used == 1, 1, 0)), fitted(topCrep)), 
                     print.thres = c(.1, .5), col = "red", print.auc = T)) # auc = 0.722
      
      ## predictive accuracy @ >50% ##  
      confusionMatrix(factor(as.character(ifelse(fitted(topCrep) > 0.5, "Yes", "No"))), 
                      factor(ifelse(modDatCrep$Used == 1, "Yes", "No")), positive = "Yes") # 
    
      
      ## binned residual plots ##
      binnedplot(fitted(topCrep), residuals(topCrep, type = "response"), main = "Crep - top model")

 

       
################################################################################################## #  

################################################################################################## #  
             
################################################################################################## #  
save.image(paste0("modelsHumanCrep", today(), ".RData"))    
 
################################################################################################## #  
       
################################################################################################## #  

################################################################################################## #  
             

