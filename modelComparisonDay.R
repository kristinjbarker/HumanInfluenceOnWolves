           ### ### ### ### ### ###  ### ### ### ### ### ### ### ### ### ### ### ### 
           #                                                                      #
           #        SUMMARIES, VISUALIZATIONS, AND PRELIMINARY DATA WORK          #
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


  
  #### Set working directory and filepath to spatial data ####


#### Set working directory, identify correct google database, and run correct cluster prep code ####

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

    
    #### Format covariates ####
    
    modDat <- modDatRaw %>% mutate(
      datetime = ymd_hms(datetime),
      # handle datetimes and dates of course
      Date = ymd(Date),
      # standardize continuous covariates
      slopeSt = (slope - mean(slope))/sd(slope),
      elevSt = (elev - mean(elev))/sd(elev),
      northnessSt = (northness - mean(northness))/sd(northness),
      snowSt = (snowCm - mean(snowCm))/sd(snowCm),
      canSt = (can - mean(can))/sd(can),
      distRdSt = (distRd - mean(distRd))/sd(distRd),
      distStrucSt = (distStruc - mean(distStruc))/sd(distStruc),
      distFeedSt = (distFeed - mean(distFeed))/sd(distFeed),
      activeFeedSt = (distFeedActive - mean(distFeedActive))/sd(distFeedActive),
      # order landcover from most to least available
      lcClass = factor(lcClass, levels = c("Forest", "Shrub", "Herbaceous", "Riparian", "NoVeg")),
      # order rec from most to least regulated, relative to private land as baseline
      recClass = factor(recClass, levels = c("noRec", "noOT", "nomotoOT", "allOT")),
      # add binary private land designation
      pvt = ifelse(recClass == "noRec", 1, 0),
      # add binary off-trail/no off-trail use designation
      otUse = ifelse(recClass == "noOT", 0, 1),
      # add binary designation for whether moto rec is allowed
      motoUse = ifelse(recClass == "allOT", 1, 0),
      # add binary indication of whether hunting was allowed in the previous year
      prevHunt = ifelse(Year == 2013 | Year == 2014 | Year >= 2018, 1, 0),
      # add time since hunting was first allowed (2012)
      tSinceHunt = ifelse(Year - 2012 + 1 < 0, 0, Year - 2012 + 1),
      # add years of continuous hunting (to account for no hunting 2014, 2015, 2016)
      tContHunt = ifelse(Year == 2012, 1,
                         ifelse(Year == 2013, 2,
                                ifelse(Year >= 2017, Year - 2017 + 1, 0))))

    
    #### split day/night ####
    
    modDatDay <- filter(modDat, daytime == "day")

    
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### #
####   | ENVIRONMENTAL MODELS |  ####
### ### ### ### ### ### ### ### ### #
    
    
    # daytime (from dataExploration.R)
    envtDay <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                     + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                     + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt + snowSt:elevSt
                     + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                     + (1|Pack/wolfYr), 
                      family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                     data = modDatDay) 

                  
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### #### #
#### | LANDSCAPE SHIELD MODELS |  ####
### ### ### ### ### ### ### ### #### #

    #### models ####      

      # roads & buildings, linear
      d1 <- update(envtDay, . ~ . + distRdSt + distStrucSt)

      # roads & buildings, quadratic
      d2 <- update(envtDay, . ~ . + distRdSt + distStrucSt + I(distRdSt^2) + I(distStrucSt^2))

      # buildings only, linear
      d3 <- update(envtDay, . ~ . + distStrucSt)

      # buildings only, quadratic
      d4 <- update(envtDay, . ~ . + distStrucSt + I(distStrucSt^2))

  #### competition ####
      
      # create list to store current subset of models in
      modsDay <- list()
      # list model names
      modNamesDay <- ls(envir = .GlobalEnv, pattern = "^d[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesDay)) { modsDay[[i]] <- get(modNamesDay[i]) }
      # create a dataframe of aicc results...
      aicDay <- data.frame(aictab(cand.set = modsDay, modnames = modNamesDay))
      # ...sorted from smallest to largest aicc value...
      aicDay <- aicDay[order(aicDay$Delta_AICc), ]
      # ... and store
      write.csv(aicDay, file = "aicDayLSM.csv", row.names = FALSE)      
      # identify best-supported models (deltaAICc < 2)
      aic2Day <- subset(aicDay, Delta_AICc < 2.0)
      aic2Day <- droplevels(aic2Day)
      aic2Day$Modnames <- as.character(aic2Day$Modnames)
      # save top-supported models
      topLSMs <- list()
      # list model names
      topLSMnames <- unique(aic2Day$Modnames)
      # store the models in the list
      for (i in 1:length(topLSMnames)) { topLSMs[[i]] <- get(topLSMnames[i]) }      
      # delete unsupported models and clear memory for next batch
      rm(list = ls(pattern = "^d[0:9]*"))
      gc()
      
      
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### # 
####  | DISTURBANCE SHIELD MODELS |  ####
### ### ### ### ### ### ### ### ### ### #
      
      
      #### all human influences ####
      
          # linear, predictable feedgrounds
          d5 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          d6 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          d7 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt)
      
          # quadratic, all feedgrounds
          d8 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2))
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          d9 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          d10 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          d11 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt)
      
          # quadratic, all feedgrounds
          d12 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2))
                    
      
      
      
      #### motorized recreation, and all others ####   
          
          # linear, predictable feedgrounds
          d13 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          d14 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          d15 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt)
      
          # quadratic, all feedgrounds
          d16 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2))
          
          
          
      #### motorized activity only ####   
          
          # linear, predictable feedgrounds
          d17 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          d18 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          d19 <- update(envtDay, . ~ . + distRdSt + motoUse + distFeedSt)
      
          # quadratic, all feedgrounds
          d20 <- update(envtDay, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2))      
      

      
      
      #### living and working activity, not recreation per se ####      
      
          # linear, predictable feedgrounds
          d21 <- update(envtDay, . ~ . + distRdSt + distStrucSt + pvt + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          d22 <- update(envtDay, . ~ . + distRdSt + distStrucSt + pvt + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          d23 <- update(envtDay, . ~ . + distRdSt + distStrucSt + pvt + distFeedSt)
      
          # quadratic, all feedgrounds
          d24 <- update(envtDay, . ~ . + distRdSt + distStrucSt + pvt + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2))
          
          
      
      #### off-trail recreation - alone; with or without structures ####  
          
          # off-trail recreation only
          d25 <- update(envtDay, . ~ . + otUse)
          
          # off-trail and structures, linear
          d26 <- update(envtDay, . ~ . + otUse + distStrucSt)
          
          # offtrail and structures, quadratic
          d27 <- update(envtDay, . ~ . + otUse + distStrucSt + I(distStrucSt^2))

            
      
      

  #### competition ####
      
      # create list to store current subset of models in
      modsDay <- list()
      # list model names
      modNamesDay <- ls(envir = .GlobalEnv, pattern = "^d[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesDay)) { modsDay[[i]] <- get(modNamesDay[i]) }
      # create a dataframe of aicc results...
      aicDay <- data.frame(aictab(cand.set = modsDay, modnames = modNamesDay))
      # ...sorted from smallest to largest aicc value...
      aicDay <- aicDay[order(aicDay$Delta_AICc), ]
      # ... and store
      write.csv(aicDay, file = "aicDayDSM.csv", row.names = FALSE)
      # identify best-supported models (deltaAICc < 2)
      aic2Day <- subset(aicDay, Delta_AICc < 2.0)
      aic2Day <- droplevels(aic2Day)
      aic2Day$Modnames <- as.character(aic2Day$Modnames)
      # save top-supported models
      topDSMs <- list()
      # list model names
      topDSMnames <- unique(aic2Day$Modnames)
      # store the models in the list
      for (i in 1:length(topDSMnames)) { topDSMs[[i]] <- get(topDSMnames[i]) }      
      # delete unsupported models and clear memory for next batch
      rm(list = ls(pattern = "^d[0:9]*"))
      gc()
            
      
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### ### ##
#### | THREAT SHIELD MODELS - Hunt Y/N |  ####
### ### ### ### ### ### ### ### ### ### ### ##
      
      
      #### all human influences ####
      
          # linear, predictable feedgrounds
          d28 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d29 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*activeFeedSt
                       + hunt*canSt)          
      
          # quadratic, predictable feedgrounds
          d30 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  hunt*distRdSt +  hunt*distStrucSt +  hunt*recClass +  hunt*activeFeedSt
                       +  hunt*I(distRdSt^2) +  hunt*I(distStrucSt^2) +  hunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          d31 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  hunt*distRdSt +  hunt*distStrucSt +  hunt*recClass +  hunt*activeFeedSt
                       +  hunt*I(distRdSt^2) +  hunt*I(distStrucSt^2) +  hunt*I(activeFeedSt^2)
                       + hunt*canSt)
      
          # linear, all feedgrounds
          d32 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d33 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt
                       + hunt*canSt)          
      
          # quadratic, all feedgrounds
          d34 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d35 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2)
                       + hunt*canSt)          
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          d36 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt
                       + hunt*distRdSt + hunt*recClass + hunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d37 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt
                       + hunt*distRdSt + hunt*recClass + hunt*activeFeedSt + hunt*canSt)          
          
      
          # quadratic, predictable feedgrounds
          d38 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*recClass + hunt*activeFeedSt + hunt*I(distRdSt^2) + hunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          d39 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*recClass + hunt*activeFeedSt + hunt*I(distRdSt^2) + hunt*I(activeFeedSt^2)
                       + hunt*canSt)
      
          # linear, all feedgrounds
          d40 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt
                       + hunt*distRdSt + hunt*recClass + hunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d41 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt
                       + hunt*distRdSt + hunt*recClass + hunt*distFeedSt + hunt*canSt)          
      
          # quadratic, all feedgrounds
          d42 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*recClass + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d43 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*recClass + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2)
                       + hunt*canSt)
                    
      
      
      
      #### motorized recreation, and all others ####   
          
          
          # linear, predictable feedgrounds
          d44 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d45 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*activeFeedSt
                       + hunt*canSt)          
      
          # quadratic, predictable feedgrounds
          d46 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*activeFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          d47 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*activeFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(activeFeedSt^2)
                       + hunt*canSt)
      
          # linear, all feedgrounds
          d48 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d49 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*distFeedSt
                       + hunt*canSt)          
      
          # quadratic, all feedgrounds
          d50 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d51 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2)
                       + hunt*canSt)          
          
          
          
      #### motorized activity only ####   
          
          # linear, predictable feedgrounds
          d52 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt
                       + hunt*distRdSt + hunt*motoUse + hunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d53 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt
                       + hunt*distRdSt + hunt*motoUse + hunt*activeFeedSt + hunt*canSt)          
      
          # quadratic, predictable feedgrounds
          d54 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*motoUse + hunt*activeFeedSt + hunt*I(distRdSt^2) + hunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          d55 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*motoUse + hunt*activeFeedSt + hunt*I(distRdSt^2) + hunt*I(activeFeedSt^2)
                       + hunt*canSt)
      
          # linear, all feedgrounds
          d56 <- update(envtDay, . ~ . + distRdSt + motoUse + distFeedSt
                       + hunt*distRdSt + hunt*motoUse + hunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d57 <- update(envtDay, . ~ . + distRdSt + motoUse + distFeedSt
                       + hunt*distRdSt + hunt*motoUse + hunt*distFeedSt + hunt*canSt)          
      
          # quadratic, all feedgrounds
          d58 <- update(envtDay, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*motoUse + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2)) 
          
          # quadratic, all feedgrounds, canopy
          d59 <- update(envtDay, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*motoUse + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2)
                       + hunt*canSt) 
      

        
      #### off-trail recreation - alone; with or without structures ####  
          
          # off-trail recreation only
          d60 <- update(envtDay, . ~ . + otUse + hunt*otUse)
          
          # off-trail recreation only, canopy
          d61 <- update(envtDay, . ~ . + otUse + hunt*otUse + hunt*canSt)          
          
          # off-trail and structures, linear
          d62 <- update(envtDay, . ~ . + otUse + distStrucSt + hunt*otUse + hunt*distStrucSt)
          
          # off-trail and structures, linear, canopy
          d63 <- update(envtDay, . ~ . + otUse + distStrucSt + hunt*otUse + hunt*distStrucSt + hunt*canSt)          
          
          # offtrail and structures, quadratic
          d64 <- update(envtDay, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + hunt*otUse + hunt*distStrucSt + hunt*I(distStrucSt^2))
          
          # offtrail and structures, quadratic, canopy
          d65 <- update(envtDay, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + hunt*otUse + hunt*distStrucSt + hunt*I(distStrucSt^2) + hunt*canSt)          
          
          
    #### change use of landscape but not response to people ####       
          
          # all environment
          d66 <- update(envtDay, . ~ . + hunt:lcClass + hunt*can + hunt*slope + hunt*elev + hunt*northness
                       + hunt*snowSt + hunt*I(slope^2) + hunt:I(elev^2) + hunt:I(northness^2))
          
          
          # canopy (i.e., hiding cover/escape terrain) only
          d67 <- update(envtDay, . ~ . + hunt*canSt)
      

  #### competition ####
      
      # create list to store current subset of models in
      modsDay <- list()
      # list model names
      modNamesDay <- ls(envir = .GlobalEnv, pattern = "^d[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesDay)) { modsDay[[i]] <- get(modNamesDay[i]) }
      # create a dataframe of aicc results...
      aicDay <- data.frame(aictab(cand.set = modsDay, modnames = modNamesDay))
      # ...sorted from smallest to largest aicc value...
      aicDay <- aicDay[order(aicDay$Delta_AICc), ]
      # ... and store
      write.csv(aicDay, file = "aicDayTSMA.csv", row.names = FALSE)      
      # identify best-supported models (deltaAICc < 2)
      aic2Day <- subset(aicDay, Delta_AICc < 2.0)
      aic2Day <- droplevels(aic2Day)
      aic2Day$Modnames <- as.character(aic2Day$Modnames)
      # save top-supported models
      topTSMAs <- list()
      # list model names
      topTSMAnames <- unique(aic2Day$Modnames)
      # store the models in the list
      for (i in 1:length(topTSMAnames)) { topTSMAs[[i]] <- get(topTSMAnames[i]) }      
      # delete unsupported models and clear memory for next batch
      rm(list = ls(pattern = "^d[0:9]*"))
      gc()      
      
################################################################################################## #  
  
    
### ### ### ### ### ###  ### ### ### ### ### ### ### ### ###
#### | THREAT SHIELD MODELS - Hunt previous year Y/N |  ####
### ### ### ### ### ###  ### ### ### ### ### ### ### ### ###
          
          
      
      #### all human influences ####
      
          # linear, predictable feedgrounds
          d68 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d69 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*activeFeedSt
                       + prevHunt*canSt)
          
          # quadratic, predictable feedgrounds
          d70 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  prevHunt*distRdSt +  prevHunt*distStrucSt +  prevHunt*recClass +  prevHunt*activeFeedSt
                       +  prevHunt*I(distRdSt^2) +  prevHunt*I(distStrucSt^2) +  prevHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          d71 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  prevHunt*distRdSt +  prevHunt*distStrucSt +  prevHunt*recClass +  prevHunt*activeFeedSt
                       +  prevHunt*I(distRdSt^2) +  prevHunt*I(distStrucSt^2) +  prevHunt*I(activeFeedSt^2)
                       + prevHunt*canSt)
          
          # linear, all feedgrounds
          d72 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d73 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*distFeedSt
                       + prevHunt*canSt)          
      
          # quadratic, all feedgrounds
          d74 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*distFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d75 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*distFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2)
                       + prevHunt*canSt)          
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          d76 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d77 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*activeFeedSt + prevHunt*canSt)          
          

          # quadratic, predictable feedgrounds
          d78 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          d79 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(activeFeedSt^2)
                       + prevHunt*canSt)
      
          # linear, all feedgrounds
          d80 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d81 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*distFeedSt + prevHunt*canSt)          
      
          # quadratic, all feedgrounds
          d82 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d83 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(distFeedSt^2)
                       + prevHunt*canSt)
                    
      
      
      
      #### motorized recreation, and all others ####   
          
          
          # linear, predictable feedgrounds
          d84 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d85 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*activeFeedSt
                       + prevHunt*canSt)          
      
          # quadratic, predictable feedgrounds
          d86 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*activeFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          d87 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*activeFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(activeFeedSt^2)
                       + prevHunt*canSt)
      
          # linear, all feedgrounds
          d88 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d89 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*distFeedSt
                       + prevHunt*canSt)          
      
          # quadratic, all feedgrounds
          d90 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*distFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d91 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*distFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2)
                       + prevHunt*canSt)          
          
          
          
      #### motorized activity only ####   
          
          # linear, predictable feedgrounds
          d92 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d93 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*activeFeedSt + prevHunt*canSt)          
      
          # quadratic, predictable feedgrounds
          d94 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          d95 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(activeFeedSt^2)
                       + prevHunt*canSt)
      
          # linear, all feedgrounds
          d96 <- update(envtDay, . ~ . + distRdSt + motoUse + distFeedSt
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d97 <- update(envtDay, . ~ . + distRdSt + motoUse + distFeedSt
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*distFeedSt + prevHunt*canSt)          
      
          # quadratic, all feedgrounds
          d98 <- update(envtDay, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(distFeedSt^2)) 
          
          # quadratic, all feedgrounds, canopy
          d99 <- update(envtDay, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(distFeedSt^2)
                       + prevHunt*canSt) 
      

        
      #### off-trail recreation - alone; with or without structures ####  
          
          # off-trail recreation only
          d100 <- update(envtDay, . ~ . + otUse + prevHunt*otUse)
          
          # off-trail recreation only, canopy
          d101 <- update(envtDay, . ~ . + otUse + prevHunt*otUse + prevHunt*canSt)          
          
          # off-trail and structures, linear
          d102 <- update(envtDay, . ~ . + otUse + distStrucSt + prevHunt*otUse + prevHunt*distStrucSt)
          
          # off-trail and structures, linear, canopy
          d103 <- update(envtDay, . ~ . + otUse + distStrucSt + prevHunt*otUse + prevHunt*distStrucSt + prevHunt*canSt)          
          
          # offtrail and structures, quadratic
          d104 <- update(envtDay, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + prevHunt*otUse + prevHunt*distStrucSt + prevHunt*I(distStrucSt^2))
          
          # offtrail and structures, quadratic, canopy
          d105 <- update(envtDay, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + prevHunt*otUse + prevHunt*distStrucSt + prevHunt*I(distStrucSt^2) + prevHunt*canSt)          
          
          
    #### change use of landscape but not response to people ####       
          
          # all environment
          d106 <- update(envtDay, . ~ . + prevHunt:lcClass + prevHunt*can + prevHunt*slope + prevHunt*elev + prevHunt*northness
                       + prevHunt*snowSt + prevHunt*I(slope^2) + prevHunt:I(elev^2) + prevHunt:I(northness^2))
          
          
          # canopy (i.e., hiding cover/escape terrain) only
          d107 <- update(envtDay, . ~ . + prevHunt*canSt)          

          
          
  #### competition ####
      
      # create list to store current subset of models in
      modsDay <- list()
      # list model names
      modNamesDay <- ls(envir = .GlobalEnv, pattern = "^d[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesDay)) { modsDay[[i]] <- get(modNamesDay[i]) }
      # create a dataframe of aicc results...
      aicDay <- data.frame(aictab(cand.set = modsDay, modnames = modNamesDay))
      # ...sorted from smallest to largest aicc value...
      aicDay <- aicDay[order(aicDay$Delta_AICc), ]
      # ... and store
      write.csv(aicDay, file = "aicDayTSMB.csv", row.names = FALSE)      
      # identify best-supported models (deltaAICc < 2)
      aic2Day <- subset(aicDay, Delta_AICc < 2.0)
      aic2Day <- droplevels(aic2Day)
      aic2Day$Modnames <- as.character(aic2Day$Modnames)
      # save top-supported models
      topTSMBs <- list()
      # list model names
      topTSMBnames <- unique(aic2Day$Modnames)
      # store the models in the list
      for (i in 1:length(topTSMBnames)) { topTSMBs[[i]] <- get(topTSMBnames[i]) }      
      # delete unsupported models and clear memory for next batch
      rm(list = ls(pattern = "^d[0:9]*"))
      gc()      
             
                
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### ### ### ### #
#### | THREAT SHIELD MODELS - Time since hunt |  ####
### ### ### ### ### ### ### ### ### ### ### ### ### #
          
      
      #### all human influences ####
      
          # linear, predictable feedgrounds
          d108 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*recClass + tSinceHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d109 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*recClass + tSinceHunt*activeFeedSt
                       + tSinceHunt*canSt)          
      
          # quadratic, predictable feedgrounds
          d110 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  tSinceHunt*distRdSt +  tSinceHunt*distStrucSt +  tSinceHunt*recClass +  tSinceHunt*activeFeedSt
                       +  tSinceHunt*I(distRdSt^2) +  tSinceHunt*I(distStrucSt^2) +  tSinceHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          d111 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  tSinceHunt*distRdSt +  tSinceHunt*distStrucSt +  tSinceHunt*recClass +  tSinceHunt*activeFeedSt
                       +  tSinceHunt*I(distRdSt^2) +  tSinceHunt*I(distStrucSt^2) +  tSinceHunt*I(activeFeedSt^2)
                       + tSinceHunt*canSt)
      
          # linear, all feedgrounds
          d112 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*recClass + tSinceHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d113 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*recClass + tSinceHunt*distFeedSt
                       + tSinceHunt*canSt)          
      
          # quadratic, all feedgrounds
          d114 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*recClass + tSinceHunt*distFeedSt
                       + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distStrucSt^2) + tSinceHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d115 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*recClass + tSinceHunt*distFeedSt
                       + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distStrucSt^2) + tSinceHunt*I(distFeedSt^2)
                       + tSinceHunt*canSt)          
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          d116 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d117 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*activeFeedSt + tSinceHunt*canSt)          
          
      
          # quadratic, predictable feedgrounds
          d118 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*activeFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          d119 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*activeFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(activeFeedSt^2)
                       + tSinceHunt*canSt)
      
          # linear, all feedgrounds
          d120 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d121 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*distFeedSt + tSinceHunt*canSt)          
      
          # quadratic, all feedgrounds
          d122 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*distFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d123 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*distFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distFeedSt^2)
                       + tSinceHunt*canSt)
                    
      
      
      
      #### motorized recreation, and all others ####   
          
          
          # linear, predictable feedgrounds
          d124 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoUse + tSinceHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d125 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoUse + tSinceHunt*activeFeedSt
                       + tSinceHunt*canSt)          
      
          # quadratic, predictable feedgrounds
          d126 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoUse + tSinceHunt*activeFeedSt
                       + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distStrucSt^2) + tSinceHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          d127 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoUse + tSinceHunt*activeFeedSt
                       + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distStrucSt^2) + tSinceHunt*I(activeFeedSt^2)
                       + tSinceHunt*canSt)
      
          # linear, all feedgrounds
          d128 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoUse + tSinceHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d129 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoUse + tSinceHunt*distFeedSt
                       + tSinceHunt*canSt)          
      
          # quadratic, all feedgrounds
          d130 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoUse + tSinceHunt*distFeedSt
                       + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distStrucSt^2) + tSinceHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d131 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoUse + tSinceHunt*distFeedSt
                       + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distStrucSt^2) + tSinceHunt*I(distFeedSt^2)
                       + tSinceHunt*canSt)          
          
          
          
      #### motorized activity only ####   
          
          # linear, predictable feedgrounds
          d132 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*motoUse + tSinceHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d133 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*motoUse + tSinceHunt*activeFeedSt + tSinceHunt*canSt)          

          # quadratic, predictable feedgrounds
          d134 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*motoUse + tSinceHunt*activeFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          d135 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*motoUse + tSinceHunt*activeFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(activeFeedSt^2)
                       + tSinceHunt*canSt)
      
          # linear, all feedgrounds
          d136 <- update(envtDay, . ~ . + distRdSt + motoUse + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*motoUse + tSinceHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d137 <- update(envtDay, . ~ . + distRdSt + motoUse + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*motoUse + tSinceHunt*distFeedSt + tSinceHunt*canSt)          
      
          # quadratic, all feedgrounds
          d138 <- update(envtDay, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*motoUse + tSinceHunt*distFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distFeedSt^2)) 
          
          # quadratic, all feedgrounds, canopy
          d139 <- update(envtDay, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*motoUse + tSinceHunt*distFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distFeedSt^2)
                       + tSinceHunt*canSt) 
      

        
      #### off-trail recreation - alone; with or without structures ####  
          
          # off-trail recreation only
          d140 <- update(envtDay, . ~ . + otUse + tSinceHunt*otUse)
          
          # off-trail recreation only, canopy
          d141 <- update(envtDay, . ~ . + otUse + tSinceHunt*otUse + tSinceHunt*canSt)          
          
          # off-trail and structures, linear
          d142 <- update(envtDay, . ~ . + otUse + distStrucSt + tSinceHunt*otUse + tSinceHunt*distStrucSt)
          
          # off-trail and structures, linear, canopy
          d143 <- update(envtDay, . ~ . + otUse + distStrucSt + tSinceHunt*otUse + tSinceHunt*distStrucSt + tSinceHunt*canSt)          
          
          # offtrail and structures, quadratic
          d144 <- update(envtDay, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + tSinceHunt*otUse + tSinceHunt*distStrucSt + tSinceHunt*I(distStrucSt^2))
          
          # offtrail and structures, quadratic, canopy
          d145 <- update(envtDay, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + tSinceHunt*otUse + tSinceHunt*distStrucSt + tSinceHunt*I(distStrucSt^2) + tSinceHunt*canSt)          
          
          
    #### change use of landscape but not response to people ####       
          
          # all environment
          d146 <- update(envtDay, . ~ . + tSinceHunt:lcClass + tSinceHunt*can + tSinceHunt*slope + tSinceHunt*elev + tSinceHunt*northness
                       + tSinceHunt*snowSt + tSinceHunt*I(slope^2) + tSinceHunt:I(elev^2) + tSinceHunt:I(northness^2))
          
          
          # canopy (i.e., hiding cover/escape terrain) only
          d147 <- update(envtDay, . ~ . + tSinceHunt*canSt)  
          
          
  #### competition ####
      
      # create list to store current subset of models in
      modsDay <- list()
      # list model names
      modNamesDay <- ls(envir = .GlobalEnv, pattern = "^d[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesDay)) { modsDay[[i]] <- get(modNamesDay[i]) }
      # create a dataframe of aicc results...
      aicDay <- data.frame(aictab(cand.set = modsDay, modnames = modNamesDay))
      # ...sorted from smallest to largest aicc value...
      # ... and store
      write.csv(aicDay, file = "aicDayTSMC.csv", row.names = FALSE)      
      aicDay <- aicDay[order(aicDay$Delta_AICc), ]
      # identify best-supported models (deltaAICc < 2)
      aic2Day <- subset(aicDay, Delta_AICc < 2.0)
      aic2Day <- droplevels(aic2Day)
      aic2Day$Modnames <- as.character(aic2Day$Modnames)
      # save top-supported models
      topTSMCs <- list()
      # list model names
      topTSMCnames <- unique(aic2Day$Modnames)
      # store the models in the list
      for (i in 1:length(topTSMCnames)) { topTSMCs[[i]] <- get(topTSMCnames[i]) }      
      # delete unsupported models and clear memory for next batch
      rm(list = ls(pattern = "^d[0:9]*"))
      gc()      
             
      
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#### | THREAT SHIELD MODELS - Continuous time since hunt | ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  
          
      
      #### all human influences ####
      
          # linear, predictable feedgrounds
          d148 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*recClass + tContHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d149 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*recClass + tContHunt*activeFeedSt
                       + tContHunt*canSt)          
      
          # quadratic, predictable feedgrounds
          d150 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  tContHunt*distRdSt +  tContHunt*distStrucSt +  tContHunt*recClass +  tContHunt*activeFeedSt
                       +  tContHunt*I(distRdSt^2) +  tContHunt*I(distStrucSt^2) +  tContHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          d151 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  tContHunt*distRdSt +  tContHunt*distStrucSt +  tContHunt*recClass +  tContHunt*activeFeedSt
                       +  tContHunt*I(distRdSt^2) +  tContHunt*I(distStrucSt^2) +  tContHunt*I(activeFeedSt^2)
                       + tContHunt*canSt)
      
          # linear, all feedgrounds
          d152 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*recClass + tContHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d153 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*recClass + tContHunt*distFeedSt
                       + tContHunt*canSt)          
      
          # quadratic, all feedgrounds
          d154 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*recClass + tContHunt*distFeedSt
                       + tContHunt*I(distRdSt^2) + tContHunt*I(distStrucSt^2) + tContHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d155 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*recClass + tContHunt*distFeedSt
                       + tContHunt*I(distRdSt^2) + tContHunt*I(distStrucSt^2) + tContHunt*I(distFeedSt^2)
                       + tContHunt*canSt)          
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          d156 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d157 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*activeFeedSt + tContHunt*canSt)          
          

          # quadratic, predictable feedgrounds
          d158 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*activeFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          d159 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*activeFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(activeFeedSt^2)
                       + tContHunt*canSt)

          # linear, all feedgrounds
          d160 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d161 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*distFeedSt + tContHunt*canSt)          
      
          # quadratic, all feedgrounds
          d162 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*distFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d163 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*distFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(distFeedSt^2)
                       + tContHunt*canSt)
                    
      
      
      
      #### motorized recreation, and all others ####   
          
          
          # linear, predictable feedgrounds
          d164 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoUse + tContHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d165 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoUse + tContHunt*activeFeedSt
                       + tContHunt*canSt)          
      
          # quadratic, predictable feedgrounds
          d166 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoUse + tContHunt*activeFeedSt
                       + tContHunt*I(distRdSt^2) + tContHunt*I(distStrucSt^2) + tContHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          d167 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoUse + tContHunt*activeFeedSt
                       + tContHunt*I(distRdSt^2) + tContHunt*I(distStrucSt^2) + tContHunt*I(activeFeedSt^2)
                       + tContHunt*canSt)
      
          # linear, all feedgrounds
          d168 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoUse + tContHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d169 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoUse + tContHunt*distFeedSt
                       + tContHunt*canSt)          
      
          # quadratic, all feedgrounds
          d170 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoUse + tContHunt*distFeedSt
                       + tContHunt*I(distRdSt^2) + tContHunt*I(distStrucSt^2) + tContHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d171 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoUse + tContHunt*distFeedSt
                       + tContHunt*I(distRdSt^2) + tContHunt*I(distStrucSt^2) + tContHunt*I(distFeedSt^2)
                       + tContHunt*canSt)          
          
          
          
      #### motorized activity only ####   
          
          # linear, predictable feedgrounds
          d172 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*motoUse + tContHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d173 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*motoUse + tContHunt*activeFeedSt + tContHunt*canSt)          
      
          # quadratic, predictable feedgrounds
          d174 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*motoUse + tContHunt*activeFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(activeFeedSt^2))

          # quadratic, predictable feedgrounds, canopy
          d175 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*motoUse + tContHunt*activeFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(activeFeedSt^2)
                       + tContHunt*canSt)
      
          # linear, all feedgrounds
          d176 <- update(envtDay, . ~ . + distRdSt + motoUse + distFeedSt
                       + tContHunt*distRdSt + tContHunt*motoUse + tContHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d177 <- update(envtDay, . ~ . + distRdSt + motoUse + distFeedSt
                       + tContHunt*distRdSt + tContHunt*motoUse + tContHunt*distFeedSt + tContHunt*canSt)          
      
          # quadratic, all feedgrounds
          d178 <- update(envtDay, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*motoUse + tContHunt*distFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(distFeedSt^2)) 
          
          # quadratic, all feedgrounds, canopy
          d179 <- update(envtDay, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*motoUse + tContHunt*distFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(distFeedSt^2)
                       + tContHunt*canSt) 
      

        
      #### off-trail recreation - alone; with or without structures ####  
          
          # off-trail recreation only
          d180 <- update(envtDay, . ~ . + otUse + tContHunt*otUse)
          
          # off-trail recreation only, canopy
          d181 <- update(envtDay, . ~ . + otUse + tContHunt*otUse + tContHunt*canSt)          
          
          # off-trail and structures, linear
          d182 <- update(envtDay, . ~ . + otUse + distStrucSt + tContHunt*otUse + tContHunt*distStrucSt)
          
          # off-trail and structures, linear, canopy
          d183 <- update(envtDay, . ~ . + otUse + distStrucSt + tContHunt*otUse + tContHunt*distStrucSt + tContHunt*canSt)          
          
          # offtrail and structures, quadratic
          d184 <- update(envtDay, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + tContHunt*otUse + tContHunt*distStrucSt + tContHunt*I(distStrucSt^2))
          
          # offtrail and structures, quadratic, canopy
          d185 <- update(envtDay, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + tContHunt*otUse + tContHunt*distStrucSt + tContHunt*I(distStrucSt^2) + tContHunt*canSt)          
          
          
    #### change use of landscape but not response to people ####       
          
          # all environment
          d186 <- update(envtDay, . ~ . + tContHunt:lcClass + tContHunt*can + tContHunt*slope + tContHunt*elev + tContHunt*northness
                       + tContHunt*snowSt + tContHunt*I(slope^2) + tContHunt:I(elev^2) + tContHunt:I(northness^2))
          
          
          # canopy (i.e., hiding cover/escape terrain) only
          d187 <- update(envtDay, . ~ . + tContHunt*canSt)        
          
  #### competition ####
      
      # create list to store current subset of models in
      modsDay <- list()
      # list model names
      modNamesDay <- ls(envir = .GlobalEnv, pattern = "^d[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesDay)) { modsDay[[i]] <- get(modNamesDay[i]) }
      # create a dataframe of aicc results...
      aicDay <- data.frame(aictab(cand.set = modsDay, modnames = modNamesDay))
      # ...sorted from smallest to largest aicc value...
      aicDay <- aicDay[order(aicDay$Delta_AICc), ]
      # ... and store
      write.csv(aicDay, file = "aicDayTSMD.csv", row.names = FALSE)      
      # identify best-supported models (deltaAICc < 2)
      aic2Day <- subset(aicDay, Delta_AICc < 2.0)
      aic2Day <- droplevels(aic2Day)
      aic2Day$Modnames <- as.character(aic2Day$Modnames)
      # save top-supported models
      topTSMDs <- list()
      # list model names
      topTSMDnames <- unique(aic2Day$Modnames)
      # store the models in the list
      for (i in 1:length(topTSMDnames)) { topTSMDs[[i]] <- get(topTSMDnames[i]) }      
      # delete unsupported models and clear memory for next batch
      rm(list = ls(pattern = "^d[0:9]*"))
      gc()      
             
      
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ##
#### | RESOURCE SUBSIDY MODELS |  ####
### ### ### ### ### ### ### ### ### ##    
      
      # all feedgrounds, linear
      d188 <- update(envtDay, . ~ . + distFeedSt)
      
      # all feedgrounds, quadratic
      d189 <- update(envtDay, . ~ . + distFeedSt + I(distFeedSt^2))
      
      # active feedgrounds, linear
      d190 <- update(envtDay, . ~ . + activeFeedSt)
      
      # active feedgrounds, quadratic
      d191 <- update(envtDay, . ~ . + activeFeedSt + I(activeFeedSt^2))
      
      
  #### competition ####
      
      # create list to store current subset of models in
      modsDay <- list()
      # list model names
      modNamesDay <- ls(envir = .GlobalEnv, pattern = "^d[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesDay)) { modsDay[[i]] <- get(modNamesDay[i]) }
      # create a dataframe of aicc results...
      aicDay <- data.frame(aictab(cand.set = modsDay, modnames = modNamesDay))
      # ...sorted from smallest to largest aicc value...
      aicDay <- aicDay[order(aicDay$Delta_AICc), ]
      # ... and store
      write.csv(aicDay, file = "aicDayRSM.csv", row.names = FALSE)      
      # identify best-supported models (deltaAICc < 2)
      aic2Day <- subset(aicDay, Delta_AICc < 2.0)
      aic2Day <- droplevels(aic2Day)
      aic2Day$Modnames <- as.character(aic2Day$Modnames)
      # save top-supported models
      topRSMs <- list()
      # list model names
      topRSMnames <- unique(aic2Day$Modnames)
      # store the models in the list
      for (i in 1:length(topRSMnames)) { topRSMs[[i]] <- get(topRSMnames[i]) }      
      # delete unsupported models and clear memory for next batch
      rm(list = ls(pattern = "^d[0:9]*"))
      gc()      
         
      
################################################################################################## #  

    
### ### ### ### ### ### ### ### 
#### | MODEL COMPARISON |  ####
### ### ### ### ### ### ### ###      
      
      
      # list all supported models and their names
      topMods <- c(topLSMs, topDSMs, topTSMAs, topTSMBs, topTSMCs, topTSMDs, topRSMs, envtDay)
      names(topMods) <- c(topLSMnames, topDSMnames, topTSMAnames, topTSMBnames,
                          topTSMCnames, topTSMDnames, topRSMnames, "envtDay")
      
      # dataframe of aicc results...
      aicAll <- data.frame(aictab(cand.set = topMods, modnames = names(topMods)))
      
      # ...sorted from smallest to largest aicc value...
      aicAll <- aicAll[order(aicAll$Delta_AICc), ]
      
      # .. and exported
      write.csv(aicAll, "aic-day.csv", row.names=F)
      
      # store and export subset of moderately-supported models (deltaAICc < 4)
      aic4All <- subset(aicAll, Delta_AICc < 4.0); aic4All <- droplevels(aic4All)
      write.csv(aic4All, "aic4-day.csv", row.names=F)
      
      # store and export subset of best-supported models (deltaAICc < 2)
      aic2All <- subset(aicAll, Delta_AICc < 2.0); aic2All <- droplevels(aic2All)
      write.csv(aic2All, "aic2-day.csv", row.names=F)  
      
      # view top-supported models
      topAllMat <- matrix(aic2All$Modname)
      # print summaries of all supported models 
      apply(topAllMat, 1, get)
      # store the top model
      topDay <- apply(topAllMat, 1, get)[[1]]
      
      
      # check bic too, just to be thorough
      
        # dataframe of bic results...
        bicAll <- data.frame(bictab(cand.set = topMods, modnames = names(topMods)))
        # ...sorted from smallest to largest bicc value...
        bicAll <- bicAll[order(bicAll$Delta_BIC), ]
        # .. and exported
        write.csv(bicAll, "bic-day.csv", row.names=F)      
      
################################################################################################## #  

      
      save.image("modelsHumanDay.RData") 
      
################################################################################################## # 
      
    
### ### ### ### ### ### ### ### ###
#### | TOP MODEL DIAGNOSTICS|  ####
### ### ### ### ### ### ### ### ###   
      
      summary(topDay)
    
      ## area under the roc curve ##
      invisible(plot(roc(factor(ifelse(modDatDay$Used == 1, 1, 0)), fitted(topDay)), 
                     print.thres = c(.1, .5), col = "red", print.auc = T)) # auc = 0.734
      
      ## predictive accuracy @ >50% ##  
      confusionMatrix(factor(as.character(ifelse(fitted(topDay) > 0.5, "Yes", "No"))), 
                      factor(ifelse(modDatDay$Used == 1, "Yes", "No")), positive = "Yes") # 68% 
    
      
      ## binned residual plots ##
      binnedplot(fitted(topDay), residuals(topDay, type = "response"), main = "Day - top model")
      
      
################################################################################################## # 
      
    
### ### ### ### ### ### ### ### ##
#### | VISUALIZING RESULTS |  ####
### ### ### ### ### ### ### ### ##  
      
      
      
      #### create dataframe of things to plot ####
        
        d <- modDatDay %>%
          mutate(
            prWolf = fitted(topDay),
            model = "Day",
            prevHunt = as.factor(prevHunt))

      
      
      
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
        
        
        
      #### odds ratios: categorical covariates (recreation) ####
        
        # dataframe of odds ratios and confidence intervals 
        # KRISTIN delete Wald for final product (it's fast but inaccurate)
        dDay <- round(exp(data.frame(
          OR = fixef(topDay),
          ciLow = confint(topDay, parm = "beta_", method = "Wald")[,1],
          ciHigh = confint(topDay, parm = "beta_", method = "Wald")[,2])), 3)
        dDay$Covariate = rownames(dDay)
        
        # create recreation-specific dataframe
        recDay <- dDay %>%
          filter(grepl("rec", Covariate)) %>%
          mutate(prevHunt = as.factor(ifelse(grepl("prevHunt", Covariate) == T, 1, 0)),
                 recType = ifelse(grepl("noOT", Covariate) == T, "No off-trail",
                                  ifelse(grepl("allOT", Covariate) == T, "All off-trail",
                                         "Non-moto off-trail"))) %>%
          mutate(recType = factor(recType, levels = c("All off-trail", "Non-moto off-trail", "No off-trail")))


        # plot OR +- 95%CI colored by hunt previous year
        pLc <- ggplot(recDay, 
                      aes(x = recType, y = OR, colour = prevHunt)) +
          geom_errorbar(aes(ymin = ciLow, ymax = ciHigh), width = 0.1) +
          geom_point() +
          geom_hline(aes(yintercept=1)) +
          labs(title = "Recreation", x = "(relative to private land)") #+
         # scale_x_discrete(labels=c("Herbaceous","NoVeg","Riparian","Shrub"))
      
      
################################################################################################## # 
      
    
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#### | RE-RUNNING MODEL WITH DIFFERENT RECREATION BASELINES |  ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        
        
    ## winter closure areas as baseline ##
        
        
        # reorder factor and rerun model
        modDatDay2 <- modDatDay %>%
          mutate(recClass = factor(recClass, levels = c("noOT", "noRec", "nomotoOT", "allOT")))
        topDay2 <- update(topDay, data = modDatDay2)
        summary(topDay2)
        
        # new dataframe of model results
        dDay2 <- round(exp(data.frame(
          OR = fixef(topDay2),
          ciLow = confint(topDay2, parm = "beta_", method = "Wald")[,1],
          ciHigh = confint(topDay2, parm = "beta_", method = "Wald")[,2])), 3)
        dDay2$Covariate = rownames(dDay2)
        
        # create recreation-specific dataframe
        recDay2 <- dDay2 %>%
          filter(grepl("rec", Covariate)) %>%
          mutate(prevHunt = as.factor(ifelse(grepl("prevHunt", Covariate) == T, 1, 0)),
                 recType = ifelse(grepl("noRec", Covariate) == T, "No recreation",
                                  ifelse(grepl("allOT", Covariate) == T, "All off-trail",
                                         "Non-moto off-trail"))) %>%
          mutate(recType = factor(recType, levels = c("All off-trail", "Non-moto off-trail", "No recreation")))


        # plot OR +- 95%CI colored by hunt previous year
        pLc2 <- ggplot(recDay2, 
                      aes(x = recType, y = OR, colour = prevHunt)) +
          geom_errorbar(aes(ymin = ciLow, ymax = ciHigh), width = 0.1) +
          geom_point() +
          geom_hline(aes(yintercept=1)) +
          labs(title = "Recreation", x = "(relative to areas closed to off-trail recreation)") 
        pLc2
  
              
        
    ## open recreation areas as baseline ##
        
        
        # reorder factor and rerun model
        modDatDay3 <- modDatDay %>%
          mutate(recClass = factor(recClass, levels = c("allOT", "nomotoOT", "noOT", "noRec")))
        topDay3 <- update(topDay, data = modDatDay3)
        summary(topDay3)
        
        # new dataframe of model results
        dDay3 <- round(exp(data.frame(
          OR = fixef(topDay3),
          ciLow = confint(topDay3, parm = "beta_", method = "Wald")[,1],
          ciHigh = confint(topDay3, parm = "beta_", method = "Wald")[,2])), 3)
        dDay3$Covariate = rownames(dDay3)
        
        # create recreation-specific dataframe
        recDay3 <- dDay3 %>%
          filter(grepl("rec", Covariate)) %>%
          mutate(prevHunt = as.factor(ifelse(grepl("prevHunt", Covariate) == T, "Yes", "No")),
                 recType = ifelse(grepl("noRec", Covariate) == T, "No recreation",
                                  ifelse(grepl("noOT", Covariate) == T, "No off-trail",
                                         "Non-moto off-trail"))) %>%
          mutate(recType = factor(recType, levels = c("Non-moto off-trail", "No off-trail", "No recreation")))


        # plot OR +- 95%CI colored by hunt previous year
        pLc3 <- ggplot(recDay3, aes(x = recType, y = OR, colour = prevHunt)) +
          geom_errorbar(aes(ymin = ciLow, ymax = ciHigh), width = 0.1) +
          geom_point() +
          geom_hline(aes(yintercept=1)) +
          labs(title = "Recreation", x = "(Relative to open recreation areas)", 
               color = "Hunted \nprevious \nyear") +
          scale_x_discrete(labels=c("Non-motorized use \nallowed off-trail",
                                    "Winter closure, \n off-trail prohibited",
                                    "No recreation \n(private land)")) 
        pLc3        