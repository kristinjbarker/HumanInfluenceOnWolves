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
      # use open recreation as baseline; reorder to match order in daytime models
      recClass = factor(recClass, levels = c("allOT", "nomotoOT", "noOT", "noRec")),
      # add binary private land designation
      pvt = ifelse(recClass == "noRec", 1, 0),
      # add binary off-trail/no off-trail use designation
      otUse = ifelse(recClass == "noOT", 0, 1),
      # add binary designation for whether moto rec is allowed
      motoUse = ifelse(recClass == "allOT", 1, 0),
      # add binary indicator of whether hunting was allowed that fall
      hunt = ifelse(Year == 2013 | Year == 2014 | Year >= 2018, 1, 0),
      # add binary indication of whether hunting was allowed in the previous year
      prevHunt = ifelse(Year == 2014 | Year == 2015 | Year >= 2019, 1, 0),
      # add time since hunting was first allowed (fall 2012, corresponds to winter 2013)
      tSinceHunt = ifelse(Year - 2013 + 1 < 0, 0, Year - 2013 + 1),
      # add years of continuous hunting (to account for no hunting 2014, 2015, 2016)
      tContHunt = ifelse(Year == 2013, 1,
                         ifelse(Year == 2014, 2,
                                ifelse(Year >= 2018, Year - 2018 + 1, 0))))


    #### split night/night ####
    
    modDatNight <- filter(modDat, daytime == "night")

    
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### #
####   | ENVIRONMENTAL MODELS |  ####
### ### ### ### ### ### ### ### ### #
    
    
    # nighttime (from dataExploration.R)
    envtNight <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                     + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                     + snowSt:canSt + snowSt:slopeSt + snowSt:elevSt
                     + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) 
                     + (1|Pack/wolfYr), 
                      family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                     data = modDatNight) 

                  
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### #### #
#### | LANDSCAPE SHIELD MODELS |  ####
### ### ### ### ### ### ### ### #### #

    #### models ####      

      # roads & buildings, linear
      d1 <- update(envtNight, . ~ . + distRdSt + distStrucSt)

      # roads & buildings, quadratic
      d2 <- update(envtNight, . ~ . + distRdSt + distStrucSt + I(distRdSt^2) + I(distStrucSt^2))

      # buildings only, linear
      d3 <- update(envtNight, . ~ . + distStrucSt)

      # buildings only, quadratic
      d4 <- update(envtNight, . ~ . + distStrucSt + I(distStrucSt^2))

  #### competition ####
      
      # create list to store current subset of models in
      modsNight <- list()
      # list model names
      modNamesNight <- ls(envir = .GlobalEnv, pattern = "^d[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesNight)) { modsNight[[i]] <- get(modNamesNight[i]) }
      # create a dataframe of aicc results...
      aicNight <- data.frame(aictab(cand.set = modsNight, modnames = modNamesNight))
      # ...sorted from smallest to largest aicc value...
      aicNight <- aicNight[order(aicNight$Delta_AICc), ]
      # ... and store
      write.csv(aicNight, file = "aicNightLSM.csv", row.names = FALSE)      
      # identify best-supported models (deltaAICc < 2)
      aic2Night <- subset(aicNight, Delta_AICc < 2.0)
      aic2Night <- droplevels(aic2Night)
      aic2Night$Modnames <- as.character(aic2Night$Modnames)
      # save top-supported models
      topLSMs <- list()
      # list model names
      topLSMnames <- unique(aic2Night$Modnames)
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
          d5 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          d6 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          d7 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt)
      
          # quadratic, all feedgrounds
          d8 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2))
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          d9 <- update(envtNight, . ~ . + distRdSt + recClass + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          d10 <- update(envtNight, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          d11 <- update(envtNight, . ~ . + distRdSt + recClass + distFeedSt)
      
          # quadratic, all feedgrounds
          d12 <- update(envtNight, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2))
                    
      
      
      
      #### motorized recreation, and all others ####   
          
          # linear, predictable feedgrounds
          d13 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          d14 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          d15 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt)
      
          # quadratic, all feedgrounds
          d16 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2))
          
          
          
      #### motorized activity only ####   
          
          # linear, predictable feedgrounds
          d17 <- update(envtNight, . ~ . + distRdSt + motoUse + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          d18 <- update(envtNight, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          d19 <- update(envtNight, . ~ . + distRdSt + motoUse + distFeedSt)
      
          # quadratic, all feedgrounds
          d20 <- update(envtNight, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2))      
      

      
      
      #### living and working activity, not recreation per se ####      
      
          # linear, predictable feedgrounds
          d21 <- update(envtNight, . ~ . + distRdSt + distStrucSt + pvt + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          d22 <- update(envtNight, . ~ . + distRdSt + distStrucSt + pvt + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          d23 <- update(envtNight, . ~ . + distRdSt + distStrucSt + pvt + distFeedSt)
      
          # quadratic, all feedgrounds
          d24 <- update(envtNight, . ~ . + distRdSt + distStrucSt + pvt + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2))
          
          
      
      #### off-trail recreation - alone; with or without structures ####  
          
          # off-trail recreation only
          d25 <- update(envtNight, . ~ . + otUse)
          
          # off-trail and structures, linear
          d26 <- update(envtNight, . ~ . + otUse + distStrucSt)
          
          # offtrail and structures, quadratic
          d27 <- update(envtNight, . ~ . + otUse + distStrucSt + I(distStrucSt^2))

            
      
      

  #### competition ####
      
      # create list to store current subset of models in
      modsNight <- list()
      # list model names
      modNamesNight <- ls(envir = .GlobalEnv, pattern = "^d[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesNight)) { modsNight[[i]] <- get(modNamesNight[i]) }
      # create a dataframe of aicc results...
      aicNight <- data.frame(aictab(cand.set = modsNight, modnames = modNamesNight))
      # ...sorted from smallest to largest aicc value...
      aicNight <- aicNight[order(aicNight$Delta_AICc), ]
      # ... and store
      write.csv(aicNight, file = "aicNightDSM.csv", row.names = FALSE)
      # identify best-supported models (deltaAICc < 2)
      aic2Night <- subset(aicNight, Delta_AICc < 2.0)
      aic2Night <- droplevels(aic2Night)
      aic2Night$Modnames <- as.character(aic2Night$Modnames)
      # save top-supported models
      topDSMs <- list()
      # list model names
      topDSMnames <- unique(aic2Night$Modnames)
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
          d28 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*activeFeedSt - hunt)
          
          # linear, predictable feedgrounds, canopy
          d29 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*activeFeedSt
                       + hunt*canSt - hunt)          
      
          # quadratic, predictable feedgrounds
          d30 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  hunt*distRdSt +  hunt*distStrucSt +  hunt*recClass +  hunt*activeFeedSt
                       +  hunt*I(distRdSt^2) +  hunt*I(distStrucSt^2) +  hunt*I(activeFeedSt^2) - hunt)

          # quadratic, predictable feedgrounds, canopy
          d31 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  hunt*distRdSt +  hunt*distStrucSt +  hunt*recClass +  hunt*activeFeedSt
                       +  hunt*I(distRdSt^2) +  hunt*I(distStrucSt^2) +  hunt*I(activeFeedSt^2)
                       + hunt*canSt - hunt)
      
          # linear, all feedgrounds
          d32 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt - hunt)
          
          # linear, all feedgrounds, canopy
          d33 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt
                       + hunt*canSt - hunt)          
      
          # quadratic, all feedgrounds
          d34 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2) - hunt)
          
          # quadratic, all feedgrounds, canopy
          d35 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2)
                       + hunt*canSt - hunt)          
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          d36 <- update(envtNight, . ~ . + distRdSt + recClass + activeFeedSt
                       + hunt*distRdSt + hunt*recClass + hunt*activeFeedSt - hunt)
          
          # linear, predictable feedgrounds, canopy
          d37 <- update(envtNight, . ~ . + distRdSt + recClass + activeFeedSt
                       + hunt*distRdSt + hunt*recClass + hunt*activeFeedSt + hunt*canSt - hunt)          
          
      
          # quadratic, predictable feedgrounds
          d38 <- update(envtNight, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*recClass + hunt*activeFeedSt + hunt*I(distRdSt^2) 
                       + hunt*I(activeFeedSt^2) - hunt)

          # quadratic, predictable feedgrounds, canopy
          d39 <- update(envtNight, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*recClass + hunt*activeFeedSt + hunt*I(distRdSt^2) 
                       + hunt*I(activeFeedSt^2)
                       + hunt*canSt - hunt)
      
          # linear, all feedgrounds
          d40 <- update(envtNight, . ~ . + distRdSt + recClass + distFeedSt
                       + hunt*distRdSt + hunt*recClass + hunt*distFeedSt - hunt)
          
          # linear, all feedgrounds, canopy
          d41 <- update(envtNight, . ~ . + distRdSt + recClass + distFeedSt
                       + hunt*distRdSt + hunt*recClass + hunt*distFeedSt + hunt*canSt - hunt)          
      
          # quadratic, all feedgrounds
          d42 <- update(envtNight, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*recClass + hunt*distFeedSt + hunt*I(distRdSt^2) 
                       + hunt*I(distFeedSt^2) - hunt)
          
          # quadratic, all feedgrounds, canopy
          d43 <- update(envtNight, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*recClass + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2)
                       + hunt*canSt - hunt)
                    
      
      
      
      #### motorized recreation, and all others ####   
          
          
          # linear, predictable feedgrounds
          d44 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*activeFeedSt - hunt)
          
          # linear, predictable feedgrounds, canopy
          d45 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*activeFeedSt
                       + hunt*canSt - hunt)          
      
          # quadratic, predictable feedgrounds
          d46 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*activeFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(activeFeedSt^2) - hunt)

          # quadratic, predictable feedgrounds, canopy
          d47 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*activeFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(activeFeedSt^2)
                       + hunt*canSt - hunt)
      
          # linear, all feedgrounds
          d48 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*distFeedSt - hunt)
          
          # linear, all feedgrounds, canopy
          d49 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*distFeedSt
                       + hunt*canSt - hunt)          
      
          # quadratic, all feedgrounds
          d50 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2) - hunt)
          
          # quadratic, all feedgrounds, canopy
          d51 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2)
                       + hunt*canSt - hunt)          
          
          
          
      #### motorized activity only ####   
          
          # linear, predictable feedgrounds
          d52 <- update(envtNight, . ~ . + distRdSt + motoUse + activeFeedSt
                       + hunt*distRdSt + hunt*motoUse + hunt*activeFeedSt - hunt)
          
          # linear, predictable feedgrounds, canopy
          d53 <- update(envtNight, . ~ . + distRdSt + motoUse + activeFeedSt
                       + hunt*distRdSt + hunt*motoUse + hunt*activeFeedSt + hunt*canSt - hunt)          
      
          # quadratic, predictable feedgrounds
          d54 <- update(envtNight, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*motoUse + hunt*activeFeedSt + hunt*I(distRdSt^2) 
                       + hunt*I(activeFeedSt^2) - hunt)

          # quadratic, predictable feedgrounds, canopy
          d55 <- update(envtNight, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*motoUse + hunt*activeFeedSt + hunt*I(distRdSt^2) + hunt*I(activeFeedSt^2)
                       + hunt*canSt - hunt)
      
          # linear, all feedgrounds
          d56 <- update(envtNight, . ~ . + distRdSt + motoUse + distFeedSt
                       + hunt*distRdSt + hunt*motoUse + hunt*distFeedSt - hunt)
          
          # linear, all feedgrounds, canopy
          d57 <- update(envtNight, . ~ . + distRdSt + motoUse + distFeedSt
                       + hunt*distRdSt + hunt*motoUse + hunt*distFeedSt + hunt*canSt - hunt)          
      
          # quadratic, all feedgrounds
          d58 <- update(envtNight, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*motoUse + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2) - hunt) 
          
          # quadratic, all feedgrounds, canopy
          d59 <- update(envtNight, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*motoUse + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2 - hunt)
                       + hunt*canSt) 
      

        
      #### off-trail recreation - alone; with or without structures ####  
          
          # off-trail recreation only
          d60 <- update(envtNight, . ~ . + otUse + hunt*otUse - hunt)
          
          # off-trail recreation only, canopy
          d61 <- update(envtNight, . ~ . + otUse + hunt*otUse + hunt*canSt - hunt)          
          
          # off-trail and structures, linear
          d62 <- update(envtNight, . ~ . + otUse + distStrucSt + hunt*otUse + hunt*distStrucSt - hunt)
          
          # off-trail and structures, linear, canopy
          d63 <- update(envtNight, . ~ . + otUse + distStrucSt + hunt*otUse + hunt*distStrucSt + hunt*canSt - hunt)          
          
          # offtrail and structures, quadratic
          d64 <- update(envtNight, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + hunt*otUse + hunt*distStrucSt + hunt*I(distStrucSt^2) - hunt)
          
          # offtrail and structures, quadratic, canopy
          d65 <- update(envtNight, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + hunt*otUse + hunt*distStrucSt + hunt*I(distStrucSt^2) + hunt*canSt - hunt)          
          
          
    #### change use of landscape but not response to people ####       
          
          # all environment
          d66 <- update(envtNight, . ~ . + hunt:lcClass + hunt*can + hunt*slope + hunt*elev + hunt*northness
                       + hunt*snowSt + hunt*I(slope^2) + hunt:I(elev^2) + hunt:I(northness^2) - hunt)
          
          
          # canopy (i.e., hiding cover/escape terrain) only
          d67 <- update(envtNight, . ~ . + hunt*canSt - hunt)
      

  #### competition ####
      
      # create list to store current subset of models in
      modsNight <- list()
      # list model names
      modNamesNight <- ls(envir = .GlobalEnv, pattern = "^d[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesNight)) { modsNight[[i]] <- get(modNamesNight[i]) }
      # create a dataframe of aicc results...
      aicNight <- data.frame(aictab(cand.set = modsNight, modnames = modNamesNight))
      # ...sorted from smallest to largest aicc value...
      aicNight <- aicNight[order(aicNight$Delta_AICc), ]
      # ... and store
      write.csv(aicNight, file = "aicNightTSMA.csv", row.names = FALSE)      
      # identify best-supported models (deltaAICc < 2)
      aic2Night <- subset(aicNight, Delta_AICc < 2.0)
      aic2Night <- droplevels(aic2Night)
      aic2Night$Modnames <- as.character(aic2Night$Modnames)
      # save top-supported models
      topTSMAs <- list()
      # list model names
      topTSMAnames <- unique(aic2Night$Modnames)
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
          d68 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*activeFeedSt - prevHunt)
          
          # linear, predictable feedgrounds, canopy
          d69 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*activeFeedSt
                       + prevHunt*canSt - prevHunt)
          
          # quadratic, predictable feedgrounds
          d70 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  prevHunt*distRdSt +  prevHunt*distStrucSt +  prevHunt*recClass +  prevHunt*activeFeedSt
                       +  prevHunt*I(distRdSt^2) +  prevHunt*I(distStrucSt^2) +  prevHunt*I(activeFeedSt^2) - prevHunt)

          # quadratic, predictable feedgrounds, canopy
          d71 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  prevHunt*distRdSt +  prevHunt*distStrucSt +  prevHunt*recClass +  prevHunt*activeFeedSt
                       +  prevHunt*I(distRdSt^2) +  prevHunt*I(distStrucSt^2) +  prevHunt*I(activeFeedSt^2)
                       + prevHunt*canSt - prevHunt)
          
          # linear, all feedgrounds
          d72 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*distFeedSt - prevHunt)
          
          # linear, all feedgrounds, canopy
          d73 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*distFeedSt
                       + prevHunt*canSt - prevHunt)          
      
          # quadratic, all feedgrounds
          d74 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*distFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2) - prevHunt)
          
          # quadratic, all feedgrounds, canopy
          d75 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*distFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2)
                       + prevHunt*canSt - prevHunt)          
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          d76 <- update(envtNight, . ~ . + distRdSt + recClass + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*activeFeedSt - prevHunt)
          
          # linear, predictable feedgrounds, canopy
          d77 <- update(envtNight, . ~ . + distRdSt + recClass + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*activeFeedSt + prevHunt*canSt - prevHunt)          
          

          # quadratic, predictable feedgrounds
          d78 <- update(envtNight, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(activeFeedSt^2) - prevHunt)

          # quadratic, predictable feedgrounds, canopy
          d79 <- update(envtNight, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(activeFeedSt^2)
                       + prevHunt*canSt - prevHunt)
      
          # linear, all feedgrounds
          d80 <- update(envtNight, . ~ . + distRdSt + recClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*distFeedSt - prevHunt)
          
          # linear, all feedgrounds, canopy
          d81 <- update(envtNight, . ~ . + distRdSt + recClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*distFeedSt + prevHunt*canSt - prevHunt)          
      
          # quadratic, all feedgrounds
          d82 <- update(envtNight, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(distFeedSt^2) - prevHunt)
          
          # quadratic, all feedgrounds, canopy
          d83 <- update(envtNight, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(distFeedSt^2)
                       + prevHunt*canSt - prevHunt)
                    
      
      
      
      #### motorized recreation, and all others ####   
          
          
          # linear, predictable feedgrounds
          d84 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*activeFeedSt - prevHunt)
          
          # linear, predictable feedgrounds, canopy
          d85 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*activeFeedSt
                       + prevHunt*canSt - prevHunt)          
      
          # quadratic, predictable feedgrounds
          d86 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*activeFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(activeFeedSt^2) - prevHunt)

          # quadratic, predictable feedgrounds, canopy
          d87 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*activeFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(activeFeedSt^2)
                       + prevHunt*canSt - prevHunt)
      
          # linear, all feedgrounds
          d88 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*distFeedSt - prevHunt)
          
          # linear, all feedgrounds, canopy
          d89 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*distFeedSt
                       + prevHunt*canSt - prevHunt)          
      
          # quadratic, all feedgrounds
          d90 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*distFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2) - prevHunt)
          
          # quadratic, all feedgrounds, canopy
          d91 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*distFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2)
                       + prevHunt*canSt - prevHunt)          
          
          
          
      #### motorized activity only ####   
          
          # linear, predictable feedgrounds
          d92 <- update(envtNight, . ~ . + distRdSt + motoUse + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*activeFeedSt - prevHunt)
          
          # linear, predictable feedgrounds, canopy
          d93 <- update(envtNight, . ~ . + distRdSt + motoUse + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*activeFeedSt + prevHunt*canSt - prevHunt)          
      
          # quadratic, predictable feedgrounds
          d94 <- update(envtNight, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(activeFeedSt^2) - prevHunt)

          # quadratic, predictable feedgrounds, canopy
          d95 <- update(envtNight, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(activeFeedSt^2)
                       + prevHunt*canSt - prevHunt)
      
          # linear, all feedgrounds
          d96 <- update(envtNight, . ~ . + distRdSt + motoUse + distFeedSt
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*distFeedSt - prevHunt)
          
          # linear, all feedgrounds, canopy
          d97 <- update(envtNight, . ~ . + distRdSt + motoUse + distFeedSt
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*distFeedSt + prevHunt*canSt - prevHunt)          
      
          # quadratic, all feedgrounds
          d98 <- update(envtNight, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(distFeedSt^2) - prevHunt) 
          
          # quadratic, all feedgrounds, canopy
          d99 <- update(envtNight, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*motoUse + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(distFeedSt^2)
                       + prevHunt*canSt - prevHunt) 
      

        
      #### off-trail recreation - alone; with or without structures ####  
          
          # off-trail recreation only
          d100 <- update(envtNight, . ~ . + otUse + prevHunt*otUse - prevHunt)
          
          # off-trail recreation only, canopy
          d101 <- update(envtNight, . ~ . + otUse + prevHunt*otUse + prevHunt*canSt - prevHunt)          
          
          # off-trail and structures, linear
          d102 <- update(envtNight, . ~ . + otUse + distStrucSt + prevHunt*otUse + prevHunt*distStrucSt - prevHunt)
          
          # off-trail and structures, linear, canopy
          d103 <- update(envtNight, . ~ . + otUse + distStrucSt + prevHunt*otUse + prevHunt*distStrucSt + prevHunt*canSt - prevHunt)          
          
          # offtrail and structures, quadratic
          d104 <- update(envtNight, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + prevHunt*otUse + prevHunt*distStrucSt + prevHunt*I(distStrucSt^2) - prevHunt)
          
          # offtrail and structures, quadratic, canopy
          d105 <- update(envtNight, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + prevHunt*otUse + prevHunt*distStrucSt + prevHunt*I(distStrucSt^2) + prevHunt*canSt - prevHunt)          
          
          
    #### change use of landscape but not response to people ####       
          
          # all environment
          d106 <- update(envtNight, . ~ . + prevHunt:lcClass + prevHunt*can + prevHunt*slope + prevHunt*elev + prevHunt*northness
                       + prevHunt*snowSt + prevHunt*I(slope^2) + prevHunt:I(elev^2) + prevHunt:I(northness^2) - prevHunt)
          
          
          # canopy (i.e., hiding cover/escape terrain) only
          d107 <- update(envtNight, . ~ . + prevHunt*canSt - prevHunt)          

          
          
  #### competition ####
      
      # create list to store current subset of models in
      modsNight <- list()
      # list model names
      modNamesNight <- ls(envir = .GlobalEnv, pattern = "^d[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesNight)) { modsNight[[i]] <- get(modNamesNight[i]) }
      # create a dataframe of aicc results...
      aicNight <- data.frame(aictab(cand.set = modsNight, modnames = modNamesNight))
      # ...sorted from smallest to largest aicc value...
      aicNight <- aicNight[order(aicNight$Delta_AICc), ]
      # ... and store
      write.csv(aicNight, file = "aicNightTSMB.csv", row.names = FALSE)      
      # identify best-supported models (deltaAICc < 2)
      aic2Night <- subset(aicNight, Delta_AICc < 2.0)
      aic2Night <- droplevels(aic2Night)
      aic2Night$Modnames <- as.character(aic2Night$Modnames)
      # save top-supported models
      topTSMBs <- list()
      # list model names
      topTSMBnames <- unique(aic2Night$Modnames)
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
          d108 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*recClass + tSinceHunt*activeFeedSt - tSinceHunt)
          
          # linear, predictable feedgrounds, canopy
          d109 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*recClass + tSinceHunt*activeFeedSt
                       + tSinceHunt*canSt - tSinceHunt)          
      
          # quadratic, predictable feedgrounds
          d110 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  tSinceHunt*distRdSt +  tSinceHunt*distStrucSt +  tSinceHunt*recClass +  tSinceHunt*activeFeedSt
                       +  tSinceHunt*I(distRdSt^2) +  tSinceHunt*I(distStrucSt^2) +  tSinceHunt*I(activeFeedSt^2) - tSinceHunt)

          # quadratic, predictable feedgrounds, canopy
          d111 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  tSinceHunt*distRdSt +  tSinceHunt*distStrucSt +  tSinceHunt*recClass +  tSinceHunt*activeFeedSt
                       +  tSinceHunt*I(distRdSt^2) +  tSinceHunt*I(distStrucSt^2) +  tSinceHunt*I(activeFeedSt^2)
                       + tSinceHunt*canSt - tSinceHunt)
      
          # linear, all feedgrounds
          d112 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*recClass + tSinceHunt*distFeedSt - tSinceHunt)
          
          # linear, all feedgrounds, canopy
          d113 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*recClass + tSinceHunt*distFeedSt
                       + tSinceHunt*canSt - tSinceHunt)          
      
          # quadratic, all feedgrounds
          d114 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*recClass + tSinceHunt*distFeedSt
                       + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distStrucSt^2) + tSinceHunt*I(distFeedSt^2) - tSinceHunt)
          
          # quadratic, all feedgrounds, canopy
          d115 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*recClass + tSinceHunt*distFeedSt
                       + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distStrucSt^2) + tSinceHunt*I(distFeedSt^2)
                       + tSinceHunt*canSt - tSinceHunt)          
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          d116 <- update(envtNight, . ~ . + distRdSt + recClass + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*activeFeedSt - tSinceHunt)
          
          # linear, predictable feedgrounds, canopy
          d117 <- update(envtNight, . ~ . + distRdSt + recClass + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*activeFeedSt + tSinceHunt*canSt - tSinceHunt)          
          
      
          # quadratic, predictable feedgrounds
          d118 <- update(envtNight, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*activeFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(activeFeedSt^2) - tSinceHunt)

          # quadratic, predictable feedgrounds, canopy
          d119 <- update(envtNight, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*activeFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(activeFeedSt^2)
                       + tSinceHunt*canSt - tSinceHunt)
      
          # linear, all feedgrounds
          d120 <- update(envtNight, . ~ . + distRdSt + recClass + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*distFeedSt - tSinceHunt)
          
          # linear, all feedgrounds, canopy
          d121 <- update(envtNight, . ~ . + distRdSt + recClass + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*distFeedSt + tSinceHunt*canSt - tSinceHunt)          
      
          # quadratic, all feedgrounds
          d122 <- update(envtNight, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*distFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distFeedSt^2) - tSinceHunt)
          
          # quadratic, all feedgrounds, canopy
          d123 <- update(envtNight, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*distFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distFeedSt^2)
                       + tSinceHunt*canSt - tSinceHunt)
                    
      
      
      
      #### motorized recreation, and all others ####   
          
          
          # linear, predictable feedgrounds
          d124 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoUse + tSinceHunt*activeFeedSt - tSinceHunt)
          
          # linear, predictable feedgrounds, canopy
          d125 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoUse + tSinceHunt*activeFeedSt
                       + tSinceHunt*canSt - tSinceHunt)          
      
          # quadratic, predictable feedgrounds
          d126 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoUse + tSinceHunt*activeFeedSt
                       + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distStrucSt^2) + tSinceHunt*I(activeFeedSt^2) - tSinceHunt)

          # quadratic, predictable feedgrounds, canopy
          d127 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoUse + tSinceHunt*activeFeedSt
                       + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distStrucSt^2) + tSinceHunt*I(activeFeedSt^2)
                       + tSinceHunt*canSt - tSinceHunt)
      
          # linear, all feedgrounds
          d128 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoUse + tSinceHunt*distFeedSt - tSinceHunt)
          
          # linear, all feedgrounds, canopy
          d129 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoUse + tSinceHunt*distFeedSt
                       + tSinceHunt*canSt - tSinceHunt)          
      
          # quadratic, all feedgrounds
          d130 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoUse + tSinceHunt*distFeedSt
                       + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distStrucSt^2) + tSinceHunt*I(distFeedSt^2) - tSinceHunt)
          
          # quadratic, all feedgrounds, canopy
          d131 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoUse + tSinceHunt*distFeedSt
                       + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distStrucSt^2) + tSinceHunt*I(distFeedSt^2)
                       + tSinceHunt*canSt - tSinceHunt)          
          
          
          
      #### motorized activity only ####   
          
          # linear, predictable feedgrounds
          d132 <- update(envtNight, . ~ . + distRdSt + motoUse + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*motoUse + tSinceHunt*activeFeedSt - tSinceHunt)
          
          # linear, predictable feedgrounds, canopy
          d133 <- update(envtNight, . ~ . + distRdSt + motoUse + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*motoUse + tSinceHunt*activeFeedSt + tSinceHunt*canSt - tSinceHunt)          

          # quadratic, predictable feedgrounds
          d134 <- update(envtNight, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*motoUse + tSinceHunt*activeFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(activeFeedSt^2) - tSinceHunt)

          # quadratic, predictable feedgrounds, canopy
          d135 <- update(envtNight, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*motoUse + tSinceHunt*activeFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(activeFeedSt^2)
                       + tSinceHunt*canSt - tSinceHunt)
      
          # linear, all feedgrounds
          d136 <- update(envtNight, . ~ . + distRdSt + motoUse + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*motoUse + tSinceHunt*distFeedSt - tSinceHunt)
          
          # linear, all feedgrounds, canopy
          d137 <- update(envtNight, . ~ . + distRdSt + motoUse + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*motoUse + tSinceHunt*distFeedSt + tSinceHunt*canSt - tSinceHunt)          
      
          # quadratic, all feedgrounds
          d138 <- update(envtNight, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*motoUse + tSinceHunt*distFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distFeedSt^2) - tSinceHunt) 
          
          # quadratic, all feedgrounds, canopy
          d139 <- update(envtNight, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*motoUse + tSinceHunt*distFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distFeedSt^2)
                       + tSinceHunt*canSt - tSinceHunt) 
      

        
      #### off-trail recreation - alone; with or without structures ####  
          
          # off-trail recreation only
          d140 <- update(envtNight, . ~ . + otUse + tSinceHunt*otUse - tSinceHunt)
          
          # off-trail recreation only, canopy
          d141 <- update(envtNight, . ~ . + otUse + tSinceHunt*otUse + tSinceHunt*canSt - tSinceHunt)          
          
          # off-trail and structures, linear
          d142 <- update(envtNight, . ~ . + otUse + distStrucSt + tSinceHunt*otUse + tSinceHunt*distStrucSt - tSinceHunt)
          
          # off-trail and structures, linear, canopy
          d143 <- update(envtNight, . ~ . + otUse + distStrucSt + tSinceHunt*otUse + tSinceHunt*distStrucSt + tSinceHunt*canSt - tSinceHunt)          
          
          # offtrail and structures, quadratic
          d144 <- update(envtNight, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + tSinceHunt*otUse + tSinceHunt*distStrucSt + tSinceHunt*I(distStrucSt^2) - tSinceHunt)
          
          # offtrail and structures, quadratic, canopy
          d145 <- update(envtNight, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + tSinceHunt*otUse + tSinceHunt*distStrucSt + tSinceHunt*I(distStrucSt^2) + tSinceHunt*canSt - tSinceHunt)          
          
          
    #### change use of landscape but not response to people ####       
          
          # all environment
          d146 <- update(envtNight, . ~ . + tSinceHunt:lcClass + tSinceHunt*can + tSinceHunt*slope + tSinceHunt*elev + tSinceHunt*northness
                       + tSinceHunt*snowSt + tSinceHunt*I(slope^2) + tSinceHunt:I(elev^2) + tSinceHunt:I(northness^2) - tSinceHunt)
          
          
          # canopy (i.e., hiding cover/escape terrain) only
          d147 <- update(envtNight, . ~ . + tSinceHunt*canSt - tSinceHunt)  
          
          
  #### competition ####
      
      # create list to store current subset of models in
      modsNight <- list()
      # list model names
      modNamesNight <- ls(envir = .GlobalEnv, pattern = "^d[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesNight)) { modsNight[[i]] <- get(modNamesNight[i]) }
      # create a dataframe of aicc results...
      aicNight <- data.frame(aictab(cand.set = modsNight, modnames = modNamesNight))
      # ...sorted from smallest to largest aicc value...
      # ... and store
      write.csv(aicNight, file = "aicNightTSMC.csv", row.names = FALSE)      
      aicNight <- aicNight[order(aicNight$Delta_AICc), ]
      # identify best-supported models (deltaAICc < 2)
      aic2Night <- subset(aicNight, Delta_AICc < 2.0)
      aic2Night <- droplevels(aic2Night)
      aic2Night$Modnames <- as.character(aic2Night$Modnames)
      # save top-supported models
      topTSMCs <- list()
      # list model names
      topTSMCnames <- unique(aic2Night$Modnames)
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
          d148 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*recClass + tContHunt*activeFeedSt - tContHunt)
          
          # linear, predictable feedgrounds, canopy
          d149 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*recClass + tContHunt*activeFeedSt
                       + tContHunt*canSt - tContHunt)          
      
          # quadratic, predictable feedgrounds
          d150 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  tContHunt*distRdSt +  tContHunt*distStrucSt +  tContHunt*recClass +  tContHunt*activeFeedSt
                       +  tContHunt*I(distRdSt^2) +  tContHunt*I(distStrucSt^2) +  tContHunt*I(activeFeedSt^2) - tContHunt)

          # quadratic, predictable feedgrounds, canopy
          d151 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  tContHunt*distRdSt +  tContHunt*distStrucSt +  tContHunt*recClass +  tContHunt*activeFeedSt
                       +  tContHunt*I(distRdSt^2) +  tContHunt*I(distStrucSt^2) +  tContHunt*I(activeFeedSt^2)
                       + tContHunt*canSt - tContHunt)
      
          # linear, all feedgrounds
          d152 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*recClass + tContHunt*distFeedSt - tContHunt)
          
          # linear, all feedgrounds, canopy
          d153 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*recClass + tContHunt*distFeedSt
                       + tContHunt*canSt - tContHunt)          
      
          # quadratic, all feedgrounds
          d154 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*recClass + tContHunt*distFeedSt
                       + tContHunt*I(distRdSt^2) + tContHunt*I(distStrucSt^2) + tContHunt*I(distFeedSt^2) - tContHunt)
          
          # quadratic, all feedgrounds, canopy
          d155 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*recClass + tContHunt*distFeedSt
                       + tContHunt*I(distRdSt^2) + tContHunt*I(distStrucSt^2) + tContHunt*I(distFeedSt^2)
                       + tContHunt*canSt - tContHunt)          
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          d156 <- update(envtNight, . ~ . + distRdSt + recClass + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*activeFeedSt - tContHunt)
          
          # linear, predictable feedgrounds, canopy
          d157 <- update(envtNight, . ~ . + distRdSt + recClass + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*activeFeedSt + tContHunt*canSt - tContHunt)          
          

          # quadratic, predictable feedgrounds
          d158 <- update(envtNight, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*activeFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(activeFeedSt^2) - tContHunt)

          # quadratic, predictable feedgrounds, canopy
          d159 <- update(envtNight, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*activeFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(activeFeedSt^2)
                       + tContHunt*canSt - tContHunt)

          # linear, all feedgrounds
          d160 <- update(envtNight, . ~ . + distRdSt + recClass + distFeedSt
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*distFeedSt - tContHunt)
          
          # linear, all feedgrounds, canopy
          d161 <- update(envtNight, . ~ . + distRdSt + recClass + distFeedSt
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*distFeedSt + tContHunt*canSt - tContHunt)          
      
          # quadratic, all feedgrounds
          d162 <- update(envtNight, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*distFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(distFeedSt^2) - tContHunt)
          
          # quadratic, all feedgrounds, canopy
          d163 <- update(envtNight, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*distFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(distFeedSt^2)
                       + tContHunt*canSt - tContHunt)
                    
      
      
      
      #### motorized recreation, and all others ####   
          
          
          # linear, predictable feedgrounds
          d164 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoUse + tContHunt*activeFeedSt - tContHunt)
          
          # linear, predictable feedgrounds, canopy
          d165 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoUse + tContHunt*activeFeedSt
                       + tContHunt*canSt - tContHunt)          
      
          # quadratic, predictable feedgrounds
          d166 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoUse + tContHunt*activeFeedSt
                       + tContHunt*I(distRdSt^2) + tContHunt*I(distStrucSt^2) + tContHunt*I(activeFeedSt^2) - tContHunt)

          # quadratic, predictable feedgrounds, canopy
          d167 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoUse + tContHunt*activeFeedSt
                       + tContHunt*I(distRdSt^2) + tContHunt*I(distStrucSt^2) + tContHunt*I(activeFeedSt^2)
                       + tContHunt*canSt - tContHunt)
      
          # linear, all feedgrounds
          d168 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoUse + tContHunt*distFeedSt - tContHunt)
          
          # linear, all feedgrounds, canopy
          d169 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoUse + tContHunt*distFeedSt
                       + tContHunt*canSt - tContHunt)          
      
          # quadratic, all feedgrounds
          d170 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoUse + tContHunt*distFeedSt
                       + tContHunt*I(distRdSt^2) + tContHunt*I(distStrucSt^2) + tContHunt*I(distFeedSt^2) - tContHunt)
          
          # quadratic, all feedgrounds, canopy
          d171 <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoUse + tContHunt*distFeedSt
                       + tContHunt*I(distRdSt^2) + tContHunt*I(distStrucSt^2) + tContHunt*I(distFeedSt^2)
                       + tContHunt*canSt - tContHunt)          
          
          
          
      #### motorized activity only ####   
          
          # linear, predictable feedgrounds
          d172 <- update(envtNight, . ~ . + distRdSt + motoUse + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*motoUse + tContHunt*activeFeedSt - tContHunt)
          
          # linear, predictable feedgrounds, canopy
          d173 <- update(envtNight, . ~ . + distRdSt + motoUse + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*motoUse + tContHunt*activeFeedSt + tContHunt*canSt - tContHunt)          
      
          # quadratic, predictable feedgrounds
          d174 <- update(envtNight, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*motoUse + tContHunt*activeFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(activeFeedSt^2) - tContHunt)

          # quadratic, predictable feedgrounds, canopy
          d175 <- update(envtNight, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*motoUse + tContHunt*activeFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(activeFeedSt^2)
                       + tContHunt*canSt - tContHunt)
      
          # linear, all feedgrounds
          d176 <- update(envtNight, . ~ . + distRdSt + motoUse + distFeedSt
                       + tContHunt*distRdSt + tContHunt*motoUse + tContHunt*distFeedSt - tContHunt)
          
          # linear, all feedgrounds, canopy
          d177 <- update(envtNight, . ~ . + distRdSt + motoUse + distFeedSt
                       + tContHunt*distRdSt + tContHunt*motoUse + tContHunt*distFeedSt + tContHunt*canSt - tContHunt)          
      
          # quadratic, all feedgrounds
          d178 <- update(envtNight, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*motoUse + tContHunt*distFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(distFeedSt^2) - tContHunt) 
          
          # quadratic, all feedgrounds, canopy
          d179 <- update(envtNight, . ~ . + distRdSt + motoUse + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*motoUse + tContHunt*distFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(distFeedSt^2)
                       + tContHunt*canSt - tContHunt) 
      

        
      #### off-trail recreation - alone; with or without structures ####  
          
          # off-trail recreation only
          d180 <- update(envtNight, . ~ . + otUse + tContHunt*otUse - tContHunt)
          
          # off-trail recreation only, canopy
          d181 <- update(envtNight, . ~ . + otUse + tContHunt*otUse + tContHunt*canSt - tContHunt)          
          
          # off-trail and structures, linear
          d182 <- update(envtNight, . ~ . + otUse + distStrucSt + tContHunt*otUse + tContHunt*distStrucSt - tContHunt)
          
          # off-trail and structures, linear, canopy
          d183 <- update(envtNight, . ~ . + otUse + distStrucSt + tContHunt*otUse + tContHunt*distStrucSt + tContHunt*canSt - tContHunt)          
          
          # offtrail and structures, quadratic
          d184 <- update(envtNight, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + tContHunt*otUse + tContHunt*distStrucSt + tContHunt*I(distStrucSt^2) - tContHunt)
          
          # offtrail and structures, quadratic, canopy
          d185 <- update(envtNight, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + tContHunt*otUse + tContHunt*distStrucSt + tContHunt*I(distStrucSt^2) + tContHunt*canSt - tContHunt)          
          
          
    #### change use of landscape but not response to people ####       
          
          # all environment
          d186 <- update(envtNight, . ~ . + tContHunt:lcClass + tContHunt*can + tContHunt*slope + tContHunt*elev + tContHunt*northness
                       + tContHunt*snowSt + tContHunt*I(slope^2) + tContHunt:I(elev^2) + tContHunt:I(northness^2) - tContHunt)
          
          
          # canopy (i.e., hiding cover/escape terrain) only
          d187 <- update(envtNight, . ~ . + tContHunt*canSt - tContHunt)        
          
  #### competition ####
      
      # create list to store current subset of models in
      modsNight <- list()
      # list model names
      modNamesNight <- ls(envir = .GlobalEnv, pattern = "^d[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesNight)) { modsNight[[i]] <- get(modNamesNight[i]) }
      # create a dataframe of aicc results...
      aicNight <- data.frame(aictab(cand.set = modsNight, modnames = modNamesNight))
      # ...sorted from smallest to largest aicc value...
      aicNight <- aicNight[order(aicNight$Delta_AICc), ]
      # ... and store
      write.csv(aicNight, file = "aicNightTSMD.csv", row.names = FALSE)      
      # identify best-supported models (deltaAICc < 2)
      aic2Night <- subset(aicNight, Delta_AICc < 2.0)
      aic2Night <- droplevels(aic2Night)
      aic2Night$Modnames <- as.character(aic2Night$Modnames)
      # save top-supported models
      topTSMDs <- list()
      # list model names
      topTSMDnames <- unique(aic2Night$Modnames)
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
      d188 <- update(envtNight, . ~ . + distFeedSt)
      
      # all feedgrounds, quadratic
      d189 <- update(envtNight, . ~ . + distFeedSt + I(distFeedSt^2))
      
      # active feedgrounds, linear
      d190 <- update(envtNight, . ~ . + activeFeedSt)
      
      # active feedgrounds, quadratic
      d191 <- update(envtNight, . ~ . + activeFeedSt + I(activeFeedSt^2))
      
      
  #### competition ####
      
      # create list to store current subset of models in
      modsNight <- list()
      # list model names
      modNamesNight <- ls(envir = .GlobalEnv, pattern = "^d[0-9]{1,5}") 
      # store the models in the list
      for (i in 1:length(modNamesNight)) { modsNight[[i]] <- get(modNamesNight[i]) }
      # create a dataframe of aicc results...
      aicNight <- data.frame(aictab(cand.set = modsNight, modnames = modNamesNight))
      # ...sorted from smallest to largest aicc value...
      aicNight <- aicNight[order(aicNight$Delta_AICc), ]
      # ... and store
      write.csv(aicNight, file = "aicNightRSM.csv", row.names = FALSE)      
      # identify best-supported models (deltaAICc < 2)
      aic2Night <- subset(aicNight, Delta_AICc < 2.0)
      aic2Night <- droplevels(aic2Night)
      aic2Night$Modnames <- as.character(aic2Night$Modnames)
      # save top-supported models
      topRSMs <- list()
      # list model names
      topRSMnames <- unique(aic2Night$Modnames)
      # store the models in the list
      for (i in 1:length(topRSMnames)) { topRSMs[[i]] <- get(topRSMnames[i]) }      
      # delete unsupported models and clear memory for next batch
      rm(list = ls(pattern = "^d[0:9]*"))
      gc()      
         
      
################################################################################################## #  
 

      
      
    
### ### ### ### ### ### ### ### 
#### | MODEL COMPARISON |  ####
### ### ### ### ### ### ### ###      
      
      
      # list of all supported models 
      topMods <- c(topLSMs, topDSMs, topTSMAs, topTSMBs, topTSMCs, topTSMDs, topRSMs, envtNight)
      
      # model names
      names(topMods) <- c(topLSMnames, topDSMnames, topTSMAnames, topTSMBnames,
                          topTSMCnames, topTSMDnames, topRSMnames, "envtNight")
      
      # dataframe of aicc results...
      aicAll <- data.frame(aictab(cand.set = topMods, modnames = names(topMods)))
      
      # ...sorted from smallest to largest aicc value...
      aicAll <- aicAll[order(aicAll$Delta_AICc), ]
      
      # .. and exported
      write.csv(aicAll, "aic-night.csv", row.names=F)
      
      # store and export subset of moderately-supported models (deltaAICc < 4)
      aic4All <- subset(aicAll, Delta_AICc < 4.0); aic4All <- droplevels(aic4All)
      write.csv(aic4All, "aic4-night.csv", row.names=F)
      
      # store and export subset of best-supported models (deltaAICc < 2)
      aic2All <- subset(aicAll, Delta_AICc < 2.0); aic2All <- droplevels(aic2All)
      write.csv(aic2All, "aic2-night.csv", row.names=F)  
      
      # view top-supported models
      topAllMat <- matrix(aic2All$Modname)
      # print summaries of all (all one)
      apply(topAllMat, 1, get)
      
      
      # check bic too, just to be thorough
      
        # dataframe of bic results...
        # ...sorted from smallest to largest bicc value...
        bicAll <- bicAll[order(bicAll$Delta_BIC), ]
        # .. and exported
        write.csv(bicAll, "bic-night.csv", row.names=F)            
      
      
      ## extract and store top model results ##
      
          # name top model as such
          topModNight <- topTSMCs[[1]]
          
          # extract estimates from model summary
          topNightEsts <- data.frame(
            Covariate = rownames(coef(summary(topModNight))),
            Estimate = round(coef(summary(topModNight))[, "Estimate"], 3),
            stdError = round(coef(summary(topModNight))[, "z value"], 3),
            OR = round(exp(coef(summary(topModNight))[, "Estimate"]), 3),
            pVal = coef(summary(topModNight))[, "Pr(>|z|)"])
          topNightEsts$sig <- ifelse(topNightEsts$pVal <= 0.05, 1, 0)
          
          # estimate CIs separately bc not positive they map to correct covariate otherwise
          topNightCIs <- data.frame(
            ciLow = round(confint(topModNight, parm = "beta_", method = "Wald")[,1], 3),
            ciHigh = round(confint(topModNight, parm = "beta_", method = "Wald")[,2], 3))
          topNightCIs$Covariate <- rownames(topNightCIs)
          
          # combine estimates and CIs
          topNight <- left_join(topNightEsts, topNightCIs, by = "Covariate")
    
          # export
          write.csv(topNight, "topModNight.csv", row.names = F)
        
     

      
      
################################################################################################## #  
 
      
      save.image("modelsHumanNight.RData") 
            
      
################################################################################################## #  
      
################################################################################################## # 
      
    
### ### ### ### ### ### ### ### ###
#### | TOP MODEL DIAGNOSTICS|  ####
### ### ### ### ### ### ### ### ###   
      
      summary(topModNight)
    
      ## area under the roc curve ##
      invisible(plot(roc(factor(ifelse(modDatNight$Used == 1, 1, 0)), fitted(topModNight)), 
                     print.thres = c(.1, .5), col = "red", print.auc = T)) # auc = 0.738
      
      ## predictive accuracy @ >50% ##  
      confusionMatrix(factor(as.character(ifelse(fitted(topModNight) > 0.5, "Yes", "No"))), 
                      factor(ifelse(modDatNight$Used == 1, "Yes", "No")), positive = "Yes") # 67% 
    
      
      ## binned residual plots ##
      binnedplot(fitted(topModNight), residuals(topModNight, type = "response"), main = "Night - top model")

            
        