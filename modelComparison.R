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
      "dplyr",          ## data manipulation and general awesomeness
      "plyr")           ## summary tables of ppn of categorical covariates
    
    
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
    
    memory.limit(size = 97500000) 

    
    
################################################################################################## #  
  
    
    
### ### ### ### ### ### #
####   | DATA PREP |  ####
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
      activeFeedSt = (activeFeed - mean(activeFeed))/sd(activeFeed),
      # order landcover from most to least available
      lcClass = factor(lcClass, levels = c("Forest", "Shrub", "Herbaceous", "Riparian", "NoVeg")),
      # order rec from most to least regulated, relative to private land as baseline
      recClass = factor(recClass, levels = c("noRec", "noOT", "nomotoOT", "allOT")),
      # add binary private land designation
      pvt = ifelse(recClass == "noRec", 1, 0),
      # add binary off-trail/no off-trail use designation
      otUse = ifelse(recClass == "noOT", 0, 1),
      # add binary indication of whether hunting was allowed in the previous year
      prevHunt = ifelse(Year == 2013 | Year == 2014 | Year >= 2018, 1, 0),
      # add time since hunting was first allowed (2012)
      tSinceHunt = Year - 2012,
      # add years of continuous hunting (to account for no hunting 2014, 2015, 2016)
      tContHunt = ifelse(Year == 2012, 1,
                         ifelse(Year == 2013, 2,
                                ifelse(Year >= 2017, Year - 2017 + 1, 0))))

    
    #### split day/night ####
    
    datDay <- filter(modDat, daytime == "day")
    datNight <- filter(modDat, daytime == "night")    

    
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
                     data = datDay) 

    
      # nighttime (from dataExploration.R)
      envtNight <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                       + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                       + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt + snowSt:elevSt 
                       + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                       + (1|Pack/wolfYr), 
                       family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                       data = datNight)    
  

          
                  
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### #### #
#### | LANDSCAPE SHIELD MODELS |  ####
### ### ### ### ### ### ### ### #### #
      

      # roads & buildings, linear
      d1 <- update(envtDay, . ~ . + distRdSt + distStrucSt)
      n1 <- update(envtNight, . ~ . + distRdSt + distStrucSt)
      
      # roads & buildings, quadratic
      d2 <- update(envtDay, . ~ . + distRdSt + distStrucSt + I(distRdSt^2) + I(distStrucSt^2))
      n2 <- update(envtNight, . ~ . + distRdSt + distStrucSt + I(distRdSt^2) + I(distStrucSt^2))
      
      # buildings only, linear
      d3 <- update(envtDay, . ~ . + distStrucSt)
      n3 <- update(envtNight, . ~ . + distStrucSt)
      
      # buildings only, quadratic
      d4 <- update(envtDay, . ~ . + distStrucSt + I(distStrucSt^2))
      n4 <- update(envtNight, . ~ . + distStrucSt + I(distStrucSt^2))      
      
      
      
      
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
          d13 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          d14 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          d15 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + distFeedSt)
      
          # quadratic, all feedgrounds
          d16 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2))
          
          
          
      #### motorized activity only ####   
          
          # linear, predictable feedgrounds
          d17 <- update(envtDay, . ~ . + distRdSt + motoClass + activeFeedSt)
      
          # quadratic, predictable feedgrounds
          d18 <- update(envtDay, . ~ . + distRdSt + motoClass + activeFeedSt + I(distRdSt^2) + + I(activeFeedSt^2))
      
          # linear, all feedgrounds
          d19 <- update(envtDay, . ~ . + distRdSt + motoClass + distFeedSt)
      
          # quadratic, all feedgrounds
          d20 <- update(envtDay, . ~ . + distRdSt + motoClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2))      
      

      
      
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
                       + hunt*can)          
      
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
                       + hunt*can)          
      
          # linear, all feedgrounds
          d32 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d33 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt
                       + hunt*can)          
      
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
                       + hunt*can)          
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          d36 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt
                       + hunt*distRdSt + hunt*recClass + hunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d37 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt
                       + hunt*distRdSt + hunt*recClass + hunt*activeFeedSt + hunt*can)          
          
      
          # quadratic, predictable feedgrounds
          d38 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*recClass + hunt*activeFeedSt + hunt*I(distRdSt^2) + hunt*I(activeFeedSt^2))
          
          # quadratic, predictable feedgrounds, canopy
          d39 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*recClass + hunt*activeFeedSt + hunt*I(distRdSt^2) + hunt*I(activeFeedSt^2)
                       + hunt*can)
      
          # linear, all feedgrounds
          d40 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt
                       + hunt*distRdSt + hunt*recClass + hunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d41 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt
                       + hunt*distRdSt + hunt*recClass + hunt*distFeedSt + hunt*can)          
      
          # quadratic, all feedgrounds
          d42 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*recClass + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d43 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*recClass + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2)
                       + hunt*can)
                    
      
      
      
      #### motorized recreation, and all others ####   
          
          
          # linear, predictable feedgrounds
          d44 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + activeFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoClass + hunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d45 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + activeFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoClass + hunt*activeFeedSt
                       + hunt*can)          
      
          # quadratic, predictable feedgrounds
          d46 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoClass + hunt*activeFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(activeFeedSt^2))
          
          # quadratic, predictable feedgrounds, canopy
          d47 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoClass + hunt*activeFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(activeFeedSt^2)
                       + hunt*can)          
      
          # linear, all feedgrounds
          d48 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoClass + hunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d49 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + distFeedSt
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoClass + hunt*distFeedSt
                       + hunt*can)          
      
          # quadratic, all feedgrounds
          d50 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoClass + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d51 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*motoClass + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2)
                       + hunt*can)          
          
          
          
      #### motorized activity only ####   
          
          # linear, predictable feedgrounds
          d52 <- update(envtDay, . ~ . + distRdSt + motoClass + activeFeedSt
                       + hunt*distRdSt + hunt*motoClass + hunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d53 <- update(envtDay, . ~ . + distRdSt + motoClass + activeFeedSt
                       + hunt*distRdSt + hunt*motoClass + hunt*activeFeedSt + hunt*can)          
      
          # quadratic, predictable feedgrounds
          d54 <- update(envtDay, . ~ . + distRdSt + motoClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*motoClass + hunt*activeFeedSt + hunt*I(distRdSt^2) + hunt*I(activeFeedSt^2))
          
          # quadratic, predictable feedgrounds, canopy
          d55 <- update(envtDay, . ~ . + distRdSt + motoClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + hunt*distRdSt + hunt*motoClass + hunt*activeFeedSt + hunt*I(distRdSt^2) + hunt*I(activeFeedSt^2)
                       + hunt*can)
      
          # linear, all feedgrounds
          d56 <- update(envtDay, . ~ . + distRdSt + motoClass + distFeedSt
                       + hunt*distRdSt + hunt*motoClass + hunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d57 <- update(envtDay, . ~ . + distRdSt + motoClass + distFeedSt
                       + hunt*distRdSt + hunt*motoClass + hunt*distFeedSt + hunt*can)          
      
          # quadratic, all feedgrounds
          d58 <- update(envtDay, . ~ . + distRdSt + motoClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*motoClass + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2)) 
          
          # quadratic, all feedgrounds, canopy
          d59 <- update(envtDay, . ~ . + distRdSt + motoClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*motoClass + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2)
                       + hunt*can) 
      

        
      #### off-trail recreation - alone; with or without structures ####  
          
          # off-trail recreation only
          d60 <- update(envtDay, . ~ . + otUse + hunt*otUse)
          
          # off-trail recreation only, canopy
          d61 <- update(envtDay, . ~ . + otUse + hunt*otUse + hunt*can)          
          
          # off-trail and structures, linear
          d62 <- update(envtDay, . ~ . + otUse + distStrucSt + hunt*otUse + hunt*distStrucSt)
          
          # off-trail and structures, linear, canopy
          d63 <- update(envtDay, . ~ . + otUse + distStrucSt + hunt*otUse + hunt*distStrucSt + hunt*can)          
          
          # offtrail and structures, quadratic
          d64 <- update(envtDay, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + hunt*otUse + hunt*distStrucSt + hunt*I(distStrucSt^2))
          
          # offtrail and structures, quadratic, canopy
          d65 <- update(envtDay, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + hunt*otUse + hunt*distStrucSt + hunt*I(distStrucSt^2) + hunt*can)          
          
          
    #### change use of landscape but not response to people ####       
          
          # all environment
          d66 <- update(envtDay, . ~ . + hunt:lcClass + hunt*can + hunt*slope + hunt*elev + hunt*northness
                       + hunt*snowSt + hunt*I(slope^2) + hunt:I(elev^2) + hunt:I(northness^2))
          
          
          # canopy (i.e., hiding cover/escape terrain) only
          d67 <- update(envtDay, . ~ . + hunt*can)
      
      
      
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
                       + prevHunt*can)          
      
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
                       + prevHunt*can)          
      
          # linear, all feedgrounds
          d72 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d73 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*distFeedSt
                       + prevHunt*can)          
      
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
                       + prevHunt*can)          
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          d76 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d77 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*activeFeedSt + prevHunt*can)          
          
      
          # quadratic, predictable feedgrounds
          d78 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(activeFeedSt^2))
          
          # quadratic, predictable feedgrounds, canopy
          d79 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(activeFeedSt^2)
                       + prevHunt*can)
      
          # linear, all feedgrounds
          d80 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d81 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*distFeedSt + prevHunt*can)          
      
          # quadratic, all feedgrounds
          d82 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d83 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*recClass + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(distFeedSt^2)
                       + prevHunt*can)
                    
      
      
      
      #### motorized recreation, and all others ####   
          
          
          # linear, predictable feedgrounds
          d84 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoClass + prevHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d85 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoClass + prevHunt*activeFeedSt
                       + prevHunt*can)          
      
          # quadratic, predictable feedgrounds
          d86 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoClass + prevHunt*activeFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(activeFeedSt^2))
          
          # quadratic, predictable feedgrounds, canopy
          d87 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoClass + prevHunt*activeFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(activeFeedSt^2)
                       + prevHunt*can)          
      
          # linear, all feedgrounds
          d88 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoClass + prevHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d89 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoClass + prevHunt*distFeedSt
                       + prevHunt*can)          
      
          # quadratic, all feedgrounds
          d90 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoClass + prevHunt*distFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d91 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoClass + prevHunt*distFeedSt
                       + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2)
                       + prevHunt*can)          
          
          
          
      #### motorized activity only ####   
          
          # linear, predictable feedgrounds
          d92 <- update(envtDay, . ~ . + distRdSt + motoClass + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*motoClass + prevHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d93 <- update(envtDay, . ~ . + distRdSt + motoClass + activeFeedSt
                       + prevHunt*distRdSt + prevHunt*motoClass + prevHunt*activeFeedSt + prevHunt*can)          
      
          # quadratic, predictable feedgrounds
          d94 <- update(envtDay, . ~ . + distRdSt + motoClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*motoClass + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(activeFeedSt^2))
          
          # quadratic, predictable feedgrounds, canopy
          d95 <- update(envtDay, . ~ . + distRdSt + motoClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*motoClass + prevHunt*activeFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(activeFeedSt^2)
                       + prevHunt*can)
      
          # linear, all feedgrounds
          d96 <- update(envtDay, . ~ . + distRdSt + motoClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*motoClass + prevHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d97 <- update(envtDay, . ~ . + distRdSt + motoClass + distFeedSt
                       + prevHunt*distRdSt + prevHunt*motoClass + prevHunt*distFeedSt + prevHunt*can)          
      
          # quadratic, all feedgrounds
          d98 <- update(envtDay, . ~ . + distRdSt + motoClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*motoClass + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(distFeedSt^2)) 
          
          # quadratic, all feedgrounds, canopy
          d99 <- update(envtDay, . ~ . + distRdSt + motoClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + prevHunt*distRdSt + prevHunt*motoClass + prevHunt*distFeedSt + prevHunt*I(distRdSt^2) + prevHunt*I(distFeedSt^2)
                       + prevHunt*can) 
      

        
      #### off-trail recreation - alone; with or without structures ####  
          
          # off-trail recreation only
          d100 <- update(envtDay, . ~ . + otUse + prevHunt*otUse)
          
          # off-trail recreation only, canopy
          d101 <- update(envtDay, . ~ . + otUse + prevHunt*otUse + prevHunt*can)          
          
          # off-trail and structures, linear
          d102 <- update(envtDay, . ~ . + otUse + distStrucSt + prevHunt*otUse + prevHunt*distStrucSt)
          
          # off-trail and structures, linear, canopy
          d103 <- update(envtDay, . ~ . + otUse + distStrucSt + prevHunt*otUse + prevHunt*distStrucSt + prevHunt*can)          
          
          # offtrail and structures, quadratic
          d104 <- update(envtDay, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + prevHunt*otUse + prevHunt*distStrucSt + prevHunt*I(distStrucSt^2))
          
          # offtrail and structures, quadratic, canopy
          d105 <- update(envtDay, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + prevHunt*otUse + prevHunt*distStrucSt + prevHunt*I(distStrucSt^2) + prevHunt*can)          
          
          
    #### change use of landscape but not response to people ####       
          
          # all environment
          d106 <- update(envtDay, . ~ . + prevHunt:lcClass + prevHunt*can + prevHunt*slope + prevHunt*elev + prevHunt*northness
                       + prevHunt*snowSt + prevHunt*I(slope^2) + prevHunt:I(elev^2) + prevHunt:I(northness^2))
          
          
          # canopy (i.e., hiding cover/escape terrain) only
          d107 <- update(envtDay, . ~ . + prevHunt*can)          
      
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
                       + tSinceHunt*can)          
      
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
                       + tSinceHunt*can)          
      
          # linear, all feedgrounds
          d112 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*recClass + tSinceHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d113 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*recClass + tSinceHunt*distFeedSt
                       + tSinceHunt*can)          
      
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
                       + tSinceHunt*can)          
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          d116 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d117 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*activeFeedSt + tSinceHunt*can)          
          
      
          # quadratic, predictable feedgrounds
          d118 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*activeFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(activeFeedSt^2))
          
          # quadratic, predictable feedgrounds, canopy
          d119 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*activeFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(activeFeedSt^2)
                       + tSinceHunt*can)
      
          # linear, all feedgrounds
          d120 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d121 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*distFeedSt + tSinceHunt*can)          
      
          # quadratic, all feedgrounds
          d122 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*distFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d123 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*distFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distFeedSt^2)
                       + tSinceHunt*can)
                    
      
      
      
      #### motorized recreation, and all others ####   
          
          
          # linear, predictable feedgrounds
          d124 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoClass + tSinceHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d125 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoClass + tSinceHunt*activeFeedSt
                       + tSinceHunt*can)          
      
          # quadratic, predictable feedgrounds
          d126 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoClass + tSinceHunt*activeFeedSt
                       + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distStrucSt^2) + tSinceHunt*I(activeFeedSt^2))
          
          # quadratic, predictable feedgrounds, canopy
          d127 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoClass + tSinceHunt*activeFeedSt
                       + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distStrucSt^2) + tSinceHunt*I(activeFeedSt^2)
                       + tSinceHunt*can)          
      
          # linear, all feedgrounds
          d128 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoClass + tSinceHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d129 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoClass + tSinceHunt*distFeedSt
                       + tSinceHunt*can)          
      
          # quadratic, all feedgrounds
          d130 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoClass + tSinceHunt*distFeedSt
                       + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distStrucSt^2) + tSinceHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d131 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*motoClass + tSinceHunt*distFeedSt
                       + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distStrucSt^2) + tSinceHunt*I(distFeedSt^2)
                       + tSinceHunt*can)          
          
          
          
      #### motorized activity only ####   
          
          # linear, predictable feedgrounds
          d132 <- update(envtDay, . ~ . + distRdSt + motoClass + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*motoClass + tSinceHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d133 <- update(envtDay, . ~ . + distRdSt + motoClass + activeFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*motoClass + tSinceHunt*activeFeedSt + tSinceHunt*can)          
      
          # quadratic, predictable feedgrounds
          d134 <- update(envtDay, . ~ . + distRdSt + motoClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*motoClass + tSinceHunt*activeFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(activeFeedSt^2))
          
          # quadratic, predictable feedgrounds, canopy
          d135 <- update(envtDay, . ~ . + distRdSt + motoClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*motoClass + tSinceHunt*activeFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(activeFeedSt^2)
                       + tSinceHunt*can)
      
          # linear, all feedgrounds
          d136 <- update(envtDay, . ~ . + distRdSt + motoClass + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*motoClass + tSinceHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d137 <- update(envtDay, . ~ . + distRdSt + motoClass + distFeedSt
                       + tSinceHunt*distRdSt + tSinceHunt*motoClass + tSinceHunt*distFeedSt + tSinceHunt*can)          
      
          # quadratic, all feedgrounds
          d138 <- update(envtDay, . ~ . + distRdSt + motoClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*motoClass + tSinceHunt*distFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distFeedSt^2)) 
          
          # quadratic, all feedgrounds, canopy
          d139 <- update(envtDay, . ~ . + distRdSt + motoClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*motoClass + tSinceHunt*distFeedSt + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distFeedSt^2)
                       + tSinceHunt*can) 
      

        
      #### off-trail recreation - alone; with or without structures ####  
          
          # off-trail recreation only
          d140 <- update(envtDay, . ~ . + otUse + tSinceHunt*otUse)
          
          # off-trail recreation only, canopy
          d141 <- update(envtDay, . ~ . + otUse + tSinceHunt*otUse + tSinceHunt*can)          
          
          # off-trail and structures, linear
          d142 <- update(envtDay, . ~ . + otUse + distStrucSt + tSinceHunt*otUse + tSinceHunt*distStrucSt)
          
          # off-trail and structures, linear, canopy
          d143 <- update(envtDay, . ~ . + otUse + distStrucSt + tSinceHunt*otUse + tSinceHunt*distStrucSt + tSinceHunt*can)          
          
          # offtrail and structures, quadratic
          d144 <- update(envtDay, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + tSinceHunt*otUse + tSinceHunt*distStrucSt + tSinceHunt*I(distStrucSt^2))
          
          # offtrail and structures, quadratic, canopy
          d145 <- update(envtDay, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + tSinceHunt*otUse + tSinceHunt*distStrucSt + tSinceHunt*I(distStrucSt^2) + tSinceHunt*can)          
          
          
    #### change use of landscape but not response to people ####       
          
          # all environment
          d146 <- update(envtDay, . ~ . + tSinceHunt:lcClass + tSinceHunt*can + tSinceHunt*slope + tSinceHunt*elev + tSinceHunt*northness
                       + tSinceHunt*snowSt + tSinceHunt*I(slope^2) + tSinceHunt:I(elev^2) + tSinceHunt:I(northness^2))
          
          
          # canopy (i.e., hiding cover/escape terrain) only
          d147 <- update(envtDay, . ~ . + tSinceHunt*can)          
      
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#### | THREAT SHIELD MODELS - Continuous time since hunt | ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  
          
      
      #### all human influences ####
      
          # linear, predictable feedgrounds
          d28 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*recClass + tContHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d29 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*recClass + tContHunt*activeFeedSt
                       + tContHunt*can)          
      
          # quadratic, predictable feedgrounds
          d30 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  tContHunt*distRdSt +  tContHunt*distStrucSt +  tContHunt*recClass +  tContHunt*activeFeedSt
                       +  tContHunt*I(distRdSt^2) +  tContHunt*I(distStrucSt^2) +  tContHunt*I(activeFeedSt^2))
          
          # quadratic, predictable feedgrounds, canopy
          d31 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  tContHunt*distRdSt +  tContHunt*distStrucSt +  tContHunt*recClass +  tContHunt*activeFeedSt
                       +  tContHunt*I(distRdSt^2) +  tContHunt*I(distStrucSt^2) +  tContHunt*I(activeFeedSt^2)
                       + tContHunt*can)          
      
          # linear, all feedgrounds
          d32 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*recClass + tContHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d33 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*recClass + tContHunt*distFeedSt
                       + tContHunt*can)          
      
          # quadratic, all feedgrounds
          d34 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*recClass + tContHunt*distFeedSt
                       + tContHunt*I(distRdSt^2) + tContHunt*I(distStrucSt^2) + tContHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d35 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*recClass + tContHunt*distFeedSt
                       + tContHunt*I(distRdSt^2) + tContHunt*I(distStrucSt^2) + tContHunt*I(distFeedSt^2)
                       + tContHunt*can)          
          
          
          
      #### activity, not structures ####      
      
          # linear, predictable feedgrounds
          d36 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d37 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*activeFeedSt + tContHunt*can)          
          
      
          # quadratic, predictable feedgrounds
          d38 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*activeFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(activeFeedSt^2))
          
          # quadratic, predictable feedgrounds, canopy
          d39 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*activeFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(activeFeedSt^2)
                       + tContHunt*can)
      
          # linear, all feedgrounds
          d40 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d41 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*distFeedSt + tContHunt*can)          
      
          # quadratic, all feedgrounds
          d42 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*distFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d43 <- update(envtDay, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*recClass + tContHunt*distFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(distFeedSt^2)
                       + tContHunt*can)
                    
      
      
      
      #### motorized recreation, and all others ####   
          
          
          # linear, predictable feedgrounds
          d44 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoClass + tContHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d45 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoClass + tContHunt*activeFeedSt
                       + tContHunt*can)          
      
          # quadratic, predictable feedgrounds
          d46 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoClass + tContHunt*activeFeedSt
                       + tContHunt*I(distRdSt^2) + tContHunt*I(distStrucSt^2) + tContHunt*I(activeFeedSt^2))
          
          # quadratic, predictable feedgrounds, canopy
          d47 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoClass + tContHunt*activeFeedSt
                       + tContHunt*I(distRdSt^2) + tContHunt*I(distStrucSt^2) + tContHunt*I(activeFeedSt^2)
                       + tContHunt*can)          
      
          # linear, all feedgrounds
          d48 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + distFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoClass + tContHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d49 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + distFeedSt
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoClass + tContHunt*distFeedSt
                       + tContHunt*can)          
      
          # quadratic, all feedgrounds
          d50 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoClass + tContHunt*distFeedSt
                       + tContHunt*I(distRdSt^2) + tContHunt*I(distStrucSt^2) + tContHunt*I(distFeedSt^2))
          
          # quadratic, all feedgrounds, canopy
          d51 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*distStrucSt + tContHunt*motoClass + tContHunt*distFeedSt
                       + tContHunt*I(distRdSt^2) + tContHunt*I(distStrucSt^2) + tContHunt*I(distFeedSt^2)
                       + tContHunt*can)          
          
          
          
      #### motorized activity only ####   
          
          # linear, predictable feedgrounds
          d52 <- update(envtDay, . ~ . + distRdSt + motoClass + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*motoClass + tContHunt*activeFeedSt)
          
          # linear, predictable feedgrounds, canopy
          d53 <- update(envtDay, . ~ . + distRdSt + motoClass + activeFeedSt
                       + tContHunt*distRdSt + tContHunt*motoClass + tContHunt*activeFeedSt + tContHunt*can)          
      
          # quadratic, predictable feedgrounds
          d54 <- update(envtDay, . ~ . + distRdSt + motoClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*motoClass + tContHunt*activeFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(activeFeedSt^2))
          
          # quadratic, predictable feedgrounds, canopy
          d55 <- update(envtDay, . ~ . + distRdSt + motoClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*motoClass + tContHunt*activeFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(activeFeedSt^2)
                       + tContHunt*can)
      
          # linear, all feedgrounds
          d56 <- update(envtDay, . ~ . + distRdSt + motoClass + distFeedSt
                       + tContHunt*distRdSt + tContHunt*motoClass + tContHunt*distFeedSt)
          
          # linear, all feedgrounds, canopy
          d57 <- update(envtDay, . ~ . + distRdSt + motoClass + distFeedSt
                       + tContHunt*distRdSt + tContHunt*motoClass + tContHunt*distFeedSt + tContHunt*can)          
      
          # quadratic, all feedgrounds
          d58 <- update(envtDay, . ~ . + distRdSt + motoClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*motoClass + tContHunt*distFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(distFeedSt^2)) 
          
          # quadratic, all feedgrounds, canopy
          d59 <- update(envtDay, . ~ . + distRdSt + motoClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                       + tContHunt*distRdSt + tContHunt*motoClass + tContHunt*distFeedSt + tContHunt*I(distRdSt^2) + tContHunt*I(distFeedSt^2)
                       + tContHunt*can) 
      

        
      #### off-trail recreation - alone; with or without structures ####  
          
          # off-trail recreation only
          d60 <- update(envtDay, . ~ . + otUse + tContHunt*otUse)
          
          # off-trail recreation only, canopy
          d61 <- update(envtDay, . ~ . + otUse + tContHunt*otUse + tContHunt*can)          
          
          # off-trail and structures, linear
          d62 <- update(envtDay, . ~ . + otUse + distStrucSt + tContHunt*otUse + tContHunt*distStrucSt)
          
          # off-trail and structures, linear, canopy
          d63 <- update(envtDay, . ~ . + otUse + distStrucSt + tContHunt*otUse + tContHunt*distStrucSt + tContHunt*can)          
          
          # offtrail and structures, quadratic
          d64 <- update(envtDay, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + tContHunt*otUse + tContHunt*distStrucSt + tContHunt*I(distStrucSt^2))
          
          # offtrail and structures, quadratic, canopy
          d65 <- update(envtDay, . ~ . + otUse + distStrucSt + I(distStrucSt^2)
                       + tContHunt*otUse + tContHunt*distStrucSt + tContHunt*I(distStrucSt^2) + tContHunt*can)          
          
          
    #### change use of landscape but not response to people ####       
          
          # all environment
          d66 <- update(envtDay, . ~ . + tContHunt:lcClass + tContHunt*can + tContHunt*slope + tContHunt*elev + tContHunt*northness
                       + tContHunt*snowSt + tContHunt*I(slope^2) + tContHunt:I(elev^2) + tContHunt:I(northness^2))
          
          
          # canopy (i.e., hiding cover/escape terrain) only
          d67 <- update(envtDay, . ~ . + tContHunt*can)        
          
          
      
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ##
#### | RESOURCE SUBSIDY MODELS |  ####
### ### ### ### ### ### ### ### ### ##    
      
      # all feedgrounds, linear
      d3 <- update(envtDay, . ~ . + distFeedSt)
      
      # all feedgrounds, quadratic
      d3 <- update(envtDay, . ~ . + distFeedSt + I(distFeedSt^2))
      
      # active feedgrounds, linear
      d3 <- update(envtDay, . ~ . + activeFeedSt)
      
      # active feedgrounds, quadratic
      d3 <- update(envtDay, . ~ . + activeFeedSt + I(activeFeedSt^2))
      
      
      
      
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### 
#### | MODEL COMPARISON |  ####
### ### ### ### ### ### ### ###      
      
      
      
################################################################################################## #  
        