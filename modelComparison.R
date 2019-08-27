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
      activeFeedSt = (distFeedActive - mean(distFeedActive))/sd(distFeedActive),
      # order landcover from most to least available
      lcClass = factor(lcClass, levels = c("Forest", "Shrub", "Herbaceous", "Riparian", "NoVeg")),
      # order rec from most to least regulated, relative to private land as baseline
      recClass = factor(recClass, levels = c("noRec", "noOT", "nomotoOT", "allOT")),
      # add binary private land designation
      pvt = ifelse(recClass == "noRec", 1, 0),
      # add binary off-trail/no off-trail use designation
      otUse = ifelse(recClass == "noOT", 0, 1),
      # add binary indicator of whether hunting was allowed that fall
      hunt = ifelse(Year == 2013 | Year == 2014 | Year >= 2018, 1, 0),
      # add binary indication of whether hunting was allowed in the previous year
      prevHunt = ifelse(Year == 2014 | Year == 2015 | Year >= 2019, 1, 0),
      # add binary indication of whether before or after hunting first started (baseline = b4)
      afterHunt = ifelse(Year > 2012, 1, 0),
      # add time since hunting was first allowed (fall 2012, corresponds to winter 2013)
      tSinceHunt = ifelse(Year - 2013 + 1 < 0, 0, Year - 2013 + 1),
      # add years of continuous hunting (to account for no hunting 2014, 2015, 2016)
      tContHunt = ifelse(Year == 2013, 1,
                         ifelse(Year == 2014, 2,
                                ifelse(Year >= 2018, Year - 2018 + 1, 0))))

    
    #### split day/night ####
    
    modDatDay <- filter(modDat, daytime == "day")
    modDatNight <- filter(modDat, daytime == "night")    


        