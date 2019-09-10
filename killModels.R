
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
####   | RAW DATA |  ####
### ### ### ### ### ### #
 
    
    modDatRaw <- read.csv("killDat.csv")
    
################################################################################################## #  
  
    
    
### ### ###  ### ### ### ###
####   | FORMAT DATA |  ####
### ### ###  ### ### ### ###

    
    modDat <- modDatRaw %>% mutate(
      # handle datetimes and dates of course
      Date = ymd(Date),
      # year as numeric
      Year = as.numeric(Year),
      # order landcover from most to least available
      lcClass = factor(lcClass, levels = c("Forest", "Shrub", "Herbaceous", "Riparian", "NoVeg")))
    
    

    #### check correlations between all environmental and human covariates ####

    dat.cor <- modDat %>%
      dplyr::select(can, elev, slope, rug, snowCm, northness,
                    distRd, distStruc, distFeed, distFeedActive)
    corDat <- cor(dat.cor)    
    # correlations >= 0.50
    cor5 <- corDat
    cor5[cor5 < 0.50] <- NA
    cor5    


################################################################################################## #  
                
    
    
### ### ### ### ### ### ### ### ### ### ### ### ##
####   | EVALUATE COVARIATE EFFECTS|  ####
### ### ### ### ### ### ### ### ### ### ### ### ##  
            
            
      #### Specify global models ####

    
    # dude there's no way you can estimate this many covariates wtf were you thinking
    
    # i think best way forward is to do separate models for each of the effects of interest
    # and just be clear that that's what you did
    # so rest of sday is making those quick models/plots
    # and finishing your presentation

 
    