                      ### ### ### ### ### ###  ### ### ### ### ### ### # 
                      #        PREDICTED MAP OF WOLF DISTRIBUTIONS     #
                      #   BASED ON ENVIRONMENTAL AND HUMAN INFLUENCES  #
                      #                                                #
                      #            Kristin Barker | Fall 2019          #
                      #             kristinjbarker@gmail.com           #
                      ### ### ### ### ### ###  ### ### ### ### ### ### # 


################################################################################################## #

### ### ### ### ### #
####  | SETUP |  ####
### ### ### ### ### #


  
  #### Set working directory and filepath to spatial data ####

      setwd("C:\\Users\\Kristin\\Box Sync\\Documents\\HumanInfluenceOnWolves")
      datDir <- "C:\\Users\\Kristin\\Box Sync\\Documents\\Data"

  

  #### Install and load any necessary packages you don't already have ####
  
    
    # list of packages needed
    packages <- c(
      "raster",        ## raster stacking, extraction, etc
      "maptools",      ## kmlPoints
      "rgdal",         ## spatial/shapefile work 
      "rgeos",         ## gDistance and other spatial work
      "sp",            ## spatial work
      "sf",            ## spatial work like the kids are doing it these days
      "beepr",         ## notify when chunk complete
      "lubridate",     ## manipulate datetime data inside dplyr pipes
      "dplyr")         ## data manipulation and general awesomeness
    
    
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

    
  
  #### Define spatial projections ####
    
    ll <- CRS("+init=epsg:4326") # WGS 84
    utm <- CRS("+init=epsg:3742") # NAD83(HARN)/UTMzone12N 
    aea <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
    
    
    
  #### Increase memory limit ####
    
    memory.limit(size = 7500000) 

    
    
################################################################################################## #  
  
    
    
### ### ### ### ### ### #
####   | RAW DATA |  ####
### ### ### ### ### ### #
 
    
    
    #### Model data ####
    
      ## raw model data
      modDatRaw <- read.csv("modDat.csv")
    
    
      ## formatted for running models
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
  
      ## split out day/night/crepuscular time periods
      modDatDay <- filter(modDat, daytime == "day")  
      modDatNight <- filter(modDat, daytime == "night")
      modDatCrep <- filter(modDat, daytime == "crep")
      
      
      
      ## models for prediction (from models-[timeframe].R)
      
        ## day
        modDay <- glmer(Used ~ 1 + canSt + slopeSt + elevSt + northnessSt + snowSt
                         + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                         + snowSt:canSt + snowSt:northnessSt + snowSt:elevSt + snowSt:I(elevSt*elevSt) 
                         + distRdSt + distStrucSt + distFeedSt
                         + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                         + hunt*distRdSt + hunt*distStrucSt + hunt*distFeedSt
                         + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2)
                         + (1|Pack), family = binomial(logit), data = modDatDay,
                         control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4), calc.derivs = FALSE))
          
      
        ## crepuscular
        modCrep <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                   + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                   + snowSt:canSt + snowSt:northnessSt + snowSt:elevSt
                   + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                   + distRdSt + distStrucSt + distFeedSt
                   + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                   + hunt*distRdSt + hunt*distStrucSt + hunt*distFeedSt
                   + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2)               
                   + (1|Pack), family = binomial(logit), data = modDatCrep,
                   control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4), calc.derivs = FALSE))
       
      
        ## night
        modNight <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                           + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                           + snowSt:canSt + snowSt:elevSt + snowSt:I(elevSt*elevSt) 
                           + distRdSt + distStrucSt + distFeedSt
                           + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*distFeedSt
                           + prevHunt*canSt
                           + (1|Pack), family = binomial(logit), data = modDatNight,
                           control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4), calc.derivs = FALSE))     
        


        
    #### Raw rasters already processed ####    
        
    
      # stack to extract data from (aspect, canopy, elev, landcov, rec, ruggedness, slope)
      files.rast <- list.files(
        path = paste0(datDir, "/xProcessedRasters/"),
        pattern = "^AEA.*tif$",
        full.names = TRUE)
      rast <- stack(files.rast)
      brk <- brick(rast)
      # remove "AEA" from raster names
      names(brk) <- substr(names(brk), 4, nchar(names(brk)))    
      # standardize and rename
      test <- lapply(files.rast, 1,  FUN = scale())
      test <- raster::scale(files.rast)
      test <- calc(brk, scale)
        
