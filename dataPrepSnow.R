                      ### ### ### ### ### ###  ### ### ### ### ### ### 
                      #     FORMATTING AND PREPARING SWE DATA        #
                      #   FOR ANALYSIS OF HUMAN INFLUENCE ON WOLF    #
                      #        DISTRIBUTIONS AND BEHAVIORS           #
                      #                                              #
                      #         Kristin Barker | Summer 2019         #
                      #           kristinjbarker@gmail.com           #
                      ### ### ### ### ### ###  ### ### ### ### ### ### 


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
      "raster",        ## for parts of justin clapp's code
      "maps",          ## for parts of justin clapp's code
      "maptools",      ## for cluster algorithm & kmlPoints
      "rgdal",         ## for cluster algorithm & spatial/shapefile work
      "rgeos",         ## gDifference (clip) - nevermind, use erase to retain sf
      "sp",            ## spatial work
      "sf",            ## for spatial work like the kids are doing it these days
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
    
    
    
################################################################################################## #  
  
    
     
   #### Read in raw  data ####
    
    snowRaw <- read.delim(
      paste0(datDir, "/Environment/SNOTEL/snowDepthAndSWE_tbl.txt"),
      header = TRUE, sep = ",")


  #### Format ####
    
    snow <- snowRaw %>%
      mutate(Date = ymd(Date),
        elevM = ifelse(Station.Name == "Granite Creek", 6770 * 0.3048,
                       ifelse(Station.Name == "Gros Ventre Summit", 8750 * 0.3048, 9590 * 0.3048)),
        staAbbv = ifelse(Station.Name == "Granite Creek", "gc", 
                         ifelse(Station.Name == "Gros Ventre Summit", "gv", "tg"))) %>%
      rename(station = Station.Name,
             staID = Station.Id,
             sweMM = Snow.Water.Equivalent..mm..Start.of.Day.Values,
             sweDelta = Change.In.Snow.Water.Equivalent..mm.,
             snowCm = Snow.Depth..cm..Start.of.Day.Values,
             snowDelta = Change.In.Snow.Depth..cm.)

  #### Export ####
    
    write.csv(snow, file = paste0(datDir, "/Environment/snowDat_2005-2019.csv"), row.names = F)
