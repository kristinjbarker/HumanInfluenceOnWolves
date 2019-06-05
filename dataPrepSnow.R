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
    
    rawGC <- read.delim(paste0(datDir, "/Environment/SNOTEL/GraniteCreekTbl.txt"))
    rawGV <- read.delim(paste0(datDir, "/Environment/SNOTEL/GrosVentreSummitTbl.txt"))
    rawTG <- read.delim(paste0(datDir, "/Environment/SNOTEL/TogwoteePassTbl.txt"))

    
            
     
   #### Format to combine ####
    
    gc <- data.frame(
      Station = "Granite Creek",
      staAbbv = "gc",
      Date = ymd(substr(rawGC$Date.Snow.Water.Equivalent..in..Start.of.Day.Values, 1, 10)),
      SWE = as.numeric(gsub(".*(,)", "", rawGC$Date.Snow.Water.Equivalent..in..Start.of.Day.Values))
    )
    
    gv <- data.frame(
      Station = "Gros Ventre Summit",
      staAbbv = "gv",
      Date = ymd(substr(rawGV$Date.Snow.Water.Equivalent..in..Start.of.Day.Values, 1, 10)),
      SWE = as.numeric(gsub(".*(,)", "", rawGV$Date.Snow.Water.Equivalent..in..Start.of.Day.Values))
    )
        
    tg <- data.frame(
      Station = "Togwotee Pass",
      staAbbv = "tg",
      Date = ymd(substr(rawTG$Date.Snow.Water.Equivalent..in..Start.of.Day.Values, 1, 10)),
      SWE = as.numeric(gsub(".*(,)", "", rawTG$Date.Snow.Water.Equivalent..in..Start.of.Day.Values))
    )
    
    swe <- rbind(gc, gv, tg)
    write.csv(swe, file = paste0(datDir, "/Environment/swe2019.csv"), row.names = F)
