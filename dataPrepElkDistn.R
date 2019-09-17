                      ### ### ### ### ### ###  ### ### ### ### ### ### 
                      #       PREPARING ELK DISTRIBUTION DATA        #
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
      "readxl",        ## import elk count data from excel workbook
      "adehabitatHR",  ## estimate UD of elk distribution
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
  
    

### ### ###  ### ### ###
####  | RAW DATA |  ####
### ### ###  ### ### ###     


    
    ## elk movement barrier (refuge fence, town, etc) to remove from KDEs
    barrierRaw <- readOGR(paste0(datDir, "\\Elk"), layer ='elkBarrierLL')
    barrierSpdf <- spTransform(barrierRaw, utm)
    barrier <- as(spTransform(barrierRaw, utm), 'SpatialLines')
    
    
    ## elk count data (formatted manually from excel spreadsheets sent by Aly)
    rawElk <- read.csv(paste0(datDir, "/Elk/countsFormatted_2008-2019.csv"))
    

    

    
################################################################################################## #  


### ### ### ### ### ### #
####  | PREP DATA |  ####
### ### ### ### ### ### #    



    #### Format count data ####
    elkDat <- rawElk %>%
      mutate(
        Date = mdy(Date),
        Cow = ifelse(is.na(Cow), 0, Cow),
        Calf = ifelse(is.na(Calf), 0, Calf),
        BullMature = ifelse(is.na(BullMature), 0, BullMature),
        BullSpike = ifelse(is.na(BullSpike), 0, BullSpike),
        Unclassified = ifelse(is.na(Unclassified), 0, Unclassified))
    
    
    #### Make count data spatial ####
    elkDatSp <- SpatialPointsDataFrame(
      data.frame("x" = elkDat$UTM_X, "y" = elkDat$UTM_Y),
      elkDat, proj4string = utm)
    
    
    #### Export shapefile of elk counts ####
    writeOGR(elkDatSp,
             dsn = paste0(datDir, "/Elk"),
             layer = "elkCounts_2008-2019",
             driver = "ESRI Shapefile",
             overwrite_layer = TRUE)
    

    #### Account for number of elk counted at each location ####
    elkNums <- elkDat[rep(row.names(elkDat), elkDat$Total), c("UTM_X", "UTM_Y", "Yr")]
    elkNumSp <- SpatialPointsDataFrame(
      data.frame("x" = elkNums$UTM_X, "y" = elkNums$UTM_Y),
      elkNums, proj4string = utm)
    

################################################################################################## #  


### ### ### ### ### ### ### ### ### #
####  | ESTIMATE DISTRIBUTION |  ####
### ### ### ### ### ### ### ### ### #    

    
    ## make 99pct kdes per year
    elkUD <- kernelUD(elkNumSp[,"Yr"], h = "href", grid = 66)
    elkKDE <- getverticeshr(elkUD, percent = 99)
    plot(elkKDE, add = TRUE, col = "red")
    
    ## make 95pct kdes per year
    elkKDEsm <- getverticeshr(elkUD, percent = 95)
    plot(elkKDEsm, add = TRUE, col = "red")    
    
    # export
    writeOGR(elkKDE,
             dsn = paste0(datDir, "/Elk"),
             layer = "elkDistn99_2008-2019",
             driver = "ESRI Shapefile",
             overwrite_layer = TRUE)

    # export
    writeOGR(elkKDEsm,
             dsn = paste0(datDir, "/Elk"),
             layer = "elkDistn95_2008-2019",
             driver = "ESRI Shapefile",
             overwrite_layer = TRUE)    
    
    
################################################################################################## #  
  
    
 

    


        