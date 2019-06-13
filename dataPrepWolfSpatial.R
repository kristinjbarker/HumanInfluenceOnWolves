                      ### ### ### ### ### ###  ### ### ### ### ### ### # 
                      #  COMBINE USED & AVAIL LOCS WITH SPATIAL DATA   #
                      #       TO ASSESS HUMAN INFLUENCE ON WOLF        #
                      #          DISTRIBUTIONS AND BEHAVIORS           #
                      #                                                #
                      #           Kristin Barker | Summer 2019         #
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
    
 

################################################################################################## #  
  
    
    
### ### ### ### ### ### #
####   | RAW DATA |  ####
### ### ### ### ### ### #
 

    
    #### Wolf Locations: Used & Available #### 
    
    locs <- read.csv("wolfLocs-UsedAvail.csv") 
    locs <- locs %>%
      mutate(wolfYr = as.character(wolfYr),
             Date = ymd(Date),
             Year = substr(wolfYr, nchar(wolfYr) - 3, nchar(wolfYr)))

    
    
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
    
    
  ####  ~~ TEMPORARY CODE ~~ just use a few wolf locs to make sure code works. delete this ####
      
    
    # want at least one location per year and would be good to have range of elevs etc
    set.seed(420)
    locs <- sample_n(locs, size = 100, replace = FALSE) 
    locs <- cbind(rowNum = rownames(locs), locs)
    unique(sort(as.numeric(locs$Year)))
    write.csv(locs, "testLocs_subset.csv", row.names = FALSE) # to double-check things in arc
    
    
    
    
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
    
    
    
    #### Kill Sites: Used & Available ####   
    
    kills <- read.csv("killSites.csv")
        
        
     #### Environmental Data #### 
    
      # read in rasters to extract point data from
      files.rast <- list.files(
        path = paste0(datDir, "/xProcessedRasters/"),
        pattern = ".tif$",
        full.names = TRUE)
      rast <- stack(files.rast)
      names(rast)
    
      # snow
      snowRaw <- read.csv(paste0(datDir, "/Environment/swe2019.csv"))
    
      
    
    #### Human and prey data ####
      
      # roads
      motoUTM <- readOGR(paste0(datDir, "/Human/Roads"), layer = 'winterRoads')

      
      # feedgrounds
      feedLL <- readOGR(paste0(datDir, "/Human/Feedgrounds"), layer = 'feedgroundsManualLL')
      feedUTM <- spTransform(feedLL, utm)
      
      # structures
      strucUTM <- readOGR(paste0(datDir, "/Human/Structures"), layer = 'strucsUTM')
      
      # prey availability
      preyUTM <- readOGR(paste0(datDir, "/Elk"), layer = 'elkDistn_2008-2019')
 
   
    
################################################################################################## #  
  
    
    
### ### ### ###  ### ### ### ### 
####   | MANIPULATE DATA |  ####
### ### ### ###  ### ### ### ###
    
    
    #### Make wolf data spatial ####
    
    locsUTM <- SpatialPointsDataFrame(
      data.frame("x" = locs$X, "y" = locs$Y), 
      locs, proj4string = utm)

    
    #### Extract spatial data at each wolf or kill location ####
    
    extRast <- extract(rast, locsUTM, buffer = NULL) # consider buffering points
    
    
    
    ## below code is from TTs dealing with landcover
    ## it extracts lc values per wolf loc and correctly identifies the landcover type
    
        # wolfLocs <- read.csv("wolfLocs-UsedAvail.csv")
        # locsUTM <- SpatialPointsDataFrame(
        #   data.frame("x" = as.numeric(wolfLocs$X), "y" = as.numeric(wolfLocs$Y)),
        #   wolfLocs, proj4string = utm)
        # locsAEA <- spTransform(locsUTM, aea)
        # 
        # extLc <- extract(lcCrop, locsAEA)
        # 
        # locsAEA$lcVal <- extract(lcCrop, locsAEA) # noice
        # 
        # lcTypes <- lcLegendRaw %>%
        #   rename(lcVal = Value, lcType = Classification, lcClass = GenericClass) %>%
        #   dplyr::select(lcVal, lcType, lcClass)
        # 
        # locsAEA@data <- left_join(locsAEA@data, lcTypes, by = "lcVal")

    
    
    #### combine with location data ####
    
    ext <- cbind(extRast, locs)
    

 
    
    
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### 
####      | MEASURE DISTANCES |    ####
### ### ### ### ### ### ### ### ### ### 

        
      ## Motorized roads ##
    
        
        # calculate shortest distance from each point to a road
        distRdRaw <- gDistance(locsUTM, motoUTM, byid = TRUE)
        
    
        # make distance longform
        distRd <- data.frame(
          rowNum = colnames(distRdRaw),
          distRd = distRdRaw[1,])
        
        
      ## Feedgrounds ##      
        
        # calc distance to all feedgrounds
        distFeedRaw <- gDistance(locsUTM, feedUTM, byid = TRUE)
        
        # identify the closest feedground (shortest distance)
        distFeedMin <- apply(distFeedRaw, 2, min)
        
        # make it longform
        distFeed <- data.frame(
          rowNum = names(distFeedMin),
          distFeed = distFeedMin)
        
        
        

      ## Structures ##      
        
        # calc distance to all structures
        distStrucRaw <- gDistance(locsUTM, strucUTM, byid = TRUE)
        
        # identify the shortest distance
        distStrucMin <- apply(distStrucRaw, 2, min)
        
        # make it longform
        distStruc <- data.frame(
          rowNum = names(distStrucMin),
          distStruc = distStrucMin)  
        
        
        
      ## Prey availability ##
        
        # create blank df to store results
        distPrey <- data.frame(rowNum = NA, distPrey = NA)
        distPrey <- distPrey[-1,]
   
        
        # use prey availability specific to the year when the location was recorded
        yrs <- unique(locs$Year)
        
        for (i in 1:length(yrs)) {
          
          # pull polygon of elk distribution during that year
          iYr <- yrs[i]
          iElk <- preyUTM[preyUTM$id == iYr, ]
          iLocs <- locsUTM[locsUTM$Year == iYr, ]

          # calculate distance to elk polygon
          distElkRaw <- gDistance(iLocs, iElk, byid = TRUE)
          
          # make it longform
          distElk <- data.frame(
            rowNum = colnames(distElkRaw),
            distPrey = distElkRaw[1,])
        
          # join to master
          distPrey <- rbind(distPrey, distElk) 
          
        }
        
        
                
        
      ## All distances ##
        
        
        # join all distance values back to main dataframe
        distDat <- locsUTM@data %>%
          left_join(distRd, by = "rowNum") %>%
          left_join(distFeed, by = "rowNum") %>%
          left_join(distStruc, by = "rowNum") %>%
          left_join(distPrey, by = "rowNum")

    

################################################################################################## #  
  
    
### ### ### ### ### ### # 
####  | ADD SNOW  |  ####
### ### ### ### ### ### #
        
        
    ## based on elevation of point, closest elevation of SWE station, and date

    ## identify snotel stations and their elevations
    stations <- snowRaw %>%
      dplyr::select(staAbbv, elevM) %>%
      distinct()
     
    ## format snow data (date as such)   
    snow <- snowRaw %>%
      mutate(Date = ymd(Date))
  
    
    ## placeholder
    modDat <- ext %>%
      mutate(SWE = NA)
    
    
    
    
    #### KRISTIN YOU LEFT OFF HERE ####
    
    
      ## loop works (yay) 
      ## but errors out because some locs have NA eev ##
    
    

      ## so next step is to check this in arcmap and figure out the NA extraction issue ##    
    z <- distDat %>%
      dplyr::select(rowNum, distRd, distFeed, distStruc, distPrey) %>%
      left_join(ext, by = "rowNum")
    write.csv(z, file = "testDat.csv", row.names = FALSE)
    
    
    
    
    ## for each used & available wolf location   
    for(i in 1:34) {
   # for(i in 1:nrow(modDat)) {
      
      # identify its date and elevation
      iDate <- modDat[i, "Date"]
      iElev <- modDat[i, "elev"]
      
      # find the snotel site closest in elevation
      iSta <- stations[which(abs(stations$elevM-iElev)==min(abs(stations$elevM-iElev))), "staAbbv"]
      
      # find value for that site and date
      iSWE <- snow[which(snow$staAbbv == iSta & snow$Date == iDate), "SWE"]
      
      # add the value to the dataframe
      modDat[i, "SWE"] <- iSWE
      
    }

    ####     
        
     