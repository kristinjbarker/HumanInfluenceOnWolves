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
      "raster",        ## for parts of justin clapp's code
      "maps",          ## for parts of justin clapp's code
      "maptools",      ## for cluster algorithm & kmlPoints
      "rgdal",         ## for cluster algorithm & spatial/shapefile work 
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
    
    
    #### Kill Sites: Used & Available ####   
    
    kills <- read.csv("killSites.csv")
        
        
    #### Environmental Data #### 
    
      # just using one for now to practice
      spat <- stack("../Data/Land/testStack.grd")
    
      # snow
      snowRaw <- read.csv(paste0(datDir, "/Environment/swe2019.csv"))
    
    
    #### Human Data ####
    
    
    
    
    
    
    
################################################################################################## #  
  
    
    
### ### ### ###  ### ### ### ### 
####   | MANIPULATE DATA |  ####
### ### ### ###  ### ### ### ###
    
    
    #### Make wolf data spatial ####
    
    locsUTM <- SpatialPointsDataFrame(
      data.frame("x" = locs$X, "y" = locs$Y), 
      locs, proj4string = utm)
    locsSpatAEA <- spTransform(locsSpat, crs(spat)) 

    
    
    
    #### Extract spatial data at each wolf or kill location ####
    
    extSpat <- extract(spatStack, locsSpatAEA, buffer = NULL) # consider buffering points
    
    
    
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
    
    ext <- cbind(extSpat, locs)
    

 
    
    
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### 
####      | MEASURE DISTANCES |    ####
# to roads, feedgrounds, & structures #
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
          rowNum = colnames(distFeedMin),
          distRd = distFeedMin[1,])
        
        
        
      ## Feedgrounds ##      
        
        # calc distance to all feedgrounds
        distFeedRaw <- gDistance(locsUTM, feedUTM, byid = TRUE)
        
        # identify the closest feedground (shortest distance)
        distFeedMin <- apply(distFeedRaw, 2, min)
        
        # make it longform
        distFeed <- data.frame(
          rowNum = colnames(distFeedMin),
          distFeed = distFeedMin[1,])        
        
        
      ## Structures ##      
        
        # calc distance to all structures
        distStrucRaw <- gDistance(locsUTM, as(strucUTM, 'Spatial'), byid = TRUE)
        
        # identify the shortest distance
        distStrucMin <- apply(distStrucRaw, 2, min)
        
        # make it longform
        distStruc <- data.frame(
          rowNum = colnames(distStrucMin),
          distStruc = distStrucMin[1,])        
        
        
                
        
      ## All distances ##
        
        
        # join all distance values back to main dataframe
        distDat <- locsUTM@data %>%
          left_join(distRd, by = "rowNum") %>%
          left_join(distFeed, by = "rowNum") %>%
          left_join(distStruc, by = "rowNum")

    

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
    modDat <- whateverYouCallThatDf %>%
      mutate(SWE = NA)
    
    
    ## for each used & available wolf location    
    for(i in 1:nrow(modDat)) {
      
      # identify its date and elevation
      iDate <- modDat[i, "Date"]
      iElev <- modDat[i, "Elev"]
      
      # find the snotel site closest in elevation
      iSta <- stations[which(abs(stations$elevM-iElev)==min(abs(stations$elevM-iElev))), "staAbbv"]
      
      # find value for that site and date
      iSWE <- snow[which(snow$staAbbv == sta & snow$Date == iDate), "SWE"]
      
      # add the value to the dataframe
      modDat[i, "SWE"] <- iSWE
      
    }

    ####     
        
     