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
    
    
    
  #### Increase memory limit ####
    
    memory.limit(size = 7500000) 

    
    
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


    
    #### Kill Sites: Used & Available ####   
    
    # kills <- read.csv("killSites.csv")
        
    
        
     #### Raster and snowtel data #### 
    
    
      # stack to extract data from (aspect, canopy, elev, landcov, rec, ruggedness, slope)
      files.rast <- list.files(
        path = paste0(datDir, "/xProcessedRasters/"),
        pattern = "^AEA.*tif$",
        full.names = TRUE)
      rast <- stack(files.rast)
      # remove "AEA" from raster names
      names(rast) <- substr(names(rast), 4, nchar(names(rast)))
    
      # snow (snow water equivalence from snotel sites in study area)
      snowRaw <- read.csv(paste0(datDir, "/Environment/swe2019.csv"))
    
      
    
    #### Human and prey data ####
      
      # roads
      motoUTM <- readOGR(paste0(datDir, "/Human/Roads"), layer = 'winterRoads')
      motoAEA <- spTransform(motoUTM, aea)

      
      # feedgrounds
      feedLL <- readOGR(paste0(datDir, "/Human/Feedgrounds"), layer = 'feedgroundsManualLL')
      feedAEA <- spTransform(feedLL, aea)
      
      # structures
      strucLL <- readOGR(paste0(datDir, "/Human/Structures"), layer = 'strucsLL')
      
      # prey availability
      preyUTM <- readOGR(paste0(datDir, "/Elk"), layer = 'elkDistn_2008-2019')
      preyAEA <- spTransform(preyUTM, aea)
      
      
      
    #### Metadata ####
      
      # landcover (maps number to type)
      lcLegendRaw <- read.csv(paste0(datDir, "//Land//LandcoverType//NLCD//nlcdLegend.csv")) 

   
    
################################################################################################## #  
  
    
    
### ### ### ###  ### ### ### ### 
####   | MANIPULATE DATA |  ####
### ### ### ###  ### ### ### ###
    
    
    #### Make wolf data spatial ####
    
    locsUTM <- SpatialPointsDataFrame(
      data.frame("x" = locs$X, "y" = locs$Y), 
      locs, proj4string = utm)
      
    #### Match wolf data projection to spatial data projection
      
      locsAEA <- spTransform(locsUTM, aea)

    
    #### Extract spatial data at each wolf or kill location ####
    
    extRast <- extract(rast, locsAEA, buffer = NULL) # consider buffering points
    
    
    #### combine with location data ####
    
    ext <- cbind(extRast, locs)


    
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### 
####      | MEASURE DISTANCES |    ####
### ### ### ### ### ### ### ### ### ### 

        
      ## Motorized roads ##
    
        
        # calculate shortest distance from each point to a road
        distRdRaw <- gDistance(locsAEA, motoAEA, byid = TRUE)
        
    
        # make distance longform
        distRd <- data.frame(
          rowNum = colnames(distRdRaw),
          distRd = distRdRaw[1,])
        
        
      ## Feedgrounds ##      
        
        # calc distance to all feedgrounds
        distFeedRaw <- gDistance(locsAEA, feedAEA, byid = TRUE)
        
        # identify the closest feedground (shortest distance)
        distFeedMin <- apply(distFeedRaw, 2, min)
        
        # make it longform
        distFeed <- data.frame(
          rowNum = names(distFeedMin),
          distFeed = distFeedMin)
        
        
        

      ## Structures ## 
        
        # transform to match projection of other spatial data
        strucAEA <- spTransform(strucLL, aea)
        
        # aggregate structure polygons to reduce computing power needed
        strucAgg <- unionSpatialPolygons(
          SpatialPolygons(strucAEA@polygons), 
          rep("1", times = length(strucAEA)))
        
        # and crop to slightly-buffered extent of wolf locs to further reduce size
        strucCrop <- crop(strucAgg, extent(locsAEA) + 100)

        # calc shortest distance from each point to a structure
        distStrucRaw <- gDistance(locsAEA, strucCrop, byid = TRUE)
        

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
          
          # identify year (but for early wolves, use 2008 elk distn bc that's earliest i have)
          iYr <- yrs[i]
          iYr2 <- ifelse(iYr < 2008, 2008, iYr)
          
          # pull polygon of elk distribution during that year
          iElk <- preyAEA[preyAEA$id == iYr2, ]
          iLocs <- locsAEA[locsAEA$Year == iYr, ]

          # calculate distance to elk polygon
          distElkRaw <- gDistance(iLocs, iElk, byid = TRUE)
            
          # identify the shortest distance
          distElkMin <- apply(distElkRaw, 2, min)
        
          # make it longform
          distElk <- data.frame(
            rowNum = names(distElkMin),
            distPrey = distElkMin)
        
          # join to master
          distPrey <- rbind(distPrey, distElk) 
          
        }
        
        
                
        
      ## All distances ##
        
        
        # join all distance values back to main dataframe
        distDat <- locsAEA@data %>%
          mutate(rowNum = rownames(locsAEA@data)) %>%
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

    ## create new dataframe to combine snow values with extracted raster values
    extSnow <- ext %>%
      mutate(swe = NA, rowNum = rownames(ext))

    
    ## for each used & available wolf location   
    for(i in 1:nrow(extSnow)) {
      
      # identify its date and elevation
      iDate <- extSnow[i, "Date"]
      iElev <- extSnow[i, "elev"]
      
      # find the snotel site closest in elevation
      iSta <- stations[which(abs(stations$elevM-iElev)==min(abs(stations$elevM-iElev))), "staAbbv"]
      
      # find value for that site and date
      iSWE <- snow[which(snow$staAbbv == iSta & snow$Date == iDate), "SWE"]
      
      # add the value to the dataframe
      extSnow[i, "swe"] <- iSWE
      
    }

    

################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### ### ### ##
####  | COMBINE AND FORMAT COVARIATE DATA  |  ####
### ### ### ### ### ### ### ### ### ### ### ### ##
        
    
    #### if not run in full from above: modDatRaw <- read.csv("modDat.csv")
    
    #### combine all covariate data ####
    
      modDatRaw <- distDat %>%
        dplyr::select(c(rowNum, distRd, distFeed, distStruc, distPrey)) %>%
        left_join(extSnow, by = "rowNum") %>%
        dplyr::select(c(wolfYr, Wolf, Pack, Used, 
                        asp, can, elev, lc, rec, rug, slope, swe, 
                        distRd, distFeed, distStruc, distPrey, 
                        datetime, Date, Time, Month, Day, Year,
                        X, Y, Latitude, Longitude, rowNum))
      
    
    #### map landcover values to landcover type ####

        # format and rename landcover classification info
        lcTypes <- lcLegendRaw %>%
          rename(lcVal = Value, lcType = Classification, lcClass = GenericClass) %>%
          dplyr::select(lcVal, lcType, lcClass)

        # add landcover classification info to model data
        modDat <- left_join(modDatRaw, lcTypes, by = c("lc" = "lcVal"))

        
        
        
        
        #### ~~ KRISTIN YOU LEFT OFF HERE ~~ ####        
    
        
        
        
    #### map rec access values to restrictions ####

        
        # you can pull these values from the rec data, but not positive if it's read in here or not
        
    
    #### translate aspect to something more usable? maybe ask owen about this ####
        
        
        
        
    #### finish him ####
        
        
        ## make it spatial
        modDatSp <- SpatialPointsDataFrame(
          data.frame("x" = modDat$X, "y" = modDat$Y),
          modDat, proj4string = utm)
        
        
        ## export
        write.csv(modDat, "modDat.csv", row.names = F)
        writeOGR(modDatSp, paste0(datDir, "/Wolf"),
                 layer = "humanInfl-modDat",
                 driver = "ESRI Shapefile",
                 overwrite_layer = TRUE)
