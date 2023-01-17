                      ### ### ### ### ### ###  ### ### ### ### ### ### # 
                      #           PROCESS KILL SITE DATA               #
                      #     AND GENERATE RANDOM AVAILABLE LOCATIONS    #
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
      "adehabitatHR",   ## kernel density estimator
      "lubridate",     ## manipulate datetime data inside dplyr pipes
      "googlesheets",  # talk to our online database
      "dplyr")         ## data manipulation and general awesomeness
      
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

    
    #### Cluster data  ####
    
      # from fieldDataSummaries.R, possibly w manual addition of older centroid locations
      clusDatRaw <- read.csv("../PredationStudy/Clusters/clusterData.csv")
      
        
     #### Raster and snowtel data #### 
    
    
      # stack to extract data from (aspect, canopy, elev, landcov, rec, ruggedness, slope)
      files.rast <- list.files(
        path = paste0(datDir, "/xProcessedRasters/"),
        pattern = "^AEA.*tif$",
        full.names = TRUE)
      rast <- stack(files.rast)
      # remove "AEA" from raster names
      names(rast) <- substr(names(rast), 4, nchar(names(rast)))
    
      # snow (snow depth and snow water equivalence from snotel sites in study area)
      snowRaw <- read.csv(paste0(datDir, "/Environment/snowDat_2005-2019.csv"))
    
      
    
    #### Human and prey data ####
      
      # roads
      motoUTM <- readOGR(paste0(datDir, "/Human/Roads"), layer = 'winterRoads')
      motoAEA <- spTransform(motoUTM, aea)
      motoPavUTM <- readOGR(paste0(datDir, "/Human/Roads"), layer = 'winterRoadsPaved')
      motoPavAEA <- spTransform(motoPavUTM, aea)

      
      # feedgrounds
      feedLL <- readOGR(paste0(datDir, "/Human/Feedgrounds"), layer = 'feedgroundsManualLL')
      feedAEA <- spTransform(feedLL, aea)
      feedUTM <- spTransform(feedLL, utm)
      feedActiveLL <- readOGR(paste0(datDir, "/Human/Feedgrounds"), layer = 'feedgroundsActiveManualLL')
      feedActiveAEA <- spTransform(feedActiveLL, aea)
      feedActiveUTM <- spTransform(feedActiveLL, utm)
      
      # structures
      strucLL <- readOGR(paste0(datDir, "/Human/Structures"), layer = 'strucsLL')
      

    #### Metadata ####
      
      # landcover (maps number to type)
      lcLegendRaw <- read.csv(paste0(datDir, "//Land//LandcoverType//NLCD//nlcdLegend.csv")) 

    
            
################################################################################################## #  

    
### ### ### ### ### ### ### ### ### ## 
####   | USED & AVAILABLE SITES|  ####
### ### ### ### ### ### ### ### ### ##
    
    
    # format used locations to combine with available locations (and to use existing wolf code)
    clusDat <- clusDatRaw %>%
      rename(X = cUTMX, Y = cUTMY, Pack = pack) %>%
      mutate(Date = dmy(visitDate),
             kill = ifelse(grepl("kill", clusDat$clusType), 1, 0),
             Used = 1)
    
    # format kill site-only data
    killDat <- clusDat %>%
      filter(kill == 1)

    # make it spatial
    clusDatSp <- SpatialPointsDataFrame( 
                   data.frame("x" = clusDat$X, "y" = clusDat$Y),
                   clusDat, proj4string = utm)
    plot(clusDatSp)
    
    # delineate 'available' area (95% kde of all cluster locs - not just kills)
    clusHR <- getverticeshr(kernelUD(clusDatSp, h = "href"), percent = 95)
    plot(clusHR, add = T)
    
    # generate available locations at 1:1 ratio used:available
    avail <- spsample(clusHR, length(which(clusDat$kill == 1)), "random")
    plot(avail, add = T, col = "blue")
    

    
    # format random locations to combine with recorded locations
    availDat <- data.frame(avail)
    colnames(availDat) <- c("X", "Y")
    availDat$Used <- 0
    availDat$Pack <- killDat$Pack
    
    # randomly assign dates
    availDat$Date <- sample(clusDat$Date, size = nrow(availDat), replace = T)
    
    
    # combine used and available kill sites
    killUA <- full_join(killDat, availDat)
    nrow(killUA); nrow(killDat) + nrow(availDat) # sanity check
    
    # make it spatial
    killUASp <- SpatialPointsDataFrame(
      data.frame("x" = killUA$X, "y" = killUA$Y),
      killUA, proj4string = utm)
    
    # export
    write.csv(killUA, "killSiteDat.csv", row.names = F)
    writeOGR(killUASp, paste0(datDir, "/Wolf"),
             layer = "killSites-usedAvail",
             driver = "ESRI Shapefile",
             overwrite_layer = TRUE)
    
    
    
    
################################################################################################## #  


### ### ### ### ### ### ### ### ###  
####  | EXTRACT SPATIAL DATA|  ####
### ### ### ### ### ### ### ### ### 
    
    
    locs <- killUA # rename dataframe to use existing code
          
    
    #### Match wolf data projection to spatial data projection
      
      locsAEA <- spTransform(killUASp, aea)

    
    #### Extract spatial data at each wolf or kill location ####
    
    extRast <- extract(rast, locsAEA, buffer = NULL) # consider buffering points
    
    
    #### combine with location data ####
    
    ext <- cbind(extRast, locs)
    
    
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### 
####      | MEASURE DISTANCES |    ####
### ### ### ### ### ### ### ### ### ### 

        
      #### Motorized routes ####
    
        
        # calculate shortest distance from each point to a road
        distRdRaw <- gDistance(locsAEA, motoAEA, byid = TRUE)

        # make longform
        distRd <- data.frame(
          rowNum = colnames(distRdRaw),
          distRd = distRdRaw[1,])
        
        # calculate shortest distance from each point to a paved, plowed road
        distRdPavRaw <- gDistance(locsAEA, motoPavAEA, byid = TRUE)
        
        # make longform
        distRdPav <- data.frame(
          rowNum = colnames(distRdPavRaw),
          distRdPav = distRdPavRaw[1,])        
        
      
        
      #### Feedgrounds ####      
        
        # calc distance to all feedgrounds
        distFeedRaw <- gDistance(locsAEA, feedAEA, byid = TRUE)
        
        # identify the closest of any feedground (shortest distance)
        distFeedMin <- apply(distFeedRaw, 2, min)

        # make it longform
        distFeed <- data.frame(
          rowNum = names(distFeedMin),
          distFeed = distFeedMin)
        
        # calc distance to consistently-active feedgrounds
        distFeedActiveRaw <- gDistance(locsAEA, feedActiveAEA, byid = TRUE)
        
        # identify the closest of any feedground (shortest distance)
        distFeedActiveMin <- apply(distFeedActiveRaw, 2, min)

        # make it longform
        distFeedActive <- data.frame(
          rowNum = names(distFeedActiveMin),
          distFeedActive = distFeedActiveMin)


      #### Structures ### 
        
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
        
        
     #### Combine all distances ####
        

        # join all distance values back to main dataframe
        distDat <- locsAEA@data %>%
          mutate(rowNum = rownames(locsAEA@data)) %>%
          left_join(distRd, by = "rowNum") %>%
          left_join(distRdPav, by = "rowNum") %>%
          left_join(distFeed, by = "rowNum") %>%
          left_join(distFeedActive, by = "rowNum") %>%
          left_join(distStruc, by = "rowNum") 
   

################################################################################################## #  
  
    
### ### ### ### ### ### # 
####  | ADD SNOW  |  ####
### ### ### ### ### ### #
        
        
    ## based on elevation of point, closest elevation of snotel station, and date

    ## identify snotel stations and their elevations
    stations <- snowRaw %>%
      dplyr::select(staAbbv, elevM) %>%
      distinct()
     
    ## format snow data (date as date)   
    snow <- snowRaw %>%
      mutate(Date = ymd(Date))

    ## create new dataframe to combine snow values with extracted raster values
    extSnow <- ext %>%
      mutate(snowCm = NA, rowNum = rownames(ext))

    
    ## for each used & available wolf location   
    for(i in 1:nrow(extSnow)) {
      
      # identify its date and elevation
      iDate <- extSnow[i, "Date"]
      iElev <- extSnow[i, "elev"]
      
      # find the snotel site closest in elevation
      iSta <- stations[which(abs(stations$elevM-iElev)==min(abs(stations$elevM-iElev))), "staAbbv"]
      
      # if prior to jan 2013 and closest snotel site was granite, use gros ventre instead (next closest in elev) 
      iStaUpd <- ifelse(iSta == "gc" & iDate <= "2013-01-01", "gv", paste(iSta))
      
      # find value for that site and date
      iSnow <- snow[which(snow$staAbbv == iStaUpd & snow$Date == iDate), "snowCm"]
      
      # add the value to the dataframe
      extSnow[i, "snowCm"] <- iSnow
      
    }

 

################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### ### ### ##
####  | COMBINE AND FORMAT COVARIATE DATA  |  ####
### ### ### ### ### ### ### ### ### ### ### ### ##

    
    #### combine all covariate data ####
    
      modDat <- distDat %>%
        dplyr::select(rowNum, distRd, distRdPav, distFeed, distFeedActive, distStruc) %>% 
        left_join(extSnow, by = "rowNum") 
    
    #### map landcover values to landcover type ####

        # format and rename landcover classification info
        lcTypes <- lcLegendRaw %>%
          rename(lcVal = Value, lcType = Classification, lcClass = GenericClass) %>%
          dplyr::select(lcVal, lcType, lcClass)

        # add landcover classification info to model data
        modDat <- left_join(modDat, lcTypes, by = c("lc" = "lcVal"))
        


        
    #### format aspect ####
        
        # translate to northness (continuous and binary) and make 0 if slope = 0
        modDat$northness <- cos(modDat$asp)
        modDat$northness <- ifelse(modDat$slope == 0, 0, modDat$northness)
        modDat$north <- ifelse(modDat$northness > 0, 1, 0)
        
        # translate to eastness (continuous and binary) and make 0 if slope = 0
        modDat$eastness <- sin(modDat$asp)
        modDat$eastness <- ifelse(modDat$slope == 0, 0, modDat$eastness)
        modDat$east <- ifelse(modDat$eastness > 0, 1, 0)
        
        # if slope = 0, set aspect(s) to 0
        modDat$northness <- ifelse(modDat$slope == 0, 0, modDat$northness)
        modDat$north <- ifelse(modDat$slope == 0, 0, modDat$north)
        modDat$eastness <- ifelse(modDat$slope == 0, 0, modDat$eastness)
        modDat$east <- ifelse(modDat$slope == 0, 0, modDat$east)

  
    #### format covariates for use in models ####

    
    modDatUpd <- modDat %>% mutate(
      Year = year(Date),
      # standardize continuous covariates
      slopeSt = (slope - mean(slope))/sd(slope),
      elevSt = (elev - mean(elev))/sd(elev),
      northnessSt = (northness - mean(northness))/sd(northness),
      snowSt = (snowCm - mean(snowCm))/sd(snowCm),
      canSt = (can - mean(can))/sd(can),
      distRdSt = (distRd - mean(distRd))/sd(distRd),
      distRdPavSt = (distRdPav - mean(distRdPav))/sd(distRdPav),
      distStrucSt = (distStruc - mean(distStruc))/sd(distStruc),
      distFeedSt = (distFeed - mean(distFeed, na.rm = T))/sd(distFeed, na.rm = T),
      activeFeedSt = (distFeedActive - mean(distFeedActive, na.rm = T))/sd(distFeedActive, na.rm = T),
      # order landcover from most to least available
      lcClass = factor(lcClass, levels = c("Forest", "Shrub", "Herbaceous", "Riparian", "NoVeg")),
      # add binary indicator of whether hunting was allowed that fall
      hunt = ifelse(Year == 2013 | Year == 2014 | Year >= 2018, 1, 0),
      # add binary indication of whether hunting was allowed in the previous year
      prevHunt = ifelse(Year == 2014 | Year == 2015 | Year >= 2019, 1, 0),
      # add time since hunting was first allowed (fall 2012, corresponds to winter 2013)
      tSinceHunt = ifelse(Year - 2013 + 1 < 0, 0, Year - 2013 + 1),
      # add years of continuous hunting (to account for no hunting 2014, 2015, 2016)
      tContHunt = ifelse(Year == 2013, 1,
                         ifelse(Year == 2014, 2,
                                ifelse(Year >= 2018, Year - 2018 + 1, 0))))
        
        
    #### finish him ####
        
        
        ## make it spatial
        modDatSp <- SpatialPointsDataFrame(
          data.frame("x" = modDatUpd$X, "y" = modDatUpd$Y),
          modDatUpd, proj4string = utm)
        
        
        ## export
        write.csv(modDatUpd, "killDat.csv", row.names = F)
        writeOGR(modDatSp, paste0(datDir, "/Wolf"),
                 layer = "killSites-modDat",
                 driver = "ESRI Shapefile",
                 overwrite_layer = TRUE)

        
 save.image(paste0("dataPrepKillSites_", today(), ".RData"))
         
            
 
   

    

    
    
    

    