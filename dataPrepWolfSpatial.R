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
 

    
    #### Wolf data #### 
    
    
    # locations: used & available
    locs <- read.csv("wolfLocs-UsedAvail.csv") 
    locs <- locs %>%
      mutate(wolfYr = as.character(wolfYr),
             Date = ymd(Date),
             Year = substr(wolfYr, nchar(wolfYr) - 3, nchar(wolfYr)))
    
    # winter home ranges
    winHRs <- readOGR(paste0(datDir, "/Wolf"), layer = "winHRswolf")

        
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
      
      # prey availability
      preyUTM <- readOGR(paste0(datDir, "/Elk"), layer = 'elkDistn_2008-2019')
      preyAEA <- spTransform(preyUTM, aea)


      
    #### Metadata ####
      
      # landcover (maps number to type)
      lcLegendRaw <- read.csv(paste0(datDir, "//Land//LandcoverType//NLCD//nlcdLegend.csv")) 
      
      # recreational access
      recRaw <- readOGR(paste0(datDir, "/Human/RecAccess"), layer = 'winterRec')
      recLegendRaw <- recRaw@data

   
    
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
        
        # determine whether wolf had feedground avail within 1km of its winter range
        availFeed <- data.frame(gIntersects(feedUTM, gBuffer(winHRs, byid = TRUE, width = 1000), byid = TRUE))
        availFeed$feedIn <- apply(availFeed, 1, any)
        availFeed$wolfYr <- winHRs$id        
        availFeed <- availFeed %>%
          dplyr::select(wolfYr, feedIn) %>%
          mutate(feedIn = ifelse(feedIn == TRUE, 1, 0))


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

        
                
        
      #### Combine all distances ####
        

        # join all distance values back to main dataframe
        distDat <- locsAEA@data %>%
          mutate(rowNum = rownames(locsAEA@data)) %>%
          left_join(distRd, by = "rowNum") %>%
          left_join(distRdPav, by = "rowNum") %>%
          left_join(distFeed, by = "rowNum") %>%
          left_join(distFeedActive, by = "rowNum") %>%
          left_join(availFeed, by = "wolfYr") %>%
          left_join(distPrey, by = "rowNum") %>%
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
        
    
    #### if not run in full from above: modDatRaw <- read.csv("modDat.csv")
    
    #### combine all covariate data ####
    
      modDatRaw <- distDat %>%
        dplyr::select(rowNum, distRd, distRdPav, distFeed, distFeedActive, feedIn, distStruc, distPrey) %>%
        left_join(extSnow, by = "rowNum") %>%
        dplyr::select(wolfYr, Wolf, Pack, Used, daytime,
                        asp, can, elev, lc, rec, rug, slope, snowCm, 
                        distRd, distRdPav, distFeed, distFeedActive, feedIn, distStruc, distPrey, 
                        datetime, Date, Time, Month, Day, Year,
                        X, Y, Latitude, Longitude, rowNum)
      
    
    #### map landcover values to landcover type ####

        # format and rename landcover classification info
        lcTypes <- lcLegendRaw %>%
          rename(lcVal = Value, lcType = Classification, lcClass = GenericClass) %>%
          dplyr::select(lcVal, lcType, lcClass)

        # add landcover classification info to model data
        modDatLc <- left_join(modDatRaw, lcTypes, by = c("lc" = "lcVal"))
        

        
        
    #### map recreation values to access type ####        
        
    
        # format and rename recreation classification info
        recTypes <- recLegendRaw %>%
          rename(mapCol = MapColor, sled = Over_Snow_, nonmoto = Non_Motori) %>%
          dplyr::select(mapCol, sled, nonmoto) %>%
          # remove NAs (from manual shp updates - this data is contained in the other rows)
          filter(!is.na(sled)) %>%
          distinct()
        
        # map factor levels to access type
        recTypes$recNum <- ifelse(
          recTypes$mapCol == "Blue", 1, 
            ifelse(recTypes$mapCol == "Dark Purple", 2, 
              ifelse(recTypes$mapCol == "Light Purple", 3, 4)))
        recTypes$recClass <- ifelse(
          recTypes$recNum == 4 | recTypes$recNum == 2, "noOT",
            ifelse(recTypes$recNum == 1, "allOT", "nomotoOT"))

    
        # add recreation classification info to model data
        modDat <- left_join(modDatLc, recTypes, by = c("rec" = "recNum"))   
        modDat$recClass <- ifelse(is.na(modDat$recClass), "noRec", modDat$recClass)
        
        
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
      datetime = ymd_hms(datetime),
      # handle datetimes and dates of course
      Date = ymd(Date),
      # format Year as numeric
      Year = as.numeric(Year),
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
      distPreySt = (distPrey - mean(distPrey, na.rm = T))/sd(distPrey, na.rm = T),
      activeFeedSt = (distFeedActive - mean(distFeedActive, na.rm = T))/sd(distFeedActive, na.rm = T),
      # order landcover from most to least available
      lcClass = factor(lcClass, levels = c("Forest", "Shrub", "Herbaceous", "Riparian", "NoVeg")),
      # use open recreation as baseline; reorder for more intuitive plot interpretation
      recClass = factor(recClass, levels = c("allOT", "nomotoOT", "noOT", "noRec")),
      # add binary private land designation
      pvt = ifelse(recClass == "noRec", 1, 0),
      # add binary off-trail/no off-trail use designation
      otUse = ifelse(recClass == "noOT", 0, 1),
      # add binary designation for whether moto rec is allowed
      motoUse = ifelse(recClass == "allOT", 1, 0),
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
        write.csv(modDatUpd, "modDat.csv", row.names = F)
        writeOGR(modDatSp, paste0(datDir, "/Wolf"),
                 layer = "humanInfl-modDat",
                 driver = "ESRI Shapefile",
                 overwrite_layer = TRUE)

        
 save.image(paste0("./RDataFiles/dataPrepWolfSpatial_", today(), ".RData"))
        
 