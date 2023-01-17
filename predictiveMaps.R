           ### ### ### ### ### ###  ### ### ### ### ### ### ### ### ##
           #                                                         #
           #          PREDICTIVE MAPS OF WOLF DISTRIBUTIONS          #
           #     IN RELATION TO ENVIRONMENT AND HUMAN INFLUENCES     #
           #                                                         #
           #                Kristin Barker | Fall 2019               #
           #                  kristinjbarker@gmail.com               #
           #                                                         #
           ### ### ### ### ### ###  ### ### ### ### ### ### ### ### ## 


################################################################################################## #


### ### ### ### ### #
####  | SETUP |  ####
### ### ### ### ### #



  #### Set working directory and filepath to spatial data ####

    wd_laptop <- "C:\\Users\\Kristin\\Box Sync\\Documents\\HumanInfluenceOnWolves"
    wd_desktop <- "C:\\Users\\krist\\Documents\\HumanInfluenceOnWolves"
    if (file.exists(wd_laptop)) { 
      setwd(wd_laptop); datDir <- "C:\\Users\\Kristin\\Box Sync\\Documents\\Data"; memory.limit(size = 9999999999999) 
    } else {
      setwd(wd_desktop); datDir <- "C:\\Users\\krist\\Documents\\Data"
    }
    rm(wd_laptop, wd_desktop)      

  

  #### Install and load any necessary packages you don't already have ####
    
    # list of packages needed
    packages <- c(
      "lme4",           ## regression models
      "raster",         ## raster stacking, extraction, etc
      "rgdal",          ## spatial/shapefile work
      "rgeos",          ## calculate distances
      "sf",             ## calculate distances more quickly?
      "ggplot2",        ## plots
      "lubridate",      ## manipulate datetime data inside dplyr pipes
      "dplyr")          ## data manipulation and general awesomeness
    
    
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
####  | DATA PREP |  ####
### ### ### ### ### ### #
 
    
    #### Model data ####
    
    
      # Raw data file
      modDatRaw <- read.csv("modDat.csv")


      # Formatted model data 
      modDat <- modDatRaw %>% mutate(
        datetime = ymd_hms(datetime, tz = "America/Denver"),
        # handle datetimes and dates of course
        Date = ymd(Date),
        # year as numeric
        Year = as.numeric(Year),
        # order landcover from most to least available
        lcClass = factor(lcClass, levels = c("Forest", "Shrub", "Herbaceous", "Riparian", "NoVeg")))
  
      # Split out day/night/crepuscular time periods
      modDatDay <- filter(modDat, daytime == "day")  
      modDatNight <- filter(modDat, daytime == "night")
      modDatCrep <- filter(modDat, daytime == "crep")
      

      
    #### Existing environmental data ####    
      

      # Read in and stack previously-processed environmental rasters
      files.rast <- list.files(path = paste0(datDir, "/xProcessedRasters/"),
        pattern = "^AEA.*tif$",full.names = TRUE)
      rast <- stack(files.rast)
      
      # remove "AEA" from raster names
      names(rast) <- substr(names(rast), 4, nchar(names(rast)))
      
      # remove covariates that aren't included in top models (recreation and ruggedness)
      rast <- dropLayer(rast, c("rec", "rug"))
      
      # rename rasters to match covariate names
      names(rast) <- gsub(pattern = "asp", replacement = "northness", x = names(rast))
      names(rast) <- gsub(pattern = "lc", replacement = "lcClass", x = names(rast))
      names(rast) # sanity check
      

      
    #### Snow data ####        
        
        
      # Snow depth from Snotel sites in study area)
      snowRaw <- read.csv(paste0(datDir, "/Environment/snowDat_2005-2019.csv"))
      
      # Determine average, high, and low snow depth for each site
      snowSum <- snowRaw %>%
        filter(snowCm > 0 & !is.na(snowCm)) %>%
        group_by(station) %>%
        summarise(staAbbv = unique(staAbbv),
                  elevM = unique(elevM),
                  avgSnow = mean(snowCm),
                  lowSnow = quantile(snowCm, 0.25),
                  highSnow = quantile(snowCm, 0.75))
      
      # Calculate elevation cutoffs for association with each weather station
      eGC <- as.numeric(snowSum[snowSum$staAbbv == "gc", "elevM"]) # grant ck
      eGV <- as.numeric(snowSum[snowSum$staAbbv == "gv", "elevM"]) # gros ventre
      eTG <- as.numeric(snowSum[snowSum$staAbbv == "tg", "elevM"]) # togwotee
      gv <- eGC+((eGV-eGC)/2) # below this = grant creek
      tg <- eGV+((eTG-eGV)/2) # above this = togwotee
      

      # Assign values for low snow year based on elevation of raster cell & weather stations
      snowLow <- rast[["elev"]]
      values(snowLow) <- ifelse(values(snowLow) < gcgv, as.numeric(snowSum[snowSum$staAbbv == "gc", "lowSnow"]), 
                             ifelse(values(snowLow) > gvtg, as.numeric(snowSum[snowSum$staAbbv == "tg", "lowSnow"]), 
                                    as.numeric(snowSum[snowSum$staAbbv == "gv", "lowSnow"])))     
      
      # Assign values for average snow year based on elevation of raster cell & weather stations
      snowAvg <- rast[["elev"]]
      values(snowAvg) <- ifelse(values(snowAvg) < gcgv, as.numeric(snowSum[snowSum$staAbbv == "gc", "avgSnow"]), 
                             ifelse(values(snowAvg) > gvtg, as.numeric(snowSum[snowSum$staAbbv == "tg", "avgSnow"]), 
                                    as.numeric(snowSum[snowSum$staAbbv == "gv", "avgSnow"])))   
      
      # Assign values for high snow year based on elevation of raster cell & weather stations
      snowHigh <- rast[["elev"]]
      values(snowHigh) <- ifelse(values(snowHigh) < gcgv, as.numeric(snowSum[snowSum$staAbbv == "gc", "highSnow"]), 
                             ifelse(values(snowHigh) > gvtg, as.numeric(snowSum[snowSum$staAbbv == "tg", "highSnow"]), 
                                    as.numeric(snowSum[snowSum$staAbbv == "gv", "highSnow"])))         

      
      
      
    
################################################################################################## #  
  
    #### Human data ####
      
      # roads
      motoUTM <- readOGR(paste0(datDir, "/Human/Roads"), layer = 'winterRoads')
      motoAEA <- spTransform(motoUTM, aea)

      # structures
      strucLL <- readOGR(paste0(datDir, "/Human/Structures"), layer = 'strucsLL')
      strucAEA <- spTransform(strucLL, aea)
      
      # # base raster converted to spatial points (to calculate distances)
      rastPts <- as(raster(paste0(datDir, "/xProcessedRasters/AEAelev.tif")), "SpatialPoints")

      
      # 
      # # New raster for distance to roads 
      # 
      #     # measure distances from raster cells to roads 
      #     distRdMatrix <- gDistance(motoAEA, as(rast[["elev"]], "SpatialPoints"), byid = TRUE)
      #     
      #     # create raster of minimum value per cell
      #     distRd <- rast[["elev"]]
      #     distRd[] <- apply(distRdMatrix, 1, min)
      #     
      #     # export
      #     writeRaster(distRd, 
      #                 paste0(datDir, "/xProcessedRasters/distRd"), 
      #                 format = "GTiff",
      #                 overwrite = TRUE)
      #     
      #     # fre up memory for distance to structures calculations
      #     rm(distRdMatrix, distRd); gc()
      #     # jk that doesn't fix it
      # 

      
      # New raster for distance to structures
          
# attempt 1: make buildings points
      

      # make structures points in sf format
      strucCent <- st_centroid(st_as_sf(strucAEA))

      
      # get base raster into sf format
      pix <- st_as_sf(as(raster(paste0(datDir, "/xProcessedRasters/AEAelev.tif")), "SpatialPixelsDataFrame"))
      
      # get index of closest building to each raster cell, i think/hope
      nearest <- st_nearest_feature(x = pix, y = strucCent) 
      
      # slice based on the index
      nearestStruc <- strucCent %>% slice(nearest)
      
      # clear out some memory, hopefully
      rm(nearest); gc()
      
      # calculate distance from each pixel to the building that was closest
      pixDist <- st_distance(x = pix, y= nearestStruc, by_element = TRUE)
      
      # add the distance calculations to the pixels
      pix$distance <- pixDist
      
      
      
      
      # ---> make it back into a raster -- KRISTIN RUN THIS ONE OVER NIGHT/LAUNDRY
      distStruc <- raster(pix, vals = as.numeric(distance))

      
      # export
      
          
      
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ##
####  | FAILS |  ####
### ### ### ### ### ### ### ##
      
      

      distBldg <- raster(pix)
      plot(distBldg) ## ERROR NO VALUES, BUMMER!
      extent(distBldg); crs(distBldg)
      # but raster is set up; maybe just need to get the values in it?
      extent(raster(paste0(datDir, "/xProcessedRasters/AEAelev.tif"))); crs(raster(paste0(datDir, "/xProcessedRasters/AEAelev.tif")))
      # doesn't exactly match the extent correctly tho so don't use it
      
      
      
      
      
      
            
      save.image("predMapData-distStruc.RData")
      

          # export
          writeRaster(distBldg,
                      paste0(datDir, "/xProcessedRasters/distStruc"),
                      format = "GTiff",
                      overwrite = TRUE)
            
      # Hunt data
      
      
      

      
      
      
      # Stack all data and name to match covariate names

      
      
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ##
####  | SPECIFY MODELS |  ####
### ### ### ### ### ### ### ##
      
      
      ## use unstandardized covariates for prediction
      
      modDay <- glmer(Used ~ 1 + can + slope + elev + northness + snow + I(slope^2) + I(elev^2) + 
                        I(northness^2) + snow:can + snow:northness + snow:elev + snow:I(elev^2) +
                        distRd + distStruc + distFeed + I(distRd^2) + I(distStruc^2) + I(distFeed^2) +
                        hunt*distRd + hunt*distStruc + hunt*distFeed + hunt*I(distRd^2) + 
                        hunt*I(distStruc^2) + hunt*I(distFeed^2) + (1|Pack), 
                      data = modDatDay, family = binomial(logit),
                      control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4)))
      
      
      modNight <- glmer(Used ~ 1 + lcClass + can + slope + elev + northness + snow + I(slope*slope) + 
                          I(elev*elev) + I(northness*northness) + snow:can + snow:elev + snow:I(elev*elev) +
                          distRd + distStruc + distFeed + prevHunt*distRd + prevHunt*distStruc + 
                          prevHunt*distFeed + prevHunt*can + (1|Pack),
                        data = modDatNight, family = binomial(logit),
                        control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4)))
        
      
 


      
      
      
################################################################################################## #  
  




################################################################# ##

#######################
# NSERP veg code to pull from  #
#######################

# top model with best predictive power
# using unstandardized covariates for prediction
h.pred <- lm(log10(dat$gHerb + 0.02) ~ cc + elev + I(elev^2) + 
    landcov + radn + ndvi_amp + precip, data = dat)

# prep rasters
# yes i realize it would be cooler if i'd done this programmatically
rast.14 <- list.files(path=paste(wd, "writtenrasters/covs2014/biomass-herb", sep="/"), 
                      pattern="tif$", full.names=TRUE) #read in 2014 rasters
stack.14 <- stack(rast.14)
names(stack.14) # rename rasters to match covariate names
names(stack.14) <- c("cc", "elev", "landcov", "ndvi_amp", "precip", "radn")
names(stack.14) # sanity check
rast.15 <- list.files(path=paste(wd, "writtenrasters/covs2015/biomass-herb", sep="/"), 
                      pattern="tif$", full.names=TRUE)  #read in 2015 rasters
stack.15 <- stack(rast.15)
names(stack.15) # rename these to match covariate names
names(stack.15) <- c("cc", "elev", "landcov", "ndvi_amp", "precip", "radn")
names(stack.15)


# function to get both response and SE predictions
predfun <- function(model, data) {
  v <- predict(model, data, se.fit=TRUE) 
  cbind(p=as.vector(v$fit), se=as.vector(v$se.fit))
}

# predict 2014 rasters of gherb and StdError (indices 1 and 2, respectively)
gherb2014 <- predict(stack.14, h.pred, fun=predfun, index=1:2, progress="text")
names(gherb2014) <- c("gherb2014","StdErr14") 
gherb2014[["gherb2014"]] <- (10^gherb2014[["gherb2014"]])-0.02 #put back in g
plot(gherb2014[["gherb2014"]]) #gaze upon the beautiful rasters
plot(gherb2014[["StdErr14"]])