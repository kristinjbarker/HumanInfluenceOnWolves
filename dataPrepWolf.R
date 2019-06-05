                      ### ### ### ### ### ###  ### ### ### ### ### ### 
                      #   FORMATTING AND PREPARING WOLF GPS DATA     #
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
      "sp",            ## spatial work
      "sf",            ## for spatial work like the kids are doing it these days
      "lubridate",     ## manipulate datetime data inside dplyr pipes
      "adehabitatHR",   ## estimate home ranges
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
 
    
    
    #### Wolf, Pack, and Collar Information #### 
    
    
        ## collar data from GTNP ## 
    
          # gps and vhf collar data
          locsRaw <- read.csv(paste0(datDir, "\\Wolf\\wolfHistoricCleaned.csv"))
    
          # capture and collar info
          wolfRaw <- read.csv(paste0(datDir, "\\Wolf\\CaptureAndCollarInfo\\wolf_metadata.csv"))
          
          
        
        ## collar data from cluster study 2019 (to delineate study area) ##
          
          # # all collar data
          # sa2019 <- read.csv("../PredationStudy/Clusters/collarLocsProcessed/locDat-20190409.csv")
          # 
          # # only successfully recorded locations
          # sa2019 <- filter(sa2019, !is.na(UTMX))  
          # 
          # # recorded locations in each spatial projection
          # saUTM <- SpatialPointsDataFrame(data.frame("x" = as.numeric(sa2019$UTMX), "y" = as.numeric(sa2019$UTMY)), sa2019, proj4string = utm) 
          # saLL <- spTransform(saUTM, ll)
          # saAEA <- spTransform(saUTM, aea)
          # 
          # 
          
          
          # Study area in each projection
          saLL <- st_read(paste0(datDir, "/Land/studyAreaLL.shp")) 
          saAEA <- st_transform(saLL, paste(aea))
          saUTM <- st_transform(saLL, paste(utm))  
          
      #### trim wolf locations to potentially usable ones ####
          
          
        ## clean and format collar & capture data ##
          
          # remove vhf collars
          wolfFmt <- wolfRaw %>%
            filter(grepl("GPS", Collar.Type)) %>%
            # remove row number
            dplyr::select(-c(Number)) %>%
            # rename some columns
            rename(wolf = Wolf,
                   packCap = Pack,
                   capUTMX = UTM_X,
                   capUTMY = UTM_Y,
                   capDate = CaptureDate,
                   capLoc = CaptureLocation,
                   collarType = Collar.Type,
                   transEnd = Dropoff..mort.date,
                   fate = Drop...Mort...Unknown.) %>%
            # add capture year and wolf-year; make "?"s into Unknowns
            mutate(capYr = substr(capDate, nchar(as.character(capDate)) - 3, nchar(as.character(capDate))),
                   wolfYr = paste0(wolf, capYr),
                   fate = ifelse(fate == "?", "Unk", paste(fate)),
                   transEnd = ifelse(transEnd == "?", "Unk", paste(transEnd))) %>%
            # remove duplicate capture entries (see MethodsNotes_HumanInflWolf.docx)
            filter(wolfYr != "565F2009" | wolfYr == "565F2009" & !is.na(capUTMX)) %>%
            filter(wolfYr != "787M2012" | wolfYr == "787M2012" & capDate != "12/20/2012") %>%
            filter(wolfYr != "799M2014" | wolfYr == "799M2014" & capDate != "12/19/2014")
          wolfFmt <- droplevels(wolfFmt)
         
          
        
        ## clean and format gps data ##
          
          
          gpsFmt <- locsRaw %>%
            # remove extraneous columns
            dplyr::select(-c(Number, X.1)) %>%
            # define winter; add wolf-year; fix trailing whitespace in LGV
            mutate(winter = ifelse(Month <= 3, 1, 0),
                   Pack = trimws(Pack),
                   wolfYr = paste0(Wolf, Year)) %>%
            # only use winter locations for analysis
            filter(winter == 1)
          gpsFmt <- droplevels(gpsFmt)

          # make it spatial
          gpsLl <- SpatialPointsDataFrame(
            data.frame("x" = as.numeric(gpsFmt$Longitude), "y" = as.numeric(gpsFmt$Latitude)),
            gpsFmt, proj4string = ll)
          
          # remove wolves clearly outside study area
          gpsSa <- crop(gpsLl, extent(saLL))
          

          # identify wolves and wolf-years for consideration of inclusion in analysis
          wolfYrsPrelim <- unique(gpsSa@data$wolfYr)
          wolvesPrelim <- unique(gpsSa@data$Wolf)
          
          # # export csv to manually update with whether wolf will be included
          # wolfYrsMaybe <- data.frame(wolfYr = wolfYrsPrelim) %>%
          #   mutate(incl = "", locsOut = "") %>%
          #   left_join(wolfFmt, by = "wolfYr")
          # write.csv(wolfYrsMaybe, file = "wolfYrs_potential.csv", row.names = F)
          # 
          # 
          # # export shapefile of each wolf-year's locations (to see which are in study area)
          # for (i in 1:length(wolfYrsPrelim)) {
          #   # identify individual
          #   w <- wolfYrsPrelim[i]
          #   # subset that indiv's gps data  
          #   gpsW <- gpsSa@data[gpsSa@data$wolfYr == w, ]
          #   gpsW <- droplevels(gpsW)
          #   # make spatial and export for visual check in arcmap
          #   sfW <- st_as_sf(gpsW, coords = c("Longitude", "Latitude"), crs = paste(ll))
          #   st_write(sfW, paste0("../Data/Wolf/indivShps/", w, "prelim.shp"), delete_layer = TRUE)
          # }
          # 


        
          # read back in file telling which wolves included
          wolfYrsAll <- read.csv("wolfYrs_potential_upd.csv")
          wolfYrs <- filter(wolfYrsAll, incl == "y") # 34 wolf-yrs
          length(unique(wolfYrs$wolf)) # 14 wolves
          length(unique(wolfYrs$packCap)) # 6 packs
          wolfYrsList <- as.character(unique(wolfYrs$wolfYr))
          
          
          # filter wolf locs to only the wolfYrs you identified to include
          locs <- semi_join(gpsFmt, dplyr::select(wolfYrs, wolfYr), by = "wolfYr")
          locs <- droplevels(locs)
          
          # make them spatial
          locsSpat <- SpatialPointsDataFrame(
            data.frame("x" = locs$X, "y" = locs$Y),
            locs, proj4string = utm)
          
          
          
        #### Delineate winter home range for each individual ####    
          
          
          wolfYrsUDs <- kernelUD(locsSpat[ ,"wolfYr"], h = "href", same4all = FALSE)
          wolfYrsHRs <- getverticeshr(wolfYrsUDs, percent = 95)
          plot(wolfYrsHRs)
          
          
    
          
          
        #### Generate 5 available locations for each used location ####
          
          
          ## create blank df to store results in
          locsUA <- data.frame(matrix(NA, nrow = 0, ncol = 4))
          colnames(locsUA) <- c("X", "Y", "Used", "wolfYr")
          

          for (i in 1:length(wolfYrsList)) {
            
            # identify individual
            w <- wolfYrsList[i]
            
            # identify its locations
            wLocs <- filter(locs, wolfYr == w)
            wLocs$Used <- 1

            # calculate number of random locations to generate (5:1 used:avail)
            nLocs <- NROW(wLocs)
            nRndm <- nLocs * 5
            
            # identify HR polygon to sample from
            wHR <- wolfYrsHRs[which(wolfYrsHRs@data$id == w),]
            
            # generate random locations
            rndmSpat <- spsample(wHR, n = nRndm, "random") # not sure this is the right function
            
            # format random locations to combine with recorded locations
            rndmDat <- data.frame(rndmSpat)
            colnames(rndmDat) <- c("X", "Y")
            rndmDat$Used <- 0
            rndmDat$wolfYr <- w
            
            # combine random and recorded locations
            wLocsOnly <- dplyr::select(wLocs, c("X", "Y", "Used", "wolfYr"))
            wDat <- rbind(wLocsOnly, rndmDat)
            
            # add to master dataframe
            locsUA <- rbind(locsUA, wDat)

          }

                   
          # export wolf locs to use in analysis
          write.csv(locsUA, file = "wolfLocs-UsedAvail.csv", row.names = F)
          
          
          
         
        
          
    ##### to figure out why some packs are different between data sources ####
        
          
        # pack assignments from capture
        wolfDatCap <- wolfFmt
        wolfDatCap$capDate <- as.character(wolfDatCap$capDate)
        wolfDatCap$yrMetadat = substr(wolfDatCap$capDate, nchar(wolfDatCap$capDate)-3, nchar(wolfDatCap$capDate))
        
        # pack assignments from gps collar data file
        wolfDatHist <- gpsLl@data %>%
          dplyr::select(Wolf, Pack, Year) %>%
          distinct() %>%
          rename(wolf = Wolf, packHistDat = Pack, yrHistDat = Year)
        
        # identify non-matching pack assignments
        packFix <- wolfDatCap %>%
          left_join(wolfDatHist) %>%
          filter(!is.na(packHistDat)) %>%
          filter(!is.na(packCap)) %>%
          filter(packHistDat != packCap) 
        
        # investigate me  
        write.csv(packFix, file = "../Data/Wolf/wolfPackDiscrepancies.csv", row.names = FALSE)
        