                      ### ### ### ### ### ### ### ### ### ### ##
                      #       CLEANING UP COLLAR DATA AND      #
                      #    IDENTIFYING WOLF-YEARS TO USE IN    #
                      #   ANALYSIS OF HUMAN INFLUENCE ON WOLF  # 
                      #        DISTRIBUTIONS AND BEHAVIORS     #
                      #                                        #
                      #     Kristin Barker | Summer 2019       #
                      #      kristinjbarker@gmail.com          #
                      ### ### ### ### ### ###  ### ### ### ### #


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
    
    
        ## historic collar data (from GTNP; cleaned by jen)  
        locsHistoric <- read.csv(paste0(datDir, "\\Wolf\\WolfCollarDownloads\\wolfHistoricCleaned.csv"))
    
        ## recent collar data files and filepaths 
        rawNames <- list.files(paste0(datDir, "\\Wolf\\WolfCollarDownloads\\Post2016FromCollaborators"))
        rawPaths <- list.files(paste0(datDir, "\\Wolf\\WolfCollarDownloads\\Post2016FromCollaborators"), full.names = TRUE) 
        
        ## recent collar data from winter 2019 (cleaned by me, but not formatted to match historic)
        rawMe <- read.csv("../PredationStudy/Clusters/collarLocsProcessed/locDat-20190409.csv")
        
        ## capture and fate info (includes historic and recent)
        refRaw <- read.csv(paste0(datDir, "\\Wolf\\CaptureAndCollarInfo\\collarsCapturesFates.csv"))
          
          
        ## Study area in each projection
        saLL <- st_read(paste0(datDir, "/Land/studyAreaLL.shp")) 
        saAEA <- st_transform(saLL, paste(aea))
        saUTM <- st_transform(saLL, paste(utm))  
          
          
 
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ##
####   | PREP RAW DATA |  ####
### ### ### ### ### ### ### ##
        
        
        
      #### prepare to loop through collar data files ####
        
        
        ## format dates in capture and fate data
        ref <- refRaw %>%
          mutate(startData = mdy(startData), endDataPrelim = mdy(endDataPrelim))

        
        ## identify all individuals
        wolves <- ref$wolfID
        
        ## link individuals to collar file locations for processing
        wolvesThem <- data.frame(wolfID = regmatches(rawNames, regexpr("[0-9]+[A-Z]", rawNames))) %>% distinct()
        wolvesMe <- data.frame(fileLoc = "rawMe", wolfID = as.character(unique(rawMe$wolfID)))

        
        ## create blank dataframes to store processed data in
        iOut <- as.data.frame(NULL)
        allOut <- as.data.frame(NULL)
        
        
        
      #### for each wolf whose data you didn't already start cleaning... ####
        
        for(i in 1:nrow(wolvesThem)) {
          
          # identify the wolf 
          iWolf <- as.character(wolvesThem[i, "wolfID"])
          iPack <- as.character(ref[ref$wolfID == iWolf, "pack"])
          
          # identify data start and end dates (one day after collar deployment or before transmission end)
          iStart <- ref[ref$wolfID == iWolf, "startData"]
          iEnd <- ref[ref$wolfID == iWolf, "endData"]
          
          # find its collar files (some have >1)
          iFiles <- rawPaths[grep(pattern = iWolf, x = rawPaths)]
          
          # for each file...
          for(j in 1:length(iFiles)) {
            
            # read in collar data (and deal with telonics' awkward headers)
            iDat <- read.csv(iFiles[j], header = FALSE, stringsAsFactors = FALSE, col.names = 1:25)  
            
            # remove awkward headers and trailing NA column
            colnames(iDat) <- unlist(iDat[which(iDat[ ,1] == "Acquisition Time"), ])
            iDat <- iDat[-c(1:which(iDat[ ,1] == "Acquisition Time")), ]
            iDat <- iDat[!is.na(names(iDat))]
            
            # remove unsuccessful locations
            iDat <- iDat[grepl(pattern = "Succe.", x = iDat$'GPS Fix Attempt'), ]
            
            # if no successful locations recorded, move on
            if(nrow(iDat) == 0) { next }
            
            # start renaming columns to match historic data format
            iDat <- iDat %>%
              rename(datetime = 'Acquisition Time',
                     Latitude = 'GPS Latitude',
                     Longitude = 'GPS Longitude') %>%
              mutate(Number = NA,
                     Wolf = iWolf,
                     Pack = iPack,
                     datetime = ymd_hms(datetime),
                     X = NA,
                     Y = NA,
                     Latitude = as.numeric(Latitude),
                     Longitude = as.numeric(Longitude)) 
            
            
            # record dates and times in local time
            attributes(iDat$datetime)$tzone <- "MST"
            iDat <- iDat %>%
              mutate(Date = substr(datetime, 1, 11),
                     Time = substr(datetime, 12, 19),
                     Month = month(datetime),
                     Day = day(datetime),
                     Year = year(datetime))
            iDat$Date <- as.Date(iDat$Date)
            
            # remove pre-deployment and non-winter locations (jan-mar to align with cluster data))
            iDat <- filter(iDat, Date >= iStart & Date < iEnd)
            
            # only use winter locations (defined 
            iDat <- filter(iDat, Month <= 3)

            
            # if no remaining locations, move on
            if(nrow(iDat) == 0) { next }
                        
            
            # order and format columns
            iDat <- iDat %>%
              dplyr::select(c("Number", "Wolf", "Pack", "datetime",
                              "Date", "Time", "Month", "Day", "Year",
                              "X", "Y", "Latitude", "Longitude")) %>%
              mutate(Latitude = as.numeric(Latitude),
                     Longitude = as.numeric(Longitude))
            
            # add identifier for locs recorded during or after transmission end date
            iDat$postEnd <- ifelse(iDat$Date >= ref[ref$wolfID == iWolf, "endDataPrelim"], 1, 0)
              
            
            # make it spatial
            iSp <- SpatialPointsDataFrame(
              data.frame("x" = as.numeric(iDat$Longitude), "y" = as.numeric(iDat$Latitude)),
              iDat, proj4string = ll)
            
            # convert lat/longs to correct utms (utm zones differed in raw files)
            iUTM <- spTransform(iSp, utm)
            
            # add correct utms to data 
            iDat$X <- iUTM@coords[ , "x"]
            iDat$Y <- iUTM@coords[ , "y"]
            
            
            # combine with other data for that wolf
            iOut <- rbind(iOut, iDat)
            
            # remove duplicates
            iOut <- distinct(iOut)
            
            # make spatial
            iOutSp <- SpatialPointsDataFrame(
              data.frame("x" = iOut$Longitude, "y" = iOut$Latitude),
              iOut, proj4string = ll)            
                    

          }
          
          
          
          # recreate blank dataframe
          iOut <- as.data.frame(NULL)
            
            
          # do whatever to make formatting match other wolves
          
          # add to combined dataframe for all wolves
          
          # recreate blank dataframe(s?) to store loop results in
          
          
        }
        
        
        
        # format my collar data to match column names etc and remove non-winter locs 
        
        # join data cleaned above to my data
        
        
        # read in cleaned historic data and cut to just winter locations
        

         # join cleaned historic data to data cleaned above
        
        
         # crop to buffered extent of elk distribution counts
        
        

        
        
         # export data - master shp, master csv, anything individual/pack?
        

        

        
        #
        
        
        
        
        
        
        
        
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### #
####   | OLDER CODE USING CLEANED AND FORMATTED HISTORIC DATA |  ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### # 
        
        
          
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
            # define winter (jan-mar); add wolf-year; fix trailing whitespace in LGV; format date
            mutate(winter = ifelse(Month <= 3, 1, 0),
                   Pack = trimws(Pack),
                   wolfYr = paste0(Wolf, Year),
                   Date = mdy(Date)) %>%
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
          length(unique(wolfYrs$wolf)) # 14 wolves - now 27 13ishjune
          length(unique(wolfYrs$packCap)) # 6 packs - now 8
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
          
          
          ## export HRs
          writeOGR(wolfYrsHRs,
                   dsn = paste0(datDir, "/Wolf"),
                   layer = "winHRswolf",
                   driver = "ESRI Shapefile",
                   overwrite_layer = TRUE)



          
        #### Generate 5 available locations for each used location ####
          
          
          ## create blank df to store results in
          locsUA <- data.frame(matrix(NA, nrow = 0, ncol = 6))
          colnames(locsUA) <- c("X", "Y", "Used", "wolfYr", "Date", "Time")
          

          for (i in 1:length(wolfYrsList)) {
            
            # identify individual
            w <- wolfYrsList[i]
            
            # identify its locations 
            wLocs <- filter(locs, wolfYr == w)
            wLocs$Used <- 1
            
            # identify dates and times (for random selection)
            wDates <- unique(wLocs$Date)
            wTimes <- unique(wLocs$Time)

            # calculate number of random locations to generate (5:1 used:avail)
            nLocs <- NROW(wLocs)
            nRndm <- nLocs * 5
            
            # identify HR polygon to sample from
            wHR <- wolfYrsHRs[which(wolfYrsHRs@data$id == w),]
            
            # generate random locations
            rndmSpat <- spsample(wHR, n = nRndm, "random") 
            
            # format random locations to combine with recorded locations
            rndmDat <- data.frame(rndmSpat)
            colnames(rndmDat) <- c("X", "Y")
            rndmDat$Used <- 0
            rndmDat$wolfYr <- w
            
            # randomly assign dates and times from those in recorded locations
            rndmDat$Date <- sample(wDates, size = nrow(rndmDat), replace = T)
            rndmDat$Time <- sample(wTimes, size = nrow(rndmDat), replace = T)
            
            # combine random and recorded locations
            wLocsOnly <- dplyr::select(wLocs, c("X", "Y", "Used", "wolfYr", "Date", "Time"))
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
        