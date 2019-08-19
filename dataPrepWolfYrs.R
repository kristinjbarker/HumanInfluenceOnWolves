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
    
 
  #### Make code reproducible ####
    
    set.seed(42)
    
    

################################################################################################## #  
  
    
    
### ### ### ### ### ### #
####   | RAW DATA |  ####
### ### ### ### ### ### #
 
    
    
    #### Wolf, Pack, and Collar Information #### 
    
    
        ## historic collar data (from GTNP; cleaned by jen)  
        rawHist <- read.csv(paste0(datDir, "\\Wolf\\WolfCollarDownloads\\wolfHistoricRecleaned.csv"))

        ## recent collar data files and filepaths 
        rawNames <- list.files(paste0(datDir, "\\Wolf\\WolfCollarDownloads\\Post2016FromCollaborators"))
        rawPaths <- list.files(paste0(datDir, "\\Wolf\\WolfCollarDownloads\\Post2016FromCollaborators"), full.names = TRUE) 
        
        ## recent collar data from winter 2019 (cleaned by me, but not formatted to match historic)
        rawMe <- read.csv("../PredationStudy/Clusters/collarLocsProcessed/locDat-20190409.csv")
        
        ## capture and fate info (includes historic and recent)
        rawRef <- read.csv(paste0(datDir, "\\Wolf\\CaptureAndCollarInfo\\collarsCapturesFates.csv"))
        
        ## collar info (to remove vhf collars)
        rawColl <- read.csv(paste0(datDir, "/Wolf/CaptureAndCollarInfo/wolf_metadata.csv"))
          
          
        ## jackson elk herd distribution counts (to define study area)
        saUTM <- st_read(paste0(datDir, "\\Elk\\elkCounts_2008-2019.shp")) 

          
 
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### #
####   | PRELIM DATA PREP |  ####
### ### ### ### ### ### ### ### ### # 

        
        
        
      #### format reference data ####
        
        ref <- mutate(rawRef, startData = mdy(startData))
        ref$endData <- ifelse(is.na(ref$endData), "12/31/2100", as.character(ref$endData))
        ref$endData <- mdy(ref$endData)

        

      #### prep historic data ####
        
          ## format data to facilitate later joins
          allHist <- rawHist %>%
            # remove weird extra column and Number column
            dplyr::select(-c("X.1", "Number")) %>%
            # format dates & times; add wolfYr
            mutate(datetime = ymd_hms(datetime, tz = "America/Denver"),
                   Date = as.Date(Date, format = "%m/%d/%Y"),
                   Time = substr(datetime, 12, 19),
                   wolfYr = paste(Wolf, Year, sep = "-")) %>%
            # remove daylight savings NAs (figure out a more refined way to handle this later)
            filter(!is.na(datetime)) %>%
            # only use winter locations
            filter(Month <= 3) %>%
            # add day/night indicator
            mutate(daytime = ifelse(hour(datetime) >= 8 & hour(datetime) <= 18, "day", "night")) %>%
            # order columns to match later dataframes for joins
            dplyr::select("Wolf", "Pack", "datetime", "Date", "daytime",
                              "Time", "Month", "Day", "Year",
                              "X", "Y", "Latitude", "Longitude", "wolfYr") 
          any(is.na(allHist)) # verify no wonky datetimes

        
          ## identify gps-collared wolves
          gps <- data.frame(Wolf = rawColl[rawColl$Collar.Type != "VHF", "Wolf"])
          gps <- droplevels(gps)
          
          ## only consider wolves with gps collars
          locsHist <- semi_join(allHist, gps)
 
                        
      #### identify wolves and their data sources ####
        
        ## all individuals
        wolves <- ref$wolfID
        
        ## individuals by data source (historic, more recent, or winter 2019)
        wolvesHist <- data.frame(wolfID = unique(locsHist$Wolf))
        wolfYrsHist <- data.frame(wolfYr = unique(locsHist$wolfYr))        
        wolvesThem <- data.frame(wolfID = regmatches(rawNames, regexpr("[0-9]+[A-Z]", rawNames))) %>% distinct()
        wolvesMe <- data.frame(wolfID = as.character(unique(rawMe$wolfID)))


        ## fix the wolfID i purposely changed, don't ask
        wolvesMe$wolfID <- ifelse(wolvesMe$wolfID == "1983F", "983F", paste(wolvesMe$wolfID))

        
    
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ###
####   | FORMAT RECENT DATA |  ####
### ### ### ### ### ### ### ### ###
        
        
        
      #### prepare to loop through collar data files ####
        
        
        ## create blank dataframes to store processed data in
        iOut <- as.data.frame(NULL)
        newOut <- as.data.frame(NULL)
        
        
        
      #### for each wolf whose data i didn't already start cleaning... ####
        
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
            iDatA <- read.csv(iFiles[j], header = FALSE, stringsAsFactors = FALSE, col.names = 1:25)  
            
            # remove awkward headers and trailing NA column
            colnames(iDatA) <- unlist(iDatA[which(iDatA[ ,1] == "Acquisition Time"), ])
            iDatA <- iDatA[-c(1:which(iDatA[ ,1] == "Acquisition Time")), ]
            iDatA <- iDatA[!is.na(names(iDatA))]
            
            # remove unsuccessful locations
            iDatA <- iDatA[grepl(pattern = "Succe.", x = iDatA$'GPS Fix Attempt'), ]
            
            # if no successful locations recorded, move on
            if(nrow(iDatA) == 0) { next }
            
            # start renaming columns to match historic data format
            iDatA <- iDatA %>%
              rename(datetime = 'Acquisition Time',
                     Latitude = 'GPS Latitude',
                     Longitude = 'GPS Longitude') %>%
              mutate(Wolf = iWolf,
                     Pack = iPack,
                     datetime = ymd_hms(datetime),
                     X = NA,
                     Y = NA,
                     Latitude = as.numeric(Latitude),
                     Longitude = as.numeric(Longitude)) 
            
            # record dates and times in local time
            attributes(iDatA$datetime)$tzone <- "America/Denver"
            iDatA <- iDatA %>%
              mutate(Date = substr(datetime, 1, 11),
                     Time = substr(datetime, 12, 19),
                     Month = month(datetime),
                     Day = day(datetime),
                     Year = year(datetime),
                     Hour = hour(datetime)) 
            
            # identify day vs nighttime locations
            iDatA$daytime = ifelse(iDatA$Hour >= 8 & iDatA$Hour <= 18, "day", "night")

            # format date
            iDatA$Date <- as.Date(iDatA$Date)
            
            # remove pre-deployment and post-transmission-end locations
            iDatA <- filter(iDatA, Date >= iStart & Date < iEnd)
            
            # final location subsets:
            iDatA <- iDatA %>%
              # remove daylight savings NAs
              filter(!is.na(datetime)) %>%
              # only use winter locations (jan-mar to align with cluster data) 
              filter(Month <= 3) %>%
              # randomly select 2 locations per day (one during day, one during night)
              group_by(Date, daytime) %>%
              sample_n(1) %>%
              ungroup()  

            # if no remaining locations, move on
            if(nrow(iDatA) == 0) { next }
            
            # order and format columns
            iDatA <- iDatA %>%
              mutate(wolfYr = paste(Wolf, Year, sep = "-")) %>%
              dplyr::select(names(locsHist)) 
            
            # remove wolfYrs already included in jen's cleaned data
            iDatA <- anti_join(iDatA, wolfYrsHist, by = "wolfYr")
            iDatA <- droplevels(iDatA)
            
            
            # if no remaining locations, move on
            if(nrow(iDatA) == 0) { next }            
 
            # make it spatial
            iSp <- SpatialPointsDataFrame(
              data.frame("x" = as.numeric(iDatA$Longitude), "y" = as.numeric(iDatA$Latitude)),
              iDatA, proj4string = ll)
            
            # convert lat/longs to correct utms (utm zones differed in raw files)
            iUTM <- spTransform(iSp, utm)
            
            # add correct utms to data 
            iDatA$X <- iUTM@coords[ , "x"]
            iDatA$Y <- iUTM@coords[ , "y"]
            
            
            # combine with other data for that wolf
            iOut <- rbind(iOut, iDatA)
            
            # remove duplicates
            iOut <- distinct(iOut)

          }
          
          
          # add individual data to master data
          newOut <- rbind(newOut, iOut)
          
          # recreate blank dataframe for next individual
          iOut <- as.data.frame(NULL)

        }
        
        
        # add cleaned new data to cleaned historic data
        locsAllPrelim <- rbind(locsHist, newOut)
        
        
        
 
################################################################################################## #  
  
    
    
### ### ### ### ###  ### ### ### ### ###
####   | FORMAT WINTER 2019 DATA |  ####
### ### ### ### ###  ### ### ### ### ###


        #### format data before looping through each individual ####
        
            # set new dataframe for combined data with above
            locsAll <- locsAllPrelim
        
            # fix that wolfID...
            locsMe <- rawMe
            locsMe$wolfID <- ifelse(locsMe$wolfID == "1983F", "983F", paste(locsMe$wolfID))
            
            # and format other columns etc
            locsMe <- locsMe %>%
              # remove pack (all GV packs were lumped for winter 2019)
              dplyr::select(-pack) %>%
              # remove unsuccessful locations 
              filter(FixStatus == "Succeeded") %>%
              # match column dates of historic data
              rename(Wolf = wolfID,
                     Latitude = Lat,
                     Longitude = Long) %>%
              # format date and time info
              mutate(datetime = ymd_hms(TelemDate, tz = "America/Denver")) %>%
              # remove daylight savings NAs
              filter(!is.na(datetime)) %>%
              # split daytime into relevant components
              mutate(Date = substr(datetime, 1, 11),
                     Time = substr(datetime, 12, 19),
                     Month = month(datetime),
                     Day = day(datetime),
                     Year = year(datetime),
                     Hour = hour(datetime)) %>%
              # only use winter locations
              filter(Month <= 3) %>%
              # identify day vs nighttime locations
              mutate(daytime = ifelse(Hour >= 8 & Hour <= 18, "day", "night")) %>%
              # randomly select 2 locations per day (one during day, one during night)
              group_by(Wolf, Day, daytime) %>%
              sample_n(1) %>%
              ungroup()  
            # finalize date format
            locsMe$Date <- as.Date(locsMe$Date)
            # remove any stored factor levels
            locsMe <- droplevels(locsMe)
            # sanity check, should print F
            any(is.na(locsMe$datetime)) 

            
       
        #### for each individual whose collar data i processed... ####
            
        for(k in 1:nrow(wolvesMe)) {
            
            # identify the individual, start date, and end date
            iWolf <- as.character(wolvesMe[k, "wolfID"])
            iPack <- as.character(ref[ref$wolfID == iWolf, "pack"])
            iEnd <- ref[ref$wolfID == iWolf, "endData"]
            
            # subset its locations
            iDat <- filter(locsMe, Wolf == iWolf & Date < iEnd)

            # make it spatial
            iSp <- SpatialPointsDataFrame(
              data.frame("x" = as.numeric(iDat$Longitude), "y" = as.numeric(iDat$Latitude)),
              iDat, proj4string = ll)
            
            # convert lat/longs to correct utms (utm zones differed in raw files)
            iUTM <- spTransform(iSp, utm)
            
            # add correct utms to data 
            iDat$X <- iUTM@coords[ , "x"]
            iDat$Y <- iUTM@coords[ , "y"]

            # format to match master dataframe
            iDat <- iDat %>%
              mutate(
                Pack = iPack,
                wolfYr = paste(Wolf, Year, sep = "-")) %>%
              dplyr::select(names(locsHist)) 

          
          # add to master dataframe
          locsAll <- rbind(locsAll, iDat)
          
          
          # recreate blank dataframe for next loop (not really necessary)
          iDat <- as.data.frame(NULL)

        }

 
        
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### ### ### ###
####   | FINALIZE AND EXPORT USED LOCATIONS |  ####
### ### ### ### ### ### ### ### ### ### ### ### ###
        
        
        # add 'used' signifier
        locsAll$Used <- 1
            
        # fix pack designations (multiple versions of LGV pack in OG wolf data sources)
        locsAll$Pack <- ifelse(grepl("Gros Ventre|GV", locsAll$Pack), "Lower Gros Ventre", as.character(locsAll$Pack))
            
        
        # make spatial (UTM format to match elk distribution data)
        locsAllUTM <- SpatialPointsDataFrame(
          data.frame("x" = locsAll$X, "y" = locsAll$Y),
          locsAll, proj4string = utm)
        
        # define jackson elk herd distn (buffer elk count locs by 500m per side; add 22km N to incl Jackson Lake area)
        newExtent <- extent(saUTM)
        newExtent@xmin <- extent(saUTM)@xmin + 500
        newExtent@ymin <- extent(saUTM)@ymin + 500
        newExtent@xmax <- extent(saUTM)@xmax + 500
        newExtent@ymax <- extent(saUTM)@ymax + 22000
        
        # crop wolf locs to extent of jackson elk herd distribution         
        locsCrop <- crop(locsAllUTM, newExtent)

        
        # make dataframe of remaining locs and associated data
        locsMost <- droplevels(locsCrop@data)
        
        # count number of locations per wolf
        nLocs <- locsMost %>%
          group_by(wolfYr) %>%
          summarise(n = n())
        nSm <- filter(nLocs, n < 20)
        
        # remove wolves with too few locations to estimate an available range
        locs <- anti_join(locsMost, nSm, by = "wolfYr")
        
        
        # make spatial
        locsSp <- SpatialPointsDataFrame(
          data.frame("x" = locs$X, "y" = locs$Y),
          locs, proj4string = utm)
        
        
        # export data - csv and shp   
        write.csv(locs, "wolflLocs-Used.csv", row.names = F)
        writeOGR(locsSp,
                 paste0(datDir, "/Wolf/indivShps"),
                 "humanInflLocs-Used",
                 "ESRI Shapefile",
                 overwrite_layer = TRUE)
        
      
################################################################################################## #  
  

        
           
### ### ### ### ### ### ### ### ### ### ### #
####   | GENERATE AVAILABLE LOCATIONS |  ####
### ### ### ### ### ### ### ### ### ### ### # 
        
        
          
        #### Delineate available area (95% KDE) for each individual ####    
          
          
            wolfYrsUDs <- kernelUD(locsSp[ ,"wolfYr"], h = "href", same4all = FALSE)
            wolfYrsHRs <- getverticeshr(wolfYrsUDs, percent = 95)
            plot(wolfYrsHRs) 
          
          
            # export HRs
            writeOGR(wolfYrsHRs,
                     dsn = paste0(datDir, "/Wolf"),
                     layer = "winHRswolf",
                     driver = "ESRI Shapefile",
                     overwrite_layer = TRUE)
         
            
               
          #### Pull random locations from available area ####
           
            
            # identify individuals and their packs and wolfYrs
            wolfDat <- locs %>%
              dplyr::select(Wolf, Pack, wolfYr) %>%
              distinct()
            wolfDat <- droplevels(wolfDat)
            wolfYrs <- unique(locs$wolfYr)
            
            
            # create blank df to store results in
            locsA <- data.frame(matrix(NA, nrow = 0, ncol = 7))
            colnames(locsA) <- c("X", "Y", "Used", "wolfYr", "Date", "Time", "daytime")  
            
            
            # for each individual...
            for (l in 1:length(wolfYrs)) {
              
              # identify the individual, pack, and wolfyr
              w <- wolfYrs[l]
              wPack <- wolfDat[wolfDat$wolfYr == w, "Pack"]
              wWolf <- wolfDat[wolfDat$wolfYr == w, "Wolf"]
              
              # identify its locations 
              wLocs <- filter(locs, wolfYr == w)
              wLocs$Used <- 1
              
              # identify dates and times of recorded locations (for random selection)
              wDates <- unique(wLocs$Date)
              wTimes <- unique(wLocs$Time)
  
              # calculate number of random locations to generate (1:1 used:avail)
              nRndm <- NROW(wLocs)

              # identify HR (available area) to sample from
              wHR <- wolfYrsHRs[which(wolfYrsHRs@data$id == w),]
              
              # generate random locations
              rndmSpat <- spsample(wHR, n = nRndm, "random") 
              
              # format random locations to combine with recorded locations
              rndmDat <- data.frame(rndmSpat)
              colnames(rndmDat) <- c("X", "Y")
              rndmDat$Used <- 0
              rndmDat$wolfYr <- w
              rndmDat$Wolf <- wWolf
              rndmDat$Pack <- wPack

              # randomly assign dates and times from those in recorded locations
              rndmDat$Date <- sample(wDates, size = nrow(rndmDat), replace = T)
              rndmDat$Time <- sample(wTimes, size = nrow(rndmDat), replace = T)
              rndmDat$daytime <- ifelse(as.numeric(substr(rndmDat$Time, 1, 2)) >= 8 & as.numeric(substr(rndmDat$Time, 1, 2)) <= 18, "day", "night")
                            
              # combine random and recorded locations
              wLocsOnly <- dplyr::select(wLocs, c("X", "Y", "Used", "wolfYr", "Wolf", "Pack", "Date", "Time", "daytime"))
              wDat <- rbind(wLocsOnly, rndmDat)
              
              # add to master dataframe
              locsA <- rbind(locsA, wDat)
  
            }

            
          # make new dataframe (to retain loop results)
          locsUA <- locsA
             
          # make spatial
          locsUA.UTM <- SpatialPointsDataFrame(
            data.frame("x" = locsUA$X, "y" = locsUA$Y),
            data = locsUA, proj4string = utm)
          
          # transform to lat/long and add lat/longs to dataframe
          locsUA.LL <- spTransform(locsUA.UTM, ll)
          locsUA$Latitude <- locsUA.LL@coords[, "y"]
          locsUA$Longitude <- locsUA.LL@coords[, "x"]
          
          # add datetime
          locsUA$datetime <- paste(locsUA$Date, locsUA$Time, sep = "")
          
          # format 
          locsUA <- locsUA %>%
            # add month, day, and year
            mutate(datetime = ymd_hms(datetime),
                   Month = month(Date),
                   Day = day(Date),
                   Year = year(Date)) %>%
            # order columns
            dplyr::select(c("Wolf", "Pack", "datetime", "Date", "daytime",
                            "Time", "Month", "Day", "Year",
                            "X", "Y", "Latitude", "Longitude", 
                            "Used", "wolfYr"))           
          
          # make spatial
          locsUAsp <- SpatialPointsDataFrame(
            data.frame("x" = locsUA$X, "y" = locsUA$Y),
            data = locsUA, proj4string = utm)          

            
          # export csv
          write.csv(locsUA, file = "wolfLocs-UsedAvail.csv", row.names = F)
          
          
          # export shp
          writeOGR(locsUAsp,
                   dsn = paste0(datDir, "/Wolf"),
                   layer = "humanInflLocs-UsedAvail",
                   driver = "ESRI Shapefile",
                   overwrite_layer = TRUE)          
        
          
          

        