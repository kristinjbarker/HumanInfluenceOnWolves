                      ### ### ### ### ### ### ### ### ### ### ##
                      #    PROCESSIG ALL WOLF COLLAR DATA      #
                      #                                        #
                      #     Kristin Barker | Fall 2019       #
                      #      kristinjbarker@gmail.com          #
                      ### ### ### ### ### ###  ### ### ### ### #


################################################################################################## #

### ### ### ### ### #
####  | SETUP |  ####
### ### ### ### ### #


  
  #### Set working directory and filepath to spatial data ####

      setwd("C:\\Users\\Kristin\\Dropbox\\Documents\\HumanInfluenceOnWolves")
      datDir <- "C:\\Users\\Kristin\\Dropbox\\Documents\\Data"

  

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
      "adehabitatHR",  ## estimate home ranges
      "suncalc",       ## determine crepuscular times based on lat/longs
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
          allHistPrelim <- rawHist %>%
            # format dates & times; add wolfYr
            mutate(datetime = mdy_hm(paste(Date, Time, sep = " "), tz = "America/Denver"),
                   Date = as.Date(Date, format = "%m/%d/%Y"),
                   Time = substr(datetime, 12, 19),
                   wolfYr = paste(Wolf, Year, sep = "-")) %>%
            # remove daylight savings NAs (figure out a more refined way to handle this later)
            filter(!is.na(datetime)) 

 
          ## add day/night/crepuscular indicator
          crepHist <- allHistPrelim %>%
            dplyr::rename(date = Date, lat = Latitude, lon = Longitude) 
          crepHist <- getSunlightTimes(data = crepHist,
                                       tz = "America/Denver",
                                       keep = c("nightEnd", "goldenHourEnd", # night is astronomical night
                                                "goldenHour", "night")) # golden hour is soft light
          allHist <- cbind(allHistPrelim, crepHist)
          allHist$daytime <- ifelse(allHist$datetime > allHist$nightEnd & allHist$datetime < allHist$goldenHourEnd |
                            allHist$datetime > allHist$goldenHour & allHist$datetime < allHist$night, "crep", 
                            ifelse(allHist$datetime < allHist$nightEnd | allHist$datetime > allHist$night, "night", "day"))

 
          # order columns to match later dataframes for joins
          locsHist <- allHist %>%     
            # order columns to match later dataframes for joins
            dplyr::select("Wolf", "Pack", "datetime", "Date", "daytime",
                              "Time", "Month", "Day", "Year",
                              "X", "Y", "Latitude", "Longitude", "wolfYr") 
          any(is.na(locsHist)) # verify no wonky datetimes


                        
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
              dplyr::rename(datetime = 'Acquisition Time',
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

            # define crepuscular time
            crepiDatA <- iDatA %>%
              mutate(Date = as.Date(Date)) %>%
              dplyr::rename(date = Date, lat = Latitude, lon = Longitude) 
            crepiDatA <- getSunlightTimes(data = crepiDatA,
                                         tz = "America/Denver",
                                         keep = c("nightEnd", "goldenHourEnd", # night is astronomical night
                                                  "goldenHour", "night")) # golden hour is soft light
            # add day/night/crepuscular indicator
            iDatA <- cbind(iDatA, crepiDatA)
            iDatA$daytime <- ifelse(iDatA$datetime > iDatA$nightEnd & iDatA$datetime < iDatA$goldenHourEnd |
                              iDatA$datetime > iDatA$goldenHour & iDatA$datetime < iDatA$night, "crep", 
                              ifelse(iDatA$datetime < iDatA$nightEnd | iDatA$datetime > iDatA$night, "night", "day"))
  
               

            # format date
            iDatA$Date <- as.Date(iDatA$Date)
            
            # remove pre-deployment and post-transmission-end locations
            iDatA <- filter(iDatA, Date >= iStart & Date < iEnd)
            
            # final location subsets:
            iDatA <- iDatA %>%
              # remove daylight savings NAs
              filter(!is.na(datetime)) 

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
              dplyr::rename(Wolf = wolfID,
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
                     Hour = hour(datetime)) 
            
            # define crepuscular time
            creplocsMe <- locsMe %>%
              mutate(Date = as.Date(Date)) %>%
              dplyr::rename(date = Date, lat = Latitude, lon = Longitude) 
            creplocsMe <- getSunlightTimes(data = creplocsMe,
                                         tz = "America/Denver",
                                         keep = c("nightEnd", "goldenHourEnd", # night is astronomical night
                                                  "goldenHour", "night")) # golden hour is soft light
            # add day/night/crepuscular indicator
            locsMe <- cbind(locsMe, creplocsMe)
            locsMe$daytime <- ifelse(locsMe$datetime > locsMe$nightEnd & locsMe$datetime < locsMe$goldenHourEnd |
                              locsMe$datetime > locsMe$goldenHour & locsMe$datetime < locsMe$night, "crep", 
                              ifelse(locsMe$datetime < locsMe$nightEnd | locsMe$datetime > locsMe$night, "night", "day"))
            

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
  

    
### ### ### ### ### ###  ### ### ### ### ###
####   | COMBINE AND FORMAT ALL DATA |  ####
### ### ### ### ### ###  ### ### ### ### ###

       
                 
    #### Fix pack designations ####
            
        # multiple versions of LGV pack in OG wolf data sources
        locsAll$Pack <- ifelse(grepl("Gros Ventre|GV", locsAll$Pack), "Lower Gros Ventre", as.character(locsAll$Pack)) 
                
        # indiv captured near lgv but turned out to be a lone wolf
        locsAll[locsAll$Wolf == "1004M", "Pack"] <- "Loner"
        
        # add pack-year
        locsAll$packYr <- paste0(locsAll$Pack, locsAll$Year)
        
        # make spatial
        locsSp <- SpatialPointsDataFrame(
          data.frame("x" = locsAll$X, "y" = locsAll$Y),
          locsAll, proj4string = utm)
        
        # # estimate territories per wolf-year
        # wyUD <- kernelUD(locsSp[,"wolfYr"], h = "href", grid = 50) # extent = 1, 10, 50, 100 all bad
        # wyKDE <- getverticeshr(wyUD[[1]], percent = 95)
        # image(wyUD[[1]])
        
        # estimate territories per wolf-year and pack-year minus current problem animals
        locsSub <- filter(locsAll, wolfYr != "1087F-2017" & wolfYr != "723M-2010" & wolfYr != "997M-2017")
        locsSub <- droplevels(locsSub)
        length(unique(locsSub$wolfYr)); length(unique(locsAll$wolfYr)) # perf
        locsSubSp <- SpatialPointsDataFrame(data.frame("x" = locsSub$X, "y" = locsSub$Y),locsSub, proj4string = utm)        
        wySubUD <- kernelUD(locsSubSp[,"wolfYr"], h = "href", grid = 50) # extent = 1, 10, 50, 100 all bad
        wySubKDE <- getverticeshr(wySubUD, percent = 95)
        writeOGR(wySubKDE,
                 paste0(datDir, "/Wolf/indivShps"),
                 "wolfHRsSub",
                 "ESRI Shapefile",
                 overwrite_layer = TRUE)      
        pySubUD <- kernelUD(locsSubSp[,"packYr"], h = "href", same4all = F) # grid = 50 no good
        pySubKDE <- getverticeshr(pySubUD, percent = 95)
        pySubKDE$id <- as.character(pySubKDE$id)
        pySubKDE$Year <- substr(pySubKDE$id, nchar(pySubKDE$id) - 3, nchar(pySubKDE$id))
        writeOGR(pySubKDE,
                 paste0(datDir, "/Wolf/packShps"),
                 "packHRsSub",
                 "ESRI Shapefile",
                 overwrite_layer = TRUE)         
        
                 
    #### Clean collar data ####
            
        # remove incorrect locations from 1087 (Tennessee, Dubois back yard...)
        

        

    
    #### Collar metadata ####        
    
        # Metadata from park 
        wolfDatGTNP <- rawColl %>%
          dplyr::select(Wolf, Sex, Collar.Type, Drop...Mort...Unknown., Pack) %>%
          rename(collar = Collar.Type, Fate = Drop...Mort...Unknown.) %>%
          mutate(collar = ifelse(grepl("GPS", collar), "GPS", "VHF"),
                 Sex = substr(Sex, 0, 1))
        wolfDatGTNP$Pack <- ifelse(grepl("Gros Ventre|GV", wolfDatGTNP$Pack), "Lower Gros Ventre", as.character(wolfDatGTNP$Pack))
        
        # Metadata from state 
        wolfDatWGFD <- ref %>%
          dplyr::select(wolfID, sex, collarType, fate, pack) %>%
          rename(Wolf = wolfID, Sex = sex, collar = collarType, Fate = fate, Pack = pack) %>%
          mutate(collar = ifelse(grepl("GPS", collar), "GPS", "VHF"))
        wolfDatWGFD$Pack <- ifelse(grepl("Gros Ventre|GV", wolfDatWGFD$Pack), "Lower Gros Ventre", as.character(wolfDatWGFD$Pack))
        
        # Combine and format (for collar type & fate)
        wolfDat <- rbind(wolfDatGTNP, wolfDatWGFD) %>%
          mutate(Fate = ifelse(grepl("Mort", Fate), "Dead", 
                        ifelse(grepl("Drop|Alive|Switch", Fate), "Alive", "Unknown")))
    
 
        
################################################################################################## #  
  

    
### ### ### ### ### ### ### ### ### 
####   | SUMMARY STATISTICS |  ####
### ### ### ### ### ### ### ### ### 
        

    # number of packs and and individuals per year
    sumYrPrelim <- locsAll %>%
      # add metadata
      left_join(wolfDat, wolfDat, by = "Wolf") %>%
      # remove duplicate Pack column (favor the one from locs not the one from capture)
      dplyr::select(-"Pack.y") %>%
      rename(Pack = Pack.x) %>%
      # number of packs and individuals
      group_by(Year) %>%
      summarise(nPack = length(unique(Pack)),
                nIndiv = length(unique(Wolf))) 
    
    # per wolf-year 
    sumColl <- locsAll %>%
      # add metadata
      left_join(wolfDat, wolfDat, by = "Wolf") %>%
      # remove duplicate Pack column (favor the one from locs not the one from capture)
      dplyr::select(-"Pack.y") %>%
      rename(Pack = Pack.x) %>%      
      # dates, number of locations, and collar type
      group_by(wolfYr) %>%
      summarise(nDays = max(Date) - min(Date),
                nlocs = n())
    
    # per wolf
    sumWolfYr <- locsAll %>%
      # add metadata
      left_join(wolfDat, wolfDat, by = "Wolf") %>%
      # remove duplicate Pack column (favor the one from locs not the one from capture)
      dplyr::select(-"Pack.y") %>%
      rename(Pack = Pack.x) %>%
      # retain useful info
      dplyr::select(Wolf, wolfYr, Year, collar, Pack) %>%
      distinct() %>%
      left_join(sumColl, by = "wolfYr")
    
    # per year
    sumYr <- sumWolf %>%
      group_by(Year) %>%
      summarise(nIndiv = length(unique(Wolf)),
                nPack = length(unique(Pack)),
                nVHF = length(which(collar == "VHF")),
                nGPS = length(which(collar == "GPS")))
    
    # per wolf
    sumWolf <- sumWolf %>%
      group_by(Wolf) %>%
      summarise(nYr = length(unique(Year)),
                nDays = sum(nDays),
                nLocs = sum(nlocs),
                collar = unique(collar))
      
    
        
        # summarize number of locs per wolfYr 
        locsWolfYr <- locsAll %>%
          group_by(wolfYr) %>%
          summarise(nLocs = n())         
        
        # and per pack-year 
        locsPack <- locsAll %>%
          group_by(packYr) %>%
          summarise(nLocs = n(),
                    nIndiv = length(unique(Wolf))) 

            
            
################################################################################################## #          
    
    
### ### ### ### ### ### ### ### ##
####   | EXPORT AND STORE  |  ####
### ### ### ### ### ### ### ### ##

            
    # summary data
    write.csv(sumYr, file = "dataSummary_Year.csv", row.names = F)
    write.csv(sumWolfYr, file = "dataSummary_WolfYear.csv", row.names = F)
    write.csv(sumWolf, file = "dataSummary_Wolf.csv", row.names = F)
    
    # wolf location data #

        # export data - csv and shp   
        write.csv(locsAll, "wolfLocsAll.csv", row.names = F)
        writeOGR(locsSp,
                 paste0(datDir, "/Wolf/indivShps"),
                 "wolfLocsAll",
                 "ESRI Shapefile",
                 overwrite_layer = TRUE)

          # store
          save.image(paste0("dataPrepWolfAll_", today(), ".RData"))
        
      
################################################################################################## #  


    
    
### ### ### ### ### ### ### ### ##
####   | POSTERIORI MANIPULATIONS  |  ####
### ### ### ### ### ### ### ### ##         
          
    dat <- read.csv("wolfLocsAll.csv")
    w <- dat %>%
      group_by(wolfID) %>%
      dplyr::select(wolfID, year) %>%
      distinct() 
    