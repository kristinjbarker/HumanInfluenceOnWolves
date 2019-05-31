## troubleshooting dealing with spatial data for human influence analysis kjb summer 2019

#### DEM / elev / slope / ruggedness ####


       dem1shp <- shapefile(paste0(datDir, "\\Land\\DEM\\USGS_n44w110\\ned_13arcsec_g.shp")) 
       dem1 <- raster(dem1shp)
       
       dem <- getData('SRTM', lon = extent(gpsLL)[1], lat = extent(gpsLL)[3]) # newp
       demOw <- raster(paste0(datDir, "\\Land\\DEM\\SRTM_Cody_Albers.tif"))
       plot(demOw)
       extent(demOw)
       crs(demOw)
       
               
        plot(dem)
        plot(gpsLL, add = TRUE)
        
        plot(dem1)
        extent(gpsLL)
        
        
#### wolf collar data ####
        
        
        table(gpsRaw$Year, gpsRaw$Pack)
        table(gpsRaw$Pack, gpsRaw$Year)
        
        # wolves with diff packs listed in diff sources
                
        
        # quick n dirty trimming down of available locations
        gpsAOI <- crop(gpsLL, extent(saLL)) # just historic wolf locs in same extent as 2019 winter wolf locs
        plot(gpsAOI, add = TRUE, col = "blue") # (sanity check on map)
        wolfAOI <- data.frame(gpsAOI@data) # just the data from that spatial file
        table(wolfAOI$Pack, wolfAOI$Year) 
        wolfAOI$Pack <- trimws(wolfAOI$Pack) # fix annoying trailing ws for LGV
        
        # dn't bother with vhf for now because not comparable to gps
        wolfGPS <- wolfDatRaw[grepl("GPS", wolfDatRaw$Collar.Type), ] # only use gps data
        wolfGPS <- droplevels(wolfGPS) # kill stored factor levels
        wolfGPS$Pack <- trimws(wolfGPS$Pack) # fix annoying trailing ws for LGV
        
        
        # identify wolves, packs, and years in trimmed data
        trim <- wolfAOI %>%
          dplyr::select(Wolf, Pack, Year) %>%
          distinct() %>%
          left_join(wolfGPS, by = "Wolf") %>%
          rename(packHistDat = Pack.x, packMetadat = Pack.y)
        table(trim$Pack.x, trim$Year)
        # 1 wolf 2004-2004, 2 wolves 2006, 3 wolves 2007, 4 wolves 2008, then plenty 2009 on
        
        
   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
        
        
        
        ## code from before you cleaned up the data ##
        
        
        length(unique(wolfDatRaw$Wolf))
        wolfDatRawGPS <- wolfDatRaw[grepl("GPS", wolfDatRaw$Collar.Type), ]
        length(unique(wolfDatRawGPS$Wolf))
        
        # all GPS collar data made spatial (WGS84 lat/longs)
        gpsLL <- SpatialPointsDataFrame(
          data.frame("x" = as.numeric(gpsRaw$Longitude), "y" = as.numeric(gpsRaw$Latitude)),
            gpsRaw, proj4string = latlong)
        
        # all wolf collar locations from 2019 winter field study (to delineate study area)
        sa2019 <- read.csv("../PredationStudy/Clusters/collarLocsProcessed/locDat-20190409.csv")
        sa2019 <- filter(sa2019, !is.na(UTMX))
        
        # above, spatial (UTMs) reprojected to lat-longs
        saUTM <- SpatialPointsDataFrame(
          data.frame("x" = as.numeric(sa2019$UTMX), "y" = as.numeric(sa2019$UTMY)),
            sa2019, proj4string = utm) 
        saLL <- spTransform(saUTM, latlong)
        

        length(unique(saLL$Year))
        length(unique(saLL$packHistDat))        
        
         
        # quick n dirty trimming down of available locations
        gpsAOI <- crop(gpsLL, extent(saLL)) # just historic wolf locs in same extent as 2019 winter wolf locs
        gpsAOIrast <- spTransform(gpsAOI, latlongrast) # to crop rasters without reprojecting giant file first
        gpsAOIutm <- spTransform(gpsAOI, utm)
        
        # packs, indivs, and years from trimmed locations
        wolfAOI <- data.frame(gpsAOI@data) # just the data from that spatial file
        wolfAOI$Pack <- trimws(wolfAOI$Pack) # fix annoying trailing ws for LGV
        
        # don't bother with vhf because not comparable to gps
        wolfGPS <- wolfDatRaw[grepl("GPS", wolfDatRaw$Collar.Type), ] # only use gps data
        wolfGPS <- droplevels(wolfGPS) # kill stored factor levels
        wolfGPS$Pack <- trimws(wolfGPS$Pack) # fix annoying trailing ws for LGV
        
        
        # identify wolves, packs, and years in trimmed data
        trim <- wolfAOI %>%
          dplyr::select(Wolf, Pack, Year) %>%
          distinct() %>%
          left_join(wolfGPS, by = "Wolf") %>%
          rename(packHistDat = Pack.x, packMetadat = Pack.y)
        table(trim$packHistDat, trim$Year) # 1 wolf 2004-2004, 2 wolves 2006, 3 wolves 2007, 4 wolves 2008, plenty 2009 on
        
        # identify wolf-years of interest, number of years per wolf and per pack
        wolfYrDat <- dplyr::select(trim, Wolf, Year, packHistDat) %>% 
          distinct() %>% 
          group_by(Wolf) %>%
          mutate(nYrWolf = n()) %>%
          ungroup() %>%
          mutate(wolfYr = paste0(Wolf, Year))
        wolfYrs <- dplyr::select(wolfYrDat, wolfYr)
        
        length(unique(wolfYrs$Wolf))
        length(unique(wolfYrs$Year))
        length(unique(wolfYrs$packHistDat))
        # 50 wolves in 12 packs over 13 years. fuckin a.
        # i'm sure it'll be less after i look at the actual data...
        
        
        # format raw wolf data for subsetting to just wolves in years/areas of interest
       gpsFmt <- gpsRaw %>% 
         mutate(wolfYr = paste0(Wolf, Year)) %>%
         semi_join(wolfYrs, "wolfYr")
       length(unique(gpsFmt$Wolf))
       length(unique(gpsFmt$Year))
       # cool, still 50 wolves over 13 years
       
       # make subsetted data spatial
       gpsAOIll <- SpatialPointsDataFrame(
          data.frame("x" = as.numeric(gpsFmt$Longitude), "y" = as.numeric(gpsFmt$Latitude)),
            gpsFmt, proj4string = latlong)
       gpsAOIaea <- spTransform(gpsAOIll, crs(elev)) # kjb need to fix this
       plot(elev); plot(gpsAOIaea, add = TRUE)
       plot(gpsAOIaea); plot(elev, add = T)

       
              
       # good lord, this wolf data is all over the place
       # need to subset better
       
       
       # first check whether it wors like you think, AND not OR
             
      d1 <- data_frame(
        x = c(1, 2, 3),
        y = c("a", "b", "c"),
        a = rnorm(3)
        )
      
      d2 <- data_frame(
        x2 = c(1, 3, 6),
        y2 = c("a", "b", "c"),
        b = rnorm(3)
        )
      
      left_join(d1, d2, by = c("x" = "x2", "y" = "y2"))
      semi_join(d1, d2, c("x" = "x2", "y" = "y2"))
      # yes it's an AND, damn
             

       # check stuff in arc, sigh
      writeRaster(elev, file = paste0(datDir, "//Land//DEM//elevCrop.tif"))
      # ok the cropped elev dem extends too far south, that's part of the problem
      # but not all of it because i also have way too mich wolf data north and northeast
        
      
      # mmk yeah let's just spit out individual shapefiles and check them in arcmap, seems quickest
      
      
        
   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
      
      
      # investigating duplicate wolfIDs per yr
      z <- wolfYrsMaybe[duplicated(wolfYrsMaybe$wolfYr), ]
      zz <- as.character(wolfYrsMaybe[duplicated(wolfYrsMaybe$wolfYr), "wolf"])
      d1 <- gpsFmt[gpsFmt$Wolf == zz[1], ] %>% mutate(Date = mdy(Date))
      d2 <- gpsFmt[gpsFmt$wolfYr == "799M2014", ] %>% mutate(Date = mdy(Date))
      d3 <- gpsFmt[gpsFmt$wolfYr == "799M2016", ] %>% mutate(Date = mdy(Date))  
        
        
        
                  wolfFmt3 <- wolfRaw %>%
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
            # remove duplicate capture entry for 565F
            filter(if(wolfYr == "565F2009") !is.na(capUTMX))
        
        
        
        
        
        
        
        
        ##### to figure out why some packs are different between data sources ####
        packFix <- trim
        packFix$CaptureDate <- as.character(packFix$CaptureDate)
        packFix$yrMetadat = substr(packFix$CaptureDate, nchar(packFix$CaptureDate)-3, nchar(packFix$CaptureDate))
        packFix <- packFix %>%
          filter(packHistDat != packFix$packMetadat) %>%
          filter(!is.na(Wolf)) %>%
          rename(
            yrHistDat = Year, 
            transEnd = Dropoff..mort.date,
            Fate = Drop...Mort...Unknown.) %>%
          dplyr::select(Wolf, yrHistDat, packHistDat, yrMetadat, packMetadat, Sex, Age, CaptureDate, CaptureLocation,
                        transEnd, Fate, Collar.Type, UTM_X, UTM_Y)
        # investigate me  
        write.csv(packFix, file = "../Data/Wolf/wolfPackDiscrepancies.csv", row.names = FALSE)
        
        
        
    #### landcover type ####
        
        
        ?read.dbf
        read.dbf(paste0(datDir, "//Land//LandcoverType//NLCD//"))
        
        lcRaw <- raster(paste0(datDir, "//Land//LandcoverType//NLCD//", "nlcd_2011_landcover_2011_edition_2014_10_10.img"))
        toCrop <- spTransform(gpsLL, crs(lcRaw))
        lcCrop <- crop(lcRaw, extent(toCrop))
        memory.limit(size = 7500000)
        lc <- projectRaster(lcCrop, crs = crs(gpsLL))
        lc <- crop(lc, extent(saLL))
        plot(lc)
        plot(saLL, add = TRUE, col = "red")  
        
        unique(lc@data@values)
        sort(unique(round(lc@data@values), 0))
        
        hist(lc@data@values)
        
        lcLegend <- read.csv(paste0(datDir, "//Land//LandcoverType//NLCD//nlcdLegend.csv"))
        
        
        # extract info from pixels
        lc

        
    #### geojson files (buildings) ####
        
        
        # you can check driver names in rgdal via
        ogrDrivers()$name
        
        # and same in sf
        z <- st_drivers()
        
        # error reading geojson file
        file.exists(paste0(datDir, "/Human/Structures/Montana.geojson"))
        # ok, i didn't fuck up the filepath or filename...
        
        # tried updating rgdal jic; problem not solved
        
        ## known issue with rgdal and long filenames or filepaths or something?
        test <- readOGR(dsn = "C:\\Users\\Kristin\\Box Sync\\Documents\\Montana.geojson", layer = "OGRGeoJSON")
       
        # problem not solved
        
        # fuck it, try sf instead
        
        bldgs <- st_read(paste0(datDir, "/Human/Structures/Montana.geojson"))
        
        # damn, guess we're switching to sf

        
    #### canopy cover raster issue ####
        
        z <- extent(-129000, 864000, 2200000, 2550000)
        zz <- crop(canRaw, z)
        # still no
        # somethings wrong with the file maybe. trying redownload.
        
        
    #### visuals ####
        
        
         plot(dem); plot(saLL, add = TRUE) 
 
        
    #### projections ####
        
        test <- spTransform(gpsAOI, latlongrast)
        test2 <- crop(canRaw, extent(test))
        # well it's a new error message so i got that going for me which is nice
        
        
        
        
        
   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
        
        
    
    #### LANDCOVER TYPE INTEGER INVESTIGATION ####
        
        # ran dataPrepSpatial thru saLL read-in
        saAEA <- st_transform(saLL, paste(aea))
    
        lcRaw <- raster(paste0(datDir, "/Land/LandcoverType/NLCD/nlcd_2011_landcover_2011_edition_2014_10_10.img"))
        crs(lcRaw) # aea
        
        lcCrop <- crop(lcRaw, extent(saAEA))
        plot(lcCrop, legend = TRUE)
    
        lcLegendRaw <- read.csv(paste0(datDir, "//Land//LandcoverType//NLCD//nlcdLegend.csv"))    
        unique(lcLegendRaw$Value) # these are the numbers you're looking for
        
        unique(lcCrop@data@attributes$NLCS.2011.Land.Cover.Class) # NULL
        View(lcCrop@data) # k
        View(lcCrop@data@attributes) # k
        View(lcCrop@data@attributes[[1]]$NLCD.2011.Land.Cover.Class)
        
        test <- getValues(lcCrop)
        unique(test)
        unique(as.integer(test))
        sort(unique(as.integer(test)))
        unique(lcLegend$Value)
        
        # actual landcover raster is missing 51, 72, 73, 74 (which are the alaska-only ones, perfect)
        # so values are correct; you just were looking at something else I guess
        
        View(lcCrop@data)
        
        # ok i don't understand where the getValues is pulling from, but the numbers it gets are correct
        # so try extracting values for some points and see how that goes
        
        wolfLocs <- read.csv("wolfLocs-UsedAvail.csv")
        locsUTM <- SpatialPointsDataFrame(
          data.frame("x" = as.numeric(wolfLocs$X), "y" = as.numeric(wolfLocs$Y)),
          wolfLocs, proj4string = utm)
        locsAEA <- spTransform(locsUTM, aea)
        
        extLc <- extract(lcCrop, locsAEA)
        
        locsAEA$lcVal <- extract(lcCrop, locsAEA) # noice
        
        lcTypes <- lcLegendRaw %>%
          rename(lcVal = Value, lcType = Classification, lcClass = GenericClass) %>%
          dplyr::select(lcVal, lcType, lcClass)
        
        locsAEA@data <- left_join(locsAEA@data, lcTypes, by = "lcVal")
        View(locsAEA@data)
        
        used <- filter(locsAEA@data, Used == 1)
        table(used$lcClass)
        
    
        
   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
        
        
    
    #### NONMOTO DATA ####
        
        wolfLocs <- read.csv("wolfLocs-UsedAvail.csv")
        locsUTM <- SpatialPointsDataFrame(
          data.frame("x" = as.numeric(wolfLocs$X), "y" = as.numeric(wolfLocs$Y)),
          wolfLocs, proj4string = utm)  
        
        test <- locsUTM@data
        testNonmoto <- nonmotoUTM$MapColor
        test$nonmoto <- extract(nonmotoUTM, locsUTM)
        test2 <- extract(nonmotoUTM, locsUTM)
        
        
    
    
        
   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
        
        
    
    #### CANOPY COVER IO ERROR ####
        
        # issue: error when try to crop
        
        # can i transform?
        canTest <- projectRaster(canRaw, crs = utm)
        # no, same error

        
   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
        
        
    
    #### moto rd data - combining and processing ####        
        
      motmoPrelim <- rbind(motoCntyNoFS, motoBt) # nope
      motoPrelim <- union(motoCntyNoFS, motoBt) # nope
      motoPrelim <- merge(motoBt, motoCntyNoFS) # nope
      # try without the attached df
      t1 <- SpatialLines(motoCntyNoFS) # nope
      motoPrelim <- sp::merge(motoCntyNoFS, motoBt) # closer
      motoPrelim <- merge(motoCntyNoFS, motoBt, duplicateGeoms = TRUE) # samesies
      motoPrelim <- merge(motoCntyNoFS, motoBt, by.x = "name", by.y = "TRAIL_NAME", duplicateGeoms = TRUE) # samsesame
      motoPrelim <- spCbind(motoCntyNoFS, motoBt) # nope. this and above are trying to add a df to a spatial obj
      motoPrelim <- gUnion(motoCntyNoFS, motoBt) # AH FINALLY

       
      writeOGR(motoPrelim2, paste0(datDir, "/Human/Roads/roadsWinterTest.shp"), delete_layer = TRUE) 
      
      # oops gUnion doesn't retain df
      z <- raster::intersect(motoCntyNoFS, motoBt) # nope
      
      # decided to be lazy and just add a df of sequential numbers
      motoPrelim2 <- gUnion(motoPrelim, motoSho)
      jenkyDf <- data.frame(ID = 1:length(motoPrelim2@lines[[1]]@Lines))
      motoPrelimSpdf <- SpatialLinesDataFrame(motoPrelim2, data = jenkyDf)
      
      # export to double-check in arcmap before continuing
      writeOGR(motoPrelimSpdf, 
               dsn = paste0(datDir, "/Human/Roads"), 
               layer = "winterRoadsTest", 
               driver = "ESRI Shapefile", 
               overwrite_layer = TRUE)
      # confirmed.  
      
      ## identify WYDOT highways on USFS land
      #  motoDotFs <- gIntersection(motoDot, fs) ## no this hangs and requires crash
      # motoDotFs <- intersect(motoDot, fs) # no this reqs a vector
      # maybe it's just too big? (heh. never)
      # if this doesn't work, cut your losses and use arc
      motoDotCrop <- crop(motoDot, extent(fs))
      motoDotFs <- gIntersection(motoDotCrop, fs) # ha whaddaya know
      