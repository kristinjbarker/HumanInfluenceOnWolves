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
        