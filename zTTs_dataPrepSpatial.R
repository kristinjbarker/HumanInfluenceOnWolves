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
      
      
      

        
   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
        
        
    
    #### SNOW data - pull swe ####         
      
      #need to replace the comma and all characters before it with ""
      z <- as.factor(paste((rawGC[1,1]))) # just to structure the same as in the data frame
      z
      gsub(",", "penis", z) # replaces comma, perf
      gsub("^*,", "penis", z) # does nothing
      gsub("*,", "penis", z) # only comma
      gsub(".,", "penis", z) # still only commma
      gsub(",", "penis", z)
      
      # what if it's a char
      z <- as.character((rawGC[1,1]))
      z
      gsub("^.,", "penis", z)
      # alright fuck this i'm just gonna split the string at the comma
      
      test <- unlist(strsplit(rawGC$Date.Snow.Water.Equivalent..in..Start.of.Day.Values., ",", fixed = TRUE))
      # e: non character argument
      test <- unlist(strsplit(as.character(rawGC$Date.Snow.Water.Equivalent..in..Start.of.Day.Values.), ",", fixed = TRUE))
      test # null
      test <- unlist(strsplit(as.character(rawGC$Date.Snow.Water.Equivalent..in..Start.of.Day.Values.), "//,", fixed = TRUE))
      test # null
      
      # fiiine back to regex
      z <- as.factor(paste((rawGC[1,1]))) 
      gsub("'\\s*,.*", "", z)
      gsub(".*(,.*)", "", z)
      z <- as.character((rawGC[1,1]))
      gsub(".*(,.*)", "", z)
      
      
    # ok quit being a little bitch and put some effort into this
      
      # figure out whether you have to make it a character or not
      zf <- as.factor(paste((rawGC[1,1]))) 
      zc <- as.character((rawGC[1,1]))
      
      # can you match any character or number or punctuation or anything
      gsub(".*", "penis", zf) # yes. lovely.
      
      # now you just have to match characters before a comma
      gsub(".*(,)", "f", zf)
      # oh dude was that it?
      
      gsub(".*(,)", "", zf)
      # yuuup
      
      
      
      
   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
      
      
      #### ~ regex notes ~ ####
      
        # . <- any character
        # * <- any number of times
        # () <- look for the pattern inside these
      
      
      
   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
        
        
    
    #### ELK DISTN data - generate points in certain grid cells ####   
      
      # playing with code from https://gis.stackexchange.com/questions/291446/generating-random-points-inside-raster-boundary-using-r
      
      z <- rasterToContour(is.na(grid))
      plot(grid)
      plot(z, add = T)
      # ok so rastToCont just outlines the grid raster, cool
      
      zz <- as(st_union(st_polygonize(st_as_sf(z))), 'Spatial')
      # gives an error
      # and just realized this code is specific to random samples anyway
      # switching to steal someone elses code
      
      
      # now https://gis.stackexchange.com/questions/277083/how-to-generate-random-points-influenced-by-underlaying-variable-in-r
      cell <- sample(1:ncell(grid), nrow(raw2014), prob = grid[], replace = TRUE)
      # error bc you have NAs in your grid raster
      
      # let's try our own brain for once shall we?
      
      # 1. make your counts spatial, that seems helpful
      z <- SpatialPointsDataFrame(data.frame("x" = raw2014$UTM_X, "y" = raw2014$UTM_Y),
                                  raw2014, proj4string = utm)
      xy <- c()
        
        
        
      # code to pull from
      r = raster(matrix(1:12,3,4))
      set.seed(23) # make this all reproducible
      cell = sample(1:ncell(r),100, prob=r[], replace=TRUE)
      head(cell)
      centres = xyFromCell(r,cell)
      View(centres)
      str(centres)
      # it's just numbers with attributes
      head(centres)
      
      # can you change the attr yourself?
      dimnames(centres) = c("doesThis", "shitWork")
      # no must be list
      
      #make named list of xy coords from elk counts
      t <- list(x = raw2014$UTM_X, y = raw2014$UTM_Y)
      # duh you don't want a list
      # you wnt a anamed vector(s?) of numbers
      
      tt <- c(raw2014$UTM_X, raw2014$UTM_Y)
      names(t) <- c("x", "y")
      
      # mmk go back to code then figure it out
      rx = centres[,1] + runif(nrow(centres), -0.5*res(r)[1], 0.5*res(r)[1])
      
      ## ooh found on another stack exchange there's a spatialgridthing
      gridSp <- SpatialGrid(GridTopology(grd, cellsize = res(grd)), proj4string = utm)
      # hm no several iterations of the above have failed
      
      # oh try new tack - https://gis.stackexchange.com/questions/43707/how-to-produce-spatial-grid-from-raster
      g <- as(grd, 'SpatialGridDataFrame')
      # oh wtf that was super easy
      # mmm but that's a huge file that kils r
      
      
      
      ## take 2: now with independent thought! ##
      
      
        ?spsample
        # i think i need to remove NAs from the grid (?)
        z <- !is.na(grd)
        plot(z) # i have no idea why this just gave them all the same value but fuck it
        print(object.size(grd))
        print(object.size(z))
        # oh interesting, removing those NAs made it a rasterlayer 
        # which significantly incresed the size. like by orders of magnitude. 
        # mk let's try without removing NAs then
        
        # first need to sample from a grid. 
        # then figure out how to sample certain number of points per cell.
        z <- spsample(grd, n = 10) # no. can't sample from rasterlayer
        
        # try making a spatial grid from which t sample
        spGrd <- as(grd, 'SpatialGrid')
        
        z <- spsample(spGrd, n = 10, type = "regular")
        plot(spGrd); plot(z, col = "red", add = T)
        zz <- spsample(spGrd, n = 10, type = "random")
        plot(zz, col = "blue", add = T)
        # neato
        zzz <- spsample(spGrd, n = 10, type = "clustered")
        plot(zzz, col = "green", add = T)
        
        # oh maybe i can use an apply function to go through each grid cell
        # and sample a certain number of points based on the elk counts
        
        # first make your DEM a square, this should help with the weird na thing and give full coverage (hopefully)
        demRaw <- raster(paste0(datDir, "\\Land\\DEM\\SRTM_Cody_Albers.tif"))
        demUTM <- projectRaster(demRaw, crs = utm)
        saUTM <- st_transform(saLL, paste(utm))
        elevCrop <- crop(demUTM, saUTM)
        plot(elevCrop)
        # oh yeah that's better
        writeRaster(elevCrop, paste0(datDir, "/Land/DEM/elevCropUTM"), format = "GTiff", overwrite = TRUE)
        
        # now make it a grid to sample from
        grd <- as(elevCrop, 'SpatialGrid')
        
        # sample from the grid
        smp <- spsample(grd, n = 100, type = "random")
        plot(smp, add = T)
        
        # most excellent
        
        # step 2: sample from certain cells
        # step 3: sample certain number of points from certain cells
        
        # step 2 attempts
        
        # 2a. identify one specific cell
        str(grd)
        grd@grid@cellcentre.offset
        grd@grid@cells.dim
        
        lns <- gridlines(grd)
        plot(lns, add = T)
        # hm. looks like your grid cells are way bigger than you thought
        
        # so, step 2 is to decrease the grid cell size to the resolution of the dem
        # step 3, sample from certain cells
        # step 4, sample certain number of points from certain cells
        # step 5, cells and number of points from elk distn data

        
      
      
      