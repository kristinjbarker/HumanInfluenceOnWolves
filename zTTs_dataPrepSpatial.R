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

        
      ## take 3 ##
        
        # start with the elk data, make it spatial, and make the extent and resolution match dem
        # then just sample from those grid cells, maybe
        
        elk <- SpatialPointsDataFrame(
          data.frame("x" = as.numeric(raw2014$UTM_X), "y" = as.numeric(raw2014$UTM_Y)),
          raw2014, proj4string = utm)
        
        plot(elk)
        plot(elevCrop); plot(elk, col = "red", add = TRUE)

        elk2 <- SpatialPointsDataFrame(
          data.frame("x" = as.numeric(raw2015$UTM_E), "y" = as.numeric(raw2015$UTM_N)),
          raw2015, proj4string = utm)     
        plot(elk2, col = "blue", add = T)
        
        
        elk3 <- SpatialPointsDataFrame(
          data.frame("x" = as.numeric(raw2019$UTM_X), "y" = as.numeric(raw2019$UTM_Y)),
          raw2019, proj4string = utm)     
        plot(elk3, col = "black", add = T)  
        
        
        # what's the resolution of the points? (do points even have resolutions?)
        str(elk)
        
        # this should be interesting
        test <- rasterFromXYZ(raw2014[,c("UTM_X", "UTM_Y", "Total")], 
                              res = res(elevCrop),
                              crs = utm)
        # error, x cell sizes are not regular
        # tried this with and without defining res
        # following help guidance leads me to attempt...
        
        test <- rasterize(x = raw2014$UTM_X, y = raw2014$UTM_Y, 
                          field = raw2014$Total)
        # this doesn't work because x is supposed to be an XY 
        # and y is supposed to be a raster
        
        # working thru the help examples, i still think i can do this somehow
        # but i have to set up an underlying raster first i guess
        r <- raster(ncols=36, nrows=18) # rasterlayer -like elevCrop
        n <- 1000
        set.seed(123)
        x <- runif(n) * 360 - 180
        y <- runif(n) * 180 - 90
        xy <- cbind(x, y)
        plot(r) # cant plot bc no values, fair enough
        r0 <- rasterize(xy, r)
        plot(r0) # ohhhh  ok it's pulling its values from numbers in x and y
        36*18
        # there are 648 cells so i don't really understand 
        # how it picks the numbers
        # y range of raster looks like -100ish - 100ish
        # x range looks like -175ish - 175ish
        summary(x) #mmk this aligns with x axis values
        summary(y) # right ok
        # so there are 1000 random combinations of xy values
        # and the raster is plotting those combos i think
        # but i'm not sure bc it doesn't make sense that some values would be
        # so close to 1000
        
        # how many points?
        r2 <- rasterize(xy, r, fun=function(x,...)length(x))
        plot(r2) # ohhhh THIS is the count
        
        # so if i create a blank raster
          # still need to figure out resolution but will deal with that later
        # then create a df of elk counts with the coords duplicated to correspond to the 
        
        # --ohhh yknow what, this is dumb
        # i don't want a raster
        # but i can just duplicate the points however many times
        # make that a spdf
        # and feed it to kernelud or whatever
        
        
        
      ## TAKE 4 ##
        
        test <- raw2014 %>%
          dplyr::select(UTM_X, UTM_Y, Total)
        testExp <- test[rep(row.names(test), test$Total), c("UTM_X", "UTM_Y")]
        # oh holy shit thank god someone already did that
        
        tSpdf <- SpatialPointsDataFrame(
          data.frame("x" = testExp$UTM_X, "y" = testExp$UTM_Y),
          testExp, proj4string = utm)
        plot(elevCrop)
        plot(tSpdf, add = T)
        plot(tSpdf)
        # can't see the duplicate points so will need to check UD 
        # eg to make sure it's  real fuckin hot on the refuge 
      
        tUDs <- kernelUD(tSpdf, h = "href")
        tHRs <- getverticeshr(tUDs, percent = 99)
        plot(tHRs)      
        
        tMCP <- mcp(tSpdf, percent = 100)
        
        plot(elevCrop); plot(tMCP, add = T); plot(tHRs, add = T, col = "blue") 
        plot(tUDs, add = T)
        
        # export UD to check heatiness in arc
        tRasty <- raster(tUDs)
        writeRaster(tRasty, paste("zTestTiff"), format="GTiff", overwrite=TRUE)   
        
        # hm i don't like missing out on the uhl hill counts
        # make sure they're really missing
        # if so, try UD with just the locations of counts but not number of animals considered
        
        
         writeOGR(tHRs, 
               dsn = paste0(datDir, "/Elk"), 
               layer = "zTestUD", 
               driver = "ESRI Shapefile", 
               overwrite_layer = TRUE)

         # yeah not only are those missing
         # but the UD blows the elk distn out across town which is a little silly
         
         
        # trying without the duplicated points per number of elk
        tSpdf2 <- SpatialPointsDataFrame(
          data.frame("x" = test$UTM_X, "y" = test$UTM_Y),
          test, proj4string = utm)         
      
        tUDs2 <- kernelUD(tSpdf2, h = "href")
        tHRs2 <- getverticeshr(tUDs2, percent = 99)
        plot(elevCrop); plot(tHRs, add = T); plot(tSpdf2, add = T)
        plot(tHRs2, add = T, col = "blue")
        # oh my that's much much worse. should've seen that coming
        
        tHRs3 <- getverticeshr(tUDs, percent = 90)
        plot(elevCrop); plot(tHRs3, add = T); plot(tSpdf2, add = T)
        # newp
               
        tHRs4 <- getverticeshr(tUDs, percent = 95)
        plot(elevCrop); plot(tHRs4, add = T); plot(tSpdf2, add = T)
        # newp    
        
        
        ## alright so it's either something to do with the 99% KDE
        ## or a binary 0/1 per cell depending whether elk were counted there
        
        
        # checking out the binary option
        
        
        # step1: create blank raster (i think)
        
        tR <- raster(nrows = nrows(elevCrop), ncols = ncols(elevCrop),
                     crs = crs(elevCrop), ext = extent(elevCrop),
                     resolution = res(elevCrop), vals = 0)
        
        # step 2: change values to 1 if elk were counted in the cell
        
        tR2 <- rasterize(cbind(test$UTM_X, test$UTM_Y), tR)
        plot(tR2)
        # nothing there
        
        tR2 <- rasterize(cbind(test$UTM_X, test$UTM_Y), tR, fun = function(x,...)length(x))
        plot(tR2)   
        # samesies
        
        tR2 <- rasterize(cbind(test$UTM_X, test$UTM_Y), tR, field = 1)
        plot(tR2)   
        # eff
        
        test$val = 1
        tR2 <- rasterize(cbind(test$UTM_X, test$UTM_Y), tR, test$val)
        plot(tR2)        
        # summary looks right but still see nothing on plot
        
        tR2 <- rasterize(cbind(test$UTM_X, test$UTM_Y), tR, test$val, fun = mean)
        plot(tR2)        
        
        
        summary(tR2)
        length(tR2)-36509674
        # yeah 118 1's, that's how many observation points there were
        # oh maybe start as null instead of 0
        # if that doesn't work, check it in arc
        
        tR3 <- raster(nrows = nrows(elevCrop), ncols = ncols(elevCrop),
                     crs = crs(elevCrop), ext = extent(elevCrop),
                     resolution = res(elevCrop), vals = NULL)   
        tr4 <- rasterize(cbind(test$UTM_X, test$UTM_Y), tR, test$val) 
        plot(tr4)
        
        # yeah ok maybe the res is just too fine-scale to see the 1s
        
        writeRaster(tr4, paste0(datDir, "/Elk/test2.tiff"), "GTiff")
        
        # this isn't working, just double-checked in arc
        
        
        
        
        
        
      ## TAKE 5ISH, 7 JUN ##
        
        # goal: make and assess utility of 0/1 binary grid val for elk presence
        
        # elk count data (just 1 yr)
        test <- raw2014 %>%
          dplyr::select(UTM_X, UTM_Y, Total)
        
        
        # elk count data made spatial
        tSpdf <- SpatialPointsDataFrame(
          data.frame("x" = test$UTM_X, "y" = test$UTM_Y),
          test, proj4string = utm)
        
        
        # blank raster to match up with count data
        tR <- raster(nrows = nrows(elevCrop), ncols = ncols(elevCrop),
                     crs = crs(elevCrop), ext = extent(elevCrop),
                     resolution = res(elevCrop), vals = 0)   
        
        # cell index for each elk count point in raster
        iCell <- cellFromXY(tR, tSpdf)
        
        # make raster cells with that index have value of 1
        tR[iCell] <- 1
        
        tR
        length(which(tR[] == 1))
        plot(tR)
        # this does work it's just too fine-scale to see i think
        # check in arc again
        
        writeRaster(tR, paste0(datDir, "/Elk/test3.tiff"), "GTiff")
        # yeah super fucking fine-scale, need to fix underlying grid
        
        
      
        
        # blank raster to match up with count data - inc resolution x10
        tR2 <- raster(crs = crs(elevCrop), ext = extent(elevCrop),
                     resolution = res(elevCrop)*100, vals = 0)   
        
        
        # cell index for each elk count point in raster
        iCell2 <- cellFromXY(tR2, tSpdf)
        
        # make raster cells with that index have value of 1
        tR2[iCell2] <- 1
        
        tR2
        length(which(tR2[] == 1))
        plot(tR2)
        writeRaster(tR2, paste0(datDir, "/Elk/test.tiff"), "GTiff")
        
        ## mmk 0/1 binary sucks no matter how i slice it
        ## BUT just discovered the option in kernelUD to use boundary, trying that 
        ## (amazing what you can do when you bother to read the fucking help file kristin)
        
        
        
        tUDs <- kernelUD(tSpdf, h = "href", boundary = fence)
        tHRs <- getverticeshr(tUDs, percent = 99)
        plot(tHRs)     
        # removed jagged fenceline near townt o fix first error
        # but now cant stop getting 
        # Error in 3 * h : non-numeric argument to binary operator
        
        
        tUDs <- kernelUD(tSpdf, h = 0.2)
        tHRs <- getverticeshr(tUDs, percent = 99)
        # newp can't do 99
        
        
        tUDs <- kernelUD(tSpdf, h = 0.5)
        tHRs <- getverticeshr(tUDs, percent = 95)
        # newp can't do 99      or 95
        
        
        
        tUDs <- kernelUD(tSpdf, h = 1)
        tHRs <- getverticeshr(tUDs, percent = 95)
        # newp can't do 99 or 95 with h=1 or h=5
        
        
        
        tUDs <- kernelUD(tSpdf, h = 5)
        tHRs <- getverticeshr(tUDs, percent = 99)
        # newp can't do 99   or 95
        
        
        tUDs <- kernelUD(tSpdf, h = "href")
        tHRs <- getverticeshr(tUDs, percent = 99)
        # h is 6116 so maybe i'll manually input that and see if r reads it as numeric now
        
        
        
        tUDs <- kernelUD(tSpdf, h = 6116, boundary = fence)
        # now the error is that the boundary segments are too small
        # which makes sense bc h is freakin huge
        
        
        
        tUDs <- kernelUD(tSpdf, h = 3000)
        tHRs <- getverticeshr(tUDs, percent = 99)
        # k...
        tUDs <- kernelUD(tSpdf, h = 3000, boundary = fence)
        # n
        tUDs <- kernelUD(tSpdf, h = 3, boundary = fence) # even 30 was too big
        plot(tUDs)
        
        # ok i just need to clip the resultant shps, this is ridiculous
        
        
        
        
        
        ## quick export of elk count data to verify utm locations
      writeOGR(elk, 
               dsn = paste0(datDir, "/Elk"), 
               layer = "test", 
               driver = "ESRI Shapefile", 
               overwrite_layer = TRUE)
        
        
        
        # here's some bad olde code from the dataprepelkdistn.R
        
            
    ## underlying grid to sample from
    grd <- as(elevCrop, 'SpatialGrid')
    lns <- gridlines(grd)
    plot(grd)
    plot(lns, col = "white", add = T)
    # hm. looks like your grid cells are way bigger than you thought

        
    ## use DEM to define grid size and extent
    elevCrop <- raster(paste0(datDir, "/Land/DEM/elevCropUTM.tif"))
    
    
    
    
    
    
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
        
        
    
  #### DISTANCE TO LINES AND POLYGONS ####  
    
    
    
    ## read in used & available wolf locs (test file from b4 finalized wolfYrs)
    locs <- read.csv("wolfLocs-UsedAvail.csv")
    

    # spatialize
    locSp <- SpatialPointsDataFrame(
      data.frame("x" = locs$X, "y" = locs$Y),
      locs, proj4string = utm)
    
    
        ## moto = lines. struc = polygons
    # 
    # #step 1. dist to lines (motorized roads)
    # 
    # distPrelim <- gDistance(locSp, motoUTM, byid = TRUE)
    # distRd <- apply(distPrelim, 2, function(X) rownames(distPrelim)[order(X)[1]])
    # 
    
    # ugh, this is unwieldy. fiiine i'll try yet another package
    library(geosphere)
    # test <- dist2Line(locSp, motoUTM)
    # nope, gotta be latlong (weird, you'd think flat would be better)
    locLL <- spTransform(locSp, ll)
    motoLL <- spTransform(moto, ll)
    test <- dist2Line(locLL, motoLL) # sloooooooooow
    # never mind that takes hours, not cool
    
    # just need smaller test data to figure out how to do the gDistance one
    tDat <- droplevels(sample_n(locs, 20))
    
    
    # add row number (for joining later)
    tDat$rowNum = row.names(tDat)
    
    # make spatial
    tDatSp <- SpatialPointsDataFrame(
      data.frame("x" = tDat$X, "y" = tDat$Y),
      tDat, proj4string = utm)
    

    # calculate shortest distance from each point to a road
    distRaw <- gDistance(tDatSp, motoUTM, byid = TRUE)
    

    # make distance longform
    distPrelim <- data.frame(
      rowNum = colnames(distRaw),
      distRd = distRaw[1,])
    
    # join distance values back to main dataframe
    distRd <- tDat %>%
      left_join(distPrelim, by = "rowNum")

    # noice
    
    
    # now try feedgrouds
    
    d2 <- gDistance(tDatSp, feedUTM, byid = TRUE)
    # make distance longform
    distPrelim2 <- data.frame(
      rowNum = colnames(d2),
      distFeed = d2[1,])    
    
     # join distance values back to main dataframe
    distRd <- tDat %>%
      left_join(distPrelim, by = "rowNum") %>%
      left_join(distPrelim2, by = "rowNum")

    # mmk structures migth get weird, may need to make spolydf
    d3 <- gDistance(tDatSp, strucUTM, byid = TRUE)
    # yup that didn't work
    
    strucUTM2 <- as(strucUTM, 'Spatial')
    # d this earlier in spatial code!
    
    d3 <- gDistance(tDatSp, strucUTM2, byid = TRUE)
    
    # ah shit i forgot i was pulling extra distances from the feedgrounds and structures 
    # need to be the closest ones
    
    d2.2 <- apply(d2, 2, min)
    # ysss apply ftw
    
    
    
    
    
    
  #############################################################################################
    
    #### |PROCESSING NEW WOLF COLLAR DATA| ####
    
    #### pull wolfID from variable file names ####
    
        ## look for 3 or more numbers followed by a letter
        
        t <- "Huckleberry_883F_1"
        gsub(pattern = "H", replacement = "F", x = t) #y
        grep(pattern = "[:digit:]+[:alpha:]", x = t) #n
        grep(pattern = "[1-9]+[a-z]", x = t) #n
        grep(pattern = "[1-9]", x = t) #y
        grep(pattern = "[1-9]+", x = t) #y
        grep(pattern = "[a-z]", x = t) #y
        grep(pattern = "[1-9]+[A-Z]", x = t) #oh. y.
        gsub(pattern = "[1-9]+[A-Z]", replacement = "F", x = t) 
        # perfect, now need to keep that instead of eplacing it
        gregexpr(pattern = "[1-9]+[A-Z]", perl = TRUE, text = t) #n
        
        # learn about capture groups
        # https://stackoverflow.com/questions/41559451/r-regex-match-pattern-and-store-only-the-pattern-in-a-new-column
        
        gsub(pattern = "[1-9]+[A-Z]", "\\1", t, perl = TRUE) # removes correct string
        gsub(pattern = ".+([1-9]+[A-Z]).+", "\\1", t, perl = TRUE) #only keeps last number of ID
        gsub(pattern = "([1-9]+[A-Z]).+", "\\1", t, perl = TRUE) # Keeps beginning of strong and entire wolfID
        gsub(pattern = ".+([1-9][1-9][1-9]+[A-Z]).+", "\\1", t, perl = TRUE) # works but not sure it works with 4digit
        t2 <- "Huckleberry_1883F_1"
        gsub(pattern = ".+([1-9][1-9][1-9]+[A-Z]).+", "\\1", t2, perl = TRUE) # yep nope
        # so need to figure out how to communicate "one or more repetitions" within the capture group
        gsub(pattern = ".+(+[1-9]+[A-Z]).+", "\\1", t2, perl = TRUE) #n
        gsub(pattern = ".+([1-9][A-Z]).+", "\\1", t2, perl = TRUE)  # still only the last number, so it's not reading the +
        gsub(pattern = ".+[1-9]+[A-Z].+", replacement = "F", t) # replaces everything
        gsub(pattern = ".+([1-9]+[A-Z]).+", replacement = "F", t) # ditto
        gsub(pattern = ".+([1-9]+[A-Z]).+", "\\1", t, perl = TRUE)
        regmatches(t, regexpr("[1-9]+[A-Z]", t)) # ha. ok then, something about the capture group just wasn't gonna work
        regmatches(t2, regexpr("[1-9]+[A-Z]", t2)) # perf
        
        
        
    #### load correct collar file(s) based on wolf id ####
        
        # load a single file by using correct index
        # get(iFileLoc)[grep(pattern = iWolf, x = get(iFileLoc))]
        tF <- filesState
        tP <- pathsState
        tP
        tP[1]
        t <- get(tP[i])
        # oh wtf, why did he change it to an xlsx?
        
        tF <- rawNames
        tP <- rawPaths
        tP[1]
        t <- get(tP[1]) # error not found
        file.exists(tP[1]) # true. huh.
        t <- read.csv(tP[1]) # oh. duh. alright then.
        grep(pattern = iWolf, x = tP)
        
        
    #### read in files differently according to their shitty shitty format ####
        
    #### read in multiple files for some individuals ####
        
        
        
      
################################################################################################## #  
  

        
           
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ## ###
####   | OLDER WOLF CODE USING CLEANED AND FORMATTED HISTORIC DATA |  ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ## ### 
        
        # for posterity's sake i guess
          
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
          
          
          
################################################################################################## #  
  

        
           
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ## ###
####   | FIXING WOLF PROCESSING BS |  ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ## ### 
          
          
          
          #### duplicate wolfYrs ####
          
          z <- locs[locs$wolfYr == "883F-2015", "Pack"]
          z
          # she has 2 diff packs, why?
          
          zwd <- intersect(wolvesHist, wolvesThem)
          zwd
          
          sort(unique(rawHist$Year))
          sort(unique(newOut$Year))
          
          # overlapping years 2014, 2015, 2016 historic data and "newer" data
          # prob easiest to skip processing "new" data if it was already processed
          
          # check 883 locs 1st
          z <- rawHist[rawHist$Wolf == "883F" & rawHist$Year == 2015, ]
          z1 <- z %>%
            dplyr::select(-c("Number", "X.1")) %>%
            mutate(datetime = ymd_hms(datetime),
                   Date = as.Date(Date, format = "%m/%d/%Y"))
          zz <- newOut[newOut$Wolf == "883F" & newOut$Year == 2015, ]
          zzz <- intersect(z1, zz)
          
          
          ## you jleft off here, code above kinda sucks
          
          ## you're trying to figure out
            ## a. which wolf-years you have duplicate data for, and 
            ## b. which data source to use when you have 2
          
          ## looks like they may not be exact duplicates, 
            ## eg timezone off by an hour or XY off by a meter
        
          # try using denver timezone so it knows when to switch
          OlsonNames() # see all timezone options
          
          # alright so now back to the wolf code 
          # if wolfyr is in historic, remove it from new
          
          # check which wolves we expect this to happen with
          # ...we... me, myself, and i? 
          # jfc get to work kristin
          
          wolfYrsHist <- locsHist %>%
            mutate(wolfYr = paste0(Wolf, "-", Year)) %>%
            dplyr::select(wolfYr) %>%
            distinct()
            
          wolfYrsThem <- newOut %>%
            mutate(wolfYr = paste0(Wolf, "-", Year)) %>%
            dplyr::select(wolfYr) %>%
            distinct()
            
          wolfYrsMe <- locsMe %>%
            mutate(wolfYr = paste0(Wolf, "-", Year)) %>%
            dplyr::select(wolfYr) %>%
            distinct()
          
          histThem <- intersect(wolfYrsHist, wolfYrsThem)
          histMe <- intersect(wolfYrsHist, wolfYrsMe)
          themMe <- intersect(wolfYrsThem, wolfYrsMe)
          
          histThem # 7, 4 wolves (883F, 974M, 799M, 997M) all 2016 & 2016 except 997M just 2015
          histMe # 0
          themMe # 0
          
          ## new question: if there are 7 duplicate wolfYrs why do i only have 3 issues?
          
          nrow(filter(locsHist, paste0(Wolf, "-", Year) == histThem[1, "wolfYr"])) #y
          nrow(filter(locsHist, paste0(Wolf, "-", Year) == histThem[2, "wolfYr"])) #y
          nrow(filter(locsHist, paste0(Wolf, "-", Year) == histThem[3, "wolfYr"])) #y
          nrow(filter(locsHist, paste0(Wolf, "-", Year) == histThem[4, "wolfYr"])) #y
          nrow(filter(locsHist, paste0(Wolf, "-", Year) == histThem[5, "wolfYr"])) #y
          nrow(filter(locsHist, paste0(Wolf, "-", Year) == histThem[6, "wolfYr"])) #y
          nrow(filter(locsHist, paste0(Wolf, "-", Year) == histThem[7, "wolfYr"])) #y
          
          # ok ijust need to get over it and remove these

            
                      ### ### ### ### ### ###  ### ### ### ### ### ### ###
                      #   DELINEATING USED & AVAILABLE COLLAR LOCATIONS  #
                      #       FOR ANALYSIS OF HUMAN INFLUENCE ON WOLF    #
                      #           DISTRIBUTIONS AND BEHAVIORS            #
                      #                                                  #
                      #            Kristin Barker | Summer 2019          #
                      #               kristinjbarker@gmail.com           #
                      ### ### ### ### ### ###  ### ### ### ### ### ### ###


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
                
        
        

################################################################################################## #  
  
    
    
### ### ### ### ### ### #
####   | MISC SPATIAL DATAWORK |  ####
### ### ### ### ### ### #
        
        
    ## challenge: measuring distance to structure takes for fucking ever and kills rstudio ####
        
        ## potential solutions:
          # combine all structures into one polygon? is that a thing?
          # um..... use greg's old computer
        
        # can't see how to reduce size
        # is it smaller as just sptialpolygons?
        strucSp <- SpatialPolygons(strucUTM@polygons)
        # i feel like that's not gonna help
        
        # union?
        ids <- rep("1", times = length(strucUTM))
        test <- unionSpatialPolygons(SpatialPolygons(strucUTM@polygons), ids)
        plot(test)
        
        # oh also should crop it more
        
        # cropped to slightly-buffered extent of wolf locs
        strucCrop <- crop(test, extent(locsUTM) + 100)
        plot(strucCrop)
        
        
        
    #### NAs in distance to prey ####
        
        z <- distDat[which(is.na(distDat$distPrey)), ]
        zsp <- SpatialPointsDataFrame(data.frame("x" = z$X, "y" = z$Y), z, proj4string = utm)
         
    #### close to feedgrounds but far from elk?!? ####
        
      z <- distDat[which(distDat$distFeed == 0 & distDat$distPrey > 0), ]
      unique(z$Year) # happens 10 times in just 2 yrs - 2010 & 2018
      
      # 2010
      plot(preyUTM[preyUTM$id == 2010, ])
      plot(feedUTM, add = T, col = "red")
      plot(zsp[zsp$Year == 2010, ], add = T, col = "blue")
      # ok this is due to fish ck feedground not being quite inside the elk distn this yr
      
      # 2018
      plot(preyUTM[preyUTM$id == 2018, ])
      plot(feedUTM, add = T, col = "red")
      plot(zsp[zsp$Year == 2018, ], add = T, col = "blue")
      # ohhh and this is because there were "no" elk up the gv in 2018
      
      # check things out in arc, maybe these numbers got mapped wrong?
      writeOGR(zsp, dsn = paste0(datDir, "/Wolf"), layer = "zDeleteMe",
               driver = "ESRI Shapefile", overwrite_layer = TRUE)
            
    
    
    
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### 
####  | PRELIM DATA VIZ  |  ####
### ### ### ### ### ### ### ### ### ###     
    
    ## prob move to separate file later 
    ## but i'm too fucking excited not to look at this real quick
    library(ggplot2)
    ggplot(data = modDat, aes(group = Used, y = distRd)) + geom_boxplot()
    ggplot(data = modDat, aes(group = Used, y = distFeed)) + geom_boxplot()
    ggplot(data = modDat, aes(group = Used, y = elev)) + geom_boxplot()
    ggplot(data = modDat, aes(group = Used, y = distPrey)) + geom_boxplot()
    ggplot(data = modDat, aes(group = Used, y = can)) + geom_boxplot()
    ggplot(data = modDat, aes(group = Used, y = SWE)) + geom_boxplot()
    # hahaha none of these look different
    # which means either i fucked something up in the data (likely)
    # or i fucked something up in the visuals (also likely)
    # or wolves aren't terribly selective (possible?)
    ggplot(data = modDat, aes(group = Used, y = distRd)) + geom_histogram()
    
    
    #### do something like this but with proportion of points rather than count
    table(modDat$Used, modDat$lcClass)
    
    
    
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### 
####  | SPATIAL DATA FIXES FROM RAW  |  ####
### ### ### ### ### ### ### ### ### ###    
    
    
    
    
    #### landcover NAs ####
    
    
    z <- modDat[is.na(modDat$lcClass),]
    zsp <- SpatialPoints(coords = data.frame(z$X, z$Y), proj4string = utm)
    zz <- raster(paste0(datDir, "/xProcessedRasters/slope.tif"))
    plot(zz)
    plot(z, add = T, col = "blue")
    
    # there aren't nas in the landcover values themselves
    # but many of them are decimals which doesn't allow them to map to classification
    
    # stepping thru lc parts of spatial data prep code to where where i effed up the raster
    
    # cropped version of raw file
    unique(lcPrelim) # these are just integers, thank god - so raw file is good
    
    # reprojected into utms
    unique(lcUTM) # ooook this is where it gets fucked
    # because of the reprojection, duh
    # let's try to figure out if rounding would make sense or not
    # but use the smaller cropped file
    
    # i guess if not you'll have to use the location latlongs to pull these values from the original aea file
    
    
    # cropped utms
    z <- data.frame(l = unique(lcCrop))
    z$lRnd = round(z$l, 0)
    unique(z$lRnd)
    unique(lcLegendRaw$Value)
    # hahaha yeah nope, rounding isn't gonna work
    # so need to deal with landcover separately
    
    # which makes me think i should consider dealing with other rasters in their original projections as well
    
    # so. new plan for the day. 
    
    # look at each raster and decide whether you need to use its original projection
    
    # find the most common projection (aea i think) and translate the wolf locations into that for extraction
    # stack all possible but don't stress it if that doesn't make sense with how the data work
    
    
#### remove aea from raster names ####
    
    z <- names(rast)
    zz <- substr(z, 0, 1)
    zz
    zz <- substr(z, 4, nchar(z))
    zz
    
    
    
#### fix mapping of recreation (you got factor levels not data) ####    
    
    z <- raster(paste0(datDir, "/xProcessedRasters/AEArec.tif"))
    plot(z)
    z@data
    z@legend
    # lost data when rasterized
    # gonna quick-fix for now by visually matching factor levels with correct zone
    # checking in arc...
    # 3 = Blue (anything goes)
    # 2 = "Light Purple" (no snowmobiles off-trail)
    # 1 = "Dark Purple" (no off-trail recreation - winter closure)
    
    
    
    
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### ### 
####  | DATA VIZ  |  ####
### ### ### ### ### ### ### ### ### ###    
    
    
    #### plotting frequency of categorical covariates (eg lc) ####
    
        
        pL <- ggplot(z, aes(x = lcClass, y = prop, fill = Used), 
                     stat = "identity", position = "dodge")
        pL
    
    
        
        pL <- a + geom_histogram(aes(lc, y = ..count../sum(..count..),
                                     fill = Used), position = "dodge")
        pL
        
        pL <- a + geom_histogram(aes(lcType, fill = Used), position = "dodge")
        pL
          
        grid.arrange(pC, pL)
        
        
    #### misc prelim plots ####    
        
        hist(modDat[modDat$Used == 1, "lc"])
        hist(modDat[modDat$Used == 0, "lc"])
        
        freq(modDat[modDat$Used == 1, "can"])
        hist(modDat[modDat$Used == 0, "lc"])
        
        
        library(cowplot)
        plot_grid(pC, pN, pE, pS, pR, pW, pL, ncol = 4, rel_widths = c(1, 1, 1, 1, 2))
        
        pA <- plot_grid(pC, pN, pE, pS, pR, pW, ncol = 2)
        pB <- plot_grid(pA, pL, rel_widths = c(1, 2))
        pB
        
        plot_grid(pA, pL, nrow = 2, rel_heights = c(0.75, 0.25))
        
    
    
    