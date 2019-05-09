                      ### ### ### ### ### ###  ### ### ### ### ### ### 
                      #    FORMATTING AND PREPATING SPATIAL DATA     #
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
    
    latlong <- CRS("+init=epsg:4326") # WGS 84
    utm <- CRS("+init=epsg:3742") # NAD83(HARN)/UTMzone12N 
    latlongrast <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
    
 

################################################################################################## #  
  
    
    
### ### ### ### ### ### #
####   | RAW DATA |  ####
### ### ### ### ### ### #
 
    
    
    #### Wolf, Pack, and Collar Information #### 
    
        # all GPS collar data
        gpsRaw <- read.csv(paste0(datDir, "\\Wolf\\wolfHistoricCleaned.csv"))
        wolfDatRaw <- read.csv(paste0(datDir, "\\Wolf\\CaptureAndCollarInfo\\wolf_metadata.csv"))
        
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
        
         
        # quick n dirty trimming down of available locations
        gpsAOI <- crop(gpsLL, extent(saLL)) # just historic wolf locs in same extent as 2019 winter wolf locs
        gpsAOIrast <- spTransform(gpsAOI, latlongrast) # to crop rasters without reprojecting giant file first
        gpsAOIutm <- spTransform(gpsAOI, utm)

 
        
        
        
    #### Canopy Cover #### 
        
        
        canRaw <- raster(paste0(datDir, "/Land/CanopyCover/NLCD/nlcd2011_usfs_conus_canopy_cartographic.img"))
        can <- crop(canRaw, extent(gpsAOIrast))
 
        
        
        
        
    #### DEM ####   
        
        demRaw <- raster(paste0(datDir, "\\Land\\DEM\\SRTM_Cody_Albers.tif"))
        elev <- crop(demRaw, extent(gpsAOIrast))
        asp <- terrain(dem, opt='aspect')
        rug <- terrain(dem, opt='tri')
        
        
        
    #### Landcover ####
        
        
        lcRaw <- raster(paste0(datDir, "//Land//LandcoverType//NLCD//", "nlcd_2011_landcover_2011_edition_2014_10_10.img"))
        lc <- crop(lcRaw, extent(gpsAOIrast))

        lcLegend <- read.csv(paste0(datDir, "//Land//LandcoverType//NLCD//nlcdLegend.csv"))        


        
        
    #### Buildings and Structures ####
        
        bldgsRaw <- st_read(paste0(datDir, "/Human/Structures/Wyoming.geojson"))
        bldgs <- st_crop(bldgsRaw, extent(gpsAOI))
        plot(bldgs, add = TRUE)

       
            

        
        
    #### Motorized Use Areas ####         
        
        
        # from owen
        motoRaw <- st_read(paste0(datDir, "/Human/Roads/roads_GYEandWY_Tiger2016_AEA.shp"))
        moto <- st_crop(motoRaw, extent(gpsAOIrast))
        plot(elev); plot(moto, add = TRUE, main = "TIGER")
        
        
        
        # mvum [in progress]
        motoRaw2 <- st_read(paste0(datDir, "/Human/Roads/Trans_MVUM_Roads_2018.shp"))
        moto2 <- st_crop(motoRaw2, extent(gpsAOIutm))

        plot(elev); plot(moto2[1], add = TRUE, main = "MVUM")
        plot(moto2[1])
        plot(moto2$RTE_CN)
        
        
        # from county
        motoRaw3 <- st_read(paste0(datDir, "/Human/Roads/Trans_MVUM_Roads_2018.shp"))
        
        
        
    #### Non-motorized Use Areas ####      
        
      
          
        
    #### Prey Distribution ####  
        
        
                
        
    #### Supplemental Feeding Areas ####    
        

        
        
        
        
        
        
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### #
####   | VISUALIZING OVERLAYS ETC |  ####
### ### ### ### ### ### ### ### ### ### #
        

        
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### #
####   | CLEANED & FORMATTED DATA |  ####
### ### ### ### ### ### ### ### ### ### #
 
    
    
    #### Wolf Locations ####  
        
       
        # only use GPS collar data if have enough, or you have to do separate analyses
        wolfDat <- wolfDatRaw %>% 
          # only consider wolves and years of data within study area
          
        
        # filter out data from PACKS that overlap the extent of gpsLL
        
  