                      ### ### ### ### ### ###  ### ### ### ### ### ### 
                      #    FORMATTING AND PREPARING SPATIAL DATA     #
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
      "rgeos",         ## gDifference (clip) - nevermind, use erase to retain sf
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
    
    ll <- CRS("+init=epsg:4326") # WGS 84
    utm <- CRS("+init=epsg:3742") # NAD83(HARN)/UTMzone12N 
    aea <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
    
    
  #### Increase memory limit ####
    
    memory.limit(size = 7500000)
 

################################################################################################## #  
  
    
    
### ### ### ### ### ### #
####   | RAW DATA |  ####
### ### ### ### ### ### #
 
    
    
    ## Study area and super-buffered study area (for clipping layers)
    saLL <- st_read(paste0(datDir, "/Land/studyAreaLL.shp")) 
    saPlusLL <- st_read(paste0(datDir, "/Land/studyAreaPlusLL.shp")) 

        
    ## Canopy cover     
    canRaw <- raster(paste0(datDir, "/Land/CanopyCover/NLCD/nlcd2011_usfs_conus_canopy_cartographic.img"))

    
    ## DEM 
    demRaw <- raster(paste0(datDir, "\\Land\\DEM\\SRTM_Cody_Albers.tif"))

    
    ## Landcover
    lcRaw <- raster(paste0(datDir, "//Land//LandcoverType//NLCD//", "nlcd_2011_landcover_2011_edition_2014_10_10.img"))
    lcLegendRaw <- read.csv(paste0(datDir, "//Land//LandcoverType//NLCD//nlcdLegend.csv")) 
        
    
    ## Buildings and Structures
    strucRaw <- st_read(paste0(datDir, "/Human/Structures/Wyoming.geojson"))

 
    ## Motorized Roads
    motoRaw <- readOGR(paste0(datDir, "/Human/Roads"), layer = 'winterRoads')

        
    ## Recreational use restrictions
    recRaw <- readOGR(paste0(datDir, "/Human/RecAccess"), layer = 'Winter_travel_restrictions_November_2016')
    recLegend <- recRaw@data %>%
      dplyr::select(MapColor, Wheeled_Ve, Over_Snow_, Non_Motori) %>%
      distinct() %>%
      rename(recCol = MapColor, recMoto = Wheeled_Ve, recSled = Over_Snow_, recNonmoto = Non_Motori)


        
    ## Prey Distribution
    elkRaw <- readOGR(paste0(datDir, "/Elk"), layer = 'elkDistn_2008-2019')              
        
    
    ## Supplemental Feeding Areas
    feedRaw <- readOGR(paste0(datDir, "/Human/Feedgrounds"), layer = 'feedgroundsManualLL')


################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### ### ### ### ###    
####   | CROPPED, PROJECTED, RESOLUTION-ED DATA |  ####
### ### ### ### ### ### ### ### ### ### ### ### ### ###   
    


    ## Study area, in other projections
      
      saAEA <- st_transform(saLL, paste(aea))
      saPlusAEA <- st_transform(saPlusLL, paste(aea))
      saUTM <- st_transform(saLL, paste(utm))
      saPlusUTM <- st_transform(saPlusLL, paste(utm))
      
      
      
      
    #### Crop all to study area  ####
    
      canPrelim <- crop(canRaw, extent(saPlusAEA)) 
      elevPrelim <- crop(demRaw, extent(saPlusAEA))
      lcPrelim <- crop(lcRaw, extent(saPlusAEA))
      strucPrelim <- st_crop(strucRaw, extent(saLL)) ## Error if use saPlus
      motoUTM <- crop(motoRaw, extent(saPlusUTM))
      recUTM <- crop(recRaw, extent(saPlusUTM))

  
      
          
    #### Project all to UTMs (and get aspect/ruggedness/slope from dem) ####
    
      
      canUTM <- projectRaster(canPrelim, crs = utm)
      elevUTM <- projectRaster(elevPrelim, crs = utm) 
      aspUTM <- terrain(elevUTM, opt = 'aspect') 
      rugUTM <- terrain(elevUTM, opt = 'tri')      
      slopeUTM <- terrain(elevUTM, opt = 'slope') 
      lcUTM <- projectRaster(lcPrelim, crs = utm)
      strucUTM <- st_transform(strucPrelim, paste(utm))
      feedUTM <- spTransform(feedRaw, utm)
      
      
    #### Crop to actual study area ####
          
      canCrop <- crop(canUTM, saUTM)
      elevCrop <- crop(elevUTM, saUTM) 
      aspCrop <- crop(aspUTM, saUTM) 
      rugCrop <- crop(rugUTM, saUTM)      
      slopeCrop <- crop(slopeUTM, saUTM) 
      lcCrop <- crop(lcUTM, saUTM)
      strucPrelim2 <- st_crop(strucUTM, saUTM)
      strucCrop <- as(strucPrelim2, 'Spatial')
      recCrop <- recUTM
      motoCrop <- motoUTM
      elk <- elkRaw
      
      
      
    #### Match resolutions and stack rasters ####

      
      # match
      can <- canCrop
      elev <- resample(elevCrop, can, "ngb")
      asp <- resample(aspCrop, can, "ngb")
      rug <- resample(rugCrop, can, "ngb")
      slope <- resample(slopeCrop, can, "ngb")
      lc <- lcCrop
      recRast <- raster(nrow = nrow(can), ncol = ncol(can))
      extent(recRast) = extent(can)
      rec <- rasterize(recCrop, recRast, field = "MapColor")
     
      # stack
      rastStk <- stack(can, elev, asp, rug, slope, lc, rec)
      names(rastStk) <- c("can", "elev", "asp", "rug", "slope", "lc", "rec")
      
      # export
      writeRaster(rastStk, 
                  paste0(datDir, "/xProcessedRasters/", names(rastStk)), 
                  bylayer = TRUE,
                  format = "GTiff",
                  overwrite. = TRUE)




# save.image(file = "dataPrepSpatial.RData")

      


  