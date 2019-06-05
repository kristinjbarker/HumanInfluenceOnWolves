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
 
    
    
    ## Study area and USFS boundaries (for clipping layers)
    saLL <- st_read(paste0(datDir, "/Land/studyAreaLL.shp")) 

        
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
                  
        
    ## Supplemental Feeding Areas
    feedRaw <- readOGR(paste0(datDir, "/Human/Feedgrounds"), layer = 'feedgroundsManualLL')
   
    
         

################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### ### ### ### ###    
####   | CROPPED, PROJECTED, RESOLUTION-ED DATA |  ####
### ### ### ### ### ### ### ### ### ### ### ### ### ###   
    


    ## Study area, in other projections
      
      saAEA <- st_transform(saLL, paste(aea))
      saUTM <- st_transform(saLL, paste(utm))
      
      
    
    #### Crop all to study area ####
    
      canCrop <- crop(canRaw, extent(saAEA)) 
      elevCrop <- crop(demRaw, extent(saAEA))
      lcCrop <- crop(lcRaw, extent(saAEA))
      strucCrop <- st_crop(strucRaw, extent(saLL))
      motoCrop <- crop(motoRaw, extent(saUTM))
      recCrop <- crop(recRaw, extent(saUTM))
      # preyCrop
  
      
    #### Project all to UTMs (and get aspect and ruggedness from cropped DEM) ####
    
      
      canUTM <- projectRaster(canCrop, crs = utm)
      elevUTM <- projectRaster(elevCrop, crs = utm)
      aspUTM <- terrain(elevUTM, opt = 'aspect')
      rugUTM <- terrain(elevUTM, opt = 'tri')
      slopeUTM <- terrain(elevUTM, opt = 'slope')
      lcUTM <- projectRaster(lcCrop, crs = utm)
      strucUTM <- st_transform(strucCrop, paste(utm))
      motoUTM <- spTransform(motoCrop, utm)
      recUTM <- spTransform(recCrop, utm)
      # preyUTM
      feedUTM <- spTransform(feedRaw, utm)
      
      
      
    #### Match resolutions so can stack ####
      
      
      
    #### Measure distance to road etc ####
     
     
        
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### #
####   | VISUALIZING OVERLAYS ETC |  ####
### ### ### ### ### ### ### ### ### ### #
        

        
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### 
####   | COMBINE AND EXPORT  |  ####
### ### ### ### ### ### ### ### ### 

      
    #### export cropped elevUTM file to define grid for elk distribution estimation
    
      #writeRaster(elevUTM, paste0(datDir, "/Land/DEM/elevCropUTM"), format = "GTiff", overwrite = TRUE)
    
    
    #### stack code for storing ... stacks ####
        
        # myRaster <- writeRaster(stk,"myStack.grd", format="raster")
        
        # just the dem-based data (elev, asp, rug) for now
        stk <- stack(elev, asp, rug)
        names(stk@layers) <- c("elev", "asp", "rug")
        writeRaster(stk, "../Data/Land/testStack.grd", format = "raster", overwrite = TRUE)
  
        
  