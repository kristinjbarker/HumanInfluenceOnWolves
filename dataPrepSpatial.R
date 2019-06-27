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
 
    
    
    ## Buffered study area (for clipping layers)
    saLL <- st_read(paste0(datDir, "/Land/studyAreaLL.shp")) 

        
    ## Canopy cover     
    canRaw <- raster(paste0(datDir, "/Land/CanopyCover/NLCD/nlcd2011_usfs_conus_canopy_cartographic.img"))

    
    ## DEM 
    demRaw <- raster(paste0(datDir, "\\Land\\DEM\\SRTM_Cody_Albers.tif"))

    
    ## Landcover
    lcRaw <- raster(paste0(datDir, "//Land//LandcoverType//NLCD//", "nlcd_2011_landcover_2011_edition_2014_10_10.img"))
    lcLegendRaw <- read.csv(paste0(datDir, "//Land//LandcoverType//NLCD//nlcdLegend.csv")) 
        
       
    ## Recreational use restrictions
    recRaw <- readOGR(paste0(datDir, "/Human/RecAccess"), layer = 'Winter_travel_restrictions_November_2016')

    
    ## Buildings and structures
    strucRaw <- st_read(paste0(datDir, "/Human/Structures/Wyoming.geojson"))

 

################################################################################################## #  
  
    
    
### ### ### ###  ### ### ###    
####   | RASTER DATA |  ####
### ### ### ###  ### ### ###   
    


    #### Delineate study area in other projections ####
      
      saAEA <- st_transform(saLL, paste(aea))
      saUTM <- st_transform(saLL, paste(utm))

 
      
    #### Crop spatial layers to study area ####
          
      canCrop <- crop(canRaw, saAEA)
      elevCrop <- crop(demRaw, saAEA) 
      lcCrop <- crop(lcRaw, saAEA)
      recCrop <- crop(recRaw, saUTM)

      
      
    #### Match resolutions, and get aspect/ruggedness/slope from DEM ####
      
      can <- canCrop # canopy's 30m x 30m resolution is coarsest (and matches lc)
      elev <- resample(elevCrop, canCrop, "bilinear")
      asp <- terrain(elev, opt = 'aspect') 
      rug <- terrain(elev, opt = 'tri')      
      slope <- terrain(elev, opt = 'slope')   
      
      
    #### Rasterize recreation data ####  
      
      recAEA <- spTransform(recCrop, aea)
      recRast <- raster(nrow = nrow(can), ncol = ncol(can))
      extent(recRast) = extent(can)
      rec <- rasterize(recAEA, recRast, field = "MapColor")

    
    #### Combine and export ####
     
      # stack
      rastStk <- stack(can, elev, asp, rug, slope, lcCrop, rec)
      names(rastStk) <- c("can", "elev", "asp", "rug", "slope", "lc", "rec")
      
      # export
      writeRaster(rastStk, 
                  paste0(datDir, "/xProcessedRasters/AEA", names(rastStk)), 
                  bylayer = TRUE,
                  format = "GTiff",
                  overwrite = TRUE)

      

################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ###
####   | STRUCTURE DATA |  ####
### ### ### ### ### ### ### ###
       
      
    ## Crop to study area  
    strucCrop <- st_crop(strucRaw, saLL)

      
    ## Transform to AEA to match other spatial data
    #strucAEA <- st_transform(strucRaw, paste(aea))  
      
            
    ## Export shapefile
    st_write(strucCrop, paste0(datDir, "/Human/Structures/strucsLL.shp"))
      
      
# save.image(file = "dataPrepSpatial.RData")



  