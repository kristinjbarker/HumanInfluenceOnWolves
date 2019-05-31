                      ### ### ### ### ### ###  ### ### ### ### ### ### 
                      #    FORMATTING AND PREPATING ROAD DATA        #
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
  
    
     
   #### Read in raw  data ####
    
    fs <- readOGR(paste0(datDir, "/Land/Ownership"), layer = 'LandBtforest')
    motoCntyRaw <- readOGR(paste0(datDir, "/Human/Roads"), layer = 'WinterRoads_AllCounties')
    motoBtRaw <- readOGR(paste0(datDir, "/Human/Roads"), layer = 'BTWinterTrails_Final_2019')
    motoShoRaw <- readOGR(paste0(datDir, "/Human/Roads"), layer = 'Sho_MotorizedTrails')
    motoDotRaw <- readOGR(paste0(datDir, "/Human/Roads"), layer = 'WYDOT_Roadway_Names')


################################################################################################## #  
  
    
    

    #### Combine & process data ####    
    
      
      ## make all roads data the same projection as the fs boundary
      motoCnty <- spTransform(motoCntyRaw, crs(fs))
      motoBt <- spTransform(motoBtRaw, crs(fs))
      motoSho <- spTransform(motoShoRaw, crs(fs))     
      motoDot <- spTransform(motoDotRaw, crs(fs)) 
      
      
      ## combine bridger-teton and shosone NF data
      motoFs <- gUnion(motoBt, motoSho)
      
      
      ## identify WYDOT highways on USFS land
      motoDotCrop <- crop(motoDot, extent(fs)) # crop 1st to speed processing
      motoDotFs <- gIntersection(motoDotCrop, fs) 
      
      
      ## remove county data where it overlaps with the more accurate USFS data 
      motoCntyNoFS <- erase(motoCnty, fs)

      
      ## use the USFS data instead in those areas
      motoCntyFs <- gUnion(motoCntyNoFS, motoFs) 
      
      ## add in highways on USFS land
      moto <- gUnion(motoCntyFs, motoDotFs)

      ## add df to the spatiallines and export as shp to hand-fix little things
      jenkyDf <- data.frame(ID = 1:length(moto@lines[[1]]@Lines))
      motoSp <- SpatialLinesDataFrame(moto, data = jenkyDf)
      writeOGR(motoSp, 
               dsn = paste0(datDir, "/Human/Roads"), 
               layer = "winterRoads", 
               driver = "ESRI Shapefile", 
               overwrite_layer = TRUE)   