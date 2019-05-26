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
 

        
        
    #### Canopy Cover #### 
        
        
        canRaw <- raster(paste0(datDir, "/Land/CanopyCover/NLCD/nlcd2011_usfs_conus_canopy_cartographic.img"))
        can <- crop(canRaw, extent(saAEA))
 
        
        
        
        
    #### DEM ####   
        
        demRaw <- raster(paste0(datDir, "\\Land\\DEM\\SRTM_Cody_Albers.tif"))
        elev <- crop(demRaw, extent(saAEA))
        asp <- terrain(elev, opt='aspect')
        rug <- terrain(elev, opt='tri')
        
        
        
    #### Landcover ####
        
        
        lcRaw <- raster(paste0(datDir, "//Land//LandcoverType//NLCD//", "nlcd_2011_landcover_2011_edition_2014_10_10.img"))
        lc <- crop(lcRaw, extent(saAEA))

        lcLegend <- read.csv(paste0(datDir, "//Land//LandcoverType//NLCD//nlcdLegend.csv"))        


        
        
    #### Buildings and Structures ####
        
        bldgsRaw <- st_read(paste0(datDir, "/Human/Structures/Wyoming.geojson"))
        bldgs <- st_crop(bldgsRaw, extent(gpsAOI))
        plot(bldgs, add = TRUE)

       
            

        
        
    #### Motorized Use Areas ####         
        
        
        # from owen
        motoRaw <- st_read(paste0(datDir, "/Human/Roads/roads_GYEandWY_Tiger2016_AEA.shp"))
        moto <- st_crop(motoRaw, extent(saAEA))
        plot(elev); plot(moto, add = TRUE, main = "TIGER")
        
        
        
        # mvum [in progress]
        motoRaw2 <- st_read(paste0(datDir, "/Human/Roads/Trans_MVUM_Roads_2018.shp"))
        moto2 <- st_crop(motoRaw2, extent(gpsAOIutm))

        plot(elev); plot(moto2[1], add = TRUE, main = "MVUM")
        plot(moto2[1])
        plot(moto2$RTE_CN)
        
        
        # from county
        motoRaw3 <- st_read(paste0(datDir, "/Human/Roads/roadcl_segmented.shp"))
        plot(motoRaw3)
        
        
        # snowmobile trails from jason
        sledRaw <- st_read(paste0{datDir, "/Human/"})
        
        
        
    #### Non-motorized Use Areas ####      
        
        
        nonmotoRaw <- st_read(paste0(datDir, "/Human/RecAccess/Winter_travel_restrictions_November_2016.shp"))
        plot(nonmotoRaw)
        
        
      
          
        
    #### Prey Distribution ####  
        
        
                
        
    #### Supplemental Feeding Areas ####    
        

        
        
        
        
        
        
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### #
####   | VISUALIZING OVERLAYS ETC |  ####
### ### ### ### ### ### ### ### ### ### #
        

        
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### 
####   | COMBINE AND EXPORT  |  ####
### ### ### ### ### ### ### ### ### 
 
    
    
    #### stack code for storing ... stacks ####
        
        # myRaster <- writeRaster(stk,"myStack.grd", format="raster")
        
        # just the dem-based data (elev, asp, rug) for now
        stk <- stack(elev, asp, rug)
        names(stk@layers) <- c("elev", "asp", "rug")
        writeRaster(stk, "../Data/Land/testStack.grd", format = "raster", overwrite = TRUE)
  
        
  