                      ### ### ### ### ### ###  ### ### ### ### ### ### 
                      #       PREPARING ELK DISTRIBUTION DATA        #
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
      "readxl",        ## import elk count data from excel workbook
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
    
    
    ## use DEM to define grid size and extent
    elevCrop <- raster(paste0(datDir, "/Land/DEM/elevCropUTM.tif"))
    
    
    ## read in elk count data from Aly 2014-2018
    rawFile <- paste0(datDir, "/Elk/Jackson_Elk_MidWinter_TrendCounts_2014_2019.xlsx")
    
    ## read in each tab separately (bc diff column names etc)
    raw2014 <- read_excel(rawFile, sheet = "Feb2014")
    raw2015 <- read_excel(rawFile, sheet = "Feb2015")
    raw2016 <- read_excel(rawFile, sheet = "Feb2016")
    raw2017 <- read_excel(rawFile, sheet = "Feb2017")
    raw2018 <- read_excel(rawFile, sheet = "Feb2018")
    raw2019 <- read_excel(rawFile, sheet = "Feb2019")
    

    
################################################################################################## #  
  
    
     
   #### prep data ####
    
    ## underlying grid to sample from
    grd <- as(elevCrop, 'SpatialGrid')
    lns <- gridlines(grd)
    plot(lns, add = T)
    # hm. looks like your grid cells are way bigger than you thought
    
    # so, step 2 is to decrease the grid cell size to the resolution of the dem
    # step 3, sample from certain cells
    # step 4, sample certain number of points from certain cells
    # step 5, cells and number of points from elk distn data    
      
    # sample from the grid - this just throws 100 points anywhere
    smp <- spsample(grd, n = 100, type = "random")
    plot(smp, add = T)
    
    
    
################################################################################################## #  
  
    
     
   #### helpful code, maybe #### 
    
    
    # manually generate random locations
    
      # could use in combo with point locations maybe, but seems onerous
      # https://gis.stackexchange.com/questions/277083/how-to-generate-random-points-influenced-by-underlaying-variable-in-r
      

    


        