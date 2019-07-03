           ### ### ### ### ### ###  ### ### ### ### ### ### ### ### ### ### ### ### 
           #                                                                      #
           #        SUMMARIES, VISUALIZATIONS, AND PRELIMINARY DATA WORK          #
           #  FOR ASSESSING HUMAN INFLUENCE ON WOLF DISTRIBUTIONS AND BEHAVIORS   #
           #                                                                      #
           #                      Kristin Barker | Summer 2019                    #
           #                        kristinjbarker@gmail.com                      #
           #                                                                      #
           ### ### ### ### ### ###  ### ### ### ### ### ### ### ### ### ### ### ### 


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
      #"raster",        ## raster stacking, extraction, etc
      #"maptools",      ## kmlPoints
      #"rgdal",         ## spatial/shapefile work 
      #"rgeos",         ## gDistance and other spatial work
      #"sp",            ## spatial work
      #"sf",            ## spatial work like the kids are doing it these days
      "ggplot2",        ## totes adorbs plots
      "gridExtra",      ## arrange multiple plots
      "cowplot",        ## samesies
      "lubridate",      ## manipulate datetime data inside dplyr pipes
      "dplyr",          ## data manipulation and general awesomeness
      "plyr")           ## summary tables of ppn of categorical covariates
    
    
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
    # 
    # ll <- CRS("+init=epsg:4326") # WGS 84
    # utm <- CRS("+init=epsg:3742") # NAD83(HARN)/UTMzone12N 
    # aea <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
    # 
    # 
    
  #### Increase memory limit ####
    
    memory.limit(size = 7500000) 

    
    
################################################################################################## #  
  
    
    
### ### ### ### ### ### #
####   | RAW DATA |  ####
### ### ### ### ### ### #
 
    
    modDatRaw <- read.csv("modDat.csv")
    
################################################################################################## #  
  
    
    
### ### ###  ### ### ### ###
####   | FORMAT DATA |  ####
### ### ###  ### ### ### ###
    
    modDat <- modDatRaw %>%
      mutate(Used = as.factor(Used),
             datetime = ymd_hms(datetime),
             Date = ymd(Date))
    
    
    
    
    
################################################################################################## #  
  
    
    
### ### ###  ### ### ### ### ### ### ###
####   | COVARIATE RELATIONSHIPS |  ####
### ### ###  ### ### ### ### ### ### ###
    
    
    
    # #### check correlations between environmental covariates ####
    # 
    # dat.cor <- modDat %>%
    #   dplyr::select(can, elev, lc, rug, slope, swe, northness)
    # 
    # source("pairs-panels.R")    
    # pairs.panels(dat.cor)
    # 
    # save.image("correlationsEnvt.RData")
    # 
    # 
    
    
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### #
####   | UNIVARIATE DISTRIBUTIONS |  ####
### ### ### ### ### ### ### ### ### ### #
    
    
    #### kristin you set aspects = 0  separately
    # by just writing those lines in wolfspatial and running only those lines
    #### will need to rerun wolfspatialprep at some point
    
    
  #### ~ Environment ~ ####
    
    #### canopy cover ####
        
        a <- ggplot(modDat, aes(colour = Used))
        
        pC <- a + geom_density(aes(can, fill = Used), alpha = 0.2, size = 1)
       # pC
    
    #### landcover ####
        
        ppnLc <- ddply(modDat, .(Used), summarise,
                       prop = prop.table(table(lcClass)),
                       lcClass = names(table(lcClass)))
        pL <- ggplot(ppnLc, aes(lcClass, fill = Used)) +
          geom_bar(aes(y = prop), stat = "identity", position = "dodge")
      #  pL
        
    #### northness ####
        
        pN <- a + geom_density(aes(northness, fill = Used), alpha = 0.2, size = 1)
    #    pN
        
        
    #### elev ####
        
        pE <- a + geom_density(aes(elev, fill = Used), alpha = 0.2, size = 1)
     #   pE
        
        
    #### slope ####
        
        pS <- a + geom_density(aes(slope, fill = Used), alpha = 0.2, size = 1)
      #  pS
        
        
        
    #### ruggedness ####
        
        pR <- a + geom_density(aes(rug, fill = Used), alpha = 0.2, size = 1)
      #  pR   
        
    #### ruggedness ####
        
        pW <- a + geom_density(aes(swe, fill = Used), alpha = 0.2, size = 1)
      #  pW            

    #### all environmental together ####
        
        pA <- plot_grid(pC, pN, pE, pS, pR, pW, ncol = 2)
        plot_grid(pA, pL, nrow = 2, rel_heights = c(0.75, 0.25))
        
        
  #### ~ Human ~ ####  
        
    #### roads ####
        pRd <- a + geom_density(aes(distRd, fill = Used), alpha = 0.2, size = 1)
      #  pRd

    #### buildings ####
        pB <- a + geom_density(aes(distStruc, fill = Used), alpha = 0.2, size = 1)
      #  pB        
        
        
    #### feedgrounds ####    
        pF <- a + geom_density(aes(distFeed, fill = Used), alpha = 0.2, size = 1)
      #  pF         
 
    #### rec ####        
     
        ppnRec <- ddply(modDat[!is.na(modDat$recClass), ], .(Used), summarise,
                       prop = prop.table(table(recClass)),
                       recClass = names(table(recClass)))
        pRc <- ggplot(ppnRec, aes(recClass, fill = Used)) +
          geom_bar(aes(y = prop), stat = "identity", position = "dodge")
     #   pRc           
        
    #### all human together ####
        
        grid.arrange(pRd, pB, pF, pRc, ncol = 2)       
        
        
    #### resources ####
        
        # https://cran.r-project.org/web/packages/cowplot/vignettes/plot_grid.html
        # https://bioinfo.iric.ca/introduction-to-cowplot/
        # https://stackoverflow.com/questions/14818529/plot-histograms-over-factor-variables
        
 
        
                   
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### #
####   | PRELIM GLOBAL MODEL |  ####
### ### ### ### ### ### ### ### ### ### #
        
        
        ## i can't stand it i have to know right the fuck right now
        
        dat <- modDat %>%
          mutate(hr = hour(datetime),
                 # define daylight as 8a-5p
                 day = ifelse(hr >= 8 & hr <= 17, 1, 0))
        
        ## diff models for night and day first, bc interacting everything would suck
        
        library(lme4)
        
        modDay <- glmer(Used ~ 1 + slope + lcClass + can + elev + northness + swe +
                          I(slope*swe) + I(northness*swe) + I(can*swe) + (wolfYr | Pack), 
                        data = filter(dat, day == 1),
                        family = binomial(logit))
        
        modNight <- glmer(Used ~ 1 + slope + lcClass + can + elev + northness + swe +
                          I(slope*swe) + I(northness*swe) + I(can*swe) + (wolfYr | Pack), 
                        data = filter(dat, day == 0),
                        family = binomial(logit))        
        