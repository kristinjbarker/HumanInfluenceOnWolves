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


#### Set working directory, identify correct google database, and run correct cluster prep code ####

wd_kjb <- "C:\\Users\\Kristin\\Box Sync\\Documents\\HumanInfluenceOnWolves"
wd_greg <- "C:\\Users\\krist\\Documents\\HumanInfluenceOnWolves"

if (file.exists(wd_kjb)) { setwd(wd_kjb); datName <- "C:\\Users\\Kristin\\Box Sync\\Documents\\Data"
} else {
  setwd(wd_greg); datName <- "C:\\Users\\krist\\Documents\\Data"
}
rm(wd_kjb, wd_greg)


  

  #### Install and load any necessary packages you don't already have ####
  
    
    # list of packages needed
    packages <- c(
      #"raster",        ## raster stacking, extraction, etc
      #"maptools",      ## kmlPoints
      #"rgdal",         ## spatial/shapefile work 
      #"rgeos",         ## gDistance and other spatial work
      #"sp",            ## spatial work
      #"sf",            ## spatial work like the kids are doing it these days
      "lme4",           ## regression models
      "nlme",           ## better option for regression models?
      "AICcmodavg",     ## model comparison
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
    
    memory.limit(size = 97500000) 

    
    
################################################################################################## #  
  
    
    
### ### ### ### ### ### #
####   | RAW DATA |  ####
### ### ### ### ### ### #
 
    
    modDatRaw <- read.csv("modDat.csv")
    
################################################################################################## #  
  
    
    
### ### ###  ### ### ### ###
####   | FORMAT DATA |  ####
### ### ###  ### ### ### ###

    
    # standardize covariates; order factor levels    
    modDat <- modDatRaw %>%
      mutate(datetime = ymd_hms(datetime),
             Date = ymd(Date),
             slopeSt = (slope - mean(slope))/sd(slope),
             elevSt = (elev - mean(elev))/sd(elev),
             northnessSt = (northness - mean(northness))/sd(northness),
             snowSt = (snowCm - mean(snowCm))/sd(snowCm),
             canSt = (can - mean(can))/sd(can),
             # order landcover from most to least available
             lcClass = factor(lcClass, levels = c("Forest", "Shrub", "Herbaceous", "Riparian", "UrbanVeg", "NoVeg")))
    


    # temp code to fix the multiple instances of LGV - will be unnecessary after rerunning from dataPrepWolfYrs.R
    modDat$Pack <- ifelse(grepl("Gros Ventre|GV", modDat$Pack), "Lower Gros Ventre", as.character(modDat$Pack))    
    
    
    
    # split data for night and day (faster than filtering in model, i think) #
    datDay <- filter(modDat, daytime == "day")
    datNight <- filter(modDat, daytime == "night")    

    
################################################################################################## #  
  
    
    
### ### ###  ### ### ### ### ### ### ###
####   | COVARIATE RELATIONSHIPS |  ####
### ### ###  ### ### ### ### ### ### ###
    
    
    
    # #### check correlations between environmental covariates ####
    # 
    # dat.cor <- modDat %>%
    #   dplyr::select(can, elev, lc, rug, slope, snow, northness)
    # 
    # source("pairs-panels.R")    
    # pairs.panels(dat.cor)
    # 
    # save.image("correlationsEnvt.RData")
    # 
    # 

    # #### check correlations between human covariates ####
    # 
    # dat.cor.h <- modDat %>%
    #   dplyr::select(distRd, distFeed, distStruc, recClass)
    # 
    # source("pairs-panels.R")
    # pairs.panels(dat.cor.h)
    # 
    # save.image("correlationsHuman.RData")


    
    # #### check correlations between "top" environmental covariates and human covariates ####
    # 
    # dat.cor <- modDat %>%
    #   dplyr::select(can, elev, lc, rug, slope, snow, northness)
    # 
    # source("pairs-panels.R")    
    # pairs.panels(dat.cor)
    # 
    # save.image("correlationsEnvt.RData")
    # 
    # 
    
################################################################################################## #  

    
### ### ### ### ### ### ### ### ### ### ### ### ### ##
####   | UNIVARIATE DISTRIBUTIONS BY DAY/NIGHT |  ####
### ### ### ### ### ### ### ### ### ### ### ### ### ##
        
        
        ## use words instead of numbers for used/avail
        plotDat <- modDat %>%
          mutate(locType = ifelse(Used == 0, "Available", "Used"))     
        
        
        ## base
        b <- ggplot(plotDat, aes(colour = daytime, fill = daytime)) + facet_grid(~ locType)
        
       
        ## environmental covariates
            
            # canopy
            pc2 <- b + geom_density(aes(can), alpha = 0.2, size = 1)

            # aspect
            pn2 <- b + geom_density(aes(northness), alpha = 0.2, size = 1)

            # elev
            pe2 <- b + geom_density(aes(elev), alpha = 0.2, size = 1)

            # slope
            ps2 <- b + geom_density(aes(slope), alpha = 0.2, size = 1)

            # snow
            pw2 <- b + geom_density(aes(snowCm), alpha = 0.2, size = 1)
            
            # ruggedness
            pg2 <- b + geom_density(aes(rug), alpha = 0.2, size = 1)

            # landcover
            ppnLc2 <- ddply(plotDat, .(locType, daytime), summarise,
                            prop = prop.table(table(lcClass)),
                            lcClass = names(table(lcClass)))
            pl2 <- ggplot(ppnLc2, aes(lcClass, fill = daytime)) +
              geom_bar(aes(y = prop), stat = "identity", position = "dodge") +
              facet_grid(~ locType) 

            # all together
            pa2 <- plot_grid(pw2, pn2, pe2, ps2, pc2, pg2, ncol = 2)
            plot_grid(pa2, pl2, nrow = 2, rel_heights = c(0.75, 0.25))

        
        ## human covariates
        
            # rd
            pr2 <- b + geom_density(aes(distRd), alpha = 0.2, size = 1) 

            # bldg
            pb2 <- b + geom_density(aes(distStruc), alpha = 0.2, size = 1) 

            # feed
            pf2 <- b + geom_density(aes(distFeed), alpha = 0.2, size = 1) 

            # rec
            ppnRec <- ddply(plotDat[!is.na(plotDat$recClass), ], .(locType, daytime), summarise,
                           prop = prop.table(table(recClass)),
                           recClass = names(table(recClass)))
            prec2 <- ggplot(ppnRec, aes(recClass, fill = daytime)) +
              geom_bar(aes(y = prop), stat = "identity", position = "dodge") +
              facet_grid(~ locType)
            prec2     
            
            # all together
            grid.arrange(pr2, pb2, pf2, prec2, ncol = 2)
        
 
                   
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### ### ### ###
####   | DETERMINE RANDOM EFFECTS STRUCTURE |  ####
### ### ### ### ### ### ### ### ### ### ### ### ###


        #### use 'beyond optimal' model to test fit of diff random effects structures ####  
        
            
            ## day ##
            
                # specify models #
                    
                dNoRE <- glm(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                               I(slopeSt*snowSt) + I(northnessSt*snowSt), 
                             family = binomial(logit), data = datDay)
                
                dPackRE <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                                  I(slopeSt*snowSt) + I(northnessSt*snowSt) + (1|Pack)
                                family = binomial(logit), data = datDay,
                                nAGQ = 0, control = glmerControl(optimizer = "nloptwrap")) # to speed processing
                
                dWolfRE <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                                   I(slopeSt*snowSt) + I(northnessSt*snowSt) + (1|wolfYr), 
                                 family = binomial(logit), data = datDay,
                                 nAGQ = 0, control = glmerControl(optimizer = "nloptwrap"))
                
                dNestRE <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                                   I(slopeSt*snowSt) + I(northnessSt*snowSt) + (1|Pack/wolfYr), 
                                 family = binomial(logit), data = datDay,
                                 nAGQ = 0, control = glmerControl(optimizer = "nloptwrap"))      
            
            # compete models #
                    
                AIC(dNoRE, dPackRE, dWolfRE, dNestRE) # model without random effects is least-supported
                candMods <- list()
                candMods[[1]] <- dPackRE
                candMods[[2]] <- dWolfRE
                candMods[[3]] <- dNestRE
                aictab(cand.set = candMods, modnames = c("dPackRE", "dWolfRE", "dNestRE"))
                # nested ftw
                reDay <- dNestRE
               
                
                 
            ## night ##
            
                nNoRE <- glm(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                               I(slopeSt*snowSt) + I(northnessSt*snowSt), 
                             family = binomial(logit), data = datNight)
                
                nPackRE <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                                   I(slopeSt*snowSt) + I(northnessSt*snowSt) + (1|Pack), 
                                 family = binomial(logit), data = datNight,
                                 nAGQ = 0, control = glmerControl(optimizer = "nloptwrap")) # to speed processing
                
                nWolfRE <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                                   I(slopeSt*snowSt) + I(northnessSt*snowSt) + (1|wolfYr), 
                                 family = binomial(logit), data = datNight,
                                 nAGQ = 0, control = glmerControl(optimizer = "nloptwrap"))
                
                nNestRE <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                                   I(slopeSt*snowSt) + I(northnessSt*snowSt) + (1|Pack/wolfYr), 
                                 family = binomial(logit), data = datNight,
                                 nAGQ = 0, control = glmerControl(optimizer = "nloptwrap"))                 
    
                # compete models #
                AIC(nNoRE, nPackRE, nWolfRE, nNestRE) # model without random effects is least-supported
                candMods <- list()
                candMods[[1]] <- nPackRE
                candMods[[2]] <- nWolfRE
                candMods[[3]] <- nNestRE
                aictab(cand.set = candMods, modnames = c("nPackRE", "nWolfRE", "nNestRE"))
                # nested ftw
                reNight <- nNestRE
                
                
        #### attempts to check model fits ####
                
                # code from bolker github
                plot(reDay, main = "day")
                plot(reNight, main = "night")
                # well these look fucking awful (i think...)
                
                # try qq plots
                qqnorm(resid(reDay, type = "pearson", scaled = "true")) # fuuuuuuck
                qqnorm(resid(reNight, type = "pearson", scaled = "true")) # literally the worst i've ever seen

                
                ## code from zuur book, don't bother
                dResid <- resid(reDay, type = "pearson", scaled = "true")
                nResid <- resid(reNight, type = "pearson", scaled = "true")
                coplot(dResid ~ slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                         slopeSt|snowSt + northnessSt|snowSt,
                       data = datDay, ylab="Normalised residuals") 
                coplot(nResid ~ slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                         slopeSt|snowSt + northnessSt|snowSt,
                       data = datDay, ylab="Normalised residuals")

              


              
              

                   
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### ### ### ###
####   | DETERMINE ENVIRONMENTAL COVARIATES |  ####
### ### ### ### ### ### ### ### ### ### ### ### ###        
        
        
        # remember to use ML not REML for this
                
                
                
                
                
                
                
                
                
        
        
################################################################################################## #  
              
              
              
        save.image("prelimGlobalModels.RData")
   
        
        #### extracting and visualizing global model output ####
        
        summary(globDay)
        summary(globNight)
        
        
        
        
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### ###
####   | PRELIM MODEL RESULTS PLOTS |  ####
### ### ### ### ### ### ### ### ### ### ###
        
        
        # make dataframe of odds ratios and confidence intervals for each model
        # (you'd be cooler if you did this in a function)
        # also don't forget to delete Wald for final product (it's fast but inaccurate)
        
        dDay <- round(exp(data.frame(
          OR = fixef(reDay),
          ciLow = confint(reDay, parm = "beta_", method = "Wald")[,1],
          ciHigh = confint(reDay, parm = "beta_", method = "Wald")[,2])), 3)
        dDay$Covariate = rownames(dDay)
        dDay$timing = "day"
        dNight <- round(exp(data.frame(
          OR = fixef(reNight),
          ciLow = confint(reNight, parm = "beta_", method = "Wald")[,1],
          ciHigh = confint(reNight, parm = "beta_", method = "Wald")[,2])), 3)
        dNight$Covariate = rownames(dNight)
        dNight$timing = "night"
        dBoth <- rbind(dDay, dNight)

        # remove Pack and Intercept
        dBothSub <- filter(dBoth, !grepl("Pack|Intercept", Covariate))
        
        # order covariates more intuitively across x-axis
        dBothSub2 <- dBothSub %>%
          filter(!grepl("lc", Covariate)) %>%
          mutate(Covariate = factor(Covariate, 
            levels=c("elevSt", "northnessSt", "slopeSt", "snowSt", 
                     "I(slopeSt * snowSt)", "I(northnessSt * snowSt)")))
        
        # plot OR +- 95%CI colored by day/night - continuous covariates
        ggplot(dBothSub2, aes(x = Covariate, y = OR, colour = timing)) +
          geom_errorbar(aes(ymin = ciLow, ymax = ciHigh), width = 0.1) +
          geom_point() +
          geom_hline(aes(yintercept=1))
        
        
        # plot OR +- 95%CI colored by day/night - categorical covariate
        ggplot(filter(dBothSub, grepl("lc", Covariate)), aes(x = Covariate, y = OR, colour = timing)) +
          geom_errorbar(aes(ymin = ciLow, ymax = ciHigh), width = 0.1) +
          geom_point() +
          geom_hline(aes(yintercept=1))        
        
        
        


        