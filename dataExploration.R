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
      "arm",            ## binned residual plots
      "aods3",          ## quick gof tests
      "pROC",           ## roc plots
      "plotROC",        ## roc plots
      "caret",          ## confusion matrices (model predictive accuracy)
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

    
    # format covariates    
    modDat <- modDatRaw %>%
      # handle datetimes and dates
      mutate(datetime = ymd_hms(datetime),
             Date = ymd(Date),
             # standardize continuous covariates
             slopeSt = (slope - mean(slope))/sd(slope),
             elevSt = (elev - mean(elev))/sd(elev),
             northnessSt = (northness - mean(northness))/sd(northness),
             snowSt = (snowCm - mean(snowCm))/sd(snowCm),
             canSt = (can - mean(can))/sd(can),
             # order landcover from most to least available
             lcClass = factor(lcClass, levels = c("Forest", "Shrub", "Herbaceous", "Riparian", "NoVeg")),
             # order rec from most to least regulated, relative to private land as baseline
             recClass = factor(recClass, levels = c("noRec", "noOT", "nomotoOT", "allOT")))

    
    # split data for night and day (faster than filtering in model, i think) #
    datDay <- filter(modDat, daytime == "day")
    datNight <- filter(modDat, daytime == "night")    

    
################################################################################################## #  
  
    
    
### ### ###  ### ### ### ### ### ### ###
####   | COVARIATE RELATIONSHIPS |  ####
### ### ###  ### ### ### ### ### ### ###
    
    
    
    #### check correlations between environmental covariates ####

    # dat.cor <- modDat %>%
    #   dplyr::select(can, elev, lc, rug, slope, snowCm, northness)
    # 
    # source("pairs-panels.R")
    # pairs.panels(dat.cor)


    # save.image("correlationsEnvt.RData")



    # #### check correlations between human covariates ####
    # 
    # dat.cor.h <- modDat %>%
    #   dplyr::select(distRd, distFeed, distStruc, recClass)
    # 
    # source("pairs-panels.R")
    # pairs.panels(dat.cor.h)
    # 
    # save.image("correlations.RData")


    
    #### check correlations between all environmental and human covariates ####
    
    dat.cor <- modDat %>%
      mutate(distActiveFeed = distFeed*activeFeed) %>%
      dplyr::select(can, elev, slope, snowCm, northness,
                    distRd, distStruc, distFeed, distActiveFeed)
    corDat <- cor(dat.cor)
    
    # correlations >= 0.70
    cor7 <- corDat
    cor7[cor7 < 0.70] <- NA
    cor7
    # just distFeed and distActiveFeed
    
    # correlations >= 0.60
    cor6 <- corDat
    cor6[cor6 < 0.60] <- NA
    cor6
    # same
  
    # correlations >= 0.50
    cor5 <- corDat
    cor5[cor5 < 0.50] <- NA
    cor5
    # distStruc & distRd 0.504, i think that's alright
    
    # take-home: no covariates so correlated that they can't be included in the same model
      


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

        
      #### Specify models ####
        
        # no random effect  
        noreDay <- glm(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                       + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                       + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt
                       + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt), 
                       family = binomial(logit),
                       data = datDay)
        noreNight <- glm(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                         + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                         + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt
                         + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt), 
                         family = binomial(logit),
                         data = datNight)            
        
        # wolf-year only
        wDay <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                       + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                       + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt
                       + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                       + (1|wolfYr), 
                        family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                       data = datDay)
        wNight <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                         + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                         + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt
                         + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                         + (1|wolfYr), 
                         family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                         data = datNight)        
        
        # wolf-year nested in pack
        wpDay <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                       + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                       + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt
                       + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                       + (1|Pack/wolfYr), 
                        family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                       data = datDay) 
        wpNight <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                         + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                         + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt
                         + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                         + (1|Pack/wolfYr), 
                         family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                         data = datNight)          
        

      #### Compete models ####
        
        AIC(noreDay, wDay, wpDay) # day: wolf nested in pack
        AIC(noreNight, wNight, wpNight) # night: wolf nested in pack
          
          
                   
################################################################################################## #  
                
    
    
### ### ### ### ### ### ### ### ### ### ### ### ##
####   | DETERMINE FIXED EFFECTS STRUCTURE |  ####
### ### ### ### ### ### ### ### ### ### ### ### ##  
            
            
      #### Specify global models ####
        
        globDay <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                         + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                         + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt + snowSt:elevSt
                         + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                         + (1|Pack/wolfYr), 
                          family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                         data = datDay) 
        globNight <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                           + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                           + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt + snowSt:elevSt
                           + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                           + (1|Pack/wolfYr), 
                           family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                           data = datNight) 
            
        
      #### Evaluate relative covariate importance ####
        

        
        ## day ##

          summary(globDay)
          roc(factor(ifelse(datDay$Used == 1, 1, 0)), fitted(globDay), plot = F, print.auc = T) # 0.6932
           
          # remove snow:northness2
          day2 <- update(globDay, . ~ . - snowSt:I(northnessSt*northnessSt))
          AIC(day2, globDay) # globDay ftw
          roc(factor(ifelse(datDay$Used == 1, 1, 0)), fitted(day2), plot = F, print.auc = T) # 0.6931
          summary(day2)


          # rolling with globDay  
          roc(factor(ifelse(datDay$Used == 1, 1, 0)), fitted(globDay), plot = F, print.auc = T) # 0.6932
                  

        
        ## night ##  
          
          summary(globNight)
          roc(factor(ifelse(datNight$Used == 1, 1, 0)), fitted(globNight), plot = F, print.auc = T) # 0.7171
        
          # remove snow:northness & snow:northness^2
          night2 <- update(globNight, . ~ . - snowSt:northnessSt - snowSt:I(northnessSt*northnessSt))
          summary(night2)
          BIC(globNight, night2) # night2 ftw
          AIC(globNight, night2) # lookslike tie but really night2
          roc(factor(ifelse(datNight$Used == 1, 1, 0)), fitted(night2), plot = F, print.auc = T) # 0.7171

          
          # also remove snow:slope^2
          night3 <- update(night2, . ~ . - snowSt:I(slopeSt * slopeSt))
          BIC(globNight, night2, night3) # night3 ftw
          AIC(globNight, night2, night3) # night3 loser, night2 ftw
          roc(factor(ifelse(datNight$Used == 1, 1, 0)), fitted(night3), plot = F, print.auc = T) # 0.7171
          summary(night3)  
          
      
        
          # rolling with night2 
          roc(factor(ifelse(datNight$Used == 1, 1, 0)), fitted(night2), plot = F, print.auc = T) # 0.7171
              
                
          
                
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### #
####   | ASSESS MODEL FIT |  ####
### ### ### ### ### ### ### ### #    
          
        # envtDay <- globDay
        envtDay <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                         + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                         + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt + snowSt:elevSt
                         + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                         + (1|Pack/wolfYr), 
                          family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                         data = datDay) 
          summary(envtDay)
        
          # envtNight <- night2
          envtNight <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                           + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                           + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt + snowSt:elevSt 
                           + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                           + (1|Pack/wolfYr), 
                           family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                           data = datNight)    
          summary(envtNight)
        
        
        ## binned residual plots ##
        
          # day - whole model
          binnedplot(fitted(envtDay), residuals(envtDay, type = "response"), main = "Day - full model")
          
          # day - individual covariates
          binnedplot(datDay$canSt, residuals(envtDay, type = "response"), main = "Day - canopy")
          binnedplot(datDay$slopeSt, residuals(envtDay, type = "response"), main = "Day - slope")
          binnedplot(datDay$elevSt, residuals(envtDay, type = "response"), main = "Day - elevation")
          binnedplot(datDay$northnessSt, residuals(envtDay, type = "response"), main = "Day - northness")
          binnedplot(datDay$snowSt, residuals(envtDay, type = "response"), main = "Day - snow")   
          
          # night - whole model
          binnedplot(fitted(envtNight), residuals(envtNight, type = "response"), main = "Night - full model")
          
           
          # night - individual covariates
          binnedplot(datNight$canSt, residuals(envtNight, type = "response"), main = "Night - canopy")
          binnedplot(datNight$slopeSt, residuals(envtNight, type = "response"), main = "Night - slope")
          binnedplot(datNight$elevSt, residuals(envtNight, type = "response"), main = "Night - elevation")
          binnedplot(datNight$northnessSt, residuals(envtNight, type = "response"), main = "Night - northness")
          binnedplot(datNight$snowSt, residuals(envtNight, type = "response"), main = "Night - snow")           
          

        ## area under the roc curve ##
          
          # day
          invisible(plot(roc(factor(ifelse(datDay$Used == 1, 1, 0)), fitted(envtDay)), 
                         print.thres = c(.1, .5), col = "red", print.auc = T)) # auc = 0.693
          
          # night
          invisible(plot(roc(factor(ifelse(datNight$Used == 1, 1, 0)), fitted(envtNight)), 
                         print.thres = c(.1, .5), col = "red", print.auc = T)) # auc = 0.717          
          
          
        ## predictive accuracy @ >50% ##  
          
          # day
          confusionMatrix(factor(as.character(ifelse(fitted(envtDay) > 0.5, "Yes", "No"))), 
                          factor(ifelse(datDay$Used == 1, "Yes", "No")), positive = "Yes") # 64% 
          
          # night
          confusionMatrix(factor(as.character(ifelse(fitted(envtNight) > 0.5, "Yes", "No"))), 
                          factor(ifelse(datNight$Used == 1, "Yes", "No")), positive = "Yes") # 65%           
          


        
################################################################################################## #  
  
    
    
### ### ### ### ###  ### ### ### ### ### 
####   | VISUALIZE MODEL RESULTS |  ####
### ### ### ### ###  ### ### ### ### ### 
        
        
      #### create dataframe of things to plot ####
        
        d <- datDay %>%
          mutate(
            prWolf = fitted(envtDay),
            model = "Day")

        
        n <- datNight %>%
          mutate(
            prWolf = fitted(envtNight),
            model = "Night")
        
        dn <- bind_rows(d, n) 
        
    
        
        
      #### linear terms ####    
        
        # snow (non-linear bc it's more informative this way)
        pSnow <- ggplot(dn, aes(x = snowCm, y = Used, colour = model)) +
          geom_point(colour = "black") +
          stat_smooth(aes(x = snowCm, y = prWolf)) +
          coord_cartesian(ylim = c(0, 1)) +
          labs(y = "Pr(Use)", title = "Snow Depth (cm)")    
 
        
      #### linear interactions ####        
        
        # considering snow depth >200cm "high" based on smoothed plot
        dn$snowLev = ifelse(dn$snowCm > 200, "Deep snow", "Shallower snow")
        
        # canopy
        pCan <- ggplot(dn, aes(x = can, y = Used, colour = model)) +
          geom_point(colour = "black") +
          stat_smooth(aes(x = can, y = prWolf, linetype = snowLev), method = "lm", formula = y ~ x) +
          coord_cartesian(ylim = c(0, 1)) +
          labs(y = "Pr(Use)", title = "Canopy cover (%)")          
        
        # slope (night only - in day the interaction doesn't affect the quadratic)
        pSlopeNight <- ggplot(filter(dn, model == "Night"), aes(x = slope, y = Used)) +
          geom_point(col = "black") +
          stat_smooth(aes(x = slope, y = prWolf, linetype = snowLev), method = "lm", formula = y ~ x) +
          coord_cartesian(ylim = c(0, 1)) +
          labs(title = "Slope - Nighttime")
          # not terribly compelling
        
        
      #### quadratic interactions ####  
        
        # elevation (day and night)
        pElev <- ggplot(dn, aes(x = elev, y = Used, colour = model)) +
          geom_point(col = "black") +
          stat_smooth(aes(x = elev, y = prWolf, linetype = snowLev), method = "lm", formula = y ~ poly(x, 2)) +
          coord_cartesian(ylim = c(0, 1)) +
          labs(title = "Elevation")
        
        # northness (day and night)
        pNorth <- ggplot(dn, aes(x = northness, y = Used, colour = model)) +
          geom_point(col = "black") +
          stat_smooth(aes(x = northness, y = prWolf, linetype = snowLev), method = "lm", formula = y ~ poly(x, 2)) +
          coord_cartesian(ylim = c(0, 1)) +
          labs(title = "Aspect")        
        
        
        # slope (day only - but consider combining with the linear night plot)
        pSlopeDay <- ggplot(filter(dn, model == "Day"), aes(x = slope, y = Used)) +
          geom_point(col = "black") +
          stat_smooth(aes(x = slope, y = prWolf, linetype = snowLev), method = "lm", formula = y ~ poly(x, 2)) +
          coord_cartesian(ylim = c(0, 1)) +
          labs(title = "Slope - Daytime")
        
        

################################################################################################## #  


    
### ### ### ### ### ### ### ### #
####   | ODDS RATIO PLOTS |  ####
### ### ### ### ### ### ### ### #
        
        
        # make dataframe of odds ratios and confidence intervals for each model
        # (you'd be cooler if you did this in a function)
        # also don't forget to delete Wald for final product (it's fast but inaccurate)
        dDay <- round(exp(data.frame(
          OR = fixef(envtDay),
          ciLow = confint(envtDay, parm = "beta_", method = "Wald")[,1],
          ciHigh = confint(envtDay, parm = "beta_", method = "Wald")[,2])), 3)
        dDay$Covariate = rownames(dDay)
        dDay$timing = "day"
        dNight <- round(exp(data.frame(
          OR = fixef(envtNight),
          ciLow = confint(envtNight, parm = "beta_", method = "Wald")[,1],
          ciHigh = confint(envtNight, parm = "beta_", method = "Wald")[,2])), 3)
        dNight$Covariate = rownames(dNight)
        dNight$timing = "night"
        dBoth <- rbind(dDay, dNight)

        # remove Pack and Intercept
        dBothSub <- filter(dBoth, !grepl("Pack|Intercept|\\*|\\:", Covariate))
        
        # # order covariates more intuitively across x-axis
        # dBothSub2 <- dBothSub %>%
        #   filter(!grepl("lc", Covariate)) %>%
        #   mutate(Covariate = factor(Covariate, 
        #     levels=c("elevSt", "canSt", "northnessSt", "slopeSt", "snowSt", 
        #              "I(slopeSt * snowSt)", "I(northnessSt * snowSt)")))
        # 
        # # plot OR +- 95%CI colored by day/night - continuous covariates
        # pORs <- ggplot(dBothSub2, aes(x = Covariate, y = OR, colour = timing)) +
        #   geom_errorbar(aes(ymin = ciLow, ymax = ciHigh), width = 0.1) +
        #   geom_point() +
        #   geom_hline(aes(yintercept=1)) +
        #   labs(title = "Univariate")
        
        
        # plot OR +- 95%CI colored by day/night - categorical covariate
        pLc <- ggplot(filter(dBothSub, grepl("lc", Covariate)), aes(x = Covariate, y = OR, colour = timing)) +
          geom_errorbar(aes(ymin = ciLow, ymax = ciHigh), width = 0.1) +
          geom_point() +
          geom_hline(aes(yintercept=1)) +
          labs(title = "Landcover type", x = "(relative to forest)") +
          scale_x_discrete(labels=c("Herbaceous","NoVeg","Riparian","Shrub"))
        

        
        #### all together now ####
        
        grid.arrange(pElev, pNorth, pSlopeDay,
                     pCan, pLc, pSlopeNight, 
                     ncol = 3)
        
        
        
################################################################################################## #  
  
save.image("environmentalModels.RData")        
        

################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### #
####   | HUMAN MODELS |  ####
### ### ### ### ### ### ### ### #    

# standardize covariates
                  