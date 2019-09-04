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

    
    modDat <- modDatRaw %>% mutate(
      datetime = ymd_hms(datetime, tz = "America/Denver"),
      # handle datetimes and dates of course
      Date = ymd(Date),
      # year as numeric
      Year = as.numeric(Year),
      # order landcover from most to least available
      lcClass = factor(lcClass, levels = c("Forest", "Shrub", "Herbaceous", "Riparian", "NoVeg")),
      # use open recreation as baseline; reorder for more intuitive plot interpretation
      recClass = factor(recClass, levels = c("allOT", "nomotoOT", "noOT", "noRec")))


    
################################################################################################## #  
  
    
    
### ### ###  ### ### ### ### ### ### ###
####   | COVARIATE RELATIONSHIPS |  ####
### ### ###  ### ### ### ### ### ### ###
    

    
    #### check correlations between all environmental and human covariates ####

    dat.cor <- modDat %>%
      dplyr::select(can, elev, slope, rug, snowCm, northness,
                    distRd, distStruc, distFeed, distFeedActive, hunt, prevHunt, tSinceHunt, tContHunt)
    corDat <- cor(dat.cor)
    source("pairs-panels.R"); pairs.panels(dat.cor)


    # correlations >= 0.70
    cor7 <- corDat
    cor7[cor7 < 0.70] <- NA
    cor7
    # just rug+slope, most hunts with each other

    # correlations >= 0.60
    cor6 <- corDat
    cor6[cor6 < 0.60] <- NA
    cor6
    # same

    # correlations >= 0.50
    cor5 <- corDat
    cor5[cor5 < 0.50] <- NA
    cor5
    # now also distStruc+distRd, but only 0.53


    # take-home: choose slope over ruggedness
      


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

        
      #### Specify 'beyond optimal' models (following Zuur bk Chap 5.7) ####
        
        # no random effect  
        nore <- glm(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                       + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                       + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt
                       + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                       + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2), 
                       family = binomial(logit), data = modDat)


        # wolf-year only
        w <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                       + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                       + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt
                       + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                       + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2)
                       + (1|wolfYr), family = binomial(logit), data = modDat,
                       control = glmerControl(optimizer = "bobyqa", 
                                              optCtrl=list(maxfun=2e4),
                                              calc.derivs = FALSE))

        
        # pack only
        p <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                       + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                       + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt
                       + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                       + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2)
                       + (1|Pack), family = binomial(logit), data = modDat,
                       control = glmerControl(optimizer = "bobyqa", 
                                              optCtrl=list(maxfun=2e4),
                                              calc.derivs = FALSE))
 
        
        
        # wolf-year nested in pack
        wNp <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                       + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                       + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt
                       + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                       + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt
                       + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2)
                       + (1|Pack/wolfYr), family = binomial(logit), data = modDat,
                       control = glmerControl(optimizer = "bobyqa", 
                                              optCtrl=list(maxfun=2e4),
                                              calc.derivs = FALSE))
        


      #### Compete models ####
          
        
        # day - calculate and export
        aicD <- data.frame(aictab(cand.set = c(w, p, wNp), 
                                    modnames = c("Wolf", "Pack", "WolfInPack")))
        aicD <- aicD[order(aicD$Delta_AICc), ] # pack only ftw
        # quick slightly-biased comparison to fixed effects only model
        AIC(nore, w, p, wNp) # pack only ftw
        write.csv(aicD, file = "aic-RE-structure.csv", row.names = FALSE)         

                   
