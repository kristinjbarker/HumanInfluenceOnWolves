
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

    # split data for night and day (faster than filtering in model, i think) #
    datDay <- filter(modDat, daytime == "day")
    datNight <- filter(modDat, daytime == "night") 
    datCrep <- filter(modDat, daytime == "crep")



################################################################################################## #  
                
    
    
### ### ### ### ### ### ### ### ### ### ### ### ##
####   | DETERMINE FIXED EFFECTS STRUCTURE |  ####
### ### ### ### ### ### ### ### ### ### ### ### ##  
    
    
    
  #### Determine whether global model should consist of environment or prey availability  ####
    
    
      ## Environment-only model ##
        
        globEnvt <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                         + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                         + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt + snowSt:elevSt
                         + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                         + (1|Pack), family = binomial(logit), data = modDat,
                         control = glmerControl(optimizer = "bobyqa", 
                                                optCtrl=list(maxfun=2e4),
                                                calc.derivs = FALSE)) 
 
        
      ## Prey-only models ##
        
        globPrey <- glmer(Used ~ 1
                         + distPreySt + I(distPreySt*distPreySt) + snowSt:distPreySt + snowSt:I(distPreySt*distPreySt)
                         + (1|Pack), family = binomial(logit), data = modDat,
                         control = glmerControl(optimizer = "bobyqa", 
                                                optCtrl=list(maxfun=2e4),
                                                calc.derivs = FALSE)) 

        
        
      #### Compare environment and prey to see which best explains overall distribution ####
        
        aictab(c(globEnvt, globPrey), modnames = c("envt", "prey"))
        invisible(plot(roc(factor(ifelse(modDat$Used == 1, 1, 0)), fitted(globEnvt)), print.thres = c(.1, .5), col = "red", print.auc = T))
        invisible(plot(roc(factor(ifelse(modDat$Used == 1, 1, 0)), fitted(globPrey)), print.thres = c(.1, .5), col = "red", print.auc = T))
        # definitely environment only (deltaAICc = 833.97; AUC = 0.658 vs 0.528)
        
  
        
        
  #### Define global models split by time of day  ####            
            
        
      #### Specify global models ####
        
        globDay <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                         + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                         + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt + snowSt:elevSt
                         + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                         + (1|Pack), family = binomial(logit), data = datDay,
                         control = glmerControl(optimizer = "bobyqa", 
                                                optCtrl=list(maxfun=2e4),
                                                calc.derivs = FALSE)) 
        
        globNight <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                           + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                           + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt + snowSt:elevSt
                           + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                           + (1|Pack), family = binomial(logit), data = datNight,
                           control = glmerControl(optimizer = "bobyqa", 
                                                  optCtrl=list(maxfun=2e4),
                                                  calc.derivs = FALSE)) 

        
        globCrep <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                   + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                   + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt + snowSt:elevSt
                   + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                   + (1|Pack), family = binomial(logit), data = datCrep,
                   control = glmerControl(optimizer = "bobyqa", 
                                          optCtrl=list(maxfun=2e4),
                                          calc.derivs = FALSE)) 
        

        
      #### Evaluate relative covariate importance ####
        
        
       #### --day ####

          summary(globDay)
           
          # remove snow:northness2
          day2 <- update(globDay, . ~ . - snowSt:I(northnessSt*northnessSt))
          aictab(c(globDay, day2), modnames = c("global", "m2")) # day2 ftw 
          summary(day2)
          
          # also remove snow:slope and snow:slope2
          day3 <- update(day2, . ~ . - snowSt:slopeSt - snowSt:I(slopeSt * slopeSt))
          aictab(c(day3, day2), modnames = c("m3", "m2")) # tie
          summary(day3)
          
          # keep the snow:slopes but remove lc instead
          day4 <- update(day2, . ~ . - lcClass)
          aictab(c(day4, day3, day2), modnames = c("m4", "m3", "m2")) # day4 and day3 similar 
          summary(day4)
          
          # remove both snow:slopes and landcover
          day5 <- update(day3, . ~ . - lcClass)
          aictab(c(day4, day2, day5, day3), modnames = c("m4", "m2", "m5", "m3")) # ugh so similar
          summary(day5)
          

          # store results of all models
          aicDay <- data.frame(aictab(cand.set = c(globDay, day2, day3, day4, day5), 
                            modnames = c("Global", "m2", "m3", "m4", "m5")))
          aicDay <- aicDay[order(aicDay$Delta_AICc), ]
          write.csv(aicDay, file = "./ResultsAIC/aic-envt-Day.csv", row.names = FALSE)      
          
          
          #### decide between day2, day3, day4, day5 ####
          
            # residual plots
            par(mfrow = c(2,2))
            binnedplot(fitted(day2), residuals(day2, type = "response"), main = "day2")
            binnedplot(fitted(day3), residuals(day3, type = "response"), main = "day3")
            binnedplot(fitted(day4), residuals(day4, type = "response"), main = "day4")
            binnedplot(fitted(day5), residuals(day5, type = "response"), main = "day5")
            par(mfrow = c(1,1))
            
            # day 5 has no outliers so i'm rolling with that


            
            
        #### --night ####  
          
          summary(globNight)
        
          # remove snow:slope and snow:slope2 
          night2 <- update(globNight, . ~ . - slopeSt:snowSt- snowSt:I(slopeSt * slopeSt))
          aictab(c(globNight, night2), modnames = c("global", "m2")) # night2 ftw
          summary(night2)
          
          
          # also remove snow:northness and snow:northness2
          night3 <- update(night2, . ~ . - northnessSt:snowSt- snowSt:I(northnessSt * northnessSt))
          aictab(c(night3, night2), modnames = c("m3", "m2")) # night3 ftw
          summary(night3)       
          
          # also remove landcover
          night4 <- update(night3, . ~ . - lcClass)
          aictab(c(night3, night4), modnames = c("m3", "m4"))  # night3 ftw
          
          # leave landcover. remove northness2?
          night5 <- update(night3, . ~ . - I(northnessSt * northnessSt))
          aictab(c(night3, night5), modnames = c("m3", "m5")) # night3 ftw
          

          
          # store results of all models
          aicNight <- data.frame(aictab(cand.set = c(globNight, night2, night3, night4, night5), 
                            modnames = c("Global", "m2", "m3", "m4", "m5")))
          aicNight <- aicNight[order(aicNight$Delta_AICc), ]
          write.csv(aicNight, file = "./ResultsAIC/aic-envt-Night.csv", row.names = FALSE) 
          
          
          #### decide between night2 and night3 #### 

            # residual plots
            par(mfrow = c(2,1))
            binnedplot(fitted(night2), residuals(night2, type = "response"), main = "night2")
            binnedplot(fitted(night3), residuals(night3, type = "response"), main = "night3")
            par(mfrow = c(1,1))
            
            # night2 fewer outliers and less patterning
          

            
        #### --crepuscular ####
        
          summary(globCrep)
        
          # remove slope:snow and snow:slope2
          c2 <- update(globCrep, . ~ . - slopeSt:snowSt - snowSt:I(slopeSt * slopeSt))
          aictab(c(globCrep, c2), modnames = c("global", "c2")) #c2 ftw
          summary(c2)    
          
          # also remove snow:northness and snow:northness2
          c3 <- update(c2, . ~ . - northnessSt:snowSt  - snowSt:I(northnessSt * northnessSt))
          aictab(c(c2, c3), modnames = c("c2", "c3")) # tie
          summary(c2)
        
          # keep snow:northness; just remove snow:northness2
          c4 <- update(c2, . ~ . - snowSt:I(northnessSt * northnessSt))
          aictab(c(c2, c4), modnames = c("c2", "c4")) # similar
          summary(c2)   
          summary(c4)
          
          # store results of all models
          aicCrep <- data.frame(aictab(cand.set = c(globCrep, c2, c3, c4), 
                            modnames = c("Global", "c2", "c3", "c4")))
          aicCrep <- aicCrep[order(aicCrep$Delta_AICc), ]
          write.csv(aicCrep, file = "./ResultsAIC/aic-envt-Crep.csv", row.names = FALSE) 
          
          
          ## decide between c2, c3, and c4 ##
               
            # residual plots
            par(mfrow = c(3,1))
            binnedplot(fitted(c2), residuals(c2, type = "response"), main = "crep2")
            binnedplot(fitted(c3), residuals(c3, type = "response"), main = "crep3")
            binnedplot(fitted(c4), residuals(c4, type = "response"), main = "crep4")
            par(mfrow = c(1,1))
            
            # crep3 has fewest outliers
            
 
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### #
####   | ASSESS MODEL FIT |  ####
### ### ### ### ### ### ### ### #    

          

      #### Specify top environmental models ####
          
          envtDay <- day5
          envtNight <- night2
          envtCrep <- c3
          

        ## ROC AUC ##
          
          # 0.657 day | 0.678 night | 0.659 crep
          invisible(plot(roc(factor(ifelse(datDay$Used == 1, 1, 0)), fitted(envtDay)), print.thres = c(.1, .5), col = "red", print.auc = T)) 
          invisible(plot(roc(factor(ifelse(datNight$Used == 1, 1, 0)), fitted(envtNight)), print.thres = c(.1, .5), col = "red", print.auc = T))  
          invisible(plot(roc(factor(ifelse(datCrep$Used == 1, 1, 0)), fitted(envtCrep)), print.thres = c(.1, .5), col = "red", print.auc = T)) 
          
          
        ## predictive accuracy @ >50% ##  
          
          # 61.71% day | 63.33% night | 60.68% crep
          confusionMatrix(factor(as.character(ifelse(fitted(envtDay) > 0.5, "Yes", "No"))), factor(ifelse(datDay$Used == 1, "Yes", "No")), positive = "Yes")
          confusionMatrix(factor(as.character(ifelse(fitted(envtNight) > 0.5, "Yes", "No"))), factor(ifelse(datNight$Used == 1, "Yes", "No")), positive = "Yes")         
          confusionMatrix(factor(as.character(ifelse(fitted(envtCrep) > 0.5, "Yes", "No"))), factor(ifelse(datCrep$Used == 1, "Yes", "No")), positive = "Yes")         

          
    

        
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
        
        
        # slope (day and night)
        pSlope <- ggplot(dn, aes(x = slope, y = Used, colour = model)) +
          geom_point(col = "black") +
          stat_smooth(aes(x = slope, y = prWolf, linetype = snowLev), method = "lm", formula = y ~ poly(x, 2)) +
          coord_cartesian(ylim = c(0, 1)) +
          labs(title = "Slope")
        
        

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

        
        
        # plot OR +- 95%CI colored by day/night - categorical covariate
        pLc <- ggplot(filter(dBothSub, grepl("lc", Covariate)), aes(x = Covariate, y = OR, colour = timing)) +
          geom_errorbar(aes(ymin = ciLow, ymax = ciHigh), width = 0.1) +
          geom_point() +
          geom_hline(aes(yintercept=1)) +
          labs(title = "Landcover type", x = "(relative to forest)") +
          scale_x_discrete(labels=c("Herbaceous","NoVeg","Riparian","Shrub"))
        

        
        #### all together now ####
        
        grid.arrange(pElev, pNorth, pSlopeDay,
                     pCan, pLc, 
                     nrow = 2)
        
        
        
################################################################################################## #  
  
save.image(paste0("environmentalModels-", today(), ".RData"))        
        

################################################################################################## #  
  
