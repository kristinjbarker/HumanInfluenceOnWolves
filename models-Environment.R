
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
      datetime = ymd_hms(datetime),
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



################################################################################################## #  
                
    
    
### ### ### ### ### ### ### ### ### ### ### ### ##
####   | DETERMINE FIXED EFFECTS STRUCTURE |  ####
### ### ### ### ### ### ### ### ### ### ### ### ##  
            
            
      #### Specify global models ####
        
        globDay <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                         + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                         + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt + snowSt:elevSt
                         + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                         + (1|Pack), 
                          family = binomial(logit), data = datDay) 
        
        globNight <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                           + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                           + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt + snowSt:elevSt
                           + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                           + (1|Pack), 
                           family = binomial(logit), data = datNight) 
            
        
      #### Evaluate relative covariate importance ####
        

        
        #### --day ####

          summary(globDay)
           
          # remove snow:northness2
          day2 <- update(globDay, . ~ . - snowSt:I(northnessSt*northnessSt))
          AIC(day2, globDay) # day2 ftw 
          summary(day2)
          
          # also remove snow:northness
          day3 <- update(day2, . ~ . - snowSt:northnessSt)
          AIC(day3, day2, globDay) # day3 ftw 
          summary(day3)
          
          # also remove snow:slope
          day4 <- update(day3, . ~ . - slopeSt:snowSt)
          AIC(day4, day3) # day4 ftw
          summary(day4)
          
          # not removing main effect of snow bc it's impt in intrxns
          # not removing main effect of canopy bc impt interacted with snow
          # not removing main effect of elev bc impt in elev^2 and interacted w snow
          # not removing main effect of northness bc it's impt in northness^2
          
          # also remove landcover? (only 1 class is impt)
          day5 <- update(day4, . ~ . - lcClass)
          AIC(day5, day4) # day4 ftw 
          summary(day4)
          
          # also remove elev:snow? (less impt than any remaining covariates)
          day6 <- update(day4, . ~ . - elevSt:snowSt)     
          AIC(day4, day6) #day4 ftw
          
          # evaluate all along with LL to avoid incorrect conclusion
          aicDay <- data.frame(aictab(cand.set = c(globDay, day2, day3, day4), 
                            modnames = c("Global", "m2", "m3", "m4")))
          aicDay$LL <- c(logLik(globDay), logLik(day2), logLik(day3), logLik(day4))        
          aicDay <- aicDay[order(aicDay$Delta_AICc), ]
          write.csv(aicDay, file = "aic-envt-Day.csv", row.names = FALSE)                   
          
          # rolling with day4

          
          

        #### --night ####  
          
          summary(globNight)
        
          # remove snow:northness2 
          night2 <- update(globNight, . ~ . - snowSt:I(northnessSt*northnessSt))
          AIC(globNight, night2) # night2 ftw
          summary(night2)
          
          # also remove snow:slope and snow:slope2
          night3 <- update(night2, . ~ . - snowSt:slopeSt - snowSt:I(slopeSt*slopeSt))
          AIC(globNight, night2, night3) # night3 ftw
          summary(night3)
          
          # also remove snow:northness
          night4 <- update(night3, . ~ . - northnessSt:snowSt)
          AIC(globNight, night2, night3, night4) # night4 ftw
          
          # evaluate all along with LL to avoid incorrect conclusion
          aicNight <- data.frame(aictab(cand.set = c(globNight, night2, night3, night4), 
                            modnames = c("Global", "m2", "m3", "m4")))
          aicNight$LL <- c(logLik(globNight), logLik(night2), logLik(night3), logLik(night4))        
          aicNight <- aicNight[order(aicNight$Delta_AICc), ]
          write.csv(aicNight, file = "aic-envt-Night.csv", row.names = FALSE)            


          
               

 
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### #
####   | ASSESS MODEL FIT |  ####
### ### ### ### ### ### ### ### #    

          

      #### Specify top environmental models ####
          
          # envtDay <- day4
          envtDay <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                           + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                           + snowSt:canSt + snowSt:elevSt + snowSt:I(slopeSt*slopeSt) 
                           + snowSt:I(elevSt*elevSt) + (1|Pack), 
                           family = binomial(logit), data = datDay)           
        ## binned residual plots ##
        
          # day - whole model
          binnedplot(fitted(envtDay), residuals(envtDay, type = "response"), main = "Day - full model") # YAY
          
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
                         print.thres = c(.1, .5), col = "red", print.auc = T)) # auc = 0.676
          
          # night
          invisible(plot(roc(factor(ifelse(datNight$Used == 1, 1, 0)), fitted(envtNight)), 
                         print.thres = c(.1, .5), col = "red", print.auc = T)) # auc = 0.676          
          
          
        ## predictive accuracy @ >50% ##  
          
          # day
          confusionMatrix(factor(as.character(ifelse(fitted(envtDay) > 0.5, "Yes", "No"))), 
                          factor(ifelse(datDay$Used == 1, "Yes", "No")), positive = "Yes") # 63% 
          
          # night
          confusionMatrix(factor(as.character(ifelse(fitted(envtNight) > 0.5, "Yes", "No"))), 
                          factor(ifelse(datNight$Used == 1, "Yes", "No")), positive = "Yes") # 62%           
          


        
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
                     pCan, pLc, 
                     nrow = 2)
        
        
        
################################################################################################## #  
  
save.image(paste0("environmentalModels-", today(), ".RData"))        
        

################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### #
####   | HUMAN MODELS |  ####
### ### ### ### ### ### ### ### #    

# standardize covariates
                  