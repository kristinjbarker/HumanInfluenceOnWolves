           ### ### ### ### ### ###  ### ### ### ### ### ### ### ### ### ### ### ### 
           #                                                                      #
           #                        BEST SUPPORTED MODELS                         #
           #     ASSESSING HUMAN INFLUENCE ON WOLF DISTRIBUTIONS AND BEHAVIORS    #
           #                                                                      #
           #                      Kristin Barker | Summer 2019                    #
           #                        kristinjbarker@gmail.com                      #
           #                                                                      #
           ### ### ### ### ### ###  ### ### ### ### ### ### ### ### ### ### ### ### 


################################################################################################## #


### ### ### ### ### #
####  | SETUP |  ####
### ### ### ### ### #

  
  #### Set working directory and filepath ####

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
      "dplyr")          ## data manipulation and general awesomeness
    
    
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
    
    memory.limit(size = 9999999999999) 

    
    
################################################################################################## #  
  
    
    
### ### ### ### ### ### #
####  | DATA PREP |  ####
### ### ### ### ### ### #
 
    
    #### Raw data ####
    
      modDatRaw <- read.csv("modDat.csv")


    #### Formatted data ####    
      
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
  
      # split out day/night/crepuscular time periods
      modDatDay <- filter(modDat, daytime == "day")  
      modDatNight <- filter(modDat, daytime == "night")
      modDatCrep <- filter(modDat, daytime == "crep")


      
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### ### #
####   | TOP MODELS |  ####
### ### ### ### ### ### ### ### ### #
    
    
    # day - tsa34, tsa50, tsa35 (from models-Day.R)
    envtDay <- glmer(Used ~ 1 + canSt + slopeSt + elevSt + northnessSt + snowSt
                     + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                     + snowSt:canSt + snowSt:northnessSt + snowSt:elevSt + snowSt:I(elevSt*elevSt) 
                     + (1|Pack), family = binomial(logit), data = modDatDay,
                     control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4), calc.derivs = FALSE))
    tsa34day <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                 + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                 + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt
                 + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2))     
    tsa50day <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                 + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                 + hunt*distRdSt + hunt*distStrucSt + hunt*motoUse + hunt*distFeedSt
                 + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2)) 
    tsa35day <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                 + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                 + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt
                 + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2)
                 + hunt*canSt)    
    
      
    # night - tsb73, tsa32, tsb89, tsa34 (from models-Night.R)
    envtNight <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                       + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                       + snowSt:canSt + snowSt:elevSt + snowSt:I(elevSt*elevSt) 
                       + (1|Pack), family = binomial(logit), data = modDatNight,
                       control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4), calc.derivs = FALSE))     
    tsb73night <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                 + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*distFeedSt
                 + prevHunt*canSt)      
    tsa32night <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                 + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt)
    tsb89night <- update(envtNight, . ~ . + distRdSt + distStrucSt + motoUse + distFeedSt
                 + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*motoUse + prevHunt*distFeedSt
                 + prevHunt*canSt) 
    tsa34night <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                 + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                 + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt
                 + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2))       
    
      
    # crepuscular - tsa 34 & tsa 42 (from models-Crep.R)
    envtCrep <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
               + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
               + snowSt:canSt + snowSt:northnessSt + snowSt:elevSt
               + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
               + (1|Pack), family = binomial(logit), data = modDatCrep,
               control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4), calc.derivs = FALSE))
    tsa34crep <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                 + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                 + hunt*distRdSt + hunt*distStrucSt + hunt*recClass + hunt*distFeedSt
                 + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2))
    tsa42crep <- update(envtCrep, . ~ . + distRdSt + recClass + distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                 + hunt*distRdSt + hunt*recClass + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2))          
      

    
      
 
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
####   | EVALUATE MODEL SUPPORT AND INFERENCE POTENTIAL |  ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###   
    

    # day
        
        # 34 vs 35 (without vs with hunt*canopy interaction)
        summary(tsa34day); summary(tsa35day)
        # because the effect of the only different covariate not distinguishable from zero
        # only using the better-supported, less complex model to draw inference
        
        # 34 vs 50 (all rec vs moto rec only)
        summary(tsa34day); summary(tsa50day)
        # diffs more subtle; check out in plots
    
    # night
        
        # 34 vs 32 (the same-year hunt models)
        # same except linear vs quadratic interactions
        # use plot visuals with raw data included to evaluate this
        
        # 73 vs 89 (the previous-year hunt models)
        # all rec vs moto only, just like with the day 34/50 diff
        # note these include canopy and the same-year hunt models don't
        summary(tsb73night); summary(tsb89night)
        # and the canopy*hunt is actually significant in these, unlike in day
        
        
    # crepuscular
        
        # 34 vs 42 (with vs without hunt*structure interaction)
        summary(tsa34crep); summary(tsa42crep)
        # effect looks pretty significant in the model where it's included
        # so can't just dismiss it

        
################################################################################################## #  
    
    
### ### ### ### ### ### ### ### ### ##
####   | EXTRACT MODEL RESULTS |  ####
### ### ### ### ### ### ### ### ### ##
        
        
    ##### odds ratios and confidence intervals ####
        
      ## wide form (for looking at in tables) ##

          # day
          orD <- round(exp(data.frame(
            ORDay = fixef(tsa34day),
            ciLowDay = confint(tsa34day, parm = "beta_", method = "Wald")[,1],
            ciHighDay = confint(tsa34day, parm = "beta_", method = "Wald")[,2])), 3)
          orD$Covariate = rownames(orD)
      

          # night
          orN <- round(exp(data.frame(
            ORNight = fixef(tsa34night),
            ciLowNight = confint(tsa34night, parm = "beta_", method = "Wald")[,1],
            ciHighNight = confint(tsa34night, parm = "beta_", method = "Wald")[,2])), 3)
          orN$Covariate = rownames(orN)
    
            
          # crepuscular
          orC <- round(exp(data.frame(
            ORCrep = fixef(tsa34crep),
            ciLowCrep = confint(tsa34crep, parm = "beta_", method = "Wald")[,1],
            ciHighCrep = confint(tsa34crep, parm = "beta_", method = "Wald")[,2])), 3)
          orC$Covariate = rownames(orC)
    
          
          # all together - combine and export
          orAll <- orD %>% left_join(orN, by = "Covariate") %>% left_join(orC, by = "Covariate")
          write.csv(orAll, file = "mod34-All.csv", row.names = F)

         
     ## long form (for plots) ##

          # day
          orDL <- round(exp(data.frame(
            OR = fixef(tsa34day),
            ciLow = confint(tsa34day, parm = "beta_", method = "Wald")[,1],
            ciHigh = confint(tsa34day, parm = "beta_", method = "Wald")[,2])), 3)
          orDL$Covariate = rownames(orD)
          orDL$timing = "Day"
      

          # night
          orNL <- round(exp(data.frame(
            OR = fixef(tsa34night),
            ciLow = confint(tsa34night, parm = "beta_", method = "Wald")[,1],
            ciHigh = confint(tsa34night, parm = "beta_", method = "Wald")[,2])), 3)
          orNL$Covariate = rownames(orN)
          orNL$timing = "Night"
    
            
          # crepuscular
          orCL <- round(exp(data.frame(
            OR = fixef(tsa34crep),
            ciLow = confint(tsa34crep, parm = "beta_", method = "Wald")[,1],
            ciHigh = confint(tsa34crep, parm = "beta_", method = "Wald")[,2])), 3)
          orCL$Covariate = rownames(orC)
          orCL$timing = "Crepuscular"
          
          # all together 
          orAllLong <- rbind(orDL, orNL, orCL)

       

        #### fitted values of continuous covariates ####
        
        
          # day
          d <- modDatDay %>%
            mutate(
              prWolf = fitted(tsa34day),
              model = "Day")
        
          # night
          n <- modDatNight %>%
            mutate(
              prWolf = fitted(tsa34night),
              model = "Night")
          
          # crepuscular
          c <- modDatCrep %>%
            mutate(
              prWolf = fitted(tsa34crep),
              model = "Crepuscular")
          
          # all together
          m34 <- bind_rows(d, n, c) 
    
      
 
################################################################################################## #  
  
    
    
### ### ### ### ###  ### ### ### ### ### 
####   | VISUALIZE MODEL RESULTS |  ####
### ### ### ### ###  ### ### ### ### ### 
        

    
    #### distance to buildings & structures ####
    
      # full plot
      pSt <- ggplot(m34, aes(x = distStruc, y = Used, colour = model)) +
        geom_point(col = "black") +
        stat_smooth(aes(x = distStruc, y = prWolf, linetype = as.factor(hunt)), method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(y = "Pr(Use)", title = "Distance to buildings & structures (m)")
      plot(pSt)
      
      # simplified plot
      sSt <- ggplot(m34, aes(x = distStruc, y = Used, colour = model)) +
        stat_smooth(aes(x = distStruc, y = prWolf, linetype = as.factor(hunt)), 
                    method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
        coord_cartesian(ylim = c(0, 0.75)) +
        labs(y = "Pr(Use)", x = "", title = "Distance to buildings & structures (m)")
      plot(sSt)
      
      
    
    #### distance to motorized route  #### 
    
      # full plot
      pRd <- ggplot(m34, aes(x = distRd, y = Used, colour = model)) +
        geom_point(col = "black") +
        stat_smooth(aes(x = distRd, y = prWolf, linetype = as.factor(hunt)), 
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(y = "Pr(Use)", title = "Distance to motorized route (m)")
      plot(pRd)
      
      # simplified plot
      sRd <- ggplot(m34, aes(x = distRd, y = Used, colour = model)) +
        stat_smooth(aes(x = distRd, y = prWolf, linetype = as.factor(hunt)), 
                    method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
        coord_cartesian(xlim = c(0, 15000), ylim = c(0, 0.75)) +
        labs(y = "Pr(Use)", x = "", title = "Distance to motorized route (m)")
      plot(sRd)
    

    
    
    #### distance to feedgrounds ####
    
      # full plot
      pFd <- ggplot(m34, aes(x = distFeed, y = Used, colour = model)) +
        geom_point(col = "black") +
        stat_smooth(aes(x = distFeed, y = prWolf, linetype = as.factor(hunt)),
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(y = "Pr(Use)", title = "Distance to feedgrounds (m)")
      plot(pFd)
      
      # simplified plot
      sFd <- ggplot(m34, aes(x = plot, y = Used, colour = model)) +
        stat_smooth(aes(x = distFeed, y = prWolf, linetype = as.factor(hunt)), 
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(xlim = c(0, 50000), ylim = c(0, 0.85)) +
        labs(y = "Pr(Use)", x = "", title = "Distance to feedgrounds (m)")
      plot(sFd)    


      
    #### recreation ####
      
      pRc <- ggplot(filter(orAllLong, grepl("recClass", Covariate)), 
                    aes(x = Covariate, y = OR, colour = timing)) +
        geom_errorbar(aes(ymin = ciLow, ymax = ciHigh), width = 0.1) +
        geom_point() +
        geom_hline(aes(yintercept=1)) +
        labs(title = "Winter recreation", x = "(relative to open recreation)") +
        scale_x_discrete(labels=c("Herbaceous","NoVeg","Riparian","Shrub"))          
      
      
 
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### # 
####   | POST HOC INVESTIGATIONS |  ####
### ### ### ### ### ### ### ### ### ### #
      
      
      # model fit if rec class is excluded
      
      noRecD <- update(tsa34day, . ~ . - recClass - hunt*recClass)
      noRecN <- update(tsa34night, . ~ . - recClass - hunt*recClass)
      noRecC <- update(tsa34crep, . ~ . - recClass - hunt*recClass)
      AIC(tsa34day, noRecD)
      AIC(tsa34night, noRecN)
      AIC(tsa34crep, noRecC)
      
                     
################################################################################################## #  
  
    
### ### ### ### ### ### ### ### #### #
#### | LANDSCAPE SHIELD MODELS |  ####
### ### ### ### ### ### ### ### #### #
