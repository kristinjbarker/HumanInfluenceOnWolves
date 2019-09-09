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
    tsa34day <- update(envtDay, . ~ . + distRdSt + distStrucSt + distFeedSt
                 + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                 + hunt*distRdSt + hunt*distStrucSt + hunt*distFeedSt
                 + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2))     
    tsa35day <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                 + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                 + hunt*distRdSt + hunt*distStrucSt + hunt*distFeedSt
                 + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2)
                 + hunt*canSt)    
    
      
    # night - tsb73, tsa32, tsb89, tsa34 (from models-Night.R)
    envtNight <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                       + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                       + snowSt:canSt + snowSt:elevSt + snowSt:I(elevSt*elevSt) 
                       + (1|Pack), family = binomial(logit), data = modDatNight,
                       control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4), calc.derivs = FALSE))     
    tsb73night <- update(envtNight, . ~ . + distRdSt + distStrucSt + distFeedSt
                 + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*distFeedSt
                 + prevHunt*canSt)      
     
    
      
    # crepuscular - tsa 34 & tsa 42 (from models-Crep.R)
    envtCrep <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
               + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
               + snowSt:canSt + snowSt:northnessSt + snowSt:elevSt
               + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
               + (1|Pack), family = binomial(logit), data = modDatCrep,
               control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4), calc.derivs = FALSE))
    tsa34crep <- update(envtCrep, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                 + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                 + hunt*distRdSt + hunt*distStrucSt + hunt*distFeedSt
                 + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2))
    tsa42crep <- update(envtCrep, . ~ . + distRdSt +distFeedSt + I(distRdSt^2) + I(distFeedSt^2)
                 + hunt*distRdSt + hunt*distFeedSt + hunt*I(distRdSt^2) + hunt*I(distFeedSt^2))          
      

    
      
 
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
####   | EVALUATE MODEL SUPPORT AND INFERENCE POTENTIAL |  ####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###   
    

    # day
        
        # 34 vs 35 (without vs with hunt*canopy interaction)
        summary(tsa34day); summary(tsa35day)
        # because the effect of the only different covariate not distinguishable from zero
        # only using the better-supported, less complex model to draw inference

        
    # crepuscular
        
        # 34 vs 42 (with vs without hunt*structure interaction)
        summary(tsa34crep); summary(tsa42crep)
        # effect slightly significant in the model where it's included
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
            ORNight = fixef(tsb73night),
            ciLowNight = confint(tsb73night, parm = "beta_", method = "Wald")[,1],
            ciHighNight = confint(tsb73night, parm = "beta_", method = "Wald")[,2])), 3)
          orN$Covariate = rownames(orN)
    
            
          # crepuscular
          orC <- round(exp(data.frame(
            ORCrep = fixef(tsa34crep),
            ciLowCrep = confint(tsa34crep, parm = "beta_", method = "Wald")[,1],
            ciHighCrep = confint(tsa34crep, parm = "beta_", method = "Wald")[,2])), 3)
          orC$Covariate = rownames(orC)
    
          
          # all together - combine and export
          orAll <- orD %>% full_join(orN, by = "Covariate") %>% full_join(orC, by = "Covariate")
          write.csv(orAll, file = "modTopAll.csv", row.names = F)

         
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
            OR = fixef(tsb73night),
            ciLow = confint(tsb73night, parm = "beta_", method = "Wald")[,1],
            ciHigh = confint(tsb73night, parm = "beta_", method = "Wald")[,2])), 3)
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
              model = "Day",
              Hunt  = hunt)
        
          # night
          n <- modDatNight %>%
            mutate(
              prWolf = fitted(tsb73night),
              model = "Night",
              Hunt = prevHunt)
          
          # crepuscular
          c <- modDatCrep %>%
            mutate(
              prWolf = fitted(tsa34crep),
              model = "Crepuscular",
              Hunt = hunt)
          
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
        stat_smooth(aes(x = distStruc, y = prWolf, linetype = as.factor(Hunt)), 
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(y = "Pr(Use)", title = "Distance to buildings & structures (m)")
      plot(pSt)
      
      # simplified plot
      sSt <- ggplot(m34, aes(x = distStruc, y = Used, colour = model)) +
        stat_smooth(aes(x = distStruc, y = prWolf, linetype = as.factor(Hunt)), 
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 0.75), xlim = c(0, 8000)) +
        labs(y = "Pr(Use)", x = "", title = "Distance to buildings & structures (m)")
      plot(sSt)
      
      # linear plot (night)
      nSt <-  ggplot(filter(m34, model == "Night"), aes(x = distStruc, y = Used)) +
        stat_smooth(aes(x = distStruc, y = prWolf, linetype = as.factor(Hunt)), 
                    method = "lm", formula = y ~ x)
      plot(nSt)
      
      
      # layering plot for easier conference explanation
      
    
    #### distance to motorized route  #### 
    
      # full plot
      pRd <- ggplot(m34, aes(x = distRd, y = Used, colour = model)) +
        geom_point(col = "black") +
        stat_smooth(aes(x = distRd, y = prWolf, linetype = as.factor(Hunt)), 
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(y = "Pr(Use)", title = "Distance to motorized route (m)")
      plot(pRd)
      
      # simplified plot
      sRd <- ggplot(m34, aes(x = distRd, y = Used, colour = model)) +
        stat_smooth(aes(x = distRd, y = prWolf, linetype = as.factor(Hunt)), 
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(xlim = c(0, 8000), ylim = c(0, 0.75)) +
        labs(y = "Pr(Use)", x = "", title = "Distance to motorized route (m)")
      plot(sRd)
    

    
    
    #### distance to feedgrounds ####
    
      # full plot
      pFd <- ggplot(filter(m34, feedIn == 1), aes(x = distFeed, y = Used, colour = model)) +
        geom_point(col = "black") +
        stat_smooth(aes(x = distFeed, y = prWolf, linetype = as.factor(Hunt)),
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 1), xlim = c(0, 30000)) +
        labs(y = "Pr(Use)", title = "Distance to feedgrounds (m)")
      plot(pFd)
      
      # simplified plot
      sFd <- ggplot(filter(m34, feedIn == 1), aes(x = plot, y = Used, colour = model)) +
        stat_smooth(aes(x = distFeed, y = prWolf, linetype = as.factor(Hunt)), 
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 0.85), xlim = c(0, 30000)) +
        labs(y = "Pr(Use)", x = "", title = "Distance to feedgrounds (m)")
      plot(sFd)    


       
    #### relative strenth of influences ####      
      
        
        # plot OR +- 95%CI colored by day/night - categorical covariate
        pLc <- ggplot(filter(orAllLong, grepl("lc", Covariate)), aes(x = Covariate, y = OR, colour = timing)) +
          geom_errorbar(aes(ymin = ciLow, ymax = ciHigh), width = 0.1) +
          geom_point() +
          geom_hline(aes(yintercept=1)) +
          labs(title = "Landcover type", x = "(relative to forest)") +
          scale_x_discrete(labels=c("Herbaceous","NoVeg","Riparian","Shrub"))      
      
 
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### # 
####   | POST HOC INVESTIGATIONS |  ####
### ### ### ### ### ### ### ### ### ### #
      
      
      # quick double check that envt-only not supported
      AIC(envtDay, tsa34day)
      AIC(envtNight, tsb73night)
      AIC(envtCrep, tsa42crep)
      
      
      
   
    
                        
################################################################################################## #  
      
################################################################################################## #  
      
      save.image(paste0("modelsHuman", today(), ".RData"))
      
################################################################################################## #  
      
################################################################################################## #        
  
    
### ### ### ### ### ### ### ### #### #
#### | LANDSCAPE SHIELD MODELS |  ####
### ### ### ### ### ### ### ### #### #
