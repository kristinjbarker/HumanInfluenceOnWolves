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
        lcClass = factor(lcClass, levels = c("Forest", "Shrub", "Herbaceous", "Riparian", "NoVeg")))
  
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
    tsa35day <- update(envtDay, . ~ . + distRdSt + distStrucSt + distFeedSt
                 + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                 + hunt*distRdSt + hunt*distStrucSt + hunt*distFeedSt
                 + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2)
                 + hunt*canSt)    
    
      
    # night - tsb73 (from models-Night.R)
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
    tsa34crep <- update(envtCrep, . ~ . + distRdSt + distStrucSt + distFeedSt
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
      
      
      # zoomed in plot (to look at dist)
      sFdZ <- ggplot(filter(m34, feedIn == 1), aes(x = plot, y = Used, colour = model)) +
        stat_smooth(aes(x = distFeed, y = prWolf, linetype = as.factor(Hunt)), 
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 0.85), xlim = c(0, 3000)) +
        labs(y = "Pr(Use)", x = "", title = "Distance to feedgrounds (m)")
      plot(sFdZ) 
      # for all indivs?
      sFdZ <- ggplot(m34, aes(x = plot, y = Used, colour = model)) +
        stat_smooth(aes(x = distFeed, y = prWolf, linetype = as.factor(Hunt)), 
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 0.85), xlim = c(0, 3000)) +
        labs(y = "Pr(Use)", x = "", title = "Distance to feedgrounds (m)")
      plot(sFdZ)
      
      
      # all wolves (not just those with feedgrounds available)
      sFdA <- ggplot(m34, aes(x = plot, y = Used, colour = model)) +
        stat_smooth(aes(x = distFeed, y = prWolf, linetype = as.factor(Hunt)), 
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 0.85), xlim = c(0, 10000)) +
        labs(y = "Pr(Use)", x = "", title = "Distance to feedgrounds (m)")
      plot(sFdA) 
      # flatline at pretty much 0.5


       
    #### relative strenth of influences ####      
      
        
        # plot OR +- 95%CI colored by day/night - categorical covariate
        pLc <- ggplot(filter(orAllLong, grepl("lc", Covariate)), aes(x = Covariate, y = OR, colour = timing)) +
          geom_errorbar(aes(ymin = ciLow, ymax = ciHigh), width = 0.1) +
          geom_point() +
          geom_hline(aes(yintercept=1)) +
          labs(title = "Landcover type", x = "(relative to forest)") +
          scale_x_discrete(labels=c("Herbaceous","NoVeg","Riparian","Shrub"))      
      
 
      
      
###  ####  ####  ###  ####  ####  ###  ####  ####  ###  ####  ####  ###  ####  ###  
  

      #### Conference Plots ####
      
      
        ## setup ##
          
          # set black theme
          theme_black = function(base_size = 12, base_family = "") {
            theme_grey(base_size = base_size, base_family = base_family) %+replace%
              theme(
                # Specify axis options
                axis.line = element_blank(),  
                axis.text.x = element_text(color = "white"),  
                axis.text.y = element_text(color = "white"),  
                axis.ticks = element_line(color = "white", size  =  0.2),  
                axis.title.x = element_text(size = base_size*1.5, color = "white", margin = margin(0, 10, 0, 0)),  
                axis.title.y = element_text(size = base_size*1.5, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
                axis.ticks.length = unit(0.3, "lines"),   
                # Specify legend options
                legend.background = element_rect(color = NA, fill = "black"),  
                legend.key = element_rect(color = "white",  fill = "black"),  
                legend.key.size = unit(1.2, "lines"),  
                legend.key.height = NULL,  
                legend.key.width = NULL,      
                legend.text = element_text(color = "white"),  
                ####legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
                legend.title = element_blank(),
                legend.position = "right",  
                legend.text.align = NULL,  
                legend.title.align = NULL,  
                legend.direction = "vertical",  
                legend.box = NULL, 
                legend.spacing.x = unit(0.3, 'cm'),
                # Specify panel options
                panel.background = element_rect(fill = "black", color  =  NA),  
                panel.border = element_rect(fill = NA, color = "white"),  
                panel.grid.major = element_line(color = "grey35"),  
                panel.grid.minor = element_line(color = "grey20"),  
                panel.margin = unit(0.5, "lines"),   
                # Specify facetting options
                strip.background = element_rect(fill = "grey30", color = "grey10"),  
                strip.text.x = element_text(size = base_size*0.8, color = "white"),  
                strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
                # Specify plot options
                plot.background = element_rect(color = "black", fill = "black"),  
                plot.title = element_text(size = base_size*2, color = "white"),  
                plot.margin = unit(rep(1, 4), "lines")
              )
          }          
               
      
        # format data to make legend easier
        ggDat <- m34 %>%
          mutate(Hunt = ifelse(Hunt == 0, "Not hunted", "Hunted"))
        
      
      #### Buildings ####
        
          b1 <- ggplot(filter(ggDat, daytime == "day" & Hunt == "Not hunted"), 
                       aes(x = distStruc, y = Used, colour = model)) +
            stat_smooth(aes(x = distStruc, y = prWolf), 
                        method = "lm", formula = y ~ poly(x, 2), linetype = "dashed") +
            coord_cartesian(ylim = c(0, 0.75), xlim = c(0, 8000)) +
            labs(y = "Pr(WolfUse)", x = "meters", title = "Distance to Buildings") +
            theme_black() +
            guides(linetype = guide_legend(order = 2, override.aes = list(col = 'white')))  +
            scale_color_manual(name = "",
                          breaks = c("Day", "Crepuscular", "Night"),
                          values = c("Day" = "green", "Crepuscular" = "orange", "Night" = "blue"))
           b1
           ggsave(b1, filename = "./Plots/bldgs1.jpg", dpi = 600, units = "in", width = 13, height = 7)
          
          
          b2 <- ggplot(filter(ggDat, daytime == "day"), 
                       aes(x = distStruc, y = Used, colour = model)) +
            stat_smooth(aes(x = distStruc, y = prWolf, linetype = as.factor(Hunt)), 
                        method = "lm", formula = y ~ poly(x, 2), color = "green") +
            coord_cartesian(ylim = c(0, 0.75), xlim = c(0, 8000)) +
            labs(y = "Pr(WolfUse)", x = "meters", title = "Distance to Buildings") +
            theme_black() +
            guides(linetype = guide_legend(order = 2, override.aes = list(col = 'white')))  +
            scale_color_manual(name = "",
                          breaks = c("Day", "Crepuscular", "Night"),
                          values = c("Day" = "green", "Crepuscular" = "orange", "Night" = "blue"))
          b2
          ggsave(b2, filename = "./Plots/bldgs2.jpg", dpi = 600, units = "in", width = 13, height = 7)
    
          b3 <- ggplot(filter(ggDat, daytime == "day" | daytime == "crep"), 
                       aes(x = distStruc, y = Used, colour = model)) +
            stat_smooth(aes(x = distStruc, y = prWolf, linetype = as.factor(Hunt)), 
                        method = "lm", formula = y ~ poly(x, 2)) +
            coord_cartesian(ylim = c(0, 0.75), xlim = c(0, 8000)) +
            labs(y = "Pr(WolfUse)", x = "meters", title = "Distance to Buildings") +
            theme_black() +
            guides(linetype = guide_legend(order = 2, override.aes = list(col = 'white')))  +
            scale_color_manual(name = "",
                          breaks = c("Day", "Crepuscular", "Night"),
                          values = c("Day" = "green", "Crepuscular" = "orange", "Night" = "blue"))
          b3 
          ggsave(b3, filename = "./Plots/bldgs3.jpg", dpi = 600, units = "in", width = 13, height = 7)
          
          b4 <- ggplot(ggDat, 
                       aes(x = distStruc, y = Used, colour = model)) +
            stat_smooth(aes(x = distStruc, y = prWolf, linetype = as.factor(Hunt)), 
                        method = "lm", formula = y ~ poly(x, 2)) +
            coord_cartesian(ylim = c(0, 0.75), xlim = c(0, 8000)) +
            labs(y = "Pr(WolfUse)", x = "meters", title = "Distance to Buildings") +
            theme_black() +
            guides(linetype = guide_legend(order = 2, override.aes = list(col = 'white')))  +
            scale_color_manual(name = "",
                          breaks = c("Day", "Crepuscular", "Night"),
                          values = c("Day" = "green", "Crepuscular" = "orange", "Night" = "blue"))
          b4   
          ggsave(b4, filename = "./Plots/bldgs4.jpg", dpi = 600, units = "in", width = 13, height = 7)
      

      #### Roads ####
        
          r1 <- ggplot(filter(ggDat, daytime == "day" & Hunt == "Not hunted"), 
                       aes(x = distRd, y = Used, colour = model)) +
            stat_smooth(aes(x = distRd, y = prWolf, linetype = as.factor(Hunt)), 
                        method = "lm", formula = y ~ poly(x, 2), linetype = "dashed") +
            coord_cartesian(ylim = c(0, 0.75), xlim = c(0, 8000)) +
            labs(y = "Pr(WolfUse)", x = "meters", title = "Distance to Route") +
            theme_black() +
            guides(linetype = guide_legend(order = 2, override.aes = list(col = 'white')))  +
            scale_color_manual(name = "",
                          breaks = c("Day", "Crepuscular", "Night"),
                          values = c("Day" = "green", "Crepuscular" = "orange", "Night" = "blue"))
           r1
           ggsave(r1, filename = "./Plots/rds1.jpg", dpi = 600, units = "in", width = 13, height = 7)
          
          
          r2 <- ggplot(filter(ggDat, daytime == "day"), 
                       aes(x = distRd, y = Used, colour = model)) +
            stat_smooth(aes(x = distRd, y = prWolf, linetype = as.factor(Hunt)), 
                        method = "lm", formula = y ~ poly(x, 2), color = "green") +
            coord_cartesian(ylim = c(0, 0.75), xlim = c(0, 8000)) +
            labs(y = "Pr(WolfUse)", x = "meters", title = "Distance to Route") +
            theme_black() +
            guides(linetype = guide_legend(order = 2, override.aes = list(col = 'white')))  +
            scale_color_manual(name = "",
                          breaks = c("Day", "Crepuscular", "Night"),
                          values = c("Day" = "green", "Crepuscular" = "orange", "Night" = "blue"))
          r2
          ggsave(r2, filename = "./Plots/rds2.jpg", dpi = 600, units = "in", width = 13, height = 7)
    
          r3 <- ggplot(filter(ggDat, daytime == "day" | daytime == "crep"), 
                       aes(x = distRd, y = Used, colour = model)) +
            stat_smooth(aes(x = distRd, y = prWolf, linetype = as.factor(Hunt)), 
                        method = "lm", formula = y ~ poly(x, 2)) +
            coord_cartesian(ylim = c(0, 0.75), xlim = c(0, 8000)) +
            labs(y = "Pr(WolfUse)", x = "meters", title = "Distance to Route") +
            theme_black() +
            guides(linetype = guide_legend(order = 2, override.aes = list(col = 'white')))  +
            scale_color_manual(name = "",
                          breaks = c("Day", "Crepuscular", "Night"),
                          values = c("Day" = "green", "Crepuscular" = "orange", "Night" = "blue"))
          r3 
          ggsave(r3, filename = "./Plots/rds3.jpg", dpi = 600, units = "in", width = 13, height = 7)
          
          r4 <- ggplot(ggDat, 
                       aes(x = distRd, y = Used, colour = model)) +
            stat_smooth(aes(x = distRd, y = prWolf, linetype = as.factor(Hunt)), 
                        method = "lm", formula = y ~ poly(x, 2)) +
            coord_cartesian(ylim = c(0, 0.75), xlim = c(0, 8000)) +
            labs(y = "Pr(WolfUse)", x = "meters", title = "Distance to Route") +
            theme_black() +
            guides(linetype = guide_legend(order = 2, override.aes = list(col = 'white')))  +
            scale_color_manual(name = "",
                          breaks = c("Day", "Crepuscular", "Night"),
                          values = c("Day" = "green", "Crepuscular" = "orange", "Night" = "blue"))
          r4   
          ggsave(r4, filename = "./Plots/rds4.jpg", dpi = 600, units = "in", width = 13, height = 7)
  
      
            
      
      
      
###  ####  ####  ###  ####  ####  ###  ####  ####  ###  ####  ####  ###  ####  ###  
 
      
      
           
 
################################################################################################## #  
      
    
### ### ### ### ### ### ### ### ### ### # 
####   | POST HOC INVESTIGATIONS |  ####
### ### ### ### ### ### ### ### ### ### #
      

    #### crossing i's and dotting t's ####          
            
      # quick double check that envt-only not supported
      AIC(envtDay, tsa34day)
      AIC(envtNight, tsb73night)
      AIC(envtCrep, tsa42crep)
      # yeah not even close
      
      
      
    #### re-evaluate feed only using wolves to whom it's available ####    
    
        # rerun models using only those wolves
        tDay <- update(tsa34day, data = filter(modDatDay, feedIn == 1))
        tNight <- update(tsb73night, data = filter(modDatNight, feedIn == 1))
        tCrep <- update(tsa34crep, data = filter(modDatCrep, feedIn == 1))

        # data for plots
        td <- filter(modDatDay, feedIn == 1) %>%
          mutate(
            prWolf = fitted(tDay),
            model = "Day",
            Hunt  = hunt)
        tn <- filter(modDatNight, feedIn == 1) %>%
          mutate(
            prWolf = fitted(tNight),
            model = "Night",
            Hunt = prevHunt)
        tc <- filter(modDatCrep, feedIn == 1) %>%
          mutate(
            prWolf = fitted(tCrep),
            model = "Crepuscular",
            Hunt = hunt)
        tRes <- bind_rows(td, tn, tc)          
          
      # full plot
      tFd <- ggplot(tRes, aes(x = distFeed, y = Used, colour = model)) +
        geom_point(col = "black") +
        stat_smooth(aes(x = distFeed, y = prWolf, linetype = as.factor(Hunt)),
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 1), xlim = c(0, 30000)) +
        labs(y = "Pr(Use)", title = "Wolves w/ Feedgrounds Available")
      plot(tFd)  
      
      # compare to other
      plot(pFd)
      
      # looks almost exactly the same
      

      
################################################################################################## #  
  
    
    
### ### ### ### ###  ### ### ##
####   | DATA SUMMARIES |  ####
### ### ### ### ###  ### ### ##
      
      
      # sample sizes
      length(unique(modDat$Pack))
      length(unique(modDat$Wolf))
      length(unique(modDat$wolfYr))
      length(unique(modDat$Year))
      sort(unique(modDat$Year))
                        
################################################################################################## #  
      
################################################################################################## #  
      
      save.image(paste0("modelsHuman", today(), ".RData"))
      
################################################################################################## #  
      
################################################################################################## #        
  
    
### ### ### ### ### ### ### ### #### #
#### | LANDSCAPE SHIELD MODELS |  ####
### ### ### ### ### ### ### ### #### #
