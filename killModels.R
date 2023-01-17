
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
 
    
    modDatRaw <- read.csv("killDat.csv")
    
################################################################################################## #  
  
    
    
### ### ###  ### ### ### ###
####   | FORMAT DATA |  ####
### ### ###  ### ### ### ###

    
    modDat <- modDatRaw %>% mutate(
      # handle datetimes and dates of course
      Date = ymd(Date),
      # year as numeric
      Year = as.numeric(Year),
      # order landcover from most to least available
      lcClass = factor(lcClass, levels = c("Forest", "Shrub", "Herbaceous", "Riparian", "NoVeg")))
    
    

    #### check correlations between all environmental and human covariates ####

    dat.cor <- modDat %>%
      dplyr::select(can, elev, slope, rug, snowCm, northness,
                    distRd, distStruc, distFeed, distFeedActive)
    corDat <- cor(dat.cor)    
    # correlations >= 0.50
    cor5 <- corDat
    cor5[cor5 < 0.50] <- NA
    cor5    


################################################################################################## #  
                
    
    
### ### ### ### ### ### ### ### ### ### ### ### ##
####   | EVALUATE COVARIATE EFFECTS|  ####
### ### ### ### ### ### ### ### ### ### ### ### ##  
            
            

    # distance to buildings and structures
    mS <- glm(Used ~ 1 + distStrucSt + I(distStrucSt^2), family = binomial(logit), data = modDat)
    summary(mS) # struc^2 insig
    
    # distance to motorized routes
    mR <- glm(Used ~ 1 + distRdSt + I(distRdSt^2), family = binomial(logit), data = modDat)
    summary(mR) # 2 sig here
    
    # distance to feedgrounds
    mF <- glm(Used ~ 1 + distFeedSt + I(distFeedSt^2), family = binomial(logit), data = modDat)
    summary(mF) # nothing sig here
    
    # all 3??
    mA <- glm(Used ~ 1 + distStrucSt + I(distStrucSt^2) 
              + distRdSt + I(distRdSt^2)+ distFeedSt + I(distFeedSt^2)
              , family = binomial(logit), data = modDat)
    summary(mA)
    # because feed isn't estimable/important i'm going to just do roads and structures
    
    m2 <- glm(Used ~ 1 + distStrucSt + I(distStrucSt^2) + distRdSt + I(distRdSt^2), 
              family = binomial(logit), data = modDat)
    summary(m2)
    
 
    #### fitted values ####
    
    
      # 
      d <- modDat %>%
        mutate(
          prWolf = fitted(m2),
          Hunt  = hunt)
           

################################################################################################## #  
                
    
    
### ### ### ### ### #
####   | PLOTS|  ####
### ### ### ### ### # 
    
    
    ## BUILDINGS AND STRUCTURES ##
    
    
    # full plot
      pSt <- ggplot(d, aes(x = distStruc, y = Used)) +
        geom_point(col = "black") +
        stat_smooth(aes(x = distStruc, y = prWolf), 
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(y = "Pr(Use)", title = "Distance to buildings & structures (m)")
      plot(pSt)
      
      # clean plot
      pStB <- ggplot(d, aes(x = distStruc, y = Used)) +
        stat_smooth(aes(x = distStruc, y = prWolf), 
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 0.75), xlim = c(0, 4000)) +
        labs(y = "Pr(Use)", x = "", title = "Distance to buildings & structures (m)")
      plot(pStB)
      

    ## ROADS ##
    
    
    # full plot
      pRd <- ggplot(d, aes(x = distRd, y = Used)) +
        geom_point(col = "black") +
        stat_smooth(aes(x = distRd, y = prWolf), 
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(y = "Pr(Use)", title = "Distance to buildings & Rdtures (m)")
      plot(pRd)
      
      # clean combo plot
      pRdB <- ggplot(d, aes(x = distRd, y = Used)) +
        stat_smooth(aes(x = distStruc, y = prWolf), 
                    method = "lm", formula = y ~ x) +
        stat_smooth(aes(x = distRd, y = prWolf, col = "red"), 
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 0.75), xlim = c(0, 6000)) +
        labs(y = "Pr(Use)", x = "Distance (m)", title = "Preliminary Kill Site Model Results")
      plot(pRdB)
      

    ## FEED ##
    
    
    # full plot
      pFd <- ggplot(d, aes(x = distFeed, y = Used)) +
        geom_point(col = "black") +
        stat_smooth(aes(x = distFeed, y = prWolf), 
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(y = "Pr(Use)", title = "Distance to feed ground (m)")
      plot(pFd)
      
      # clean combo plot
      pFdB <- ggplot(d, aes(x = distFeed, y = Used)) +
        stat_smooth(aes(x = distFeed, y = prWolf), 
                    method = "lm", formula = y ~ poly(x, 2))  +
        coord_cartesian(ylim = c(0, 0.75), xlim = c(0, 6000)) +
        labs(y = "Pr(Use)", x = "Distance to feed ground (m)", title = "Preliminary Kill Site Model Results")
      plot(pFdB)
    
    # plot without LGV (available feed only)
      pFd2 <- ggplot(filter(d, Pack != "Lower Gros Ventre"), aes(x = distFeed, y = Used)) +
        geom_point(col = "black") +
        stat_smooth(aes(x = distFeed, y = prWolf), 
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(y = "Pr(Use)", title = "Distance to feed ground (m)")
      plot(pFd2)      
      
      
      
   ## COMBO ##     
      
      # clean combo plot
      p3 <- ggplot(d, aes(y = Used)) +
        stat_smooth(aes(x = distStruc, y = prWolf, col = "Buildings"), 
                    method = "lm", formula = y ~ x) +
        stat_smooth(aes(x = distRd, y = prWolf, col = "Routes"), 
                    method = "lm", formula = y ~ poly(x, 2)) +
        stat_smooth(aes(x = distFeed, y = prWolf, col = "Feedgrounds"), 
                    method = "lm", formula = y ~ poly(x, 2))  +        
        coord_cartesian(ylim = c(0, 0.75), xlim = c(0, 6000)) +
        labs(y = "Pr(Use)", x = "Distance (m)", title = "Preliminary Kill Site Model Results")
      plot(p3)      
      
      
      
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
               
      
          plotKill <- ggplot(d, aes(y = prWolf)) +
            stat_smooth(aes(x = distStruc, col = "Buildings"), 
                        method = "lm", formula = y ~ poly(x, 2)) +
            stat_smooth(aes(x = distRd, y = prWolf, col = "Routes"), 
                        method = "lm", formula = y ~ poly(x, 2)) +            
            coord_cartesian(ylim = c(0, 0.75), xlim = c(0, 8000)) +
            labs(y = "Pr(Kill)", x = "Distance (m)", title = "Preliminary Predation Results") +
            theme_black() 
          plotKill   
          ggsave(plotKill, filename = "./Plots/kills.jpg", dpi = 600, units = "in", width = 13, height = 7)      
      

            
      
###  ####  ####  ###  ####  ####  ###  ####  ####  ###  ####  ####  ###  ####  ###  
      
      save.image("prelimKillModels.RData")
      
      
      # playtime contrasting strength of effect of each

      