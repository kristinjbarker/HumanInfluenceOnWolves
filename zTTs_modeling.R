                   
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### 
####   | MODEL DIAGNOSTICS |  ####
### ### ### ### ### ### ### ### ### 




        #### attempts to check model fits ####
                
                # code from bolker github
                plot(reDay, main = "day")
                plot(reNight, main = "night")
                # well these look fucking awful (i think...)
                
                # try qq plots?
                qqnorm(resid(reDay, type = "pearson", scaled = "true")) 
                # fuuuuuuck
                qqnorm(resid(reNight, type = "pearson", scaled = "true")) 
                # literally the worst qq plot i've ever seen

                
                ## code from zuur book, don't bother
                dResid <- resid(reDay, type = "pearson", scaled = "true")
                nResid <- resid(reNight, type = "pearson", scaled = "true")
                coplot(dResid ~ slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                         slopeSt|snowSt + northnessSt|snowSt,
                       data = datDay, ylab="Normalised residuals") 
                coplot(nResid ~ slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                         slopeSt|snowSt + northnessSt|snowSt,
                       data = datDay, ylab="Normalised residuals")
                
                
       #### plot residuals by covariate to determine whether quadratic terms/additive models would be more appropriate ####
           
            # this and other helpful info from
            # https://stats.stackexchange.com/questions/63566/unexpected-residuals-plot-of-mixed-linear-model-using-lmer-lme4-package-in-r
            arm::binnedplot(fitted(globDay),resid(globDay))
            arm::binnedplot(fitted(globNight),resid(globNight))
            
            # qq-plot of random effects
            # sjPlot::sjp.glmer(globDay, type = "re.qq")
            # nvr mind this package is bs
            
            car::residualPlots(globDay) # doesn't work for glmer
            # screw it let's just make a linear model and look at that
            # oh actually try one more thing
            
            DHARMa::simulateResiduals(fittedModel = globDay, n = 100)
                
            # what if i kill interactions
            playDay <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                               I(slopeSt*slopeSt) + I(elevSt*elevSt) + # quadratics
                               (1|Pack/wolfYr), family = binomial(logit), 
                             nAGQ = 0, control = glmerControl(optimizer = "nloptwrap"), # jenky convergence fix
                             data = datDay)    
            arm::binnedplot(fitted(playDay), resid(playDay))
            AIC(playDay, fullDay)
            
            # what if i kill optimizer
            playDay <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                               I(slopeSt*slopeSt) + I(elevSt*elevSt) + # quadratics
                               I(canSt*snowSt) + I(slopeSt*snowSt) + I(northnessSt*snowSt) + # interactions
                               (1|Pack/wolfYr), family = binomial(logit), 
                             data = datDay)    
            arm::binnedplot(fitted(playDay), resid(playDay))
            AIC(playDay, fullDay)
            # ha yeah i'll just wait a month to see how that went. not.
            
            # what if i get rid of optimizer but keep low AGQ and just don't calc derviatives
            playDay <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                               I(slopeSt*slopeSt) + I(elevSt*elevSt) + # quadratics
                               I(canSt*snowSt) + I(slopeSt*snowSt) + I(northnessSt*snowSt) + # interactions
                               (1|Pack/wolfYr), family = binomial(logit), 
                             nAGQ = 0, control = glmerControl(calc.derivs = FALSE), 
                             data = datDay)    
            arm::binnedplot(fitted(playDay), resid(playDay))
            AIC(playDay, fullDay)
            # no diff with previous model residual plot         
            
            # what if i get rid of optimizer and AGQ and just don't calc derviatives
            playDay <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                               I(slopeSt*slopeSt) + I(elevSt*elevSt) + # quadratics
                               # I(canSt*snowSt) + I(slopeSt*snowSt) + I(northnessSt*snowSt) + # interactions
                               (1|Pack/wolfYr), family = binomial(logit), 
                             control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                             data = datDay)    
            arm::binnedplot(fitted(playDay), resid(playDay))
            AIC(playDay, fullDay)
            # >5 mins, aint nobody got time fo that
            
            
            # what if i get keep AGQ and just don't calc derivatives
            playDay <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                               I(slopeSt*slopeSt) + I(elevSt*elevSt) + # quadratics
                               I(canSt*snowSt) + I(slopeSt*snowSt) + I(northnessSt*snowSt) + # interactions
                               (1|Pack/wolfYr), family = binomial(logit), 
                             control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                             data = datDay)    
            arm::binnedplot(fitted(playDay), resid(playDay))
            AIC(playDay, fullDay)
            # uuuugh same fing plot i'm f'ed
            # but i am gonna change to this rather than the agq = 0 thing, seems better     
            
            
            
            
            # residual plots - pearson, or response?
            # response: in course owen sent - oh this is just a glm https://bookdown.org/jefftemplewebb/IS-6489/logistic-regression.html#assessing-logistic-model-performance
            # pearson: bolker seems to use them in http://bbolker.github.io/mixedmodels-misc/ecostats_chap.html
            # rolling with pearson
            par(mfrow = c(2, 1))
            # day
            arm::binnedplot(fitted(thinDay), residuals(thinDay, type = "pearson"), 
                            main = "pearson - day", idLabels = ~.obs)
            arm::binnedplot(fitted(thinNight), residuals(thinNight, type = "pearson"), main = "pearson - Night") 
            par(mfrow = c(1, 1))    
            
            binnedplot(thinDatDay$canSt, residuals(thinDay, type = "pearson"), main = "canopy - day")
            binnedplot(thinDatDay$slopeSt, residuals(thinDay, type = "pearson"), main = "slope - day")
            binnedplot(thinDatDay$elevSt, residuals(thinDay, type = "pearson"), main = "elev - day")
            binnedplot(thinDatDay$northnessSt, residuals(thinDay, type = "pearson"), main = "northness - day")
            binnedplot(thinDatDay$snowSt, residuals(thinDay, type = "pearson"), main = "snow - day")
            
            binnedplot(thinDatNight$canSt, residuals(thinNight, type = "pearson"), main = "canopy - Night")
            binnedplot(thinDatNight$slopeSt, residuals(thinNight, type = "pearson"), main = "slope - Night")
            binnedplot(thinDatNight$elevSt, residuals(thinNight, type = "pearson"), main = "elev - Night")
            binnedplot(thinDatNight$northnessSt, residuals(thinNight, type = "pearson"), main = "northness - Night")
            binnedplot(thinDatNight$snowSt, residuals(thinNight, type = "pearson"), main = "snow - Night")
            
          
            # attempting to assess outliers ####
            modDat[max(modDat$snowCm),]
            
            
            
            # what if i use quadratic for northness bc the distn is so skewy? (this makes no sense kristin)
            playDay <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                               I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) + # quadratics
                               I(canSt*snowSt) + I(slopeSt*snowSt) + I(northnessSt*snowSt) + # interactions
                               (1|Pack/wolfYr), family = binomial(logit), 
                             control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                             data = thinDatDay)   
            AIC(playDay, thinDay)
            invisible(plot(roc(factor(ifelse(thinDatDay$Used == 1, 1, 0)),
                               fitted(playDay)), print.thres = c(.1, .5),
                           col = "red", print.auc = T))  
            summary(playDay)
            # improvement
            
            # what if i use quadratic for northness bc the distn is so skewy? (this makes no sense kristin)
            playNight <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                               I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) + # quadratics
                               I(canSt*snowSt) + I(slopeSt*snowSt) + I(northnessSt*snowSt) + # interactions
                               (1|Pack/wolfYr), family = binomial(logit), 
                             control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                             data = thinDatNight)   
            AIC(playNight, thinNight)
            invisible(plot(roc(factor(ifelse(thinDatNight$Used == 1, 1, 0)),
                               fitted(playNight)), print.thres = c(.1, .5),
                           col = "red", print.auc = T))  
            summary(playNight)   
            
            binnedplot(thinDatDay$canSt, residuals(playDay, type = "pearson"), main = "canopy - day")
            binnedplot(thinDatDay$slopeSt, residuals(playDay, type = "pearson"), main = "slope - day")
            binnedplot(thinDatDay$elevSt, residuals(playDay, type = "pearson"), main = "elev - day")
            binnedplot(thinDatDay$northnessSt, residuals(playDay, type = "pearson"), main = "northness - day")
            binnedplot(thinDatDay$snowSt, residuals(playDay, type = "pearson"), main = "snow - day")
            
            binnedplot(thinDatNight$canSt, residuals(playNight, type = "pearson"), main = "canopy - Night")
            binnedplot(thinDatNight$slopeSt, residuals(playNight, type = "pearson"), main = "slope - Night")
            binnedplot(thinDatNight$elevSt, residuals(playNight, type = "pearson"), main = "elev - Night")
            binnedplot(thinDatNight$northnessSt, residuals(playNight, type = "pearson"), main = "northness - Night")
            binnedplot(thinDatNight$snowSt, residuals(playNight, type = "pearson"), main = "snow - Night")
            
             
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### 
####   | OVERDISPERSION |  ####
### ### ### ### ### ### ### ### ### 
            
          #### evaluating ####
            
            # overdispersion check
            install.packages("aods3")
            aods3::gof(thinDay)
            aods3::gof(playDay) 
            # is the sum of squared pearson residuals > residual degrees of freedom?
            # yes, much (by 18966.5 for day...)
            
            # different check (by stealing code from bolker, of course)
            overdisp_fun <- function(model) {
                rdf <- df.residual(model)
                rp <- residuals(model,type="pearson")
                Pearson.chisq <- sum(rp^2)
                prat <- Pearson.chisq/rdf
                pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
                c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
            }         
            overdisp_fun(playDay) 
            # yes yes ok very overdispersed
            
            
            
          #### adjusting for ####
                        
  
            #quasi-likelihood adjustment?
                
              cc <- coef(summary(playDay))
              phi <- overdisp_fun(playDay)["ratio"]
              cc <- within(as.data.frame(cc),
              {   `Std. Error` <- `Std. Error`*sqrt(phi)
                  `z value` <- Estimate/`Std. Error`
                  `Pr(>|z|)` <- 2*pnorm(abs(`z value`), lower.tail=FALSE)
              })
              printCoefmat(cc,digits=3)
              summary(playDay)
              # this makes little to no difference in covariate estimates (sometimes by 0.0001 or so)
              
              
            # add observation level random effect
              
              # from https://figshare.com/articles/Step_by_Step_Guide_to_Observation_Level_Random_Effects/1144470
              # (linked from bolker GLMM worked example)
  
              #Create a sequence of numbers corresponding to each observation (rows of the dataframe)
              obs<-seq(nrow(thinDatDay))
              
              #fit a new model using that sequence as an additional random effect (jfc biggest model ever)
              newDay <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                                 I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) + # quadratics
                                 I(canSt*snowSt) + I(slopeSt*snowSt) + I(northnessSt*snowSt) + # interactions
                                 (1|Pack/wolfYr) + (1|obs), family = binomial(logit), 
                               control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                               data = thinDatDay) 
              AIC(newDay, playDay, thinDay)
              # playday ftw; new day is 2 points worse due to addl dof
              BIC(newDay, playDay, thinDay)
              # playday very much ftw
              
              aods3::gof(newDay)
              # still overdispersed
              
              invisible(plot(roc(factor(ifelse(thinDatDay$Used == 1, 1, 0)),
                                 fitted(newDay)), print.thres = c(.1, .5),
                             col = "red", print.auc = T))   
              # and auc is no better. 
              
              summary(newDay)
              summary(playDay)
              # and again the estimates don't change much
              
              # quick comparison of with ad without random effects
              feDay <- glm(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                                 I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) + # quadratics
                                 I(canSt*snowSt) + I(slopeSt*snowSt) + I(northnessSt*snowSt), 
                           family = binomial(logit), data = thinDatDay)     
              AIC(feDay, playDay)
              # ok yeah random effects way better
              
              

            
            ## try different random effects specifications?
            
              
              ## random slopes and intercepts, vary independently of each other
              seDay <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                                 I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) + # quadratics
                                 I(canSt*snowSt) + I(slopeSt*snowSt) + I(northnessSt*snowSt) + # interactions
                                 (1|wolfYr) + (Pack - 1|wolfYr), family = binomial(logit), 
                               control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                               data = thinDatDay) 
              seNight <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                                 I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) + # quadratics
                                 I(canSt*snowSt) + I(slopeSt*snowSt) + I(northnessSt*snowSt) + # interactions
                                 (1|wolfYr) + (Pack - 1|wolfYr), family = binomial(logit), 
                               control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                               data = thinDatNight)      
              
              ## random slopes and intercepts, covary
              seDay2 <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                                 I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) + # quadratics
                                 I(canSt*snowSt) + I(slopeSt*snowSt) + I(northnessSt*snowSt) + # interactions
                                 (1 + Pack|wolfYr), family = binomial(logit), 
                               control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                               data = thinDatDay) 
              seNight2 <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                                 I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) + # quadratics
                                 I(canSt*snowSt) + I(slopeSt*snowSt) + I(northnessSt*snowSt) + # interactions
                                 (1 + Pack|wolfYr), family = binomial(logit), 
                               control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                               data = thinDatNight)
  
  
              aods3::gof(seDay)
              aods3::gof(newDay)
              aods3::gof(thinDay)
              # all are overdispersed
              
              # compare the above "se" models to the existing best models (which are the "play" ones)
              AIC(seDay, seDay2, playDay) # playDay much better
              AIC(seNight, seNight2, playNight) # playNight also better
              
              
              #save.image("randomEffectsModels.RData")  
              
              
              BIC(seDay, seDay2, playDay) # playDay much better bc of dof which you just learned allll about
              
              # so duh, adding that much complexity with the random effects blows up the dof
              # i wonder what it does the the predictive power tho, just out of curiosity...
              
                invisible(plot(roc(factor(ifelse(thinDatDay$Used == 1, 1, 0)),
                                   fitted(seDay)), print.thres = c(.1, .5),
                               col = "red", print.auc = T)) # auc = 0.692   
                
                invisible(plot(roc(factor(ifelse(thinDatDay$Used == 1, 1, 0)),
                                   fitted(seDay2)), print.thres = c(.1, .5),
                               col = "red", print.auc = T)) # auc = 0.692  
                
                invisible(plot(roc(factor(ifelse(thinDatDay$Used == 1, 1, 0)),
                                   fitted(playDay)), print.thres = c(.1, .5),
                               col = "red", print.auc = T)) # auc = 0.692      
                
                # it's exacty the same for all of them,
                # which i think tells me the estimates for the fixed effects are essentially unchanged

          
            ## try combining day/night models, just for funzies
                
                play <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + daytime +
                               I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) + # quadratics
                               I(canSt*snowSt) + I(slopeSt*snowSt) + I(northnessSt*snowSt) + # interactions
                               (1|Pack/wolfYr), family = binomial(logit), 
                             control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                             data = thinDat)                     

              
                summary(play)
               
                invisible(plot(roc(factor(ifelse(thinDat$Used == 1, 1, 0)),
                                   fitted(play)), print.thres = c(.1, .5),
                               col = "red", print.auc = T)) # auc = 0.717, oooh
                
                binnedplot(fitted(play), residuals(play, type = "pearson"), main = "pearson - all times")
                # hm how much better is this...
                
                par(mfrow = c(3, 1))
                binnedplot(fitted(play), residuals(play, type = "pearson"), main = "pearson - all times")
                binnedplot(fitted(playDay), residuals(playDay, type = "pearson"), main = "pearson - day")
                binnedplot(fitted(playNight), residuals(playNight, type = "pearson"), main = "pearson - night")
                par(mfrow = c(1,1))
                # doesn't look that much better
                # although it's now missing some daynight interactions...
                # so let's start a new model selection game.
                # we have selected our random effects structure, great
                # and we have determined that overdispersion isn't something we need to stress about, yay
                # so we are now going to specify the shit out of the most ridiculous global model we can think of
                # and if that gets weird then we'll go back to the day/night separate models
                
                
                
                
                
            ## NEVER MIND I'M DUMB
                
                ## just learned via actually reading things that 
                ## overdispersion isn't identifiable for binary binomials,
                ## so i don't need to worry about this
                ## (see bolker glmm faq and https://stats.stackexchange.com/questions/230721/how-to-check-overdispersion-of-binomial-glmms-lme4-package)
                
                

            # send diagnostics of whatever models you find best to owen, if you need to ask about fit etc
            # send preliminary covariate plots to arthur
            
            
            
        
                        
            


            
            # there's a crazy snow outlier in both day and night; need to figure that out
            
            
            
        #### roc ####
            
            install.packages(c("pROC", "plotROC"))
            library(pROC); library(plotROC)
            
            # full "overspecified" model
            invisible(plot(roc(factor(ifelse(datDay$Used == 1, 1, 0)),
                               fitted(fullDay)), print.thres = c(.1, .5),
                           col = "red", print.auc = T)) # auc = 0.689
            
            # model without interaction terms
            invisible(plot(roc(factor(ifelse(datDay$Used == 1, 1, 0)),
                               fitted(playDay)), print.thres = c(.1, .5),
                           col = "red", print.auc = T)) # auc = 0.686 
            
            # model without quadratics
            invisible(plot(roc(factor(ifelse(datDay$Used == 1, 1, 0)),
                               fitted(globDay)), print.thres = c(.1, .5),
                           col = "red", print.auc = T)) # auc = 0.666 heh               
            
            
            
             
################################################################################################## #  
  
    
    
### ### ### ###  ### ### ### ### ### 
####   | COVARIATE SELECTION |  ####
### ### ### ###  ### ### ### ### ### 
            
            

    #### Global models - day and night ####
            
            
            
      # choose canopy cover, landcover type, or both
            
        
        ## day ##
            
          # models
          modDayCan <- glmer(Used ~ 1 + canSt + slopeSt + elevSt + northnessSt + snowSt
                             + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                             + canSt:snowSt + slopeSt:snowSt + northnessSt:snowSt + (1|Pack/wolfYr), 
                             family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                             data = thinDatDay)
          modDayLc <- glmer(Used ~ 1 + lcClass + slopeSt + elevSt + northnessSt + snowSt
                             + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                             + slopeSt:snowSt + northnessSt:snowSt + (1|Pack/wolfYr), 
                             family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                             data = thinDatDay)
          modDayBoth <- glmer(Used ~ 1 + canSt + lcClass + slopeSt + elevSt + northnessSt + snowSt
                             + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                             + canSt:snowSt + slopeSt:snowSt + northnessSt:snowSt + (1|Pack/wolfYr), 
                             family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                             data = thinDatDay)
          
          # compete with bic
          BIC(modDayCan, modDayLc, modDayBoth) # both > canopy > landcover
          
          # check auc
          invisible(plot(roc(factor(ifelse(thinDatDay$Used == 1, 1, 0)),
                             fitted(modDayCan)), print.thres = c(.1, .5),
                         col = "red", print.auc = T)) # 0.689 canopy (0.692 for both)
          
          # check residual plot
          binnedplot(fitted(modDayCan), residuals(modDayCan, type = "pearson"), main = "day - canopy only")        
          
          # conclude that it's best to have both canopy and landcover in day model
        
        
        ## night ##  
        
          # models
          modNightCan <- glmer(Used ~ 1 + canSt + slopeSt + elevSt + northnessSt + snowSt
                             + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                             + canSt:snowSt + slopeSt:snowSt + northnessSt:snowSt + (1|Pack/wolfYr), 
                             family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                             data = thinDatNight)
          modNightLc <- glmer(Used ~ 1 + lcClass + slopeSt + elevSt + northnessSt + snowSt
                             + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                             + slopeSt:snowSt + northnessSt:snowSt + (1|Pack/wolfYr), 
                             family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                             data = thinDatNight) 
          modNightBoth <- glmer(Used ~ 1 + canSt + lcClass + slopeSt + elevSt + northnessSt + snowSt
                             + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                             + canSt:snowSt + slopeSt:snowSt + northnessSt:snowSt + (1|Pack/wolfYr), 
                             family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                             data = thinDatNight)        
          
          # compete with bic
          BIC(modNightCan, modNightLc, modNightBoth) # both > canopy > landcover
           
              
          # check auc
          invisible(plot(roc(factor(ifelse(thinDatNight$Used == 1, 1, 0)),
                             fitted(modNightCan)), print.thres = c(.1, .5),
                         col = "red", print.auc = T)) # 0.747 canopy (0.752 for both)
          
          # check residual plot
          binnedplot(fitted(modNightCan), residuals(modNightCan, type = "pearson"), main = "night - canopy only")        
          
          # conclude that t's best to have both canopy and landcover in night model 
          
          
        ## check residual plots against covariates (created above) ##
          
          ## see notes in deardiary
          
        ## work through online tutorial to assess model fit

          
        caret::confusionMatrix(factor(as.character(ifelse(fitted(modDayBoth) > 0.5, "Yes", "No"))), 
                        factor(ifelse(thinDatDay$Used == 1, "Yes", "No")), positive = "Yes") # accuracy = 63.98%          
              
        caret::confusionMatrix(factor(as.character(ifelse(fitted(modNightBoth) > 0.5, "Yes", "No"))), 
                        factor(ifelse(thinDatNight$Used == 1, "Yes", "No")), positive = "Yes") # accuracy = 69.11%   
        
        
        ## look at residuals 
        
        # day - whole model
        binnedplot(
          fitted(modDayBoth),
          residuals(modDayBoth, type = "response"), 
          main = "day - full model")
        
        # day - per covariate
        binnedplot(thinDatDay$canSt, residuals(modDayBoth, type = "response"), main = "day - canSt")
        binnedplot(thinDatDay$slopeSt, residuals(modDayBoth, type = "response"), main = "day - slopeSt")
        binnedplot(thinDatDay$elevSt, residuals(modDayBoth, type = "response"), main = "day - elevSt")
        binnedplot(thinDatDay$northnessSt, residuals(modDayBoth, type = "response"), main = "day - northnessSt")
        binnedplot(thinDatDay$snowSt, residuals(modDayBoth, type = "response"), main = "day - snowSt")

        

        ## partial residual plots, no idea what these really are
        install.packages("effect") # not avail for this r version
        install.packages("remef") # supposedly this isn't avail either
        # last time this happened i closed and restarted and the package worked
        
            
            
            
            
            
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### ### ### ###
####   | QUICK RECHECK OF RANDOM EFFECT SELECTION |  ####
### ### ### ### ### ### ### ### ### ### ### ### ###            
        
        ## day ##
            
          # models
          tDay1 <- glmer(Used ~ 1 + canSt + slopeSt + elevSt + northnessSt + snowSt
                             + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                             + canSt:snowSt + slopeSt:snowSt + northnessSt:snowSt + (1|Pack/wolfYr), 
                             family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                             data = thinDatDay)
          tDay2 <- glmer(Used ~ 1 + lcClass + slopeSt + elevSt + northnessSt + snowSt
                             + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                             + slopeSt:snowSt + northnessSt:snowSt + (1|Pack), 
                             family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                             data = thinDatDay)
          tDay3 <- glmer(Used ~ 1 + canSt + lcClass + slopeSt + elevSt + northnessSt + snowSt
                             + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                             + canSt:snowSt + slopeSt:snowSt + northnessSt:snowSt + (1|wolfYr), 
                             family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                             data = thinDatDay)  
          BIC(tDay1, tDay2, tDay3)
          # oh this is new and interesting. this supports tDay3, just wolfYr
          AIC(tDay1, tDay2, tDay3) # ditto
          
          # residuals
          binnedplot(fitted(tDay3), residuals(tDay3, type = "response"), main = "day - full model")    
          
          # confusion matrix
          confusionMatrix(factor(as.character(ifelse(fitted(tDay3) > 0.5, "Yes", "No"))), 
                          factor(ifelse(thinDatDay$Used == 1, "Yes", "No")), positive = "Yes") 
          # accuracy = 63.98% (same as model with nested RE)
          
          # roc on
          invisible(plot(roc(factor(ifelse(thinDatDay$Used == 1, 1, 0)), fitted(tDay3)), 
                         print.thres = c(.1, .5), col = "red", print.auc = T)) # auc = 0.692, same
          
          # so the wolfYr model comes out on top because of its reduced complexity
          # but it doesn't change actual estimates much; fits the data about the same amount
          # so you need to decide biologically i guess
          # and biologically i think it's important to include pack. so there.
                        
            
            
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### ### ### ###
####   | COVARIATE SELECTION - OLD |  ####
### ### ### ### ### ### ### ### ### ### ### ### ###
            
                
      #### attempts to use hos&leme's 7 steps of variable selection ####
                
       #### day ##
            

            
            # first remove canopy (p = 0.06, highest  value)
            day1 <- glmer(Used ~ 1 + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                               I(slopeSt*snowSt) + I(northnessSt*snowSt) + (1|Pack/wolfYr), 
                             family = binomial(logit), data = datDay,
                             nAGQ = 0, control = glmerControl(optimizer = "nloptwrap"))
            AIC(globDay, day1) # worse
            
            # remove canopy and slope (p = 0.02, next highest)
            day2 <- glmer(Used ~ 1 + lcClass + elevSt + northnessSt + snowSt + 
                               I(slopeSt*snowSt) + I(northnessSt*snowSt) + (1|Pack/wolfYr), 
                             family = binomial(logit), data = datDay,
                             nAGQ = 0, control = glmerControl(optimizer = "nloptwrap"))      
            AIC(globDay, day2) # yep also worse
            
            # maybe consider quadratics for some of these? like elev and slope, really
            
                ## hm ok the global model seems best, whichis suspicious but what can ya do?
                ## i may need to be considering interaction terms, though. Next step is to ponder those.
                ## maybe read owen's paper because apparently residuals are a good way to determine whether you need these
                
                

            day3 <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                             (1|Pack/wolfYr), 
                             family = binomial(logit), data = datDay,
                             nAGQ = 0, control = glmerControl(optimizer = "nloptwrap"))       
            summary(day3)
            
                 
                
                
            
################################################################################################## #  
            
################################################################################################## #  
  
            
################################################################################################## #  
  
    
    
### ### ### ### ### ### ##
####   | OG CODE |  ####
### ### ### ### ### ### ### 

################################################################################################## #  
            
    
    
### ### ### ### ### ### ### ### ### ### ### ### ###
####   | DETERMINE RANDOM EFFECTS STRUCTURE |  ####
### ### ### ### ### ### ### ### ### ### ### ### ###


        ## day ##
        
            # specify models #
                
            dNoRE <- glm(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                           I(slopeSt*snowSt) + I(northnessSt*snowSt), 
                         family = binomial(logit), data = datDay)
            
            dPackRE <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                              I(slopeSt*snowSt) + I(northnessSt*snowSt) + (1|Pack),
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
            globDay <- dNestRE
           
            
             
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
            globNight <- nNestRE
            
            
################################################################################################## #  
                
    
### ### ### ### ### ### ### ### ### ## 
####   | ASSESS FIXED EFFECTS  |  ####
### ### ### ### ### ### ### ### ### ## 

            
        #### specify models (if not run in full above) ####
            
            globDay <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                   I(slopeSt*snowSt) + I(northnessSt*snowSt) + (1|Pack/wolfYr), 
                 family = binomial(logit), data = datDay,
                 nAGQ = 0, control = glmerControl(optimizer = "nloptwrap")) 
            
            globNight <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                               I(slopeSt*snowSt) + I(northnessSt*snowSt) + (1|Pack/wolfYr), 
                             family = binomial(logit), data = datNight,
                             nAGQ = 0, control = glmerControl(optimizer = "nloptwrap"))      
            
            
            
        #### plot binned residuals ####    
            

            # calculate scaled residuals
              
              simulDay <- DHARMa::simulateResiduals(fittedModel = globDay, n = 100) # increase n later 
              simulNight <- DHARMa::simulateResiduals(fittedModel = globNight, n = 100) # to 1000+
              
            
            # plot residuals per covariate 
            
              ## day
              DHARMa::plotResiduals(datDay$canSt, simulDay$scaledResiduals, main = "can")
              DHARMa::plotResiduals(datDay$slopeSt, simulDay$scaledResiduals, main = "slope") 
              # quadratic maybe appropriate here
              DHARMa::plotResiduals(datDay$elevSt, simulDay$scaledResiduals, main = "elev")
              # quadratic may be helpful for elev too
              DHARMa::plotResiduals(datDay$northnessSt, simulDay$scaledResiduals, main = "northness")
              DHARMa::plotResiduals(datDay$snowSt, simulDay$scaledResiduals, main = "snow")
              DHARMa::plotResiduals(datDay$lcClass, simulDay$scaledResiduals, main = "landcover")
              
              ## night
              DHARMa::plotResiduals(datNight$canSt, simulNight$scaledResiduals, main = "can")
              DHARMa::plotResiduals(datNight$slopeSt, simulNight$scaledResiduals, main = "slope") 
              # quadratic maybe appropriate here
              DHARMa::plotResiduals(datNight$elevSt, simulNight$scaledResiduals, main = "elev")
              # quadratic may be helpful for elev too
              DHARMa::plotResiduals(datNight$northnessSt, simulNight$scaledResiduals, main = "northness")
              DHARMa::plotResiduals(datNight$snowSt, simulNight$scaledResiduals, main = "snow")
              DHARMa::plotResiduals(datNight$lcClass, simulNight$scaledResiduals, main = "landcover")

                
                
            
        #### start with global model that includes all reasonable interactions and quadratics ####
              
              fullDay <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                                 I(slopeSt*slopeSt) + I(elevSt*elevSt) + # quadratics
                                 I(canSt*snowSt) + I(slopeSt*snowSt) + I(northnessSt*snowSt) + # interactions
                                 (1|Pack/wolfYr), family = binomial(logit), 
                               control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                               data = datDay)  
              
              fullNight <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                                 I(slopeSt*slopeSt) + I(elevSt*elevSt) + # quadratics
                                 I(canSt*snowSt) + I(slopeSt*snowSt) + I(northnessSt*snowSt) + # interactions
                                 (1|Pack/wolfYr), family = binomial(logit), 
                               control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                               data = datNight)  
              
              # hm apparently everything's important, except slope at night
              
            # check residual plots  
            arm::binnedplot(fitted(fullDay),resid(fullDay))
            # jfc this is awful
            arm::binnedplot(fitted(globDay),resid(globDay))
            # but is a major improvement so ive got that going for me which is nice

            
            # check roc before you bug poor owen again

            
            # full "overspecified" model
            invisible(plot(roc(factor(ifelse(datDay$Used == 1, 1, 0)),
                               fitted(fullDay)), print.thres = c(.1, .5),
                           col = "red", print.auc = T)) 
            # auc = 0.689, not ideal but not the worst thing
            # could argue that as generalists we don't expect to predict much of wolf distirbutino
            # based on environment alone
            
            
            # full "overspecified" model
            invisible(plot(roc(factor(ifelse(datNight$Used == 1, 1, 0)),
                               fitted(fullNight)), print.thres = c(.1, .5),
                           col = "red", print.auc = T)) # auc = 0.755, nice!             
            
            # check predictions against observations
            caret::confusionMatrix(factor(as.character(ifelse(fitted(fullDay) > 0.5, "Yes", "No"))), 
                                   factor(ifelse(datDay$Used == 1, "Yes", "No")))$table
            # model tends to predict 'no' more so than yes
            # not sure if this is just an artifact of selecting >1 unused point for every used
            # which then overrepresents no in comparison to yes?
            # i'm gonna see what happens if i use a lower probability cutoff for funzies
            caret::confusionMatrix(factor(as.character(ifelse(fitted(fullDay) > 0.3, "Yes", "No"))), 
                                   factor(ifelse(datDay$Used == 1, "Yes", "No")))$table     
            # try thinning data to 1:1 ratio and rerunning the model as a last resort
            # then i'll check residuals against predictors
            # then i'll ask owen wtf
            
            used <- filter(modDat, Used == 1)
            unused <- filter(modDat, Used == 0)
            nrow(unused)/nrow(used) # selected at 5:1 ratio initially
            unusedThinned <- unused %>%
              group_by(wolfYr) %>%
              # randomly select 1/5 of the unused locations for each individual
              sample_frac(size = 0.20, replace = FALSE) %>%
              ungroup()
            thinDat <- rbind(used, unusedThinned)
            
            thinDatDay <- filter(thinDat, daytime == "day")
            thinDatNight <- filter(thinDat, daytime == "night")
            
            thinDay <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                                 I(slopeSt*slopeSt) + I(elevSt*elevSt) + # quadratics
                                 I(canSt*snowSt) + I(slopeSt*snowSt) + I(northnessSt*snowSt) + # interactions
                                 (1|Pack/wolfYr), family = binomial(logit), 
                               control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                               data = thinDatDay)  
            
            thinNight <- glmer(Used ~ 1 + canSt + slopeSt + lcClass + elevSt + northnessSt + snowSt + 
                                 I(slopeSt*slopeSt) + I(elevSt*elevSt) + # quadratics
                                 I(canSt*snowSt) + I(slopeSt*snowSt) + I(northnessSt*snowSt) + # interactions
                                 (1|Pack/wolfYr), family = binomial(logit), 
                               control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                               data = thinDatNight) 

            

            # binned residual plots
            par(mfrow = c(2, 1))
            # day
            arm::binnedplot(fitted(fullDay), residuals(fullDay, type = "pearson"), main = "full model - day")
            arm::binnedplot(fitted(thinDay), residuals(thinDay, type = "response"), main = "thinned model - day")  
            # night
            arm::binnedplot(fitted(fullNight), resid(fullNight), main = "full model - Night")
            arm::binnedplot(fitted(thinNight),resid(thinNight), main = "thinned model - Night")  
            par(mfrow = c(1, 1))
            
            # roc curves
            invisible(plot(roc(factor(ifelse(thinDatDay$Used == 1, 1, 0)),
                               fitted(thinDay)), print.thres = c(.1, .5),
                           col = "red", print.auc = T)) # AUC = 0.688, only worse by 0.001
            invisible(plot(roc(factor(ifelse(thinDatNight$Used == 1, 1, 0)),
                               fitted(thinNight)), print.thres = c(.1, .5),
                           col = "red", print.auc = T)) # AUC = 0.753, only worse by 0.002

            # confusion matrices
            caret::confusionMatrix(factor(as.character(ifelse(fitted(thinDay) > 0.5, "Yes", "No"))), 
                                   factor(ifelse(thinDatDay$Used == 1, "Yes", "No")))$table            
            caret::confusionMatrix(factor(as.character(ifelse(fitted(thinNight) > 0.5, "Yes", "No"))), 
                                   factor(ifelse(thinDatNight$Used == 1, "Yes", "No")))$table                
            
            # residuals against predictors 
            
            
            
################################################################################################## #  
                
    
### ### ### ### ### ### ### ### ### ## 
####   | WHY NO 2019 WOLVES?!  |  ####
### ### ### ### ### ### ### ### ### ## 
            
            z <- read.csv("wolflLocs-Used.csv")
            unique(z$Year)
            # shit it's fucked up in the OG code, now i get to start over.
                         
            
################################################################################################## #  
                
    
### ### ### ### ### ### ### ### ### ## 
####   | RANK DEFICIENCY  |  ####            
### ### ### ### ### ### ### ### ### ## 
            
    ## checking it out with the simplest version model i can easily find
    ## r plz fix tabs jfc
    ## using combo of info from
    ## https://stackoverflow.com/questions/37090722/lme4lmer-reports-fixed-effect-model-matrix-is-rank-deficient-do-i-need-a-fi
    ## https://stackoverflow.com/questions/38766155/rank-deficiency-warning-mixed-model-lmer
    fixFormula <- datDay$Used ~ datDay$lcClass + datDay$canSt + datDay$slopeSt 
            + datDay$elevSt + datDay$northnessSt + datDay$snowSt
                     + I(datDay$slopeSt^2) + I(datDay$elevSt^2) + I(datDay$northnessSt^2) 
                     + datDay$snowSt:datDay$canSt + datDay$snowSt:datDay$slopeSt 
            + datDay$snowSt:datDay$northnessSt + datDay$snowSt:datDay$elevSt
                     + datDay$snowSt:I(datDay$slopeSt^2) + datDay$snowSt:I(datDay$elevSt^2) 
                     + datDay$snowSt:I(datDay$northnessSt^2) + datDay$hunt*datDay$can
    X <- model.matrix(fixFormula, datDay) 
    X
    caret::findLinearCombos(X)
    # no linear combos found
    
    # just realized i was using unstandardized canopy everywhere, dumb. fixed; rerunning
    
    #### model-specific investigation
    
    # first issue: quadratic, predictable feedgrounds (in disturbance shield)
    d6 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                 + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2))
    
    # next issue, quadratic, predictable feedgrounds
    d10 <- update(envtDay, . ~ . + distRdSt + recClass + activeFeedSt + I(distRdSt^2) + I(activeFeedSt^2))    
    

    # might be quadratic predictable because there wasn't an issue with quadratic all feedgrounds
    # see whether model d12 has an issue...
    # oh yeah i think what I did was ake distActive be 0 if the closest feedground was a semi-active one
    # that was probably a bad thing to do numberically-wise (and possibly biologically-wise)
    
    # mmk yep it's the quadratic with predictable feedgrounds that's the issue. 
    # which is alright by me; not sure how to fix that covariate but will make note to ponder later today
    
    # next issues
    d14 <- update(envtDay, . ~ . + distRdSt + distStrucSt + motoUse + activeFeedSt
     d18 <- update(envtDay, . ~ . + distRdSt + motoUse + activeFeedSt + I(distRdSt^2) + + I(activeFeedSt^2))
     
             + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2))    
    
    # another stupid issue, i'd used activeFeed as a binary yes/no rather than a distance
    # bc initial idea was to use it as an interaction
    # instead i rewrote the code to calc distance to a) closest feedground, and b) closest active feedground
    # so this is now fixed
    
    
################################################################################################## #  
                
    
    
### ### ### ### ### ### ### ### ### ## 
####   | MODEL COMPARISON  |  ####            
### ### ### ### ### ### ### ### ### ##     
    
    #### prepping model comparison code while the models are running
    
      ## code to use
      c(1, 4, 7 ,3 ,1, 6 ,3, 2)
      sort(as.numeric(c(1, 4, 7 ,3 ,1, 6 ,3, 2)))
      ms <- c("d10", "d34", "d54")
      d10 <- "a"; d34 <- "b"; d54 <- "f"; d5 <- "e"
      get("d10")
      get(ms); ms
      lapply(ms, 1, FUN = get()) # newp
      mz <- list(c("d10", "d34", "d54"))
      lapply(mz, 1, FUN = get()) # lots of newps
      mm <- matrix(c("d10", "d34", "d54"))
      apply(mm, 1, get) # oh. dur.
      apply(ms, 1, get) # still no, so doesnt work with just the characters
      apply(mz, 1, get) # list not quickly doable either so just use matrix. oh wait
      lapply(mz, get) # newp
      lapply(mz, 1, get) # newp ok move on
      z <- matrix(ms)
      apply(z, 1, get)
      

      
      ## compete with AICc - day ##
      
      # create blank list to store models in
      modsDay <- list()
      # identify all model names (d1 through d191)
      modNamesDay <- paste0("d", 1:191)
      # store the models in the list
      for (i in 1:length(modsDay)) { modsDay[[i]] <- get(modNamesDay[[i]]) }
      # create a dataframe of aicc results...
      aicDay <- data.frame(aictab(cand.set = modsDay, modnames = modNamesDay))
      # ...sorted from smallest to largest aicc value...
      aicDay <- sort(aicDay, as.numeric(AICc))
      # .. and export it
      write.csv(aicDay, "aic-day.csv", row.names=F)
      # store and export subset of moderately-supported models (deltaAICc < 4)
      aic4Day <- subset(aictab, Delta_AICc < 4.0); aic4Day <- droplevels(aic4Day)
      write.csv(aic4Day, "aic4-day.csv", row.names=F)
      # store and export subset of best-supported models (deltaAICc < 2)
      aic2Day <- subset(aictab, Delta_AICc < 2.0); aic2Day <- droplevels(aic2Day)
      write.csv(aic2Day, "aic2-day.csv", row.names=F)  
      # view top-supported models
      topDayMat <- matrix(aic2Day$Modname)
      # print summaries of all (prob figure out something more elegant here)
      apply(topDayMat, 1, get)
      
      
      ## compete with AICc - night ##
      
      # create blank list to store models in
      modsNight <- list()
      # identify all model names (d1 through d191)
      modNamesNight <- paste0("n", 1:191)
      # store the models in the list
      for (i in 1:length(modsNight)) { modsNight[[i]] <- get(modNamesNight[[i]]) }
      # create a dataframe of aicc results...
      aicNight <- data.frame(aictab(cand.set = modsNight, modnames = modNamesNight))
      # ...sorted from smallest to largest aicc value...
      aicNight <- sort(aicNight, as.numeric(AICc))
      # .. and export it
      write.csv(aicNight, "aic-night.csv", row.names=F)
      # store and export subset of moderately-supported models (deltaAICc < 4)
      aic4Night <- subset(aictab, Delta_AICc < 4.0); aic4Night <- droplevels(aic4Night)
      write.csv(aic4Night, "aic4-night.csv", row.names=F)
      # store and export subset of best-supported models (deltaAICc < 2)
      aic2Night <- subset(aictab, Delta_AICc < 2.0); aic2Night <- droplevels(aic2Night)
      write.csv(aic2Night, "aic2-night.csv", row.names=F)  
      # view top-supported models
      topNightMat <- matrix(aic2Night$Modname)
      # print summaries of all (prob figure out something more elegant here)
      apply(topNightMat, 1, get)
      
      
      
      #### competing models in small batches because memory constraints are a real thing ####
      
      
      # get current batch of models from environment?
      get("d1") # need quotes
      testy <- "does this work?"
      get("testy")
      get(grep("testy"), ls()) # way hoff
      ls()["testy"] # NA
      ls(envir = .GlobalEnv, pattern = "testy") # "testy"
      get(ls(envir = .GlobalEnv, pattern = "testy")) # woo!
      testy1 <- "and also this?"
      get(ls(envir = .GlobalEnv, pattern = "testy")) # newp only testy
      get(ls(envir = .GlobalEnv, pattern = "testy.")) # only testy1
      get(ls(envir = .GlobalEnv, pattern = "t.")) # um somehow this killed r?!
      get(grep(pattern = "t.", x = globalenv())) # globalenv() is an issue
      get(ls(envir = .GlobalEnv, pattern = "test..")) 
      # ok i tried several iterations of the above and only seem to be able to get one thing at a time
      ls("testy")
      # maybe try removing first
      # bc it's easier and you can probably scale up and apply that to your get() stuff
      rm(list = c("testy", "testy1"))
      
      testy <- "does this work?"
      testy1 <- "and also this?"
      
      rm(list = ls(pattern = "test")) # i just came
      t1 <- d1; t2 <- d2; t3 <- d3; t4 <- d4
      rm(list = ls(pattern = "t[:digit:]")) # newp, i never could get digit to work anyway
      rm(list = ls(pattern = "t[0:9]")) # newp
      rm(list = ls(pattern = "t[0:9].")) # newp
      rm(list = ls(pattern = "t[0:9]*")) # oh shit that cleared everything but the models bc you didn't say just at start of name
      t1 <- d1; t2 <- d2; t3 <- d3; t4 <- d4
      rm(list = ls(pattern = "^t[0:9]*")) # *angelic harp music*

      # k now apply that to your get() plan
      get(list = ls(pattern = "^d[0:9]*")) # shit. oh didn't tell it where?
      get(list = ls(envir = .GlobalEnv, pattern = "^d[0:9]*")) # nope that wasn't it
      get(ls(envir = .GlobalEnv, pattern = "^d[0:9]*")) # dammit still just gets one, how to make it a list?
      
      # maybe combine with above aic code
      
      # start with hard-coded model numbers, if that works then move to list all m+digits
      # create blank list to store models in
      modsDay <- list()
      # identify all model names (d1 through d191)
      modNamesDay <- paste0("d", 1:4)
      # store the models in the list
      for (i in 1:length(modsDay)) { modsDay[[i]] <- get(modNamesDay[i]) }
      modsDay[1]
      modsDay[2]
      modsDay[[1]]
      str(modsDay) # fuck it's a list of 1
      modNamesDay[1]
      modNamesDay[2]
      length(modsDay) # oh. 
      length(modNamesDay)

      for (i in 1:length(modNamesDay)) { modsDay[[i]] <- get(modNamesDay[i]) } # oook now we're cooking
      
      # but still can't figure out the list thing
      # goal: list the models in the environment based on their naming convention of d + number(s)
      ls(envir = .GlobalEnv, pattern = "^d[0:9]*") # OH that works but also includes datDay, how to require the number?
      ls(envir = .GlobalEnv, pattern = "^d[0:9]") # as expected...
      # oh turns out * means 0 or more times, try +
      ls(envir = .GlobalEnv, pattern = "^d[0:9]+") # what the shit
      ls(envir = .GlobalEnv, pattern = "^d+[0:9]") # eff
      ls(envir = .GlobalEnv, pattern = "^d([0:9])+") # dammmmmmmmmmmitttttttttt
      ls(envir = .GlobalEnv, pattern = "(^d)([0:9])*")
      
      t11 <- "test"
      ls(envir = .GlobalEnv, pattern = "(^d)([0:9])\2")       # no
      ls(envir = .GlobalEnv, pattern = "(^d)([0:9])\\2*") # no
      ls(envir = .GlobalEnv, pattern = "(^d)([0:9])\2") # no
      # oh fuck this i'll just change the naming convention
      # gah but i'm so clooooose
      ls(envir = .GlobalEnv, pattern = "^[d][0:9]{1,5}")
      ls(envir = .GlobalEnv, pattern = "^d[0:9]{1,5}")
      ls(envir = .GlobalEnv, pattern = "^d[0-9]{1,5}") # AHHHHHHHHHHHHHHHHHHH YAYYYYYYYYYYYYY
      
      # store supported omdels with new name
      
      # but pretend more than one model supported to make sure it works with multiple
      aic2Day <- subset(aicDay, Delta_AICc < 6000)
      aic2Day <- droplevels(aic2Day)
      aic2Day$Modnames <- as.character(aic2Day$Modnames) 
      unique(aic2Day$Modnames)
      
      topLSMs <- list()
      # list model names
      topLSMnames <- unique(aic2Day$Modnames)
      # store the models in the list
      for (i in 1:length(topLSMnames)) { topLSMs[[i]] <- get(topLSMnames[i]) }
      topLSMs[1]
      topLSMs[2]
      # i'm proud of me
      
    #### combine lists for final test ####
      
      z1 <- list("a", "b", "c")
      z2 <- list("aa", "bc")
      z3 <- list("f")
      z <- c(z1, z2, z3)
      # didn't actually expect that to work

      

################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### 
####   | REMOVING MAIN EFFECT OF HUNTING|  ####
### ### ### ### ### ### ### ### ###     
    
      # does it work to just add "- hunt" to models so don't have to retype them all?
      t1 <- update(d75, . ~ . - prevHunt)
      d75 <- update(envtNight, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                   + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                   + prevHunt*distRdSt + prevHunt*distStrucSt + prevHunt*recClass + prevHunt*distFeedSt
                   + prevHunt*I(distRdSt^2) + prevHunt*I(distStrucSt^2) + prevHunt*I(distFeedSt^2)
                   + prevHunt*canSt - prevHunt)        
      # yes, awesome, will do this at home
      
      
      
      ## checking model fit to data of new top model without main effect
      ## note to self: also ponder why the top model may have changed due to this
      ## never mind did that in the normal code, duh
      
      ## now let's figure out the plot showing the continuous x continuous squared interaction
      
      pdR1 <- ggplot(filter(d, tSinceHunt > 0), aes(x = distRd, y = prWolf, group = tSinceHunt)) +
        stat_smooth(aes(color = tSinceHunt), method = "lm", formula = y ~ poly(x, 2)) +
        labs(title = "Distance to motorized route - Daytime") +
        scale_colour_gradient(name = "tSinceHunt", low = "blue", high = "red")
      pdR1
      pdR0 <- ggplot(d, aes(x = distRd, y = prWolf, group = tSinceHunt)) +
        stat_smooth(aes(color = tSinceHunt), method = "lm", formula = y ~ poly(x, 2)) +
        labs(title = "Distance to motorized route - Daytime") +
        scale_colour_gradient(name = "tSinceHunt", low = "blue", high = "red")
      pdR

      
         # canopy (previous year hunting y/n)
        pCan <- ggplot(d, aes(x = can, y = prWolf, colour = prevHunt)) +
          stat_smooth(aes(linetype = prevHunt), method = "lm", formula = y ~ poly(x, 2)) +
          # coord_cartesian(ylim = c(0, 1)) +
          labs(title = "Canopy cover")     
    
    
    

################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### ### ###
####   | REMOVING MAIN EFFECT OF HUNTING|  ####
### ### ### ### ### ### ### ### ### ### ### ###
        
        
 # getting serious about looking at the actual effect 
        
    #### Raw data ####
    
      modDatRaw <- read.csv("modDat.csv")

    
    #### Format covariates ####
    
      modDat <- modDatRaw %>% mutate(
        datetime = ymd_hms(datetime),
        # handle datetimes and dates of course
        Date = ymd(Date),
        # standardize continuous covariates
        slopeSt = (slope - mean(slope))/sd(slope),
        elevSt = (elev - mean(elev))/sd(elev),
        northnessSt = (northness - mean(northness))/sd(northness),
        snowSt = (snowCm - mean(snowCm))/sd(snowCm),
        canSt = (can - mean(can))/sd(can),
        distRdSt = (distRd - mean(distRd))/sd(distRd),
        distStrucSt = (distStruc - mean(distStruc))/sd(distStruc),
        distFeedSt = (distFeed - mean(distFeed))/sd(distFeed),
        activeFeedSt = (distFeedActive - mean(distFeedActive))/sd(distFeedActive),
        # order landcover from most to least available
        lcClass = factor(lcClass, levels = c("Forest", "Shrub", "Herbaceous", "Riparian", "NoVeg")),
        # use open recreation as baseline; reorder for more intuitive plot interpretation
        recClass = factor(recClass, levels = c("allOT", "nomotoOT", "noOT", "noRec")),
        # add binary private land designation
        pvt = ifelse(recClass == "noRec", 1, 0),
        # add binary off-trail/no off-trail use designation
        otUse = ifelse(recClass == "noOT", 0, 1),
        # add binary designation for whether moto rec is allowed
        motoUse = ifelse(recClass == "allOT", 1, 0),
        # add binary indicator of whether hunting was allowed that fall
        hunt = ifelse(Year == 2013 | Year == 2014 | Year >= 2018, 1, 0),
        # add binary indication of whether hunting was allowed in the previous year
        prevHunt = ifelse(Year == 2014 | Year == 2015 | Year >= 2019, 1, 0),
        # add time since hunting was first allowed (fall 2012, corresponds to winter 2013)
        tSinceHunt = ifelse(Year - 2013 + 1 < 0, 0, Year - 2013 + 1),
        # add years of continuous hunting (to account for no hunting 2014, 2015, 2016)
        tContHunt = ifelse(Year == 2013, 1,
                           ifelse(Year == 2014, 2,
                                  ifelse(Year >= 2018, Year - 2018 + 1, 0))))

    
    #### split day/night ####
      
      modDatDay <- filter(modDat, daytime == "day")
      modDatNight <- filter(modDat, daytime == "night")   
      
      
    #### specify base environmental model ####
    
      envtDay <- glmer(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                       + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                       + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt + snowSt:elevSt
                       + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                       + (1|Pack/wolfYr), 
                        family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                       data = modDatDay) 

      
    #### specify models with and without binary and continuous hunt covariates ####
      
      # binary hunt included (interactions with quadratic distRd and categorical recClass)
      binIn <- update(envtDay, . ~ . + distRdSt + recClass + I(distRdSt^2) 
                   + hunt*distRdSt + hunt*recClass + hunt*I(distRdSt^2))
      
      # binary hunt excluded (interactions with quadratic distRd and categorical recClass)
      binOut <- update(envtDay, . ~ . + distRdSt + recClass + I(distRdSt^2) 
                   + hunt*distRdSt + hunt*recClass + hunt*I(distRdSt^2) - hunt)      
      
      # continuous hunt included (interactions with quadratic distRd and categorical recClass)
      contIn <- update(envtDay, . ~ . + distRdSt + recClass + I(distRdSt^2) 
                   + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*I(distRdSt^2)) 
      
      # continuous hunt excluded (interactions with quadratic distRd and categorical recClass)
      contOut <- update(envtDay, . ~ . + distRdSt + recClass + I(distRdSt^2) 
                   + tSinceHunt*distRdSt + tSinceHunt*recClass + tSinceHunt*I(distRdSt^2) - tSinceHunt)  
      
      
      
    #### investigate model differences ####
      
      # view model summaries
      summary(binIn); summary(binOut)
      summary(contIn); summary(contOut)
      
      # extract coefficients 
      binInEst <- data.frame(
        Cov = rownames(coef(summary(binIn))),
        EstIN = round(coef(summary(binIn))[, "Estimate"], 3))
      binOutEst <- data.frame(
        Cov = rownames(coef(summary(binOut))),
        EstOUT = round(coef(summary(binOut))[, "Estimate"], 3))
      binaryResults <- binInEst %>%
        full_join(binOutEst, by = "Cov") %>%
        mutate(diff = ifelse(EstOUT - EstIN == 0, NA, "DIFF"))
      
      contInEst <- data.frame(
        Cov = rownames(coef(summary(contIn))),
        EstIN = round(coef(summary(contIn))[, "Estimate"], 3))
      contOutEst <- data.frame(
        Cov = rownames(coef(summary(contOut))),
        EstOUT = round(coef(summary(contOut))[, "Estimate"], 3))
      continuousResults <- contInEst %>%
        full_join(contOutEst, by = "Cov") %>%
        mutate(diff = ifelse(EstOUT - EstIN == 0, NA, "DIFF"))        

      
      
    #### check out hunt covariate diffs bt used and available samples ####           
     
      test <- modDat %>%
        mutate(huntYN = ifelse(hunt == 0, "nothunt", "hunt"))
      table(test$huntYN, test$Used)
      # same proportion hunt for used vs available
      
      testmod <- update(envtDay, . ~  . + hunt)
      summary(testmod)
      # but effect of hunt isn't 0
      
      t <- glmer(Used ~ 1 + hunt + (1|Pack/wolfYr), 
                        family = binomial(logit), control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE), 
                       data = modDat) 
      summary(t)
      t2 <- glm(Used ~ 1 + hunt, family = binomial(logit),data = modDat)
      summary(t2) # ok it's basically 0 for univariate model without the nested random effect
      
      
      
    
      t3 <- glm(Used ~ 1 + lcClass + canSt + slopeSt + elevSt + northnessSt + snowSt
                       + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                       + snowSt:canSt + snowSt:slopeSt + snowSt:northnessSt + snowSt:elevSt
                       + snowSt:I(slopeSt*slopeSt) + snowSt:I(elevSt*elevSt) + snowSt:I(northnessSt*northnessSt)
                       + distRdSt + recClass + I(distRdSt^2) 
                       + hunt*distRdSt + hunt*recClass + hunt*I(distRdSt^2),
                        family = binomial(logit), data = modDat) 
      summary(t3)
      # effect of hunt is *NOT 0* when all the other covariates are included
      # using just a regular glm (no random effects)
      

      # make sure models are doing what you think they are
      # interaction shortcut
      t4b <- glm(Used ~ 1 + hunt*distRdSt + hunt*recClass, family = binomial(logit), data = modDat)
      summary(t4b)
      t4ba <- glm(Used ~ 1 + hunt + distRdSt + recClass + hunt:distRdSt + hunt:recClass , family = binomial(logit), data = modDat)
      summary(t4ba)
      # removing main effect of hunt
      t4b1 <- glm(Used ~ 1 + hunt*distRdSt + hunt*recClass - hunt, family = binomial(logit), data = modDat)
      summary(t4b1)
      t4ba1 <- glm(Used ~ 1 + distRdSt + recClass + hunt:distRdSt + hunt:recClass , family = binomial(logit), data = modDat)
      summary(t4ba1)
      # yup all good there
      
      # investigate with/without main effect of hunt using simpler models
      binIn <- glm(Used ~ 1 + hunt*distRdSt + hunt*recClass, family = binomial(logit), data = modDat)
      binOut <- glm(Used ~ 1 + hunt*distRdSt + hunt*recClass - hunt, family = binomial(logit), data = modDat)
      summary(binIn); summary(binOut)
      
      # actually let's go simpler. 
      
      # binaryHunt*continuousCovariate
      binInCont <- glm(Used ~ 1 + hunt*distRdSt, family = binomial(logit), data = modDat)
      binOutCont <- glm(Used ~ 1 + hunt*distRdSt - hunt, family = binomial(logit), data = modDat)
      summary(binInCont); summary(binOutCont)  
      # these are different models with different aic scores, degrees of freedom
      
      # binaryHunt*categoricalCovariate
      binInCat <- glm(Used ~ 1 + hunt*recClass, family = binomial(logit), data = modDat)
      binOutCat <- glm(Used ~ 1 + hunt*recClass - hunt, family = binomial(logit), data = modDat)
      summary(binInCat); summary(binOutCat)       
      # these have the same aic, dof, etc.
      # so using a categorical predictor just gives different specifications of the same model
      # is that true when hunt is continuous, too?
      # my guess is that this is not true when hunt is continuous.
      
      
      # continuousHunt*continuousCovariate
      cntInCont <- glm(Used ~ 1 + tSinceHunt*distRdSt, family = binomial(logit), data = modDat)
      cntOutCont <- glm(Used ~ 1 + tSinceHunt*distRdSt - tSinceHunt, family = binomial(logit), data = modDat)
      summary(cntInCont); summary(cntOutCont)  
      # these are different models with different aic scores, degrees of freedom
      
      # continuousHunt*categoricalCovariate
      cntInCat <- glm(Used ~ 1 + tSinceHunt*recClass, family = binomial(logit), data = modDat)
      cntOutCat <- glm(Used ~ 1 + tSinceHunt*recClass - tSinceHunt, family = binomial(logit), data = modDat)
      summary(cntInCat); summary(cntOutCat)         
      # ohhhhhh this is something i'm misunderstanding
      # because these models are the same and i thought theyd be different
      
      
      # binaryHunt*continuousCovariate + binaryHunt*categoricalCovariate
      binInContCat <- glm(Used ~ 1 + hunt*distRdSt + hunt*recClass, family = binomial(logit), data = modDat)
      binOutContCat <- glm(Used ~ 1 + hunt*distRdSt + hunt*recClass - hunt, family = binomial(logit), data = modDat)
      summary(binInContCat); summary(binOutContCat)  
      # these are different models with different aic scores, degrees of freedom      
            
      z <- glm(Used ~ 1 + tSinceHunt, family = binomial(logit), data = modDat)
      summary(z)
      # and as expected, diff when interacted with both continuous and categorical
      
      
      #### back at it 2019/09/01
      
      # first let's see how envtDay changes when you add just the main effect of hunt
      # and try to understand what that means mathematically
      # specifically keep an eye on betas.
      
      h <- update(envtDay, . ~ . + hunt)
      AIC(envtDay, h); logLik(envtDay); logLik(h) # hunt doesn't explain appreciably more
      View(data.frame(fixef(envtDay))); View(data.frame(fixef(h)))
      # betas and signs don't change much
      summary(h)
      # effect of hunt isn't important, but it's not 0 either
      # which it shouldn't be bc there aren't equal - 
      # wait are there equal numbers of hunt/no hunt locs? there shouldn't be
      # but the proportion of hunt:no hunt should be equal for used:avail
      # so double check that
      table(modDat$Used, modDat$hunt)
      z <- modDat %>% mutate(h = ifelse(hunt == 0, "noHunt", "Hunt"),
                             t = ifelse(Used == 0, "avail", "used"))
      table(z$t, z$h)
      # yeah ok so there are more locs during nohunt than hunt
      # but ratios are exactly the same bt used and avail
      
      # returning to the models with and without main effects
      # to see exactly how they change
      # my thought is that if the interaction shows the difference between
      # the "base" effect of hunt and the interacted effect
      # that it's not actually relevant to include the base effect
      
      ##
      
      # binaryHunt*categoricalCovariate
      binInCat <- glm(Used ~ 1 + hunt*recClass, family = binomial(logit), data = modDat)
      binOutCat <- glm(Used ~ 1 + hunt*recClass - hunt, family = binomial(logit), data = modDat)
      summary(binInCat); summary(binOutCat)       
      # these have the same aic, dof, etc.
      
      # binaryHunt*continuousCovariate
      binInCont <- glm(Used ~ 1 + hunt*distRdSt, family = binomial(logit), data = modDat)
      binOutCont <- glm(Used ~ 1 + hunt*distRdSt - hunt, family = binomial(logit), data = modDat)
      summary(binInCont); summary(binOutCont)  
      # these are different models with different aic scores, degrees of freedom
      
      ##
      
      # continuousHunt*categoricalCovariate
      cntInCat <- glm(Used ~ 1 + tSinceHunt*recClass, family = binomial(logit), data = modDat)
      cntOutCat <- glm(Used ~ 1 + tSinceHunt*recClass - tSinceHunt, family = binomial(logit), data = modDat)
      summary(cntInCat); summary(cntOutCat)      

      
      # continuousHunt*continuousCovariate
      cntInCont <- glm(Used ~ 1 + tSinceHunt*distRdSt, family = binomial(logit), data = modDat)
      cntOutCont <- glm(Used ~ 1 + tSinceHunt*distRdSt - tSinceHunt, family = binomial(logit), data = modDat)
      summary(cntInCont); summary(cntOutCont)  
      # these are different models with different aic scores, degrees of freedom
   

      
      
      # binaryHunt*continuousCovariate + binaryHunt*categoricalCovariate
      binInContCat <- glm(Used ~ 1 + hunt*distRdSt + hunt*recClass, family = binomial(logit), data = modDat)
      binOutContCat <- glm(Used ~ 1 + hunt*distRdSt + hunt*recClass - hunt, family = binomial(logit), data = modDat)
      summary(binInContCat); summary(binOutContCat)  
      #       
    

################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### ### ### ###
####   | AIC WEIRDNESS WHEN CHECKED RE STRUC|  ####
### ### ### ### ### ### ### ### ### ### ### ###      
      
        # day - calculate and export
        aicDay1 <- data.frame(AIC(noreDay, wDay, pDay, wNpDay), 
                             randomEffect = c("None", "Wolf", "Pack", "WolfInPack"))
        aicDay1 <- aicDay1[order(aicDay1$AIC), ]
        aicDay1$deltaAIC = aicDay1$AIC - min(aicDay1$AIC)
        aicDay1$LL <- c(logLik(noreDay), logLik(wDay), logLik(pDay), logLik(wNpDay))
     
  
        # night - calculate and export
        aicNight1 <- data.frame(AIC(noreNight, wNight, pNight, wNpNight), 
                               randomEffect = c("None", "Wolf", "Pack", "WolfInPack"))
        aicNight1 <- aicNight1[order(aicNight1$AIC), ]
        aicNight1$deltaAIC = aicNight1$AIC - min(aicNight1$AIC)
        aicNight1$LL <- c(logLik(noreNight), logLik(wNight), logLik(pNight), logLik(wNpNight))
           
        # oh i didn't mapt he right LLs to the right models dur
    

        
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### 
####   | CONVERGENCE ISSUE |  ####
### ### ### ### ### ### ### ### ### 
        
        
        # following https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
        
        # check singularity
        z <- getME(d2, "theta")
        x <- getME(d2, "lower")
        min(z[x==0])
        # 0.2681533
        # singularity means some random effects parameters (re = theta) are really close to 0
        # like, < 10^-6. Definitely not 
        rm(z, x)
        
        # restart from previous fit and increase numer fo iterations
        z <- getME(d2, c("theta","fixef"))
        mz <- update(d2, start = z, control=glmerControl(optCtrl=list(maxfun=2e4)))
        # oh. that worked.
        
        summary(mz)
        invisible(plot(roc(factor(ifelse(datDay$Used == 1, 1, 0)), fitted(mz)), 
               print.thres = c(.1, .5), col = "red", print.auc = T)) 
        confusionMatrix(factor(as.character(ifelse(fitted(mz) > 0.5, "Yes", "No"))), 
                        factor(ifelse(datDay$Used == 1, "Yes", "No")), positive = "Yes")  
        binnedplot(fitted(mz), residuals(mz, type = "response"), main = "Day - top model")
        # and the model's not insanely sucky either
        # but restarting seems onerous for all those models so i'd like to 
        # either always do that many iterations to begin with or
        # try a different optimizer
        
        z2 <- update(envtDay, . ~ . + distRdSt + distStrucSt + I(distRdSt^2) + I(distStrucSt^2),
                     control=glmerControl(optCtrl=list(maxfun=2e4)))
        # just doing lots of iterations to begin with didn't fix it
        
        z2 <- update(envtDay, . ~ . + distRdSt + distStrucSt + I(distRdSt^2) + I(distStrucSt^2),
                     control = glmerControl(optimizer = "nloptwrap"))     
        # that actually made it worse, ha ha
        
        
         z2 <- update(envtDay, . ~ . + distRdSt + distStrucSt + I(distRdSt^2) + I(distStrucSt^2),
                     control = glmerControl(optimizer = "bobyqa")) 
         # oh. that worked. i'll just do that for everything i guess.
         
         
         
         
     #### issues after first full day run using the above fix ####
         

          # d111, 114, & 115: tSinceHunt * rd, struc, feed
          # warning messages: maximum number of function evaluations exceeded from bobyqa
            
            # first pass: increase max iterations even more (optCtrl=list(maxfun=4e4))
            # yeah that worked
            # trying with just 3e4 cuz aint nobody got time for that
            
            
          # quadratic, predictable feedgrounds, canopy
          d111 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + activeFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(activeFeedSt^2)
                       +  tSinceHunt*distRdSt +  tSinceHunt*distStrucSt +  tSinceHunt*recClass +  tSinceHunt*activeFeedSt
                       +  tSinceHunt*I(distRdSt^2) +  tSinceHunt*I(distStrucSt^2) +  tSinceHunt*I(activeFeedSt^2)
                       + tSinceHunt*canSt,
                                            control = glmerControl(optimizer = "bobyqa", 
                                            optCtrl=list(maxfun=3e4),
                                            calc.derivs = FALSE))  
          
          # quadratic, all feedgrounds
          d114 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*recClass + tSinceHunt*distFeedSt
                       + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distStrucSt^2) + tSinceHunt*I(distFeedSt^2),
                                            control = glmerControl(optimizer = "bobyqa", 
                                            optCtrl=list(maxfun=3e4),
                                            calc.derivs = FALSE))
          
          # quadratic, all feedgrounds, canopy
          d115 <- update(envtDay, . ~ . + distRdSt + distStrucSt + recClass + distFeedSt
                       + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                       + tSinceHunt*distRdSt + tSinceHunt*distStrucSt + tSinceHunt*recClass + tSinceHunt*distFeedSt
                       + tSinceHunt*I(distRdSt^2) + tSinceHunt*I(distStrucSt^2) + tSinceHunt*I(distFeedSt^2)
                       + tSinceHunt*canSt,
                                            control = glmerControl(optimizer = "bobyqa", 
                                            optCtrl=list(maxfun=3e4),
                                            calc.derivs = FALSE))        
 
          
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### 
####   | CREPUSCULAR |  ####
### ### ### ### ### ### ### ### ###         
         
         
         
         install.packages("suncalc")
         library(suncalc)
         
         
         z <- modDat %>%
           dplyr::rename(date = Date, lat = Latitude, lon = Longitude) 

         test <- getSunlightTimes(data = z,
                    # nightEnd = astronomical. golden hour  = soft light
                    keep = c("nightEnd", "goldenHourEnd",
                    "goldenHour", "night"), tz = "MST")
         # golden hour is ~1/2hr later in AM and 1/2hr earlier in PM; corresponds to :soft: light good for photography
         # i'll use that since it's probably more biologically relevant
        
         z <- cbind(z, test)

         z$modtime <- ifelse(z$datetime > z$nightEnd & z$datetime < z$goldenHourEnd |
                            z$datetime > z$goldenHour & z$datetime < z$night, "crepusc", paste(as.character(z$daytime)))
         
                   
         # need to put this in wolfYr code and randomly select one of each, then you're good to go
         # when you select random effects stucture just use all the data, not sep day/night/crep
         
         
          
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### 
####   | FIXING 1ST PASS MODELS |  ####
### ### ### ### ### ### ### ### ###         
                  
         
        # d66X, d106X, d111Y, d114Y, d115Y, d146X, d186X
         
         # d66, d106, d146, 186: hunt * slope, elev, and aspect
         # prevHunt tSinceHunt tContHunt
         # messages: model matrix rank deficient (yeah kristin this isn't a convergence issue)
          
          d66 <- update(envtDay, . ~ . + hunt:lcClass + hunt:canSt + hunt:slopeSt + hunt:elevSt + hunt:northnessSt
                        + hunt:snowSt + hunt:I(slopeSt^2) + hunt:I(elevSt^2) + hunt:I(northnessSt^2))
       
          d106 <- update(envtDay, . ~ . + hunt:lcClass + hunt:canSt + hunt:slopeSt + hunt:elevSt + hunt:northnessSt
                         + hunt:snowSt + hunt:I(slopeSt^2) + hunt:I(elevSt^2) + hunt:I(northnessSt^2))   
          
          d146 <- update(envtDay, . ~ . + hunt:lcClass + hunt:canSt + hunt:slopeSt + hunt:elevSt + hunt:northnessSt
                         + hunt:snowSt + hunt:I(slopeSt^2) + hunt:I(elevSt^2) + hunt:I(northnessSt^2))
          
          d186 <- update(envtDay, . ~ . + hunt:lcClass + hunt:canSt + hunt:slopeSt + hunt:elevSt + hunt:northnessSt
                         + hunt:snowSt + hunt:I(slopeSt^2) + hunt:I(elevSt^2) + hunt:I(northnessSt^2))
          
            
            # this *does* work when just interacted with canopy but not when interacted with everything else
            # oh i think i may have typed out the model wrong with the interactions. christ. 
            # not sure if i hope that's the issue or not haha
            
            d66b <- update(envtDay, . ~ . + hunt:lcClass + hunt:canSt + hunt:slopeSt + hunt:elevSt + hunt:northnessSt
                         + hunt:snowSt + hunt:I(slopeSt^2) + hunt:I(elevSt^2) + hunt:I(northnessSt^2))
            
            ## OH SHIT the environmental model changed and now has fewer covariates - so i need to remove those from all these fml
            
                envtDay <- glmer(Used ~ 1 + canSt + slopeSt + elevSt + northnessSt + snowSt
                     + I(slopeSt*slopeSt) + I(elevSt*elevSt) + I(northnessSt*northnessSt) 
                     + snowSt:canSt + snowSt:northnessSt + snowSt:elevSt + snowSt:I(elevSt*elevSt) 
                     + (1|Pack), family = binomial(logit), data = modDatDay,
                     control = glmerControl(optimizer = "bobyqa", 
                                            optCtrl=list(maxfun=3e4),
                                            calc.derivs = FALSE)) 
                
                # can 
                # slope AND slope2
                # elev AND elev2
                # northness AND northness2
                # snow
                  # snow:can
                  # snow:northness
                  # snow*elev AND snow*elev2
                
                
                # aic ####
                
                z <- aicDay %>%
                  dplyr::select(AICc, LL) %>%
                  duplicated()
                any(z == TRUE)
                which(z == TRUE)
                zz <- aicDay[which(z == TRUE),]
                View(zz)
                # steal smarter peoples code
                
                d <- aicDay
                indDuplicatedVec <- duplicated(d[,c("AICc", "LL")]) | duplicated(d[,c("AICc", "LL")], fromLast = TRUE)
                myDuplicates <- d[indDuplicatedVec, ]
                View(myDuplicates)
                # hahah duh these are just the models i renumbered and reran
                
                # one more time without those included
                d <- aicDay
                indDuplicatedVec <- duplicated(d[,c("AICc", "LL")]) | duplicated(d[,c("AICc", "LL")], fromLast = TRUE)
                myDuplicates <- d[indDuplicatedVec, ]
                View(myDuplicates)            
                # aaand a typo in the updated hunt:envt models. nice work.
                
                
                
                
          
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### 
####   | RANK DEFICIENCY |  ####
### ### ### ### ### ### ### ### ###         
                                  
    # this happens with models of hunt:environmental covariates only
    # but not with hunt: just canopy
    
    z <-  update(envtDay, . ~ . + hunt*canSt + hunt*slopeSt + hunt*elevSt + hunt*northnessSt
                 + hunt*snowSt + hunt*I(slopeSt^2) + hunt*I(elevSt^2) + hunt*I(northnessSt^2))
    # i'm wondering whether it has to do with the face that elev2 is alread interacted with snow
    # so ill remove that first
    update(envtDay, . ~ . + hunt*canSt + hunt*slopeSt + hunt*elevSt + hunt*northnessSt
                 + hunt*snowSt + hunt*I(slopeSt^2) + hunt*I(northnessSt^2))    
    # better. i think it's actually all the squared interactions.
    summary(z)
    zz <- update(envtDay, . ~ . + hunt*canSt + hunt*slopeSt + hunt*elevSt + hunt*northnessSt
                 + hunt*snowSt)    
    # yeah that fixes it
    summary(zz)    
    # so it's a model complexity issue, i think. Im going to remove those quadratic interactions
                
                
                
################################################################################################## #  
  
    
    
### ### ### ### ### ### ### ### ### 
####   | FEED AVAIL UUUUUGH |  ####
### ### ### ### ### ### ### ### ###                 
                
  

    # ran top of models-day to get envtal model
    summary(modDatDay$feedIn)
    
    # update data to be NA if no feed avail
    newDat <- modDatDay %>%
      mutate(distFeed = ifelse(feedIn == 0, NA, distFeed),
             distFeedSt = (distFeed - mean(distFeed, na.rm = T))/sd(distFeed, na.rm = T))
    summary(newDat$distFeed)
    summary(newDat$distFeedSt)
    summary(modDatDay$distFeedSt)
    
    # update standardized data
    newDat$distFeedSt = (distFeed - mean(distFeed, na.rm = T))/sd(distFeed, na.rm = T)
    
    # update model to the feedground one but only use avail feedground info for it
    newMod <- update(envtDay, . ~ . + distRdSt + distStrucSt + distFeedSt
                 + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                 + hunt*distRdSt + hunt*distStrucSt + hunt*distFeedSt
                 + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2),
               data = newDat)
    # rank deficiency warning
    summary(newMod)
    
    # compare to version with all wolves
    tsa34 <- update(envtDay, . ~ . + distRdSt + distStrucSt + distFeedSt
                 + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                 + hunt*distRdSt + hunt*distStrucSt + hunt*distFeedSt
                 + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2))    
    fixef(newMod)
    fixef(tsa34)
    
    # plot the NA data
    d <- newDat %>%
      mutate(
        prWolf = fitted(newMod),
        model = "Day",
        Hunt  = hunt)    

    

    
    
    
    
    
    
    ################## deleete me              
          
    # day
    d <- modDatDay %>%
      mutate(
        prWolf = fitted(tsa34),
        model = "Day",
        Hunt  = hunt)
    
    ggplot(filter(d, feedIn == 1), aes(x = distFeed, y = Used)) +
        stat_smooth(aes(x = distFeed, y = prWolf, linetype = as.factor(Hunt)),
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(y = "Pr(Use)", title = "Only wolves with feed available")

    
    ggplot(d, aes(x = distFeed, y = Used)) +
        stat_smooth(aes(x = distFeed, y = prWolf, linetype = as.factor(Hunt)),
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(y = "Pr(Use)", title = "All wolves")                
                
                
    ggplot(d, aes(x = distFeed, y = Used)) +
        stat_smooth(aes(x = distFeed, y = prWolf, linetype = as.factor(feedIn)),
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(y = "Pr(Use)", title = "Only wolves with feed available")
    
    
    # try again removing it
    z <- modDatDay %>%
      mutate(distFeed <- ifelse(feedIn == 0, NA, distFeed)) %>%
      rename(distFeedStAll = distFeedSt) %>%
      mutate(distFeedSt = (distFeed - mean(distFeed, na.rm = T))/sd(distFeed, na.rm = T))
    

    t <- update(envtDay, . ~ . + distRdSt + distStrucSt + distFeedSt
                 + I(distRdSt^2) + I(distStrucSt^2) + I(distFeedSt^2)
                 + hunt*distRdSt + hunt*distStrucSt + hunt*distFeedSt
                 + hunt*I(distRdSt^2) + hunt*I(distStrucSt^2) + hunt*I(distFeedSt^2),
                data = z)           
                                
    summary(t)    
    AIC(t, tsa34day)
    # model was estimated. it appears the same as the old version
    
    zz <- z %>%
      mutate(
        prWolf = fitted(t),
        model = "Day",
        Hunt  = hunt)
    
    
        ggplot(zz, aes(x = distFeed, y = Used)) +
        stat_smooth(aes(x = distFeed, y = prWolf, linetype = as.factor(Hunt)),
                    method = "lm", formula = y ~ poly(x, 2)) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(y = "Pr(Use)", title = "All wolves")                
length(which(is.na(z$distFeed)))                
                       