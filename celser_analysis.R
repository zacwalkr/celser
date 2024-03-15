library(dplyr)
library(ggplot2)
library(MuMIn)
library(car)
library(vegan)
library(cowplot)
library(ggpubr)

###read in data
cel <- read.csv("celser_data.csv")

###check correlation between variables of interest
cor(cel[c("trees","elev", "patch_sqrt","track_sqrt", "slope", "deer1000")],
    method = "spearman")

###run global binomial glm
mod0 <- glm(deer_binary ~ trees  + elev + patch_sqrt + 
              track_sqrt + slope + deer1000, 
            family = binomial(link = "logit"),
            na.action = "na.fail", data = cel)

###calculate type-II anova table for model
Anova(mod0)

### dredge global model
aa <- dredge(mod0, rank = "AICc")

### get table of top models (delta AICc < 2 )
bb <- aa %>% filter(delta < 2)

### fit each of the top performing models
mod1 <- glm(deer_binary ~ trees  + patch_sqrt + deer1000 + elev, family = binomial(link = "logit"),  na.action = "na.fail", data = cel)
mod2 <- glm(deer_binary ~ trees  + patch_sqrt + deer1000, family = binomial(link = "logit"),  na.action = "na.fail", data = cel)
mod3 <- glm(deer_binary ~ trees  + patch_sqrt + track_sqrt + deer1000, family = binomial(link = "logit"),  na.action = "na.fail", data = cel)
mod4 <- glm(deer_binary ~ trees  + patch_sqrt + track_sqrt, family = binomial(link = "logit"),  na.action = "na.fail", data = cel)

#create model selection object then average parameters 
mods <- model.sel(mod1,mod2,mod3,mod4)
mod.av <- model.avg(mods)

#check summary
summary(mod.av)
confint(mod.av, full = TRUE)
coefTable(mod.av, full = TRUE)


### predict main effects that were present in the top performing models

##trees
newdat1 <- data.frame(trees = seq(min(cel$trees), max(cel$trees), length.out = 100),
                       patch_sqrt = mean(cel$patch_sqrt),
                       track_sqrt = mean(cel$track_sqrt),
                       elev = mean(cel$elev),
                      deer1000 = mean(cel$deer1000))

predictions <- predict(mod.av, newdata = newdat1, type = "link", se.fit = TRUE)
newdat1$link_vals <- predictions$fit
newdat1$se_link <- predictions$se.fit
newdat1$lower_bound <- newdat1$link_vals - 1.96 * newdat1$se_link
newdat1$upper_bound <- newdat1$link_vals + 1.96 * newdat1$se_link
newdat1$predicted_probs <- plogis(newdat1$link_vals)
newdat1$lower_ci <- plogis(newdat1$lower_bound)
newdat1$upper_ci <- plogis(newdat1$upper_bound)

##patch size
newdat2 <- data.frame(track_sqrt = seq(min(cel$track_sqrt), max(cel$track_sqrt), length.out = 100),
                      patch_sqrt = mean(cel$patch_sqrt),
                      trees = mean(cel$trees),
                      elev = mean(cel$elev),
                      deer1000 = mean(cel$deer1000))

predictions <- predict(mod.av, newdata = newdat2, type = "link", se.fit = TRUE)
newdat2$link_vals <- predictions$fit
newdat2$se_link <- predictions$se.fit
newdat2$lower_bound <- newdat2$link_vals - 1.96 *  newdat1$se_link
newdat2$upper_bound <- newdat2$link_vals + 1.96 *  newdat1$se_link
newdat2$predicted_probs <- plogis(newdat2$link_vals)
newdat2$lower_ci <- plogis(newdat2$lower_bound)
newdat2$upper_ci <- plogis(newdat2$upper_bound)

##track density
newdat3 <- data.frame(patch_sqrt = seq(min(cel$patch_sqrt), max(cel$patch_sqrt), length.out = 100),
                      track_sqrt = mean(cel$track_sqrt),
                      trees = mean(cel$trees),
                      elev = mean(cel$elev),
                      deer1000 = mean(cel$deer1000))

predictions <- predict(mod.av, newdata = newdat3, type = "link", se.fit = TRUE)
newdat3$link_vals <- predictions$fit
newdat3$se_link <- predictions$se.fit
newdat3$lower_bound <- newdat3$link_vals - 1.96 * newdat1$se_link
newdat3$upper_bound <- newdat3$link_vals + 1.96 * newdat1$se_link
newdat3$predicted_probs <- plogis(newdat3$link_vals)
newdat3$lower_ci <- plogis(newdat3$lower_bound)
newdat3$upper_ci <- plogis(newdat3$upper_bound)

##elev
newdat4 <- data.frame(elev = seq(min(cel$elev), max(cel$elev), length.out = 100),
                      patch_sqrt = mean(cel$patch_sqrt),
                      track_sqrt = mean(cel$track_sqrt),
                      trees = mean(cel$trees),
                      deer1000 = mean(cel$deer1000))

predictions <- predict(mod.av, newdata = newdat4, type = "link", se.fit = TRUE)
newdat4$link_vals <- predictions$fit
newdat4$se_link <- predictions$se.fit
newdat4$lower_bound <- newdat4$link_vals - 1.96 * newdat1$se_link
newdat4$upper_bound <- newdat4$link_vals + 1.96 * newdat1$se_link
newdat4$predicted_probs <- plogis(newdat4$link_vals)
newdat4$lower_ci <- plogis(newdat4$lower_bound)
newdat4$upper_ci <- plogis(newdat4$upper_bound)

#deer1000
newdat5 <- data.frame(deer1000 = seq(min(cel$deer1000), max(cel$deer1000), length.out = 100),
                      patch_sqrt = mean(cel$patch_sqrt),
                      track_sqrt = mean(cel$track_sqrt),
                      trees = mean(cel$trees),
                      elev = mean(cel$elev))

predictions <- predict(mod.av, newdata = newdat5, type = "link", se.fit = TRUE)
newdat5$link_vals <- predictions$fit
newdat5$se_link <- predictions$se.fit
newdat5$lower_bound <- newdat5$link_vals - 1.96 * newdat1$se_link
newdat5$upper_bound <- newdat5$link_vals + 1.96 * newdat1$se_link
newdat5$predicted_probs <- plogis(newdat5$link_vals)
newdat5$lower_ci <- plogis(newdat5$lower_bound)
newdat5$upper_ci <- plogis(newdat5$upper_bound)



##### create predicted probability plot for each main effect
plota <- ggplot(newdat5, aes(x = deer1000, y = predicted_probs))+
  geom_line()+
  geom_jitter(data = cel, aes(x = deer1000, y =deer_binary), colour = "blue", alpha = 0.3, height = 0, width = 0.3)+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.25)+
  theme_cowplot(12)+
  xlab("Deer shot (within 1 km of site)")+
  ylab(" ")

plotb <- ggplot(newdat3, aes(x = patch_sqrt, y = predicted_probs))+
  geom_line()+
  geom_jitter(data = cel, aes(x = patch_sqrt, y =deer_binary), colour = "blue", alpha = 0.3, height = 0, width = 1)+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.25)+
  theme_cowplot(12)+
  xlab(bquote("Patch size "(sqrt(m^2))))+
  ylab(" ")+
  geom_text(aes(x = 5, y = 0.9), label = "*", size = 8)

plotc <- ggplot(newdat1, aes(x = trees, y = predicted_probs))+
  geom_line()+
  geom_point(data = cel, aes(x = trees, y = deer_binary), colour = "blue", alpha = 0.3)+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.25)+
  theme_cowplot(12)+
  xlab("Distance to treeline (m)")+
  ylab("Probability of deer impact")+
  geom_text(aes(x = 105, y = 0.9), label = "**", size = 8)

plotd <- ggplot(newdat2, aes(x = track_sqrt, y = predicted_probs))+
  geom_line()+
  geom_jitter(data = cel, aes(x = track_sqrt, y =deer_binary), colour = "blue", alpha = 0.3, height = 0, width = 0.25)+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.25)+
  theme_cowplot(12)+
  xlab(bquote("Track density "(km/km^2)))+
  ylab(" ")

plote <- ggplot(newdat4, aes(x = elev, y = predicted_probs))+
  geom_line()+
  geom_jitter(data = cel, aes(x = elev, y =deer_binary), colour = "blue", alpha = 0.3, height = 0, width = 0.25)+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.25)+
  theme_cowplot(12)+
  xlab("Elevation (m)")+
  ylab("Probability of deer impact")


### combine above plots to create figure 3 
ggarrange(plotc, plotb, plota, plote, plotd, labels = "AUTO")


### summarise deer impact scores to determine proportion of sites with each impact score
cel2  <- cel %>% group_by(deer_scale) %>% 
  mutate(n = n(), prop = n / 57) %>% ungroup() %>% 
  distinct(deer_scale, prop)

### plot to create figure 2  
ggplot(cel2, aes(x = deer_scale, y = prop))+
  geom_col(width = 0.5)+
  xlab("Deer impact score")+
  ylab("Proportion of populations")+
  theme_cowplot()+
  ylim(0,0.6)



### Check  global and each of the 4 best performing models using DHARMa #############
library(DHARMa)

simulationOutput <- simulateResiduals(fittedModel = mod0)
plot(simulationOutput, asFactor = F) # qq residuals and residuals vs predicted are fine
testDispersion(simulationOutput) # no dispersion
testOutliers(simulationOutput, type = "binomial") #no outliers

simulationOutput <- simulateResiduals(fittedModel = mod1)
plot(simulationOutput, asFactor = F) # qq residuals and residuals vs predicted are slightly deviated but fine (combined adjusted quantile test not signif)
testDispersion(simulationOutput) # no dispersion
testOutliers(simulationOutput, type = "binomial") #no outliers

simulationOutput <- simulateResiduals(fittedModel = mod2)
plot(simulationOutput, asFactor = F) # qq residuals and residuals vs predicted are fine
testDispersion(simulationOutput) # no dispersion
testOutliers(simulationOutput, type = "binomial") #no outliers

simulationOutput <- simulateResiduals(fittedModel = mod3)
plot(simulationOutput, asFactor = F) # qq residuals and residuals vs predicted are fine
testDispersion(simulationOutput) # no dispersion
testOutliers(simulationOutput, type = "binomial") #no outliers

simulationOutput <- simulateResiduals(fittedModel = mod4)
plot(simulationOutput, asFactor = F) # qq residuals and residuals vs predicted are fine
testDispersion(simulationOutput) # no dispersion
testOutliers(simulationOutput, type = "binomial") #no outliers

#

