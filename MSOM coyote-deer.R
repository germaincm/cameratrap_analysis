library(unmarked)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(plyr)
library(rstan)

getwd()
setwd("~/Documents/GitHub/cameratrap_analysis")


###################START HERE IF WANTING TO USE DIRECTLY THE DETECTION MATRIX #############
###read directly the detection matrix RDS to avoid every step of this script up to here###
detection_matrix  <- readRDS("detection_matrix_revsites_28072022.rds")

###save species specific detection matrix into an object:
coyote <- detection_matrix$coyote %>%
  mutate(site_name = ifelse(site_name=="TUW35a", "TUW35", site_name)) #%>%
  #filter(rowSums(is.na(detection_matrix$coyote)) < 49) ##use this to reduce to only sites deployed for >4 weeks
fox <- detection_matrix$fox %>%
  mutate(site_name = ifelse(site_name=="TUW35a", "TUW35", site_name))
deer <- detection_matrix$deer %>%
  mutate(site_name = ifelse(site_name=="TUW35a", "TUW35", site_name))
rabbit <- detection_matrix$rabbit %>%
  mutate(site_name = ifelse(site_name=="TUW35a", "TUW35", site_name))
raccoon <- detection_matrix$raccoon %>%
  mutate(site_name = ifelse(site_name=="TUW35a", "TUW35", site_name))
cat <- detection_matrix$cat %>%
  mutate(site_name = ifelse(site_name=="TUW35a", "TUW35", site_name))
squirrel <- detection_matrix$squirrel %>%
  mutate(site_name = ifelse(site_name=="TUW35a", "TUW35", site_name)) 

#### COVARIATES ########

##GENERATE COVARIATE dataframes for the model , make sure to readapt the site_names AND add human/dog presence####
# urlfile100="https://raw.githubusercontent.com/germaincm/cameratrap_analysis/main/cov_100.csv"
# urlfile500="https://raw.githubusercontent.com/germaincm/cameratrap_analysis/main/cov_500.csv"
# urlfile1000="https://raw.githubusercontent.com/germaincm/cameratrap_analysis/main/cov_1000.csv"
# urlfile2000="https://raw.githubusercontent.com/germaincm/cameratrap_analysis/main/cov_2000.csv"
# urlfilehumans="https://raw.githubusercontent.com/germaincm/cameratrap_analysis/main/human_dog_df.csv"

human_dog_df <- read.csv("human_dog_df.csv") %>% 
  select(-1) %>% 
  select(site_name, total_freq_humans ,total_freq_dogs ) %>% 
  mutate(site_name = ifelse(site_name=="TUW35a", "TUW35", site_name))

imperv <- read.csv("cov_impervious.csv")

b100 <- read.csv("cov_100.csv")%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))
b100 <- left_join(b100, human_dog_df, by="site_name")
b100 <- left_join(b100, imperv, by="site_name") %>%
  arrange(site_name) %>%
  dplyr::filter(site_name %in% unique(coyote$site_name)) ##filter those relevant for the analysis

b500 <- read.csv("cov_500.csv")%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))
b500 <- left_join(b500, human_dog_df, by="site_name")
b500 <- left_join(b500, imperv, by="site_name") %>%
  arrange(site_name) %>%
  dplyr::filter(site_name %in% unique(coyote$site_name)) ##filter those relevant for the analysis

b2000 <- read.csv("cov_2000.csv")%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))
b2000 <- left_join(b2000, human_dog_df, by="site_name")
b2000 <- left_join(b2000, imperv, by="site_name") %>%
  arrange(site_name) %>%
  dplyr::filter(site_name %in% unique(coyote$site_name)) ##filter those relevant for the analysis

##call occupancy covariates

#### here I've selected for only my covariates of interest
cov<- b100 %>%
  select(WVF_dist, WVF_PA, Fdec_PA, Fcon_PA, Fmix_PA, imperv_100, POP_mean,
         POP_median, LFT_dist, total_freq_humans) %>%
  dplyr::rename(imperv = imperv_100)
cov100 <- as.data.frame(scale(cov))

cov<- b500 %>%
  select(WVF_dist, WVF_PA, Fdec_PA, Fcon_PA, Fmix_PA, imperv_500, POP_mean,
         POP_median, LFT_dist, total_freq_humans) %>%
  dplyr::rename(imperv = imperv_500)
cov500 <- as.data.frame(scale(cov))

cov<- b2000 %>%
  select(WVF_dist, WVF_PA, Fdec_PA, Fcon_PA, Fmix_PA, imperv_2000, POP_mean,
         POP_median, LFT_dist, total_freq_humans) %>%
  dplyr::rename(imperv = imperv_2000)
cov2000 <- as.data.frame(scale(cov))

###without selecting for only covariates of interest; not scaling categorical columns
# cov<- b100 %>% select(-1, -BUFF_DIST, -SHAPE_Length, -ORIG_FID, -SHAPE_Area) %>%
#   dplyr::rename(imperv = imperv_100)
# cov1 <- cov[,1:2]
# cov2 <- as.data.frame(scale(cov[,3:ncol(cov)]))
# cov<- cbind(cov1,cov2)
# cov100a <- cov
# 
# cov<- b500 %>% select(-1, -BUFF_DIST, -SHAPE_Length, -ORIG_FID, -SHAPE_Area) %>%
#   dplyr::rename(imperv = imperv_500)
# cov1 <- cov[,1:2]
# cov2 <- as.data.frame(scale(cov[,3:ncol(cov)]))
# cov<- cbind(cov1,cov2)
# cov500a <- cov
# 
# cov<- b2000 %>% select(-1, -BUFF_DIST, -SHAPE_Length, -ORIG_FID, -SHAPE_Area) %>%
#   dplyr::rename(imperv = imperv_2000)
# cov1 <- cov[,1:2]
# cov2 <- as.data.frame(scale(cov[,3:ncol(cov)]))
# cov<- cbind(cov1,cov2)
# cov2000a <- cov

##call detection covariate matrix here if using

d5 <- coyote %>% select()
d5 [, 1:11] <- "winter"
d5 [, 12:24] <- "spring"
d5 [, 25:37] <- "summer"
d5 [, 38:50] <- "fall"
d5 [, 51:53] <- "winter"

det_covs <- d5 
det_list <- list(season = det_covs)


#### MULTISPECIES OCCUPANCY ####

##create y_list for interaction of interest
y_list <- list(coyote = as.matrix(coyote %>% select(-1)),  ##toggle predator of interest
                  deer = as.matrix(deer %>% select(-1)))   ##toggle prey of interest

##create an unmarked frame for each buffer for clarity
cd100 <- unmarkedFrameOccuMulti(y = y_list,
                                      siteCovs = cov100,
                                      obsCovs = det_list)

cd500 <- unmarkedFrameOccuMulti(y = y_list,
                                siteCovs = cov500,
                                obsCovs = det_list)

cd2000 <- unmarkedFrameOccuMulti(y = y_list,
                                siteCovs = cov2000,
                                obsCovs = det_list)

#### MODEL FIT + SELECTION ####

##call animal data
mdata <- cd2000              ##toggle buffer size here

#first selection
fit_null <- occuMulti(detformulas = c('~season', '~season'),
                      stateformulas = c('~1', '~1', '~1'),
                      maxOrder = 2,
                      data = mdata)

fit_WVF_dist <- occuMulti(detformulas = c('~season', '~season'),
                     stateformulas = c('~1', '~1', '~WVF_dist'),
                     maxOrder = 2,
                     data = mdata)

fit_WVF_PA <- occuMulti(detformulas = c('~season', '~season'),
                        stateformulas = c('~1', '~1', '~WVF_PA'),
                        maxOrder = 2,
                        data = mdata)

fit_Fdec_PA <- occuMulti(detformulas = c('~season', '~season'),
                         stateformulas = c('~1', '~1', '~Fdec_PA'),
                         maxOrder = 2,
                         data = mdata)

fit_Fcon_PA <- occuMulti(detformulas = c('~season', '~season'),
                         stateformulas = c('~1', '~1', '~Fcon_PA'),
                         maxOrder = 2,
                         data = mdata)

fit_Fmix_PA <- occuMulti(detformulas = c('~season', '~season'),
                         stateformulas = c('~1', '~1', '~Fmix_PA'),
                         maxOrder = 2,
                         data = mdata)

fit_imperv <- occuMulti(detformulas = c('~season', '~season'),
                       stateformulas = c('~1', '~1', '~imperv'),
                       maxOrder = 2,
                       data = mdata)

fit_LFT_dist <- occuMulti(detformulas = c('~season', '~season'),
                          stateformulas = c('~1', '~1', '~LFT_dist'),
                          maxOrder = 2,
                          data = mdata)

fit_POP_mean <- occuMulti(detformulas = c('~season', '~season'),
                          stateformulas = c('~1', '~1', '~POP_mean'),
                          maxOrder = 2,
                          data = mdata)

fit_POP_median <- occuMulti(detformulas = c('~season', '~season'),
                            stateformulas = c('~1', '~1', '~POP_median'),
                            maxOrder = 2,
                            data = mdata)

fit_hum <- occuMulti(detformulas = c('~season', '~season'),
                     stateformulas = c('~1', '~1', '~total_freq_humans'),
                     maxOrder = 2,
                     data = mdata)


fit <- fitList(fit_null, fit_WVF_dist, fit_WVF_PA, fit_Fdec_PA, fit_Fcon_PA,
               fit_Fmix_PA, fit_imperv, fit_LFT_dist, fit_POP_mean,
               fit_POP_median, fit_hum)
modSel(fit)

sink("cdeer_modSel_100.txt")
print(modSel(fit))
sink()

fit_cdeer_100 <- fit_imperv #is best model

sink("cdeer_fit_100.txt")
print(summary(fit_cdeer_100))
sink()


##notes for interpretation
##COYOTE-DEER
# at 100 buffer: from the models with AIC <2 null model, Fdec_PA is the one with
  # a most (near-)significant effect (1.32, p = 0.0571)
# at 500 buffer: from the models with AIC <2 null model, IMPERV is the only one
  # with a significant effect (-1.27, p = 0.0494)
# at 2000 buffer: from the models with AIC <2 null model, IMPERV is the only one
  # with a significant effect (-1.33, p = 0.0389)

#second selection
fit_multi1 <- occuMulti(detformulas = c('~season', '~season'),
                        stateformulas = c('~1', '~1', '~imperv+POP_mean'),
                        maxOrder = 2,
                        data = mdata)

fit_multi2 <- occuMulti(detformulas = c('~season', '~season'),
                        stateformulas = c('~1', '~1', '~imperv+total_freq_humans'),
                        maxOrder = 2,
                        data = mdata)

fit_multi3 <- occuMulti(detformulas = c('~season', '~season'),
                        stateformulas = c('~1', '~1', '~imperv+WVF_dist'),
                        maxOrder = 2,
                        data = mdata)

fit_multi4 <- occuMulti(detformulas = c('~season', '~season'),
                        stateformulas = c('~1', '~1', '~imperv+total_freq_humans+POP_mean+WVF_dist'),
                        maxOrder = 2,
                        data = mdata)
# 
# fit_multi5 <- occuMulti(detformulas = c('~season', '~season'),
#                         stateformulas = c('~1', '~1', '~Fdec_PA+Fmix_PA'),
#                         maxOrder = 2,
#                         data = mdata)
# 
# fit_multi6 <- occuMulti(detformulas = c('~season', '~season'),
#                         stateformulas = c('~1', '~1', '~Fdec_PA+Fcon_PA+Fmix_PA'),
#                         maxOrder = 2,
#                         data = mdata)


fit <- fitList(fit_null, fit_imperv, fit_hum, fit_multi1, fit_multi2, fit_multi3, fit_multi4)
modSel(fit)


#### CONDITIONAL OCCUPANCY ####

##predict
nd_cond1 <- data.frame(
  Fcon_PA = rep(mean(cov2000$Fcon_PA), 100),
  Fdec_PA = seq(min(cov2000$Fdec_PA), max(cov2000$Fdec_PA), length.out = 100))  #cov of interest is the only one not averaged out
coy_deer1 <- unmarked::predict(fit_multi4, type = 'state', species = 'coyote', cond = 'deer', 
                               newdata = nd_cond1)
coy_deer0 <- unmarked::predict(fit_multi4, type = 'state', species = 'coyote',
                               cond = '-deer', newdata = nd_cond1)

gg_coy_cond <- data.frame(
  Fdec_PA = rep(nd_cond1$Fdec_PA, 2),
  occupancy = c(coy_deer1$Predicted, coy_deer0$Predicted),
  low = c(coy_deer1$lower, coy_deer0$lower),
  high = c(coy_deer1$upper, coy_deer0$upper),
  conditional = rep(c('Present', 'Absent'),
                    each = 100))

ggplot(gg_coy_cond, aes(x = Fdec_PA, y = occupancy, group = conditional)) +
  geom_ribbon(aes(ymin = low, ymax = high, fill = conditional)) +
  geom_line() +
  ylab('Conditional coyote\noccupancy probability') +
  xlab('Deciduous forest % area') +
  labs(fill = 'Deer:') # +
#theme(text = element_text(size = 25),
#legend.position = c(0.75, 0.85))
ggsave("cdeer_2000_Fdec_predict_100.png")


#### MARGINAL OCCUPANCY ####

nd_marg <- data.frame(
  Fcon_PA = rep(mean(cov2000$Fcon_PA), 100),
  Fdec_PA = seq(min(cov2000$Fdec_PA), max(cov2000$Fdec_PA), length.out = 100))  #cov of interest is the only one not averaged out
coy_marg <- unmarked::predict(fit_multi4, type = 'state', species = 'coyote', 
                               newdata = nd_marg)
deer_marg <- unmarked::predict(fit_multi4, type = 'state', species = 'deer', 
                              newdata = nd_marg)

##coyote marginal
gg_coy_marg <- data.frame(
  Fdec_PA = nd_marg$Fdec_PA,
  occupancy = coy_marg$Predicted,
  low = coy_marg$lower,
  high = coy_marg$upper)

ggplot(gg_coy_marg, aes(x = Fdec_PA, y = occupancy)) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.5) +
  geom_line() +
  ylab('Marginal coyote\noccupancy probability') +
  xlab('Deciduous forest % area')
#theme(text = element_text(size = 25),
#legend.position = c(0.75, 0.85))
#ggsave("cdeer_2000_Fdec_predict_100.png")

##deer marginal
gg_deer_marg <- data.frame(
  Fdec_PA = nd_marg$Fdec_PA,
  occupancy = deer_marg$Predicted,
  low = deer_marg$lower,
  high = deer_marg$upper)

ggplot(gg_deer_marg, aes(x = Fdec_PA, y = occupancy)) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.5) +
  geom_line() +
  ylab('Marginal deer\noccupancy probability') +
  xlab('Deciduous forest % area')
#theme(text = element_text(size = 25),
#legend.position = c(0.75, 0.85))
#ggsave("cdeer_2000_Fdec_predict_100.png")



