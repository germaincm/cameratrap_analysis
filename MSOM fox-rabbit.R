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
  mutate(site_name = ifelse(site_name=="TUW35a", "TUW35", site_name))
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
  dplyr::filter(site_name %in% unique(coyote$site_name)) ##filter those relevant for the analysis

b500 <- read.csv("cov_500.csv")%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))
b500 <- left_join(b500, human_dog_df, by="site_name")
b500 <- left_join(b500, imperv, by="site_name") %>%
  dplyr::filter(site_name %in% unique(coyote$site_name)) ##filter those relevant for the analysis

b2000 <- read.csv("cov_2000.csv")%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))
b2000 <- left_join(b2000, human_dog_df, by="site_name")
b2000 <- left_join(b2000, imperv, by="site_name") %>%
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
# cov<- b100 %>% select(-1, -BUFF_DIST, -SHAPE_Length, -ORIG_FID, -SHAPE_Area, -WVO_PA) %>%
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


###MULTISPECIES OCCUPANCY

##create y_list for interaction of interest
y_list <- list(fox = as.matrix(fox %>% select(-1)),  ##toggle predator of interest
               rabbit = as.matrix(rabbit %>% select(-1)))   ##toggle prey of interest

##create an unmarked frame for each buffer for clarity
fr100 <- unmarkedFrameOccuMulti(y = y_list,
                                siteCovs = cov100,
                                obsCovs = det_list)

fr500 <- unmarkedFrameOccuMulti(y = y_list,
                                siteCovs = cov500,
                                obsCovs = det_list)

fr2000 <- unmarkedFrameOccuMulti(y = y_list,
                                 siteCovs = cov2000,
                                 obsCovs = det_list)

##call animal data
mdata <- fr100

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

sink("frabbit_modSel_2000.txt")
print(modSel(fit))
sink()

fit_frabbit_2000 <- fit_null #is best model

sink("frabbit_fit_2000.txt")
print(summary(fit_frabbit_2000))
sink()


##notes for interpretation
##FOX-RABBIT
# at 100 buffer: from the models with AIC <2 null model, WVF_PA is the only one
#   with a significant effect (-0.981, p = 0.0478)
# at 500 buffer:  no model with AIC <2 null 
# at 2000 buffer: no model with AIC <2 null :(


#second selection
fit_multi1 <- occuMulti(detformulas = c('~season', '~season'),
                        stateformulas = c('~1', '~1', '~LFT_dist+Fdec_PA'),
                        maxOrder = 2,
                        data = mdata)

fit_multi2 <- occuMulti(detformulas = c('~season', '~season'),
                        stateformulas = c('~1', '~1', '~Fdec_PA+LFT_dist'),
                        maxOrder = 2,
                        data = mdata)

fit_multi3 <- occuMulti(detformulas = c('~season', '~season'),
                        stateformulas = c('~1', '~1', '~Fdec_PA+WVF_dist'),
                        maxOrder = 2,
                        data = mdata)

fit_multi4 <- occuMulti(detformulas = c('~season', '~season'),
                        stateformulas = c('~1', '~1', '~Fdec_PA+Fcon_PA'),
                        maxOrder = 2,
                        data = mdata)

fit_multi5 <- occuMulti(detformulas = c('~season', '~season'),
                        stateformulas = c('~1', '~1', '~Fdec_PA+Fmix_PA'),
                        maxOrder = 2,
                        data = mdata)


fit <- fitList(fit_Fdec_PA, fit_multi1, fit_multi2, fit_multi3, fit_multi4, fit_multi5)
modSel(fit)




##predict
nd_cond1 <- data.frame(
  WVF_PA = seq(min(cov100$WVF_PA), max(cov100$WVF_PA), length.out = 1000))  #cov of interest is the only one not averaged out
fox_rab1 <- unmarked::predict(fit_WVF_PA, type = 'state', species = 'fox', cond = 'rabbit',
                               newdata = nd_cond1)
fox_rab0 <- unmarked::predict(fit_WVF_PA, type = 'state', species = 'fox',
                               cond = '-rabbit', newdata = nd_cond1)

gg_fox_cond <- data.frame(
  WVF_PA = rep(nd_cond1$WVF_PA, 2),
  occupancy = c(fox_rab1$Predicted, fox_rab0$Predicted),
  low = c(fox_rab1$lower, fox_rab0$lower),
  high = c(fox_rab1$upper, fox_rab0$upper),
  conditional = rep(c('Present', 'Absent'),
                    each = 1000))
# 
ggplot(gg_fox_cond, aes(x = WVF_PA, y = occupancy, group = conditional)) +
  geom_ribbon(aes(ymin = low, ymax = high, fill = conditional)) +
  geom_line() +
  ylab('Conditional fox\noccupancy probability') +
  xlab('Forest % area') +
  labs(fill = 'Rabbit state') # +
#theme(text = element_text(size = 25),
#legend.position = c(0.75, 0.85))
ggsave("frabbit_2000_WVF_PA_predict_100.png")


