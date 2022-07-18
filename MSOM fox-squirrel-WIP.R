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
detection_matrix  <- readRDS(gzcon(url("https://github.com/germaincm/cameratrap_analysis/raw/main/detection_matrix_all_15072022.rds")))

###save species specific detection matrix into an object:
coyote <- detection_matrix$coyote
fox <- detection_matrix$fox
deer <- detection_matrix$deer
rabbit <- detection_matrix$rabbit
raccoon <- detection_matrix$raccoon
cat <- detection_matrix$cat
squirrel <- detection_matrix$squirrel


#### COVARIATES ########

##GENERATE COVARIATE dataframes for the model , make sure to readapt the site_names AND add human/dog presence####
urlfile100="https://raw.githubusercontent.com/germaincm/cameratrap_analysis/main/cov_100.csv"
urlfile500="https://raw.githubusercontent.com/germaincm/cameratrap_analysis/main/cov_500.csv"
#urlfile1000="https://raw.githubusercontent.com/germaincm/cameratrap_analysis/main/cov_1000.csv"
urlfile2000="https://raw.githubusercontent.com/germaincm/cameratrap_analysis/main/cov_2000.csv"
urlfilehumans="https://raw.githubusercontent.com/germaincm/cameratrap_analysis/main/human_dog_df.csv"

human_dog_df <- read.csv(urlfilehumans) %>% 
  select(-1) %>% 
  select(site_name, total_freq_humans ,total_freq_dogs )

b100 <- read.csv(urlfile100)%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))
b100 <- left_join(b100, human_dog_df, by="site_name")%>%
  dplyr::filter(site_name %in% unique(detection_matrix$deer$site_name)) ##filter those relevant for the analysis

b500 <- read.csv(urlfile500)%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))
b500 <- left_join(b500, human_dog_df, by="site_name")%>%
  dplyr::filter(site_name %in% unique(detection_matrix$deer$site_name)) ##filter those relevant for the analysis

# b1000 <- read.csv(urlfile1000)%>%
#   mutate(site_name = gsub("_", "", site_name))%>%
#   mutate(site_name = gsub("TUW0", "TUW", site_name))
# b1000 <- left_join(b1000, human_dog_df, by="site_name")%>%
#   dplyr::filter(site_name %in% unique(detection_matrix$deer$site_name)) ##filter those relevant for the analysis

b2000 <- read.csv(urlfile2000)%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))
b2000 <- left_join(b2000, human_dog_df, by="site_name")%>%
  dplyr::filter(site_name %in% unique(detection_matrix$deer$site_name)) ##filter those relevant for the analysis


##call occupancy covariates
cov<- b100 %>% select(-1, -BUFF_DIST, -SHAPE_Length, -ORIG_FID, -SHAPE_Area)
cov100 <- as.data.frame(scale(cov))

cov<- b500 %>% select(-1, -BUFF_DIST, -SHAPE_Length, -ORIG_FID, -SHAPE_Area)
cov500 <- as.data.frame(scale(cov))

cov<- b2000 %>% select(-1, -BUFF_DIST, -SHAPE_Length, -ORIG_FID, -SHAPE_Area)
cov2000 <- as.data.frame(scale(cov))

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
               squirrel = as.matrix(squirrel %>% select(-1)))   ##toggle prey of interest

##create an unmarked frame for each buffer for clarity
fs00 <- unmarkedFrameOccuMulti(y = y_list,
                                  siteCovs = cov100,
                                  obsCovs = det_list)

fs500 <- unmarkedFrameOccuMulti(y = y_list,
                                  siteCovs = cov500,
                                  obsCovs = det_list)

fs2000 <- unmarkedFrameOccuMulti(y = y_list,
                                   siteCovs = cov2000,
                                   obsCovs = det_list)

##call animal data
mdata <- fs100

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

fit_built <- occuMulti(detformulas = c('~season', '~season'),
                       stateformulas = c('~1', '~1', '~built'),
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
               fit_Fmix_PA, fit_built, fit_LFT_dist, fit_POP_mean,
               fit_POP_median, fit_hum)
modSel(fit)

sink("fsquirrel_modSel_2000.txt")
print(modSel(fit))
sink()

fit_fsquirrel_2000 <- fit_multi4 #is best model

sink("fsquirrel_fit_2000.txt")
print(summary(fit_fsquirrel_2000))
sink()


##notes for interpretation
##FOX-SQUIRREL
# at 100 buffer:

# at 500 buffer:

# at 2000 buffer: 


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
  DEM_mean = seq(min(cov2000$DEM_mean), max(cov2000$DEM_mean), length.out = 1000))  #cov of interest is the only one not averaged out
coy_deer1 <- unmarked::predict(fit_DEM_mean, type = 'state', species = 'coyote', cond = 'deer', 
                               newdata = nd_cond1)
coy_deer0 <- unmarked::predict(fit_DEM_mean, type = 'state', species = 'coyote',
                               cond = '-deer', newdata = nd_cond1)

gg_coy_cond <- data.frame(
  DEM_mean = rep(nd_cond1$DEM_mean, 2),
  occupancy = c(coy_deer1$Predicted, coy_deer0$Predicted),
  low = c(coy_deer1$lower, coy_deer0$lower),
  high = c(coy_deer1$upper, coy_deer0$upper),
  conditional = rep(c('Deer present', 'Deer absent'),
                    each = 1000))

ggplot(gg_coy_cond, aes(x = DEM_mean, y = occupancy, color = conditional)) +
  #geom_ribbon(aes(ymin = low, ymax = high, fill = conditional)) +
  geom_line() +
  ylab('Conditional coyote\noccupancy probability') +
  xlab('Digital Elevation Model (mean)') +
  labs(fill = 'Deer state') # +
#theme(text = element_text(size = 25),
#legend.position = c(0.75, 0.85))


