library(unmarked)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)


###################START HERE IF WANTING TO USE DIRECTLY THE DETECTION MATRIX #############
###read directly the detection matrix RDS to avoid every step of this script up to here###
detection_matrix  <- readRDS(gzcon(url("https://github.com/tgelmi-candusso/cameratrap_analysis/raw/main/detection_matrix_Scarborough.rds")))

##now to call for the detection matrix of a specific animal you can call it this way
detection_matrix$deer
detection_matrix$coyote

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
urlfile100="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/cov_1000.csv"
urlfile500="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/cov_500.csv"
#urlfile1000="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/cov_1000.csv"
urlfile2000="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/cov_2000.csv"
urlfilehumans="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/human_dog_df.csv"

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

##create an unmarked frame for each interaction of interest 
y_list <- list(coyote = as.matrix(coyote %>% select(-1)),  ##toggle predator of interest
                  rabbit = as.matrix(rabbit %>% select(-1)))   ##toggle prey of interest

crab100 <- unmarkedFrameOccuMulti(y = y_list,
                                      siteCovs = cov100,
                                      obsCovs = det_list)

crab500 <- unmarkedFrameOccuMulti(y = y_list,
                                siteCovs = cov500,
                                obsCovs = det_list)

crab2000 <- unmarkedFrameOccuMulti(y = y_list,
                                siteCovs = cov2000,
                                obsCovs = det_list)

##call animal data
mdata <- crab100

#first selection
fit_null <- occuMulti(detformulas = c('~season', '~season'),
                      stateformulas = c('~1', '~1', '~1'),
                      maxOrder = 2,
                      data = mdata)

fit_veg <- occuMulti(detformulas = c('~season', '~season'),
                     stateformulas = c('~1', '~1', '~veg_complexity'),
                     maxOrder = 2,
                     data = mdata)

fit_cor <- occuMulti(detformulas = c('~season', '~season'),
                     stateformulas = c('~1', '~1', '~corridor'),
                     maxOrder = 2,
                     data = mdata)

fit_LFT <- occuMulti(detformulas = c('~season', '~season'),
                     stateformulas = c('~1', '~1', '~LFT_dist'),
                     maxOrder = 2,
                     data = mdata)

fit_H2O <- occuMulti(detformulas = c('~season', '~season'),
                     stateformulas = c('~1', '~1', '~H2O_dist'),
                     maxOrder = 2,
                     data = mdata)

fit_WV <- occuMulti(detformulas = c('~season', '~season'),
                    stateformulas = c('~1', '~1', '~WV_dist'),
                    maxOrder = 2,
                    data = mdata)

fit_MV <- occuMulti(detformulas = c('~season', '~season'),
                    stateformulas = c('~1', '~1', '~MV_dist'),
                    maxOrder = 2,
                    data = mdata)

fit_WVF <- occuMulti(detformulas = c('~season', '~season'),
                     stateformulas = c('~1', '~1', '~WVF_dist'),
                     maxOrder = 2,
                     data = mdata)

fit_WVO <- occuMulti(detformulas = c('~season', '~season'),
                     stateformulas = c('~1', '~1', '~WVO_dist'),
                     maxOrder = 2,
                     data = mdata)

fit_built <- occuMulti(detformulas = c('~season', '~season'),
                       stateformulas = c('~1', '~1', '~built'),
                       maxOrder = 2,
                       data = mdata)

fit_DEM_median <- occuMulti(detformulas = c('~season', '~season'),
                            stateformulas = c('~1', '~1', '~DEM_median'),
                            maxOrder = 2,
                            data = mdata)

fit_DEM_mean <- occuMulti(detformulas = c('~season', '~season'),
                          stateformulas = c('~1', '~1', '~DEM_mean'),
                          maxOrder = 2,
                          data = mdata)

fit_NDVI_median <- occuMulti(detformulas = c('~season', '~season'),
                             stateformulas = c('~1', '~1', '~NDVI_median'),
                             maxOrder = 2,
                             data = mdata)

fit_NDVI_mean <- occuMulti(detformulas = c('~season', '~season'),
                           stateformulas = c('~1', '~1', '~NDVI_mean'),
                           maxOrder = 2,
                           data = mdata)

fit_POP_median <- occuMulti(detformulas = c('~season', '~season'),
                            stateformulas = c('~1', '~1', '~POP_median'),
                            maxOrder = 2,
                            data = mdata)

fit_POP_mean <- occuMulti(detformulas = c('~season', '~season'),
                          stateformulas = c('~1', '~1', '~POP_mean'),
                          maxOrder = 2,
                          data = mdata)

fit_WVO_PA <- occuMulti(detformulas = c('~season', '~season'),
                        stateformulas = c('~1', '~1', '~WVO_PA'),
                        maxOrder = 2,
                        data = mdata)

fit_WVF_PA <- occuMulti(detformulas = c('~season', '~season'),
                        stateformulas = c('~1', '~1', '~WVF_PA'),
                        maxOrder = 2,
                        data = mdata)

fit_MV_PA <- occuMulti(detformulas = c('~season', '~season'),
                       stateformulas = c('~1', '~1', '~MV_PA'),
                       maxOrder = 2,
                       data = mdata)

fit_FD_PA <- occuMulti(detformulas = c('~season', '~season'),
                       stateformulas = c('~1', '~1', '~Fdec_PA'),
                       maxOrder = 2,
                       data = mdata)

fit_FM_PA <- occuMulti(detformulas = c('~season', '~season'),
                       stateformulas = c('~1', '~1', '~Fmix_PA'),
                       maxOrder = 2,
                       data = mdata)

fit_FC_PA <- occuMulti(detformulas = c('~season', '~season'),
                       stateformulas = c('~1', '~1', '~Fcon_PA'),
                       maxOrder = 2,
                       data = mdata)

fit_hum <- occuMulti(detformulas = c('~season', '~season'),
                     stateformulas = c('~1', '~1', '~total_freq_humans'),
                     maxOrder = 2,
                     data = mdata)
fit_dog <- occuMulti(detformulas = c('~season', '~season'),
                     stateformulas = c('~1', '~1', '~total_freq_dogs'),
                     maxOrder = 2,
                     data = mdata)

fit <- fitList(fit_null, fit_cor, fit_veg, fit_LFT, fit_H2O, fit_WV, fit_MV, fit_WVF,
               fit_WVO, fit_built, fit_DEM_median, fit_DEM_mean, fit_NDVI_median,
               fit_NDVI_mean, fit_POP_median, fit_POP_mean, fit_WVO_PA, fit_WVF_PA, 
               fit_MV_PA, fit_FC_PA, fit_FM_PA, fit_FD_PA, fit_hum, fit_dog)
modSel(fit)

#second selection
fit_multi1 <- occuMulti(detformulas = c('~season', '~season'),
                        stateformulas = c('~1', '~1', '~veg_complexity+WVF_PA'),
                        maxOrder = 2,
                        data = mdata)

fit_multi2 <- occuMulti(detformulas = c('~season', '~season'),
                        stateformulas = c('~1', '~1', '~~DEM_mean+WVF_dist'),
                        maxOrder = 2,
                        data = mdata)

fit_multi3 <- occuMulti(detformulas = c('~season', '~season'),
                        stateformulas = c('~1', '~1', '~DEM_mean+POP_mean'),
                        maxOrder = 2,
                        data = mdata)

fit_multi4 <- occuMulti(detformulas = c('~season', '~season'),
                        stateformulas = c('~1', '~1', '~NDVI_mean+DEM_mean'),
                        maxOrder = 2,
                        data = mdata)

fit_multi5 <- occuMulti(detformulas = c('~season', '~season'),
                        stateformulas = c('~1', '~1', '~DEM_mean+built'),
                        maxOrder = 2,
                        data = mdata)

fit <- fitList(fit_NDVI_mean, fit_multi1, fit_multi2, fit_multi3, fit_multi4, fit_multi5)
modSel(fit)

fit_cdeer_2000 <- fit_DEM_mean #is best model

sink("fit_cdeer_2000a.txt")
print(summary(fit_cdeer_2000))
sink()


##notes for interpretation
##COYOTE-RABBIT
##at 100 buffer: 
##at 500 buffer:
##at 2000 buffer: 


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


