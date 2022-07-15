#### OPENER ####

library(dplyr)
library(plyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)

getwd()
setwd("~/Documents/GitHub/cameratrap_analysis")

#### CLEAN UP DATA ####

##call dataset (raw timelapse export csv)

data<-read_csv("TimelapseDatabase_FULL_14072022 (1).csv")%>%
  filter(DeleteFlag == FALSE)%>%
  filter(revised==TRUE)

data<-data%>%
  dplyr::rename(site_name_original = RelativePath, common_name=Species)%>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "America/New_York")) %>%
  mutate(DateTime = force_tz(DateTime, "America/New_York", roll = FALSE))%>%
  mutate(site_name_original = as.character(site_name_original))%>% 
  mutate(site_name_clean = gsub("TUWCPC_", "CPC", site_name_original))%>%
  mutate(site_name_clean = gsub("_", "", site_name_clean))%>%
  mutate(site_name_clean = gsub("TUW0", "TUW", site_name_clean))%>%
  separate(site_name_clean, c("site_name","checkup"), fill="right")%>%
  dplyr::arrange(site_name, common_name, DateTime)%>%
  dplyr::mutate(delta.time = DateTime - lag(DateTime),
                delta.time_secs = as.numeric(delta.time, units = 'secs')) %>%
  dplyr::filter(delta.time_secs > 20)%>% #key line to decide independent events, here 20 seconds, but not important if doing presence/absence
  dplyr::mutate(season = factor(month(DateTime)))%>% 
  dplyr::mutate(week = factor(week(DateTime)))%>% 
  dplyr::mutate(site_name = factor(site_name))%>% 
  dplyr::mutate(date = factor(date(DateTime)))%>% 
  dplyr::mutate(common_name = factor(common_name))

##filter cameras that have inconsistent data ####
data <- data %>% 
  dplyr::filter(site_name != "TUW40")%>%
  dplyr::filter(site_name != "TUW41")%>%
  dplyr::filter(site_name != "CPC5") %>%
  dplyr::filter(site_name != "TUW20") 

##correct TUW42
d42 <- data %>% filter(site_name == "TUW42") %>%
  filter(!(year(DateTime)==2017))
d42_no2020 <- d42%>%  filter(!(year(DateTime)==2020))
d42_fix <- d42 %>%
  filter((year(DateTime)==2020))%>%
  mutate(DateTime = DateTime %m+% years(1))
d42_2020_fix <- rbind(d42_fix, d42_no2020)
d_no42 <- data %>% filter(site_name != "TUW42")
data <- rbind(d_no42, d42_2020_fix) #the hole of data stil existing in TUW42 is not due to not deployment nor malfunctioning, rather recovered photos for which I recovered only those with animals

##correct TUW31 and TUW32 switcharoo
data1<-data %>% 
  mutate(site_name = ifelse(site_name_original == "TUW31" & (DateTime>= "2021-07-01 17:28:26" & DateTime<= "2021-08-29 11:23:51"), "TUW32", site_name)) %>%
  mutate(site_name = ifelse(site_name_original == "TUW31" & (DateTime>= "2021-08-29 13:02:26" & DateTime<= "2021-08-29 13:03:42"), "TUW32", site_name)) %>%
  mutate(site_name = ifelse(site_name_original == "TUW32" & (DateTime>= "2021-07-01 17:55:22" & DateTime<= "2021-08-29 11:57:01"), "TUW31", site_name))




##establish transects + revised sites
Sca <- c("TUW16",
         "TUW17",
         "TUW18",
         "TUW19", 
         "TUW20", 
         "TUW21", 
         "TUW22", 
         "TUW23",
         "TUW24",
         "TUW25",
         "TUW26",
         "TUW27",
         "TUW28",
         "TUW29",
         "TUW29b",
         "TUW39",
         "TUW42",
         "TUW40",
         "TUW41",
         "TUW42")

Hum <- c("TUW10", 
         "TUW11", 
         "TUW12", 
         "TUW1",
         "TUW2",
         "TUW3",
         "TUW4",
         "TUW5",
         "TUW6",
         "TUW7",
         "TUW8",
         "TUW9",
         "TUW9b")

Don <- c("TUW13",
         "TUW14",
         "TUW15",
         "TUW30",
         "TUW31",
         "TUW32",
         "TUW33",
         "TUW33b",
         "TUW33c",
         "TUW34",
         "TUW35",
         "TUW35b",
         "TUW36",
         "TUW36b",
         "TUW37",
         "TUW37b",
         "TUW38",
         "TUW38b")
CPC <- c("CPC1","CPC2", "CPC3", "CPC4", "CPC5", "CPC6")

rev_sites <- c("TUW17",
               #"TUW18",
               "TUW19", 
               "TUW21", 
               "TUW23",
               "TUW24",
               "TUW25",
               "TUW26",
               "TUW27",
               "TUW28",
               "TUW29",
               "TUW29b",
               "TUW39",
               "TUW42",
               "TUW36",
               "TUW38",
               "TUW37b",
               "TUW35a",
               "TUW37",
               #"TUW36b",
               #"TUW33b",
               #"TUW34",
               "TUW38b",
               #"TUW35b",
               #"TUW32",
               "TUW31",
               #"TUW33",
               "TUW2")
               #"TUW14",
               #"TUW1")

#### COVARIATES ####

##GENERATE COVARIATE dataframes for the model , make sure to readapt the site_names AND add human/dog presence
urlfile100="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/cov_100.csv"
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
b100 <- left_join(b100, human_dog_df, by="site_name")

b500 <- read.csv(urlfile500)%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))
b500 <- left_join(b500, human_dog_df, by="site_name")

b2000 <- read.csv(urlfile2000)%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))
b2000 <- left_join(b2000, human_dog_df, by="site_name")

##call covariates
# cov<- b100 %>% select(-1, BUFF_DIST, -SHAPE_Length, -ORIG_FID, -SHAPE_Area)
# cov100 <- as.data.frame(scale(cov))
# 
# cov<- b500 %>% select(-1, BUFF_DIST, -SHAPE_Length, -ORIG_FID, -SHAPE_Area)
# cov500 <- as.data.frame(scale(cov))
# 
# cov<- b2000 %>% select(-1, BUFF_DIST, -SHAPE_Length, -ORIG_FID, -SHAPE_Area)
# cov2000 <- as.data.frame(scale(cov))


#### WAITING TIME SCRIPT ####

#set locale
locale = locale(date_format = "%Y-%m-%d",
                time_format = "%H:%M",
                tz = "UTC")

#set date + species name format
as.character(data$common_name) -> data$common_name
as.POSIXlt(data$DateTime, format="%Y-%m-%d %H:%M") -> data$DateTime
#WHY? as.POSIXlt(paste(data$DateTime), format="%Y-%m-%d %H:%M:%S") -> data$Date

##filter out unrevised sites + species not of interest
data1 <- data %>%
  filter(site_name %in% rev_sites)
  filter(common_name!="dog")%>%
  filter(common_name!="chipmunk")%>%
  filter(common_name!="groundhog")%>%
  filter(common_name!="skunk")%>%
  filter(common_name!="oppossum")

#create a summary (why?)
aver <- data1 %>%
  dplyr::group_by(common_name, site_name, .drop=FALSE) %>%
  dplyr::summarize(n())

##sort following time
data1 <- data1 %>% dplyr::arrange(DateTime)

#### JUMP TO LOAD RDS ####

##now the combination
###keep species names
  start_time <- Sys.time()                           #toggle to measure runtime
combn(data1$common_name, 2, simplify=TRUE) -> m              #takes 2 min to run
  end_time <- Sys.time()
  end_time - start_time
m[1,] -> Species_1                               
m[2,] -> Species_2                                

##keep station name (referred to as site_name in this .csv)
  start_time <- Sys.time()                           #toggle to measure runtime
combn(data1$site_name,2, simplify=TRUE) -> n                 #takes 2 min to run  
  end_time <- Sys.time()
  end_time - start_time
n[1,] -> site_name                                

  start_time <- Sys.time()                           #toggle to measure runtime
combn(as.character(data1$DateTime),2, simplify=TRUE) -> p    #takes 2 min to run
  end_time <- Sys.time()
  end_time - start_time
p[1,] -> a -> Date_1                     
p[2,] -> b -> Date_2                         

##now estimate time difference between records with previous combn
interval(a,b)->c                                             #takes 2 min to run
as.duration(c)->d
as.numeric(d)/3600-> e    #time interval in hours 
e -> time_interval

##join all combination vectors:
output_dataset <- data.frame(Species_1, Species_2, site_name, Date_1, Date_2, time_interval)
view(output_dataset)

#### LOAD OUTPUT DATASET RDS HERE ####
##save and/or call up resulting object to avoid heavy computation above 
#saveRDS(output_dataset, "output_dataset.rds")
output_dataset <- readRDS("output_dataset.rds")

levels(as.factor((output_dataset$Species_1))) ###to see the Levels you have

###select species of interest

rabbitfox <- output_dataset%>% subset( Species_1 =="rabbit") %>%
  subset(Species_2 == "fox")
#view(rabbitfox) 

deercoyote <- output_dataset%>% subset( Species_1 =="deer") %>%
  subset(Species_2 == "coyote")
#view(deercoyote) 

allprey_allpred <- output_dataset%>% subset( Species_1 =="rabbit" | Species_1 =="deer" | Species_1 =="squirrel" | Species_1 =="raccoon" | Species_1 =="cat") %>%
  subset(Species_2 == "fox" | Species_2 =="coyote")
#view(allprey_allpred) 

foxprey <- output_dataset%>% subset( Species_1 =="rabbit" | Species_1 =="squirrel" | Species_1 =="raccoon" | Species_1 =="cat") %>%
  subset(Species_2 == "fox")
#view(foxprey) 

#w/o cat & raccoon
foxprey2 <- output_dataset%>% subset( Species_1 =="rabbit" | Species_1 =="squirrel") %>%
  subset(Species_2 == "fox")
#view(foxprey2) 

coyprey <- output_dataset%>% subset( Species_1 =="rabbit" | Species_1 =="deer" | Species_1 =="squirrel" | Species_1 =="raccoon" | Species_1 =="cat") %>%
  subset(Species_2 =="coyote")
#view(coyprey) 


#In the real dataset it will be something more like this of Course.
#output_dataset%>% subset(Species_1 == "Dog" | Species_1 =="Coyote" | Species_1 =="Fox") %>%
#subset(Species_2 == "Dog" | Species_2 =="Coyote" | Species_2 =="Fox")

#plot predator-after-prey minimum waiting times 
min_deercoyote <- filter(deercoyote, time_interval > 0)
min_deercoyote <- min_deercoyote[!duplicated(min_deercoyote$Date_1),]
min_rabbitfox <- filter(rabbitfox, time_interval > 0)
min_rabbitfox <- min_rabbitfox[!duplicated(min_rabbitfox$Date_1),]
min_all <- filter(allprey_allpred, time_interval > 0)
min_all <- min_all[!duplicated(min_all$Date_1),]
min_foxprey <- filter(foxprey2, time_interval > 0)             #using foxprey2 (no cat or raccoon)
min_foxprey <- min_foxprey[!duplicated(min_foxprey$Date_1),]
min_coyprey <- filter(coyprey, time_interval > 0)
min_coyprey <- min_coyprey[!duplicated(min_coyprey$Date_1),]
min_coyprey24 <- filter(coyprey, time_interval > 0)
min_coyprey24 <- filter(coyprey, time_interval < 24)
min_coyprey24 <- min_coyprey24[!duplicated(min_coyprey24$Date_1),]
min_coyprey12<- filter(coyprey, time_interval > 0)
min_coyprey12 <- filter(coyprey, time_interval < 12)
min_coyprey12 <- min_coyprey12[!duplicated(min_coyprey12$Date_1),]

min_rabbitfox12 <- filter(rabbitfox, time_interval > 0)
min_rabbitfox12 <- filter(rabbitfox, time_interval < 12)
min_rabbitfox12 <- min_rabbitfox12[!duplicated(min_rabbitfox12$Date_1),]

#### PRED-PREY PLOTS ####

##COYOTE VS ALL PREY

#BOXPLOT
ggplot(data = min_coyprey, 
       mapping = aes(x = Species_1, y = time_interval)) +
  geom_boxplot(aes(fill = Species_1), show.legend = TRUE) +
  theme_bw() +
  labs(x = "Species",
       y = "Minimum-time between detection at same site (hour)", 
       title = "Minimum time-to-encounter, Prey then Coyote, 24 hrs") +
  scale_y_continuous(limits=c(0,24))
ggsave("waitingtime_coyprey_boxplot_24.png")

ggplot(data = min_coyprey, 
       mapping = aes(x = Species_1, y = time_interval)) +
  geom_boxplot(aes(fill = Species_1), show.legend = TRUE) +
  theme_bw() +
  labs(x = "Species",
       y = "Minimum-time between detection at same site (hour)", 
       title = "Minimum time-to-encounter, Prey then Coyote, 12 hrs") +
  scale_y_continuous(limits=c(0,12))
ggsave("waitingtime_coyprey_boxplot_12.png")

#DENSITY
ggplot(data = min_coyprey, 
       mapping = aes(x = time_interval, color = Species_1)) +
  geom_density()+
  theme_bw() +
  labs(x = "Minimum-time between detection at same site (hour)",
       y = "Density", 
       title = "Minimum time-to-encounter, Prey then Coyote, 24 hrs") +
  scale_x_continuous(limits=c(0,24))
ggsave("waitingtime_coyprey_density_24.png")

ggplot(data = min_coyprey, 
       mapping = aes(x = time_interval, color = Species_1)) +
  geom_density()+
  theme_bw() +
  labs(x = "Minimum-time between detection at same site (hour)",
       y = "Density", 
       title = "Minimum time-to-encounter, Prey then Coyote, 12 hrs") +
  scale_x_continuous(limits=c(0,12))
ggsave("waitingtime_coyprey_density_12.png")


##FOX VS ALL PREY

#BOXPLOT
ggplot(data = min_foxprey, 
       mapping = aes(x = Species_1, y = time_interval)) +
  geom_boxplot(aes(fill = Species_1), show.legend = TRUE) +
  theme_bw() +
  labs(x = "Species",
       y = "Minimum-time between detection at same site (hour)", 
       title = "Minimum time-to-encounter, Prey then Fox, 24 hrs") +
  scale_y_continuous(limits=c(0,24))
ggsave("waitingtime_foxprey_boxplot_24.png")

ggplot(data = min_foxprey, 
       mapping = aes(x = Species_1, y = time_interval)) +
  geom_boxplot(aes(fill = Species_1), show.legend = TRUE) +
  theme_bw() +
  labs(x = "Species",
       y = "Minimum-time between detection at same site (hour)", 
       title = "Minimum time-to-encounter, Prey then Fox, 12 hrs") +
  scale_y_continuous(limits=c(0,12))
ggsave("waitingtime_foxprey_boxplot_12.png")

#DENSITY
ggplot(data = min_foxprey, 
       mapping = aes(x = time_interval, color = Species_1)) +
  geom_density()+
  theme_bw() +
  labs(x = "Minimum-time between detection at same site (hour)",
       y = "Density", 
       title = "Minimum time-to-encounter, Prey then Fox, 24 hrs") +
  scale_x_continuous(limits=c(0,24))
ggsave("waitingtime_foxprey_density_24.png")

ggplot(data = min_foxprey, 
       mapping = aes(x = time_interval, color = Species_1)) +
  geom_density()+
  theme_bw() +
  labs(x = "Minimum-time between detection at same site (hour)",
       y = "Density", 
       title = "Minimum time-to-encounter, Prey then Fox, 12 hrs") +
  scale_x_continuous(limits=c(0,12))
ggsave("waitingtime_foxprey_density_12.png")


##OLDER PLOTS

# #Rabbit then fox
# ggplot(data = min_rabbitfox, 
#        mapping = aes(x = time_interval)) +
#   geom_histogram(aes(fill = site_name), show.legend = TRUE) +
#   theme_bw() +
#   labs(x = "Minimum-time between detection at same site (hour)",
#        y = "Count", 
#        title = "Minimum time-to-encounter, Rabbit-Fox") +
#   scale_x_continuous(limits=c(0,72))
# ggsave("waitingtime_rabbitfox_week2.png")
# 
# #Deer then coyote
# ggplot(data = min_deercoyote, 
#        mapping = aes(x = time_interval)) +
#   geom_histogram(aes(fill = site_name), show.legend = TRUE) +
#   theme_bw() +
#   labs(x = "Minimum-time between detection at same site (hour)",
#        y = "Count", 
#        title = "Minimum time-to-encounter, Deer-Coyote") +
#   scale_x_continuous(limits=c(0,72))
# ggsave("waitingtime_deercoyote_week2.png")
# 
# #All prey/predators
# ggplot(data = min_all, 
#        mapping = aes(x = time_interval)) +
#   geom_histogram(aes(fill = Species_2), show.legend = TRUE) +
#   theme_bw() +
#   labs(x = "Minimum-time between detection at same site (hour)",
#        y = "Count", 
#        title = "Minimum time-to-encounter, Predator-Prey") +
#   scale_x_continuous(limits=c(0,36))
# ggsave("waitingtime_allpredprey_week2.png")
# 
# #All coyote prey HISTO
# ggplot(data = min_coyprey, 
#        mapping = aes(x = time_interval)) +
#   geom_histogram(aes(fill = Species_1), show.legend = TRUE) +
#   theme_bw() +
#   labs(x = "Minimum-time between detection at same site (hour)",
#        y = "Count", 
#        title = "Minimum time-to-encounter, Prey then Coyote") +
#   scale_x_continuous(limits=c(0,72))
# ggsave("waitingtime_coyprey_week2.png")
# 
# #All coyote prey JITTER
# ggplot(data = min_coyprey, 
#        mapping = aes(x = Species_1, y = time_interval)) +
#   geom_jitter(aes(fill = Species_1))+
#   geom_boxplot(aes(fill = Species_1), show.legend = TRUE) +
#   theme_bw() +
#   labs(x = "Minimum-time between detection at same site (hour)",
#        y = "Count", 
#        title = "Minimum time-to-encounter, Prey then Coyote") +
#   scale_y_continuous(limits=c(0,72))
# #ggsave("waitingtime_coyprey_week4.png")
# 
# #Fox all prey HISTO
# ggplot(data = min_foxprey, 
#        mapping = aes(x = time_interval)) +
#   geom_histogram(aes(fill = Species_1), show.legend = TRUE) +
#   theme_bw() +
#   labs(x = "Minimum-time between detection at same site (hour)",
#        y = "Count", 
#        title = "Minimum time-to-encounter, Prey then Fox") +
#   scale_x_continuous(limits=c(0,72))
# ggsave("waitingtime_foxprey_72hr_histo1.png")


#### GLMM: REGRESSION (WAITING TIME AGAINST COVARIATES) ####


##**check gamlss package??**##

## Histograms to check for normal distribution

hist(x = coyprey$time_interval, breaks = 1000) #xlim = c(0, 100))

hist(x = min_coyprey$time_interval, breaks = 5000, xlim = c(0, 100))

hist(x = sqrt(min_coyprey$time_interval), breaks = 500, xlim = c(0, 10))

hist(x = log(min_coyprey$time_interval), breaks = 500)

install.packages("forecast")
library(forecast)

x <- min_coyprey$time_interval

Test <- BoxCox.lambda(x, method = c("guerrero", "loglik"), lower = -1, upper = 2)
Test2 <- BoxCox(x, Test)
hist(x = Test2, breaks = 500)

#now try with 12hr subset

hist(x = min_coyprey12$time_interval, breaks = 500, xlim = c(0, 100))

hist(x = sqrt(min_coyprey12$time_interval), breaks = 500, xlim = c(0, 10))

hist(x = log(min_coyprey12$time_interval), breaks = 500)

  #boxcox
  x2 <- min_coyprey12$time_interval
  
  Test <- BoxCox.lambda(x2, method = c("guerrero", "loglik"), lower = -1, upper = 2)
  Test2 <- BoxCox(x2, Test)
  hist(x = Test2, breaks = 500)
  
  
#now try with rabbit-fox 12hr subset (nocturnal only)
  
  hist(x = min_rabbitfox12$time_interval, breaks = 100, xlim = c(0, 100))
  
  hist(x = sqrt(min_rabbitfox12$time_interval), breaks = 100, xlim = c(0, 10))
  
  hist(x = log(min_rabbitfox12$time_interval), breaks = 100)
  
  #boxcox
  x3 <- min_rabbitfox12$time_interval
  
  Test <- BoxCox.lambda(x3, method = c("guerrero", "loglik"), lower = -1, upper = 2)
  Test2 <- BoxCox(x3, Test)
  hist(x = Test2, breaks = 100)
  

  
  min_rabbitfox12
## LET'S SEE
min_coyprey$time_interval_log <- log(min_coyprey$time_interval)
min_coyprey$time_interval_sq <- sqrt(min_coyprey$time_interval)
min_coyprey$time_interval_box <- Test2

## Join waiting time dataset w/ covariates

#please note this is the old covariate file w/ no veg complexity indicators
coyprey_reg1 <-left_join(min_coyprey, cov, by="site_name")%>%
  mutate(Tree_PA.500 = scale(Tree_PA.500))%>%
  mutate(NDVI.500 = scale(NDVI.500))%>%
  mutate(Built_PA.500 = scale(Built_PA.500))%>%
  mutate(POP.500 = scale(POP.500))%>%
  mutate(DEM.500 = scale(DEM.500))

library(lme4)

try1_NDVI <- glmer(time_interval ~ 0+Species_1+Species_1:NDVI.500+(1|site_name), data = coyprey_reg1, family = Gamma(link="log"))
summary(try1_NDVI)

try1_built <- glmer(time_interval ~ 0+Species_1+Species_1:Built_PA.500+(1|site_name), data = coyprey_reg1, family = Gamma(link="log"))
summary(try1_built)

try2_NDVI_log <- glmer(time_interval_log ~ 0+Species_1+Species_1:NDVI.500+(1|site_name), data = coyprey_reg1)
summary(try2_NDVI)

try2_NDVI_sq <- glmer(time_interval_sq ~ 0+Species_1+Species_1:NDVI.500+(1|site_name), data = coyprey_reg1)
summary(try2_NDVI)

try2_NDVI_box <- glmer(time_interval_box ~ 0+Species_1+Species_1:NDVI.500+(1|site_name), data = coyprey_reg1)
summary(try2_NDVI)

plot(try1_NDVI)
plot(try2_NDVI)

### What does it mean?????



#### PLOT WAITING TIMES AGAINST A SPECIFIC COVARIATE ####

## 1- coyote after deer 
## Join waiting time w/ covariates
deercoy_cov <-left_join(min_deercoyote, cov, by="site_name")
deercoy_NDVI <-   deercoy_cov%>%
  dplyr::select("Species_1", "Species_2", "site_name", "time_interval", "NDVI.500") %>%
  mutate(NDVI = NDVI.500)%>%
  mutate(NDVI = ifelse(NDVI.500 < 0.2, "low",NDVI))%>%
  mutate(NDVI = ifelse(NDVI.500 > 0.2, "mid",NDVI))%>%
  mutate(NDVI = ifelse(NDVI.500 > 0.4, "high",NDVI))
  
ggplot(data = deercoy_NDVI, 
         mapping = aes(x = time_interval, color = NDVI)) +
  geom_density()+
  theme_bw() +
  labs(x = "Minimum-time between detection at same site (hour)",
       y = "Count", 
       title = "Minimum time-to-encounter, Deer then Coyote") +
  scale_x_continuous(limits=c(0,72))
#ggsave(".png")

##1.1 coyote after all prey

coyprey_cov <-left_join(min_coyprey, cov, by="site_name")
coyprey_NDVI <- coyprey_cov%>%
  dplyr::select("Species_1", "Species_2", "time_interval", "NDVI.500") %>%
  #filter(Species_1 != "cat") %>%
  mutate(NDVI = NDVI.500)%>%
  mutate(NDVI = ifelse(NDVI.500 < 0.2, "low",NDVI))%>%
  mutate(NDVI = ifelse(NDVI.500 > 0.2, "mid",NDVI))%>%
  mutate(NDVI = ifelse(NDVI.500 > 0.4, "high",NDVI))

ggplot(data = coyprey_NDVI, 
       mapping = aes(x = time_interval, color = Species_1, linetype = NDVI)) +
  geom_density()+
  theme_bw() +
  labs(x = "Minimum-time between detection at same site (hour)",
       y = "Count", 
       title = "Minimum time-to-encounter, Coyote after all prey") +
  scale_x_continuous(limits=c(0,72))
#ggsave(".png")

##1.2 coyote after all prey - DENSITY/PRESENCE WITHIN 8 HRS

coyprey_NDVI8 <- coyprey_cov%>%
  dplyr::select("Species_1", "Species_2", "time_interval", "NDVI.500") %>%
  filter(Species_1 != "cat") %>%
  filter(time_interval < 8)

coydeer_NDVI8 <- coyprey_cov%>%
  dplyr::select("Species_1", "Species_2", "time_interval", "NDVI.500") %>%
  filter(Species_1 == "deer") %>%
  filter(time_interval < 8)

ggplot(data = coyprey_NDVI8, 
       mapping = aes(x = NDVI.500, color = Species_1)) +
  geom_density()+
  theme_bw() +
  labs(x = "NDVI",
       title = "Coyote presence within 8 hr of prey")
#ggsave(".png")

ggplot(data = coyprey_NDVI8, 
       mapping = aes(x = NDVI.500, color = Species_1)) +
  geom_histogram(position = "dodge")+
  theme_bw() +
  labs(x = "NDVI",
       title = "Coyote presence within 8 hr of prey")
#ggsave(".png")

ggplot(data = coydeer_NDVI8, 
       mapping = aes(x = NDVI.500, fill = Species_1)) +
  geom_histogram(position = "dodge")+
  theme_bw() +
  labs(x = "NDVI",
       title = "Coyote presence within 8 hr of deer")
#ggsave(".png")



## 2.0 - fox after rabbit

## Join waiting time w/ covariates
rabfox_cov <-left_join(min_rabbitfox, cov, by="site_name")
rabfox_NDVI <-   rabfox_cov%>%
  dplyr::select("Species_1", "Species_2", "site_name", "time_interval", "NDVI.500") %>%
  mutate(NDVI = NDVI.500)%>%
  mutate(NDVI = ifelse(NDVI.500 < 0.2, "low",NDVI))%>%
  mutate(NDVI = ifelse(NDVI.500 > 0.2, "mid",NDVI))%>%
  mutate(NDVI = ifelse(NDVI.500 > 0.4, "high",NDVI))

ggplot(data = rabfox_NDVI, 
       mapping = aes(x = time_interval, color = NDVI)) +
  geom_density()+
  theme_bw() +
  labs(x = "Minimum-time between detection at same site (hour)",
       y = "Count", 
       title = "Minimum time-to-encounter, Rabbit then Fox") +
  scale_x_continuous(limits=c(0,72))
#ggsave(".png")

##2.1 fox after all prey

foxprey_cov <-left_join(min_foxprey, cov, by="site_name")
foxprey_NDVI <- foxprey_cov%>%
  dplyr::select("Species_1", "Species_2", "time_interval", "NDVI.500") %>%
  mutate(NDVI = NDVI.500)%>%
  mutate(NDVI = ifelse(NDVI.500 < 0.2, "low",NDVI))%>%
  mutate(NDVI = ifelse(NDVI.500 > 0.2, "mid",NDVI))%>%
  mutate(NDVI = ifelse(NDVI.500 > 0.4, "high",NDVI))

ggplot(data = foxprey_NDVI, 
       mapping = aes(x = time_interval, color = Species_1, linetype = NDVI)) +
  geom_density()+
  theme_bw() +
  labs(x = "Minimum-time between detection at same site (hour)",
       y = "Count", 
       title = "Minimum time-to-encounter, Prey then Fox") +
  scale_x_continuous(limits=c(0,72))
#ggsave(".png")


##2.2 fox after all prey - DENSITY/PRESENCE WITHIN 8 HRS

foxprey_NDVI8 <- foxprey_NDVI%>%
  filter(Species_1 != "cat") %>%
  filter(time_interval < 8)

rabfox_NDVI8 <- foxprey_NDVI%>%
  filter(Species_1 == "rabbit") %>%
  filter(time_interval < 8)


ggplot(data = foxprey_NDVI8, 
       mapping = aes(x = NDVI.500, color = Species_1)) +
  geom_density()+
  theme_bw() +
  labs(x = "NDVI",
       title = "Fox presence within 8 hr of prey")
#ggsave(".png")

ggplot(data = foxprey_NDVI8, 
       mapping = aes(x = NDVI.500, color = Species_1)) +
  geom_histogram(position = "dodge")+
  theme_bw() +
  labs(x = "NDVI",
       title = "Coyote presence within 8 hr of prey")
#ggsave(".png")

ggplot(data = rabfox_NDVI8, 
       mapping = aes(x = NDVI.500, fill = Species_1)) +
  geom_histogram(position = "dodge")+
  theme_bw() +
  labs(x = "NDVI",
       title = "Fox presence within 8 hr of rabbit")
#ggsave(".png")



#### COMPETITION PLOTS ####

comp <- output_dataset%>% subset( Species_1 =="coyote" | Species_1 =="fox" | Species_1 =="raccoon") %>%
  subset(Species_2 == "fox" | Species_2 =="coyote" | Species_2 =="raccoon")
#view(comp) 

min_comp <- filter(comp, time_interval > 0)
min_comp <- min_comp[!duplicated(min_comp$Date_1),]
min_comp_coy <- min_comp %>% filter(Species_2 == "coyote") %>% filter(Species_1 != "coyote") 
min_comp_fox <- min_comp %>% filter(Species_2 == "fox") %>% filter(Species_1 != "fox") 
min_comp_rac <- min_comp %>% filter(Species_2 == "raccoon") %>% filter(Species_1 != "raccoon") 

#BOXPLOT
ggplot(mapping = aes(x = Species_1, y = time_interval)) +
  geom_boxplot(data = min_comp_coy, aes(fill = Species_1), show.legend = TRUE) +
  geom_boxplot(data = min_comp_fox, aes(fill = Species_1), show.legend = TRUE) +
  geom_boxplot(data = min_comp_rac, aes(fill = Species_1), show.legend = TRUE) +
  facet_wrap(~Species_2, scale = "free_x") +
  theme_bw() +
  labs(x = "Species",
       y = "Minimum-time between detection at same site (hour)", 
       title = "Minimum time-to-encounter, Competing Mesopredators, 24 hrs") +
  scale_y_continuous(limits=c(0,24))
ggsave("waitingtime_comp_boxplot_24.png")

ggplot(mapping = aes(x = Species_1, y = time_interval)) +
  geom_boxplot(data = min_comp_coy, aes(fill = Species_1), show.legend = TRUE) +
  geom_boxplot(data = min_comp_fox, aes(fill = Species_1), show.legend = TRUE) +
  geom_boxplot(data = min_comp_rac, aes(fill = Species_1), show.legend = TRUE) +
  facet_wrap(~Species_2, scale = "free_x") +
  theme_bw() +
  labs(x = "Species",
       y = "Minimum-time between detection at same site (hour)", 
       title = "Minimum time-to-encounter, Competing Mesopredators, 12 hrs") +
  scale_y_continuous(limits=c(0,12))
ggsave("waitingtime_comp_boxplot_12.png")

#DENSITY
ggplot(mapping = aes(x = time_interval, color = Species_1)) +
  geom_density(data = min_comp_coy)+
  geom_density(data = min_comp_fox)+
  geom_density(data = min_comp_rac)+
  facet_wrap(~Species_2)+
  theme_bw() +
  labs(x = "Minimum-time between detection at same site (hour)",
       y = "Density", 
       title = "Minimum time-to-encounter, Competing Mesopredators, 24 hrs") +
  scale_x_continuous(limits=c(0,24))
ggsave("waitingtime_comp_density_24.png")

ggplot(mapping = aes(x = time_interval, color = Species_1)) +
  geom_density(data = min_comp_coy)+
  geom_density(data = min_comp_fox)+
  geom_density(data = min_comp_rac)+
  facet_wrap(~Species_2)+
  theme_bw() +
  labs(x = "Minimum-time between detection at same site (hour)",
       y = "Density", 
       title = "Minimum time-to-encounter, Competing Mesopredators, 12 hrs") +
  scale_x_continuous(limits=c(0,12))
ggsave("waitingtime_comp_density_12.png")


#### SEASONAL PLOTS ####

view(min_coyprey)
view(min_foxprey)

min_coyprey$season <- min_coyprey$Date_1
min_coyprey$season <- month(min_coyprey$season)
min_coyprey$season <- as.character(min_coyprey$season)
min_coyprey$season <- factor(min_coyprey$season, levels = c("1", "4", "7", "10"))


min_foxprey$season <- min_foxprey$Date_1
min_foxprey$season <- month(min_foxprey$season)
min_foxprey$season <- as.character(min_foxprey$season)
min_foxprey$season <- factor(min_foxprey$season, levels = c("1", "4", "7", "10"))


##COYOTE VS ALL PREY - SEASONAL

#BOXPLOT
ggplot(data = min_coyprey, 
       mapping = aes(x = Species_1, y = time_interval)) +
  geom_boxplot(aes(fill = season), show.legend = TRUE) +
  theme_bw() +
  facet_grid(~Species_1, scale = "free_x")+
  labs(x = "Species",
       y = "Minimum-time between detection at same site (hour)", 
       title = "Minimum time-to-encounter, Prey then Coyote, 24 hrs") +
  scale_fill_discrete(name = "Season", labels = c("Winter", "Spring", "Summer", "Fall"))+
  theme(strip.text.x = element_blank())+
  scale_y_continuous(limits=c(0,24))
ggsave("WT_seasonal_coyprey_boxplot_24.png")

ggplot(data = min_coyprey, 
       mapping = aes(x = Species_1, y = time_interval)) +
  geom_boxplot(aes(fill = season), show.legend = TRUE) +
  theme_bw() +
  facet_grid(~Species_1, scale = "free_x")+
  labs(x = "Species",
       y = "Minimum-time between detection at same site (hour)", 
       title = "Minimum time-to-encounter, Prey then Coyote, 12 hrs") +
  scale_fill_discrete(name = "Season", labels = c("Winter", "Spring", "Summer", "Fall"))+
  theme(strip.text.x = element_blank())+
  scale_y_continuous(limits=c(0,12))
ggsave("WT_seasonal_coyprey_boxplot_12.png")

#DENSITY
ggplot(data = min_coyprey, 
       mapping = aes(x = time_interval, color = season)) +
  geom_density()+
  facet_grid(~Species_1)+
  theme_bw() +
  labs(x = "Minimum-time between detection at same site (hour)",
       y = "Density", 
       title = "Minimum time-to-encounter, Prey then Coyote, 24 hrs") +
  scale_color_discrete(name = "Season", labels = c("Winter", "Spring", "Summer", "Fall"))+
  scale_x_continuous(limits=c(0,24))
ggsave("WT_seasonal_coyprey_density_24.png")

ggplot(data = min_coyprey, 
       mapping = aes(x = time_interval, color = season)) +
  geom_density()+
  facet_grid(~Species_1)+
  theme_bw() +
  labs(x = "Minimum-time between detection at same site (hour)",
       y = "Density", 
       title = "Minimum time-to-encounter, Prey then Coyote, 12 hrs") +
  scale_color_discrete(name = "Season", labels = c("Winter", "Spring", "Summer", "Fall"))+
  scale_x_continuous(limits=c(0,12))
ggsave("WT_seasonal_coyprey_density_12.png")


##FOX VS ALL PREY - SEASONAL

#BOXPLOT
ggplot(data = min_foxprey, 
       mapping = aes(x = Species_1, y = time_interval)) +
  geom_boxplot(aes(fill = season), show.legend = TRUE) +
  theme_bw() +
  facet_grid(~Species_1, scale = "free_x")+
  labs(x = "Species",
       y = "Minimum-time between detection at same site (hour)", 
       title = "Minimum time-to-encounter, Prey then Fox, 24 hrs") +
  scale_fill_discrete(name = "Season", labels = c("Winter", "Spring", "Summer", "Fall"))+
  theme(strip.text.x = element_blank())+
  scale_y_continuous(limits=c(0,24))
ggsave("WT_seasonal_foxprey_boxplot_24.png")

ggplot(data = min_foxprey, 
       mapping = aes(x = Species_1, y = time_interval)) +
  geom_boxplot(aes(fill = season), show.legend = TRUE) +
  theme_bw() +
  facet_grid(~Species_1, scale = "free_x")+
  labs(x = "Species",
       y = "Minimum-time between detection at same site (hour)", 
       title = "Minimum time-to-encounter, Prey then Fox, 12 hrs") +
  scale_fill_discrete(name = "Season", labels = c("Winter", "Spring", "Summer", "Fall"))+
  theme(strip.text.x = element_blank())+
  scale_y_continuous(limits=c(0,12))
ggsave("WT_seasonal_foxprey_boxplot_12.png")

#DENSITY
ggplot(data = min_foxprey, 
       mapping = aes(x = time_interval, color = season)) +
  geom_density()+
  facet_grid(~Species_1)+
  theme_bw() +
  labs(x = "Minimum-time between detection at same site (hour)",
       y = "Density", 
       title = "Minimum time-to-encounter, Prey then Fox, 24 hrs") +
  scale_color_discrete(name = "Season", labels = c("Winter", "Spring", "Summer", "Fall"))+
  scale_x_continuous(limits=c(0,24))
ggsave("WT_seasonal_foxprey_density_24.png")

ggplot(data = min_foxprey, 
       mapping = aes(x = time_interval, color = season)) +
  geom_density()+
  facet_grid(~Species_1)+
  theme_bw() +
  labs(x = "Minimum-time between detection at same site (hour)",
       y = "Density", 
       title = "Minimum time-to-encounter, Prey then Fox, 12 hrs") +
  scale_color_discrete(name = "Season", labels = c("Winter", "Spring", "Summer", "Fall"))+
  scale_x_continuous(limits=c(0,12))
ggsave("WT_seasonal_foxprey_density_12.png")


##COMPETITORS

min_comp_coy$season <- min_comp_coy$Date_1
min_comp_coy$season <- month(min_comp_coy$season)
min_comp_coy$season <- as.character(min_comp_coy$season)
min_comp_coy$season <- factor(min_comp_coy$season, levels = c("1", "4", "7", "10"))

min_comp_fox$season <- min_comp_fox$Date_1
min_comp_fox$season <- month(min_comp_fox$season)
min_comp_fox$season <- as.character(min_comp_fox$season)
min_comp_fox$season <- factor(min_comp_fox$season, levels = c("1", "4", "7", "10"))

min_comp_rac$season <- min_comp_rac$Date_1
min_comp_rac$season <- month(min_comp_rac$season)
min_comp_rac$season <- as.character(min_comp_rac$season)
min_comp_rac$season <- factor(min_comp_rac$season, levels = c("1", "4", "7", "10"))

### **not plotted yet**

