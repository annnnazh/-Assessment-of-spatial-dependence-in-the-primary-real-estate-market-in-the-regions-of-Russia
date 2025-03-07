install.packages('Hmisc', type='binary')
library(Hmisc)

library(spdep)
library(sf)
library(sp)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(openxlsx)
library(haven)
library(dplyr)
library(spdep)
library(spatialreg)
library(lmtest)
library(spatialreg)
library(plm)

setwd("C:/Users/annaz/Documents/HSE-2/Spatial econometrics")

data <- read.xlsx("data_SE.xlsx",colNames = T)
data$primary <- as.numeric(data$primary)
data$secondary <- as.numeric(data$secondary)
data <- data %>% filter(NAME_1 != 'Chukot' & NAME_1 != 'Murmansk')
is.na.data.frame(data)



data2011 <- data %>% filter(year == 2011)
data2012 <- data %>% filter(year == 2012)
data2013 <- data %>% filter(year == 2013)
data2014 <- data %>% filter(year == 2014)
data2015 <- data %>% filter(year == 2015)
data2016 <- data %>% filter(year == 2016)
data2017 <- data %>% filter(year == 2017)
data2018 <- data %>% filter(year == 2018)
data2019 <- data %>% filter(year == 2019)
data2020 <- data %>% filter(year == 2020)
data2021 <- data %>% filter(year == 2021)
data2022 <- data %>% filter(year == 2022)


data2011$primary <- impute(data2011$primary, mean)
data2012$primary <- impute(data2012$primary, mean)
data2013$primary <- impute(data2013$primary, mean)
data2014$primary <- impute(data2014$primary, mean)
data2015$primary <- impute(data2015$primary, mean)
data2016$primary <- impute(data2016$primary, mean)
data2017$primary <- impute(data2017$primary, mean)
data2018$primary <- impute(data2018$primary, mean)
data2019$primary <- impute(data2019$primary, mean)
data2020$primary <- impute(data2020$primary, mean)
data2021$primary <- impute(data2021$primary, mean)
data2022$primary <- impute(data2022$primary, mean)


## Add info for map:
st_layers("gadm41_RUS.gpkg")
ru_reg<-st_read("gadm41_RUS.gpkg", layer = "ADM_ADM_1")
#ru_reg %>% st_geometry() %>% plot()
#We will use different projection - 3576
ru_reg_3576 <- st_transform(ru_reg, 3576)
#ru_reg_3576 <- left_join(ru_reg_3576, avg_data, by = "NAME_1")
ru_reg_3576 = ru_reg_3576 %>% filter(NAME_1 != 'Chukot') %>% filter(NAME_1 != 'Murmansk')
ru_reg_3576 %>% st_geometry() %>% plot() 

head(ru_reg_3576)

ru_reg_3576 %>% ggplot(aes(fill = primary)) +
  geom_sf(lwd = 0.2) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.key.width = unit(1, "cm")) +
  scale_fill_distiller() +
  labs(title = "The main plot")



## Weighted matrix
nbr = poly2nb(ru_reg_3576)

str(nbr)
summary(nbr)

nbr[[20]] = c(as.integer(13), as.integer(55), as.integer(37)) #SPb and Penza and Leningrad oblast' to Kaliningrad
nbr[[59]] = c(as.integer(27), as.integer(54)) #Primor'ye and Khabarovsk to Sakhalin

# make symmetric for neighbours
nbr[[13]] = c(nbr[[13]], as.integer(20))
nbr[[55]] = c(nbr[[55]], as.integer(20))
nbr[[37]] = c(nbr[[37]], as.integer(20))
nbr[[27]] = c(nbr[[27]], as.integer(59))
nbr[[55]] = c(nbr[[55]], as.integer(59))

summary(nbr)
W_1 <- nb2mat(nbr, style = "W")  ##  деленная на число соседей


# coordinates() computes coordinates of centroids for each region
ru_reg_3576_sp <- as(ru_reg_3576, "Spatial")
coord <- coordinates(ru_reg_3576_sp)
dist <- as.matrix(dist(coord)) # dist() computes Euclidean distance between centroids


knn_list <- knearneigh(coord, k=5)
nbr_knn <- knn2nb(knn_list)
W_2 <- nb2mat(nbr_knn)
unique(rowSums(W_2))


ru_reg_3576 <- ru_reg_3576 %>% dplyr::select(-c("VARNAME_1", "TYPE_1", "ENGTYPE_1",
                                                "CC_1", "HASC_1", "ISO_1", "GID_1", "GID_0", "COUNTRY"))

ru_reg_3576_2011 <- left_join(ru_reg_3576, data2011, by = "NAME_1")
ru_reg_3576_2012 <- left_join(ru_reg_3576, data2012, by = "NAME_1")
ru_reg_3576_2013 <- left_join(ru_reg_3576, data2013, by = "NAME_1")
ru_reg_3576_2014 <- left_join(ru_reg_3576, data2014, by = "NAME_1")
ru_reg_3576_2015 <- left_join(ru_reg_3576, data2015, by = "NAME_1")
ru_reg_3576_2016 <- left_join(ru_reg_3576, data2011, by = "NAME_1")
ru_reg_3576_2017 <- left_join(ru_reg_3576, data2017, by = "NAME_1")
ru_reg_3576_2018 <- left_join(ru_reg_3576, data2018, by = "NAME_1")
ru_reg_3576_2019 <- left_join(ru_reg_3576, data2019, by = "NAME_1")
ru_reg_3576_2020 <- left_join(ru_reg_3576, data2020, by = "NAME_1")
ru_reg_3576_2021 <- left_join(ru_reg_3576, data2021, by = "NAME_1")
ru_reg_3576_2022 <- left_join(ru_reg_3576, data2022, by = "NAME_1")

######################## Global autocorrelation ########################
## na = mean
data2011$primary <- impute(data2011$primary, mean)
data2012$primary <- impute(data2012$primary, mean)
data2013$primary <- impute(data2013$primary, mean)
data2014$primary <- impute(data2014$primary, mean)
data2015$primary <- impute(data2015$primary, mean)
data2016$primary <- impute(data2016$primary, mean)
data2017$primary <- impute(data2017$primary, mean)
data2018$primary <- impute(data2018$primary, mean)
data2019$primary <- impute(data2019$primary, mean)
data2020$primary <- impute(data2020$primary, mean)
data2021$primary <- impute(data2021$primary, mean)
data2022$primary <- impute(data2022$primary, mean)

# let's start with Moran's I
## Lag Y

WY_2011 <- W_1 %*% data2011$primary
WY_2012 <- W_1 %*% data2012$primary
WY_2013 <- W_1 %*% data2013$primary
WY_2014 <- W_1 %*% data2014$primary
WY_2015 <- W_1 %*% data2015$primary
WY_2016 <- W_1 %*% data2016$primary
WY_2017 <- W_1 %*% data2017$primary
WY_2018 <- W_1 %*% data2018$primary
WY_2019 <- W_1 %*% data2019$primary
WY_2020 <- W_1 %*% data2020$primary
WY_2021 <- W_1 %*% data2021$primary
WY_2022 <- W_1 %*% data2022$primary

# then we estimate a simple OLS linear regression
m_2011 <- lm(WY_2011 ~ data2011$primary)
m_2012 <- lm(WY_2012 ~ data2012$primary)
m_2013 <- lm(WY_2013 ~ data2013$primary)
m_2014 <- lm(WY_2014 ~ data2014$primary)
m_2015 <- lm(WY_2015 ~ data2015$primary)
m_2016 <- lm(WY_2016 ~ data2016$primary)
m_2017 <- lm(WY_2017 ~ data2017$primary)
m_2018 <- lm(WY_2018 ~ data2018$primary)
m_2019 <- lm(WY_2019 ~ data2019$primary)
m_2020 <- lm(WY_2020 ~ data2020$primary)
m_2021 <- lm(WY_2021 ~ data2021$primary)
m_2022 <- lm(WY_2022 ~ data2022$primary)

# here we see that the slope coefficient estimate is 0.3768 
summary(m_2011)
summary(m_2012)
summary(m_2013)
summary(m_2014)
summary(m_2015)
summary(m_2016)
summary(m_2017)
summary(m_2018)
summary(m_2019)
summary(m_2020)
summary(m_2021)
summary(m_2022)

W <- mat2listw(W_1, style = "W")

#2011
moran.test(ru_reg_3576_2011$primary, listw = W)
moran.test(ru_reg_3576_2011$primary, listw = W, alternative = "two.sided")
moran.test(ru_reg_3576_2011$primary, listw = W, alternative = "less")  

geary.test(as.vector(ru_reg_3576_2011$primary), listw = W)
globalG.test(as.vector(ru_reg_3576_2011$primary), listw = W)

#2012
moran.test(ru_reg_3576_2012$primary, listw = W)
moran.test(ru_reg_3576_2012$primary, listw = W, alternative = "two.sided")
moran.test(ru_reg_3576_2012$primary, listw = W, alternative = "less")  

geary.test(as.vector(ru_reg_3576_2012$primary), listw = W)
globalG.test(as.vector(ru_reg_3576_2012$primary), listw = W)

#2013
moran.test(ru_reg_3576_2013$primary, listw = W)
moran.test(ru_reg_3576_2013$primary, listw = W, alternative = "two.sided")
moran.test(ru_reg_3576_2013$primary, listw = W, alternative = "less")  

geary.test(as.vector(ru_reg_3576_2013$primary), listw = W)
globalG.test(as.vector(ru_reg_3576_2013$primary), listw = W)

#2014
moran.test(ru_reg_3576_2014$primary, listw = W)
moran.test(ru_reg_3576_2014$primary, listw = W, alternative = "two.sided")
moran.test(ru_reg_3576_2014$primary, listw = W, alternative = "less")

geary.test(as.vector(ru_reg_3576_2014$primary), listw = W)
globalG.test(as.vector(ru_reg_3576_2014$primary), listw = W)








## Local indices
loc_m_2011 <- localmoran(as.vector(ru_reg_3576_2011$primary), listw = W)
loc_m_2012 <- localmoran(as.vector(ru_reg_3576_2012$primary), listw = W)
loc_m_2013 <- localmoran(as.vector(ru_reg_3576_2013$primary), listw = W)
loc_m_2014 <- localmoran(as.vector(ru_reg_3576_2014$primary), listw = W)
loc_m_2015 <- localmoran(as.vector(ru_reg_3576_2015$primary), listw = W)
loc_m_2016 <- localmoran(as.vector(ru_reg_3576_2016$primary), listw = W)
loc_m_2017 <- localmoran(as.vector(ru_reg_3576_2017$primary), listw = W)
loc_m_2018 <- localmoran(as.vector(ru_reg_3576_2018$primary), listw = W)
loc_m_2019 <- localmoran(as.vector(ru_reg_3576_2019$primary), listw = W)
loc_m_2020 <- localmoran(as.vector(ru_reg_3576_2020$primary), listw = W)
loc_m_2021 <- localmoran(as.vector(ru_reg_3576_2021$primary), listw = W)
loc_m_2022 <- localmoran(as.vector(ru_reg_3576_2022$primary), listw = W)


mean_primary_2022 <- mean(ru_reg_3576_2022$primary)
mean_WY_2022 <- mean(WY_2022)

mean(loc_m_2022[,1])
hist(loc_m_2022[,5])

### Local Moran's Index's Map for 2022
q_1 <- c(1:81)

for (i in 1:81) {
  if (ru_reg_3576_2022$primary[[i]] >= mean_primary_2022 &
      WY_2022[[i]] >= mean_WY_2022) q_1[i] <- 1
  if (ru_reg_3576_2022$primary[[i]] < mean_primary_2022 &
      WY_2022[[i]] < mean_WY_2022) q_1[i] <- 2
  if (ru_reg_3576_2022$primary[[i]] > mean_primary_2022 &
      WY_2022[[i]] < mean_WY_2022) q_1[i] <- 3
  if (ru_reg_3576_2022$primary[[i]] < mean_primary_2022 &
      WY_2022[[i]] > mean_WY_2022) q_1[i] <- 4
}

q_col <- c("red", "blue", "pink", "lightblue")
colors <- q_col[q_1]

plot(ru_reg_3576_2022$geom, col = colors)
legend("bottomright", legend = c("high-high", "low-low", "high-low", "low-high"),
       fill = q_col)



### Local Moran's Index with sampling (permutations) and  Map for 2022
loc_m_perm <- localmoran_perm(as.vector(ru_reg_3576_2022$primary), listw = W,
                              nsim = 9999)

significance <- 0.7
signif <- ifelse(loc_m_perm[, 6] < significance, 1, 0)

q_2 <- c(1:81)
for (i in 1:81) {
  if (ru_reg_3576_2022$primary[[i]] >= mean_primary_2022 &
      WY_2022[[i]] >= mean_WY_2022 & signif[[i]] == 1) q_2[i] <- "high-high"
  if (ru_reg_3576_2022$primary[[i]] < mean_primary_2022 &
      WY_2022[[i]] < mean_WY_2022 & signif[[i]] == 1) q_2[i] <- "low-low"
  if (ru_reg_3576_2022$primary[[i]] > mean_primary_2022 &
      WY_2022[[i]] < mean_WY_2022 & signif[[i]] == 1) q_2[i] <- "high-low"
  if (ru_reg_3576_2022$primary[[i]] < mean_primary_2022 &
      WY_2022[[i]] > mean_WY_2022 & signif[[i]] == 1) q_2[i] <- "low-high"
  if (signif[i]== 0) q_2[i] <- "non-significant"
}

ggplot(ru_reg_3576_2022, aes(fill = q_2)) +
  geom_sf(lwd = 0.2) +
  scale_fill_manual(values = c("high-high" = "red", "low-low" = "blue", "high-low" =
                                 "salmon1", "low-high" = "dodgerblue1", 
                               "non-significant" = "grey90"))


### Local Geary  Index with sampling (permutations) and pseudo-p, and  Map for 2022
local_c_perm <- localC_perm(as.vector(ru_reg_3576_2022$primary), listw = W,
                            nsim = 9999)
local_c_pv <- attr(local_c_perm, "pseudo-p")
local_c_clusters <- attr(local_c_perm, "pseudo-p")

q_3 <- c(1:81)
for (i in 1:81) {
  if (ru_reg_3576_2022$primary[[i]] < mean_primary_2022 &
      WY_2022[[i]] >= mean_WY_2022 & signif[[i]] == 1) q_2[i] <- "high-high"
  if (ru_reg_3576_2022$primary[[i]] < mean_primary_2022 &
      WY_2022[[i]] < mean_WY_2022 & signif[[i]] == 1) q_2[i] <- "low-low"
  if (ru_reg_3576_2022$primary[[i]] > mean_primary_2022 &
      WY_2022[[i]] < mean_WY_2022 & signif[[i]] == 1) q_2[i] <- "high-low"
  if (ru_reg_3576_2022$primary[[i]] < mean_primary_2022 &
      WY_2022[[i]] > mean_WY_2022 & signif[[i]] == 1) q_2[i] <- "low-high"
  if (signif[i]== 0) q_2[i] <- "non-significant"
}

ggplot(ru_reg_3576_2022, aes(fill = q_2)) +
  geom_sf(lwd = 0.2) +
  scale_fill_manual(values = c("high-high" = "red", "low-low" = "blue", "high-low" =
                                 "salmon1", "low-high" = "dodgerblue1", 
                               "non-significant" = "grey90"))

q_3
local_c_pv[[1,5]]









## Непонятно как это интерпретировать, поэтому, думаю, не нужно их так считать:

## Avarage Local indexes 
## Calculate the mean primary price
data_ru_reg <- left_join(data, ru_reg_3576, by = "NAME_1")
data_ru_reg$primary
data_ru_reg$primary <- impute(data_ru_reg$primary, mean)


#loc_m <- localmoran(as.vector(data_ru_reg$primary), listw = W) ### пока не представляю как это посчитать
#mean_primary <- mean(data_ru_reg$primary)  ## слишком маленькое значение, видимо из-за замены пропусков и разных подходов к расчётам - такие отличия

mean_primary <- mean(c(mean_primary_2011,mean_primary_2012,mean_primary_2013,mean_primary_2014,
                       mean_primary_2015,mean_primary_2016,mean_primary_2017,mean_primary_2018,
                       mean_primary_2019,mean_primary_2020, mean_primary_2021, mean_primary_2022))

mean_WY <- mean(c(WY_2011, WY_2012, WY_2013, WY_2014, WY_2015, WY_2016, WY_2017,
                  WY_2018, WY_2019, WY_2020, WY_2021, WY_2022))


### AVARAGE Local Moran's Index's Map
q_1 <- c(1:81)

for (i in 1:81) {
  if (data_ru_reg$primary[[i]] >= mean_primary &
      WY[[i]] >= mean_WY) q_1[i] <- 1
  if (data_ru_reg$primary[[i]] < mean_primary &
      WY[[i]] < mean_WY) q_1[i] <- 2
  if (data_ru_reg$primary[[i]] > mean_primary &
      WY[[i]] < mean_WY) q_1[i] <- 3
  if (data_ru_reg$primary[[i]] < mean_primary &
      WY[[i]] > mean_WY) q_1[i] <- 4
}

q_col <- c("red", "blue", "pink", "lightblue")
colors <- q_col[q_1]

plot(data_ru_reg$geom, col = colors)
legend("bottomright", legend = c("high-high", "low-low", "high-low", "low-high"),
       fill = q_col)



### AVARAGE  Local Moran's Index with sampling (permutations) and  Map
loc_m_perm <- localmoran_perm(data_ru_reg$primary, listw = W,
                              nsim = 9999)

significance <- 0.7
signif <- ifelse(loc_m_perm[, 6] < significance, 1, 0)

q_2 <- c(1:81)
for (i in 1:81) {
  if (data_ru_reg$primary[[i]] >= mean_primary &
      WY[[i]] >= mean_WY & signif[[i]] == 1) q_2[i] <- "high-high"
  if (data_ru_reg$primary[[i]] < mean_primary &
      WY[[i]] < mean_WY & signif[[i]] == 1) q_2[i] <- "low-low"
  if (data_ru_reg$primary[[i]] > mean_primary &
      WY[[i]] < mean_WY & signif[[i]] == 1) q_2[i] <- "high-low"
  if (data_ru_reg$primary[[i]] < mean_primary &
      WY[[i]] > mean_WY & signif[[i]] == 1) q_2[i] <- "low-high"
  if (signif[i]== 0) q_2[i] <- "non-significant"
}

ggplot(data_ru_reg, aes(fill = q_2)) +
  geom_sf(lwd = 0.2) +
  scale_fill_manual(values = c("high-high" = "red", "low-low" = "blue", "high-low" =
                                 "salmon1", "low-high" = "dodgerblue1", 
                               "non-significant" = "grey90"))


### AVARAGE Local Moran's Index with sampling (permutations) and pseudo-p, and  Map 
local_c_perm <- localC_perm(data_ru_reg$primary, listw = W,
                            nsim = 9999)
local_c_pv <- attr(local_c_perm, "pseudo-p")
local_c_clusters <- attr(local_c_perm, "pseudo-p")

q_3 <- c(1:81)
for (i in 1:81) {
  if (data_ru_reg$primary[[i]] < mean_primary &
      WY[[i]] >= mean_WY & signif[[i]] == 1) q_2[i] <- "high-high"
  if (data_ru_reg$primary[[i]] < mean_primary &
      WY[[i]] < mean_WY & signif[[i]] == 1) q_2[i] <- "low-low"
  if (data_ru_reg$primary[[i]] > mean_primary &
      WY[[i]] < mean_WY & signif[[i]] == 1) q_2[i] <- "high-low"
  if (data_ru_reg$primary[[i]] < mean_primary &
      WY[[i]] > mean_WY & signif[[i]] == 1) q_2[i] <- "low-high"
  if (signif[i]== 0) q_2[i] <- "non-significant"
}

ggplot(data_ru_reg, aes(fill = q_2)) +
  geom_sf(lwd = 0.2) +
  scale_fill_manual(values = c("high-high" = "red", "low-low" = "blue", "high-low" =
                                 "salmon1", "low-high" = "dodgerblue1", 
                               "non-significant" = "grey90"))

local_c_pv[[1,5]]

















