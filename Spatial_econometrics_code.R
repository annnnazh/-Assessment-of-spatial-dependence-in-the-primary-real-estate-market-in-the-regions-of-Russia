library(sf) 
library(spdep)
library(dplyr)
library(sp)
library(ggplot2)
library(openxlsx)
library(ggpubr)
library(patchwork)

library(haven)
library(writexl)
library(GGally)

library(tidyverse)
library(broom)
library(AER)
library(margins)
library(stargazer)
library(wooldridge)

library(sandwich)
library(lmtest)
library(xtable)

library(psych)
library("gridExtra")
library(spatialreg)

library(Matrix)
library(Hmisc)

####################### Spatial W matrix ####################################

st_layers("gadm41_RUS.gpkg")
ru_reg = st_read("gadm41_RUS.gpkg", layer = "ADM_ADM_1")

ru_reg %>% st_geometry() %>% plot() 

ru_reg_3576 = st_transform(ru_reg, 3576)
ru_reg_3576 %>% st_geometry() %>% plot() 
ru_reg_3576 = ru_reg_3576 %>% filter(NAME_1 != 'Chukot') %>% filter(NAME_1 != 'Murmansk')

nbr = poly2nb(ru_reg_3576)

str(nbr)
summary(nbr)
ru_reg_3576$NAME_1

nbr[[20]] = c(as.integer(13), as.integer(55), as.integer(37)) #SPb and Penza and Leningrad oblast' to Kaliningrad
nbr[[59]] = c(as.integer(27), as.integer(54)) #Primor'ye and Khabarovsk to Sakhalin

# make symmetric for neighbours
nbr[[13]] = c(nbr[[13]], as.integer(20))
nbr[[55]] = c(nbr[[55]], as.integer(20))
nbr[[37]] = c(nbr[[37]], as.integer(20))
nbr[[27]] = c(nbr[[27]], as.integer(59))
nbr[[55]] = c(nbr[[55]], as.integer(59))

summary(nbr)

W_1 <- nb2mat(nbr, style = "W") #Row standartizied matrix

ru_reg_3576 <- ru_reg_3576 %>% dplyr::select(-c("VARNAME_1", "TYPE_1", "ENGTYPE_1",
                                                "CC_1", "HASC_1", "ISO_1", "GID_1", "GID_0", "COUNTRY"))



####################### Data #################################################

data <- read.xlsx("data_SE.xlsx", colNames = T)
data$primary <- as.numeric(data$primary)
data$secondary <- as.numeric(data$secondary)

avg_data <- data %>% filter(year >= 2011) %>% group_by(NAME_1) %>% summarize(secondary = mean(secondary, na.rm=TRUE), primary = mean(primary, na.rm=TRUE))
avg_data = avg_data %>% filter(is.na(avg_data$primary) != TRUE)
unique(data$NAME_1)

ru_reg_3576 <- left_join(ru_reg_3576, avg_data, by = "NAME_1")
# seems ok
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

# data <- data %>% filter(NAME_1 != 'Chukot' & NAME_1 != 'Murmansk')

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

#fill NAs with mean values
library(Hmisc)
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



####################### Descriptive statistics ###############################

data = data %>% filter(year >= 2011)
data$constructprice = as.numeric(data$constructprice)
data$pop_gr = as.numeric(data$pop_gr)
data$wforce_gr = as.numeric(data$wforce_gr)
data$avg_mortgage_rate = as.numeric(data$avg_mortgage_rate)
data$total_mortgage_volume = as.numeric(data$total_mortgage_volume)
data$total_mortgage_quantity = as.numeric(data$total_mortgage_quantity)
data$avg_mortgage_term = as.numeric(data$avg_mortgage_term)
data$crime = as.numeric(data$crime)

data['logcrime'] = log(data['crime']+0.001)
data["logprimary"] = log(data['primary']+0.001)
data["logsecondary"] = log(data['secondary']+0.001)
data['logpop'] = log(data['pop']+0.001)
data["logincome_pc"] = log(data['income_pc']+0.001)
data["logtotal_mortgage_volume"] = log(data['total_mortgage_volume']+0.001)

summary(data)
describe(data)

cols_x = c("logprimary", "logsecondary" ,"logpop","wforce_gr", "unempl","logincome_pc", "constructprice","logcrime","avg_mortgage_rate","logtotal_mortgage_volume","migration_rate")
cols_x

#cor(data[,-c(1, 2, 3)])
cordata = data %>% dplyr::select(cols_x)
ggpairs(cordata,na.rm = TRUE)

ggplot(data, mapping = aes(x = secondary, y = primary), colour = as.character(year)) + 
  geom_point(alpha = 0.5) +
  scale_y_log10() +
  scale_x_log10() +
  ggtitle('Распределение log ср. за период цен на первичную и вторичную недвижимость') +
  xlab('Log средней в регионе цены за кв.м. вторичной недвижимости') +
  ylab('Log средней в регионе цены за кв.м. первичной недвижимости')

?ggplot()

hist_primary = ggplot(data = data) +
  geom_histogram(aes(x = logprimary), na.rm=TRUE)+
  ggtitle('Распределение первичных цен в выборке') +
  xlab('Первичная цена') +
  ylab('Количество')
hist_primary

hist_avg_mortgage_rate11 = ggplot(data = filter(data, year == 2011)) +
  geom_histogram(aes(x = avg_mortgage_rate), na.rm=TRUE)+
  ggtitle('2011') +
  xlab('Ипотечная ставка (%)') +
  ylab('Количество')

hist_avg_mortgage_rate14 = ggplot(data = filter(data, year == 2014)) +
  geom_histogram(aes(x = avg_mortgage_rate), na.rm=TRUE)+
  ggtitle('2014') +
  xlab('Ипотечная ставка (%)') +
  ylab('Количество')

hist_avg_mortgage_rate18 = ggplot(data = filter(data, year == 2018)) +
  geom_histogram(aes(x = avg_mortgage_rate), na.rm=TRUE)+
  ggtitle('2018') +
  xlab('Ипотечная ставка (%)') +
  ylab('Количество')

hist_avg_mortgage_rate22 = ggplot(data = filter(data, year == 2022)) +
  geom_histogram(aes(x = avg_mortgage_rate), na.rm=TRUE)+
  ggtitle('2022') +
  xlab('Ипотечная ставка (%)') +
  ylab('Количество')


grid.arrange(hist_avg_mortgage_rate11, hist_avg_mortgage_rate14, hist_avg_mortgage_rate18, hist_avg_mortgage_rate22 + rremove("x.text"), 
             ncol = 2, nrow = 2)

bxp <- ggboxplot(data, x = "year", y = "primary", na.rm=TRUE) +
  scale_y_log10() +
  ggtitle('Ящики с усами по годам') +
  xlab('Год') +
  ylab('Цены на первичную недвижимость')

bxp

filter(data, data$year == 2022)$primary



####################### Spatial indicies ###############################

###################### Global 

WY <- W_1 %*% avg_data$primary
m <- lm(WY ~ avg_data$primary)
summary(m)
W <- mat2listw(W_1, style = "W")

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

#2015
moran.test(ru_reg_3576_2015$primary, listw = W)
moran.test(ru_reg_3576_2015$primary, listw = W, alternative = "two.sided")
moran.test(ru_reg_3576_2015$primary, listw = W, alternative = "less")  

geary.test(as.vector(ru_reg_3576_2015$primary), listw = W)
globalG.test(as.vector(ru_reg_3576_2015$primary), listw = W)

#2016
moran.test(ru_reg_3576_2016$primary, listw = W)
moran.test(ru_reg_3576_2016$primary, listw = W, alternative = "two.sided")
moran.test(ru_reg_3576_2016$primary, listw = W, alternative = "less")  

geary.test(as.vector(ru_reg_3576_2016$primary), listw = W)
globalG.test(as.vector(ru_reg_3576_2016$primary), listw = W)

#2017
moran.test(ru_reg_3576_2017$primary, listw = W)
moran.test(ru_reg_3576_2017$primary, listw = W, alternative = "two.sided")
moran.test(ru_reg_3576_2017$primary, listw = W, alternative = "less")  

geary.test(as.vector(ru_reg_3576_2017$primary), listw = W)
globalG.test(as.vector(ru_reg_3576_2017$primary), listw = W)

#2018
moran.test(ru_reg_3576_2018$primary, listw = W)
moran.test(ru_reg_3576_2018$primary, listw = W, alternative = "two.sided")
moran.test(ru_reg_3576_2018$primary, listw = W, alternative = "less")  

geary.test(as.vector(ru_reg_3576_2018$primary), listw = W)
globalG.test(as.vector(ru_reg_3576_2018$primary), listw = W)

#2019
moran.test(ru_reg_3576_2019$primary, listw = W)
moran.test(ru_reg_3576_2019$primary, listw = W, alternative = "two.sided")
moran.test(ru_reg_3576_2019$primary, listw = W, alternative = "less")  

geary.test(as.vector(ru_reg_3576_2019$primary), listw = W)
globalG.test(as.vector(ru_reg_3576_2019$primary), listw = W)

#2020
moran.test(ru_reg_3576_2020$primary, listw = W)
moran.test(ru_reg_3576_2020$primary, listw = W, alternative = "two.sided")
moran.test(ru_reg_3576_2020$primary, listw = W, alternative = "less")  

geary.test(as.vector(ru_reg_3576_2020$primary), listw = W)
globalG.test(as.vector(ru_reg_3576_2020$primary), listw = W)

#2021
moran.test(ru_reg_3576_2021$primary, listw = W)
moran.test(ru_reg_3576_2021$primary, listw = W, alternative = "two.sided")
moran.test(ru_reg_3576_2021$primary, listw = W, alternative = "less")  

geary.test(as.vector(ru_reg_3576_2021$primary), listw = W)
globalG.test(as.vector(ru_reg_3576_2021$primary), listw = W)

#2022
moran.test(ru_reg_3576_2022$primary, listw = W)
moran.test(ru_reg_3576_2022$primary, listw = W, alternative = "two.sided")
moran.test(ru_reg_3576_2022$primary, listw = W, alternative = "less")  

geary.test(as.vector(ru_reg_3576_2022$primary), listw = W)
globalG.test(as.vector(ru_reg_3576_2022$primary), listw = W)

# Plot:
moran.plot(as.vector(ru_reg_3576_2011$primary), listw = W,
           xlab = "primary (2011)", ylab = "WY",
           pch = 16, cex = 0.4, labels = F)

moran.plot(as.vector(ru_reg_3576_2012$primary), listw = W,
           xlab = "primary (2012)", ylab = "WY",
           pch = 16, cex = 0.4, labels = F)

moran.plot(as.vector(ru_reg_3576_2013$primary), listw = W,
           xlab = "primary (2013)", ylab = "WY",
           pch = 16, cex = 0.4, labels = F)

moran.plot(as.vector(ru_reg_3576_2014$primary), listw = W,
           xlab = "primary (2014)", ylab = "WY",
           pch = 16, cex = 0.4, labels = F)

moran.plot(as.vector(ru_reg_3576_2015$primary), listw = W,
           xlab = "primary (2015)", ylab = "WY",
           pch = 16, cex = 0.4, labels = F)

moran.plot(as.vector(ru_reg_3576_2016$primary), listw = W,
           xlab = "primary (2016)", ylab = "WY",
           pch = 16, cex = 0.4, labels = F)

moran.plot(as.vector(ru_reg_3576_2017$primary), listw = W,
           xlab = "primary (2017)", ylab = "WY",
           pch = 16, cex = 0.4, labels = F)

moran.plot(as.vector(ru_reg_3576_2018$primary), listw = W,
           xlab = "primary (2018)", ylab = "WY",
           pch = 16, cex = 0.4, labels = F)

moran.plot(as.vector(ru_reg_3576_2019$primary), listw = W,
           xlab = "primary (2019)", ylab = "WY",
           pch = 16, cex = 0.4, labels = F)

moran.plot(as.vector(ru_reg_3576_2020$primary), listw = W,
           xlab = "primary (2020)", ylab = "WY",
           pch = 16, cex = 0.4, labels = F)

moran.plot(as.vector(ru_reg_3576_2021$primary), listw = W,
           xlab = "primary (2021)", ylab = "WY",
           pch = 16, cex = 0.4, labels = F)

moran.plot(as.vector(ru_reg_3576_2021$primary), listw = W,
           xlab = "primary (2022)", ylab = "WY",
           pch = 16, cex = 0.4, labels = F)


###################### Local 

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
loc_m_perm_2022 <- localmoran_perm(as.vector(ru_reg_3576_2022$primary), listw = W,
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
                                 "pink", "low-high" = "lightblue", 
                               "non-significant" = "grey90"))


### Local Geary  Index with sampling (permutations) and pseudo-p, and  Map for 2022
local_c_perm_2022 <- localC_perm(as.vector(ru_reg_3576_2022$primary), listw = W,
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

ggplot(ru_reg_3576, aes(fill = q_3)) +
  geom_sf(lwd = 0.2) +
  scale_fill_manual(values = c("high-high" = "red", "low-low" = "blue", "high-low" =
                                 "pink", "low-high" = "lightblue", 
                               "non-significant" = "grey90"))

q_3
local_c_pv[[1,5]]

q_col <- c("red", "blue", "pink", "lightblue", "grey90")
colors <- q_col[q_3]

plot(ru_reg_3576_2022$geom, col = colors)
legend("bottomright", legend = c("high-high", "low-low", "high-low", "low-high", "non-significant"),
       fill = q_col)

q_3
local_c_pv[[1,5]]

