# ?
# Vaclavovo Skorenice 2020
# varianty 1, 5, 7, 10, 11

# base  -------------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")
# install.packages("RColorBrewer")
# install.packages("writexl")
# install.packages("plyr")
# install.packages("extrafont")
require(tidyverse)
require(readxl)
require(reshape2)
require(RColorBrewer)
require(writexl)
require(plyr)
library(extrafont)
# font_import()
# loadfonts(device="win")       #Register fonts for Windows bitmap output
# fonts()                       #vector of font family names
##  [1] "Andale Mono"                  "AppleMyungjo"                
##  [3] "Arial Black"                  "Arial"                       
##  [5] "Arial Narrow"                 "Arial Rounded MT Bold"  

# data_summary <- function(data, varname, groupnames){ # funkce pro výpočet errorbars
#   # require(plyr)
#   summary_func <- function(x, col){
#     c(mean = mean(x[[col]], na.rm=TRUE),
#       sd = sd(x[[col]], na.rm=TRUE))
#   }
#   data_sum<-ddply(data, groupnames, .fun=summary_func,
#                   varname)
#   data_sum <- rename(data_sum, c("mean" = varname))
#   return(data_sum)
# }

# general load ------------------------------------------------------------

# GEE NDVI ---------------------------------------------------------------------

# ndvi <- read_excel("red/NDVImeansd.xlsx")
ndvi <- read.csv("red/NDVImeansd.csv", header = TRUE)

ndvi <- ndvi %>% 
  mutate(date = substr(`system.index`, 1, 8)) %>% 
  select(Varianta, mean, stdDev, date)%>% 
  # rename(Varianta = id) %>%
  filter(Varianta %in% c("1", "5", "7", "10", "11")) %>% 
  mutate(index = "NDVI")

names(ndvi)[1] <- "id"

ndvi$date <- factor(ndvi$date)
levels(ndvi$date)
ndvi$id <- factor(ndvi$id)
ndvi$mean <- as.numeric(ndvi$mean)
ndvi$stdDev <- as.numeric(ndvi$stdDev)

# plot

ggplot(ndvi, aes(date, mean, color = id))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol)
  labs(y = "NDVI", 
       x = "", title = "", color = "")+
  scale_y_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, by = 0.1))+
  scale_color_brewer(palette="Spectral")+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 25)+ # base_size = 20
  theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(angle = 90), 
        legend.position = "top") # axis.text.x = element_text(size = 11) # legend.position = c(0.9, 0.85))
ggsave("plots/ndvi.png", device = "png", width = 10, height = 7, dpi = 300)

# GEE NDWI ---------------------------------------------------------------------

# ndvi <- read_excel("red/NDVImeansd.xlsx")
ndwi <- read.csv("red/NDWImeansd.csv", header = TRUE)

ndwi <- ndwi %>% 
  mutate(date = substr(`system.index`, 1, 8)) %>% 
  select(Varianta, mean, stdDev, date)%>% 
  # rename(Varianta = id) %>%
  filter(Varianta %in% c("1", "5", "7", "10", "11")) %>% 
  mutate(index = "NDWI")

names(ndwi)[1] <- "id"

ndwi$date <- factor(ndwi$date)
levels(ndwi$date)
ndwi$id <- factor(ndwi$id)
ndwi$mean <- as.numeric(ndwi$mean)
ndwi$stdDev <- as.numeric(ndwi$stdDev)

# plot

ggplot(ndwi, aes(date, mean, color = id))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol)
  labs(y = "NDWI", 
       x = "", title = "", color = "")+
  # scale_y_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, by = 0.1))+
  scale_color_brewer(palette="Spectral")+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 25)+ # base_size = 20
  theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(angle = 90), 
        legend.position = "top") # axis.text.x = element_text(size = 11) # legend.position = c(0.9, 0.85))
ggsave("plots/ndwi.png", device = "png", width = 10, height = 7, dpi = 300)

# GEE LAI ---------------------------------------------------------------------

# lai <- read_excel("red/laimeansd.xlsx")
lai <- read.csv("red/LAImeansd.csv", header = TRUE)

lai <- lai %>% 
  mutate(date = substr(`system.index`, 1, 8)) %>% 
  select(Varianta, mean, stdDev, date)%>% 
  # rename(Varianta = id) %>%
  filter(Varianta %in% c("1", "5", "7", "10", "11")) %>% 
  mutate(index = "LAI")

names(lai)[1] <- "id"

lai$date <- factor(lai$date)
levels(lai$date)
lai$id <- factor(lai$id)
lai$mean <- as.numeric(lai$mean)
lai$stdDev <- as.numeric(lai$stdDev)

# plot

ggplot(lai, aes(date, mean, color = id))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol)
  labs(y = "LAI", 
       x = "", title = "", color = "")+
  # scale_y_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, by = 0.1))+
  scale_color_brewer(palette="Spectral")+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 25)+ # base_size = 20
  theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(angle = 90), 
        legend.position = "top") # axis.text.x = element_text(size = 11) # legend.position = c(0.9, 0.85))
ggsave("plots/lai.png", device = "png", width = 10, height = 7, dpi = 300)

