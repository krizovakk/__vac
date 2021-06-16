# MDPI SUSTAINABILITY
# NeoSol - do pudy
# 3 typy hnoje (krava, prase, slepice)
# Slovec 2014-2020
# aplikace SOL kazdy rok
# aplikace hnoje 2014, 2016, 2017, 2019

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

data_summary <- function(data, varname, groupnames){ # funkce pro výpočet errorbars
  # require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

# general load ------------------------------------------------------------

varlev <- c("3", "4", "5", "6", "9", "10", "13", "14")
varlab <- c("3", "4", "5", "6", "9", "10", "13", "14")

# UNIT DRAFT --------------------------------------------------------------

ud <- read_excel("red/iud_new_plot.xlsx")

# factors

ud$var <- factor(ud$var, levels = varlev, labels = varlab)
ud$term <- factor(ud$term)

ud <- ud %>%
  mutate(sol_t = (var %in% c("4", "5", "10", "14"))) %>%
  mutate(sol_f = (var %in% c("3", "6", "9", "13")))  %>%
  mutate(sol = case_when(sol_f == "TRUE" ~ "FALSE", 
                         sol_t == "TRUE" ~ "TRUE")) %>%
  mutate(duo = case_when(var %in% c("3","4") ~ c("cattle"), 
                         var %in% c("5","6") ~ "npk", 
                         var %in% c("9","10") ~ "pig", 
                         var %in% c("13","14") ~ "poultry"))
         
ud$duo <- factor(ud$duo, levels = c("npk", "cattle", "pig", "poultry"))

dfud <- data_summary(ud, varname="unitd", 
                     groupnames=c("term", "var", "sol", "duo"))
head(dfud)
# write_xlsx(dfud,"prstat.xlsx") # package "writexl"

# plot ----------------------------------------------------

# lab_let <- c("a","a","a", "a","a","a",
#            "ab","ab","b","ab","ab","a",
#            "ac","c","a","a","b","b",
#            "ab","ab","ab","b","a","c")

lab_ud <- c("ab","ab","a", "a","ac","c","ab","ab",
           "b","ab","a","a","a","a","ab","b",
           "ab","a","a","a","b","b","a","c")

ggplot(dfud, aes(term, unitd, group=sol))+
  geom_line(aes(linetype = sol), size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=unitd-sd, ymax=unitd+sd), width=.5, size = .5, position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol), 
  facet_grid(. ~ dfud$duo)+
  labs(y = expression("Unit Draft [%]"), 
       x = "", title = "", fill = "")+
  coord_cartesian(ylim = c(85, 115))+
  scale_linetype_discrete(name="SOL")+
  guides(shape = FALSE)+
  geom_text(aes(y = 113, label = lab_ud),
            size = 5, position=position_dodge(1))+
  theme_classic(base_size = 20)+ # base_size = 20
  theme(text=element_text(family="Times New Roman")) # axis.text.x = element_text(size = 11)
ggsave("plots/iud.png", device = "png", width = 10, height = 5, dpi = 300)

# SFH --------------------------------------------------------------

sfh <- read_excel("red/sfh.xlsx")

# factors

sfh$var <- factor(sfh$var, levels = varlev, labels = varlab)
sfh$Year <- factor(sfh$Year)

sfh <- sfh %>%
  mutate(sol_t = (var %in% c("4", "5", "10", "14"))) %>%
  mutate(sol_f = (var %in% c("3", "6", "9", "13"))) %>% 
  mutate(sol = case_when(sol_f == "TRUE" ~ "FALSE", 
                         sol_t == "TRUE" ~ "TRUE")) %>% 
  mutate(duo = case_when(var %in% c("3","4") ~ c("cattle"), 
                         var %in% c("5","6") ~ "npk", 
                         var %in% c("9","10") ~ "pig", 
                         var %in% c("13","14") ~ "poultry")) %>% 
  mutate(term = case_when(Year == "2015" ~ "T1",
                          Year == "2017" ~ "T2",
                          Year == "2020" ~ "T3"))

sfh$duo <- factor(sfh$duo, levels = c("npk", "cattle", "pig", "poultry"))
sfh$term <- factor(sfh$term)

dfsfh <- data_summary(sfh, varname="value", 
                     groupnames=c("term", "var", "sol", "duo"))
head(dfsfh)
#write_xlsx(dfsfh,"prstat.xlsx") # package "writexl"

# plot ----------------------------------------------------

lab_sfh <- c("a","a","a", "a","a","a","a","a",
           "a","a","a","a","a","a","a","a",
           "a","a","a","a","a","a","a","a")

ggplot(dfsfh, aes(term, value, group=sol))+
  geom_line(aes(linetype = sol), size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol), 
  facet_grid(. ~ dfsfh$duo)+
  labs(y = "Saturated Hydraulic Conductivity [%]", 
       x = "", title = "", fill = "")+
  coord_cartesian(ylim = c(85, 150))+
  scale_linetype_discrete(name="SOL")+
  guides(shape = FALSE)+
  geom_text(aes(y = 145, label = lab_sfh),
            size = 5, position=position_dodge(1))+
  theme_classic(base_size = 20)+ # base_size = 20
  theme(text=element_text(family="Times New Roman")) # axis.text.x = element_text(size = 11)

ggsave("plots/sfh.png", device = "png", width = 10, height = 5, dpi = 300)

# RBD --------------------------------------------------------------

rbd <- read_excel("red/rbd.xlsx")

# factors

rbd$var <- factor(rbd$var, levels = varlev, labels = varlab)
rbd$year <- factor(rbd$year)

rbd <- rbd %>%
  mutate(sol_t = (var %in% c("4", "5", "10", "14"))) %>%
  mutate(sol_f = (var %in% c("3", "6", "9", "13"))) %>% 
  mutate(sol = case_when(sol_f == "TRUE" ~ "FALSE", 
                         sol_t == "TRUE" ~ "TRUE")) %>% 
  mutate(duo = case_when(var %in% c("3","4") ~ c("cattle"), 
                         var %in% c("5","6") ~ "npk", 
                         var %in% c("9","10") ~ "pig", 
                         var %in% c("13","14") ~ "poultry")) %>% 
  mutate(term = case_when(year == "2015" ~ "T1",
                          year == "2017" ~ "T2",
                          year == "2020" ~ "T3"))

rbd$duo <- factor(rbd$duo, levels = c("npk", "cattle", "pig", "poultry"))
rbd$term <- factor(rbd$term)

dfrbd <- data_summary(rbd, varname="value", 
                     groupnames=c("term", "var", "sol", "duo"))
head(dfrbd)
#write_xlsx(dfrbd,"prstat.xlsx") # package "writexl"

# plot ----------------------------------------------------

lab_rbd <- c("a","a","a", "a","a","a","a","a",
           "a","a","b","a","a","a","a","a",
           "a","a","b","a","a","a","a","a")

ggplot(dfrbd, aes(term, value, group=sol))+
  geom_line(aes(linetype = sol), size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol), 
  facet_grid(. ~ dfrbd$duo)+
  labs(y = "Reduced Bulk Density [%]", 
       x = "", title = "", fill = "")+
  coord_cartesian(ylim = c(75, 120))+
  scale_linetype_discrete(name="SOL")+
  guides(shape = FALSE)+
  geom_text(aes(y = 118, label = lab_rbd),
            size = 5, position=position_dodge(1))+
  theme_classic(base_size = 20)+ # base_size = 20
  theme(text=element_text(family="Times New Roman")) # axis.text.x = element_text(size = 11)

ggsave("plots/rbd.png", device = "png", width = 10, height = 5, dpi = 300)

# CI--------------------------------------------------------------

ci <- read_excel("red/ci.xlsx")
ci <-  melt(ci, id.vars = c("var", "year"), variable.name = ("depth"), value.name = "value")



# factors

ci$var <- factor(ci$var, levels = varlev, labels = varlab)
ci$year <- factor(ci$year)

ci <- ci %>%
  mutate(sol_t = (var %in% c("4", "5", "10", "14"))) %>%
  mutate(sol_f = (var %in% c("3", "6", "9", "13"))) %>% 
  mutate(sol = case_when(sol_f == "TRUE" ~ "FALSE", 
                         sol_t == "TRUE" ~ "TRUE")) %>% 
  mutate(duo = case_when(var %in% c("3","4") ~ c("cattle"), 
                         var %in% c("5","6") ~ "npk", 
                         var %in% c("9","10") ~ "pig", 
                         var %in% c("13","14") ~ "poultry")) %>% 
  mutate(term = case_when(year == "2015" ~ "T1",
                          year == "2017" ~ "T2",
                          year == "2020" ~ "T3"))

ci$duo <- factor(ci$duo, levels = c("npk", "cattle", "pig", "poultry"))
ci$term <- factor(ci$term)

dfci <- data_summary(ci, varname="value", 
                     groupnames=c("term", "var", "depth", "sol", "duo"))
head(dfci)
#write_xlsx(dfci,"prstat.xlsx") # package "writexl"

# plot ----------------------------------------------------

# # lab_ci <- c("a","a","a", "a","a","a","a","a",
#            "a","a","b","a","a","a","a","a",
#            "a","a","b","a","a","a","a","a")

ggplot(dfci, aes(term, value, group=sol))+
  geom_line(aes(linetype = sol), size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol), 
  facet_grid(dfci$depth ~ dfci$duo)+
  labs(y = "Cone Index [%]", 
       x = "", title = "", fill = "")+
  # coord_cartesian(ylim = c(75, 120))+
  scale_linetype_discrete(name="SOL")+
  guides(shape = FALSE)+
  # geom_text(aes(y = 118, label = lab_ci),
            # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 20)+ # base_size = 20
  theme(text=element_text(family="Times New Roman")) # axis.text.x = element_text(size = 11)

ggsave("plots/ci.png", device = "png", width = 10, height = 5, dpi = 300)


# GEE NDVI ---------------------------------------------------------------------

# ndvi <- read_excel("red/NDVImeansd.xlsx")
ndvi <- read.csv("red/NDVImeansd.csv", header = TRUE)

ndvi <- ndvi %>% 
  mutate(date = substr(`system.index`, 1, 8)) %>% 
  select(id, mean, stdDev, date)

ndvi$date <- factor(ndvi$date)
levels(ndvi$date)

ndvi$id <- factor(ndvi$id)
ndvi$mean <- as.numeric(ndvi$mean)
ndvi$stdDev <- as.numeric(ndvi$stdDev)

ndvi_var <- ndvi %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14", "1001", "1002")) %>% 
  mutate(var = case_when(id == "3" ~ "catt",
                         id == "4" ~ "cattSOL",
                         id == "5" ~ "npkSOL",
                         id == "6" ~ "npk",
                         id == "9" ~ "pig",
                         id == "10" ~ "pigSOL",
                         id %in% c("13", "1001") ~ "pou",
                         id %in% c("14", "1002") ~ "pouSOL"))

ndvi_var$var <- factor(ndvi_var$var, levels = c("npk", "npkSOL",  "catt", "cattSOL", "pig", "pigSOL", "pou", "pouSOL"))

ggplot(ndvi_var, aes(date, mean, color = var))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol), 
  labs(y = "NDVI", 
       x = "", title = "", color = "")+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 20)+ # base_size = 20
  theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(angle = 90)) # axis.text.x = element_text(size = 11)

# separated seasons

NDVIs1 <- ndvi_var %>% 
  filter(date %in% c("20170511", "20170603", "20170620")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "1001", "1002")) %>% 
  mutate(seas = "s1") %>% 
  mutate(crop = "winter wheat")

NDVIs2 <- ndvi_var %>% 
  filter(date %in% c("20180411", "20180419", "20180421", "20180429", "20180514", "20180521",
                     "20180531", "20180703")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s2") %>% 
  mutate(crop = "corn")

NDVIs3 <- ndvi_var %>% 
  filter(date %in% c("20181011", "20181013", "20181016", "20181018",
                     "20181031", "20181115", "20181117", "20181205",
                     "20190205", "20190218", "20190228", "20190401",
                     "20190416", "20190419", "20190421", "20190603",
                     "20190625", "20190630", "20190723")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s3")%>% 
  mutate(crop = "winter wheat")

NDVIs4 <- ndvi_var %>% 
  filter(date %in% c("20191031", "20200101", "20200329", "20200405",
                     "20200408", "20200418", "20200420", "20200423",
                     "20200428", "20200518", "20200622", "20200714", "20200722")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s4")%>% 
  mutate(crop = "winter wheat")

# write_xlsx(NDVIs1,"NDVIs1.xlsx") # package "writexl"
# write_xlsx(NDVIs2,"NDVIs2.xlsx") # package "writexl"
# write_xlsx(NDVIs3,"NDVIs3.xlsx") # package "writexl"
# write_xlsx(NDVIs4,"NDVIs4.xlsx") # package "writexl"


#facet plot all seasons

ndvi_seas <- rbind(NDVIs1, NDVIs2, NDVIs3, NDVIs4)

# write_xlsx(ndvi_seas,"NDVI.xlsx") # package "writexl"

# plots

# seas facet

to_string <- as_labeller(c("s1" = "winter wheat", "s2" = "corn", "s3" = "winter wheat", "s4" = "winter wheat"))

ggplot(ndvi_seas, aes (date, mean, color = var))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol)
  labs(y = "NDVI", 
       x = "", title = "", color = "")+
  # scale_y_continuous(limits = c(0.8, 1), breaks = seq(0.8, 1, by = 0.1))+
  scale_color_brewer(palette="Spectral")+
  facet_grid(. ~ seas, scales = "free", labeller = to_string)+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 25)+ # base_size = 20
  theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(angle = 90), 
        legend.position = "top") # axis.text.x = element_text(size = 11) # legend.position = c(0.9, 0.85))
ggsave("plots/NDVIall.png", device = "png", width = 10, height = 6, dpi = 300)

# separate seasons

ggplot(NDVIs1, aes(date, mean, color = var))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol)
  labs(y = "NDVI", 
       x = "", title = "", color = "")+
  scale_y_continuous(limits = c(0.8, 1), breaks = seq(0.8, 1, by = 0.1))+
  scale_color_brewer(palette="Spectral")+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 25)+ # base_size = 20
  theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(angle = 90), 
        legend.position = "top") # axis.text.x = element_text(size = 11) # legend.position = c(0.9, 0.85))
ggsave("plots/NDVIs1.png", device = "png", width = 10, height = 6, dpi = 300)

ggplot(NDVIs2, aes(date, mean, color = var))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol), 
  labs(y = "NDVI", 
       x = "", title = "", color = "")+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 20)+ # base_size = 20
  theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(angle = 90)) # axis.text.x = element_text(size = 11)

ggplot(NDVIs3, aes(date, mean, color = var))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol), 
  labs(y = "NDVI", 
       x = "", title = "", color = "")+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 20)+ # base_size = 20
  theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(angle = 90)) # axis.text.x = element_text(size = 11)

ggplot(NDVIs4, aes(date, mean, color = var))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol), 
  labs(y = "NDVI", 
       x = "", title = "", color = "")+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 20)+ # base_size = 20
  theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(angle = 90)) # axis.text.x = element_text(size = 11)

# analysis

hist(ndvi_seas$mean)
shapiro.test(ndvi_seas$mean)
bartlett.test(ndvi_seas$mean ~ ndvi_seas$var) # yep

install.packages("lme4")
require(lme4)

m1 <- lmer(ndvi_seas$mean ~ ndvi_seas$var + 1|ndvi_seas$date)
summary(m1)

# GEE NDWI ---------------------------------------------------------------------

ndwi <- read.csv("red/NDWImeansd.csv", header = TRUE)

ndwi <- ndwi %>% 
  mutate(date = substr(`system.index`, 1, 8)) %>% 
  select(id, mean, stdDev, date)

ndwi$date <- factor(ndwi$date)
levels(ndwi$date)

ndwi$id <- factor(ndwi$id)
ndwi$mean <- as.numeric(ndwi$mean)
ndwi$stdDev <- as.numeric(ndwi$stdDev)

ndwi_var <- ndwi %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14", "1001", "1002")) %>% 
  mutate(var = case_when(id == "3" ~ "catt",
                         id == "4" ~ "cattSOL",
                         id == "5" ~ "npkSOL",
                         id == "6" ~ "npk",
                         id == "9" ~ "pig",
                         id == "10" ~ "pigSOL",
                         id %in% c("13", "1001") ~ "pou",
                         id %in% c("14", "1002") ~ "pouSOL"))

ndwi_var$var <- factor(ndwi_var$var, levels = c("npk", "npkSOL",  "catt", "cattSOL", "pig", "pigSOL", "pou", "pouSOL"))

ggplot(ndwi_var, aes(date, mean, color = var))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol), 
  labs(y = "NDWI", 
       x = "", title = "", color = "")+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 20)+ # base_size = 20
  theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(angle = 90)) # axis.text.x = element_text(size = 11)

# separated seasons

NDWIs1 <- ndwi_var %>% 
  filter(date %in% c("20170511", "20170603", "20170620")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "1001", "1002")) %>% 
  mutate(seas = "s1")%>% 
  mutate(crop = "winter wheat")

NDWIs2 <- ndwi_var %>% 
  filter(date %in% c("20180411", "20180419", "20180421", "20180429", "20180514", "20180521",
                     "20180531", "20180703")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s2")%>% 
  mutate(crop = "corn")
 
NDWIs3 <- ndwi_var %>% 
  filter(date %in% c("20181011", "20181013", "20181016", "20181018",
                     "20181031", "20181115", "20181117", "20181205",
                     "20190205", "20190218", "20190228", "20190401",
                     "20190416", "20190419", "20190421", "20190603",
                     "20190625", "20190630", "20190723")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s3")%>% 
  mutate(crop = "winter wheat")

NDWIs4 <- ndwi_var %>% 
  filter(date %in% c("20191031", "20200101", "20200329", "20200405",
                     "20200408", "20200418", "20200420", "20200423",
                     "20200428", "20200518", "20200622", "20200714", "20200722")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s4")%>% 
  mutate(crop = "winter wheat")

#facet plot all seasons

ndwi_seas <- rbind(NDWIs1, NDWIs2, NDWIs3, NDWIs4)

write_xlsx(ndwi_seas,"NDWI.xlsx") # package "writexl"

# seas facet

to_string <- as_labeller(c("s1" = "winter wheat", "s2" = "corn", "s3" = "winter wheat", "s4" = "winter wheat"))

ggplot(ndwi_seas, aes (date, mean, color = var))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol)
  labs(y = "NDWI", 
       x = "", title = "", color = "")+
  # scale_y_continuous(limits = c(0.8, 1), breaks = seq(0.8, 1, by = 0.1))+
  scale_color_brewer(palette="Spectral")+
  facet_grid(. ~ seas, scales = "free", labeller = to_string)+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 25)+ # base_size = 20
  theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(angle = 90), 
        legend.position = "top") # axis.text.x = element_text(size = 11) # legend.position = c(0.9, 0.85))
ggsave("plots/NDWIall.png", device = "png", width = 10, height = 6, dpi = 300)

# plots

ggplot(NDWIs1, aes(date, mean, color = var))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol)
  labs(y = "NDWI", 
       x = "", title = "", color = "")+
 # scale_y_continuous(limits = c(0.8, 1), breaks = seq(0.8, 1, by = 0.1))+
  scale_color_brewer(palette="Spectral")+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 25)+ # base_size = 20
  theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(angle = 90), 
        legend.position = "top") # axis.text.x = element_text(size = 11) # legend.position = c(0.9, 0.85))
ggsave("plots/NDWIs1.png", device = "png", width = 10, height = 6, dpi = 300)

ggplot(NDWIs2, aes(date, mean, color = var))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol), 
  labs(y = "NDVI", 
       x = "", title = "", color = "")+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 20)+ # base_size = 20
  theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(angle = 90)) # axis.text.x = element_text(size = 11)

ggplot(NDWIs3, aes(date, mean, color = var))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol), 
  labs(y = "NDVI", 
       x = "", title = "", color = "")+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 20)+ # base_size = 20
  theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(angle = 90)) # axis.text.x = element_text(size = 11)

ggplot(NDWIs4, aes(date, mean, color = var))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol), 
  labs(y = "NDVI", 
       x = "", title = "", color = "")+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 20)+ # base_size = 20
  theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(angle = 90)) # axis.text.x = element_text(size = 11)

# analysis

 hist(ndwi_seas$mean)
 shapiro.test(ndwi_seas$mean)
 bartlett.test(ndwi_seas$mean ~ ndwi_seas$var) # yep

 # install.packages("lme4")
require(lme4)

m2 <- lmer(ndwi_seas$mean ~ ndvi_seas$var + 1|ndvi_seas$date)
summary(m2)

# GEE LAI ---------------------------------------------------------------------

lai <- read.csv("red/LAImeansd.csv", header = TRUE)

lai <- lai %>% 
  mutate(date = substr(`system.index`, 1, 8)) %>% 
  select(id, mean, stdDev, date)

lai$date <- factor(lai$date)
levels(lai$date)

lai$id <- factor(lai$id)
lai$mean <- as.numeric(lai$mean)
lai$stdDev <- as.numeric(lai$stdDev)

lai_var <- lai %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14", "1001", "1002")) %>% 
  mutate(var = case_when(id == "3" ~ "catt",
                         id == "4" ~ "cattSOL",
                         id == "5" ~ "npkSOL",
                         id == "6" ~ "npk",
                         id == "9" ~ "pig",
                         id == "10" ~ "pigSOL",
                         id %in% c("13", "1001") ~ "pou",
                         id %in% c("14", "1002") ~ "pouSOL"))

lai_var$var <- factor(lai_var$var, levels = c("npk", "npkSOL",  "catt", "cattSOL", "pig", "pigSOL", "pou", "pouSOL"))

# separated seasons

LAIs1 <- lai_var %>% 
  filter(date %in% c("20170511", "20170603", "20170620")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "1001", "1002")) %>% 
  mutate(seas = "s1")%>% 
  mutate(crop = "winter wheat")

LAIs2 <- lai_var %>% 
  filter(date %in% c("20180411", "20180419", "20180421", "20180429", "20180514", "20180521",
                     "20180531", "20180703")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s2")%>% 
  mutate(crop = "corn")

LAIs3 <- lai_var %>% 
  filter(date %in% c("20181011", "20181013", "20181016", "20181018",
                     "20181031", "20181115", "20181117", "20181205",
                     "20190205", "20190218", "20190228", "20190401",
                     "20190416", "20190419", "20190421", "20190603",
                     "20190625", "20190630", "20190723")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s3")%>% 
  mutate(crop = "winter wheat")

LAIs4 <- lai_var %>% 
  filter(date %in% c("20191031", "20200101", "20200329", "20200405",
                     "20200408", "20200418", "20200420", "20200423",
                     "20200428", "20200518", "20200622", "20200714", "20200722")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s4")%>% 
  mutate(crop = "winter wheat")

#facet plot all seasons

lai_seas <- rbind(LAIs1, LAIs2, LAIs3, LAIs4)

write_xlsx(lai_seas,"LAI.xlsx") # package "writexl"

# seas facet

to_string <- as_labeller(c("s1" = "winter wheat", "s2" = "corn", "s3" = "winter wheat", "s4" = "winter wheat"))

ggplot(lai_seas, aes (date, mean, color = var))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol)
  labs(y = "LAI", 
       x = "", title = "", color = "")+
  # scale_y_continuous(limits = c(0.8, 1), breaks = seq(0.8, 1, by = 0.1))+
  scale_color_brewer(palette="Spectral")+
  facet_grid(. ~ seas, scales = "free", labeller = to_string)+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 25)+ # base_size = 20
  theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(angle = 90), 
        legend.position = "top") # axis.text.x = element_text(size = 11) # legend.position = c(0.9, 0.85))
ggsave("plots/LAIall.png", device = "png", width = 10, height = 6, dpi = 300)

