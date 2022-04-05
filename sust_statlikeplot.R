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

windowsFonts(A = windowsFont("Times New Roman")) # https://statisticsglobe.com/change-font-of-plot-in-r

# general load ------------------------------------------------------------

varlev <- c("6", "5", "3", "4", "9", "10", "13", "14")
varlab <- c("NPK", "NPKSOL", "catt", "cattSOL", "pig", "pigSOL", "pou", "pouSOL")

# UNIT DRAFT --------------------------------------------------------------

# ud <- read_excel("red/iud_new_plot.xlsx")
ud <- read_excel("red/iud_nocalc.xlsx")

# factors

# ud$var <- factor(ud$var, levels = varlev, labels = varlab)
ud$var <- factor(ud$var)
ud$term <- factor(ud$term)
ud$unitd <- ud$unitd/1000

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

# dfud <- data_summary(ud, varname="unitd", 
#                      groupnames=c("term", "var", "sol", "duo"))
# head(dfud)
# write_xlsx(dfud,"prstat.xlsx") # package "writexl"

# plot ----------------------------------------------------

varlev <- c("6", "5", "3", "4", "9", "10", "13", "14")
varlab <- c("NPK", "NPKSOL", "catt", "cattSOL", "pig", "pigSOL", "pou", "pouSOL")

ud$var <- factor(ud$var, levels = varlev, labels = varlab)

ud_string <- as_labeller(c("T1" = "Term I", "T2" = "Term II", "T3" = "Term III"))

# ggplot(ud, aes(var, unitd, fill = sol, alpha = sol, color = duo))+ # BETTER BOXPLOT BELOW
#   geom_boxplot()+
#   scale_fill_manual(values=c("grey", "grey")) +
#   scale_alpha_manual(values=c(0.1,0.8)) +
#   scale_color_brewer(palette = "Spectral")+
#   # scale_fill_manual(breaks = var, values = c("azure1", "azure4", "cornsilk2", "lightgoldenrod4", 
#   #                               "lightpink", "lightpink4", "peachpuff", "peru"))+
#   facet_grid(.~ ud$term, labeller = ud_string)+
#   labs(y = expression("Unit Draft [ kN "~ m^-2~"]"),  #"Unit Draft [%]"
#        x = "", title = "", fill = "")+
#   coord_cartesian(ylim = c(85, 260))+
#   # geom_text(aes(y = 260, label = lab_ud_df), size = 5)+
#   theme_classic(base_size = 20)+ # base_size = 20
#   theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(angle = 90),
#         legend.position = "none")
# # ggsave("plots/ud_box.png", device = "png", width = 10, height = 5, dpi = 300)

ggplot(ud, aes(var, unitd, fill = sol, alpha = sol, color = duo))+
  geom_boxplot(lwd=0.8)+
  scale_fill_manual(values=c("grey", "grey")) +
  scale_alpha_manual(values=c(0.1,0.6)) +
  scale_color_brewer(palette = "Spectral")+
  # scale_fill_manual(breaks = var, values = c("azure1", "azure4", "cornsilk2", "lightgoldenrod4", 
  #                               "lightpink", "lightpink4", "peachpuff", "peru"))+
  facet_grid(.~ ud$term, labeller = ud_string)+
  labs(y = expression("Unit Draft ( kN "~ m^-2~")"),  #"Unit Draft [%]"
       x = "", title = "", fill = "")+
  coord_cartesian(ylim = c(85, 260))+
  # geom_text(aes(y = 260, label = lab_ud_df), size = 5)+
  theme_classic(base_size = 25)+ # base_size = 20
  theme(text=element_text(family="A"), axis.text.x = element_text(angle = 90, hjust = 0),
        legend.position = "none")
ggsave("plots/ud.png", device = "png", width = 10, height = 6, dpi = 300)


# SFH --------------------------------------------------------------

sfh <- read_excel("red/inf.xlsx")

# factors


sfh <- sfh %>%
  mutate(sol_t = (var %in% c("4", "5", "10", "14"))) %>%
  mutate(sol_f = (var %in% c("3", "6", "9", "13"))) %>% 
  mutate(sol = case_when(sol_f == "TRUE" ~ "FALSE", 
                         sol_t == "TRUE" ~ "TRUE")) %>% 
  mutate(duo = case_when(var %in% c("3","4") ~ c("cattle"), 
                         var %in% c("5","6") ~ "npk", 
                         var %in% c("9","10") ~ "pig", 
                         var %in% c("13","14") ~ "poultry")) 

sfh$duo <- factor(sfh$duo, levels = c("npk", "cattle", "pig", "poultry"))
sfh$term <- factor(sfh$term)
sfh$var <- factor(sfh$var, levels = varlev, labels = varlab)

# plot ----------------------------------------------------

# lab_sfh <- c("a","a","a", "a","a","a","a","a",
#            "a","a","a","a","a","a","a","a",
#            "a","a","a","a","a","a","a","a")

sfh_string <- as_labeller(c("T1" = "Term I", "T2" = "Term II", "T3" = "Term III"))

ggplot(sfh, aes(var, val, fill = sol, alpha = sol, color = duo))+
  geom_boxplot(lwd = 0.8)+
  scale_fill_manual(values=c("grey", "grey")) +
  scale_alpha_manual(values=c(0.1,0.6)) +
  scale_color_brewer(palette = "Spectral")+
  # scale_fill_manual(breaks = var, values = c("azure1", "azure4", "cornsilk2", "lightgoldenrod4", 
  #                               "lightpink", "lightpink4", "peachpuff", "peru"))+
  facet_grid(.~ sfh$term)+
  labs(y = expression("Satur. Hydr. Conductivity ( mm "~ h^-1~")"),  #"Unit Draft [%]"
       x = "", title = "", fill = "")+
  coord_cartesian(ylim = c(3, 16))+
  # geom_text(aes(y = 260, label = lab_sfh), size = 5)+
  theme_classic(base_size = 25)+ # base_size = 20
  theme(text=element_text(family="A"), axis.text.x = element_text(angle = 90, hjust = 0),
        legend.position = "none")
ggsave("plots/sfh.png", device = "png", width = 10, height = 6, dpi = 300)

# RBD --------------------------------------------------------------

rbd <- read_excel("red/bd.xlsx")

# factors

rbd$var <- factor(rbd$var)

rbd <- rbd %>%
  mutate(sol_t = (var %in% c("4", "5", "10", "14"))) %>%
  mutate(sol_f = (var %in% c("3", "6", "9", "13"))) %>% 
  mutate(sol = case_when(sol_f == "TRUE" ~ "FALSE", 
                         sol_t == "TRUE" ~ "TRUE")) %>% 
  mutate(duo = case_when(var %in% c("3","4") ~ c("cattle"), 
                         var %in% c("5","6") ~ "npk", 
                         var %in% c("9","10") ~ "pig", 
                         var %in% c("13","14") ~ "poultry"))

rbd$duo <- factor(rbd$duo, levels = c("npk", "cattle", "pig", "poultry"))
rbd$term <- factor(rbd$term)

# plot ----------------------------------------------------

rbd$var <- factor(rbd$var, levels = varlev, labels = varlab)

ggplot(rbd, aes(var, val, fill = sol, alpha = sol, color = duo))+
  geom_boxplot(lwd = 0.8)+
  scale_fill_manual(values=c("grey", "grey")) +
  scale_alpha_manual(values=c(0.1,0.6)) +
  scale_color_brewer(palette = "Spectral")+
  # scale_fill_manual(breaks = var, values = c("azure1", "azure4", "cornsilk2", "lightgoldenrod4", 
  #                               "lightpink", "lightpink4", "peachpuff", "peru"))+
  facet_grid(.~ rbd$term)+
  labs(y = expression("Bulk Density ( g "~ cm^-3~")"),  #"Unit Draft [%]"
       x = "", title = "", fill = "")+
  coord_cartesian(ylim = c(1, 1.65))+
  # geom_text(aes(y = 260, label = lab_ud_df), size = 5)+
  theme_classic(base_size = 25)+ # base_size = 20
  theme(text=element_text(family="A"), axis.text.x = element_text(angle = 90, hjust = 0),
        legend.position = "none")
ggsave("plots/bd.png", device = "png", width = 10, height = 6, dpi = 300)

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
  select(id, mean, stdDev, date)%>% 
  mutate(index = "NDVI")

library(lubridate)
x <- "631022"
ndvi$date <- ymd(paste0(ndvi$date))

ndvi$date <- as.character(ndvi$date)
ndvi$date <- factor(ndvi$date)

# levels(ndvi$date)
# ndvi$date <- as.numeric(ndvi$date)
# ndvi$date <- as.Date(ndvi$date, origin = "1970-01-01")
# ndvi$date <- format(ndvi$date, "%Y-%m-%d")


ndvi$id <- factor(ndvi$id)
ndvi$mean <- as.numeric(ndvi$mean)
ndvi$stdDev <- as.numeric(ndvi$stdDev)

ndvi_var <- ndvi %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14", "1001", "1002")) %>% 
  mutate(var = case_when(id == "3" ~ "catt",
                         id == "4" ~ "cattSOL",
                         id == "5" ~ "NPKSOL",
                         id == "6" ~ "NPK",
                         id == "9" ~ "pig",
                         id == "10" ~ "pigSOL",
                         id %in% c("13", "1001") ~ "pou",
                         id %in% c("14", "1002") ~ "pouSOL"))

ndvi_var$var <- factor(ndvi_var$var, levels = c("NPK", "NPKSOL",  "catt", "cattSOL", "pig", "pigSOL", "pou", "pouSOL"))

# separated seasons

NDVIs1 <- ndvi_var %>% 
  # filter(startsWith(date, "2017")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "1001", "1002")) %>% 
  mutate(seas = "s1") %>% 
  mutate(crop = "winter wheat") %>% 
  filter(date %in% c("2017-05-11", "2017-06-03", "2017-06-20"))

NDVIs2 <- ndvi_var %>% 
  # filter(startsWith(date, "2018")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s2") %>% 
  mutate(crop = "corn") %>% 
  filter(date %in% c("2018-05-14", "2018-05-21", # "20180411", "20180419", "20180421", "20180429", 
                     "2018-05-31", "2018-07-03"))

NDVIs3 <- ndvi_var %>% 
  # filter(startsWith(date, "2019")) %>%  # , "20190723"
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s3") %>% 
  mutate(crop = "winter wheat") %>% 
  filter(date %in% c("2019-04-01", # "20181011", "20181013", "20181016", "20181018", "20181031", "20181115", "20181117", "20181205",  "20190205", "20190218", "20190228",
                     "2019-04-16", "2019-04-19", "2019-04-21", "2019-06-03",
                     "2019-06-25", "2019-06-30")) 
NDVIs4 <- ndvi_var %>% 
  # filter(startsWith(date, "2020")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s4") %>% 
  mutate(crop = "winter wheat") %>% 
  filter(date %in% c("2020-03-29", "2020-04-05", # "20191031", "20200101", 
                     "2020-04-08", "2020-04-18", "2020-04-20", "2020-04-23",
                     "2020-04-28", "2020-05-18", "2020-06-22", "2020-07-14", "2020-07-22")) 

ndvi_seas <- rbind(NDVIs1, NDVIs2, NDVIs3, NDVIs4)

# plots

# seas facet

to_string <- as_labeller(c("s1" = "winter wheat", "s2" = "corn", "s3" = "winter wheat", "s4" = "winter wheat"))

ndvi_plt <- ggplot(ndvi_seas, aes (date, mean, color = var))+
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
  theme_classic(base_size = 20)+ # base_size = 20
  theme(text=element_text(family="A"), axis.text.x = element_text(angle = 90), 
        legend.position = "top") # axis.text.x = element_text(size = 11) # legend.position = c(0.9, 0.85))
# ggsave("plots/NDVIall.png", device = "png", width = 15, height = 8, dpi = 300)

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

# GEE NDWI ---------------------------------------------------------------------

ndwi <- read.csv("red/NDWImeansd.csv", header = TRUE)

ndwi <- ndwi %>% 
  mutate(date = substr(`system.index`, 1, 8)) %>% 
  select(id, mean, stdDev, date) %>% 
  mutate(index = "NDWI")

ndwi$date <- factor(ndwi$date)
levels(ndwi$date)

ndwi$id <- factor(ndwi$id)
ndwi$mean <- as.numeric(ndwi$mean)
ndwi$stdDev <- as.numeric(ndwi$stdDev)

ndwi_var <- ndwi %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14", "1001", "1002")) %>% 
  mutate(var = case_when(id == "3" ~ "catt",
                         id == "4" ~ "cattSOL",
                         id == "5" ~ "NPKSOL",
                         id == "6" ~ "NPK",
                         id == "9" ~ "pig",
                         id == "10" ~ "pigSOL",
                         id %in% c("13", "1001") ~ "pou",
                         id %in% c("14", "1002") ~ "pouSOL"))

ndwi_var$var <- factor(ndwi_var$var, levels = c("NPK", "NPKSOL",  "catt", "cattSOL", "pig", "pigSOL", "pou", "pouSOL"))

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
  filter(date %in% c("20180514", "20180521", # "20180411", "20180419", "20180421", "20180429", 
                     "20180531", "20180703")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s2")%>% 
  mutate(crop = "corn")
 
NDWIs3 <- ndwi_var %>% 
  filter(date %in% c("20190401", # "20181011", "20181013", "20181016", "20181018", "20181031", "20181115", "20181117", "20181205",  "20190205", "20190218", "20190228",
                     "20190416", "20190419", "20190421", "20190603",
                     "20190625", "20190630")) %>%  # , "20190723"
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s3")%>% 
  mutate(crop = "winter wheat")

NDWIs4 <- ndwi_var %>% 
  filter(date %in% c("20200329", "20200405", # "20191031", "20200101", 
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

ndwi_plt <- ggplot(ndwi_seas, aes (date, mean, color = var))+
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
  theme_classic(base_size = 20)+ # base_size = 20
  theme(text=element_text(family="A"), axis.text.x = element_text(angle = 90), 
        legend.position = "top") # axis.text.x = element_text(size = 11) # legend.position = c(0.9, 0.85))
# ggsave("plots/NDWIall.png", device = "png", width = 15, height = 8, dpi = 300)

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

m2 <- lmer(NDWIs3$mean ~ NDWIs3$var + 1|NDWIs3$date)
summary(m2)

# GEE LAI ---------------------------------------------------------------------

lai <- read.csv("red/LAImeansd.csv", header = TRUE)

lai <- lai %>% 
  mutate(date = substr(`system.index`, 1, 8)) %>% 
  select(id, mean, stdDev, date) %>% 
  mutate(index = "LAI")

lai$date <- factor(lai$date)
levels(lai$date)

lai$id <- factor(lai$id)
lai$mean <- as.numeric(lai$mean)
lai$stdDev <- as.numeric(lai$stdDev)

lai_var <- lai %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14", "1001", "1002")) %>% 
  mutate(var = case_when(id == "3" ~ "catt",
                         id == "4" ~ "cattSOL",
                         id == "5" ~ "NPKSOL",
                         id == "6" ~ "NPK",
                         id == "9" ~ "pig",
                         id == "10" ~ "pigSOL",
                         id %in% c("13", "1001") ~ "pou",
                         id %in% c("14", "1002") ~ "pouSOL"))

lai_var$var <- factor(lai_var$var, levels = c("NPK", "NPKSOL",  "catt", "cattSOL", "pig", "pigSOL", "pou", "pouSOL"))

# separated seasons

LAIs1 <- lai_var %>% 
  filter(date %in% c("20170511", "20170603", "20170620")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "1001", "1002")) %>% 
  mutate(seas = "s1")%>% 
  mutate(crop = "winter wheat")

LAIs2 <- lai_var %>% 
  filter(date %in% c("20180514", "20180521", # "20180411", "20180419", "20180421", "20180429", 
                     "20180531", "20180703")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s2")%>% 
  mutate(crop = "corn")

LAIs3 <- lai_var %>% 
  filter(date %in% c("20190401", # "20181011", "20181013", "20181016", "20181018", "20181031", "20181115", "20181117", "20181205",  "20190205", "20190218", "20190228",
                     "20190416", "20190419", "20190421", "20190603",
                     "20190625", "20190630")) %>%  # , "20190723"
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s3")%>% 
  mutate(crop = "winter wheat")

LAIs4 <- lai_var %>% 
  filter(date %in% c("20200329", "20200405", # "20191031", "20200101", 
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

lai_plt <- ggplot(lai_seas, aes (date, mean, color = var))+
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
  theme_classic(base_size = 20)+ # base_size = 20
  theme(text=element_text(family="A"), axis.text.x = element_text(angle = 90), 
        legend.position = "top") # axis.text.x = element_text(size = 11) # legend.position = c(0.9, 0.85))
# ggsave("plots/LAIall.png", device = "png", width = 15, height = 8, dpi = 300)


# LAI analysis

hist(lai_seas$mean)
shapiro.test(lai_seas$mean)
bartlett.test(lai_seas$mean ~ lai_seas$var) # yep

# install.packages("lme4")
require(lme4)

m3 <- lmer(lai_seas$mean ~ lai_seas$var + 1|lai_seas$date)
summary(m3)

m4 <- lm(lai_seas$mean ~ lai_seas$var)
summary(aov(m4))

# GEE all in one ----------------------------------------------------------

rs <- rbind(ndvi_seas, ndwi_seas, lai_seas)
rs$index <- factor(rs$index, levels = c("NDVI", "NDWI", "LAI"))

to_string <- as_labeller(c("s1" = "winter wheat", "s2" = "corn", 
                           "s3" = "winter wheat", "s4" = "winter wheat",
                           "NDVI" = "NDVI", "NDWI" = "NDWI", "LAI" = "LAI"))

ggplot(rs, aes (date, mean, color = var))+
 # geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 2, position=position_dodge(width = 1))+ # aes(shape = sol)
  labs(y = "Index value", 
       x = "", title = "", color = "")+
  # scale_y_continuous(limits = c(0.8, 1), breaks = seq(0.8, 1, by = 0.1))+
  scale_color_brewer(palette="Spectral")+
  facet_grid(index ~ seas, scales = "free", labeller = to_string)+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 25)+ # base_size = 20
  theme(text=element_text(family="A"), axis.text.x = element_text(angle = 270), 
        legend.position = "top") # axis.text.x = element_text(size = 11) # legend.position = c(0.9, 0.85))
# ggsave("plots/RSall.png", device = "png", width = 14, height = 9, dpi = 300)

ggplot(rs, aes (var, mean))+
  geom_boxplot()+
  labs(y = "Index value", 
       x = "", title = "", color = "")+
  # scale_y_continuous(limits = c(0.8, 1), breaks = seq(0.8, 1, by = 0.1))+
  # scale_color_brewer(palette="Spectral")+
  facet_grid(index ~ seas, scales = "free")+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 25)+ # base_size = 20
  theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(angle = 90), 
        legend.position = "top") # axis.text.x = element_text(size = 11) # legend.position = c(0.9, 0.85))
ggsave("plots/RSall_boxplot.png", device = "png", width = 14, height = 9, dpi = 300)


# GEE analysis emmeans ----------------------------------------------------

# data used:
  # NDVIs1, NDVIs2, NDVIs3, NDVIs4
  # NDWIs1, NDWIs2, NDWIs3, NDWIs4
  # LAIs1, LAIs2, LAIs3, LAIs4

# install.packages("lme4")
require(lme4)
install.packages("emmeans")
require(emmeans) # https://cran.r-project.org/web/packages/emmeans/vignettes/sophisticated.html

# NDVI 

m1 <- lmer(mean ~ var + (1|date), data = NDVIs1)
em1 <- emmeans(m1, "var")
emmeans(m1, pairwise ~ var) 

m2 <- lmer(mean ~ var + (1|date), data = NDVIs2)
em2 <- emmeans(m2, "var")
emmeans(m2, pairwise ~ var) 

m3 <- lmer(mean ~ var + (1|date), data = NDVIs3)
em3 <- emmeans(m3, "var")
emmeans(m3, pairwise ~ var) 

m4 <- lmer(mean ~ var + (1|date), data = NDVIs4)
em4 <- emmeans(m4, "var")
emmeans(m4, pairwise ~ var) 

# NDWI

m5 <- lmer(mean ~ var + (1|date), data = NDWIs1)
em5 <- emmeans(m5, "var")
emmeans(m5, pairwise ~ var) 

m6 <- lmer(mean ~ var + (1|date), data = NDWIs2)
em6 <- emmeans(m6, "var")
emmeans(m6, pairwise ~ var) 

m7 <- lmer(mean ~ var + (1|date), data = NDWIs3)
em7 <- emmeans(m7, "var")
emmeans(m7, pairwise ~ var) 

m8 <- lmer(mean ~ var + (1|date), data = NDWIs4)
em8 <- emmeans(m8, "var")
emmeans(m8, pairwise ~ var) 

# LAI

m9 <- lmer(mean ~ var + (1|date), data = LAIs1)
em9 <- emmeans(m9, "var")
emmeans(m9, pairwise ~ var) 

m10 <- lmer(mean ~ var + (1|date), data = LAIs2)
em10 <- emmeans(m10, "var")
emmeans(m10, pairwise ~ var) 

m11 <- lmer(mean ~ var + (1|date), data = LAIs3)
em11 <- emmeans(m11, "var")
emmeans(m11, pairwise ~ var) 

m12 <- lmer(mean ~ var + (1|date), data = LAIs4)
em12 <- emmeans(m12, "var")
emmeans(m12, pairwise ~ var) 

# REVIEW VIs --------------------------------------------------------------


# N D V I


ndvi <- read.csv("red/NDVImeansd.csv", header = TRUE)

ndvi <- ndvi %>% 
  mutate(date = substr(`system.index`, 1, 8)) %>% 
  select(id, mean, stdDev, date)%>% 
  mutate(index = "NDVI")

library(lubridate)
x <- "631022"
ndvi$date <- ymd(paste0(ndvi$date))

ndvi$date <- as.character(ndvi$date)
ndvi$date <- factor(ndvi$date)

ndvi$id <- factor(ndvi$id)
ndvi$mean <- as.numeric(ndvi$mean)
ndvi$stdDev <- as.numeric(ndvi$stdDev)

ndvi_var <- ndvi %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14", "1001", "1002")) %>% 
  mutate(var = case_when(id == "3" ~ "catt",
                         id == "4" ~ "cattSOL",
                         id == "5" ~ "NPKSOL",
                         id == "6" ~ "NPK",
                         id == "9" ~ "pig",
                         id == "10" ~ "pigSOL",
                         id %in% c("13", "1001") ~ "pou",
                         id %in% c("14", "1002") ~ "pouSOL"))

ndvi_var$var <- factor(ndvi_var$var, levels = c("NPK", "NPKSOL",  "catt", "cattSOL", "pig", "pigSOL", "pou", "pouSOL"))

NDVIs1 <- ndvi_var %>% 
  # filter(startsWith(date, "2017")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "1001", "1002")) %>% 
  mutate(seas = "s1") %>% 
  mutate(crop = "winter wheat") %>% 
  filter(date %in% c("2017-05-11", "2017-06-03", "2017-06-20"))

NDVIs2 <- ndvi_var %>% 
  # filter(startsWith(date, "2018")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s2") %>% 
  mutate(crop = "corn") %>% 
  filter(date %in% c("2018-05-14", "2018-05-21", # "20180411", "20180419", "20180421", "20180429", 
                     "2018-05-31", "2018-07-03"))

NDVIs3 <- ndvi_var %>% 
  # filter(startsWith(date, "2019")) %>%  # , "20190723"
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s3") %>% 
  mutate(crop = "winter wheat") %>% 
  filter(date %in% c("2019-04-01", # "20181011", "20181013", "20181016", "20181018", "20181031", "20181115", "20181117", "20181205",  "20190205", "20190218", "20190228",
                     "2019-04-16", "2019-04-19", "2019-04-21", "2019-06-03",
                     "2019-06-25", "2019-06-30")) 
NDVIs4 <- ndvi_var %>% 
  # filter(startsWith(date, "2020")) %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s4") %>% 
  mutate(crop = "winter wheat") %>% 
  filter(date %in% c("2020-03-29", "2020-04-05", # "20191031", "20200101", 
                     "2020-04-08", "2020-04-18", "2020-04-20", "2020-04-23",
                     "2020-04-28", "2020-05-18", "2020-06-22", "2020-07-14", "2020-07-22")) 

ndvi_seas <- rbind(NDVIs1, NDVIs2, NDVIs3, NDVIs4)


# N D W I


ndwi <- read.csv("red/NDWImeansd.csv", header = TRUE)

ndwi <- ndwi %>% 
  mutate(date = substr(`system.index`, 1, 8)) %>% 
  select(id, mean, stdDev, date) %>% 
  mutate(index = "NDWI")

library(lubridate)
x <- "631022"
ndwi$date <- ymd(paste0(ndwi$date))
ndwi$date <- as.character(ndwi$date)
ndwi$date <- factor(ndwi$date)

ndwi$id <- factor(ndwi$id)
ndwi$mean <- as.numeric(ndwi$mean)
ndwi$stdDev <- as.numeric(ndwi$stdDev)


ndwi$date <- factor(ndwi$date)
levels(ndwi$date)

ndwi$id <- factor(ndwi$id)
ndwi$mean <- as.numeric(ndwi$mean)
ndwi$stdDev <- as.numeric(ndwi$stdDev)

ndwi_var <- ndwi %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14", "1001", "1002")) %>% 
  mutate(var = case_when(id == "3" ~ "catt",
                         id == "4" ~ "cattSOL",
                         id == "5" ~ "NPKSOL",
                         id == "6" ~ "NPK",
                         id == "9" ~ "pig",
                         id == "10" ~ "pigSOL",
                         id %in% c("13", "1001") ~ "pou",
                         id %in% c("14", "1002") ~ "pouSOL"))

ndwi_var$var <- factor(ndwi_var$var, levels = c("NPK", "NPKSOL",  "catt", "cattSOL", "pig", "pigSOL", "pou", "pouSOL"))

# separated seasons

NDWIs1 <- ndwi_var %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "1001", "1002")) %>% 
  mutate(seas = "s1") %>% 
  mutate(crop = "winter wheat") %>% 
  filter(date %in% c("2017-05-11", "2017-06-03", "2017-06-20"))

NDWIs2 <- ndwi_var %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s2") %>% 
  mutate(crop = "corn") %>% 
  filter(date %in% c("2018-05-14", "2018-05-21", # "20180411", "20180419", "20180421", "20180429", 
                     "2018-05-31", "2018-07-03"))

NDWIs3 <- ndwi_var %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s3") %>% 
  mutate(crop = "winter wheat") %>% 
  filter(date %in% c("2019-04-01", # "20181011", "20181013", "20181016", "20181018", "20181031", "20181115", "20181117", "20181205",  "20190205", "20190218", "20190228",
                     "2019-04-16", "2019-04-19", "2019-04-21", "2019-06-03",
                     "2019-06-25", "2019-06-30")) 

NDWIs4 <- ndwi_var %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s4") %>% 
  mutate(crop = "winter wheat") %>% 
  filter(date %in% c("2020-03-29", "2020-04-05", # "20191031", "20200101", 
                     "2020-04-08", "2020-04-18", "2020-04-20", "2020-04-23",
                     "2020-04-28", "2020-05-18", "2020-06-22", "2020-07-14", "2020-07-22")) 

ndwi_seas <- rbind(NDWIs1, NDWIs2, NDWIs3, NDWIs4)


# L A I


lai <- read.csv("red/LAImeansd.csv", header = TRUE)

lai <- lai %>% 
  mutate(date = substr(`system.index`, 1, 8)) %>% 
  select(id, mean, stdDev, date) %>% 
  mutate(index = "LAI")


library(lubridate)
x <- "631022"
lai$date <- ymd(paste0(lai$date))
lai$date <- as.character(lai$date)
lai$date <- factor(lai$date)

lai$date <- factor(lai$date)
levels(lai$date)

lai$id <- factor(lai$id)
lai$mean <- as.numeric(lai$mean)
lai$stdDev <- as.numeric(lai$stdDev)

lai_var <- lai %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14", "1001", "1002")) %>% 
  mutate(var = case_when(id == "3" ~ "catt",
                         id == "4" ~ "cattSOL",
                         id == "5" ~ "NPKSOL",
                         id == "6" ~ "NPK",
                         id == "9" ~ "pig",
                         id == "10" ~ "pigSOL",
                         id %in% c("13", "1001") ~ "pou",
                         id %in% c("14", "1002") ~ "pouSOL"))

lai_var$var <- factor(lai_var$var, levels = c("NPK", "NPKSOL",  "catt", "cattSOL", "pig", "pigSOL", "pou", "pouSOL"))

# separated seasons

LAIs1 <- lai_var %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "1001", "1002")) %>% 
  mutate(seas = "s1") %>% 
  mutate(crop = "winter wheat") %>% 
  filter(date %in% c("2017-05-11", "2017-06-03", "2017-06-20"))

LAIs2 <- lai_var %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s2") %>% 
  mutate(crop = "corn") %>% 
  filter(date %in% c("2018-05-14", "2018-05-21", # "20180411", "20180419", "20180421", "20180429", 
                     "2018-05-31", "2018-07-03"))

LAIs3 <- lai_var %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s3") %>% 
  mutate(crop = "winter wheat") %>% 
  filter(date %in% c("2019-04-01", # "20181011", "20181013", "20181016", "20181018", "20181031", "20181115", "20181117", "20181205",  "20190205", "20190218", "20190228",
                     "2019-04-16", "2019-04-19", "2019-04-21", "2019-06-03",
                     "2019-06-25", "2019-06-30")) 

LAIs4 <- lai_var %>% 
  filter(id %in% c("3", "4", "5", "6", "9", "10", "13", "14")) %>% 
  mutate(seas = "s4") %>% 
  mutate(crop = "winter wheat") %>% 
  filter(date %in% c("2020-03-29", "2020-04-05", # "20191031", "20200101", 
                     "2020-04-08", "2020-04-18", "2020-04-20", "2020-04-23",
                     "2020-04-28", "2020-05-18", "2020-06-22", "2020-07-14", "2020-07-22")) 

lai_seas <- rbind(LAIs1, LAIs2, LAIs3, LAIs4)


# REVIEW plots ------------------------------------------------------------

to_string <- as_labeller(c("s1" = "wheat", "s2" = "corn", "s3" = "wheat", "s4" = "wheat"))


ndvi_plt <- ggplot(ndvi_seas, aes (date, mean, color = var))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 3, position=position_dodge(width = 1))+ # aes(shape = sol)
  labs(y = "NDVI", 
       x = "", title = "", color = "")+
  # scale_y_continuous(limits = c(0.8, 1), breaks = seq(0.8, 1, by = 0.1))+
  scale_color_brewer(palette="Spectral")+
  facet_grid(. ~ seas, labeller = to_string, scales = "free", space = "free_x")+ # 
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 30,)+ # base_size = 20
  theme(text=element_text(family="A"), axis.text.x = element_blank(), 
        legend.position = "top") # axis.text.x = element_text(size = 11) # legend.position = c(0.9, 0.85))

ndvi_plt
# 
# ggsave("plots/NDVIrev.png", device = "png", width = 12, height = 7, dpi = 300)


ndwi_plt <- ggplot(ndwi_seas, aes (date, mean, color = var))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 3, position=position_dodge(width = 1))+ # aes(shape = sol)
  labs(y = "NDWI", 
       x = "", title = "", color = "")+
  # scale_y_continuous(limits = c(0.8, 1), breaks = seq(0.8, 1, by = 0.1))+
  scale_color_brewer(palette="Spectral")+
  facet_grid(. ~ seas, scales = "free", labeller = to_string, space = "free_x")+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 30)+ # base_size = 20
  theme(text=element_text(family="A"), axis.text.x = element_blank(), 
        strip.background = element_blank(), strip.text.x = element_blank(), 
        legend.position = "none") # axis.text.x = element_text(size = 11) # legend.position = c(0.9, 0.85))

# ndwi_plt
# 
# ggsave("plots/NDWIrev.png", device = "png", width = 12, height = 7, dpi = 300)


lai_plt <- ggplot(lai_seas, aes (date, mean, color = var))+
  geom_line(size = .7, position=position_dodge(width = 1))+
  geom_errorbar(aes(ymin=mean-stdDev, ymax=mean+stdDev), width=.5, size = .5, 
                position=position_dodge(width = 1))+ # linetype = sol
  geom_point(size = 3, position=position_dodge(width = 1))+ # aes(shape = sol)
  labs(y = "LAI", 
       x = "", title = "", color = "")+
  # scale_y_continuous(limits = c(0.8, 1), breaks = seq(0.8, 1, by = 0.1))+
  scale_color_brewer(palette="Spectral")+
  facet_grid(. ~ seas, scales = "free", labeller = to_string, space = "free_x")+
  # coord_cartesian(ylim = c(75, 120))+
  # geom_text(aes(y = 118, label = lab_ci),
  # size = 5, position=position_dodge(1))+
  theme_classic(base_size = 30)+ # base_size = 20
  theme(text=element_text(family="A"), axis.text.x = element_text(angle = 90),
        strip.background = element_blank(), strip.text.x = element_blank(), 
        legend.position = "none") # axis.text.x = element_text(size = 11) # legend.position = c(0.9, 0.85))

# lai_plt
# 
# ggsave("plots/LAIrev.png", device = "png", width = 12, height = 7, dpi = 300)
# ggsave("plots/LAIrev.png", device = "png", width = 16, height = 7, dpi = 300)

# install.packages("ggpubr")
require(ggpubr)

ggarrange(ndvi_plt, ndwi_plt, lai_plt, ncol = 1, nrow = 3)
# ggsave("plots/VISrev.png", device = "png", width = 16, height = 16, dpi = 300)
ggsave("plots/VISrev_30.png", device = "png", width = 20, height = 16, dpi = 300)
