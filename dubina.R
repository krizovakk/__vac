# ***************
# Vaclav - Dubina
# ***************
# 
# 
# 
# poznamky k projektu




# PACKAGES ----------------------------------------------------------------


# install.packages("tidyverse", dependencies = T)
require(tidyverse)
# install.packages("lme4") # ANOVA 1|date
library(lme4)




# BASE  -------------------------------------------------------------------


path_in <- "G:/TF/_VAC_/2022_dubina/raw/" # UPRAVIT
path_out<- "G:/TF/_VAC_/2022_dubina/out/" # UPRAVIT

# index <- "LAI" 
# index <- "NDVI"
index <- "NDWI"


start_date18 <- as.Date("2017-05-08")
end_date18 <- as.Date("2018-07-23")

start_date19 <- as.Date("2018-10-04")
end_date19 <- as.Date("2019-07-23")

start_date20 <- as.Date("2020-03-28")
end_date20 <- as.Date("2020-08-01")



# LOAD --------------------------------------------------------------------



# *** READ *** READ *** READ ** READ ***
df1 <- read.table(paste0(path_in, index, "meansd.csv"), header = TRUE, sep = ",")
# *** READ *** READ *** READ ** READ ***

df1 <- df1 %>% 
  rename("date"= system.index) %>% 
  rename("sd"= stdDev) %>% 
  mutate(date = substr(date, 1, 8)) %>% 
  mutate(year = substr(date, 1, 4)) 

df1$date <- as.Date(df1$date, tryFormats = c("%Y%m%d", "%Y/%m/%d"))

df1 <- df1 %>% 
  mutate(seas = case_when(date %in% start_date18:end_date18 ~ "2018",
                          date %in% start_date19:end_date19 ~ "2019", 
                          date %in% start_date20:end_date20 ~ "2020")) %>% 
  na.omit()


df1$date <- factor(df1$date)
df1$id <- factor(df1$id)
df1$seas <- factor(df1$seas)


# VIS ---------------------------------------------------------------------



ggplot(df1, aes(date, mean, color = id, group = id))+
  geom_point()+
  geom_path()+
  # geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
  #               position=position_dodge(.9))+
  labs(y = "mean", x = "date", color = "", title = paste0(index))+
  facet_grid(seas ~ .)+
  theme_classic(base_size = 15)+
  theme(axis.text.x = element_text(angle = 270, hjust = 0), legend.position="top")

ggsave(paste0(path_out, index, "_line.png"), device = "png", width = 10, height = 6, dpi = 300)



# ANOVA -------------------------------------------------------------------

# season <- "2018"
# season <- "2019"
season <- "2020"


df2 <- df1 %>% 
  filter(seas == season) 


library(lme4)
# install.packages("lmerTest")
library(lmerTest)

mod = lmer(mean ~ id + (1|date),
            data=df2,
            REML=TRUE)


anova(mod)
summary(mod)
rand(mod)

# install.packages("multcomp")
require(multcomp)

tuk <- glht(mod, linfct=mcp(id="Tukey"))
summary(tuk)          # standard display
tuk.cld <- cld(tuk)   # letter-based display
tuk.cld


# FUC (frequently used commands :D) ---------------------------------------


# *** WR *** WR *** WR ** WR ***
# write.table(df1, file = paste0(path_out, crop, "/epic_inputs/WHE_crop_cal.csv"), row.names=FALSE, sep = ";") # UPRAVIT
# *** WR *** WR *** WR ** WR ***
