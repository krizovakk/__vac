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
require(tidyverse)
require(readxl)
require(reshape2)
require(RColorBrewer)
require(writexl)
require(plyr)

# install.packages("extrafont")
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

ud <- read_excel("red/iud_new_plot.xlsx")

varlev <- c("3", "4", "5", "6", "9", "10", "13", "14")
varlab <- c("3", "4", "5", "6", "9", "10", "13", "14")

# UNIT DRAFT --------------------------------------------------------------

# factors

ud$var <- factor(ud$var, levels = varlev, labels = varlab)
ud$term <- factor(ud$term)

ud <- ud %>%
  mutate(sol_f = (var %in% c("4", "5", "10", "14"))) %>%
  mutate(sol_t = (var %in% c("3", "6", "9", "13"))) 
ud <- ud %>%
  mutate(sol = case_when(sol_f == "TRUE" ~ "FALSE", 
                         sol_t == "TRUE" ~ "TRUE"))

ud <- ud %>%
  mutate(duo = case_when(var %in% c("3","4") ~ c("cattle"), 
                         var %in% c("5","6") ~ "npk", 
                         var %in% c("9","10") ~ "pig", 
                         var %in% c("13","14") ~ "poultry"))

dfud <- data_summary(ud, varname="unitd", 
                     groupnames=c("term", "var", "sol", "duo"))
head(dfud)
write_xlsx(dfud,"prstat.xlsx") # package "writexl"

# STATISTICA-like plot ----------------------------------------------------

ggplot(dfud, aes(term, unitd, group=sol))+
  geom_line(aes(color = sol), size = 1, position=position_dodge(width = .5))+
  geom_errorbar(aes(ymin=unitd-sd, ymax=unitd+sd, color = sol), width=.5, size = 1, position=position_dodge(width = .5))+
  geom_point(aes(color = sol), size = 2, position=position_dodge(width = .5))+
  facet_grid(. ~ dfud$duo)+
  # geom_text(aes(y = 123, label = labelkwud),
  #           size = 3, position=position_dodge(0.9))+
  # scale_fill_brewer(palette = "Greys")+
  labs(y = expression("Unit Draft [ kN"~ m^-2~"]"), 
       x = "", title = "", fill = "")+
  scale_color_discrete(name="SOL")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"), axis.text.x = element_text(size = 9))
ggsave("plots/statlike_ud.png", device = "png", width = 8, height = 4, dpi = 300)


