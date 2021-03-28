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
require(tidyverse)
require(readxl)
require(reshape2)
require(RColorBrewer)
require(writexl)

# install.packages("extrafont")
library(extrafont)
# font_import()
# loadfonts(device="win")       #Register fonts for Windows bitmap output
# fonts()                       #vector of font family names
##  [1] "Andale Mono"                  "AppleMyungjo"                
##  [3] "Arial Black"                  "Arial"                       
##  [5] "Arial Narrow"                 "Arial Rounded MT Bold"  

data_summary <- function(data, varname, groupnames){ # funkce pro výpočet errorbars
  require(plyr)
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

ud <- read_excel("red/unitd.xlsx")
ud <- ud %>% 
  mutate(udkn = unitdraft/1000)

# pr <- read_excel("red/sust2.xlsx", sheet = 1)
# roh <- read_excel("red/sust2.xlsx", sheet = 2)
# sfh <- read_excel("red/sust2.xlsx", sheet = 3)

# VARIANTS
# 4 cattle manure SOL ; 5 SOL ; 6 control ; 10 pig manure SOL ; 14 poultry (hen) manure SOL

varlev <- c("6", "5", "4", "14", "10")
varlab <- c("C", "SOL", "cSOL", "hSOL", "pSOL")

# UNIT DRAFT --------------------------------------------------------------

# factors

ud$var <- factor(ud$var, levels = varlev, labels = varlab)
ud$year <- factor(ud$year)
                     
ud <- ud %>%
  mutate(apl1 = (year %in% c("2015", "2017", "2018", "2020"))) %>%
  mutate(apl2 = (year %in% c("2016", "2019"))) 

ud <- ud %>%
  mutate(apl = case_when(apl1 == "TRUE" ~ "apl1", 
                              apl2 == "TRUE" ~ "apl2"))
# stats

dfud <- data_summary(ud, varname="unitd", 
                     groupnames=c("year", "var"))
head(dfud)
write_xlsx(dfud,"udstat.xlsx") # package "writexl"

# plots

ggplot(dfud, aes(year, unitd, fill=var))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=unitd-sd, ymax=unitd+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_brewer(palette = "Greys")+
  labs(y = expression("Unit Draft [ kN"~ m^-2~"]"), 
       x = "", title = "", fill = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
# ggsave("plots/ud.png", device = "png", width = 8, height = 4, dpi = 300)

# analysis ----------------------------------------------------------------

ud15 <- ud %>% 
  filter(year == "2015")
ud16 <- ud %>% 
  filter(year == "2016")
ud17 <- ud %>% 
  filter(year == "2017")
ud18 <- ud %>% 
  filter(year == "2018")
ud19 <- ud %>% 
  filter(year == "2019")
ud20 <- ud %>% 
  filter(year == "2020")

# homogenity of variance check

bartlett.test(unitd ~ var, data = ud15)
bartlett.test(unitd ~ var, data = ud16)
bartlett.test(unitd ~ var, data = ud17) # nope
bartlett.test(unitd ~ var, data = ud18) # nope
bartlett.test(unitd ~ var, data = ud19) # nope
bartlett.test(unitd ~ var, data = ud20)

# kw 

kruskal.test(unitd ~ var, data = ud15) # yep
kruskal.test(unitd ~ var, data = ud16)
kruskal.test(unitd ~ var, data = ud17) # yep
kruskal.test(unitd ~ var, data = ud18) # yep
kruskal.test(unitd ~ var, data = ud19) # yep
kruskal.test(unitd ~ var, data = ud20) # yep

# install.packages("pgirmess") # post-hoc testy
require(pgirmess)
 
kruskalmc(unitd ~ var, data = ud15, p=0.05)
kruskalmc(unitd ~ var, data = ud17, p=0.05)
kruskalmc(unitd ~ var, data = ud18, p=0.05)
kruskalmc(unitd ~ var, data = ud19, p=0.05)
kruskalmc(unitd ~ var, data = ud20, p=0.05)

# ## one way ANOVA
# 
# simpud15 <- aov(unitd ~ var, data = ud15)
# simpkraudc2 <- aov(udkn ~ var, data = c2_kraud)
# simpkraudc3 <- aov(udkn ~ var, data = c3_kraud)
# 
# summary(simpud15)
# 
# TukeyHSD(simpud15)
# # plot(TukeyHSD(simp20))
# # install.packages("multcomp")
# require(multcomp)
# summary(glht(simpud15, linfct=mcp(var="Tukey")))

# analysis of year of application

cud <- ud %>% 
  filter(var == "cSOL")
hud <- ud %>% 
  filter(var == "hSOL")
pud <- ud %>% 
  filter(var == "pSOL")
# 
shapiro.test(cud$unitd) # nope
shapiro.test(hud$unitd) # nope
shapiro.test(pud$unitd) # nope

boxplot(cud$unitd[cud$apl1], cud$unitd[cud$apl2]) 
boxplot(hud$unitd[cud$apl1], cud$unitd[hud$apl2]) 
boxplot(pud$unitd[cud$apl1], cud$unitd[pud$apl2]) 

wilcox.test(cud$unitd[cud$apl1], cud$unitd[cud$apl2]) # no diff
wilcox.test(hud$unitd[cud$apl1], cud$unitd[hud$apl2]) # no diff
wilcox.test(pud$unitd[cud$apl1], cud$unitd[pud$apl2]) # no diff

# t.test(cud$unitd[cud$apl1], cud$unitd[cud$apl2])
# t.test(hud$unitd[cud$apl1], cud$unitd[hud$apl2])
# t.test(pud$unitd[cud$apl1], cud$unitd[pud$apl2])


# pismenka pro Vaclava ----------------------------------------------------

# install.packages("multcompView")
# install.packages("pgirmess")
require(multcompView)
library(pgirmess)

kruskal.test(unitd ~ var, data = ud15) 
kmcud15 <- kruskalmc(unitd ~ var, data = ud15, p=0.05)
mcud15=kmcud15$dif.com[,3]
names(mcud15)=row.names(kmcud15$dif.com)
multcompLetters(mcud15)

kruskal.test(unitd ~ var, data = ud16) 
kmcud16 <- kruskalmc(unitd ~ var, data = ud16, p=0.05)
mcud16=kmcud16$dif.com[,3]
names(mcud16)=row.names(kmcud16$dif.com)
multcompLetters(mcud16)

kruskal.test(unitd ~ var, data = ud17) 
kmcud17 <- kruskalmc(unitd ~ var, data = ud17, p=0.05)
mcud17=kmcud17$dif.com[,3]
names(mcud17)=row.names(kmcud17$dif.com)
multcompLetters(mcud17)

kruskal.test(unitd ~ var, data = ud18) 
kmcud18 <- kruskalmc(unitd ~ var, data = ud18, p=0.05)
mcud18=kmcud18$dif.com[,3]
names(mcud18)=row.names(kmcud18$dif.com)
multcompLetters(mcud18)

kruskal.test(unitd ~ var, data = ud19) 
kmcud19 <- kruskalmc(unitd ~ var, data = ud19, p=0.05)
mcud19=kmcud19$dif.com[,3]
names(mcud19)=row.names(kmcud19$dif.com)
multcompLetters(mcud19)

kruskal.test(unitd ~ var, data = ud20) 
kmcud20 <- kruskalmc(unitd ~ var, data = ud20, p=0.05)
mcud20=kmcud20$dif.com[,3]
names(mcud20)=row.names(kmcud20$dif.com)
multcompLetters(mcud20)

