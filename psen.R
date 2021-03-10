# PLANT, SOIL & ENVIRONMENT
# Vaclav Novak, Petr Novak, Petr Sarec, KK

# three variants 
# 5051 FYM+ZFIX+NPK -> FYM_ZF
# 5052 FYM+NPK -> FYM
# 5053 NPK -> C

# penres
# unit draft
# reduced bulk density
# saturated hydraulic conductivity
# rain simulator
# remote sensing / Sentinel-2

#base --------------------------------------------------------------------
# 
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")
# install.packages("psych")
# install.packages("writexl")

require(tidyverse)
require(readxl)
require(reshape2)
require(psych)
require(writexl)

palet3 <- c("grey92", "darkgrey", "grey40")
palet4 <- c("grey92", "darkgrey", "grey40", "grey10")

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

# Times New Roman ----------------------------------------------------------

# install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()                       #vector of font family names
##  [1] "Andale Mono"                  "AppleMyungjo"                
##  [3] "Arial Black"                  "Arial"                       
##  [5] "Arial Narrow"                 "Arial Rounded MT Bold"  

# PENRES ------------------------------------------------------------------

# pen - analysis
# penl - plots, errorbars caclulation

pen <- read_excel("red/penres.xlsx") 

pen <- pen %>% 
  select(year, Variant_No, Depth_4, Depth_8, Depth_12, Depth_16, Depth_20)

pen <- pen[-c(27), ] # row with extreme values deleted

colnames(pen)[1] <- "seas"
colnames(pen)[2] <- "var"
colnames(pen)[3] <- "cm4"
colnames(pen)[4] <- "cm8"
colnames(pen)[5] <- "cm12"
colnames(pen)[6] <- "cm16"
colnames(pen)[7] <- "cm20"

pen$var[pen$var == 5051] <- "FYM_ZF"
pen$var[pen$var == 5052] <- "FYM"
pen$var[pen$var == 5053] <- "C"

penl <- pen %>%
  select(seas, var, cm4, cm8, cm12, cm16, cm20) %>%
  melt(id.vars = c("seas", "var"), variable.name = ("depth"), value.name = "penres")

# penres errorbar comp ----------------------------------------------------

df1 <- data_summary(penl, varname="penres", 
                    groupnames=c("seas", "var", "depth"))

# Convert dose to a factor variable
df1$var=as.factor(df1$var)
df1$seas <- factor(df1$seas)
head(df1)

# descriptive statistics

write_xlsx(df1,"penstat.xlsx") # package "writexl"

# penres explorative ------------------------------------------------------

dept <- c("20", "16", "12", "8", "4")

df1$depth <- factor(df1$depth, levels = c("cm20", "cm16", "cm12", "cm8", "cm4"), 
                    labels = dept)
df1$seas <- factor(df1$seas, levels = c("2018", "2019", "2020"))
df1$var <- factor(df1$var, levels = c("FYM_ZF", "FYM", "C"))

ggplot(df1, aes(depth, penres, fill=var))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=penres-sd, ymax=penres+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_manual(values= palet3, breaks = rev(levels(df1$var)))+
  coord_flip()+
  facet_grid(. ~ seas)+
  labs(y = "\nPenetration Resistance [MPa]", x = "Depth [cm]", fill = "", title = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
# ggsave("plots/penres_se.png", device = "png", width = 8, height = 4, dpi = 300)

# penres analysis ---------------------------------------------------------

p18 <- pen %>%
  filter(seas == 2018) %>% 
  select(var, cm4, cm8, cm12, cm16, cm20)

p19 <- pen %>%
  filter(seas == 2019) %>% 
  select(var, cm4, cm8, cm12, cm16, cm20)

p20 <- pen %>%
  filter(seas == 2020) %>% 
  select(var, cm4, cm8, cm12, cm16, cm20)

#2018
pairwise.wilcox.test(p18$cm4, p18$var,
                     p.adjust.method = "BH")

# significant difference is where p-value < 0.05
# diff everywhere except Var 4 & control

pairwise.wilcox.test(p18$cm8, p18$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p18$cm12, p18$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p18$cm16, p18$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p18$cm20, p18$var,
                     p.adjust.method = "BH")

#2019

pairwise.wilcox.test(p19$cm4, p19$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p19$cm8, p19$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p19$cm12, p19$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p19$cm16, p19$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p19$cm20, p19$var,
                     p.adjust.method = "BH")

#2020

pairwise.wilcox.test(p20$cm4, p20$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p20$cm8, p20$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p20$cm12, p20$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p20$cm16, p20$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p20$cm20, p20$var,
                     p.adjust.method = "BH")

# UNITD -------------------------------------------------------------------

uni <- read_excel("red/unitd.xlsx")

uni <- uni %>% 
  select(Trial_Date_Label_En, Variant_No, `Unit Draft-Depth-Level`)

colnames(uni)[1] <- "seas"
colnames(uni)[2] <- "var"
colnames(uni)[3] <- "unitd"


uni$var[uni$var == 5051] <- "FYM_ZF"
uni$var[uni$var == 5052] <- "FYM"
uni$var[uni$var == 5053] <- "C"
uni$seas[uni$seas == 'Autumn 2018'] <- "2018"
uni$seas[uni$seas == 'Autumn 2019'] <- "2019"
uni$seas[uni$seas == 'Autumn 2020'] <- "2020"

uni <- uni %>% 
  mutate(kn = unitd/1000) # kn = kiloNewton

# unitd errorbars calc ----------------------------------------------------

df2 <- data_summary(uni, varname="kn", # absolute values in kN
                    groupnames=c("seas", "var"))
# Convert dose to a factor variable
df2$var=as.factor(df2$var)
df2$seas <- factor(df2$seas)
head(df2)

write_xlsx(df2,"unitdstat.xlsx")

# unitd explorative -------------------------------------------------------

ggplot(df2, aes(seas, kn, fill = var))+
  geom_bar(stat = "identity", color="black", 
           position = position_dodge())+
  geom_errorbar(aes(ymin=kn-sd, ymax=kn+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_manual(values = palet3)+
  labs(y = expression("Unit Draft [ kN"~ m^-2~"]"), 
       x = "", fill = "", title = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
# ggsave("plots/unitd_se_kN.png", device = "png", width = 8, height = 5, dpi = 300)

# unitd analysis ----------------------------------------------------------

u18 <- uni %>%
  filter(seas == 2018) %>% 
  select(var, unitd)

u19 <- uni %>%
  filter(seas == 2019) %>% 
  select(var, unitd)

u20 <- uni %>%
  filter(seas == 2020) %>% 
  select(var, unitd)


pairwise.wilcox.test(u18$unitd, u18$var,
                     p.adjust.method = "BH")
# significant difference is where p-value < 0.05
# diff everywhere except Var 4 & control

pairwise.wilcox.test(u19$unitd, u19$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(u20$unitd, u20$var,
                     p.adjust.method = "BH")

# INFILTRATION (KFS) ------------------------------------------------------------

inf <- read_excel("red/inf.xlsx")

colnames(inf)[1] <- "var"

inf$var[inf$var == 5051] <- "FYM_ZF"
inf$var[inf$var == 5052] <- "FYM"
inf$var[inf$var == 5053] <- "C"

infl <- inf %>%
  melt(id.vars = "var", variable.name = "seas", value.name = "inf")

# inf errorbar comp -------------------------------------------------------

df3 <- data_summary(infl, varname="inf", 
                    groupnames=c("seas", "var"))
# Convert dose to a factor variable
df3$var=as.factor(df3$var)
df3$seas <- factor(df3$seas)
head(df3)

write_xlsx(df3,"infstat.xlsx")

# inf explorative ---------------------------------------------------------

ggplot(df3, aes(seas, inf, fill=var)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=inf-sd, ymax=inf+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_manual(values = palet3)+
  labs(y = expression("Saturated Hydraulic Conductivity [ mm"~ h^-1~"]"), 
       x = "", fill = "", title = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
ggsave("plots/inf_se.png", device = "png", width = 8, height = 5, dpi = 300)

# inf analysis ------------------------------------------------------------

i18 <- infl %>%
  filter(seas == 2018) %>% 
  select(var, inf)

i19 <- infl %>%
  filter(seas == 2019) %>% 
  select(var, inf)

i20 <- infl %>%
  filter(seas == 2020) %>% 
  select(var, inf)

pairwise.wilcox.test(i18$inf, i18$var,
                     p.adjust.method = "BH")
# significant difference is where p-value < 0.05
# diff everywhere except Var 4 & control

pairwise.wilcox.test(i19$inf, i19$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(i20$inf, i20$var,
                     p.adjust.method = "BH")

# RBD ---------------------------------------------------------------------

rbd <- read_excel("red/rbd.xlsx")

rbd <- rbd %>% 
  select(seas, Variant_No, ROH)

colnames(rbd)[1] <- "seas"
colnames(rbd)[2] <- "var"
colnames(rbd)[3] <- "rbd"

rbd$var[rbd$var == 5051] <- "FYM_ZF"
rbd$var[rbd$var == 5052] <- "FYM"
rbd$var[rbd$var == 5053] <- "C"

# rbd errorbar comp -------------------------------------------------------

df4 <- data_summary(rbd, varname="rbd", 
                    groupnames=c("seas", "var"))
# Convert dose to a factor variable
df4$var=as.factor(df4$var)
df4$seas <- factor(df4$seas)
head(df4)

write_xlsx(df4,"rbdstat.xlsx")


# rbd explorative ---------------------------------------------------------

ggplot(df4, aes(seas, rbd, fill = var))+
  geom_bar(stat="identity", color = "black",
           position=position_dodge())+
  geom_errorbar(aes(ymin=rbd-sd, ymax=rbd+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_manual(values = palet3)+
  labs(y = expression("Reduced Bulk Density [ g"~ cm^-3~"]"), x = "", fill = "")+
  coord_cartesian(ylim = c(1.0, 1.5))+ # omezeni y osy, aby byly lepe videt rozdily
  theme(legend.title=element_blank())+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
ggsave("plots/rbd_se.png", device = "png", width = 8, height = 4, dpi = 300)

# METEO -------------------------------------------------------------------

met <- read_excel("red/meteo.xlsx")
colnames(met) [1] <- "month"

met$month <- factor(met$month, levels = c("jan", "feb", "mar", "apr", "may", "jun",
                               "jul", "aug", "sept", "oct", "nov", "dec"))

met <- met %>% 
  melt(id.vars = c("month"), variable.name = ("year"), value.name = "rain")

ggplot(met, aes(month, rain, fill=year))+
  geom_bar(stat="identity", color = "black", 
           position=position_dodge())+
  scale_fill_manual(values = palet4, 
                    name = "", 
                    labels = c("2018", "2019", "2020", 
                               "longterm normal (1981-2010)"))+
  labs(y = "Sum of Precipitation [mm]", x = "", fill = "")+
  theme_classic(base_size = 15)+
  theme(legend.position="top")+
  theme(text=element_text(family="Times New Roman"))
ggsave("plots/meteo.png", device = "png", width = 8, height = 5, dpi = 300)

# RS NDVI -----------------------------------------------------------------

rs18 <- read_excel("red/2018_results.xlsx")
rs19 <- read_excel("red/2019_results.xlsx")
rs20 <- read_excel("red/2020_results.xlsx")

term18 <- read_excel("red/2018_terminy.xlsx")
term19 <- read_excel("red/2019_terminy.xlsx")
term20 <- read_excel("red/2020_terminy.xlsx")

rsl18 <- rs18 %>% 
  melt(id.vars = c("var", "pixid"), variable.name = "term", value.name = "ndvi")
rsl19 <- rs19 %>% 
  melt(id.vars = c("var", "pixid"), variable.name = "term", value.name = "ndvi")
rsl20 <- rs20 %>% 
  melt(id.vars = c("var", "pixid"), variable.name = "term", value.name = "ndvi")

# rs explorative ----------------------------------------------------------

paletblue <- c("azure3", "lightskyblue", "blue3")

# 2018

rsl18$var <- as.factor(rsl18$var)
rsl18$term <- factor(rsl18$term, 
                        labels = c( 
                          "2018-04-19", "2018-04-21", "2018-04-29",
                          "2018-05-04", "2018-05-21", "2018-05-26",
                          "2018-07-03", "2018-07-05", "2018-08-22",
                          "2018-08-27", "2018-08-29", "2018-09-18",
                          "2018-09-28", "2018-10-11", "2018-10-16", "2018-10-18"))


ggplot(rsl18, aes(term, ndvi, fill = var))+
  geom_boxplot()+
  scale_fill_manual(values = paletblue)+
  labs(y = "NDVI", x = "", fill = "", title = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"), legend.position="top",
        axis.text.x = element_text(angle = 90))
# ggsave("plots/18ndvi.png", device = "png", width = 8, height = 5, dpi = 300)

# 2019

rsl19$var <- as.factor(rsl19$var)
rsl19$term <- factor(rsl19$term, 
                     labels = c("2019-04-01", "2019-04-04", "2019-04-21",
                                "2019-05-26", "2019-06-03", "2019-06-05", "2019-06-15"))

ggplot(rsl19, aes(term, ndvi, fill = var))+
  geom_boxplot()+
  scale_fill_manual(values = paletblue)+
  labs(y = "NDVI", x = "", fill = "", title = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"), legend.position="top",
        axis.text.x = element_text(angle = 90))
# ggsave("plots/19ndvi.png", device = "png", width = 8, height = 5, dpi = 300)

# 2020

rsl20$var <- as.factor(rsl20$var)
rsl20$term <- factor(rsl20$term,
                     labels = c("2019-10-31", "2019-11-30", "2020-03-24", 
                                "2020-03-29", "2020-04-05", "2020-04-08",
                                "2020-04-20", "2020-04-23", "2020-04-28",
                                "2020-05-08", "2020-05-18", "2020-06-22", "2020-06-27"))

ggplot(rsl20, aes(term, ndvi, fill = var))+
  geom_boxplot()+
  scale_fill_manual(values = paletblue)+
  labs(y = "NDVI", x = "", fill = "", title = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"), legend.position="top",
        axis.text.x = element_text(angle = 90))
# ggsave("plots/20ndvi.png", device = "png", width = 8, height = 5, dpi = 300)

# rs analysis -------------------------------------------------------------

## KW non-parametric ANOVA-like test

kruskal.test(ndvi ~ var, data = rsl18)
kruskal.test(ndvi ~ var, data = rsl19)
kruskal.test(ndvi ~ var, data = rsl20)

# install.packages("pgirmess")
require(pgirmess)

kruskalmc(ndvi ~ var, data = rsl18, p=0.05)
kruskalmc(ndvi ~ var, data = rsl19, p=0.05)
kruskalmc(ndvi ~ var, data = rsl20, p=0.05)

## one way ANOVA

bartlett.test(ndvi ~ var, data = rsl18) # homognita varianci splnena
simp18 <- aov(ndvi ~ var, data = rsl18)
simp19 <- aov(ndvi ~ var, data = rsl19)
simp20 <- aov(ndvi ~ var, data = rsl20)
summary(simp18)
summary(simp19)
summary(simp20)
TukeyHSD(simp18)
TukeyHSD(simp20)
# plot(TukeyHSD(simp18))
# plot(TukeyHSD(simp20))

# install.packages("multcomp")
require(multcomp)
summary(glht(simp18, linfct=mcp(var="Tukey")))
summary(glht(simp20, linfct=mcp(var="Tukey")))

## ANOVA with random effects

# testovani homogenity varianci
# variance chceme mit homogenni (tzn. p-value > 0.05)
# pokud neni splneno, lze pouzit logartimickou transformaci

bartlett.test(ndvi ~ var, data = rsl18) # p-value = 0.2499 = homogenni
bartlett.test(ndvi ~ var, data = rsl19) # p-value = 0.1052
bartlett.test(ndvi ~ var, data = rsl20) # p-value < 2.2e-16 = homgenita zamitnuta, variance se lisi
bartlett.test(log(ndvi) ~ var, data = rsl20) # p-value < 2.2e-16 -> nelze pouzit param test!

# ANOVA pomerne robustni vuci naruseni predpokladu ve velkych souborech
# dulezite je spise chovani rezidualu

## https://rcompanion.org/handbook/G_03.html

if(!require(FSA)){install.packages("FSA")}
if(!require(psych)){install.packages("psych")}
if(!require(lme4)){install.packages("lme4")}
if(!require(lmerTest)){install.packages("lmerTest")}
if(!require(nlme)){install.packages("nlme")}
if(!require(car)){install.packages("car")}

library(lme4)
library(lmerTest)

rm18 = lmer(ndvi ~ var + (1|term),
             data=rsl18,
             REML=TRUE)

rm19 = lmer(ndvi ~ var + (1|term),
             data=rsl19,
             REML=TRUE)

rm20 = lmer(ndvi ~ var + (1|term),
             data=rsl20,
             REML=TRUE)

anova(rm18)
summary(rm18)
rand(rm18)

anova(rm19)
summary(rm19)
rand(rm19)

anova(rm20)
summary(rm20)
rand(rm20)

# install.packages("emmeans") # for multiple comparisons
library(emmeans)

emmeans(rm18, list(pairwise ~ var), adjust = "tukey")
emmeans(rm19, list(pairwise ~ var), adjust = "tukey")
emmeans(rm20, list(pairwise ~ var), adjust = "tukey")

# RAIN simulator ----------------------------------------------------------

rain <- read_excel("red/dest.xlsx", sheet = 2)

rain$depth <- as.factor(rain$depth)
rain$var <- as.factor(rain$var)
rain$year <- as.factor(rain$year)

# rainl <- rain %>% 
#   melt(id.vars = c("var", "depth"), variable.name = "year", value.name = "blue")

# rain errorbar -----------------------------------------------------------

df5 <- data_summary(rain, varname="value", 
                    groupnames=c("year", "var", "depth"))
head(df5)

write_xlsx(df5,"rainstat.xlsx") # package "writexl"

# rain explorative --------------------------------------------------------

df5$depth <- factor(df5$depth, levels = c("e", "d", "c", "b", "a"))
# df1$seas <- factor(df1$seas, levels = c("2018", "2019", "2020"))
df5$var <- factor(df5$var, levels = c("FYM_ZF", "FYM", "C"))

ggplot(df5, aes(depth, value, fill=var))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_manual(values= palet3, breaks = rev(levels(df5$var)))+
  coord_flip()+
  facet_grid(. ~ year)+
  labs(y = "\nBlue dye in soil profile [%]",
       x = "Depth level", fill = "", title = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
# ggsave("plots/blue pix_se.png", device = "png", width = 8, height = 4, dpi = 300)

# rain analysis ------------------------------------------------------------

rain18 <- rain %>%
  filter(year == 2018)

rain19 <- rain %>%
  filter(year == 2019) 

rain20 <- rain %>%
  filter(year == 2020) 

# KW

kruskal.test(value ~ var, data = rain18)
kruskal.test(value ~ var, data = rain19)
kruskal.test(value ~ var, data = rain20)

# install.packages("pgirmess")
require(pgirmess)

kruskalmc(value ~ var, data = rain18, p=0.05)
kruskalmc(value ~ var, data = rain19, p=0.05)
kruskalmc(value ~ var, data = rain20, p=0.05)

# pairwise wilcox

pairwise.wilcox.test(rain18$value, rain18$var,
                     p.adjust.method = "BH")
pairwise.wilcox.test(rain19$value, rain19$var,
                     p.adjust.method = "BH")
pairwise.wilcox.test(rain20$value, rain20$var,
                     p.adjust.method = "BH")

