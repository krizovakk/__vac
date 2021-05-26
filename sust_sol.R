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

ud <- read_excel("red/unitd.xlsx")
ud <- ud %>%
  mutate(udkn = unitdraft/1000)

pr <- read_excel("red/penres.xlsx", sheet = 1)
roh <- read_excel("red/roh.xlsx", sheet = 1)
# sfh <- read_excel("red/sust2.xlsx", sheet = 3)

# VARIANTS
# 4 cattle manure SOL ; 5 SOL ; 6 control ; 10 pig manure SOL ; 14 poultry (hen) manure SOL

varlev <- c("6", "5", "4", "14", "10")
varlab <- c("C", "SOL", "cSOL", "hSOL", "pSOL")


# UNIT DRAFT --------------------------------------------------------------

# factors

ud$var <- factor(ud$var, levels = varlev, labels = varlab)
# ud$year <- factor(ud$year)
ud$term <- factor(ud$term)

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

labelkwud <- c("a","a","a", "b","b","a","a","a","a","a","a", 
               "b","ac","bc","bc","abc","a","ab","bc","c",
               "ab","a","b","ab","ab","ab","a","ab","a","b" )

ggplot(dfud, aes(year, unitd, fill=var))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=unitd-sd, ymax=unitd+sd), width=.2,
                position=position_dodge(.9))+
  geom_text(aes(y = 123, label = labelkwud),
            size = 3, position=position_dodge(0.9))+
  scale_fill_brewer(palette = "Greys")+
  labs(y = expression("Unit Draft [ kN"~ m^-2~"]"), 
       x = "", title = "", fill = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
# ggsave("plots/ud.png", device = "png", width = 8, height = 4, dpi = 300)
# ggsave("plots/ud_kwlabel.png", device = "png", width = 8, height = 4, dpi = 300)

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

# PENRES ------------------------------------------------------------------

pr <- read_excel("red/penres.xlsx", sheet = 1)

pr <- pr %>% 
  select(year, var, d04, d08, d12, d16, d20) %>% 
  mutate(apl1 = (year %in% c("2015", "2017", "2018", "2020"))) %>%
  mutate(apl2 = (year %in% c("2016", "2019"))) %>% 
  mutate(apl = case_when(apl1 == "TRUE" ~ "apl1", 
                         apl2 == "TRUE" ~ "apl2")) %>% 
  select(year, var, apl, d04, d08, d12, d16, d20) %>% 
  melt(id.vars = c("var", "year", "apl"), 
       variable.name = ("depth"), value.name = "penres") 

# factors

# pr$var <- factor(pr$var, levels = varlev, labels = varlab)
# # prl$var <- factor(prl$var, levels = c("pSOL", "hSOL", "cSOL", "SOL", "C"))
# pr$year <- factor(pr$year)
# pr$apl <- factor(pr$apl)
# 
# # dept <- c("20", "16", "12", "8", "4")
# # prl$depth <- factor(prl$depth, labels = dept)
# # prl$depth <- fct_rev(prl$depth)

dept <- c("20", "16", "12", "8", "4")

pr$depth <- factor(pr$depth, levels = c("d20", "d16", "d12", "d08", "d04"), 
                   labels = dept)
pr$year <- factor(pr$year)
pr$var <- factor(pr$var, levels = c("10", "14", "4", "5", "6"), 
                 labels = c("pSOL", "hSOL", "cSOL", "SOL", "C"))

# stats

dfpr <- data_summary(pr, varname="penres", 
                     groupnames=c("year", "var", "depth"))
head(dfpr)
write_xlsx(dfpr,"prstat.xlsx") # package "writexl"

# plots

# labelkwud <- c("a","a","a", "b","b","a","a","a","a","a","a", 
#                "b","ac","bc","bc","abc","a","ab","bc","c",
#                "ab","a","b","ab","ab","ab","a","ab","a","b")

palet5 <- c("white", "grey82", "darkgrey", "grey40", "grey10")

ggplot(dfpr, aes(depth, penres, fill=var))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=penres-sd, ymax=penres+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_manual(values = palet5, breaks = rev(levels(dfpr$var)))+
  coord_flip()+
  facet_grid(. ~ year)+
  labs(y = "\nPenetration Resistance [MPa]", x = "Depth [cm]", 
       fill = "", title = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
# ggsave("plots/pr.png", device = "png", width = 8, height = 4, dpi = 300)
# ggsave("plots/pr_kwlabel.png", device = "png", width = 8, height = 4, dpi = 300)

# analysis ----------------------------------------------------------------

pr15 <- pr %>% 
  filter(year == "2015")
pr16 <- pr %>% 
  filter(year == "2016")
pr17 <- pr %>% 
  filter(year == "2017")
pr18 <- pr %>% 
  filter(year == "2018")
pr19 <- pr %>% 
  filter(year == "2019")
pr20 <- pr %>% 
  filter(year == "2020")

# homogenity of variance check

bartlett.test(penres ~ var, data = pr15)
bartlett.test(penres ~ var, data = pr16)
bartlett.test(penres ~ var, data = pr17) # nope
bartlett.test(penres ~ var, data = pr18) # nope
bartlett.test(penres ~ var, data = pr19) # nope
bartlett.test(penres ~ var, data = pr20)

# kw 

kruskal.test(penres ~ var, data = pr15) # yep
kruskal.test(penres ~ var, data = pr16)
kruskal.test(penres ~ var, data = pr17) # yep
kruskal.test(penres ~ var, data = pr18) # yep
kruskal.test(penres ~ var, data = pr19) # yep
kruskal.test(penres ~ var, data = pr20) # yep

# install.packages("pgirmess") # post-hoc testy
require(pgirmess)

kruskalmc(penres ~ var, data = pr15, p=0.05)
kruskalmc(penres ~ var, data = pr17, p=0.05)
kruskalmc(penres ~ var, data = pr18, p=0.05)
kruskalmc(penres ~ var, data = pr19, p=0.05)
kruskalmc(penres ~ var, data = pr20, p=0.05)

# ## one way ANOVA
# 
# simppr15 <- aov(penres ~ var, data = pr15)
# simpkraprc2 <- aov(prkn ~ var, data = c2_krapr)
# simpkraprc3 <- aov(prkn ~ var, data = c3_krapr)
# 
# summary(simppr15)
# 
# TukeyHSD(simppr15)
# # plot(TukeyHSD(simp20))
# # install.packages("multcomp")
# require(multcomp)
# summary(glht(simppr15, linfct=mcp(var="Tukey")))

# analysis of year of application

cpr <- pr %>% 
  filter(var == "cSOL")
hpr <- pr %>% 
  filter(var == "hSOL")
ppr <- pr %>% 
  filter(var == "pSOL")

cpr04 <- cpr %>% 
  filter(depth == "d04")

shapiro.test(cpr04$penres) # nope
shapiro.test(hpr$penres) # nope
shapiro.test(ppr$penres) # nope

boxplot(cpr04$penres[cpr$apl1], cpr04$penres[cpr$apl2]) 
boxplot(hpr$penres[cpr$apl1], cpr$penres[hpr$apl2]) 
boxplot(ppr$penres[cpr$apl1], cpr$penres[ppr$apl2]) 

wilcox.test(cpr04$penres[cpr04$apl1], cpr04$penres[cpr04$apl2]) # no diff
wilcox.test(hpr$penres[cpr$apl1], cpr$penres[hpr$apl2]) # no diff
wilcox.test(ppr$penres[cpr$apl1], cpr$penres[ppr$apl2]) # no diff


# ROH ---------------------------------------------------------------------

roh <- read_excel("red/roh.xlsx", sheet = 1)

colnames(roh)[3] <- "year"
colnames(roh)[4] <- "var"
colnames(roh)[5] <- "roh"

roh$var <- factor(roh$var, levels = varlev, labels = varlab)
roh$year <- factor(roh$year)

# roh <- roh %>%
#   mutate(apl1 = (year %in% c("2015", "2017", "2018", "2020"))) %>%
#   mutate(apl2 = (year %in% c("2016", "2019"))) 

# stats

dfroh <- data_summary(roh, varname="roh", 
                      groupnames=c("year", "var"))
head(dfroh)
write_xlsx(dfroh,"rohstat.xlsx") # package "writexl"

# explorative

ggplot(dfroh, aes(year, roh, fill=var))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=roh-sd, ymax=roh+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_brewer(palette = "Greys")+
  labs(y = expression("Reduced Bulk Density [ g"~ cm^-3~"]"), 
       x = "", title = "", fill = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
# ggsave("plots/roh.png", device = "png", width = 8, height = 4, dpi = 300)

# DPZ ---------------------------------------------------------------------

dpz <- read_excel("red/dpz.xlsx", sheet = 1)

dpz$var <- as.character(dpz$var)
dpz$var <- factor(dpz$var, levels = c("3", "4", "5", "6", "9", "10", "13", "14"),
                  labels = c("cattle", "cattleSOL", "npkSOL", "npk", "pig", "pigSOL", "hen", "henSOL"))
dpz$term <- factor(dpz$term)


# residuals check ---------------------------------------------------------

install.packages("olsrr") # https://cran.r-project.org/web/packages/olsrr/vignettes/residual_diagnostics.html
require("olsrr")

model <- lm(ndvi ~ var, data = dpz)
ols_plot_resid_qq(model)
ols_test_normality(model)
ols_test_correlation(model)
ols_plot_resid_fit(model)
ols_plot_resid_hist(model)

par(mfrow = c(2, 2))
plot(model)

# log transf --------------------------------------------------------------

#ndvi_log <- (log(dpz$ndvi))
#shapiro.test(ndvi_log) # did not help

# data distribution check ---------------------------------------------------------
# 
# hist(dpz$ndvi)
# hist(dpz$ndwi)
# 
# shapiro.test(dpz$ndvi[dpz$var == "3"])
# shapiro.test(dpz$ndvi[dpz$var == "4"])
# shapiro.test(dpz$ndvi[dpz$var == "5"])
# shapiro.test(dpz$ndvi[dpz$var == "6"])
# shapiro.test(dpz$ndvi[dpz$var == "9"])
# shapiro.test(dpz$ndvi[dpz$var == "10"])
# shapiro.test(dpz$ndvi[dpz$var == "13"])
# shapiro.test(dpz$ndvi[dpz$var == "14"]) # no var has NR
# 
# install.packages("goft")
# require("goft")
# gamma_test(dpz$ndvi) # nope


# GLM ---------------------------------------------------------------------

# install.packages("goft")
# require("goft")
# gamma_test(dpz$ndvi) # nope
# 
# glm1 <- glm(ndvi ~ var, data = dpz, family = "Gamma")
# summary(glm1)

# two sample wilcoxon test ------------------------------------------------

# NDVI

wilcox.test(dpz$ndvi[dpz$var == "3"], dpz$ndvi[dpz$var == "4"], 
            alternative = "two.sided")

boxplot(dpz$ndvi[dpz$var == "3"], dpz$ndvi[dpz$var == "4"]) 
# p-value = 0.005504, var 3 higher

wilcox.test(dpz$ndvi[dpz$var == "5"], dpz$ndvi[dpz$var == "6"], 
            alternative = "two.sided")

boxplot(dpz$ndvi[dpz$var == "5"], dpz$ndvi[dpz$var == "6"]) 
# p-value = 0.5117

wilcox.test(dpz$ndvi[dpz$var == "9"], dpz$ndvi[dpz$var == "10"], 
            alternative = "two.sided")

boxplot(dpz$ndvi[dpz$var == "9"], dpz$ndvi[dpz$var == "10"]) 
# p-value = 0.5445

wilcox.test(dpz$ndvi[dpz$var == "13"], dpz$ndvi[dpz$var == "14"], 
            alternative = "two.sided")

boxplot(dpz$ndvi[dpz$var == "13"], dpz$ndvi[dpz$var == "14"]) 
# p-value = 0.1828

# NDWI

wilcox.test(dpz$ndwi[dpz$var == "3"], dpz$ndwi[dpz$var == "4"], 
            alternative = "two.sided")

boxplot(dpz$ndwi[dpz$var == "3"], dpz$ndwi[dpz$var == "4"]) 
# p-value = 0.002243, var 3 higher

wilcox.test(dpz$ndwi[dpz$var == "5"], dpz$ndwi[dpz$var == "6"], 
            alternative = "two.sided")

boxplot(dpz$ndwi[dpz$var == "5"], dpz$ndwi[dpz$var == "6"]) 
# p-value = 0.1625

wilcox.test(dpz$ndwi[dpz$var == "9"], dpz$ndwi[dpz$var == "10"], 
            alternative = "two.sided")

boxplot(dpz$ndwi[dpz$var == "9"], dpz$ndwi[dpz$var == "10"]) 
# p-value = 0.2191

wilcox.test(dpz$ndwi[dpz$var == "13"], dpz$ndwi[dpz$var == "14"], 
            alternative = "two.sided")

boxplot(dpz$ndwi[dpz$var == "13"], dpz$ndwi[dpz$var == "14"]) 
# p-value = 0.3021


# ANOVA ----------------------------------------------------------

# homogenity of variance check

# bartlett.test(ndvi ~ var, data = dpz) # nope
# bartlett.test(ndwi ~ var, data = dpz) # nope
# 
# # kw 
# 
# kruskal.test(ndvi ~ var, data = dpz) # yep
# kruskal.test(ndwi ~ var, data = dpz) # yep
# 
# # install.packages("pgirmess") # post-hoc testy
# require(pgirmess)
# 
# kruskalmc(ndvi ~ var, data = dpz, p=0.05)
# kruskalmc(ndwi ~ var, data = dpz, p=0.05)

## one way ANOVA NDVI

ggplot(dpz, aes(x = var, y = ndvi, group = term, col = term)) +  # https://stat.ethz.ch/~meier/teaching/anova/random-and-mixed-effects-models.html
  geom_point() + 
  stat_summary(fun.y = mean, geom = "line")

with(dpz, interaction.plot(x.factor = var, trace.factor = term, 
                           response = ndvi))

library(lme4)

a_ndvi <- lmer(ndvi ~ var + (1|term) + (1|term:var), data = dpz)

summary(a_ndvi)
anova(a_ndvi)

fit.fixed <- aov(ndvi ~ var * term, data = dpz)
summary(fit.fixed)


TukeyHSD(a_ndvi)
# plot(TukeyHSD(simp20))
# install.packages("multcomp")
require(multcomp)
summary(glht(a_ndvi, linfct=mcp(var="Tukey")))

## one way ANOVA NDWI

ggplot(dpz, aes(x = var, y = ndwi, group = term, col = term)) +  # https://stat.ethz.ch/~meier/teaching/anova/random-and-mixed-effects-models.html
  geom_point() + 
  stat_summary(fun.y = mean, geom = "line")

with(dpz, interaction.plot(x.factor = var, trace.factor = term, 
                           response = ndwi))

library(lme4)

a_ndwi <- lmer(ndwi ~ var + (1|term) + (1|term:var), data = dpz)

summary(a_ndwi)
anova(a_ndwi)

fit.fixed <- aov(ndwi ~ var * term, data = dpz)
summary(fit.fixed)


TukeyHSD(a_ndwi)
# plot(TukeyHSD(simp20))
# install.packages("multcomp")
require(multcomp)
summary(glht(a_ndwi, linfct=mcp(var="Tukey")))

# DPZ boxplot

ggplot(dpz, aes(var, ndvi))+
  geom_boxplot()+
  labs(x="", y="NDVI")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))

ggplot(dpz, aes(var, ndwi))+
  geom_boxplot()+
  labs(x="", y="NDWI")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
