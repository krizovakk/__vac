# base --------------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")
require(tidyverse)
require(readxl)
require(reshape2)


# PENRES ------------------------------------------------------------------

pen <- read_excel("red/korbic.xlsx", sheet = 1)

# pen$var[pen$var == 7] <- "ZF"
# pen$var[pen$var == 8] <- "ZF_SOL"
# pen$var[pen$var == 9] <- "C"
# pen$var[pen$var == 10] <- "SOL"

pen$var <- factor(pen$var, levels = c(0, 1, 2, 3, 4), 
                  labels = c("0", "10", "20", "30", "40"))

penl <- pen %>% 
  select(var, d4, d8, d12, d16, d20) %>% 
  melt(id.vars = c("var"), variable.name = ("depth"), value.name = "penres")

dept <- c("20", "16", "12", "8", "4")
penl$depth <- factor(penl$depth, levels = c("d20", "d16", "d12", "d8", "d4"), 
                                            labels = dept)

ggplot(penl, aes(depth, penres))+
  geom_bar(aes(width = 0.5),stat = "summary", fun.y = "mean")+
  coord_flip()+
  facet_grid(. ~ var)+
  labs(y = "Penetration Resistance [MPa]", x = "Depth [cm]")+
  ggtitle(expression("Digestate Dose [ t "~ha^-1~"]"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 11))
ggsave("plots/penres.png", device = "png", width = 6, height = 3, dpi = 500)

hist(pen$d4)
hist(pen$d8)
hist(pen$d12)
hist(pen$d16)
hist(pen$d20)

shapiro.test(pen$d4)
shapiro.test(pen$d8)
shapiro.test(pen$d12)
shapiro.test(pen$d16)
shapiro.test(pen$d20) # NR was not found by any depth

pairwise.wilcox.test(pen$d4, pen$var,
                     p.adjust.method = "BH")

# significant difference is where p-value < 0.05

pairwise.wilcox.test(pen$d8, pen$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(pen$d12, pen$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(pen$d16, pen$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(pen$d20, pen$var,
                     p.adjust.method = "BH")



# RBD ---------------------------------------------------------------------

rbd <- read_excel("red/korbic.xlsx", sheet = 2)

rbd$var <- factor(rbd$var, levels = c(0, 1, 2, 3, 4), 
                     labels = c("0", "10", "20", "30", "40"))

ggplot(rbd, aes(var, roh))+
  geom_bar(stat = "summary", fun.y = "mean", width = 0.6)+
  labs(y = expression("Reduced Bulk Density [g "~ cm^-3~"]"), 
       x = expression("Digestate Dose [ t "~ha^-1~"]"), title = "")+
  theme_minimal()
ggsave("plots/rbd.png", device = "png", width = 6, height = 3, dpi = 500)

# hraju si s grafem, aby byl vyraznejsi


ggplot(rbd, aes(var, roh))+
  geom_bar(stat = "summary", fun.y = "mean", width = 0.6)+
  labs(y = expression("Reduced Bulk Density [g "~ cm^-3~"]"), 
       x = expression("Digestate Dose [ t "~ha^-1~"]"), title = "")+
  coord_cartesian(ylim = c(1, 1.50))+
  theme_minimal()
ggsave("plots/rbd_scale.png", device = "png", width = 6, height = 3, dpi = 500)

# EMMISIONS ---------------------------------------------------------------

em <- read_excel("red/korbic.xlsx", sheet = 3)

eml <- em %>% 
  select(var, dose, amm, co2, meth) %>% 
  melt(id.vars = c("var", "dose"), variable.name = ("gas"), value.name = "emmision")

eml$var <- as.factor(eml$var)
eml$dose <- as.factor(eml$dose)

# ammonium 

amm <- eml %>% 
  filter(gas == "amm")

ggplot(amm, aes(dose, emmision))+
  geom_boxplot()+
  labs(y = expression(paste("N",H[3]," [kg ",m^-2 , y^-1,"]")), 
       x = expression("Digestate Dose [ t "~ha^-1~"]"))+
  theme_minimal()
ggsave("plots/ammonia.png", device = "png", width = 6, height = 3, dpi = 500)

shapiro.test(amm$emmision)
hist(amm$emmision)

aov1 <- aov(emmision ~ var, data = amm)
summary(aov1)

# co2

co2 <- eml %>% 
  filter(gas == "co2")

ggplot(co2, aes(dose, emmision))+
  geom_boxplot()+
  labs(y = expression(paste("C",O[2]," [kg ",m^-2 , y^-1,"]")), 
       x = expression("Digestate Dose [ t "~ha^-1~"]"))+
  theme_minimal()
ggsave("plots/co2.png", device = "png", width = 6, height = 3, dpi = 500)

shapiro.test(co2$emmision)
hist(co2$emmision)

aov2 <- aov(emmision ~ var, data = co2)
summary(aov2)

TukeyHSD(aov2)

# meth

meth <- eml %>% 
  filter(gas == "meth")

ggplot(meth, aes(var, emmision))+
  geom_boxplot()+
  labs(y = expression(paste("C",H[4]," [kg ",m^-2 , y^-1,"]")), 
       x = expression("Digestate Dose [ t "~ha^-1~"]"))+
  theme_minimal()
ggsave("plots/methane.png", device = "png", width = 6, height = 3, dpi = 500)

shapiro.test(meth$emmision)
hist(meth$emmision)

aov3 <- aov(emmision ~ var, data = meth)
summary(aov3)

TukeyHSD(aov3)

# NDVI --------------------------------------------------------------------

ndvi <- read_excel("red/ndvi.xlsx", sheet = 1)

ndvi$var <- factor(ndvi$var, levels = c("D0", "D10", "D20", "D30", "D40"), 
                  labels = c("0", "10", "20", "30", "40"))
ndvi$date <- as.factor(ndvi$date)

ggplot(ndvi, aes(date, ndvi, color = var))+
  geom_boxplot()+
  labs(y = "NDVI", 
       x = "Date")+
  theme_minimal()
ggsave("plots/ndvi_date.png", device = "png", width = 6, height = 3, dpi = 500)

ggplot(ndvi, aes(var, ndvi, color = date))+
  geom_boxplot()+
  labs(y = "NDVI", 
       x = expression("Digestate Dose [ t "~ha^-1~"]"))+
  theme_minimal()
ggsave("plots/ndvi_dose.png", device = "png", width = 6, height = 3, dpi = 500)

# GNDVI -------------------------------------------------------------------

gndvi <- read_excel("red/gndvi.xlsx", sheet = 1)

gndvi$var <- factor(gndvi$var, levels = c("D0", "D10", "D20", "D30", "D40"), 
                   labels = c("0", "10", "20", "30", "40"))
gndvi$date <- as.factor(gndvi$date)

ggplot(gndvi, aes(date, gndvi, color = var))+
  geom_boxplot()+
  labs(y = "GNDVI", 
       x = "Date")+
  theme_minimal()
ggsave("plots/gndvi_date.png", device = "png", width = 6, height = 3, dpi = 500)

ggplot(gndvi, aes(var, gndvi, color = date))+
  geom_boxplot()+
  labs(y = "GNDVI", 
       x = expression("Digestate Dose [ t "~ha^-1~"]"))+
  theme_minimal()
ggsave("plots/gndvi_dose.png", device = "png", width = 6, height = 3, dpi = 500)

# SAVI --------------------------------------------------------------------

savi <- read_excel("red/savi.xlsx", sheet = 1)

savi$var <- factor(savi$var, levels = c("D0", "D10", "D20", "D30", "D40"), 
                    labels = c("0", "10", "20", "30", "40"))
savi$date <- as.factor(savi$date)

ggplot(savi, aes(date, savi, color = var))+
  geom_boxplot()+
  labs(y = "SAVI", 
       x = "Date")+
  theme_minimal()
ggsave("plots/savi_date.png", device = "png", width = 6, height = 3, dpi = 500)

ggplot(savi, aes(var, savi, color = date))+
  geom_boxplot()+
  labs(y = "SAVI", 
       x = expression("Digestate Dose [ t "~ha^-1~"]"))+
  theme_minimal()
ggsave("plots/savi_dose.png", device = "png", width = 6, height = 3, dpi = 500)


# RS data -----------------------------------------------------------------

# ANOVA repeated measures

summary(aov(ndvi ~ var + Error(date), data=ndvi))
        
install.packages("multcomp", dependencies = TRUE)




