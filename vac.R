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
  labs(y = "MPa", x = "depth [cm]", title = "Penetration Resistance")+
  theme_minimal()
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

rbd$var <- as.factor(rbd$var)

ggplot(rbd, aes(var, roh))+
  geom_bar(stat = "summary", fun.y = "mean", width = 0.6)+
  labs(y = expression("g."~ cm^-3~""), x = "var", title = "Reduced Bulk Density")+
  theme_minimal()
ggsave("plots/rbd.png", device = "png", width = 6, height = 3, dpi = 500)

# hraju si s grafem, aby byl vyraznejsi


ggplot(rbd, aes(var, roh))+
  geom_bar(stat = "summary", fun.y = "mean", width = 0.6)+
  labs(y = expression("g."~ cm^-3~""), x = "var", title = "Reduced Bulk Density")+
  coord_cartesian(ylim = c(1, 1.50))+
  theme_minimal()
ggsave("plots/rbd_scale.png", device = "png", width = 6, height = 3, dpi = 500)

# EMMISIONS ---------------------------------------------------------------

em <- read_excel("red/korbic.xlsx", sheet = 3)

eml <- em %>% 
  select(var, amm, co2, meth) %>% 
  melt(id.vars = c("var"), variable.name = ("gas"), value.name = "emmision")

eml$var <- as.factor(eml$var)

# ammonium  ---------------------------------------------------------------

amm <- eml %>% 
  filter(gas == "amm")

ggplot(amm, aes(var, emmision))+
  geom_boxplot()+
  labs(y = expression("kg."~m^2~"/ y"), x = "var", fill = "", title = "Ammonia")+
  theme_minimal()
ggsave("plots/ammonia.png", device = "png", width = 6, height = 3, dpi = 500)

shapiro.test(amm$emmision)
hist(amm$emmision)

aov1 <- aov(emmision ~ var, data = amm)
summary(aov1)

# co2

co2 <- eml %>% 
  filter(gas == "co2")

ggplot(co2, aes(var, emmision))+
  geom_boxplot()+
  labs(y = expression("kg."~m^2~"/ y"), x = "var", fill = "", 
       title = expression("CO"[2]))+
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
  labs(y = expression("kg."~m^2~"/ y"), x = "var", fill = "", 
       title = "Methane")+
  theme_minimal()
ggsave("plots/methane.png", device = "png", width = 6, height = 3, dpi = 500)

shapiro.test(meth$emmision)
hist(meth$emmision)

aov3 <- aov(emmision ~ var, data = meth)
summary(aov3)

TukeyHSD(aov3)
