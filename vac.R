# base --------------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")
require(tidyverse)
require(readxl)
require(reshape2)

# file
# sheet 1 var [5051, 5052, 5053]
# sheet 2 reduced bulk density
# sheet 3 penetration resistance
# sheet 4 unit draft

# PENRES ------------------------------------------------------------------

pen <- read_excel("red/litva.xlsx", sheet = 3)

# pen$var[pen$var == 7] <- "ZF"
# pen$var[pen$var == 8] <- "ZF_SOL"
# pen$var[pen$var == 9] <- "C"
# pen$var[pen$var == 10] <- "SOL"

penl <- pen %>% 
  select(var, cm4, cm8, cm12, cm16, cm20) %>% 
  melt(id.vars = c("var"), variable.name = ("depth"), value.name = "penres")

dept <- c("20", "16", "12", "8", "4")

penl$depth <- factor(penl$depth, levels = c("cm20", "cm16", "cm12", "cm8", "cm4"), labels = dept)

ggplot(penl, aes(depth, penres))+
  geom_bar(aes(width = 0.5), stat = "summary", fun.y = "mean")+
  coord_flip()+
  facet_grid(. ~ var)+
  labs(y = "MPa", x = "depth [cm]", title = "Penetration Resistance")+
  theme_minimal()
ggsave("plots/penres", device = "png", width = 8, height = 4, dpi = 500)

hist(pen$cm4)
hist(pen$cm8)
hist(pen$cm12)
hist(pen$cm16)
hist(pen$cm20)

shapiro.test(pen$cm4)
shapiro.test(pen$cm8)
shapiro.test(pen$cm12)
shapiro.test(pen$cm16) #*
shapiro.test(pen$cm20) #*

pairwise.wilcox.test(pen$cm4, pen$var,
                     p.adjust.method = "BH")

# significant difference is where p-value < 0.05

pairwise.wilcox.test(pen$cm8, pen$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(pen$cm12, pen$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(pen$cm16, pen$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(pen$cm20, pen$var,
                     p.adjust.method = "BH")

# UNITD -------------------------------------------------------------------

uni <- read_excel("red/litva.xlsx", sheet = 4)

# uni$var[uni$var == 7] <- "ZF"
# uni$var[uni$var == 8] <- "ZF_SOL"
# uni$var[uni$var == 9] <- "C"
# uni$var[uni$var == 10] <- "SOL"

uni$seas <- as.factor(uni$seas)

unil <- uni %>% 
  select(seas, var, ud)

unil$seas <- factor(unil$seas)
unil$var <- factor(unil$var)

ggplot(unil, aes(var, ud, fill = seas))+
  geom_bar(aes(width = 0.5),stat = "summary", fun.y = "mean",
           position = position_dodge())+
  scale_fill_manual(values=c("darkgrey", "grey30"))+
  labs(y = "%", x = "", fill = "", title = "Unit Draft ")+
  theme_minimal()
ggsave("plots/unitd.png", device = "png", width = 6, height = 3, dpi = 500)

# tree plot

unil <- unil %>% 
  mutate(dev = 100-ud)

unil$var <- factor(unil$var, levels = c("5053", "5052", "5051"))
unil$seas <- factor(unil$seas, levels = c("2018", "2017"))

ggplot(unil, aes(var, dev, fill = seas))+
  geom_bar(aes(width = 0.5),stat = "summary", fun.y = "mean",
           position = position_dodge())+
  scale_fill_manual(values=c("darkgrey", "grey30"), breaks = rev(levels(unil$seas)))+
  # scale_fill_manual(values=c("grey30", "darkgrey"), breaks = rev(levels(penl$seas)))+
  labs(y = "%", x = "", fill = "", title = "Unit Draft")+
  coord_flip()+
  theme_minimal()
ggsave("plots/unitd_dev.png", device = "png", width = 6, height = 3, dpi = 500)

u17 <- uni %>%
  filter(seas == 2017)

u18 <- uni %>%
  filter(seas == 2018)

pairwise.wilcox.test(u17$ud, u17$var,
                     p.adjust.method = "BH")
# significant difference is where p-value < 0.05
# diff everywhere except Var 4 & control

pairwise.wilcox.test(u18$ud, u18$var,
                     p.adjust.method = "BH")


# RBD ---------------------------------------------------------------------

rbd <- read_excel("red/litva.xlsx", sheet = 2)

rbd <- rbd %>% 
  select(var, roh, seas)

rbd$seas <- as.factor(rbd$seas)
rbd$var <- as.factor(rbd$var)

ggplot(rbd, aes(var, roh, fill = seas))+
  geom_bar(stat="summary", fun.y = "mean", position=position_dodge(), width = 0.5)+
  scale_fill_manual(values=c("darkgrey","grey30"))+
  labs(y = expression(" g."~ cm^-3~""), x = "", fill = "", title = "Reduced Bulk Density")+
  theme(legend.title=element_blank())+
  theme_minimal()
ggsave("plots/rbd.png", device = "png", width = 6, height = 3, dpi = 500)

# scale

ggplot(rbd, aes(var, roh, fill = seas))+
  geom_bar(stat="summary", fun.y = "mean", position=position_dodge(), width = 0.5)+
  scale_fill_manual(values=c("darkgrey","grey30"))+
  labs(y = expression(" g."~ cm^-3~""), x = "", fill = "", title = "Reduced Bulk Density")+
  coord_cartesian(ylim = c(1, 1.40))+
  theme(legend.title=element_blank())+
  theme_minimal()
ggsave("plots/rbd_scale.png", device = "png", width = 6, height = 3, dpi = 500)
