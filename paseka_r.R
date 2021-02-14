# base --------------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")
# install.packages("psych")

require(tidyverse)
require(readxl)
require(reshape2)
require(psych)

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

# PENRES ------------------------------------------------------------------

pen <- read_excel("red/penres.xlsx")

pen$var[pen$var == 7] <- "ZF"
pen$var[pen$var == 8] <- "ZF_SOL"
pen$var[pen$var == 9] <- "C"
pen$var[pen$var == 10] <- "SOL"

penl <- pen %>%
  select(seas, var, cm4, cm8, cm12, cm16, cm20) %>%
  melt(id.vars = c("seas", "var"), variable.name = ("depth"), value.name = "penres")

# penres errorbar comp ----------------------------------------------------

df3 <- data_summary(penl, varname="penres", 
                    groupnames=c("seas", "var", "depth"))

# Convert dose to a factor variable
df3$var=as.factor(df3$var)
df3$seas <- factor(df3$seas)
head(df3)

# descriptive statistics

# install.packages("writexl")
require(writexl)
write_xlsx(df3,"penstat15.xlsx")

# penres explorative ------------------------------------------------------

dept <- c("20", "16", "12", "8", "4")

df3$depth <- factor(df3$depth, levels = c("cm20", "cm16", "cm12", "cm8", "cm4"), 
                    labels = dept)
df3$seas <- factor(df3$seas, levels = c("2017", "2016", "2015"))

# all three years

ggplot(df3, aes(depth, penres, fill=seas))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=penres-sd, ymax=penres+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_manual(values= palet3, breaks = rev(levels(df3$seas)))+
  coord_flip()+
  facet_grid(. ~ var)+
  labs(y = "\nPenetration Resistance [MPa]", x = "Depth [cm]", fill = "", title = "")+
  theme_minimal(base_size = 15)
# ggsave("plots/penres_se.png", device = "png", width = 8, height = 4, dpi = 500)


# separate plots for 2015 nad 2017

# p15l$depth <- factor(p15l$depth, levels = c("cm20", "cm16", "cm12", "cm8", "cm4"), labels = dept)
# p17l$depth <- factor(p17l$depth, levels = c("cm20", "cm16", "cm12", "cm8", "cm4"), labels = dept)
# 
# ggplot(p15l, aes(depth, penres))+
#   geom_bar(aes(width = 0.5),stat = "summary", fun.y = "mean")+
#   coord_flip()+
#   facet_grid(. ~ var)+
#   labs(y = "Penetration Resistance [MPa]", x = "Depth [cm]", fill = "", title = "2015")+
#   theme_minimal()
# ggsave("penres15.png", device = "png", width = 6, height = 3, dpi = 500)
# 
# ggplot(p17l, aes(depth, penres))+
#   geom_bar(aes(width = 0.5),stat = "summary", fun.y = "mean")+
#   coord_flip()+
#   facet_grid(. ~ var)+
#   labs(y = "Penetration Resistance [MPa]", x = "Depth [cm]", fill = "", title = "2017")+
#   theme_minimal()
# ggsave("penres17.png", device = "png", width = 6, height = 3, dpi = 500)


# penres analysis ---------------------------------------------------------

p15 <- pen %>%
  filter(seas == 2015) %>% 
  select(var, cm4, cm8, cm12, cm16, cm20)

p16 <- pen %>%
  filter(seas == 2016) %>% 
  select(var, cm4, cm8, cm12, cm16, cm20)

p17 <- pen %>%
  filter(seas == 2017) %>% 
  select(var, cm4, cm8, cm12, cm16, cm20)

#2015
pairwise.wilcox.test(p15$cm4, p15$var,
                     p.adjust.method = "BH")

# significant difference is where p-value < 0.05
# diff everywhere except Var 4 & control

pairwise.wilcox.test(p15$cm8, p15$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p15$cm12, p15$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p15$cm16, p15$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p15$cm20, p15$var,
                     p.adjust.method = "BH")

#2016

  pairwise.wilcox.test(p16$cm4, p16$var,
                       p.adjust.method = "BH")
  
  pairwise.wilcox.test(p16$cm8, p16$var,
                       p.adjust.method = "BH")
  
  pairwise.wilcox.test(p16$cm12, p16$var,
                       p.adjust.method = "BH")
  
  pairwise.wilcox.test(p16$cm16, p16$var,
                       p.adjust.method = "BH")
  
  pairwise.wilcox.test(p16$cm20, p16$var,
                       p.adjust.method = "BH")
  
#2017

  pairwise.wilcox.test(p17$cm4, p17$var,
                       p.adjust.method = "BH")
  
  pairwise.wilcox.test(p17$cm8, p17$var,
                       p.adjust.method = "BH")
  
  pairwise.wilcox.test(p17$cm12, p17$var,
                       p.adjust.method = "BH")
  
  pairwise.wilcox.test(p17$cm16, p17$var,
                       p.adjust.method = "BH")
  
  pairwise.wilcox.test(p17$cm20, p17$var,
                       p.adjust.method = "BH")

# UNITD -------------------------------------------------------------------

uni <- read_excel("red/unitd.xlsx")
  
uni$var[uni$var == 7] <- "ZF"
uni$var[uni$var == 8] <- "ZF_SOL"
uni$var[uni$var == 9] <- "C"
uni$var[uni$var == 10] <- "SOL"

unil <- uni %>% 
  select(seas, var, unitd)

u15 <- uni %>% 
  filter(seas == 2015)

u17 <- uni %>% 
  filter(seas == 2017)

u15l <- u15 %>% 
  select(var, unitd)

u17l <- u17 %>% 
  select(var, unitd)

# unitd explorative -------------------------------------------------------

# both years

unil$unitd <- unil$unitd*100
unil$seas <- factor(unil$seas)


ggplot(unil, aes(var, unitd, fill = seas))+
  geom_bar(aes(width = 0.5),stat = "summary", fun.y = "mean",
           position = position_dodge())+
  scale_fill_manual(values=c("darkgrey", "grey30"))+
  labs(y = "Unit Draft [%]", x = "", fill = "", title = "")+
  theme_minimal()
ggsave("unitd_both_percentage.png", device = "png", width = 6, height = 3, dpi = 500)

# single year

unil$id <- paste(unil$seas, unil$var, sep="_") # vytvoreni sloupce ID ze sloupců 
unil$id <- factor(unil$id)

ggplot(unil, aes(x = var, y = unitd, fill = id)) + 
  geom_col(aes(width = 0.5))+
  labs(y = "Unit Draft [%]", x = "", fill = "", title = "2015")+
  ylim(0, 105)+
  theme_minimal() 
ggsave("unitd15_percentage.png", device = "png", width = 6, height = 3, dpi = 500)

# p15l$depth <- factor(p15l$depth, levels = c("cm20", "cm16", "cm12", "cm8", "cm4"), labels = dept)
# p17l$depth <- factor(p17l$depth, levels = c("cm20", "cm16", "cm12", "cm8", "cm4"), labels = dept)

u15l$unitd <- u15l$unitd * 100
u17l$unitd <- u17l$unitd * 100

# u15 issue - control mean is not 100 % - why?

u15$unitd <- u15$unitd * 100

u15 %>% 
  group_by(var) %>% 
  summarise(mean = mean(unitd)) %>% 
  ggplot(aes(x = var, y = mean)) + 
  geom_col(aes(width = 0.5))+
  labs(y = "Unit Draft [%]", x = "", fill = "", title = "2015")+
  ylim(0, 105)+
  theme_minimal() 

ggsave("unitd15_percentage.png", device = "png", width = 6, height = 3, dpi = 500)

u17$unitd <- u17$unitd * 100

u17 %>% 
  group_by(var) %>% 
  summarise(mean = mean(unitd)) %>% 
  ggplot(aes(x = var, y = mean)) + 
  geom_col(aes(width = 0.5))+
  labs(y = "Unit Draft [%]", x = "", fill = "", title = "2017")+
  ylim(0, 120)+
  theme_minimal() 

ggsave("unitd17_percentage.png", device = "png", width = 6, height = 3, dpi = 500)

# ggplot(u15l, aes(var, unitd))+
#   geom_bar(aes(width = 0.5), stat = "summary", fun.y = "mean")+
#   labs(y = "Unit Draft [%]", x = "", fill = "", title = "2015")+
#   ylim(0.00, 130)+
#   theme_minimal()
# ggsave("unitd15.png", device = "png", width = 6, height = 3, dpi = 500)

# ggplot(u17l, aes(var, unitd))+
#   geom_bar(aes(width = 0.5),stat = "summary", fun.y = "mean")+
#   labs(y = "Unit Draft [%]", x = "", fill = "", title = "2017")+
#   ylim(0.00, 110)+
#   theme_minimal()
# ggsave("unitd17.png", device = "png", width = 6, height = 3, dpi = 500)

# unitd analysis ----------------------------------------------------------

pairwise.wilcox.test(u15$unitd, u15$var,
                     p.adjust.method = "BH")
# significant difference is where p-value < 0.05
# diff everywhere except Var 4 & control

pairwise.wilcox.test(u17$unitd, u17$var,
                     p.adjust.method = "BH")

# INFILTRATION (KFS) ------------------------------------------------------------

inf <- read_excel("red/inf.xlsx")

inf$var[inf$var == 7] <- "ZF"
inf$var[inf$var == 8] <- "ZF_SOL"
inf$var[inf$var == 9] <- "C"
inf$var[inf$var == 10] <- "SOL"

infl <- inf %>%
  select(seas, var, inf)

# inf errorbar comp -------------------------------------------------------

df2 <- data_summary(infl, varname="inf", 
                    groupnames=c("seas", "var"))
# Convert dose to a factor variable
df2$var=as.factor(df2$var)
df2$seas <- factor(df2$seas)
head(df2)

write_xlsx(df2,"infstat15.xlsx")

# inf explorative ---------------------------------------------------------

# all three years

ggplot(df2, aes(x=var, y=inf, fill=seas)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=inf-sd, ymax=inf+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_manual(values = palet3)+
  labs(y = expression("Saturated Hydraulic Conductivity [ mm"~ h^-1~"]"), 
       x = "", fill = "", title = "")+
  theme_minimal(base_size = 15)
# ggsave("plots/inf_se.png", device = "png", width = 8, height = 5, dpi = 500)

#separate

# ggplot(i15l, aes(var, inf))+
#   geom_bar(aes(width = 0.5),stat = "summary", fun.y = "mean")+
#   labs(y = expression("Saturated Hydraulic Conductivity [ mm"~ h^-1~"]"), 
#        x = "", fill = "", title = "2015")+
#   theme_minimal()
# # ggsave("inf15.png", device = "png", width = 6, height = 4, dpi = 500)
# 
# ggplot(i17l, aes(var, inf))+
#   geom_bar(aes(width = 0.5),stat = "summary", fun.y = "mean")+
#   labs(y = expression("Saturated Hydraulic Conductivity [ mm"~ h^-1~"]"),
#        x = "", fill = "", title = "2017")+
#   theme_minimal()
# ggsave("inf17.png", device = "png", width = 6, height = 4, dpi = 500)

# inf analysis ------------------------------------------------------------

i15 <- inf %>%
  filter(seas == 2015) %>% 
  select(var, inf)

i16 <- inf %>%
  filter(seas == 2016) %>% 
  select(var, inf)

i17 <- inf %>%
  filter(seas == 2017) %>% 
  select(var, inf)

pairwise.wilcox.test(i15$inf, i15$var,
                     p.adjust.method = "BH")
# significant difference is where p-value < 0.05
# diff everywhere except Var 4 & control

pairwise.wilcox.test(i16$inf, i16$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(i17$inf, i17$var,
                     p.adjust.method = "BH")

# RBD ---------------------------------------------------------------------

rbd <- read_excel("red/rbd.xlsx")

rbd$var[rbd$var == 7] <- "ZF"
rbd$var[rbd$var == 8] <- "ZF_SOL"
rbd$var[rbd$var == 9] <- "C"
rbd$var[rbd$var == 10] <- "SOL"

rbdl <- rbd %>%
  select(var, seas, rbd)

# rbd errorbar comp -------------------------------------------------------

df4 <- data_summary(rbdl, varname="rbd", 
                    groupnames=c("seas", "var"))
# Convert dose to a factor variable
df4$var=as.factor(df4$var)
df4$seas <- factor(df4$seas)
head(df4)

write_xlsx(df4,"rbdstat15.xlsx")


# rbd explorative ---------------------------------------------------------

ggplot(df4, aes(var, rbd, fill = seas))+
  geom_bar(stat="identity", color = "black",
           position=position_dodge())+
  geom_errorbar(aes(ymin=rbd-sd, ymax=rbd+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_manual(values = palet3)+
  labs(y = expression("Reduced Bulk Density [ g"~ cm^-3~"]"), x = "", fill = "")+
  coord_cartesian(ylim = c(1.0, 1.35))+ # omezeni y osy, aby byly lepe videt rozdily
  theme(legend.title=element_blank())+
  theme_minimal(base_size = 15)
# ggsave("plots/rbd_se.png", device = "png", width = 8, height = 4, dpi = 500)

# METEO -------------------------------------------------------------------

met <- read_excel("red/meteo.xlsx")
colnames(met) [1] <- "month"

met$month <- factor(met$month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))

met <- met %>% 
  melt(id.vars = c("month"), variable.name = ("year"), value.name = "rain")

ggplot(met, aes(month, rain, fill=year))+
  geom_bar(stat="identity", color = "black", 
           position=position_dodge())+
  scale_fill_manual(values = palet4, 
                    name = "", 
                    labels = c("2015", "2016", "2017", 
                               "longterm normal (1981-2010)"))+
  labs(y = "Sum of Precipitation [mm]", x = "", fill = "")+
  theme_minimal(base_size = 15)+
  theme(legend.position="top")
ggsave("plots/meteo.png", device = "png", width = 8, height = 5, dpi = 500)

# CZ labels for VAC / 17/11/20 --------------------------------------------

## penres

dept <- c("20", "16", "12", "8", "4")
penl$depth <- factor(penl$depth, 
                     levels = c("cm20", "cm16", "cm12", "cm8", "cm4"), 
                     labels = dept)

penl$seas <- factor(penl$seas, levels = c("2017", "2015"))

ggplot(penl, aes(depth, penres, fill=seas))+
  geom_bar(aes(width = 0.5), stat = "summary", fun.y = "mean", 
           position = position_dodge())+
  scale_fill_manual(values=c("grey30", "darkgrey"), breaks = rev(levels(penl$seas)))+
  coord_flip()+
  facet_grid(. ~ var)+
  labs(y = "\npenetrační odpor [MPa]", x = "hloubka půdního profilu [cm]", fill = "", title = "")+
  theme_minimal(base_size = 15)
ggsave("penres_both.png", path = "cz_plot", device = "png", 
       width = 8, height = 4, dpi = 500)

## unitdraft

unil$unitd <- unil$unitd*100
unil$seas <- factor(unil$seas)


ggplot(unil, aes(var, unitd, fill = seas))+
  geom_bar(aes(width = 0.5),stat = "summary", fun.y = "mean",
           position = position_dodge())+
  scale_fill_manual(values=c("darkgrey", "grey30"))+
  labs(y = "tahový odpor [%]", x = "", fill = "", title = "")+
  theme_minimal(base_size = 15)
ggsave("unitd_both_percentage.png", path = "cz_plot", 
       device = "png", width = 6, height = 3, dpi = 500)

## infiltration

ggplot(infl, aes(var, inf, fill = seas))+
  geom_bar(aes(width = 0.5),stat = "summary", fun.y = "mean",
           position = position_dodge())+
  scale_fill_manual(values=c("darkgrey", "grey30"))+
  labs(y = expression("nasycená hydr. vodivost [ mm"~ h^-1~"]"), 
       x = "", fill = "", title = "")+
  theme_minimal(base_size = 15)
ggsave("inf_both.png", path = "cz_plot", 
       device = "png", width = 8, height = 4, dpi = 500)
 
## reduced bulk density

rbd$year <- as.factor(rbd$year)

ggplot(rbd, aes(var, rbd, fill = year))+
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=c("darkgrey","grey30"))+
  labs(y = expression("red. obj. hmotnost [ g"~ cm^-3~"]"), x = "", fill = "")+
  theme(legend.title=element_blank())+
  theme_minimal(base_size = 15)
ggsave("rbd_bothyears.png", path = "cz_plot", 
       device = "png", width = 6, height = 4, dpi = 500)

## meteo

met <- read_excel("red/meteo.xlsx")
colnames(met) [1] <- "month"

met$month <- factor(met$month, 
                    levels = c("jan", "feb", "mar", "apr", "may", "jun", 
                               "jul", "aug", "sep", "oct", "nov", "dec"),
                    labels= c("01", "02", "03", "04", "05", "06",
                              "07", "08", "09", "10", "11", "12"))

met <- met %>% 
  melt(id.vars = c("month"), variable.name = ("year"), value.name = "rain")

ggplot(met, aes(month, rain, fill=year))+
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=c("grey90", "grey70", "grey50", "grey1"), 
                    name = "", 
                    labels = c("2015", "2016", "2017", 
                               "normál (1981-2010)"))+
  labs(y = "srážkový úhrn [mm]", x = "", fill = "")+
  theme_minimal(base_size = 15)+
  theme(legend.position="top")
ggsave("meteo.png", path = "cz_plot", 
       device = "png", width = 6, height = 4, dpi = 500)
