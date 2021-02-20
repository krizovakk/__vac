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
# palet4 <- c("grey92", "darkgrey", "grey40", "grey10")

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
ggsave("plots/meteo.png", device = "png", width = 8, height = 5, dpi = 300)

