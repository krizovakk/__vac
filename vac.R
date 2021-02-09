# base --------------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")
# install.packages("RColorBrewer")
require(tidyverse)
require(readxl)
require(reshape2)
require(RColorBrewer)

# nasepaleta <- c("darkgreen", "limegreen", "darkturquoise", "dodgerblue4",
#                  "gray60","burlywood4", "gray11")
# nasepaleta2 <- c("limegreen", "darkturquoise", "dodgerblue4",
#                  "gray60","burlywood4", "gray11") # pro HC / 2014 chybi mereni
tripaleta <- c("limegreen", "dodgerblue4", "gray11") # pro 3 roky vyhodnoceni

# load KRAVA -----------------------------------------------------------

kra_pr <- read_excel("red/krava.xlsx", sheet = 2)
kra_roh <- read_excel("red/krava.xlsx", sheet = 3)
kra_ud <- read_excel("red/krava.xlsx", sheet = 4)
kra_hc <- read_excel("red/krava.xlsx", sheet = 5)

# variants

kra_hc$var[kra_hc$var == 1] <- "C_fix"
kra_hc$var[kra_hc$var == 2] <- "C_fix_sol"
kra_hc$var[kra_hc$var == 3] <- "C_fym"
kra_hc$var[kra_hc$var == 4] <- "C_sol"

kra_pr$var[kra_pr$var == 1] <- "C_fix"
kra_pr$var[kra_pr$var == 2] <- "C_fix_sol"
kra_pr$var[kra_pr$var == 3] <- "C_fym"
kra_pr$var[kra_pr$var == 4] <- "C_sol"

kra_roh$var[kra_roh$var == 1] <- "C_fix"
kra_roh$var[kra_roh$var == 2] <- "C_fix_sol"
kra_roh$var[kra_roh$var == 3] <- "C_fym"
kra_roh$var[kra_roh$var == 4] <- "C_sol"

kra_ud$var[kra_ud$var == 1] <- "C_fix"
kra_ud$var[kra_ud$var == 2] <- "C_fix_sol"
kra_ud$var[kra_ud$var == 3] <- "C_fym"
kra_ud$var[kra_ud$var == 4] <- "C_sol"

# load PRASE-------------------------------------------------------------------

pra_pr <- read_excel("red/prase.xlsx", sheet = 2)
pra_roh <- read_excel("red/prase.xlsx", sheet = 3)
pra_ud <- read_excel("red/prase.xlsx", sheet = 4)
pra_hc <- read_excel("red/prase.xlsx", sheet = 5)

# variants

pra_hc$var[pra_hc$var == 7] <- "P_fix"
pra_hc$var[pra_hc$var == 8] <- "P_fix_sol"
pra_hc$var[pra_hc$var == 9] <- "P_fym"
pra_hc$var[pra_hc$var == 10] <- "P_sol"

pra_pr$var[pra_pr$var == 7] <- "P_fix"
pra_pr$var[pra_pr$var == 8] <- "P_fix_sol"
pra_pr$var[pra_pr$var == 9] <- "P_fym"
pra_pr$var[pra_pr$var == 10] <- "P_sol"

pra_roh$var[pra_roh$var == 7] <- "P_fix"
pra_roh$var[pra_roh$var == 8] <- "P_fix_sol"
pra_roh$var[pra_roh$var == 9] <- "P_fym"
pra_roh$var[pra_roh$var == 10] <- "P_sol"

pra_ud$var[pra_ud$var == 7] <- "P_fix"
pra_ud$var[pra_ud$var == 8] <- "P_fix_sol"
pra_ud$var[pra_ud$var == 9] <- "P_fym"
pra_ud$var[pra_ud$var == 10] <- "P_sol"

# load NPK  -------------------------------------------
npk_pr <- read_excel("red/npk.xlsx", sheet = 2)
npk_roh <- read_excel("red/npk.xlsx", sheet = 3)
npk_ud <- read_excel("red/npk.xlsx", sheet = 4)
npk_hc <- read_excel("red/npk.xlsx", sheet = 5)

# variants

npk_hc$var[npk_hc$var == 3] <- "C_fym"
npk_hc$var[npk_hc$var == 5] <- "sol"
npk_hc$var[npk_hc$var == 6] <- "npk"
npk_hc$var[npk_hc$var == 9] <- "P_fym"

npk_pr$var[npk_pr$var == 3] <- "C_fym"
npk_pr$var[npk_pr$var == 5] <- "sol"
npk_pr$var[npk_pr$var == 6] <- "npk"
npk_pr$var[npk_pr$var == 9] <- "P_fym"

npk_roh$var[npk_roh$var == 3] <- "C_fym"
npk_roh$var[npk_roh$var == 5] <- "sol"
npk_roh$var[npk_roh$var == 6] <- "npk"
npk_roh$var[npk_roh$var == 9] <- "P_fym"

npk_ud$var[npk_ud$var == 3] <- "C_fym"
npk_ud$var[npk_ud$var == 5] <- "sol"
npk_ud$var[npk_ud$var == 6] <- "npk"
npk_ud$var[npk_ud$var == 9] <- "P_fym"

# select KRAVA ------------------------------------------------------------------
# pr

kra_pr <- kra_pr %>%
  select(var, year, d04, d08, d12, d16, d20) %>% 
  filter(year %in% c("2015", "2018", "2020"))

kra_pr$var <- factor(kra_pr$var)
kra_pr$year <- factor(kra_pr$year, levels = c("2015", "2018", "2020"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))
#roh

kra_roh <- kra_roh %>%
  select(var, year, roh)%>% 
  filter(year %in% c("2015", "2018", "2020"))

kra_roh$var <- factor(kra_roh$var)
kra_roh$year <- factor(kra_roh$year, levels = c("2015", "2018", "2020"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))

#ud

kra_ud <- kra_ud %>%
  select(var, year, unitd)%>% 
  filter(year %in% c("2015", "2018", "2020"))

kra_ud$var <- factor(kra_ud$var)
kra_ud$year <- factor(kra_ud$year, levels = c("2015", "2018", "2020"),
                       labels = c("cycle 1", "cycle 2", "cycle 3"))

# hc

kra_hc <- kra_hc %>%
  select(var, year, hcon)%>% 
  filter(year %in% c("2015", "2018", "2020"))

kra_hc$var <- factor(kra_hc$var)
kra_hc$year <- factor(kra_hc$year, levels = c("2015", "2018", "2020"),
                       labels = c("cycle 1", "cycle 2", "cycle 3"))

# select PRASE  ------------------------------------------------------------

# pr

pra_pr <- pra_pr %>%
  select(var, year, d04, d08, d12, d16, d20) %>% 
  filter(year %in% c("2015", "2018", "2020"))

pra_pr$var <- factor(pra_pr$var)
pra_pr$year <- factor(pra_pr$year, levels = c("2015", "2018", "2020"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))
#roh

pra_roh <- pra_roh %>%
  select(var, year, roh)%>% 
  filter(year %in% c("2015", "2018", "2020"))

pra_roh$var <- factor(pra_roh$var)
pra_roh$year <- factor(pra_roh$year, levels = c("2015", "2018", "2020"),
                       labels = c("cycle 1", "cycle 2", "cycle 3"))

#ud

pra_ud <- pra_ud %>%
  select(var, year, unitd)%>% 
  filter(year %in% c("2015", "2018", "2020"))

pra_ud$var <- factor(pra_ud$var)
pra_ud$year <- factor(pra_ud$year, levels = c("2015", "2018", "2020"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))

# hc

pra_hc <- pra_hc %>%
  select(var, year, hcon)%>% 
  filter(year %in% c("2015", "2018", "2020"))

pra_hc$var <- factor(pra_hc$var)
pra_hc$year <- factor(pra_hc$year, levels = c("2015", "2018", "2020"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))
# select NPK  --------------------------------------------------------------

# pr

npk_pr <- npk_pr %>%
  select(var, year, d04, d08, d12, d16, d20) %>% 
  filter(year %in% c("2015", "2018", "2020"))

npk_pr$var <- factor(npk_pr$var)
npk_pr$year <- factor(npk_pr$year, levels = c("2015", "2018", "2020"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))
#roh

npk_roh <- npk_roh %>%
  select(var, year, roh)%>% 
  filter(year %in% c("2015", "2018", "2020"))

npk_roh$var <- factor(npk_roh$var)
npk_roh$year <- factor(npk_roh$year, levels = c("2015", "2018", "2020"),
                       labels = c("cycle 1", "cycle 2", "cycle 3"))

#ud

npk_ud <- npk_ud %>%
  select(var, year, unitd)%>% 
  filter(year %in% c("2015", "2018", "2020"))

npk_ud$var <- factor(npk_ud$var)
npk_ud$year <- factor(npk_ud$year, levels = c("2015", "2018", "2020"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))

# hc

npk_hc <- npk_hc %>%
  select(var, year, hcon)%>% 
  filter(year %in% c("2015", "2018", "2020"))

npk_hc$var <- factor(npk_hc$var)
npk_hc$year <- factor(npk_hc$year, levels = c("2015", "2018", "2020"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))

# analysis KRAVA ---------------------------------------------------------

#pr

kra_pr_l <- kra_pr %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres")

kra_pr_c1 <- kra_pr_l %>%
  filter(year %in% c("cycle 1"))
kra_pr_c2 <- kra_pr_l %>%
  filter(year %in% c("cycle 2"))
kra_pr_c3 <- kra_pr_l %>%
  filter(year %in% c("cycle 3"))

mkrapr_c1<- aov(penres ~ var, data = kra_pr_c1)
summary(mkrapr_c1)
TukeyHSD(mkrapr_c1)
mkrapr_c2<- aov(penres ~ var, data = kra_pr_c2)
summary(mkrapr_c2)
TukeyHSD(mkrapr_c2)
mkrapr_c3<- aov(penres ~ var, data = kra_pr_c3)
summary(mkrapr_c3)
TukeyHSD(mkrapr_c3)

#roh

kra_roh_c1 <- kra_roh %>%
  filter(year %in% c("cycle 1"))
kra_roh_c2 <- kra_roh %>%
  filter(year %in% c("cycle 2"))
kra_roh_c3 <- kra_roh %>%
  filter(year %in% c("cycle 3"))

mkraroh_c1<- aov(roh ~ var, data = kra_roh_c1)
summary(mkraroh_c1)
TukeyHSD(mkraroh_c1)
mkraroh_c2<- aov(roh ~ var, data = kra_roh_c2)
summary(mkraroh_c2)
TukeyHSD(mkraroh_c2)
mkraroh_c3<- aov(roh ~ var, data = kra_roh_c3)
summary(mkraroh_c3)
TukeyHSD(mkraroh_c3)

#ud

kra_ud_c1 <- kra_ud %>%
  filter(year %in% c("cycle 1"))
kra_ud_c2 <- kra_ud %>%
  filter(year %in% c("cycle 2"))
kra_ud_c3 <- kra_ud %>%
  filter(year %in% c("cycle 3"))

mkraud_c1<- aov(unitd ~ var, data = kra_ud_c1)
summary(mkraud_c1)
TukeyHSD(mkraud_c1)
mkraud_c2<- aov(unitd ~ var, data = kra_ud_c2)
summary(mkraud_c2)
TukeyHSD(mkraud_c2)
mkraud_c3<- aov(unitd ~ var, data = kra_ud_c3)
summary(mkraud_c3)
TukeyHSD(mkraud_c3)

#hc

kra_hc_c1 <- kra_hc %>%
  filter(year %in% c("cycle 1"))
kra_hc_c2 <- kra_hc %>%
  filter(year %in% c("cycle 2"))
kra_hc_c3 <- kra_hc %>%
  filter(year %in% c("cycle 3"))

mkrahc_c1<- aov(hcon ~ var, data = kra_hc_c1)
summary(mkrahc_c1)
TukeyHSD(mkrahc_c1)
mkrahc_c2<- aov(hcon ~ var, data = kra_hc_c2)
summary(mkrahc_c2)
TukeyHSD(mkrahc_c2)
mkrahc_c3<- aov(hcon ~ var, data = kra_hc_c3)
summary(mkrahc_c3)
TukeyHSD(mkrahc_c3)

# analysis PRASE ----------------------------------------------------------

#pr

pra_pr_l <- pra_pr %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres")

pra_pr_c1 <- pra_pr_l %>%
  filter(year %in% c("cycle 1"))
pra_pr_c2 <- pra_pr_l %>%
  filter(year %in% c("cycle 2"))
pra_pr_c3 <- pra_pr_l %>%
  filter(year %in% c("cycle 3"))

mprapr_c1<- aov(penres ~ var, data = pra_pr_c1)
summary(mprapr_c1)
TukeyHSD(mprapr_c1)
mprapr_c2<- aov(penres ~ var, data = pra_pr_c2)
summary(mprapr_c2)
TukeyHSD(mprapr_c2)
mprapr_c3<- aov(penres ~ var, data = pra_pr_c3)
summary(mprapr_c3)
TukeyHSD(mprapr_c3)

#roh

pra_roh_c1 <- pra_roh %>%
  filter(year %in% c("cycle 1"))
pra_roh_c2 <- pra_roh %>%
  filter(year %in% c("cycle 2"))
pra_roh_c3 <- pra_roh %>%
  filter(year %in% c("cycle 3"))

mpraroh_c1<- aov(roh ~ var, data = pra_roh_c1)
summary(mpraroh_c1)
TukeyHSD(mpraroh_c1)
mpraroh_c2<- aov(roh ~ var, data = pra_roh_c2)
summary(mpraroh_c2)
TukeyHSD(mpraroh_c2)
mpraroh_c3<- aov(roh ~ var, data = pra_roh_c3)
summary(mpraroh_c3)
TukeyHSD(mpraroh_c3)

#ud

pra_ud_c1 <- pra_ud %>%
  filter(year %in% c("cycle 1"))
pra_ud_c2 <- pra_ud %>%
  filter(year %in% c("cycle 2"))
pra_ud_c3 <- pra_ud %>%
  filter(year %in% c("cycle 3"))

mpraud_c1<- aov(unitd ~ var, data = pra_ud_c1)
summary(mpraud_c1)
TukeyHSD(mpraud_c1)
mpraud_c2<- aov(unitd ~ var, data = pra_ud_c2)
summary(mpraud_c2)
TukeyHSD(mpraud_c2)
mpraud_c3<- aov(unitd ~ var, data = pra_ud_c3)
summary(mpraud_c3)
TukeyHSD(mpraud_c3)

#hc

pra_hc_c1 <- pra_hc %>%
  filter(year %in% c("cycle 1"))
pra_hc_c2 <- pra_hc %>%
  filter(year %in% c("cycle 2"))
pra_hc_c3 <- pra_hc %>%
  filter(year %in% c("cycle 3"))

mprahc_c1<- aov(hcon ~ var, data = pra_hc_c1)
summary(mprahc_c1)
TukeyHSD(mprahc_c1)
mprahc_c2<- aov(hcon ~ var, data = pra_hc_c2)
summary(mprahc_c2)
TukeyHSD(mprahc_c2)
mprahc_c3<- aov(hcon ~ var, data = pra_hc_c3)
summary(mprahc_c3)
TukeyHSD(mprahc_c3)


# analysis NPK ------------------------------------------------------------

#pr

npk_pr_l <- npk_pr %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres")

npk_pr_c1 <- npk_pr_l %>%
  filter(year %in% c("cycle 1"))
npk_pr_c2 <- npk_pr_l %>%
  filter(year %in% c("cycle 2"))
npk_pr_c3 <- npk_pr_l %>%
  filter(year %in% c("cycle 3"))

mnpkpr_c1<- aov(penres ~ var, data = npk_pr_c1)
summary(mnpkpr_c1)
TukeyHSD(mnpkpr_c1)
mnpkpr_c2<- aov(penres ~ var, data = npk_pr_c2)
summary(mnpkpr_c2)
TukeyHSD(mnpkpr_c2)
mnpkpr_c3<- aov(penres ~ var, data = npk_pr_c3)
summary(mnpkpr_c3)
TukeyHSD(mnpkpr_c3)

#roh

npk_roh_c1 <- npk_roh %>%
  filter(year %in% c("cycle 1"))
npk_roh_c2 <- npk_roh %>%
  filter(year %in% c("cycle 2"))
npk_roh_c3 <- npk_roh %>%
  filter(year %in% c("cycle 3"))

mnpkroh_c1<- aov(roh ~ var, data = npk_roh_c1)
summary(mnpkroh_c1)
TukeyHSD(mnpkroh_c1)
mnpkroh_c2<- aov(roh ~ var, data = npk_roh_c2)
summary(mnpkroh_c2)
TukeyHSD(mnpkroh_c2)
mnpkroh_c3<- aov(roh ~ var, data = npk_roh_c3)
summary(mnpkroh_c3)
TukeyHSD(mnpkroh_c3)

#ud

npk_ud_c1 <- npk_ud %>%
  filter(year %in% c("cycle 1"))
npk_ud_c2 <- npk_ud %>%
  filter(year %in% c("cycle 2"))
npk_ud_c3 <- npk_ud %>%
  filter(year %in% c("cycle 3"))

mnpkud_c1<- aov(unitd ~ var, data = npk_ud_c1)
summary(mnpkud_c1)
TukeyHSD(mnpkud_c1)
mnpkud_c2<- aov(unitd ~ var, data = npk_ud_c2)
summary(mnpkud_c2)
TukeyHSD(mnpkud_c2)
mnpkud_c3<- aov(unitd ~ var, data = npk_ud_c3)
summary(mnpkud_c3)
TukeyHSD(mnpkud_c3)

#hc

npk_hc_c1 <- npk_hc %>%
  filter(year %in% c("cycle 1"))
npk_hc_c2 <- npk_hc %>%
  filter(year %in% c("cycle 2"))
npk_hc_c3 <- npk_hc %>%
  filter(year %in% c("cycle 3"))

mnpkhc_c1<- aov(hcon ~ var, data = npk_hc_c1)
summary(mnpkhc_c1)
TukeyHSD(mnpkhc_c1)
mnpkhc_c2<- aov(hcon ~ var, data = npk_hc_c2)
summary(mnpkhc_c2)
TukeyHSD(mnpkhc_c2)
mnpkhc_c3<- aov(hcon ~ var, data = npk_hc_c3)
summary(mnpkhc_c3)
TukeyHSD(mnpkhc_c3)

# Yields ------------------------------------------------------------------

col15 <- "limegreen"
col18 <- "dodgerblue4"
col20 <- "gray11"

yld <- read_excel("red/yield.xlsx", sheet = 1)

yldl <- yld %>% 
  melt(id.vars = c("var", "var_id"), variable.name = "year", value.name = "yield") %>% 
  filter(year %in% c("2015", "2018", "2020"))

yldl$var <- factor(yldl$var)

# ggplot(yldl, aes(year, yield, fill = var))+
#   geom_bar(stat = "identity", position = "dodge")+
#   theme_minimal()

y15 <- yldl %>% 
  filter(year == "2015")
y18 <- yldl %>% 
  filter(year == "2018")
y20 <- yldl %>% 
  filter(year == "2020")

ggplot(y15, aes(var, yield))+
  geom_bar(stat = "identity", position = "dodge", fill = col15)+
  ylab(expression("Yield [ta"~ha^-1~"]"))+ #expression("Digestate Dose [ t "~ha^-1~"]")
  xlab("")+
  ggtitle("2015 / maize")+
  theme_minimal()
ggsave("plots/yield15.png", device = "png", width = 6, height = 3, dpi = 500)

ggplot(y18, aes(var, yield))+
  geom_bar(stat = "identity", position = "dodge", fill = col18)+
  ylab(expression("Yield [ta"~ha^-1~"]"))+ #expression("Digestate Dose [ t "~ha^-1~"]")
  xlab("")+
  ggtitle("2018 / maize")+
  theme_minimal()
ggsave("plots/yield18.png", device = "png", width = 6, height = 3, dpi = 500)

ggplot(y20, aes(var, yield))+
  geom_bar(stat = "identity", position = "dodge", fill = col20)+
  ylab(expression("Yield [ta"~ha^-1~"]"))+ #expression("Digestate Dose [ t "~ha^-1~"]")
  xlab("")+
  ggtitle("2020 / winter wheat")+
  theme_minimal()
ggsave("plots/yield20.png", device = "png", width = 6, height = 3, dpi = 500)

# plots KRAVA -------------------------------------------------------------------

# pr
kra_pr_div <- kra_pr %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres") %>% 
  mutate(div = penres*100-100) %>% 
  filter(var %in% c("C_sol", "C_fix", "C_fix_sol"))

dept <- c("20", "16", "12", "8", "4")
kra_pr_div$depth <- factor(kra_pr_div$depth, levels = c("d20", "d16", "d12", "d08", "d04"), 
                           labels = dept)

ggplot(kra_pr_div, aes(depth, div, fill = year))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", width = 0.6)+
  scale_fill_manual(values = tripaleta)+
  facet_grid(year ~ var, scales="fixed")+
  coord_flip()+
  labs(x = "Depth [cm]", y = "Cone Index [%]", title = "Cattle Manure")+
  theme_minimal()+
  theme(legend.position = "none")
# ggsave("plots/kra_pr.png", device = "png", width = 6, height = 4, dpi = 500)

#roh

kra_roh_div <- kra_roh %>% 
  mutate(div = roh*100-100) %>% 
  filter(var %in% c("C_sol", "C_fix", "C_fix_sol"))

ggplot(kra_roh_div, aes(var, div, fill = year))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", width = 0.6)+
  scale_fill_manual(values = tripaleta)+
  labs(y = "Reduced Bulk Density [%]", 
       x = "", title = "Cattle Manure", fill = "")+
  theme_minimal()
# ggsave("plots/kra_roh.png", device = "png", width = 6, height = 3, dpi = 500)

#ud

kra_ud_div <- kra_ud %>% 
  mutate(div = unitd*100-100) %>% 
  filter(var %in% c("C_sol", "C_fix", "C_fix_sol"))

ggplot(kra_ud_div, aes(var, div, fill = year))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", width = 0.6)+
  scale_fill_manual(values = tripaleta)+
  labs(y = "Unit Draft [%]", 
       x = "", title = "Cattle Manure", fill = "")+
  theme_minimal()
# ggsave("plots/kra_ud.png", device = "png", width = 6, height = 3, dpi = 500)

# hc

kra_hc_div <- kra_hc %>% 
  mutate(div = hcon*100-100) %>% 
  filter(var %in% c("C_sol", "C_fix", "C_fix_sol"))

ggplot(kra_hc_div, aes(var, div, fill = year))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", width = 0.6)+
  scale_fill_manual(values = tripaleta)+
  labs(y = "Saturated Hydraulic Conductivity [%]", 
       x = "", title = "Cattle Manure", fill = "")+
  theme_minimal()
# ggsave("plots/kra_hc.png", device = "png", width = 6, height = 3, dpi = 500)

# plots PRASE---------------------------------------------------------------------

#pr

pra_pr_div <- pra_pr %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres") %>% 
  mutate(div = penres*100-100) %>% 
  filter(var %in% c("P_sol", "P_fix", "P_fix_sol"))
# pra_pr_div$year <- factor(pra_pr_div$year)

pra_pr_div$depth <- factor(pra_pr_div$depth, levels = c("d20", "d16", "d12", "d08", "d04"), labels = dept)

ggplot(pra_pr_div, aes(depth, div, fill = year))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", width = 0.6)+
  scale_fill_manual(values = tripaleta)+
  facet_grid(year ~ var, scales="fixed")+
  coord_flip()+
  labs(x = "Depth [cm]", y = "Cone Index [%]", title = "Pig Manure")+
  theme_minimal()+
  theme(legend.position = "none")
# ggsave("plots/pra_pr.png", device = "png", width = 6, height = 4, dpi = 500)

#roh 

pra_roh_div <- pra_roh %>% 
  mutate(div = roh*100-100) %>% 
  filter(var %in% c("P_sol", "P_fix", "P_fix_sol"))

ggplot(pra_roh_div, aes(var, div, fill = year))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", width = 0.6)+
  scale_fill_manual(values = tripaleta)+
  labs(y = "Reduced Bulk Density [%]", 
       x = "", title = "Pig Manure", fill = "")+
  theme_minimal()
# ggsave("plots/pra_roh.png", device = "png", width = 6, height = 3, dpi = 500)

#ud

pra_ud_div <- pra_ud %>% 
  mutate(div = unitd*100-100) %>% 
  filter(var %in% c("P_sol", "P_fix", "P_fix_sol"))

ggplot(pra_ud_div, aes(var, div, fill = year))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", width = 0.6)+
  scale_fill_manual(values = tripaleta)+
  labs(y = "Unit Draft [%]", 
       x = "", title = "Pig Manure", fill = "")+
  theme_minimal()
# ggsave("plots/pra_ud.png", device = "png", width = 6, height = 3, dpi = 500)

#hc

pra_hc_div <- pra_hc %>% 
  mutate(div = hcon*100-100) %>% 
  filter(var %in% c("P_sol", "P_fix", "P_fix_sol"))

ggplot(pra_hc_div, aes(var, div, fill = year))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", width = 0.6)+
  scale_fill_manual(values = tripaleta)+
  labs(y = "Saturated Hydraulic Conductivity [%]", 
       x = "", title = "Pig Manure", fill = "")+
  theme_minimal()
# ggsave("plots/pra_hc.png", device = "png", width = 6, height = 3, dpi = 500)

# plots NPK ---------------------------------------------------------------

#pr

npk_pr_div <- npk_pr %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres") %>% 
  mutate(div = penres*100-100) %>% 
  filter(var %in% c("C_fym", "sol", "P_fym"))
# npk_pr_div$year <- factor(npk_pr_div$year)

npk_pr_div$depth <- factor(npk_pr_div$depth, levels = c("d20", "d16", "d12", "d08", "d04"), 
                           labels = dept)

ggplot(npk_pr_div, aes(depth, div, fill = year))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", width = 0.6)+
  scale_fill_manual(values = tripaleta)+
  facet_grid(year ~ var, scales="fixed")+
  coord_flip()+
  labs(x = "Depth [cm]", y = "Cone Index [%]", title = "NPK")+
  theme_minimal()+
  theme(legend.position = "none")
ggsave("plots/npk_pr.png", device = "png", width = 6, height = 4, dpi = 500)

#roh

npk_roh_div <- npk_roh %>% 
  mutate(div = roh*100-100) %>% 
  filter(var %in% c("C_fym", "sol", "P_fym"))

ggplot(npk_roh_div, aes(var, div, fill = year))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", width = 0.6)+
  scale_fill_manual(values = tripaleta)+
  labs(y = "Reduced Bulk Density [%]", 
       x = "", title = "NPK", fill = "")+
  theme_minimal()
# ggsave("plots/npk_roh.png", device = "png", width = 6, height = 3, dpi = 500)

#ud 

npk_ud_div <- npk_ud %>% 
  mutate(div = unitd*100-100) %>% 
  filter(var %in% c("C_fym", "sol", "P_fym"))

ggplot(npk_ud_div, aes(var, div, fill = year))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", width = 0.6)+
  scale_fill_manual(values = tripaleta)+
  labs(y = "Unit Draft [%]", 
       x = "", title = "NPK", fill = "")+
  theme_minimal()
# ggsave("plots/npk_ud.png", device = "png", width = 6, height = 3, dpi = 500)

#hc

npk_hc_div <- npk_hc %>% 
  mutate(div = hcon*100-100) %>% 
  filter(var %in% c("C_fym", "sol", "P_fym"))

ggplot(npk_hc_div, aes(var, div, fill = year))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", width = 0.6)+
  scale_fill_manual(values = tripaleta)+
  labs(y = "Saturated Hydraulic Conductivity [%]", 
       x = "", title = "NPK", fill = "")+
  theme_minimal()
# ggsave("plots/npk_hc.png", device = "png", width = 6, height = 3, dpi = 500)


# C_fix------------------------

cfix_pr <- kra_pr %>% 
  filter(var == "C_fix") %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres")

mcfix_pr<- aov(penres ~ year, data = cfix_pr)
summary(mcfix_pr)
TukeyHSD(mcfix_pr)

#roh
cfix_roh <- kra_roh %>% 
  filter(var == "C_fix")

mcfix_roh<- aov(roh ~ year, data = cfix_roh)
summary(mcfix_roh)
TukeyHSD(mcfix_roh)

#ud
cfix_ud <- kra_ud %>% 
  filter(var == "C_fix")

mcfix_ud<- aov(unitd ~ year, data = cfix_ud)
summary(mcfix_ud)
TukeyHSD(mcfix_ud)

#hc
cfix_hc <- kra_hc %>% 
  filter(var == "C_fix")

mcfix_hc<- aov(hcon ~ year, data = cfix_hc)
summary(mcfix_hc)
TukeyHSD(mcfix_hc)

# C_fix_sol ---------------------------------------------------------------

cfixsol_pr <- kra_pr %>% 
  filter(var == "C_fix_sol") %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres")

mcfixsol_pr<- aov(penres ~ year, data = cfixsol_pr)
summary(mcfixsol_pr)
TukeyHSD(mcfixsol_pr)

#roh
cfixsol_roh <- kra_roh %>% 
  filter(var == "C_fix_sol")

mcfixsol_roh<- aov(roh ~ year, data = cfixsol_roh)
summary(mcfixsol_roh)
TukeyHSD(mcfixsol_roh)

#ud
cfixsol_ud <- kra_ud %>% 
  filter(var == "C_fix_sol")

mcfixsol_ud<- aov(unitd ~ year, data = cfixsol_ud)
summary(mcfixsol_ud)
TukeyHSD(mcfixsol_ud)

#hc
cfixsol_hc <- kra_hc %>% 
  filter(var == "C_fix_sol")

mcfixsol_hc<- aov(hcon ~ year, data = cfixsol_hc)
summary(mcfixsol_hc)
TukeyHSD(mcfixsol_hc)

# C_sol ---------------------------------------------------------------

csol_pr <- kra_pr %>% 
  filter(var == "C_sol") %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres")

mcsol_pr<- aov(penres ~ year, data = csol_pr)
summary(mcsol_pr)
TukeyHSD(mcsol_pr)

#roh
csol_roh <- kra_roh %>% 
  filter(var == "C_sol")

mcsol_roh<- aov(roh ~ year, data = csol_roh)
summary(mcsol_roh)
TukeyHSD(mcsol_roh)

#ud
csol_ud <- kra_ud %>% 
  filter(var == "C_sol")

mcsol_ud<- aov(unitd ~ year, data = csol_ud)
summary(mcsol_ud)
TukeyHSD(mcsol_ud)

#hc
csol_hc <- kra_hc %>% 
  filter(var == "C_sol")

mcsol_hc<- aov(hcon ~ year, data = csol_hc)
summary(mcsol_hc)
TukeyHSD(mcsol_hc)

# P_fix------------------------
  
pfix_pr <- pra_pr %>% 
filter(var == "P_fix") %>% 
melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres")

mpfix_pr<- aov(penres ~ year, data = pfix_pr)
summary(mpfix_pr)
TukeyHSD(mpfix_pr)

#roh
pfix_roh <- pra_roh %>% 
  filter(var == "P_fix")

mpfix_roh<- aov(roh ~ year, data = pfix_roh)
summary(mpfix_roh)
TukeyHSD(mpfix_roh)

#ud
pfix_ud <- pra_ud %>% 
  filter(var == "P_fix")

mpfix_ud<- aov(unitd ~ year, data = pfix_ud)
summary(mpfix_ud)
TukeyHSD(mpfix_ud)

#hc
pfix_hc <- pra_hc %>% 
  filter(var == "P_fix")

mpfix_hc<- aov(hcon ~ year, data = pfix_hc)
summary(mpfix_hc)
TukeyHSD(mpfix_hc)

# P_fix_sol ---------------------------------------------------------------

pfixsol_pr <- pra_pr %>% 
  filter(var == "P_fix_sol") %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres")

mpfixsol_pr<- aov(penres ~ year, data = pfixsol_pr)
summary(mpfixsol_pr)
TukeyHSD(mpfixsol_pr)

#roh
pfixsol_roh <- pra_roh %>% 
  filter(var == "P_fix_sol")

mpfixsol_roh<- aov(roh ~ year, data = pfixsol_roh)
summary(mpfixsol_roh)
TukeyHSD(mpfixsol_roh)

#ud
pfixsol_ud <- pra_ud %>% 
  filter(var == "P_fix_sol")

mpfixsol_ud<- aov(unitd ~ year, data = pfixsol_ud)
summary(mpfixsol_ud)
TukeyHSD(mpfixsol_ud)

#hc
pfixsol_hc <- pra_hc %>% 
  filter(var == "P_fix_sol")

mpfixsol_hc<- aov(hcon ~ year, data = pfixsol_hc)
summary(mpfixsol_hc)
TukeyHSD(mpfixsol_hc)

# P_sol ---------------------------------------------------------------

psol_pr <- pra_pr %>% 
  filter(var == "P_sol") %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres")

mpsol_pr<- aov(penres ~ year, data = psol_pr)
summary(mpsol_pr)
TukeyHSD(mpsol_pr)

#roh
psol_roh <- pra_roh %>% 
  filter(var == "P_sol")

mpsol_roh<- aov(roh ~ year, data = psol_roh)
summary(mpsol_roh)
TukeyHSD(mpsol_roh)

#ud
psol_ud <- pra_ud %>% 
  filter(var == "P_sol")

mpsol_ud<- aov(unitd ~ year, data = psol_ud)
summary(mpsol_ud)
TukeyHSD(mpsol_ud)

#hc
psol_hc <- pra_hc %>% 
  filter(var == "P_sol")

mpsol_hc<- aov(hcon ~ year, data = psol_hc)
summary(mpsol_hc)
TukeyHSD(mpsol_hc)

# C_fym ---------------------------------------------------------------

cfym_pr <- npk_pr %>% 
  filter(var == "C_fym") %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres")

mcfym_pr<- aov(penres ~ year, data = cfym_pr)
summary(mcfym_pr)
TukeyHSD(mcfym_pr)

#roh
cfym_roh <- npk_roh %>% 
  filter(var == "C_fym")

mcfym_roh<- aov(roh ~ year, data = cfym_roh)
summary(mcfym_roh)
TukeyHSD(mcfym_roh)

#ud
cfym_ud <- npk_ud %>% 
  filter(var == "C_fym")

mcfym_ud<- aov(unitd ~ year, data = cfym_ud)
summary(mcfym_ud)
TukeyHSD(mcfym_ud)

#hc
cfym_hc <- npk_hc %>% 
  filter(var == "C_fym")

mcfym_hc<- aov(hcon ~ year, data = cfym_hc)
summary(mcfym_hc)
TukeyHSD(mcfym_hc)

# P_fym ---------------------------------------------------------------

pfym_pr <- npk_pr %>% 
  filter(var == "P_fym") %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres")

mpfym_pr<- aov(penres ~ year, data = pfym_pr)
summary(mpfym_pr)
TukeyHSD(mpfym_pr)

#roh
pfym_roh <- npk_roh %>% 
  filter(var == "P_fym")

mpfym_roh<- aov(roh ~ year, data = pfym_roh)
summary(mpfym_roh)
TukeyHSD(mpfym_roh)

#ud
pfym_ud <- npk_ud %>% 
  filter(var == "P_fym")

mpfym_ud<- aov(unitd ~ year, data = pfym_ud)
summary(mpfym_ud)
TukeyHSD(mpfym_ud)

#hc
pfym_hc <- npk_hc %>% 
  filter(var == "P_fym")

mpfym_hc<- aov(hcon ~ year, data = pfym_hc)
summary(mpfym_hc)
TukeyHSD(mpfym_hc)

# sol ---------------------------------------------------------------

sol_pr <- npk_pr %>% 
  filter(var == "sol") %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres")

msol_pr<- aov(penres ~ year, data = sol_pr)
summary(msol_pr)
TukeyHSD(msol_pr)

#roh
sol_roh <- npk_roh %>% 
  filter(var == "sol")

msol_roh<- aov(roh ~ year, data = sol_roh)
summary(msol_roh)
TukeyHSD(msol_roh)

#ud
sol_ud <- npk_ud %>% 
  filter(var == "sol")

msol_ud<- aov(unitd ~ year, data = sol_ud)
summary(msol_ud)
TukeyHSD(msol_ud)

#hc
sol_hc <- npk_hc %>% 
  filter(var == "sol")

msol_hc<- aov(hcon ~ year, data = sol_hc)
summary(msol_hc)
TukeyHSD(msol_hc)

