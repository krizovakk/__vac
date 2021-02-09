# base --------------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")
# install.packages("RColorBrewer")
require(tidyverse)
require(readxl)
require(reshape2)
require(RColorBrewer)

nasepaleta <- c("darkgreen", "limegreen", "darkturquoise", "dodgerblue4",
                 "gray60","burlywood4", "gray11")
nasepaleta2 <- c("limegreen", "darkturquoise", "dodgerblue4",
                 "gray60","burlywood4", "gray11") # pro HC / 2014 chybi mereni
tripaleta <- c("limegreen", "dodgerblue4", "gray11") # pro 3 roky vyhodnoceni

# KRAVA load -----------------------------------------------------------

kra_pr <- read_excel("red/krava.xlsx", sheet = 2)
kra_roh <- read_excel("red/krava.xlsx", sheet = 3)
kra_ud <- read_excel("red/krava.xlsx", sheet = 4)
kra_hc <- read_excel("red/krava.xlsx", sheet = 5)

# PRASE load -------------------------------------------------------------------

pra_pr <- read_excel("red/prase.xlsx", sheet = 2)
pra_roh <- read_excel("red/prase.xlsx", sheet = 3)
pra_ud <- read_excel("red/prase.xlsx", sheet = 4)
pra_hc <- read_excel("red/prase.xlsx", sheet = 5)

# HNOJE load -------------------------------------------------------------------

npk_pr <- read_excel("red/npk.xlsx", sheet = 2)
npk_roh <- read_excel("red/npk.xlsx", sheet = 3)
npk_ud <- read_excel("red/npk.xlsx", sheet = 4)
npk_hc <- read_excel("red/npk.xlsx", sheet = 5)

# KRAVA VARIANTS  ---------------------------------------------------------------

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

# PRASE VARIANTS ----------------------------------------------------------

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

# NPK VARIANTS ----------------------------------------------------------

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

# SELECT ------------------------------------------------------------------

# penres

kra_pr <- kra_pr %>%
  select(var, year, d04, d08, d12, d16, d20) %>% 
  filter(year %in% c("2015", "2018", "2020"))
pra_pr <- pra_pr %>%
  select(var, year, d04, d08, d12, d16, d20)%>% 
  filter(year %in% c("2015", "2018", "2020"))
npk_pr <- npk_pr %>%
  select(var, year, d04, d08, d12, d16, d20)%>% 
  filter(year %in% c("2015", "2018", "2020"))

# roh

kra_roh <- kra_roh %>%
  select(var, year, roh)%>% 
  filter(year %in% c("2015", "2018", "2020"))
pra_roh <- pra_roh %>%
  select(var, year, roh)%>% 
  filter(year %in% c("2015", "2018", "2020"))
npk_roh <- npk_roh %>%
  select(var, year, roh)%>% 
  filter(year %in% c("2015", "2018", "2020"))

# unitd

kra_ud <- kra_ud %>%
  select(var, year, unitd)%>% 
  filter(year %in% c("2015", "2018", "2020"))
pra_ud <- pra_ud %>%
  select(var, year, unitd)%>% 
  filter(year %in% c("2015", "2018", "2020"))
npk_ud <- npk_ud %>%
  select(var, year, unitd)%>% 
  filter(year %in% c("2015", "2018", "2020"))

# hcon

kra_hc <- kra_hc %>%
  select(var, year, hcon)%>% 
  filter(year %in% c("2015", "2018", "2020"))
pra_hc <- pra_hc %>%
  select(var, year, hcon)%>% 
  filter(year %in% c("2015", "2018", "2020"))
npk_hc <- npk_hc %>%
  select(var, year, hcon)%>% 
  filter(year %in% c("2015", "2018", "2020"))

# PENRES ------------------------------------------------------------------

kra_pr$var <- factor(kra_pr$var)
pra_pr$var <- factor(pra_pr$var)
npk_pr$var <- factor(npk_pr$var)

kra_pr$year <- factor(kra_pr$year, levels = c("2015", "2018", "2020"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))
pra_pr$year <- factor(pra_pr$year, levels = c("2015", "2018", "2020"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))
npk_pr$year <- factor(npk_pr$year, levels = c("2015", "2018", "2020"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))

# krava

kra_pr_div <- kra_pr %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres") %>% 
  mutate(div = penres*100-100) %>% 
  filter(var %in% c("C_sol", "C_fix", "C_fix_sol"))
# kra_pr_div$year <- factor(kra_pr_div$year)

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

# prase

pra_pr_div <- pra_pr %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres") %>% 
  mutate(div = penres*100-100) %>% 
  filter(var %in% c("P_sol", "P_fix", "P_fix_sol"))
# pra_pr_div$year <- factor(pra_pr_div$year)

dept <- c("20", "16", "12", "8", "4")
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

# npk

npk_pr_div <- npk_pr %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres") %>% 
  mutate(div = penres*100-100) %>% 
  filter(var %in% c("C_fym", "sol", "P_fym"))
# npk_pr_div$year <- factor(npk_pr_div$year)

dept <- c("20", "16", "12", "8", "4")
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

###

# hist(pen$d4)
# hist(pen$d8)
# hist(pen$d12)
# hist(pen$d16)
# hist(pen$d20)
# 
# shapiro.test(pen$d4)
# shapiro.test(pen$d8)
# shapiro.test(pen$d12)
# shapiro.test(pen$d16)
# shapiro.test(pen$d20) # NR was not found by any depth
# 
# pairwise.wilcox.test(pen$d4, pen$var,
#                      p.adjust.method = "BH")
# 
# # significant difference is where p-value < 0.05
# 
# pairwise.wilcox.test(pen$d8, pen$var,
#                      p.adjust.method = "BH")
# 
# pairwise.wilcox.test(pen$d12, pen$var,
#                      p.adjust.method = "BH")
# 
# pairwise.wilcox.test(pen$d16, pen$var,
#                      p.adjust.method = "BH")
# 
# pairwise.wilcox.test(pen$d20, pen$var,
#                      p.adjust.method = "BH")

# ROH ---------------------------------------------------------------------

kra_roh$var <- factor(kra_roh$var)
pra_roh$var <- factor(pra_roh$var)
npk_roh$var <- factor(npk_roh$var)

kra_roh$year <- factor(kra_roh$year, levels = c("2015", "2018", "2020"),
                       labels = c("cycle 1", "cycle 2", "cycle 3"))
pra_roh$year <- factor(pra_roh$year, levels = c("2015", "2018", "2020"),
                       labels = c("cycle 1", "cycle 2", "cycle 3"))
npk_roh$year <- factor(npk_roh$year, levels = c("2015", "2018", "2020"),
                       labels = c("cycle 1", "cycle 2", "cycle 3"))

# krava

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

# prase

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

# npk

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

# UNIT DRAFT --------------------------------------------------------------

kra_ud$var <- factor(kra_ud$var)
pra_ud$var <- factor(pra_ud$var)
npk_ud$var <- factor(npk_ud$var)

kra_ud$year <- factor(kra_ud$year, levels = c("2015", "2018", "2020"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))
pra_ud$year <- factor(pra_ud$year, levels = c("2015", "2018", "2020"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))
npk_ud$year <- factor(npk_ud$year, levels = c("2015", "2018", "2020"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))

# krava

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

# prase

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

# npk

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

# HYDRAULIC COND -----------------------------------------------------------

kra_hc$var <- factor(kra_hc$var)
pra_hc$var <- factor(pra_hc$var)
npk_hc$var <- factor(npk_hc$var)

kra_hc$year <- factor(kra_hc$year, levels = c("2015", "2018", "2020"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))
pra_hc$year <- factor(pra_hc$year, levels = c("2015", "2018", "2020"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))
npk_hc$year <- factor(npk_hc$year, levels = c("2015", "2018", "2020"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))

# krava

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

# prase

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

# npk

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

# PENRES analysis ---------------------------------------------------------

# krava

kra_pr_l <- kra_pr %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres")

kra_pr_18 <- kra_pr_l %>%
  filter(year %in% c("2018"))
kra_pr_19 <- kra_pr_l %>%
  filter(year %in% c("2019"))
kra_pr_20 <- kra_pr_l %>%
  filter(year %in% c("2020"))

mkrapr18<- aov(penres ~ var, data = kra_pr_18)
summary(mkrapr18)
TukeyHSD(mkrapr18)
mkrapr19<- aov(penres ~ var, data = kra_pr_19)
summary(mkrapr19)
TukeyHSD(mkrapr19)
mkrapr20<- aov(penres ~ var, data = kra_pr_20)
summary(mkrapr20)
TukeyHSD(mkrapr20)

# prase

pra_pr_l <- pra_pr %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres")

pra_pr_18 <- pra_pr_l %>%
  filter(year %in% c("2018"))
pra_pr_19 <- pra_pr_l %>%
  filter(year %in% c("2019"))
pra_pr_20 <- pra_pr_l %>%
  filter(year %in% c("2020"))

mprapr18<- aov(penres ~ var, data = pra_pr_18)
summary(mprapr18)
TukeyHSD(mprapr18)
mprapr19<- aov(penres ~ var, data = pra_pr_19)
summary(mprapr19)
TukeyHSD(mprapr19)
mprapr20<- aov(penres ~ var, data = pra_pr_20)
summary(mprapr20)
TukeyHSD(mprapr20)

# npk

npk_pr_l <- npk_pr %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres")

npk_pr_18 <- npk_pr_l %>%
  filter(year %in% c("2018"))
npk_pr_19 <- npk_pr_l %>%
  filter(year %in% c("2019"))
npk_pr_20 <- npk_pr_l %>%
  filter(year %in% c("2020"))

mnpkpr18<- aov(penres ~ var, data = npk_pr_18)
summary(mnpkpr18)
TukeyHSD(mnpkpr18)
mnpkpr19<- aov(penres ~ var, data = npk_pr_19)
summary(mnpkpr19)
TukeyHSD(mnpkpr19)
mnpkpr20<- aov(penres ~ var, data = npk_pr_20)
summary(mnpkpr20)
TukeyHSD(mnpkpr20)


# ROH analysis ------------------------------------------------------------

# shapiro.test(kra_roh_an$roh) # p-value = 0.6447
# shapiro.test(pra_roh_an$roh) # p-value = 0.006437
# shapiro.test(npk_roh_an$roh) # p-value = 0.6524

# kra_roh_an <- kra_roh %>% 
#   filter(year %in% c("2018", "2019", "2020"))
# pra_roh_an <- pra_roh %>% 
#   filter(year %in% c("2018", "2019", "2020"))
# npk_roh_an <- npk_roh %>% 
#   filter(year %in% c("2018", "2019", "2020")) # mozna se bude hodit pro grafy

# krava

kra_roh_18 <- kra_roh %>%
  filter(year %in% c("2018"))
kra_roh_19 <- kra_roh %>%
  filter(year %in% c("2019"))
kra_roh_20 <- kra_roh %>%
  filter(year %in% c("2020"))
 
mkraroh18<- aov(roh ~ var, data = kra_roh_18)
summary(mkraroh18)
TukeyHSD(mkraroh18)
mkraroh19<- aov(roh ~ var, data = kra_roh_19)
summary(mkraroh19)
TukeyHSD(mkraroh19)
mkraroh20<- aov(roh ~ var, data = kra_roh_20)
summary(mkraroh20)
TukeyHSD(mkraroh20)

# prase

pra_roh_18 <- pra_roh %>%
  filter(year %in% c("2018"))
pra_roh_19 <- pra_roh %>%
  filter(year %in% c("2019"))
pra_roh_20 <- pra_roh %>%
  filter(year %in% c("2020"))

mpraroh18<- aov(roh ~ var, data = pra_roh_18)
summary(mpraroh18)
TukeyHSD(mpraroh18)
mpraroh19<- aov(roh ~ var, data = pra_roh_19)
summary(mpraroh19)
TukeyHSD(mpraroh19)
mpraroh20<- aov(roh ~ var, data = pra_roh_20)
summary(mpraroh20)
TukeyHSD(mpraroh20)

# npk

npk_roh_18 <- npk_roh %>%
  filter(year %in% c("2018"))
npk_roh_19 <- npk_roh %>%
  filter(year %in% c("2019"))
npk_roh_20 <- npk_roh %>%
  filter(year %in% c("2020"))

mnpkroh18<- aov(roh ~ var, data = npk_roh_18)
summary(mnpkroh18)
TukeyHSD(mnpkroh18)
mnpkroh19<- aov(roh ~ var, data = npk_roh_19)
summary(mnpkroh19)
TukeyHSD(mnpkroh19)
mnpkroh20<- aov(roh ~ var, data = npk_roh_20)
summary(mnpkroh20)
TukeyHSD(mnpkroh20)

# UNITD analysis ----------------------------------------------------------

# krava

kra_ud_18 <- kra_ud %>%
  filter(year %in% c("2018"))
kra_ud_19 <- kra_ud %>%
  filter(year %in% c("2019"))
kra_ud_20 <- kra_ud %>%
  filter(year %in% c("2020"))

mkraud18<- aov(unitd ~ var, data = kra_ud_18)
summary(mkraud18)
TukeyHSD(mkraud18)
mkraud19<- aov(unitd ~ var, data = kra_ud_19)
summary(mkraud19)
TukeyHSD(mkraud19)
mkraud20<- aov(unitd ~ var, data = kra_ud_20)
summary(mkraud20)
TukeyHSD(mkraud20)

# prase

pra_ud_18 <- pra_ud %>%
  filter(year %in% c("2018"))
pra_ud_19 <- pra_ud %>%
  filter(year %in% c("2019"))
pra_ud_20 <- pra_ud %>%
  filter(year %in% c("2020"))

mpraud18<- aov(unitd ~ var, data = pra_ud_18)
summary(mpraud18)
TukeyHSD(mpraud18)
mpraud19<- aov(unitd ~ var, data = pra_ud_19)
summary(mpraud19)
TukeyHSD(mpraud19)
mpraud20<- aov(unitd ~ var, data = pra_ud_20)
summary(mpraud20)
TukeyHSD(mpraud20)

# npk

npk_ud_18 <- npk_ud %>%
  filter(year %in% c("2018"))
npk_ud_19 <- npk_ud %>%
  filter(year %in% c("2019"))
npk_ud_20 <- npk_ud %>%
  filter(year %in% c("2020"))

mnpkud18<- aov(unitd ~ var, data = npk_ud_18)
summary(mnpkud18)
TukeyHSD(mnpkud18)
mnpkud19<- aov(unitd ~ var, data = npk_ud_19)
summary(mnpkud19)
TukeyHSD(mnpkud19)
mnpkud20<- aov(unitd ~ var, data = npk_ud_20)
summary(mnpkud20)
TukeyHSD(mnpkud20)

# HCOND analysis ----------------------------------------------------------

# krava

kra_hc_18 <- kra_hc %>%
  filter(year %in% c("2018"))
kra_hc_19 <- kra_hc %>%
  filter(year %in% c("2019"))
kra_hc_20 <- kra_hc %>%
  filter(year %in% c("2020"))

mkrahc18<- aov(hcon ~ var, data = kra_hc_18)
summary(mkrahc18)
TukeyHSD(mkrahc18)
mkrahc19<- aov(hcon ~ var, data = kra_hc_19)
summary(mkrahc19)
TukeyHSD(mkrahc19)
mkrahc20<- aov(hcon ~ var, data = kra_hc_20)
summary(mkrahc20)
TukeyHSD(mkrahc20)

# prase

pra_hc_18 <- pra_hc %>%
  filter(year %in% c("2018"))
pra_hc_19 <- pra_hc %>%
  filter(year %in% c("2019"))
pra_hc_20 <- pra_hc %>%
  filter(year %in% c("2020"))

mprahc18<- aov(hcon ~ var, data = pra_hc_18)
summary(mprahc18)
TukeyHSD(mprahc18)
mprahc19<- aov(hcon ~ var, data = pra_hc_19)
summary(mprahc19)
TukeyHSD(mprahc19)
mprahc20<- aov(hcon ~ var, data = pra_hc_20)
summary(mprahc20)
TukeyHSD(mprahc20)

# npk

npk_hc_18 <- npk_hc %>%
  filter(year %in% c("2018"))
npk_hc_19 <- npk_hc %>%
  filter(year %in% c("2019"))
npk_hc_20 <- npk_hc %>%
  filter(year %in% c("2020"))

mnpkhc18<- aov(hcon ~ var, data = npk_hc_18)
summary(mnpkhc18)
TukeyHSD(mnpkhc18)
mnpkhc19<- aov(hcon ~ var, data = npk_hc_19)
summary(mnpkhc19)
TukeyHSD(mnpkhc19)
mnpkhc20<- aov(hcon ~ var, data = npk_hc_20)
summary(mnpkhc20)
TukeyHSD(mnpkhc20)

# Yields ------------------------------------------------------------------

col18 <- "gray60"
col19 <- "burlywood4"
col20 <- "gray11"

yld <- read_excel("red/yield.xlsx", sheet = 1)

yldl <- yld %>% 
  melt(id.vars = c("var", "var_id"), variable.name = "year", value.name = "yield") %>% 
  filter(year %in% c("2018", "2019", "2020"))

yldl$var <- factor(yldl$var)

# ggplot(yldl, aes(year, yield, fill = var))+
#   geom_bar(stat = "identity", position = "dodge")+
#   theme_minimal()

y18 <- yldl %>% 
  filter(year == "2018")
y19 <- yldl %>% 
  filter(year == "2019")
y20 <- yldl %>% 
  filter(year == "2020")

ggplot(y18, aes(var, yield))+
  geom_bar(stat = "identity", position = "dodge", fill = col18)+
  ylab(expression("Yield [ta"~ha^-1~"]"))+ #expression("Digestate Dose [ t "~ha^-1~"]")
  xlab("")+
  ggtitle("2018 / maize")+
  theme_minimal()
ggsave("plots/yield18.png", device = "png", width = 6, height = 3, dpi = 500)

ggplot(y19, aes(var, yield))+
  geom_bar(stat = "identity", position = "dodge", fill = col19)+
  ylab(expression("Yield [ta"~ha^-1~"]"))+ #expression("Digestate Dose [ t "~ha^-1~"]")
  xlab("")+
  ggtitle("2019 / winter wheat")+
  theme_minimal()
ggsave("plots/yield19.png", device = "png", width = 6, height = 3, dpi = 500)

ggplot(y20, aes(var, yield))+
  geom_bar(stat = "identity", position = "dodge", fill = col20)+
  ylab(expression("Yield [ta"~ha^-1~"]"))+ #expression("Digestate Dose [ t "~ha^-1~"]")
  xlab("")+
  ggtitle("2020 / winter wheat")+
  theme_minimal()
ggsave("plots/yield20.png", device = "png", width = 6, height = 3, dpi = 500)



