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
# KRAVA load -----------------------------------------------------------

kra_pr <- read_excel("red/krava.xlsx", sheet = 2)
kra_roh <- read_excel("red/krava.xlsx", sheet = 3)
kra_ud <- read_excel("red/krava.xlsx", sheet = 4)
kra_hc <- read_excel("red/krava.xlsx", sheet = 5)
kra_y <- read_excel("red/krava.xlsx", sheet = 6)

# PRASE load -------------------------------------------------------------------

pra_pr <- read_excel("red/prase.xlsx", sheet = 2)
pra_roh <- read_excel("red/prase.xlsx", sheet = 3)
pra_ud <- read_excel("red/prase.xlsx", sheet = 4)
pra_hc <- read_excel("red/prase.xlsx", sheet = 5)
pra_y <- read_excel("red/prase.xlsx", sheet = 6)

# HNOJE load -------------------------------------------------------------------

npk_pr <- read_excel("red/npk.xlsx", sheet = 2)
npk_roh <- read_excel("red/npk.xlsx", sheet = 3)
npk_ud <- read_excel("red/npk.xlsx", sheet = 4)
npk_hc <- read_excel("red/npk.xlsx", sheet = 5)
npk_y <- read_excel("red/npk.xlsx", sheet = 6)


# KRAVA VARIANTS  ---------------------------------------------------------------

kra_hc$var[kra_hc$var == 1] <- "fix"
kra_hc$var[kra_hc$var == 2] <- "fix_sol"
kra_hc$var[kra_hc$var == 3] <- "c"
kra_hc$var[kra_hc$var == 4] <- "sol"

kra_pr$var[kra_pr$var == 1] <- "fix"
kra_pr$var[kra_pr$var == 2] <- "fix_sol"
kra_pr$var[kra_pr$var == 3] <- "c"
kra_pr$var[kra_pr$var == 4] <- "sol"

kra_roh$var[kra_roh$var == 1] <- "fix"
kra_roh$var[kra_roh$var == 2] <- "fix_sol"
kra_roh$var[kra_roh$var == 3] <- "c"
kra_roh$var[kra_roh$var == 4] <- "sol"

kra_ud$var[kra_ud$var == 1] <- "fix"
kra_ud$var[kra_ud$var == 2] <- "fix_sol"
kra_ud$var[kra_ud$var == 3] <- "c"
kra_ud$var[kra_ud$var == 4] <- "sol"

kra_y$var[kra_y$var == 1] <- "fix"
kra_y$var[kra_y$var == 2] <- "fix_sol"
kra_y$var[kra_y$var == 3] <- "c"
kra_y$var[kra_y$var == 4] <- "sol"

# PRASE VARIANTS ----------------------------------------------------------

pra_hc$var[pra_hc$var == 7] <- "fix"
pra_hc$var[pra_hc$var == 8] <- "fix_sol"
pra_hc$var[pra_hc$var == 9] <- "c"
pra_hc$var[pra_hc$var == 10] <- "sol"

pra_pr$var[pra_pr$var == 7] <- "fix"
pra_pr$var[pra_pr$var == 8] <- "fix_sol"
pra_pr$var[pra_pr$var == 9] <- "c"
pra_pr$var[pra_pr$var == 10] <- "sol"

pra_roh$var[pra_roh$var == 7] <- "fix"
pra_roh$var[pra_roh$var == 8] <- "fix_sol"
pra_roh$var[pra_roh$var == 9] <- "c"
pra_roh$var[pra_roh$var == 10] <- "sol"

pra_ud$var[pra_ud$var == 7] <- "fix"
pra_ud$var[pra_ud$var == 8] <- "fix_sol"
pra_ud$var[pra_ud$var == 9] <- "c"
pra_ud$var[pra_ud$var == 10] <- "sol"

pra_y$var[pra_y$var == 7] <- "fix"
pra_y$var[pra_y$var == 8] <- "fix_sol"
pra_y$var[pra_y$var == 9] <- "c"
pra_y$var[pra_y$var == 10] <- "sol"

# NPK VARIANTS ----------------------------------------------------------

npk_hc$var[npk_hc$var == 3] <- "c_fym"
npk_hc$var[npk_hc$var == 5] <- "sol"
npk_hc$var[npk_hc$var == 6] <- "c"
npk_hc$var[npk_hc$var == 9] <- "p_fym"

npk_pr$var[npk_pr$var == 3] <- "c_fym"
npk_pr$var[npk_pr$var == 5] <- "sol"
npk_pr$var[npk_pr$var == 6] <- "c"
npk_pr$var[npk_pr$var == 9] <- "p_fym"

npk_roh$var[npk_roh$var == 3] <- "c_fym"
npk_roh$var[npk_roh$var == 5] <- "sol"
npk_roh$var[npk_roh$var == 6] <- "c"
npk_roh$var[npk_roh$var == 9] <- "p_fym"

npk_ud$var[npk_ud$var == 3] <- "c_fym"
npk_ud$var[npk_ud$var == 5] <- "sol"
npk_ud$var[npk_ud$var == 6] <- "c"
npk_ud$var[npk_ud$var == 9] <- "p_fym"

npk_y$var[npk_y$var == 3] <- "c_fym"
npk_y$var[npk_y$var == 5] <- "sol"
npk_y$var[npk_y$var == 6] <- "c"
npk_y$var[npk_y$var == 9] <- "p_fym"

# SELECT ------------------------------------------------------------------

# penres

kra_pr <- kra_pr %>%
  select(var, year, d04, d08, d12, d16, d20)
pra_pr <- pra_pr %>%
  select(var, year, d04, d08, d12, d16, d20)
npk_pr <- npk_pr %>%
  select(var, year, d04, d08, d12, d16, d20)

# roh

kra_roh <- kra_roh %>%
  select(var, year, roh)
pra_roh <- pra_roh %>%
  select(var, year, roh)
npk_roh <- npk_roh %>%
  select(var, year, roh)

# unitd

kra_ud <- kra_ud %>%
  select(var, year, unitd)
pra_ud <- pra_ud %>%
  select(var, year, unitd)
npk_ud <- npk_ud %>%
  select(var, year, unitd)

# hcon

kra_hc <- kra_hc %>%
  select(var, year, hcon)
pra_hc <- pra_hc %>%
  select(var, year, hcon)
npk_hc <- npk_hc %>%
  select(var, year, hcon)

# yield

kra_y <- kra_y %>%
  select(var, s15, s16, s17, s18, s19, s20)
pra_y <- pra_y %>%
  select(var, s15, s16, s17, s18, s19, s20)
npk_y <- npk_y %>%
  select(var, s15, s16, s17, s18, s19, s20)

# PENRES ------------------------------------------------------------------

kra_pr$var <- factor(kra_pr$var)
pra_pr$var <- factor(pra_pr$var)
npk_pr$var <- factor(npk_pr$var)

kra_pr$year <- factor(kra_pr$year)
pra_pr$year <- factor(pra_pr$year)
npk_pr$year <- factor(npk_pr$year)

# krava

kra_pr_div <- kra_pr %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres") %>% 
  mutate(div = penres*100-100) %>% 
  filter(var %in% c("sol", "fix", "fix_sol"))
kra_pr_div$year <- factor(kra_pr_div$year)

dept <- c("20", "16", "12", "8", "4")
kra_pr_div$depth <- factor(kra_pr_div$depth, levels = c("d20", "d16", "d12", "d08", "d04"), labels = dept)

ggplot(kra_pr_div, aes(depth, div, fill = year))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", width = 0.6)+
  scale_fill_manual(values = nasepaleta)+
  facet_grid(year ~ var, scales="fixed")+
  coord_flip()+
  labs(x = "Depth [cm]", y = "Cone Index [%]", title = "Cattle Manure")+
  theme_minimal()+
  theme(legend.position = "none")
ggsave("plots/kra_pr.png", device = "png", width = 5, height = 6, dpi = 500)

###
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



# ROH ---------------------------------------------------------------------

kra_roh$var <- factor(kra_roh$var)
pra_roh$var <- factor(pra_roh$var)
npk_roh$var <- factor(npk_roh$var)

kra_roh$year <- factor(kra_roh$year)
pra_roh$year <- factor(pra_roh$year)
npk_roh$year <- factor(npk_roh$year)

# krava

kra_roh_div <- kra_roh %>% 
  mutate(div = roh*100-100) %>% 
  filter(var %in% c("sol", "fix", "fix_sol"))

ggplot(kra_roh_div, aes(var, div, fill = year))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", width = 0.6)+
  scale_fill_manual(values = nasepaleta)+
  labs(y = "Reduced Bulk Density [%]", 
       x = "", title = "Cattle Manure", fill = "Season")+
  theme_minimal()
ggsave("plots/kra_roh.png", device = "png", width = 6, height = 3, dpi = 500)

# UNIT DRAFT --------------------------------------------------------------

kra_ud$var <- factor(kra_ud$var)
pra_ud$var <- factor(pra_ud$var)
npk_ud$var <- factor(npk_ud$var)

kra_ud$year <- factor(kra_ud$year)
pra_ud$year <- factor(pra_ud$year)
npk_ud$year <- factor(npk_ud$year)

# krava

kra_ud_div <- kra_ud %>% 
  mutate(div = unitd*100-100) %>% 
  filter(var %in% c("sol", "fix", "fix_sol"))

ggplot(kra_ud_div, aes(var, div, fill = year))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", width = 0.6)+
  scale_fill_manual(values = nasepaleta)+
  labs(y = "Unit Draft [%]", 
       x = "", title = "Cattle Manure", fill = "Season")+
  theme_minimal()
ggsave("plots/kra_ud.png", device = "png", width = 6, height = 3, dpi = 500)

# HYDRAULIC COND -----------------------------------------------------------

kra_hc$var <- factor(kra_hc$var)
pra_hc$var <- factor(pra_hc$var)
npk_hc$var <- factor(npk_hc$var)

kra_hc$year <- factor(kra_hc$year)
pra_hc$year <- factor(pra_hc$year)
npk_hc$year <- factor(npk_hc$year)

# krava

kra_hc_div <- kra_hc %>% 
  mutate(div = hcon*100-100) %>% 
  filter(var %in% c("sol", "fix", "fix_sol"))

ggplot(kra_hc_div, aes(var, div, fill = year))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", width = 0.6)+
  scale_fill_manual(values = nasepaleta2)+
  labs(y = "Saturated Hydraulic Conductivity [%]", 
       x = "", title = "Cattle Manure", fill = "Season")+
  theme_minimal()
ggsave("plots/kra_hc.png", device = "png", width = 6, height = 3, dpi = 500)
