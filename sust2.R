# base --------------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")
# install.packages("RColorBrewer")
# install.packages("writexl")
require(tidyverse)
require(readxl)
require(reshape2)
require(RColorBrewer)
require(writexl)

# install.packages("extrafont")
# library(extrafont)
# font_import()
# loadfonts(device="win")       #Register fonts for Windows bitmap output
# fonts()                       #vector of font family names
##  [1] "Andale Mono"                  "AppleMyungjo"                
##  [3] "Arial Black"                  "Arial"                       
##  [5] "Arial Narrow"                 "Arial Rounded MT Bold"  

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

# general load -------------------------------------------------------------------

pr <- read_excel("red/sust2.xlsx", sheet = 1)
roh <- read_excel("red/sust2.xlsx", sheet = 2)
sfh <- read_excel("red/sust2.xlsx", sheet = 3)
ud <- read_excel("red/sust2.xlsx", sheet = 4)

ud <- ud %>% 
  mutate(udkn = ud/1000)

# krava PENRES -------------------------------------------------------------------

# load

kra_pr <- pr %>%
  select(var, varno, year, d04, d08, d12, d16, d20) %>% 
  filter(varno %in% c("1", "2", "3", "4")) %>% 
  select(var, year, d04, d08, d12, d16, d20)

# long format

kra_prl <- kra_pr %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres") 

# factors

dept <- c("20", "16", "12", "8", "4")
kra_prl$depth <- factor(kra_prl$depth, levels = c("d20", "d16", "d12", "d08", "d04"), 
                        labels = dept)
kra_prl$var <- factor(kra_prl$var)
kra_prl$year <- factor(kra_prl$year, levels = c("cycle_1", "cycle_2", "cycle_3"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))

# stats

df1 <- data_summary(kra_prl, varname="penres", 
                    groupnames=c("year", "var", "depth"))
head(df1)
write_xlsx(df1,"kra_penstat.xlsx") # package "writexl"

# plots

ggplot(df1, aes(depth, penres, fill=var))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=penres-sd, ymax=penres+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_brewer(palette = "Greys", breaks = rev(levels(df1$var)))+
  coord_flip()+
  facet_grid(. ~ year)+
  labs(y = "\nPenetration Resistance [MPa]", x = "Depth [cm]", fill = "", title = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
ggsave("plots/kra_pr.png", device = "png", width = 8, height = 4, dpi = 300)

# analyses

c1_krapr <- kra_prl %>% 
  filter(year == "cycle 1")
c2_krapr <- kra_prl %>% 
  filter(year == "cycle 2")
c3_krapr <- kra_prl %>% 
  filter(year == "cycle 3")

# kw 

kruskal.test(penres ~ var, data = c1_krapr)
kruskal.test(penres ~ var, data = c2_krapr)
kruskal.test(penres ~ var, data = c3_krapr)

# install.packages("pgirmess") # post-hoc testy
require(pgirmess)

kruskalmc(penres ~ var, data = c1_krapr, p=0.05)
kruskalmc(penres ~ var, data = c2_krapr, p=0.05)
kruskalmc(penres ~ var, data = c3_krapr, p=0.05)

## one way ANOVA

bartlett.test(penres ~ var, data = c1_krapr) 
bartlett.test(penres ~ var, data = c2_krapr) 
bartlett.test(penres ~ var, data = c3_krapr) 

simpkraprc1 <- aov(penres ~ var, data = c1_krapr)
simpkraprc2 <- aov(penres ~ var, data = c2_krapr)
simpkraprc3 <- aov(penres ~ var, data = c3_krapr)

summary(simpkraprc1)
summary(simpkraprc2)
summary(simpkraprc3)

# TukeyHSD(simp18)
# TukeyHSD(simp20)
# plot(TukeyHSD(simp18))
# plot(TukeyHSD(simp20))

# install.packages("multcomp")
# require(multcomp)
# summary(glht(simp18, linfct=mcp(var="Tukey")))
# summary(glht(simp20, linfct=mcp(var="Tukey")))

# prase PENRES -------------------------------------------------------------------

# load

pra_pr <- pr %>%
  select(var, varno, year, d04, d08, d12, d16, d20) %>% 
  filter(varno %in% c("7", "8", "9", "10")) %>% 
  select(var, year, d04, d08, d12, d16, d20)

# long format

pra_prl <- pra_pr %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres") 

# factors

dept <- c("20", "16", "12", "8", "4")
pra_prl$depth <- factor(pra_prl$depth, levels = c("d20", "d16", "d12", "d08", "d04"), 
                        labels = dept)
pra_prl$var <- factor(pra_prl$var)
pra_prl$year <- factor(pra_prl$year, levels = c("cycle_1", "cycle_2", "cycle_3"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))

# stats

df2 <- data_summary(pra_prl, varname="penres", 
                    groupnames=c("year", "var", "depth"))
head(df2)
write_xlsx(df2,"pra_penstat.xlsx") # package "writexl"

# plots

ggplot(df2, aes(depth, penres, fill=var))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=penres-sd, ymax=penres+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_brewer(palette = "Greys", breaks = rev(levels(df2$var)))+
  coord_flip()+
  facet_grid(. ~ year)+
  labs(y = "\nPenetration Resistance [MPa]", x = "Depth [cm]", fill = "", title = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
ggsave("plots/pra_pr.png", device = "png", width = 8, height = 4, dpi = 300)

# analyses

c1_prapr <- pra_prl %>% 
  filter(year == "cycle 1")
c2_prapr <- pra_prl %>% 
  filter(year == "cycle 2")
c3_prapr <- pra_prl %>% 
  filter(year == "cycle 3")

# kw 

kruskal.test(penres ~ var, data = c1_prapr)
kruskal.test(penres ~ var, data = c2_prapr)
kruskal.test(penres ~ var, data = c3_prapr)

# install.packages("pgirmess") # post-hoc testy
require(pgirmess)

kruskalmc(penres ~ var, data = c1_prapr, p=0.05)
kruskalmc(penres ~ var, data = c2_prapr, p=0.05)
kruskalmc(penres ~ var, data = c3_prapr, p=0.05)

## one way ANOVA

bartlett.test(penres ~ var, data = c1_prapr) 
bartlett.test(penres ~ var, data = c2_prapr) 
bartlett.test(penres ~ var, data = c3_prapr) 

simppraprc1 <- aov(penres ~ var, data = c1_prapr)
simppraprc2 <- aov(penres ~ var, data = c2_prapr)
simppraprc3 <- aov(penres ~ var, data = c3_prapr)

summary(simppraprc1)
summary(simppraprc2)
summary(simppraprc3)

# TukeyHSD(simp18)
# TukeyHSD(simp20)
# plot(TukeyHSD(simp18))
# plot(TukeyHSD(simp20))

# install.packages("multcomp")
# require(multcomp)
# summary(glht(simp18, linfct=mcp(var="Tukey")))
# summary(glht(simp20, linfct=mcp(var="Tukey")))

# npk PENRES -------------------------------------------------------------------

# load

npk_pr <- pr %>%
  select(var, varno, year, d04, d08, d12, d16, d20) %>% 
  filter(varno %in% c("3", "5", "6", "9")) %>% 
  select(var, year, d04, d08, d12, d16, d20)

# long format

npk_prl <- npk_pr %>% 
  melt(id.vars = c("var", "year"), variable.name = ("depth"), value.name = "penres") 

# factors

dept <- c("20", "16", "12", "8", "4")
npk_prl$depth <- factor(npk_prl$depth, levels = c("d20", "d16", "d12", "d08", "d04"), 
                        labels = dept)
npk_prl$var <- factor(npk_prl$var)
npk_prl$year <- factor(npk_prl$year, levels = c("cycle_1", "cycle_2", "cycle_3"),
                      labels = c("cycle 1", "cycle 2", "cycle 3"))

# stats

df3 <- data_summary(npk_prl, varname="penres", 
                    groupnames=c("year", "var", "depth"))
head(df3)
write_xlsx(df3,"npk_penstat.xlsx") # package "writexl"

# plots

ggplot(df3, aes(depth, penres, fill=var))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=penres-sd, ymax=penres+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_brewer(palette = "Greys", breaks = rev(levels(df3$var)))+
  coord_flip()+
  facet_grid(. ~ year)+
  labs(y = "\nPenetration Resistance [MPa]", x = "Depth [cm]", fill = "", title = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
ggsave("plots/npk_pr.png", device = "png", width = 8, height = 4, dpi = 300)

# analyses

c1_npkpr <- npk_prl %>% 
  filter(year == "cycle 1")
c2_npkpr <- npk_prl %>% 
  filter(year == "cycle 2")
c3_npkpr <- npk_prl %>% 
  filter(year == "cycle 3")

# kw 

kruskal.test(penres ~ var, data = c1_npkpr)
kruskal.test(penres ~ var, data = c2_npkpr)
kruskal.test(penres ~ var, data = c3_npkpr)

# install.packages("pgirmess") # post-hoc testy
require(pgirmess)

kruskalmc(penres ~ var, data = c1_npkpr, p=0.05)
kruskalmc(penres ~ var, data = c2_npkpr, p=0.05)
kruskalmc(penres ~ var, data = c3_npkpr, p=0.05)

## one way ANOVA

bartlett.test(penres ~ var, data = c1_npkpr) 
bartlett.test(penres ~ var, data = c2_npkpr) 
bartlett.test(penres ~ var, data = c3_npkpr) 

simpnpkprc1 <- aov(penres ~ var, data = c1_npkpr)
simpnpkprc2 <- aov(penres ~ var, data = c2_npkpr)
simpnpkprc3 <- aov(penres ~ var, data = c3_npkpr)

summary(simpnpkprc1)
summary(simpnpkprc2)
summary(simpnpkprc3)

TukeyHSD(simpnpkprc2)
# plot(TukeyHSD(simp20))

# install.packages("multcomp")
require(multcomp)
summary(glht(simpnpkprc2, linfct=mcp(var="Tukey")))

# krava ROH ---------------------------------------------------------------

# load

kra_roh <- roh %>%
  select(year, varno, var, roh) %>% 
  filter(varno %in% c("1", "2", "3", "4")) %>% 
  select(var, year, roh)

# long format already met

# factors

kra_roh$var <- factor(kra_roh$var)
kra_roh$year <- factor(kra_roh$year, levels = c("cycle_1", "cycle_2", "cycle_3"),
                       labels = c("cycle 1", "cycle 2", "cycle 3"))

# stats

df4 <- data_summary(kra_roh, varname="roh", 
                    groupnames=c("year", "var"))
head(df4)
write_xlsx(df4,"kra_rohstat.xlsx") # package "writexl"

# plots

ggplot(df4, aes(year, roh, fill=var))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=roh-sd, ymax=roh+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_brewer(palette = "Greys")+
  labs(y = expression("Reduced Bulk Density [ g"~ cm^-3~"]"), 
       x = "", title = "", fill = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
ggsave("plots/kra_roh.png", device = "png", width = 8, height = 4, dpi = 300)

# analyses

c1_kraroh <- kra_roh %>% 
  filter(year == "cycle 1")
c2_kraroh <- kra_roh %>% 
  filter(year == "cycle 2")
c3_kraroh <- kra_roh %>% 
  filter(year == "cycle 3")

# kw 

kruskal.test(roh ~ var, data = c1_kraroh)
kruskal.test(roh ~ var, data = c2_kraroh)
kruskal.test(roh ~ var, data = c3_kraroh)

# install.packages("pgirmess") # post-hoc testy
# require(pgirmess)
# 
# kruskalmc(penres ~ var, data = c1_kraroh, p=0.05)
# kruskalmc(penres ~ var, data = c2_kraroh, p=0.05)
# kruskalmc(penres ~ var, data = c3_kraroh, p=0.05)

## one way ANOVA

bartlett.test(roh ~ var, data = c1_kraroh) 
bartlett.test(roh ~ var, data = c2_kraroh) 
bartlett.test(roh ~ var, data = c3_kraroh) 

simpkrarohc1 <- aov(roh ~ var, data = c1_kraroh)
simpkrarohc2 <- aov(roh ~ var, data = c2_kraroh)
simpkrarohc3 <- aov(roh ~ var, data = c3_kraroh)

summary(simpkrarohc1)
summary(simpkrarohc2)
summary(simpkrarohc3)

# TukeyHSD(simpkrarohc2)
# plot(TukeyHSD(simp20))
# install.packages("multcomp")
# require(multcomp)
# summary(glht(simpkraprc2, linfct=mcp(var="Tukey")))

# prase ROH ---------------------------------------------------------------

# load

pra_roh <- roh %>%
  select(var, varno, year, roh) %>% 
  filter(varno %in% c("7", "8", "9", "10")) %>% 
  select(var, year, roh)

# long format already met

# factors

pra_roh$var <- factor(pra_roh$var)
pra_roh$year <- factor(pra_roh$year, levels = c("cycle_1", "cycle_2", "cycle_3"),
                       labels = c("cycle 1", "cycle 2", "cycle 3"))

# stats

df5 <- data_summary(pra_roh, varname="roh", 
                    groupnames=c("year", "var"))
head(df5)
write_xlsx(df5,"pra_rohstat.xlsx") # package "writexl"

# plots

ggplot(df5, aes(year, roh, fill=var))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=roh-sd, ymax=roh+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_brewer(palette = "Greys")+
  # facet_grid(. ~ year)+
  labs(y = expression("Reduced Bulk Density [ g"~ cm^-3~"]"), 
       x = "", title = "", fill = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
ggsave("plots/pra_roh.png", device = "png", width = 8, height = 4, dpi = 300)

# analyses

c1_praroh <- pra_roh %>% 
  filter(year == "cycle 1")
c2_praroh <- pra_roh %>% 
  filter(year == "cycle 2")
c3_praroh <- pra_roh %>% 
  filter(year == "cycle 3")

# kw 

kruskal.test(roh ~ var, data = c1_praroh)
kruskal.test(roh ~ var, data = c2_praroh)
kruskal.test(roh ~ var, data = c3_praroh)

# install.packages("pgirmess") # post-hoc testy
# require(pgirmess)
# 
# kruskalmc(penres ~ var, data = c1_prapr, p=0.05)
# kruskalmc(penres ~ var, data = c2_prapr, p=0.05)
# kruskalmc(penres ~ var, data = c3_prapr, p=0.05)

## one way ANOVA

bartlett.test(roh ~ var, data = c1_praroh) 
bartlett.test(roh ~ var, data = c2_praroh) 
bartlett.test(roh ~ var, data = c3_praroh) 

simpprarohc1 <- aov(roh ~ var, data = c1_praroh)
simpprarohc2 <- aov(roh ~ var, data = c2_praroh)
simpprarohc3 <- aov(roh ~ var, data = c3_praroh)

summary(simpprarohc1)
summary(simpprarohc2)
summary(simpprarohc3)

# TukeyHSD(simppraprc2)
# # plot(TukeyHSD(simp20))
# 
# # install.packages("multcomp")
# require(multcomp)
# summary(glht(simppraprc2, linfct=mcp(var="Tukey")))

# npk ROH ---------------------------------------------------------------

# load

npk_roh <- roh %>%
  select(var, varno, year, roh) %>% 
  filter(varno %in% c("3", "5", "6", "9")) %>% 
  select(var, year, roh)

# long format already met

# factors

npk_roh$var <- factor(npk_roh$var)
npk_roh$year <- factor(npk_roh$year, levels = c("cycle_1", "cycle_2", "cycle_3"),
                       labels = c("cycle 1", "cycle 2", "cycle 3"))

# stats

df6 <- data_summary(npk_roh, varname="roh", 
                    groupnames=c("year", "var"))
head(df6)
write_xlsx(df6,"npk_rohstat.xlsx") # package "writexl"

# plots

ggplot(df6, aes(year, roh, fill=var))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=roh-sd, ymax=roh+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_brewer(palette = "Greys")+
  # facet_grid(. ~ year)+
  labs(y = expression("Reduced Bulk Density [ g"~ cm^-3~"]"), 
       x = "", title = "", fill = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
ggsave("plots/npk_roh.png", device = "png", width = 8, height = 4, dpi = 300)

# analyses

c1_npkroh <- npk_roh %>% 
  filter(year == "cycle 1")
c2_npkroh <- npk_roh %>% 
  filter(year == "cycle 2")
c3_npkroh <- npk_roh %>% 
  filter(year == "cycle 3")

# kw 

kruskal.test(roh ~ var, data = c1_npkroh)
kruskal.test(roh ~ var, data = c2_npkroh)
kruskal.test(roh ~ var, data = c3_npkroh)

# install.packages("pgirmess") # post-hoc testy
require(pgirmess)

# kruskalmc(roh ~ var, data = c1_npkroh, p=0.05)
# kruskalmc(roh ~ var, data = c2_npkroh, p=0.05)
kruskalmc(roh ~ var, data = c3_npkroh, p=0.05)

## one way ANOVA

bartlett.test(roh ~ var, data = c1_npkroh) 
bartlett.test(roh ~ var, data = c2_npkroh) 
bartlett.test(roh ~ var, data = c3_npkroh) 

simpnpkrohc1 <- aov(roh ~ var, data = c1_npkroh)
simpnpkrohc2 <- aov(roh ~ var, data = c2_npkroh)
simpnpkrohc3 <- aov(roh ~ var, data = c3_npkroh)

summary(simpnpkrohc1)
summary(simpnpkrohc2)
summary(simpnpkrohc3)

TukeyHSD(simpnpkrohc3)
# plot(TukeyHSD(simp20))

# install.packages("multcomp")
require(multcomp)
summary(glht(simpnpkrohc3, linfct=mcp(var="Tukey")))

# krava SFH ----------------------------------------------------------------

# load

kra_sfh <- sfh %>%
  select(var, varno, year, sfh) %>% 
  filter(varno %in% c("1", "2", "3", "4")) %>% 
  select(var, year, sfh)

# long format already met

# factors

kra_sfh$var <- factor(kra_sfh$var)
kra_sfh$year <- factor(kra_sfh$year, levels = c("cycle_1", "cycle_2", "cycle_3"),
                       labels = c("cycle 1", "cycle 2", "cycle 3"))

# stats

df7 <- data_summary(kra_sfh, varname="sfh", 
                    groupnames=c("year", "var"))
head(df7)
write_xlsx(df7,"kra_sfhstat.xlsx") # package "writexl"

# plots

ggplot(df7, aes(year, sfh, fill=var))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=sfh-sd, ymax=sfh+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_brewer(palette = "Greys")+
  labs(y = expression("Saturated Hydraulic Conductivity [ mm"~ h^-1~"]"), 
       x = "", title = "", fill = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
ggsave("plots/kra_sfh.png", device = "png", width = 8, height = 4, dpi = 300)

# analyses

c1_krasfh <- kra_sfh %>% 
  filter(year == "cycle 1")
c2_krasfh <- kra_sfh %>% 
  filter(year == "cycle 2")
c3_krasfh <- kra_sfh %>% 
  filter(year == "cycle 3")

# kw 

kruskal.test(sfh ~ var, data = c1_krasfh)
kruskal.test(sfh ~ var, data = c2_krasfh)
kruskal.test(sfh ~ var, data = c3_krasfh)

# install.packages("pgirmess") # post-hoc testy
# require(pgirmess)
# 
# kruskalmc(penres ~ var, data = c1_krasfh, p=0.05)
# kruskalmc(penres ~ var, data = c2_krasfh, p=0.05)
# kruskalmc(penres ~ var, data = c3_krasfh, p=0.05)

## one way ANOVA

bartlett.test(sfh ~ var, data = c1_krasfh) 
bartlett.test(sfh ~ var, data = c2_krasfh) 
bartlett.test(sfh ~ var, data = c3_krasfh) 

simpkrasfhc1 <- aov(sfh ~ var, data = c1_krasfh)
simpkrasfhc2 <- aov(sfh ~ var, data = c2_krasfh)
simpkrasfhc3 <- aov(sfh ~ var, data = c3_krasfh)

summary(simpkrasfhc1)
summary(simpkrasfhc2)
summary(simpkrasfhc3)

# TukeyHSD(simpkrasfhc2)
# plot(TukeyHSD(simp20))
# install.packages("multcomp")
# require(multcomp)
# summary(glht(simpkraprc2, linfct=mcp(var="Tukey")))

# prase SFH ----------------------------------------------------------------

# load

pra_sfh <- sfh %>%
  select(var, varno, year, sfh) %>% 
  filter(varno %in% c("7", "8", "9", "10")) %>% 
  select(var, year, sfh)

# long format already met

# factors

pra_sfh$var <- factor(pra_sfh$var)
pra_sfh$year <- factor(pra_sfh$year, levels = c("cycle_1", "cycle_2", "cycle_3"),
                       labels = c("cycle 1", "cycle 2", "cycle 3"))

# stats

df8 <- data_summary(pra_sfh, varname="sfh", 
                    groupnames=c("year", "var"))
head(df8)
write_xlsx(df8,"pra_sfhstat.xlsx") # package "writexl"

# plots

ggplot(df8, aes(year, sfh, fill=var))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=sfh-sd, ymax=sfh+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_brewer(palette = "Greys")+
  labs(y = expression("Saturated Hydraulic Conductivity [ mm"~ h^-1~"]"), 
       x = "", title = "", fill = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
ggsave("plots/pra_sfh.png", device = "png", width = 8, height = 4, dpi = 300)

# analyses

c1_prasfh <- pra_sfh %>% 
  filter(year == "cycle 1")
c2_prasfh <- pra_sfh %>% 
  filter(year == "cycle 2")
c3_prasfh <- pra_sfh %>% 
  filter(year == "cycle 3")

# kw 

kruskal.test(sfh ~ var, data = c1_prasfh)
kruskal.test(sfh ~ var, data = c2_prasfh)
kruskal.test(sfh ~ var, data = c3_prasfh)

# install.packages("pgirmess") # post-hoc testy
# require(pgirmess)
# 
# kruskalmc(penres ~ var, data = c1_prapr, p=0.05)
# kruskalmc(penres ~ var, data = c2_prapr, p=0.05)
# kruskalmc(penres ~ var, data = c3_prapr, p=0.05)

## one way ANOVA

bartlett.test(sfh ~ var, data = c1_prasfh) 
bartlett.test(sfh ~ var, data = c2_prasfh) 
bartlett.test(sfh ~ var, data = c3_prasfh) 

simpprasfhc1 <- aov(sfh ~ var, data = c1_prasfh)
simpprasfhc2 <- aov(sfh ~ var, data = c2_prasfh)
simpprasfhc3 <- aov(sfh ~ var, data = c3_prasfh)

summary(simpprasfhc1)
summary(simpprasfhc2)
summary(simpprasfhc3)

# TukeyHSD(simppraprc2)
# # plot(TukeyHSD(simp20))
# 
# # install.packages("multcomp")
# require(multcomp)
# summary(glht(simppraprc2, linfct=mcp(var="Tukey")))

# npk SFH ----------------------------------------------------------------

# load

npk_sfh <- sfh %>%
  select(var, varno, year, sfh) %>% 
  filter(varno %in% c("3", "5", "6", "9")) %>% 
  select(var, year, sfh)

# long format already met

# factors

npk_sfh$var <- factor(npk_sfh$var)
npk_sfh$year <- factor(npk_sfh$year, levels = c("cycle_1", "cycle_2", "cycle_3"),
                       labels = c("cycle 1", "cycle 2", "cycle 3"))

# stats

df9 <- data_summary(npk_sfh, varname="sfh", 
                    groupnames=c("year", "var"))
head(df9)
write_xlsx(df9,"npk_sfhstat.xlsx") # package "writexl"

# plots

ggplot(df9, aes(year, sfh, fill=var))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=sfh-sd, ymax=sfh+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_brewer(palette = "Greys")+
  labs(y = expression("Saturated Hydraulic Conductivity [ mm"~ h^-1~"]"), 
       x = "", title = "", fill = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
ggsave("plots/npk_sfh.png", device = "png", width = 8, height = 4, dpi = 300)

# analyses

c1_npksfh <- npk_sfh %>% 
  filter(year == "cycle 1")
c2_npksfh <- npk_sfh %>% 
  filter(year == "cycle 2")
c3_npksfh <- npk_sfh %>% 
  filter(year == "cycle 3")

# kw 

kruskal.test(sfh ~ var, data = c1_npksfh)
kruskal.test(sfh ~ var, data = c2_npksfh)
kruskal.test(sfh ~ var, data = c3_npksfh)

# install.packages("pgirmess") # post-hoc testy
require(pgirmess)

# kruskalmc(sfh ~ var, data = c1_npksfh, p=0.05)
# kruskalmc(sfh ~ var, data = c2_npksfh, p=0.05)
kruskalmc(sfh ~ var, data = c3_npksfh, p=0.05)

## one way ANOVA

bartlett.test(sfh ~ var, data = c1_npksfh) 
bartlett.test(sfh ~ var, data = c2_npksfh) 
bartlett.test(sfh ~ var, data = c3_npksfh) 

simpnpksfhc1 <- aov(sfh ~ var, data = c1_npksfh)
simpnpksfhc2 <- aov(sfh ~ var, data = c2_npksfh)
simpnpksfhc3 <- aov(sfh ~ var, data = c3_npksfh)

summary(simpnpksfhc1)
summary(simpnpksfhc2)
summary(simpnpksfhc3)

TukeyHSD(simpnpksfhc3)
# plot(TukeyHSD(simp20))

# install.packages("multcomp")
require(multcomp)
summary(glht(simpnpksfhc3, linfct=mcp(var="Tukey")))

# krava UD ----------------------------------------------------------------

# load

kra_ud <- ud %>%
  select(var, varno, year, udkn) %>% 
  filter(varno %in% c("1", "2", "3", "4")) %>% 
  select(var, year, udkn)

# long format already met

# factors

kra_ud$var <- factor(kra_ud$var)
kra_ud$year <- factor(kra_ud$year, levels = c("cycle_1", "cycle_2", "cycle_3"),
                       labels = c("cycle 1", "cycle 2", "cycle 3"))

# stats

df10 <- data_summary(kra_ud, varname="udkn", 
                    groupnames=c("year", "var"))
head(df10)
write_xlsx(df10,"kra_udstat.xlsx") # package "writexl"

# plots

ggplot(df10, aes(year, udkn, fill=var))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=udkn-sd, ymax=udkn+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_brewer(palette = "Greys")+
  labs(y = expression("Unit Draft [ kN"~ m^-2~"]"), 
       x = "", title = "", fill = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
ggsave("plots/kra_ud.png", device = "png", width = 8, height = 4, dpi = 300)

# analyses

c1_kraud <- kra_ud %>% 
  filter(year == "cycle 1")
c2_kraud <- kra_ud %>% 
  filter(year == "cycle 2")
c3_kraud <- kra_ud %>% 
  filter(year == "cycle 3")

# kw 

kruskal.test(udkn ~ var, data = c1_kraud)
kruskal.test(udkn ~ var, data = c2_kraud)
kruskal.test(udkn ~ var, data = c3_kraud)

# install.packages("pgirmess") # post-hoc testy
require(pgirmess)
# 
kruskalmc(udkn ~ var, data = c1_kraud, p=0.05)
# kruskalmc(penres ~ var, data = c2_kraud, p=0.05)
# kruskalmc(penres ~ var, data = c3_kraud, p=0.05)

## one way ANOVA

bartlett.test(udkn ~ var, data = c1_kraud) 
bartlett.test(udkn ~ var, data = c2_kraud) 
bartlett.test(udkn ~ var, data = c3_kraud) 

simpkraudc1 <- aov(udkn ~ var, data = c1_kraud)
simpkraudc2 <- aov(udkn ~ var, data = c2_kraud)
simpkraudc3 <- aov(udkn ~ var, data = c3_kraud)

summary(simpkraudc1)
summary(simpkraudc2)
summary(simpkraudc3)

TukeyHSD(simpkraudc2)
# plot(TukeyHSD(simp20))
# install.packages("multcomp")
require(multcomp)
summary(glht(simpkraudc2, linfct=mcp(var="Tukey")))

# prase UD ----------------------------------------------------------------

# load

pra_ud <- ud %>%
  select(var, varno, year, udkn) %>% 
  filter(varno %in% c("7", "8", "9", "10")) %>% 
  select(var, year, udkn)

# long format already met

# factors

pra_ud$var <- factor(pra_ud$var)
pra_ud$year <- factor(pra_ud$year, levels = c("cycle_1", "cycle_2", "cycle_3"),
                       labels = c("cycle 1", "cycle 2", "cycle 3"))

# stats

df11 <- data_summary(pra_ud, varname="udkn", 
                    groupnames=c("year", "var"))
head(df11)
write_xlsx(df11,"pra_udstat.xlsx") # package "writexl"

# plots

ggplot(df11, aes(year, udkn, fill=var))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=udkn-sd, ymax=udkn+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_brewer(palette = "Greys")+
  labs(y = expression("Unit Draft [ kN"~ m^-2~"]"), 
       x = "", title = "", fill = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
ggsave("plots/pra_ud.png", device = "png", width = 8, height = 4, dpi = 300)

# analyses

c1_praud <- pra_ud %>% 
  filter(year == "cycle 1")
c2_praud <- pra_ud %>% 
  filter(year == "cycle 2")
c3_praud <- pra_ud %>% 
  filter(year == "cycle 3")

# kw 

kruskal.test(udkn ~ var, data = c1_praud)
kruskal.test(udkn ~ var, data = c2_praud)
kruskal.test(udkn ~ var, data = c3_praud)

# install.packages("pgirmess") # post-hoc testy
require(pgirmess)
# 
# kruskalmc(penres ~ var, data = c1_prapr, p=0.05)
kruskalmc(udkn ~ var, data = c2_praud, p=0.05)
kruskalmc(udkn ~ var, data = c3_praud, p=0.05)

## one way ANOVA

bartlett.test(udkn ~ var, data = c1_praud) 
bartlett.test(udkn ~ var, data = c2_praud) 
bartlett.test(udkn ~ var, data = c3_praud) 

simppraudc1 <- aov(udkn ~ var, data = c1_praud)
simppraudc2 <- aov(udkn ~ var, data = c2_praud)
simppraudc3 <- aov(udkn ~ var, data = c3_praud)

summary(simppraudc1)
summary(simppraudc2)
summary(simppraudc3)

TukeyHSD(simppraudc2)
TukeyHSD(simppraudc3)
# plot(TukeyHSD(simp20))

# install.packages("multcomp")
require(multcomp)
summary(glht(simppraudc2, linfct=mcp(var="Tukey")))
summary(glht(simppraudc2, linfct=mcp(var="Tukey")))

# npk UD ----------------------------------------------------------------

# load

npk_ud <- ud %>%
  select(var, varno, year, udkn) %>% 
  filter(varno %in% c("3", "5", "6", "9")) %>% 
  select(var, year, udkn)

# long format already met

# factors

npk_ud$var <- factor(npk_ud$var)
npk_ud$year <- factor(npk_ud$year, levels = c("cycle_1", "cycle_2", "cycle_3"),
                       labels = c("cycle 1", "cycle 2", "cycle 3"))

# stats

df12 <- data_summary(npk_ud, varname="udkn", 
                    groupnames=c("year", "var"))
head(df12)
write_xlsx(df12,"npk_udstat.xlsx") # package "writexl"

# plots

ggplot(df12, aes(year, udkn, fill=var))+
  geom_bar(stat="identity", color="black", position = position_dodge())+
  geom_errorbar(aes(ymin=udkn-sd, ymax=udkn+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_brewer(palette = "Greys")+
  labs(y = expression("Unit Draft [ kN"~ m^-2~"]"), 
       x = "", title = "", fill = "")+
  theme_classic(base_size = 15)+
  theme(text=element_text(family="Times New Roman"))
ggsave("plots/npk_ud.png", device = "png", width = 8, height = 4, dpi = 300)


# analyses

c1_npkud <- npk_ud %>% 
  filter(year == "cycle 1")
c2_npkud <- npk_ud %>% 
  filter(year == "cycle 2")
c3_npkud <- npk_ud %>% 
  filter(year == "cycle 3")

# kw 

kruskal.test(udkn ~ var, data = c1_npkud)
kruskal.test(udkn ~ var, data = c2_npkud)
kruskal.test(udkn ~ var, data = c3_npkud)

# install.packages("pgirmess") # post-hoc testy
require(pgirmess)

kruskalmc(udkn ~ var, data = c1_npkud, p=0.05)
kruskalmc(udkn ~ var, data = c2_npkud, p=0.05)
kruskalmc(udkn ~ var, data = c3_npkud, p=0.05)

## one way ANOVA

bartlett.test(udkn ~ var, data = c1_npkud) 
bartlett.test(udkn ~ var, data = c2_npkud) 
bartlett.test(udkn ~ var, data = c3_npkud) 

simpnpkudc1 <- aov(udkn ~ var, data = c1_npkud)
simpnpkudc2 <- aov(udkn ~ var, data = c2_npkud)
simpnpkudc3 <- aov(udkn ~ var, data = c3_npkud)

summary(simpnpkudc1)
summary(simpnpkudc2)
summary(simpnpkudc3)

TukeyHSD(simpnpkudc1)
TukeyHSD(simpnpkudc2)
TukeyHSD(simpnpkudc3)

# install.packages("multcomp")
require(multcomp)
summary(glht(simpnpkudc1, linfct=mcp(var="Tukey")))
summary(glht(simpnpkudc2, linfct=mcp(var="Tukey")))
summary(glht(simpnpkudc3, linfct=mcp(var="Tukey")))

