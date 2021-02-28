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

# kw 



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

# krava ROH ---------------------------------------------------------------

# load

kra_roh <- roh %>%
  select(var, varno, year, roh) %>% 
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
