if(require(foreign) == F) install.packages("foreign"); require(foreign)
if(require(here) == F) install.packages("here"); require(here)
if(require(readstata13) == F) install.packages("readstata13"); require(readstata13)
if(require(pscl) == F) install.packages("pscl"); require(pscl)
if(require(sandwich) == F) install.packages("sandwich"); require(sandwich)
if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
if(require(magrittr) == F) install.packages("magrittr"); require(magrittr)
if(require(ggplot2) == F) install.packages("ggplot2"); require(ggplot2)
if(require(naniar) == F) install.packages("naniar"); require(naniar)
if(require(stargazer) == F) install.packages("stargazer"); require(stargazer)
if(require(readxl) == F) install.packages("readxl"); require(readxl)
if(require(janitor) == F) install.packages("janitor"); require(janitor)
if(require(patchwork) == F) install.packages("patchwork"); require(patchwork)
if(require(summarytools) == F) install.packages("summarytools"); require(summarytools)
if(require(openxlsx) == F) install.packages("openxlsx"); require(openxlsx)
if(require(lubridate) == F) install.packages("lubridate"); require(lubridate)
# install.packages("devtools")
if(require(vdemdata) == F) devtools::install_github("vdeminstitute/vdemdata"); require(vdemdata)
theme_set(theme_bw())

# CONSTRUÇÃO DO BANCO (VARIAVEIS DEPENDENTES)--------

setwd(here("data"))

df1 <- read_xlsx("novo_banco15012023.xlsx", sheet = "Assert1") %>%
  mutate(COUNTRY = trimws(COUNTRY)) %>%
  group_by(COUNTRY, YEAR) %>% summarise(ASSERT1 = sum(ASSERT1)) %>%
  filter(YEAR >= 1995 &
           YEAR <= 2019)
df2 <- read_xlsx("novo_banco15012023.xlsx", sheet = "Assert2") %>%
  mutate(COUNTRY = trimws(COUNTRY)) %>%
  group_by(COUNTRY, YEAR) %>% summarise(ASSERT2 = sum(ASSERT2)) %>%
  filter(YEAR >= 1995 &
           YEAR <= 2019)
df3 <- read_xlsx("novo_banco15012023.xlsx", sheet = " assert3") %>%
  mutate(COUNTRY = trimws(COUNTRY)) %>%
  group_by(COUNTRY, YEAR) %>% summarise(ASSERT3 = sum(ASSERT3)) %>%
  filter(YEAR >= 1995 &
           YEAR <= 2019)


paises_tratado <- read_xlsx("country name standardization.xlsx")

df <- df1 %>% left_join(df2, by = c("COUNTRY", "YEAR")) %>%
  left_join(df3, by = c("COUNTRY", "YEAR")) %>%
  left_join(paises_tratado, by = "COUNTRY") %>%
  mutate(COUNTRY = ifelse(is.na(COUNTRY_NEW) == T, COUNTRY,
                          COUNTRY_NEW),
         COUNTRY_NEW = NULL)


# dados qog

setwd("../../") # set to where you downloaded the dataset bellow

qog <- read.dta13("qog_std_ts_jan23_stata14.dta") # download at 'https://www.gu.se/en/quality-government/qog-data/data-downloads'

qog_abb <- qog %>% select(cname, ccodealp) %>%
  rename(COUNTRY = cname,
         ABB = ccodealp) %>%
  mutate(COUNTRY = str_to_upper(COUNTRY))



df <- df %>% left_join(unique(qog_abb), by = "COUNTRY") %>%
  filter(COUNTRY != "EUROPEAN UNION")


df <- df %>% filter(ABB != "AUT",
                         ABB != "BEL",
                         ABB != "BGR",
                         ABB != "HRV",
                         ABB != "CYP",
                         ABB != "CZE",
                         ABB != "DNK",
                         ABB != "EST",
                         ABB != "FIN",
                         ABB != "FRA",
                         ABB != "DEU",
                         ABB != "GRC",
                         ABB != "HUN",
                         ABB != "IRL",
                         ABB != "ITA",
                         ABB != "LVA",
                         ABB != "LTU",
                         ABB != "LUX",
                         ABB != "MLT",
                         ABB != "NLD",
                         ABB != "POL",
                         ABB != "PRT",
                         ABB != "ROU",
                         ABB != "SVK",
                         ABB != "SVN",
                         ABB != "ESP",
                         ABB != "SWE",
                         ABB != "GBR",
)


# OECD COUNTRY LIST IN  https://www.oecd.org/about/document/ratification-oecd-convention.htm

oecd <- read_xlsx("oecd.xlsx") %>%
  filter(DATE <= 2019) # Filtrando para países que entraram para oecd no max em 2014

df <- df %>% left_join(oecd %>% mutate(OECD = 1) %>% select(COUNTRY_CODE, OECD), by = c("ABB" = "COUNTRY_CODE")) %>%
  mutate(OECD = replace_na(OECD, 0))

# CONSTRUÇÃO DO BANCO (VARIAVEIS INDEPENDENTES)--------

df <- df %>% 
  mutate(OECD = as.factor(OECD),
         ASSERT1 = as.integer(ASSERT1),
         ASSERT2 = as.integer(ASSERT2),
         ASSERT3 = as.integer(ASSERT3))



head(df)

# Histgram - show 0 overdispersion


g1 <- ggplot(df, aes(x = ASSERT1)) +
  geom_histogram(color = "black", fill = "gray80", breaks = c(0:20)) +
  scale_x_continuous(breaks = c(0:20)) +
  labs(x = "Assertivity 1", y = "Count") 
g1


g2 <- ggplot(df, aes(x = ASSERT2)) +
  geom_histogram(color = "black", fill = "gray80", breaks = c(0:20)) +
  scale_x_continuous(breaks = c(0:20)) +
  labs(x = "Assertivity 2", y = "Count")
g2

g3 <- ggplot(df, aes(x = ASSERT3)) +
  geom_histogram(color = "black", fill = "gray80", breaks = c(0:20)) +
  scale_x_continuous(breaks = c(0:20)) +
  labs(x = "Assertivity 3", y = "Count")
g3

g1 + g2 / g3


setwd(here("graphs"))
ggsave("Figura 1 Dist.png")


# Montando o banco, variavel por variavel

vdem <- vdem %>% filter(year >= 1995)

# VIs

# Veto Power: v2exdfvths
veto <- var_info("v2exdfvths")
veto$clarification
descr(vdem$v2exdfvths)

# electoral competition até 2018 - QOG

# The competition variable portrays the electoral success of smaller parties, 
# that is, the percentage of votes gained by the smaller parties in parliamentary 
# and/or presidential elections. The variable is calculated by subtracting from 
# 100 the percentage of votes won by the largest party (the party which wins most votes) 
# in parliamentary elections or by the party of the successful candidate in presidential 
# elections. Depending on their importance, either parliamentary or presidential 
# elections are used in the calculation of the variable, or both elections are used, 
# with weights. If information on the distribution of votes is not available, 
# or if the distribution does not portray the reality accurately, the distribution 
# of parliamentary seats is used instead. If parliament members are elected but 
# political parties are not allowed to take part in elections, it is assumed 
# that one party has taken all votes or seats. In countries where parties are 
# not banned but yet only independent candidates participate in elections, 
# it is assumed that the share of the largest party is not over 30 percent.

elec_comp <- qog %>% filter(year >= 1995) %>% select(cname, year, van_comp)


# legislative competition FRAC - DPI
# The probability that two deputies picked at random from the legislature will be of different parties.

setwd("../../") # set directory to where you downloaded the dataset bellow

dpi <- read.dta13("DPI2020_stata13.dta") # download dataset in 'https://data.iadb.org/dataset/the-database-of-political-institutions-dpi-2020'
dpi <- dpi %>% filter(year >= 1995)
options(scipen = 999)
leg_frac <- dpi %>% select(countryname, year, frac)

# VETO
df <- df %>% 
  left_join(vdem %>% select(country_text_id, year, v2exdfvths) %>% rename("ABB" = 1, "YEAR" = 2, VETO = 3), by = c("ABB", "YEAR"))

# electoral competition

df <- df %>% 
  left_join(qog %>% select(ccodealp, year, van_comp) %>% rename("ABB" = 1, "YEAR" = 2, "ELEC_COMP" = 3), by = c("ABB", "YEAR"))

# legislative competition FRAC - DPI

df <- df %>% 
  left_join(dpi %>% select(ifs, year, frac) %>% rename("ABB" = 1, "YEAR" = 2, "LEG_FRAC" = 3) %>% 
              mutate(YEAR = year(YEAR)), by = c("ABB", "YEAR"))


# Controles institutionais

# Democracia até 2018 e_democ
democ <- var_info("e_democ")
democ$clarification
descr(vdem$e_democ)
b <- vdem %>% select(country_name, year, e_democ)


# proportional representation

# PR Proportional Representation? (1 if yes, 0 if no)
# “1” if candidates are elected based on the percent of votes received by their 
# party and/or if our sources specifically call the system “proportional 
# representation.” “0” otherwise, except if LIEC is 4 or lower, when “NA” is reported.

pr <- dpi %>% select(countryname, year, pr)

# Democracia

df <- df %>% 
  left_join(vdem %>% select(country_text_id, year, e_democ) %>% rename("ABB" = 1, "YEAR" = 2, DEMOCRACY = 3), by = c("ABB", "YEAR"))


# PR

df <- df %>% 
  left_join(dpi %>% select(ifs, year, pr) %>% rename("ABB" = 1, "YEAR" = 2, PR = 3) %>% 
              mutate(YEAR = as.double(year(YEAR))), by = c("ABB", "YEAR"))




# Social Controls (WORLD BANK)

setwd(here("data"))

wb_soc <- read_csv("world bank soc var.csv") %>% 
  select(-`Series Code`, -`Country Name`) %>% gather(year, var, `1995 [YR1995]`:`2019 [YR2019]`) %>% 
  mutate(var = ifelse(var == "..", NA, var),
         var = as.numeric(var),
         `Series Name` = ifelse(`Series Name` == "Gini index", "GINI",
                                ifelse(`Series Name` == "Government expenditure on education, total (% of GDP)", "EXP_EDUC_GDP",
                                       ifelse(`Series Name` == "Unemployment, total (% of total labor force) (national estimate)", "UNEMPLOYMENT",
                                              "EXP_MILITARY_GDP"))),
         year = gsub(" \\[.*", "", year),
         year = as.double(year)) %>% filter(is.na(`Country Code`) == F) %>% 
  spread(key = `Series Name`, value = var)


df <- df %>% 
  left_join(wb_soc %>% rename("ABB" = 1, "YEAR" = 2), by = c("ABB", "YEAR"))



# Economic controls (WORLD BANK)

wb_eco <- read_csv("world bank var eco.csv") %>% 
  select(-`Series Code`, -`Country Name`) %>% gather(year, var, `1995 [YR1995]`:`2019 [YR2019]`) %>% 
  mutate(var = ifelse(var == "..", NA, var),
         var = as.numeric(var),
         `Series Name` = ifelse(`Series Name` == "Trade (% of GDP)", "TRADE_GDP",
                                ifelse(`Series Name` == "Exports of goods and services (% of GDP)", "EXPORTS",
                                       ifelse(`Series Name` == "GDP growth (annual %)", "GDP_GROWTH",
                                              ifelse(`Series Name` == "Tax revenue (% of GDP)", "TAX_REVENUE",
                                                     ifelse(`Series Name` == "Crop production index (2014-2016 = 100)", "CROP_PROD",
                                                            ifelse(`Series Name` == "Fuel exports (% of merchandise exports)", "FUEL_EXP",
                                                                   "FDI")))))),
         year = gsub(" \\[.*", "", year),
         year = as.double(year)) %>% filter(is.na(`Country Code`) == F) %>% 
  spread(key = `Series Name`, value = var)


df <- df %>% 
  left_join(wb_eco %>% rename("ABB" = 1, "YEAR" = 2), by = c("ABB", "YEAR"))

# INDICE DE GLOBALIZACAO

# The overall index of globalization (scale of 1 to 100) is the weighted average 
# of the following variables: economic globalization, social globalization and 
# political globalization (dr_eg, dr_sg and dr_pg). Most weight has been given 
# to economic followed by social globalization

df <- df %>% 
  left_join(qog %>% select(ccodealp, year, dr_ig) %>% rename("ABB" = 1, "YEAR" = 2, "GLOBALIZATION" = 3), by = c("ABB", "YEAR"))

df <- df %>% select(COUNTRY, ABB, everything())

# POLITICAL RISK AS QUALITY OF GOVERNMENT

# The mean value of the ICRG variables ’Corruption’, ’Law and Order’ and 
# ’Bureaucracy Quality’, scaled from 0 to 1. Higher values indicate higher quality of government.

df <- df %>% 
  left_join(qog %>% select(ccodealp, year, icrg_qog) %>% rename("ABB" = 1, "YEAR" = 2, "POLITICAL_RISK" = 3), by = c("ABB", "YEAR"))


# BANCO FINAL

# COLOCANDO NAs EM -999, -88, -77, -66


df[df==-999] <- NA
df[df==-88] <- NA
df[df==-77] <- NA
df[df==-66] <- NA


# new democracy

nd <- vdemdata::vdem %>% select("country_text_id", "year", "v2x_regime") %>% 
  filter(year >= 1950 & year <= 2019) %>%
  group_by(country_text_id) %>%
  mutate(prop_democracy = mean(v2x_regime %in% c(2, 3))) %>% 
  mutate(nd = ifelse(prop_democracy >= 0.25 & prop_democracy <= 0.75, 1, 0))


nd <- nd %>% select(country_text_id, nd) %>% unique()

df <- df %>% left_join(nd, by = c("ABB" = "country_text_id"))

df <- df %>% left_join(vdemdata::vdem %>% transmute(ABB = country_text_id, 
                                                    YEAR = year, 
                                                    REGIME_TYPE = v2x_regime), by = c("ABB", "YEAR"))

ABB_2018_REGIME <- df %>% filter(YEAR == 2018) %>% filter(REGIME_TYPE <= 1) %>% 
  select(ABB) %>% pull()

df <- df %>% mutate(nd = ifelse(ABB %in% ABB_2018_REGIME, 0, nd))
df <- df %>% mutate(nd = replace_na(nd, 0))


# saveRDS(df, "DATASET FINAL.RDS")


