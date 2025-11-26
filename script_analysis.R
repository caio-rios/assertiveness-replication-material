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

setwd(here("data"))

df <- readRDS("DATASET FINAL.RDS")


# Gráfico evolucao OECD vs NEW DEMOCRACY

ev_assert <- rbind(df %>% filter(OECD == 1) %>% group_by(YEAR) %>% summarise(ASSERT1 = mean(ASSERT1),
                                                                             ASSERT2 = mean(ASSERT2),
                                                                             ASSERT3 = mean(ASSERT3)) %>% 
                     mutate(TYPE = "OECD"),
                   df %>% filter(nd == 1) %>% group_by(YEAR) %>% summarise(ASSERT1 = mean(ASSERT1),
                                                                           ASSERT2 = mean(ASSERT2),
                                                                           ASSERT3 = mean(ASSERT3)) %>% 
                     mutate(TYPE = "ND")) %>% gather(key = "Assertiveness", value = "Assert", c(ASSERT1, ASSERT2, ASSERT3))


g <- ggplot(ev_assert, aes(x = YEAR, y = Assert)) +
  geom_line(aes(linetype = TYPE), color = "black") +
  facet_wrap( ~ Assertiveness) +
  theme(legend.position = "top") +
  labs(x = "Year", y = "Assertiveness", linetype = "")
g

setwd(here("graphs"))

ggsave("Ev Assert.png")


# ND vs Resto do mundo

ev_assert2 <- df %>% select(YEAR, nd, ASSERT1, ASSERT2, ASSERT3) %>% 
  group_by(nd, YEAR) %>% summarise(ASSERT1 = mean(ASSERT1),
                                   ASSERT2 = mean(ASSERT2),
                                   ASSERT3 = mean(ASSERT3)) %>% 
  mutate(ND = ifelse(nd == 0, "Non ND", "ND")) %>% 
  gather(key = "Assertiveness", value = "Assert", c(ASSERT1, ASSERT2, ASSERT3))


g <- ggplot(ev_assert2, aes(x = YEAR, y = Assert)) +
  geom_line(aes(linetype = ND), color = "black", se = F) +
  facet_wrap( ~ Assertiveness) +
  theme(legend.position = "top") +
  labs(x = "Year", y = "Assertiveness", linetype = "")
g

ggsave("ev assert 2.png")


# Histgram - show 0 overdispersion


g1 <- ggplot(df, aes(x = ASSERT1)) +
  geom_histogram(color = "black", fill = "gray80", binwidth = 1) +
  scale_x_continuous(breaks = seq(0,18, 2)) +
  labs(x = "Assertivity 1", y = "Count") 
g1


g2 <- ggplot(df, aes(x = ASSERT2)) +
  geom_histogram(color = "black", fill = "gray80", binwidth = 1) +
  scale_x_continuous(breaks = seq(0,30, 5)) +
  labs(x = "Assertivity 2", y = "Count")
g2

g3 <- ggplot(df, aes(x = ASSERT3)) +
  geom_histogram(color = "black", fill = "gray80", binwidth = 1) +
  scale_x_continuous(breaks = seq(0,71, 10)) +
  labs(x = "Assertivity 3", y = "Count")
g3

g1 + g2 / g3


ggsave("overdispesions.png")

# RODANDO OS MODELOS

# estatisticas descritivas das variaveis dependentes


dfSummary(df[, c("ASSERT1", "ASSERT2", "ASSERT3")])

a <- data.frame(descr(df$ASSERT1)) 
b <- rownames(a)
a <- a %>% mutate(metrics = b)

b <- data.frame(descr(df$ASSERT2)) 
c <- rownames(b)
b <- b %>% mutate(metrics = c)

c <- data.frame(descr(df$ASSERT3)) 
d <- rownames(c)
c <- c %>% mutate(metrics = d)

a <- a %>% left_join(b) %>% left_join(c)
a <- a %>% relocate(metrics, .before = 1)

# descriptive statistics
a %>% mutate_if(is.numeric, round, 2)


# HURDLE MODELS----------



M1 <- hurdle(ASSERT1 ~ VETO + nd + nd*VETO + DEMOCRACY  + PR + EXP_EDUC_GDP + EXP_MILITARY_GDP + CROP_PROD + EXPORTS + FDI + FUEL_EXP + GDP_GROWTH + TRADE_GDP + GLOBALIZATION + POLITICAL_RISK, 
             data = df, dist = "negbin", link = "logit")

M2 <- hurdle(ASSERT1 ~ ELEC_COMP + nd + nd*ELEC_COMP + DEMOCRACY + PR + EXP_EDUC_GDP + EXP_MILITARY_GDP + CROP_PROD + EXPORTS + FDI + FUEL_EXP + GDP_GROWTH + TRADE_GDP + GLOBALIZATION + POLITICAL_RISK, 
             data = df, dist = "negbin", link = "logit")

M3 <- hurdle(ASSERT1 ~ LEG_FRAC + nd + nd*LEG_FRAC + DEMOCRACY + PR + EXP_EDUC_GDP + EXP_MILITARY_GDP + CROP_PROD + EXPORTS + FDI + FUEL_EXP + GDP_GROWTH + TRADE_GDP + GLOBALIZATION + POLITICAL_RISK, 
             data = df, dist = "negbin", link = "logit")

summary(M1)
summary(M2)
summary(M3)


tail(M1$coefficients$count, 1)
confint(M1)


M4 <- hurdle(ASSERT2 ~ VETO + nd + nd*VETO + DEMOCRACY + PR + EXP_EDUC_GDP + EXP_MILITARY_GDP + CROP_PROD + EXPORTS + FDI + FUEL_EXP + GDP_GROWTH + TRADE_GDP + GLOBALIZATION + POLITICAL_RISK, 
             data = df, dist = "negbin", link = "logit")

M5 <- hurdle(ASSERT2 ~ ELEC_COMP + nd + nd*ELEC_COMP + DEMOCRACY + PR + EXP_EDUC_GDP + EXP_MILITARY_GDP + CROP_PROD + EXPORTS + FDI + FUEL_EXP + GDP_GROWTH + TRADE_GDP + GLOBALIZATION + POLITICAL_RISK, 
             data = df, dist = "negbin", link = "logit")

M6 <- hurdle(ASSERT2 ~ LEG_FRAC + nd + nd*LEG_FRAC + DEMOCRACY + PR + EXP_EDUC_GDP + EXP_MILITARY_GDP + CROP_PROD + EXPORTS + FDI + FUEL_EXP + GDP_GROWTH + TRADE_GDP + GLOBALIZATION + POLITICAL_RISK, 
             data = df, dist = "negbin", link = "logit")

summary(M4)
summary(M5)
summary(M6)


M7 <- hurdle(ASSERT3 ~ VETO + nd + nd*VETO + DEMOCRACY + PR + EXP_EDUC_GDP + EXP_MILITARY_GDP + CROP_PROD + EXPORTS + FDI + FUEL_EXP + GDP_GROWTH + TRADE_GDP + GLOBALIZATION + POLITICAL_RISK, 
             data = df, dist = "negbin", link = "logit")

M8 <- hurdle(ASSERT3 ~ ELEC_COMP + nd + nd*ELEC_COMP + DEMOCRACY + PR + EXP_EDUC_GDP + EXP_MILITARY_GDP + CROP_PROD + EXPORTS + FDI + FUEL_EXP + GDP_GROWTH + TRADE_GDP + GLOBALIZATION + POLITICAL_RISK, 
             data = df, dist = "negbin", link = "logit")

M9 <- hurdle(ASSERT3 ~ LEG_FRAC + nd + nd*LEG_FRAC + DEMOCRACY + PR + EXP_EDUC_GDP + EXP_MILITARY_GDP + CROP_PROD + EXPORTS + FDI + FUEL_EXP + GDP_GROWTH + TRADE_GDP + GLOBALIZATION + POLITICAL_RISK, 
             data = df, dist = "negbin", link = "logit")

summary(M7)
summary(M8)
summary(M9)



stargazer(M1, M2, M3, M4, M5, M6, M7, M8, M9, column.labels = c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9"),
          align=TRUE, zero.component = FALSE, title = "Zero-Truncated Negative Binomial")


stargazer(M1, M2, M3, M4, M5, M6, M7, M8, M9, column.labels = c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9"),
          align=TRUE, zero.component = TRUE, title = "Zero Hurdle Logit")


# building the graph
grafico1 <- data.frame(var = names(M1$coefficients$count),
                       estimate = M1$coefficients$count,
                       ci_lower = confint(M1)[1:16,1],
                       ci_upper = confint(M1)[1:16, 2]) %>% mutate(type = "Count", VD = "ASSERT 1")

grafico2 <- data.frame(var = names(M2$coefficients$count),
                       estimate = M2$coefficients$count,
                       ci_lower = confint(M2)[1:16,1],
                       ci_upper = confint(M2)[1:16, 2]) %>% mutate(type = "Count", VD = "ASSERT 1")

grafico3 <- data.frame(var = names(M3$coefficients$count),
                       estimate = M3$coefficients$count,
                       ci_lower = confint(M3)[1:16,1],
                       ci_upper = confint(M3)[1:16, 2]) %>% mutate(type = "Count", VD = "ASSERT 1")

grafico4 <- data.frame(var = names(M4$coefficients$count),
                       estimate = M4$coefficients$count,
                       ci_lower = confint(M4)[1:16,1],
                       ci_upper = confint(M4)[1:16, 2]) %>% mutate(type = "Count", VD = "ASSERT 2")

grafico5 <- data.frame(var = names(M5$coefficients$count),
                       estimate = M5$coefficients$count,
                       ci_lower = confint(M5)[1:16,1],
                       ci_upper = confint(M5)[1:16, 2]) %>% mutate(type = "Count", VD = "ASSERT 2")

grafico6 <- data.frame(var = names(M6$coefficients$count),
                       estimate = M6$coefficients$count,
                       ci_lower = confint(M6)[1:16,1],
                       ci_upper = confint(M6)[1:16, 2]) %>% mutate(type = "Count", VD = "ASSERT 2")

grafico7 <- data.frame(var = names(M7$coefficients$count),
                       estimate = M7$coefficients$count,
                       ci_lower = confint(M7)[1:16,1],
                       ci_upper = confint(M7)[1:16, 2]) %>% mutate(type = "Count", VD = "ASSERT 3")

grafico8 <- data.frame(var = names(M8$coefficients$count),
                       estimate = M8$coefficients$count,
                       ci_lower = confint(M8)[1:16,1],
                       ci_upper = confint(M8)[1:16, 2]) %>% mutate(type = "Count", VD = "ASSERT 3")

grafico9 <- data.frame(var = names(M9$coefficients$count),
                       estimate = M9$coefficients$count,
                       ci_lower = confint(M9)[1:16,1],
                       ci_upper = confint(M9)[1:16, 2]) %>% mutate(type = "Count", VD = "ASSERT 3")


grafico11 <- data.frame(var = names(M1$coefficients$zero),
                        estimate = M1$coefficients$zero,
                        ci_lower = confint(M1)[17:32,1],
                        ci_upper = confint(M1)[17:32, 2]) %>% mutate(type = "Zero", VD = "ASSERT 1")

grafico12 <- data.frame(var = names(M2$coefficients$zero),
                        estimate = M2$coefficients$zero,
                        ci_lower = confint(M2)[17:32,1],
                        ci_upper = confint(M2)[17:32, 2]) %>% mutate(type = "Zero", VD = "ASSERT 1")

grafico13 <- data.frame(var = names(M3$coefficients$zero),
                        estimate = M3$coefficients$zero,
                        ci_lower = confint(M3)[17:32,1],
                        ci_upper = confint(M3)[17:32, 2]) %>% mutate(type = "Zero", VD = "ASSERT 1")

grafico14 <- data.frame(var = names(M4$coefficients$zero),
                        estimate = M4$coefficients$zero,
                        ci_lower = confint(M4)[17:32,1],
                        ci_upper = confint(M4)[17:32, 2]) %>% mutate(type = "Zero", VD = "ASSERT 2")

grafico15 <- data.frame(var = names(M5$coefficients$zero),
                        estimate = M5$coefficients$zero,
                        ci_lower = confint(M5)[17:32,1],
                        ci_upper = confint(M5)[17:32, 2]) %>% mutate(type = "Zero", VD = "ASSERT 2")

grafico16 <- data.frame(var = names(M6$coefficients$zero),
                        estimate = M6$coefficients$zero,
                        ci_lower = confint(M6)[17:32,1],
                        ci_upper = confint(M6)[17:32, 2]) %>% mutate(type = "Zero", VD = "ASSERT 2")

grafico17 <- data.frame(var = names(M7$coefficients$zero),
                        estimate = M7$coefficients$zero,
                        ci_lower = confint(M7)[17:32,1],
                        ci_upper = confint(M7)[17:32, 2]) %>% mutate(type = "Zero", VD = "ASSERT 3")

grafico18 <- data.frame(var = names(M8$coefficients$zero),
                        estimate = M8$coefficients$zero,
                        ci_lower = confint(M8)[17:32,1],
                        ci_upper = confint(M8)[17:32, 2]) %>% mutate(type = "Zero", VD = "ASSERT 3")

grafico19 <- data.frame(var = names(M9$coefficients$zero),
                        estimate = M9$coefficients$zero,
                        ci_lower = confint(M9)[17:32,1],
                        ci_upper = confint(M9)[17:32, 2]) %>% mutate(type = "Zero", VD = "ASSERT 3")





grafico <- rbind(grafico1, grafico2, grafico3,
                 grafico4, grafico5, grafico6,
                 grafico7, grafico8, grafico9,
                 grafico11, grafico12, grafico13,
                 grafico14, grafico15, grafico16,
                 grafico17, grafico18, grafico19)





ggplot(grafico %>% 
         filter(var %in% c("VETO", "nd", "VETO:nd",
                           "ELEC_COMP", "ELEC_COMP:nd",
                           "LEG_FRAC", "LEG_FRAC:nd")) %>%
         mutate(var = factor(var, levels = c("nd",
                                             "VETO", 
                                             "ELEC_COMP", 
                                             "LEG_FRAC", 
                                             "ELEC_COMP:nd", 
                                             "LEG_FRAC:nd", 
                                             "VETO:nd"))),
       aes(x = var, y = estimate, color = type)) +
  geom_hline(yintercept = 0) +
  geom_point(position = position_dodge2(width = 0.8, preserve = "single")) +
  geom_segment(aes(y = ci_lower, yend = ci_upper),
               position = position_dodge2(width = 0.8, preserve = "single")) +
  coord_flip() +
  facet_wrap(~VD, ncol = 2) +
  theme_bw() +
  theme(panel.grid = element_blank())


ggsave("main results.png")

