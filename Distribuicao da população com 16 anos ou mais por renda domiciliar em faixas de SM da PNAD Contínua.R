# Distribuicao da população com 16 anos ou mais por renda domiciliar em faixas de SM da 
# PNADC 2021 e PNADC 2022 (5a entrevista), e PNADC 2023 (1a entrevista) 

# 1. carrega pacotes ----
library(PNADcIBGE)
library(tidyverse)
library(survey)
library(srvyr)
library(scales)
library(kableExtra)

# 2. opcoes do pacote survey ----
options(survey.lonely.psu = "adjust")

# 3. baixa dados da PNADC 2021 - 5a entrevista ----
pnadc_2021.5 <- get_pnadc(year = 2021, interview = 5, vars = c("Estrato","UPA","V1032","V2009","VD5010"), 
                          defyear = 2021, design = FALSE)

# 4. calcula distribuicao da populacao com 16 anos ou mais por renda domiciliar em faixas de SM da PNADC 2021 5a entrevista ---- 
renda_dom_2021 <- 
  pnadc_2021.5 %>% 
  mutate(renda = VD5010 * CO1) %>% # calcula renda domiciliar deflacionada  
  mutate(faixa_renda = case_when(  # categoriza renda domiciliar em faixas de SM (SM em 2021 = R$1100)
    renda < (1100 * 2) ~ 1, 
    renda >= (1100 * 2) & renda < (1100 * 5) ~ 2,
    renda >= (1100 * 5) ~ 3),
    faixa_renda = factor(faixa_renda, levels = c(1,2,3), labels = c("Até 2 SM", "De 2 até 5 SM", "Acima de 5 SM"))) %>%
  as_survey(ids = UPA, strata = Estrato, weights = V1032) %>% # define desenho amostral 
  subset(V2009 >= 16) %>%  # filtra pessoas com 16 anos ou mais
  group_by(faixa_renda) %>% 
  summarise(proportion = survey_mean(na.rm = T)) %>% 
  filter(!is.na(faixa_renda))

# 5. baixa dados da PNADC 2022 - 5a entrevista ----
pnadc_2022.5 <- get_pnadc(year = 2022, interview = 5, vars = c("Estrato","UPA","V1032","V2009","VD5010"), 
                          defyear = 2022, design = FALSE)

# 6. calcula distribuicao da populacao com 16 anos ou mais por renda domiciliar em faixas de SM da PNADC 2022 5a entrevista ---- 
renda_dom_2022 <- 
  pnadc_2022.5 %>% 
  mutate(renda = VD5010 * CO1) %>% # calcula renda domiciliar deflacionada  
  mutate(faixa_renda = case_when(  # categoriza renda domiciliar em faixas de SM (SM em 2022 = R$1212)
    renda < (1212 * 2) ~ 1, 
    renda >= (1212 * 2) & renda < (1212 * 5) ~ 2,
    renda >= (1212 * 5) ~ 3),
    faixa_renda = factor(faixa_renda, levels = c(1,2,3), labels = c("Até 2 SM", "De 2 até 5 SM", "Acima de 5 SM"))) %>%
  as_survey(ids = UPA, strata = Estrato, weights = V1032) %>% # define desenho amostral 
  subset(V2009 >= 16) %>%  # filtra pessoas com 16 anos ou mais
  group_by(faixa_renda) %>% 
  summarise(proportion = survey_mean(na.rm = T)) %>% 
  filter(!is.na(faixa_renda))

# 7. baixa dados da PNADC 2023 - 1a entrevista ----
pnadc_2023.1 <- get_pnadc(year = 2023, interview = 1, vars = c("Estrato","UPA","V1032","V2009","VD5010"), 
                          defyear = 2023, design = FALSE)

# 8. calcula calcula distribuicao da populacao com 16 anos ou mais por renda domiciliar em faixas de SM da PNADC 2023 1a entrevista ----
# Usando SM de janeiro a abril de 2023
renda_dom_2023.1 <- 
  pnadc_2023.1 %>% 
  mutate(renda = VD5010 * CO1) %>% # calcula renda domiciliar deflacionada  
  mutate(faixa_renda = case_when(  # categoriza renda domiciliar em faixas de SM (SM em 01/2023-04/2023 = R$1302)
    renda < (1302 * 2) ~ 1, 
    renda >= (1302 * 2) & renda < (1302 * 5) ~ 2,
    renda >= (1302 * 5) ~ 3),
    faixa_renda = factor(faixa_renda, levels = c(1,2,3), labels = c("Até 2 SM", "De 2 até 5 SM", "Acima de 5 SM"))) %>%
  as_survey(ids = UPA, strata = Estrato, weights = V1032) %>% # define desenho amostral 
  subset(V2009 >= 16) %>%  # filtra pessoas com 16 anos ou mais
  group_by(faixa_renda) %>% 
  summarise(proportion = survey_mean(na.rm = T)) %>% 
  filter(!is.na(faixa_renda))

# Usando SM de maio de 2023
renda_dom_2023.2 <- 
  pnadc_2023.1 %>% 
  mutate(renda = VD5010 * CO1) %>% # calcula renda domiciliar deflacionada  
  mutate(faixa_renda = case_when(  # categoriza renda domiciliar em faixas de SM (SM em 05/2023 = R$1320)
    renda < (1320 * 2) ~ 1, 
    renda >= (1320 * 2) & renda < (1320 * 5) ~ 2,
    renda >= (1320 * 5) ~ 3),
    faixa_renda = factor(faixa_renda, levels = c(1,2,3), labels = c("Até 2 SM", "De 2 até 5 SM", "Acima de 5 SM"))) %>%
  as_survey(ids = UPA, strata = Estrato, weights = V1032) %>% # define desenho amostral 
  subset(V2009 >= 16) %>%  # filtra pessoas com 16 anos ou mais
  group_by(faixa_renda) %>% 
  summarise(proportion = survey_mean(na.rm = T)) %>% 
  filter(!is.na(faixa_renda))


# 9. distribuicao de renda domiciliar de SM para populacao com 16 anos ou mais em 2021, 2022 e 2023 ----
# em 2023 o SM iniciou o ano em R$1302 e depois foi ajustado para R$1320 em maio
renda_dom_2021 %>% 
  select(-proportion_se) %>% 
  mutate(proportion = label_percent()(proportion)) %>% 
  rename("2021" = "proportion") %>% 
  left_join(renda_dom_2022 %>% 
              select(-proportion_se) %>% 
              mutate(proportion = label_percent()(proportion)), 
            by = "faixa_renda") %>% 
  rename("2022" = "proportion") %>% 
  left_join(renda_dom_2023.1 %>% 
              select(-proportion_se) %>% 
              mutate(proportion = label_percent()(proportion)), 
            by = "faixa_renda") %>% 
  rename("2023 <br> (Jan-Abr)" = "proportion") %>%
  left_join(renda_dom_2023.2 %>% 
              select(-proportion_se) %>% 
              mutate(proportion = label_percent()(proportion)), 
            by = "faixa_renda") %>% 
  rename("2023 <br> (Maio-Dez)" = "proportion") %>%
  rename("Faixa de renda domiciliar" = "faixa_renda") %>%
  kbl(escape = FALSE, caption = "Distribuição da população com 16 anos ou mais por faixa de renda domiciliar (em Salários Mínimos = SM)") %>%
  kable_classic(full_width = F) %>% 
  footnote(general = c("Fonte: IBGE - PNAD Contínua 2021 e 2022 (5ª entrevista), 2023 (1ª entrevista)",
                       "Salário Minimo 2021: R$1100",
                       "Salário Minimo 2022: R$1212",
                       "Salário Minimo 2023 (Jan-Abr): R$1302",
                       "Salário Minimo 2023 (Maio-Dez): R$1320"))


# bonus: distribuição da população com 18 anos ou mais por faixas de renda domiciliar alternativa ----
pnadc_2023.1 %>% 
  mutate(renda = VD5010 * CO1) %>% # calcula renda domiciliar deflacionada  
  mutate(faixa_renda = case_when(  # categoriza renda domiciliar em faixas 
    renda <= 2000 ~ 1, 
    renda > 2000 & renda <= 3000 ~ 2,
    renda > 3000 & renda <= 5000 ~ 3,
    renda > 5000 & renda <= 10000 ~ 4,
    renda > 10000 ~ 5),
    faixa_renda = factor(faixa_renda, levels = c(1,2,3,4,5), labels = c("Até R$ 2.000", "Mais de 2.000 a 3.000", "Mais 3.000 a 5.000", "Mais 5.000 a 10.000", "Mais de R$ 10.000"))) %>%
  as_survey(ids = UPA, strata = Estrato, weights = V1032) %>% # define desenho amostral 
  subset(V2009 >= 18) %>%  # filtra pessoas com 18 anos ou mais
  group_by(faixa_renda) %>% 
  summarise(proportion = survey_mean(na.rm = T)) %>% 
  select(-proportion_se) %>% 
  mutate(proportion = label_percent(accuracy = 0.1)(proportion)) %>% 
  rename("Percentual" = "proportion") %>%
  filter(!is.na(faixa_renda)) %>%
  rename("Faixa de renda domiciliar" = "faixa_renda") %>%
  kbl(escape = FALSE, caption = "Distribuição da população com 18 anos ou mais por faixa de renda domiciliar") %>%
  kable_classic(full_width = F) %>% 
  footnote(general = "Fonte: IBGE - PNAD Contínua 2023 (1ª entrevista)")
