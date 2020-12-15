

library(tidyverse)
library(ggplot2)

# Em primeiro lugar, importei para ca a primeira base do Economatica, ela contem 
# parte dos indicadores que irei analisar:

setwd("C:/Users/gutao/OneDrive - Insper - Institudo de Ensino e Pesquisa/Documents/Insper/IC/Data")

#base0 <- readxl::read_xlsx("base0.xlsx")
base1 <- readxl::read_xlsx("base1.xlsx")

# Agora farei alguns mutates pra ajeitar os dados:

# Base 0:

base_crua <- base1 %>%
  filter(!is.na(...1)) %>% 
  rename(id = ...1, nome = ...2, classe = ...3, bolsa = ...4, tipo = ...5, status = ...6, 
         ticker = ...7, retorno = ...8, mkt_cap = ...9, ROE = ...10, ROA = ...11, 
         ROIC = ...12, lucro_bruto = ...13, LPA_growth = ...14, alav_BS = ...15,
         EBIT_DivLiq = ...16, ativo_growth = ...17, provlp = ...18, provcp = ...19,
         sharpe = Economatica) %>%
  mutate(retorno = as.numeric(retorno), mkt_cap = as.numeric(mkt_cap),
         ROE = as.numeric(ROE), ROA = as.numeric(ROA), ROIC = as.numeric(ROIC),
         lucro_bruto = as.numeric(lucro_bruto), LPA_growth = as.numeric(LPA_growth),
         alav_BS = as.numeric(alav_BS), EBIT_DivLiq = as.numeric(EBIT_DivLiq),
         ativo_growth = as.numeric(ativo_growth), provlp = as.numeric(provlp),
         provcp = as.numeric(provcp), sharpe = as.numeric(sharpe)) %>%
  mutate(provisoes = provlp + provcp) %>%
  select(-c(tipo, status, bolsa, provcp, provlp))

#base_crua %>% 
  #View

# Agora, vamos dividir a base em dois portfolios iniciais, de acordo com o tamanho dos mkt_cap:


largefolio <- base_crua %>% 
  filter(!is.na(mkt_cap)) %>%
  filter(mkt_cap >= median(mkt_cap))  # 179 observaçoes

  
smallfolio <- base_crua %>% 
  filter(!is.na(mkt_cap)) %>%
  filter(mkt_cap < median(mkt_cap))   # 179 observaçoes


#---------------------------------------------------------------
#---------------------------------------------------------------  
  

# Agora vamos analisar o ROE no largefolio: ---------------------------------------


# Essas são as 30% com maiores ROE's
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, ROE) %>% 
  filter(!is.na(ROE), !is.na(retorno)) %>%
  arrange(desc(ROE)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Essas são as 30% com menores ROE's
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, ROE) %>% 
  filter(!is.na(ROE), !is.na(retorno)) %>%
  arrange(ROE) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Comparação:
t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)

  


# Agora vamos analisar o ROE no smallfolio: ---------------------------------------


# Essas são as 30% com maiores ROE's
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, ROE) %>% 
  filter(!is.na(ROE), !is.na(retorno)) %>%
  arrange(desc(ROE)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #View()
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Essas são as 30% com menores ROE's
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, ROE) %>% 
  filter(!is.na(ROE), !is.na(retorno)) %>%
  arrange(ROE) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>%
  #View()
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Comparação:
t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)




#---------------------------------------------------------------
#---------------------------------------------------------------  


# Agora vamos analisar o ROA no largefolio: ---------------------------------------


# Essas são as 30% com maiores ROA's
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, ROA) %>% 
  filter(!is.na(ROA), !is.na(retorno)) %>%
  arrange(desc(ROA)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Essas são as 30% com menores ROA's
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, ROA) %>% 
  filter(!is.na(ROA), !is.na(retorno)) %>%
  arrange(ROA) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Comparação:
t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)




# Agora vamos analisar o ROA no smallfolio: ---------------------------------------


# Essas são as 30% com maiores ROA's
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, ROA) %>% 
  filter(!is.na(ROA), !is.na(retorno)) %>%
  arrange(desc(ROA)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #View()
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Essas são as 30% com menores ROE's
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, ROA) %>% 
  filter(!is.na(ROA), !is.na(retorno)) %>%
  arrange(ROA) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>%
  #View()
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Comparação:
t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)

  

#---------------------------------------------------------------
#---------------------------------------------------------------  


# Agora vamos analisar o ROIC no largefolio: ---------------------------------------


# Essas são as 30% com maiores ROIC's
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, ROIC) %>% 
  filter(!is.na(ROIC), !is.na(retorno)) %>%
  arrange(desc(ROIC)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Essas são as 30% com menores ROIC's
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, ROIC) %>% 
  filter(!is.na(ROIC), !is.na(retorno)) %>%
  arrange(ROIC) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Comparação:
t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)




# Agora vamos analisar o ROIC no smallfolio: ---------------------------------------


# Essas são as 30% com maiores ROIC's
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, ROIC) %>% 
  filter(!is.na(ROIC), !is.na(retorno)) %>%
  arrange(desc(ROIC)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #View()
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Essas são as 30% com menores ROIC's
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, ROIC) %>% 
  filter(!is.na(ROIC), !is.na(retorno)) %>%
  arrange(ROIC) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>%
  #View()
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Comparação:
t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)





#---------------------------------------------------------------
#---------------------------------------------------------------  


# Agora vamos analisar o Lucro bruto no largefolio: ---------------------------------------


# Essas são as 30% com maiores Lucro Bruto
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, lucro_bruto) %>% 
  filter(!is.na(lucro_bruto), !is.na(retorno)) %>%
  arrange(desc(lucro_bruto)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Essas são as 30% com menores Lucro Bruto
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, lucro_bruto) %>% 
  filter(!is.na(lucro_bruto), !is.na(retorno)) %>%
  arrange(lucro_bruto) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Comparação:
t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)




# Agora vamos analisar o Lucro Bruto no smallfolio: ---------------------------------------


# Essas são as 30% com maiores Lucro Bruto
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, lucro_bruto) %>% 
  filter(!is.na(lucro_bruto), !is.na(retorno)) %>%
  arrange(desc(lucro_bruto)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #View()
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Essas são as 30% com menores Lucro Bruto
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, lucro_bruto) %>% 
  filter(!is.na(lucro_bruto), !is.na(retorno)) %>%
  arrange(lucro_bruto) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>%
  #View()
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Comparação:
t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)




#---------------------------------------------------------------
#---------------------------------------------------------------  


# Agora vamos analisar o LPA growth no largefolio: ---------------------------------------


# Essas são as 30% com maiores LPA growth
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, LPA_growth) %>% 
  filter(!is.na(LPA_growth), !is.na(retorno)) %>%
  arrange(desc(LPA_growth)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Essas são as 30% com menores LPA growth
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, LPA_growth) %>% 
  filter(!is.na(LPA_growth), !is.na(retorno)) %>%
  arrange(LPA_growth) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Comparação:
t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)




# Agora vamos analisar o LPA growth no smallfolio: ---------------------------------------


# Essas são as 30% com maiores LPA growth
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, LPA_growth) %>% 
  filter(!is.na(LPA_growth), !is.na(retorno)) %>%
  arrange(desc(LPA_growth)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #View()
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Essas são as 30% com menores LPA growth
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, LPA_growth) %>% 
  filter(!is.na(LPA_growth), !is.na(retorno)) %>%
  arrange(LPA_growth) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>%
  #View()
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Comparação:
t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)




#---------------------------------------------------------------
#---------------------------------------------------------------  


# Agora vamos analisar o BS no largefolio: ---------------------------------------


# Essas são as 30% com maiores BS
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, alav_BS) %>% 
  filter(!is.na(alav_BS), !is.na(retorno)) %>%
  arrange(desc(alav_BS)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Essas são as 30% com menores BS
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, alav_BS) %>% 
  filter(!is.na(alav_BS), !is.na(retorno)) %>%
  arrange(alav_BS) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Comparação:
t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)




# Agora vamos analisar o BS no smallfolio: ---------------------------------------


# Essas são as 30% com maiores BS
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, alav_BS) %>% 
  filter(!is.na(alav_BS), !is.na(retorno)) %>%
  arrange(desc(alav_BS)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #View()
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Essas são as 30% com menores BS
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, alav_BS) %>% 
  filter(!is.na(alav_BS), !is.na(retorno)) %>%
  arrange(alav_BS) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>%
  #View()
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Comparação:
t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)







#---------------------------------------------------------------
#---------------------------------------------------------------  


# Agora vamos analisar o EBIT_DivLiq no largefolio: ---------------------------------------


# Essas são as 30% com maiores EBIT_DivLiq
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, EBIT_DivLiq) %>% 
  filter(!is.na(EBIT_DivLiq), !is.na(retorno)) %>%
  arrange(desc(EBIT_DivLiq)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Essas são as 30% com menores EBIT_DivLiq
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, EBIT_DivLiq) %>% 
  filter(!is.na(EBIT_DivLiq), !is.na(retorno)) %>%
  arrange(EBIT_DivLiq) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Comparação:
t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)




# Agora vamos analisar o EBIT_DivLiq no smallfolio: ---------------------------------------


# Essas são as 30% com maiores EBIT_DivLiq
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, EBIT_DivLiq) %>% 
  filter(!is.na(EBIT_DivLiq), !is.na(retorno)) %>%
  arrange(desc(EBIT_DivLiq)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #View()
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Essas são as 30% com menores EBIT_DivLiq
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, EBIT_DivLiq) %>% 
  filter(!is.na(EBIT_DivLiq), !is.na(retorno)) %>%
  arrange(EBIT_DivLiq) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>%
  #View()
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Comparação:
t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)




#---------------------------------------------------------------
#---------------------------------------------------------------  


# Agora vamos analisar o ativo_growth no largefolio: ---------------------------------------


# Essas são as 30% com maiores ativo_growth
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, ativo_growth) %>% 
  filter(!is.na(ativo_growth), !is.na(retorno)) %>%
  arrange(desc(ativo_growth)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Essas são as 30% com menores ativo_growth
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, ativo_growth) %>% 
  filter(!is.na(ativo_growth), !is.na(retorno)) %>%
  arrange(ativo_growth) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Comparação:
t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)




# Agora vamos analisar o ativo_growth no smallfolio: ---------------------------------------


# Essas são as 30% com maiores ativo_growth
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, ativo_growth) %>% 
  filter(!is.na(ativo_growth), !is.na(retorno)) %>%
  arrange(desc(ativo_growth)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #View()
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Essas são as 30% com menores ativo_growth
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, ativo_growth) %>% 
  filter(!is.na(ativo_growth), !is.na(retorno)) %>%
  arrange(ativo_growth) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>%
  #View()
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Comparação:
t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)




#---------------------------------------------------------------
#---------------------------------------------------------------  


# Agora vamos analisar o provisoes no largefolio: ---------------------------------------


# Essas são as 30% com maiores provisoes
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, provisoes) %>% 
  filter(!is.na(provisoes), !is.na(retorno)) %>%
  arrange(desc(provisoes)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Essas são as 30% com menores provisoes
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, provisoes) %>% 
  filter(!is.na(provisoes), !is.na(retorno)) %>%
  arrange(provisoes) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Comparação:
t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)




# Agora vamos analisar o provisoes no smallfolio: ---------------------------------------


# Essas são as 30% com maiores provisoes
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, provisoes) %>% 
  filter(!is.na(provisoes), !is.na(retorno)) %>%
  arrange(desc(provisoes)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #View()
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Essas são as 30% com menores provisoes
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, provisoes) %>% 
  filter(!is.na(provisoes), !is.na(retorno)) %>%
  arrange(provisoes) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>%
  #View()
  mutate(ret_ponderado = retorno * cap_weight) %>%
  select(ret_ponderado)


# Comparação:
t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)



