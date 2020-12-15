
library(tidyverse)
library(ggplot2)

#####################################################################################################################
#####################################################################################################################

# Aqui eu vou puxar a base 3, com mais variaveis

#####################################################################################################################
#####################################################################################################################


setwd("C:/Users/gutao/OneDrive - Insper - Institudo de Ensino e Pesquisa/Documents/Insper/IC/Data/Dataset")

#base0 <- readxl::read_xlsx("base0.xlsx")
#base1 <- readxl::read_xlsx("base2.xlsx")


base1 <- readxl::read_xlsx("base3.xlsx")


base_crua%>% 
  View

#####################################################################################################################
#####################################################################################################################

# Agora farei alguns mutates pra ajeitar os dados:

#####################################################################################################################
#####################################################################################################################


base_crua <- base1 %>%
  filter(!is.na(...1)) %>% 
  rename(id = ...1, nome = ...2, classe = ...3, bolsa = ...4, tipo = ...5, status = ...6, 
         ticker = ...7, retorno_total = ...8, ret_1 = ...9, ret_2 = ...10, ret_3 = ...11, 
         ret_4 = ...12, ret_5 = ...13, ret_6 = ...14, ret_7 = ...15, ret_8 = ...16, 
         mkt_cap = ...17, ROE = ...18, ROA = ...19, 
         ROIC = ...20, lucro_bruto = ...21, LPA_growth = ...22, alav_BS = ...23,
         EBIT_DivLiq = ...24, ativo_growth = ...25, provlp = ...26, provcp = ...27,
         setor_econ = ...28, desvpad_ret = ...29, sharpe_economatica = ...30, div_yld = ...31,
         LPA_1 = ...32, LPA_2 = ...33, LPA_3 = ...34, LPA_4 = ...35, LPA_5 = ...36, LPA_6 = ...37, 
         LPA_se = ...38,  sharpe_calc = Economatica) %>%
  mutate(retorno_total = as.numeric(retorno_total), ret_1 = as.numeric(ret_1), ret_2 = as.numeric(ret_2),
         ret_3 = as.numeric(ret_3), ret_4 = as.numeric(ret_4), ret_5 = as.numeric(ret_5), 
         ret_6 = as.numeric(ret_6), ret_7 = as.numeric(ret_7), ret_8 = as.numeric(ret_8),
         mkt_cap = as.numeric(mkt_cap), ROE = as.numeric(ROE), ROA = as.numeric(ROA), 
         ROIC = as.numeric(ROIC), lucro_bruto = as.numeric(lucro_bruto), LPA_growth = as.numeric(LPA_growth),
         alav_BS = as.numeric(alav_BS), EBIT_DivLiq = as.numeric(EBIT_DivLiq),
         ativo_growth = as.numeric(ativo_growth), provlp = as.numeric(provlp),
         provcp = as.numeric(provcp), desvpad_ret = as.numeric(desvpad_ret), LPA_1 = as.numeric(LPA_1),
         LPA_2 = as.numeric(LPA_2), LPA_3 = as.numeric(LPA_3), LPA_4 = as.numeric(LPA_4),
         LPA_5 = as.numeric(LPA_5), LPA_6 = as.numeric(LPA_6), LPA_se = as.numeric(LPA_se),
         sharpe_calc = as.numeric(sharpe_calc),
         sharpe_economatica = as.numeric(sharpe_economatica), div_yld = as.numeric(div_yld),
         desvpad_ret = as.numeric(desvpad_ret)) %>%
  mutate(provisoes = provlp + provcp) %>%
  select(-c(tipo, status, bolsa, provcp, provlp, setor_econ, ret_1, ret_2, ret_3, ret_4, ret_5,
            ret_6, ret_7, ret_8, LPA_1, LPA_2, LPA_3, LPA_4, LPA_5, LPA_6))


#base_crua %>% 
  #View



#####################################################################################################################
#####################################################################################################################

# Agora, vamos dividir a base em dois portfolios iniciais, de acordo com o tamanho dos mkt_cap:

#####################################################################################################################
#####################################################################################################################


largefolio <- base_crua %>% 
    filter(!is.na(mkt_cap)) %>%
    filter(mkt_cap >= median(mkt_cap)) #%>% 
                                               # 89 observaçoes


smallfolio <- base_crua %>% 
  filter(!is.na(mkt_cap)) %>%
  filter(mkt_cap < median(mkt_cap)) #%>% 
View                                         # 89 observaçoes







#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

# Agora vamos analisar o ROE no largefolio: 

#####################################################################################################################
#####################################################################################################################

# Essas são as 30% com maiores ROE's
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, ROE) %>% 
  filter(!is.na(ROE), !is.na(retorno)) %>%
  filter(ROE > 0 & ROE <= 100) %>% 
  arrange(desc(ROE)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Essas são as 30% com menores ROE's
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, ROE) %>% 
  filter(!is.na(ROE), !is.na(retorno)) %>%
  filter(ROE > 0 & ROE <= 100) %>% 
  arrange(ROE) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno)




# Comparação:
pavel <- t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)$p.value




#####################################################################################################################
#####################################################################################################################

# Agora vamos analisar o ROE no smallfolio: 

#####################################################################################################################
#####################################################################################################################

# Essas são as 30% com maiores ROE's
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, ROE) %>% 
  filter(!is.na(ROE), !is.na(retorno)) %>%
  filter(ROE > 0 & ROE <= 100) %>% 
  arrange(desc(ROE)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno)


# Essas são as 30% com menores ROE's
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, ROE) %>% 
  filter(!is.na(ROE), !is.na(retorno)) %>%
  filter(ROE > 0 & ROE <= 100) %>% 
  arrange(ROE) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno)


# Comparação:
t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)


#####################################################################################################################
#####################################################################################################################

# Agora vamos analisar o ROA no largefolio: 

#####################################################################################################################
#####################################################################################################################


# Essas são as 30% com maiores ROA's
top30 <- largefolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, ROA) %>% 
  filter(!is.na(ROA), !is.na(retorno)) %>%
  filter(ROA > 0) %>% 
  arrange(desc(ROA)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno)


# Essas são as 30% com menores ROA's
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, ROA) %>% 
  filter(!is.na(ROA), !is.na(retorno)) %>%
  filter(ROA > 0) %>%
  arrange(ROA) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno)


# Comparação:
t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)


#####################################################################################################################
#####################################################################################################################

# Agora vamos analisar o ROA no smallfolio: 

#####################################################################################################################
#####################################################################################################################


# Essas são as 30% com maiores ROA's
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, ROA) %>% 
  filter(!is.na(ROA), !is.na(retorno)) %>%
  filter(ROA > 0) %>%
  arrange(desc(ROA)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  select(retorno)


# Essas são as 30% com menores ROA's
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, ROA) %>% 
  filter(!is.na(ROA), !is.na(retorno)) %>%
  filter(ROA > 0) %>%
  arrange(ROA) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno)


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
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Essas são as 30% com menores ROIC's
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, ROIC) %>% 
  filter(!is.na(ROIC), !is.na(retorno)) %>%
  arrange(ROIC) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


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
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #View()
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Essas são as 30% com menores ROIC's
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, ROIC) %>% 
  filter(!is.na(ROIC), !is.na(retorno)) %>%
  arrange(ROIC) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  select(retorno)


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
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Essas são as 30% com menores Lucro Bruto
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, lucro_bruto) %>% 
  filter(!is.na(lucro_bruto), !is.na(retorno)) %>%
  arrange(lucro_bruto) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


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
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #View()
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Essas são as 30% com menores Lucro Bruto
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, lucro_bruto) %>% 
  filter(!is.na(lucro_bruto), !is.na(retorno)) %>%
  arrange(lucro_bruto) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>%
  #View()
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


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
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Essas são as 30% com menores LPA growth
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, LPA_growth) %>% 
  filter(!is.na(LPA_growth), !is.na(retorno)) %>%
  arrange(LPA_growth) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


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
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #View()
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Essas são as 30% com menores LPA growth
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, LPA_growth) %>% 
  filter(!is.na(LPA_growth), !is.na(retorno)) %>%
  arrange(LPA_growth) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>%
  #View()
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


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
  filter(alav_BS > 0) %>% 
  arrange(desc(alav_BS)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Essas são as 30% com menores BS
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, alav_BS) %>% 
  filter(!is.na(alav_BS), !is.na(retorno)) %>%
  filter(alav_BS > 0) %>%
  arrange(alav_BS) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Comparação:
t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)




# Agora vamos analisar o BS no smallfolio: ---------------------------------------


# Essas são as 30% com maiores BS
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, alav_BS) %>% 
  filter(!is.na(alav_BS), !is.na(retorno)) %>%
  filter(alav_BS > 0) %>%
  arrange(desc(alav_BS)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #View()
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Essas são as 30% com menores BS
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, alav_BS) %>% 
  filter(!is.na(alav_BS), !is.na(retorno)) %>%
  filter(alav_BS > 0) %>%
  arrange(alav_BS) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>%
  #View()
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


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
  filter(EBIT_DivLiq > 0) %>%
  arrange(desc(EBIT_DivLiq)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Essas são as 30% com menores EBIT_DivLiq
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, EBIT_DivLiq) %>% 
  filter(!is.na(EBIT_DivLiq), !is.na(retorno)) %>%
  filter(EBIT_DivLiq > 0) %>%
  arrange(EBIT_DivLiq) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Comparação:
t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)




# Agora vamos analisar o EBIT_DivLiq no smallfolio: ---------------------------------------


# Essas são as 30% com maiores EBIT_DivLiq
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, EBIT_DivLiq) %>% 
  filter(!is.na(EBIT_DivLiq), !is.na(retorno)) %>%
  filter(EBIT_DivLiq > 0) %>%
  arrange(desc(EBIT_DivLiq)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #View()
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Essas são as 30% com menores EBIT_DivLiq
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, EBIT_DivLiq) %>% 
  filter(!is.na(EBIT_DivLiq), !is.na(retorno)) %>%
  filter(EBIT_DivLiq > 0) %>%
  arrange(EBIT_DivLiq) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>%
  #View()
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


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
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Essas são as 30% com menores ativo_growth
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, ativo_growth) %>% 
  filter(!is.na(ativo_growth), !is.na(retorno)) %>%
  arrange(ativo_growth) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


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
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #View()
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Essas são as 30% com menores ativo_growth
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, ativo_growth) %>% 
  filter(!is.na(ativo_growth), !is.na(retorno)) %>%
  arrange(ativo_growth) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>%
  #View()
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


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
  filter(provisoes > 0) %>% 
  arrange(desc(provisoes)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Essas são as 30% com menores provisoes
bottom30 <- largefolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, provisoes) %>% 
  filter(!is.na(provisoes), !is.na(retorno)) %>%
  filter(provisoes > 0) %>% 
  arrange(provisoes) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Comparação:
t.test(return_top30, return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)




# Agora vamos analisar o provisoes no smallfolio: ---------------------------------------


# Essas são as 30% com maiores provisoes
top30 <- smallfolio %>%       
  select(id, nome, classe, ticker, retorno, mkt_cap, provisoes) %>% 
  filter(!is.na(provisoes), !is.na(retorno)) %>%
  filter(provisoes > 0) %>% 
  arrange(desc(provisoes)) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_top30 <- top30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>% 
  #View()
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Essas são as 30% com menores provisoes
bottom30 <- smallfolio %>%
  select(id, nome, classe, ticker, retorno, mkt_cap, provisoes) %>% 
  filter(!is.na(provisoes), !is.na(retorno)) %>%
  filter(provisoes > 0) %>% 
  arrange(provisoes) %>%
  filter(row_number() < (0.3)*max(row_number()))

return_bottom30 <- bottom30 %>% 
  #mutate(cap_weight = (mkt_cap/sum(mkt_cap))) %>%
  #summarise(return = sum(retorno * cap_weight)) %>%
  #View()
  #mutate(ret_ponderado = retorno * cap_weight) %>%
  select(retorno)


# Comparação:
t.test(x = return_top30, y = return_bottom30, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)
